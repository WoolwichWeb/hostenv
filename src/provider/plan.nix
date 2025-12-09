{ inputs
, system
, lib
, pkgs
, letsEncrypt
, deployPublicKey
, hostenvHostname
, nodeFor ? { production = "backend04"; default = "backend02"; testing = "backend02"; development = "backend02"; }
, nodesPath ? ./nodes
, secretsPath ? ./secrets/secrets.yaml
, statePath ? ./generated/state.json
, nodeSystems ? { }
, cloudflare ? { enable = false; zoneId = null; apiTokenFile = null; }
, testProjects ? null
, testState ? null
, testLockData ? null
}:

# Provider-side infrastructure generator.
# Largely ported from for_refactoring/generate-infra.nix, but relocated under src/provider.

let
  cfgHostenvHostname = hostenvHostname;
  # Detect hostenv project inputs by checking for the presence of evaluated environments.
  projectInputs = if testProjects != null then [ ] else builtins.filter
    (name:
      builtins.hasAttr "hostenv" inputs.${name} &&
      builtins.hasAttr system inputs.${name}.hostenv &&
      builtins.hasAttr "environments" inputs.${name}.hostenv.${system}
    )
    (builtins.attrNames inputs);

  state =
    if testState != null then testState
    else if builtins.pathExists statePath then
      let rawValues = lib.importJSON statePath;
      in lib.filterAttrs (name: _: name != "_description") rawValues
    else
      { };

  lockData =
    if testLockData != null then testLockData
    else if builtins.pathExists ./flake.lock
    then builtins.fromJSON (builtins.readFile ./flake.lock)
    else
      builtins.throw ''
        flake.lock is missing in ${builtins.toString ./.}.
        Please run: nix flake lock (or nix flake update)
      '';

  nextUid =
    let
      minUid = 1001;
      maxUid = builtins.foldl'
        (a: b:
          if b.uid > a.uid then b else a
        )
        { uid = minUid; }
        (builtins.attrValues state);
    in
    maxUid.uid + 1;

  inputNameToProject = inputName:
    let
      orgAndProject = lib.filter (s: s != "") (lib.splitString "__" inputName);
      organisation =
        if builtins.length orgAndProject == 2
        then builtins.elemAt orgAndProject 0
        else
          builtins.throw ''
            Could not get organisation from input name: '${inputName}'.
            Is the input name formatted correctly? i.e. organisation__project
          '';
      project =
        if builtins.length orgAndProject == 2
        then builtins.elemAt orgAndProject 1
        else
          builtins.throw ''
            Could not get project from input name: '${inputName}'.
            Is the input name formatted correctly? i.e. organisation__project
          '';
    in
    { inherit project organisation; };

  # Build a list of hostenv projects, then build a list of environments for each project.
  realEnvs = builtins.concatLists (map
    (name:
      let
        repo = lockData.nodes.${name}.original or (builtins.throw ''
          Could not find ${name} in Flake inputs, do you need to run 'nix flake update ${name}'?
        '');

        orgAndProject = inputNameToProject name;

        minimalHostenv = pkgs.lib.evalModules {
          specialArgs = inputs // { inherit inputs pkgs; };
          modules = [
            (inputs.hostenv.modules + /top-level/full-env.nix)
            (inputs.${name} + /hostenv.nix)
            ({ config, ... }: {
              hostenv.organisation = lib.mkForce orgAndProject.organisation;
              hostenv.project = lib.mkForce orgAndProject.project;
              hostenv.environmentName = lib.mkForce config.defaultEnvironment;
              hostenv.root = lib.mkForce inputs.${name}.hostenv.${system}.environments.${config.defaultEnvironment}.hostenv.root;
            })
          ];
        };

      in
      lib.attrsets.mapAttrsToList
        (
          envName: envCfg:
            let
              hostenv = envCfg.hostenv;

              node =
                if envCfg.type == "production"
                then (nodeFor.production or nodeFor.default)
                else (nodeFor.${envCfg.type} or nodeFor.default);

              authorizedKeys =
                let
                  allUsers = builtins.attrValues envCfg.users;
                in builtins.concatLists (map (u: u.publicKeys or [ ]) allUsers);

              # Hostname reservation logic (copied from legacy generator).
              # Remove current env by username (state keyed by hostenv.userName), not envName.
              allVHosts = builtins.catAttrs "virtualHosts" (builtins.attrValues (builtins.removeAttrs state [ hostenv.userName ]));
              unreservableVHosts = builtins.filter (vhost: vhost != hostenv.hostname) allVHosts;
              filteredEnvVHosts = lib.filterAttrs
                (
                  vhostName: vhost: ! builtins.any
                    (reservedName: vhostName == reservedName)
                    unreservableVHosts
                )
                envCfg.virtualHosts;

              virtualHosts = builtins.mapAttrs
                (
                  n: vhost:
                    (builtins.removeAttrs vhost [ "enableLetsEncrypt" ]) // {
                      enableACME = vhost.enableLetsEncrypt;
                      forceSSL = vhost.enableLetsEncrypt;
                      locations =
                        let
                          locationDefault =
                            if builtins.isNull vhost.globalRedirect then
                              {
                                "/" = {
                                  recommendedProxySettings = true;
                                  proxyPass = "http://${envCfg.hostenv.userName}_upstream";
                                };
                              }
                            else
                              { };
                        in
                        vhost.locations // locationDefault;
                    }
                )
                filteredEnvVHosts;
            in
            let
              hostenv' = hostenv // { hostenvHostname = cfgHostenvHostname; };
            in envCfg // {
              inherit node authorizedKeys virtualHosts;
              hostenv = hostenv';
              repo = repo // { ref = hostenv'.gitRef; };
            }
        )
        minimalHostenv.config.environments
    )
    projectInputs);

  allEnvs = if testProjects != null then testProjects else realEnvs;

  # Assign unique UIDs to new environments; keep existing ones from state or explicit extras.uid.
  allEnvsWithUid = lib.imap0 (idx: env:
    let
      user = env.hostenv.userName;
      extras = env.extras or { };
      uidFromState = if builtins.hasAttr user state then state.${user}.uid else null;
      uidManual = extras.uid or null;
      uid = if uidFromState != null then uidFromState
            else if uidManual != null then uidManual
            else nextUid + idx;
      extras' = extras // { uid = uid; };
    in env // { inherit uid; extras = extras'; }
  ) allEnvs;

  generatedFlake =
    let
      localSystem = "x86_64-linux";
      inputsBlock = builtins.concatStringsSep "\n" (map
        (
          val: ''
            ${val.hostenv.userName} = {
              type = "${val.repo.type}";
              dir = "${if val.repo ? dir then val.repo.dir else ".hostenv"}";
              ref = "${val.repo.ref}";
              ${lib.optionalString (val.repo ? url) ''
              url = "${val.repo.url}";
              ''}
              ${lib.optionalString (! val.repo ? url) ''
              owner = "${val.repo.owner}";
              repo = "${val.repo.repo}";
              ''}
              inputs.hostenv.follows = "hostenv";
            };
          ''
          + (if cloudflare.enable && cloudflare.apiTokenFile != null && cloudflare.zoneId != null then ''
            ${val.hostenv.userName}-cf = {
              type = "path";
              path = "${cloudflare.apiTokenFile}";
            };
          ''
          else "")
        )
        allEnvs);

    in pkgs.writeText "flake.nix" ''
      {
        inputs = {
          systems.url = "github:nix-systems/default";
          deploy-rs = {
            url = "github:serokell/deploy-rs";
            inputs.nixpkgs.follows = "nixpkgs";
          };
          sops-nix = {
            url = "github:Mic92/sops-nix";
            inputs.nixpkgs.follows = "nixpkgs";
          };
          hostenv.url = "${builtins.toString inputs.hostenv}";

          ${inputsBlock}
        };

        outputs = { self, nixpkgs, deploy-rs, systems, ... } @ inputs:
          let
            config = builtins.removeAttrs
              (builtins.fromJSON (builtins.readFile ./plan.json))
              [ "_description" ];
            forEachSystem = nixpkgs.lib.genAttrs (import systems);
            pkgs = forEachSystem (system: import nixpkgs { inherit system; });
            localSystem = "x86_64-linux";

            nixosSystem = node: import ${builtins.toString ./nixos-system.nix} {
              inherit config node nixpkgs pkgs inputs localSystem;
              nodesPath = ${builtins.toString nodesPath};
              secretsPath = "${builtins.toString secretsPath}";
              nodeSystems = ${lib.generators.toPretty {} nodeSystems};
            };

          in
          {
            inherit inputs config;
          };
      }
    '';

  # JSON representation of every environment returned by each hostenv flake.
  generatedConfig =
    let
      configAttrs = builtins.foldl'
        (acc: elem:
          let
            nameParts = builtins.split "-" elem.hostenv.userName;
            firstPart = builtins.elemAt nameParts 0;
            sliceName = "user-${elem.hostenv.organisation}-${firstPart}";
            uid_ = builtins.toString elem.uid;
          in
          lib.recursiveUpdate acc {
            _description = ''
              Contains a build and deployment plan for hostenv servers on NixOS.
              There are two data substructures:

              1. Under **environments** is a JSON representation of hostenv's own modules config, retaining the original structure of that representation.
              2. Each element under **nodes** is NixOS server configuration, and will be merged into the configuration of that server during build.

              Note: all manual changes to this file will be discarded.
            '';
            hostenvHostname = cfgHostenvHostname;
            cloudflare = cloudflare;
            environments = {
              ${elem.hostenv.userName} = elem;
            };
            nodes = {
              ${elem.node} = {
                security.acme = {
                  acceptTerms = letsEncrypt.acceptTerms;
                  defaults.email = letsEncrypt.adminEmail;
                };

                users.groups.${elem.hostenv.userName} = {
                  gid = elem.uid;
                };

                users.users.${elem.hostenv.userName} = {
                  uid = elem.uid;
                  group = elem.hostenv.userName;
                  openssh.authorizedKeys.keys = elem.authorizedKeys ++ [ deployPublicKey ];
                  isNormalUser = true;
                  createHome = true;
                  linger = true;
                };

                systemd.slices = {
                  ${sliceName} = {
                    description = "${firstPart} slice";
                    sliceConfig = {
                      CPUAccounting = "yes";
                      CPUQuota = "200%";
                      MemoryAccounting = "yes";
                      MemoryMax = "12G";
                    };
                  };
                  "user-${elem.hostenv.organisation}-" = { };
                  "${sliceName}-" = { };
                };

                systemd.services."user@${uid_}" = {
                  overrideStrategy = "asDropin";
                  serviceConfig.Slice = "${sliceName}-${uid_}.slice";
                };

                services.nginx.virtualHosts = elem.virtualHosts // {
                  default = {
                    serverName = "_";
                    default = true;
                    rejectSSL = true;
                    locations."/".return = "444";
                  };
                };

              };
            };
          })
        { }
        allEnvsWithUid;
    in
    pkgs.writers.writeJSON "plan.json" configAttrs;

  generatedState =
    let
      planState = builtins.listToAttrs (builtins.map
        (envCfg: {
          name = envCfg.hostenv.userName;
          value = {
            userName = envCfg.hostenv.userName;
            uid = envCfg.uid;
            node = envCfg.node;
            virtualHosts = builtins.attrNames envCfg.virtualHosts;
          };
        })
        allEnvsWithUid);
      mergedState =
        {
          _description = ''
            Persistent state to retain across deployments. Should be committed to version control.
          '';
        } // lib.recursiveUpdate state planState;
    in
    pkgs.writers.writeJSON "state.json" mergedState;

in
{
  flake = generatedFlake;
  plan = generatedConfig;
  state = generatedState;
  environments = allEnvs;
}
