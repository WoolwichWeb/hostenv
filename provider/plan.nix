{ inputs
, system
, lib
, pkgs
, letsEncrypt
, deployPublicKey
, hostenvHostname
, warnInvalidDeployKey ? true
, nodeFor ? { default = null; }
, statePath ? (if inputs ? self then inputs.self + /generated/state.json else null)
, planPath ? (if inputs ? self then inputs.self + /generated/plan.json else null)
, nodeSystems ? { }
, cloudflare ? { enable = false; zoneId = null; apiTokenFile = null; }
, planSource ? "eval"
, lockPath ? (if inputs ? self then inputs.self + /flake.lock else ../flake.lock)
}:

# Provider-side infrastructure generator.
# Largely ported from for_refactoring/generate-infra.nix, but relocated under provider/.

let
  useEval = planSource == "eval";
  cfgHostenvHostname = hostenvHostname;
  sanitizeHeaderValue = label: value:
    if value == null then null else
    if lib.strings.hasInfix "\"" value then
      builtins.throw "provider plan: ${label} may not contain double quotes"
    else
      value;
  mkHeaderLine = name: value:
    if value == null then ""
    else
      let
        hasDouble = lib.strings.hasInfix "\"" value;
        hasSingle = lib.strings.hasInfix "'" value;
        quote =
          if hasDouble && !hasSingle then "'"
          else if hasDouble && hasSingle then
            builtins.throw "provider plan: ${name} header value may not contain both single and double quotes"
          else
            "\"";
      in
      ''add_header ${name} ${quote}${value}${quote} always;'';
  mkSecurityHeaders = { vhost, envType }:
    let
      security = vhost.security or { };
      referrerPolicy = security.referrerPolicy or "strict-origin-when-cross-origin";
      xFrameOptions = security.xFrameOptions or "SAMEORIGIN";
      xContentTypeOptions = security.xContentTypeOptions or true;
      reportTo = security.reportTo or null;
      hstsEnabled =
        (security.hsts or (vhost.hsts or true))
        && (vhost.enableLetsEncrypt or false);
      cspBase = sanitizeHeaderValue "security.csp" (security.csp or null);
      cspReportTo = security.cspReportTo or null;
      cspValue =
        if cspBase == null then null else
        let trimmed = lib.strings.removeSuffix ";" cspBase;
        in
        if cspReportTo == null then
          trimmed
        else
          "${trimmed}; report-to ${cspReportTo}";
      cspHeaderName =
        if (security.cspMode or "enforce") == "report-only" then
          "Content-Security-Policy-Report-Only"
        else
          "Content-Security-Policy";
      allowIndexing = vhost.allowIndexing or (envType == "production");
    in
    lib.concatStringsSep "\n" (lib.filter (line: line != "") [
      (mkHeaderLine "Report-To" reportTo)
      (mkHeaderLine "Referrer-Policy" referrerPolicy)
      (if xFrameOptions == null then "" else mkHeaderLine "X-Frame-Options" xFrameOptions)
      (if xContentTypeOptions then mkHeaderLine "X-Content-Type-Options" "nosniff" else "")
      (if hstsEnabled then mkHeaderLine "Strict-Transport-Security" "max-age=63072000; includeSubDomains; preload" else "")
      (if cspValue == null then "" else mkHeaderLine cspHeaderName cspValue)
      (if allowIndexing then "" else mkHeaderLine "X-Robots-Tag" "noindex")
    ]);
  deployPublicKeyList =
    let
      key = deployPublicKey;
      parts =
        if key == null
        then [ ]
        else lib.filter (s: s != "") (lib.strings.splitString " " key);
      keyType = if builtins.length parts > 0 then builtins.elemAt parts 0 else "";
      keyData = if builtins.length parts > 1 then builtins.elemAt parts 1 else "";
      allowedKeyTypes = [
        "ssh-ed25519"
        "ssh-rsa"
        "ecdsa-sha2-nistp256"
        "ecdsa-sha2-nistp384"
        "ecdsa-sha2-nistp521"
        "sk-ssh-ed25519@openssh.com"
        "sk-ecdsa-sha2-nistp256@openssh.com"
      ];
      isValid =
        key != null
        && key != ""
        && builtins.length parts >= 2
        && lib.elem keyType allowedKeyTypes
        && builtins.match "^[A-Za-z0-9+/=]+$" keyData != null;
    in
    if key == null || key == "" then
      if warnInvalidDeployKey then
        lib.warn "provider.deployPublicKey is unset or empty; skipping deploy key for provider user." [ ]
      else
        [ ]
    else if isValid then
      [ key ]
    else if warnInvalidDeployKey then
      lib.warn "provider.deployPublicKey is not a valid SSH public key; skipping it." [ ]
    else
      [ ];
  requirePath = { name, path, hint ? "" }:
    if path == null then
      builtins.throw ''
        provider plan: ${name} is required.
        ${hint}
      ''
    else if builtins.pathExists path then
      path
    else
      builtins.throw ''
        provider plan: ${name} not found at ${builtins.toString path}.
        ${hint}
      '';

  statePathChecked = requirePath {
    name = "statePath";
    path = statePath;
    hint = "Set provider.statePath (or pass statePath explicitly) and ensure the file exists (it can be an empty JSON object).";
  };

  planPathChecked =
    if planSource == "disk" then
      requirePath
        {
          name = "planPath";
          path = planPath;
          hint = "Set provider.planPath (or pass planPath explicitly) and ensure the file exists when planSource=\"disk\".";
        }
    else
      planPath;
  # Detect hostenv project inputs by checking for the presence of evaluated environments.
  projectInputs =
    if (!useEval) then [ ] else
    builtins.filter
      (name:
        let
          input = inputs.${name};
          hostenvPath = input + /hostenv.nix;
        in
        lib.strings.hasInfix "__" name
        && builtins.hasAttr "lib" input
        && builtins.hasAttr "hostenv" input.lib
        && builtins.hasAttr system input.lib.hostenv
        && builtins.hasAttr "environments" input.lib.hostenv.${system}
        && builtins.pathExists hostenvPath
      )
      (builtins.attrNames inputs);

  assertProjectInputs =
    if useEval && projectInputs == [ ] then
      builtins.throw ''
        provider plan: no client projects found.

        Each client flake must expose a `lib.hostenv.<system>.environments` output.
        Ensure inputs are named organisation__project, export `outputs.lib.hostenv`,
        and include `hostenv.nix` at the flake root (typically by using dir=.hostenv).
      ''
    else
      true;

  state =
    let rawValues = lib.importJSON statePathChecked;
    in lib.filterAttrs (name: _: name != "_description") rawValues;

  lockData =
    if builtins.pathExists lockPath
    then builtins.fromJSON (builtins.readFile lockPath)
    else
      builtins.throw ''
        flake.lock is missing at ${builtins.toString lockPath}.
        Please run: nix flake lock (or nix flake update) at repo root
      '';

  nixosSystemPathExpr = "inputs.parent.lib.provider.nixosSystem";

  planFromDisk =
    if useEval then null
    else lib.importJSON planPathChecked;

  assertDiskUids =
    if useEval then
      true
    else
      let
        envs = planFromDisk.environments or { };
        missing = lib.filterAttrs
          (_: env:
            !(env ? uid) || env.uid == null || builtins.typeOf env.uid != "int"
          )
          envs;
        missingNames = builtins.attrNames missing;
      in
      if missingNames == [ ] then
        true
      else
        builtins.throw ''
          provider plan: plan.json is missing integer uid values for: ${lib.concatStringsSep ", " missingNames}

          Each environment in plan.json must include a numeric "uid" to avoid
          implicit UID assignment by NixOS. Regenerate with planSource="eval"
          or add uid values to the existing plan file.
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
  realEnvs =
    if useEval then
      builtins.concatLists
        (map
          (name:
            let
              repo = lockData.nodes.${name}.original or (builtins.throw ''
                Could not find ${name} in Flake inputs, do you need to run 'nix flake update ${name}'?
              '');

              orgAndProject = inputNameToProject name;

              projectLib =
                if builtins.hasAttr "lib" inputs.${name} then inputs.${name}.lib
                else builtins.throw "provider plan: input '${name}' missing lib output.";

              projectHostenv =
                if builtins.hasAttr "hostenv" projectLib then projectLib.hostenv
                else builtins.throw "provider plan: input '${name}' missing lib.hostenv output.";

              projectHostenvSystem =
                if builtins.hasAttr system projectHostenv then projectHostenv.${system}
                else builtins.throw "provider plan: input '${name}' missing lib.hostenv.${system} output.";

              projectEnvironments =
                if builtins.hasAttr "environments" projectHostenvSystem then projectHostenvSystem.environments
                else builtins.throw "provider plan: input '${name}' is missing lib.hostenv.${system}.environments (export outputs.lib.hostenv.<system>.environments from the project flake).";

              defaultEnvName =
                if builtins.hasAttr "defaultEnvironment" projectHostenvSystem
                then projectHostenvSystem.defaultEnvironment
                else builtins.throw "provider plan: input '${name}' is missing lib.hostenv.${system}.defaultEnvironment (export outputs.lib.hostenv from the project flake).";
              envCfg =
                if builtins.hasAttr defaultEnvName projectEnvironments then projectEnvironments.${defaultEnvName}
                else builtins.throw "provider plan: defaultEnvironment '${defaultEnvName}' missing in ${name}.lib.hostenv.${system}.environments";

              envRoot =
                if envCfg ? hostenv && envCfg.hostenv ? root then envCfg.hostenv.root
                else builtins.throw "provider plan: environment '${defaultEnvName}' in ${name} is missing hostenv.root";

              minimalHostenv = pkgs.lib.evalModules {
                specialArgs = inputs // { inherit inputs pkgs; };
                modules = [
                  ../platform/hostenv-modules/full-env.nix
                  (inputs.${name} + /hostenv.nix)
                  ({ config, ... }: {
                    hostenv.organisation = lib.mkForce orgAndProject.organisation;
                    hostenv.project = lib.mkForce orgAndProject.project;
                    hostenv.environmentName = lib.mkForce config.defaultEnvironment;
                    hostenv.root = lib.mkForce envRoot;
                    hostenv.hostenvHostname = lib.mkForce cfgHostenvHostname;
                  })
                ];
              };

            in
            lib.attrsets.mapAttrsToList
              (
                envName: envCfg:
                  let
                    hostenv = envCfg.hostenv;

                    node = nodeFor.${envCfg.type} or nodeFor.default;

                    authorizedKeys =
                      let
                        allUsers = builtins.attrValues envCfg.users;
                      in
                      builtins.concatLists (map (u: u.publicKeys or [ ]) allUsers);

                    # Hostname reservation logic (copied from legacy generator).
                    # Remove current env by username (state keyed by hostenv.userName), not envName.
                    stateVHosts =
                      builtins.concatLists (
                        map (v: v.virtualHosts or [ ])
                          (builtins.attrValues (builtins.removeAttrs state [ hostenv.userName ]))
                      );
                    # All virtualHosts already reserved by other environments.
                    unreservableVHosts = stateVHosts;
                    conflictsWithState = lib.intersectLists (builtins.attrNames envCfg.virtualHosts) unreservableVHosts;
                  in
                  if conflictsWithState != [ ] then
                    builtins.throw ''
                      provider plan: environment '${envName}' declares virtualHosts that are already reserved in state: ${lib.concatStringsSep ", " conflictsWithState}
                    ''
                  else
                    let
                      filteredEnvVHosts = lib.filterAttrs
                        (
                          vhostName: vhost: ! builtins.any
                            (reservedName: vhostName == reservedName)
                            unreservableVHosts
                        )
                        envCfg.virtualHosts;
                      conflicts = lib.subtractLists
                        (builtins.attrNames envCfg.virtualHosts)
                        (builtins.attrNames filteredEnvVHosts);

                      virtualHosts =
                        if conflicts != [ ] then
                          builtins.throw ''
                            provider plan: environment '${envName}' declares virtualHosts that are already reserved in state: ${lib.concatStringsSep ", " conflicts}
                          ''
                        else
                          builtins.mapAttrs
                            (
                              n: vhost:
                                let
                                  extraConfig = mkSecurityHeaders { vhost = vhost; envType = envCfg.type; };
                                in
                                (builtins.removeAttrs vhost [ "enableLetsEncrypt" "allowIndexing" "security" "hsts" ]) // {
                                  enableACME = vhost.enableLetsEncrypt;
                                  forceSSL = vhost.enableLetsEncrypt;
                                  extraConfig = extraConfig;
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
                    in
                    envCfg // {
                      inherit node authorizedKeys virtualHosts;
                      hostenv = hostenv';
                      repo = repo // { ref = hostenv'.gitRef; };
                    }
              )
              minimalHostenv.config.environments
          )
          projectInputs) else [ ];

  allEnvsUnvalidated =
    if useEval then realEnvs
    else builtins.attrValues (planFromDisk.environments or { });

  # Fail fast on any virtualHost collisions (state or new envs) in one pass.
  allEnvs =
    let
      stateClaims = builtins.concatLists (map
        (name:
          let vhosts = lib.unique (state.${name}.virtualHosts or [ ]);
          in map (v: { name = v; owner = "state:${name}"; }) vhosts
        )
        (builtins.attrNames state));

      newClaims = builtins.concatLists (map
        (env: map (v: { name = v; owner = env.hostenv.userName; }) (builtins.attrNames (env.virtualHosts or { })))
        allEnvsUnvalidated);

      claimTable =
        lib.foldl'
          (acc: claim:
            let
              key = claim.name;
              ownerRaw = claim.owner;
              ownerKey = lib.removePrefix "state:" ownerRaw;
              ownersPrev = acc."${key}".owners or [ ];
              ownersPrevKeys = acc."${key}".ownerKeys or [ ];
              ownersNew = if lib.elem ownerRaw ownersPrev then ownersPrev else ownersPrev ++ [ ownerRaw ];
              ownerKeysNew = if lib.elem ownerKey ownersPrevKeys then ownersPrevKeys else ownersPrevKeys ++ [ ownerKey ];
            in
            acc // {
              "${key}" = {
                name = key;
                owners = ownersNew;
                ownerKeys = ownerKeysNew;
              };
            }
          )
          { }
          (stateClaims ++ newClaims);

      duplicates = lib.filter (x: (lib.length x.ownerKeys) > 1) (builtins.attrValues claimTable);
    in
    if duplicates != [ ] then
      builtins.throw ''
        provider plan: duplicate virtualHosts detected: ${
          lib.concatStringsSep "; "
            (map (d: "${d.name} claimed by ${lib.concatStringsSep "," d.owners}") duplicates)
        }
      ''
    else allEnvsUnvalidated;

  # Assign unique UIDs to new environments when evaluating; in disk mode keep the
  # UIDs already present in the plan JSON.
  allEnvsWithUid =
    if useEval then
      lib.imap0
        (idx: env:
          let
            user = env.hostenv.userName;
            uidFromState = if builtins.hasAttr user state then state.${user}.uid else null;
            uid =
              if uidFromState != null then uidFromState
              else nextUid + idx;
          in
          env // { inherit uid; }
        )
        allEnvs
    else
      map
        (env:
          let uid = env.uid or null;
          in env // { inherit uid; }
        )
        allEnvs;

  generatedFlake =
    let
      inputsBlock = builtins.concatStringsSep "\n" (map
        (
          val:
          let
            lockNode = lockData.nodes.${val.hostenv.userName} or null;
            lockedRev = if lockNode != null && lockNode ? locked then lockNode.locked.rev else null;
            lockedNarHash = if lockNode != null && lockNode ? locked then lockNode.locked.narHash else null;
            lockedRef = if lockNode != null && lockNode ? locked then lockNode.locked.ref or val.repo.ref else val.repo.ref;
            inputName =
              let
                name = val.hostenv.userName;
                isIdent = builtins.match "^[A-Za-z_][A-Za-z0-9_'-]*$" name != null;
              in
              if isIdent then name else "\"${name}\"";
          in
          ''
            ${inputName} = {
              type = "${val.repo.type}";
              dir = "${val.repo.dir or "."}";
              ref = "${lockedRef}";
              ${lib.optionalString (lockedRev != null) ''rev = "${lockedRev}";''}
              ${lib.optionalString (lockedNarHash != null) ''narHash = "${lockedNarHash}";''}
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

    in
    pkgs.writeText "flake.nix" ''
      {
        inputs = {
          parent.url = "path:..";
          systems.url = "github:nix-systems/default";
          deploy-rs.follows = "parent/deploy-rs";
          sops-nix.follows = "parent/sops-nix";
          hostenv.follows = "parent/hostenv-platform";

          ${inputsBlock}
        };

        outputs = { self, nixpkgs, deploy-rs, systems, ... } @ inputs:
          let
            config = builtins.removeAttrs
              (builtins.fromJSON (builtins.readFile ./plan.json))
              [ "_description" ];
            localSystem = "x86_64-linux";
          in
          import inputs.parent.lib.provider.deployOutputs {
            inherit config nixpkgs deploy-rs systems inputs localSystem;
            nodesPath = ../nodes;
            secretsPath = ../secrets/secrets.yaml;
            nodeSystems = ${lib.generators.toPretty {} nodeSystems};
          };
      }
    '';

  # JSON representation of every environment returned by each hostenv flake.
  generatedConfig =
    if useEval then
      let
        base = {
          _description = ''
            Contains a build and deployment plan for hostenv servers on NixOS.
            There are two data substructures:

            1. Under **environments** is a JSON representation of hostenv's own modules config, retaining the original structure of that representation.
            2. Each element under **nodes** is NixOS server configuration, and will be merged into the configuration of that server during build.

            Note: all manual changes to this file will be discarded.
          '';
          hostenvHostname = cfgHostenvHostname;
          cloudflare = cloudflare;
          environments = { };
          nodes = { };
        };

        configAttrs = builtins.foldl'
          (acc: elem:
            let
              nameParts = builtins.split "-" elem.hostenv.userName;
              firstPart = builtins.elemAt nameParts 0;
              sliceName = "user-${elem.hostenv.organisation}-${firstPart}";
              uid_ = builtins.toString elem.uid;
              nodeName = if builtins.isString elem.node && elem.node != "" then elem.node else builtins.throw "nodeFor/default must be set to a node name for environment ${elem.hostenv.userName}";
            in
            lib.recursiveUpdate acc {
              environments = acc.environments // {
                ${elem.hostenv.userName} = elem;
              };
              nodes = acc.nodes // {
                ${nodeName} =
                  let
                    existing = acc.nodes.${elem.node} or { };
                  in
                  lib.recursiveUpdate existing {
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
                      openssh.authorizedKeys.keys = elem.authorizedKeys ++ deployPublicKeyList;
                      isNormalUser = true;
                      createHome = true;
                      linger = true;
                    };

                    users.users.deploy = {
                      isNormalUser = true;
                      extraGroups = [ "wheel" "keys" ];
                      openssh.authorizedKeys.keys = deployPublicKeyList;
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
          base
          allEnvsWithUid;
      in
      pkgs.writers.writeJSON "plan.json" configAttrs
    else
      planPathChecked;

  generatedState =
    if useEval then
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
      pkgs.writers.writeJSON "state.json" mergedState
    else statePathChecked;

in
assert (assertProjectInputs && assertDiskUids);
{
  flake = generatedFlake;
  plan = generatedConfig;
  state = generatedState;
  environments = allEnvs;
}
