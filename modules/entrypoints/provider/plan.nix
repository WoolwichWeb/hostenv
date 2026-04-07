{ config, ... }:
let
  hostenvInputs = config.flake.lib.hostenvInputs;
  # Provider-side infrastructure generator.
  providerPlan =
    { inputs
    , repoRoot
    , system
    , lib
    , pkgs
    , hostenvHostname
    , letsEncrypt
    , nodeFor
    , nodeSystems
    , nodeModules ? [ ]
    , nixSigning ? { trustedPublicKeys = [ ]; }
    , statePath ? (repoRoot + "/generated/state.json")
    , lockPath ? (repoRoot + /flake.lock)
    , cloudflare ? { enable = false; zoneId = null; apiTokenFile = null; }
    , generatedFlake ? { }
    , deploy
    , serviceResolution
    }:

    let
      cfgHostenvHostname = hostenvHostname;
      hostenvInput =
        hostenvInputs.requireInput {
          inherit inputs;
          name = "hostenv";
          context = "provider plan";
        };
      hostenvMakeHostenv =
        if hostenvInput ? makeHostenv
        then hostenvInput.makeHostenv.${system}
        else builtins.throw "provider plan: hostenv input missing makeHostenv output.";
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

      isNullOrEmpty = value: value == null || value == "";

      normalizeWebhookHost = webhookHostRaw:
        let
          raw =
            if webhookHostRaw == null then ""
            else lib.strings.toLower webhookHostRaw;
          withoutScheme =
            if lib.strings.hasPrefix "https://" raw then
              lib.strings.removePrefix "https://" raw
            else if lib.strings.hasPrefix "http://" raw then
              lib.strings.removePrefix "http://" raw
            else
              raw;
        in
        if withoutScheme == "" then
          builtins.throw "provider plan: services.hostenv-provider.webhookHost must not be empty"
        else if lib.strings.hasInfix "/" withoutScheme then
          builtins.throw "provider plan: services.hostenv-provider.webhookHost must be host[:port] without a path"
        else
          withoutScheme;

      trimTrailingNewlines = value:
        if lib.hasSuffix "\n" value then
          trimTrailingNewlines (lib.removeSuffix "\n" value)
        else if lib.hasSuffix "\r" value then
          trimTrailingNewlines (lib.removeSuffix "\r" value)
        else
          value;

      stripTrailingSlashes = value:
        if lib.hasSuffix "/" value then
          stripTrailingSlashes (lib.removeSuffix "/" value)
        else
          value;

      joinUrlPath = baseUrl: pathSegment:
        "${stripTrailingSlashes baseUrl}/${pathSegment}";

      defaultEnvInputFollows = {
        hostenv = "hostenv";
        nixpkgs = "nixpkgs";
        flake-parts = "flake-parts";
        phps = "phps";
      };

      generatedFlakeInputs = generatedFlake.inputs or { };
      envInputsCfg = generatedFlake.envInputs or { };
      envInputFollows =
        if envInputsCfg ? follows && envInputsCfg.follows != null
        then envInputsCfg.follows
        else defaultEnvInputFollows;
      envInputExtra = if envInputsCfg ? extra then envInputsCfg.extra else (_: { });

      nodeModulesRel =
        let
          basePath = builtins.toString repoRoot;
          normalize = module:
            if builtins.isString module then
              module
            else if builtins.isPath module then
              let modulePath = builtins.toString module;
              in
              if lib.strings.hasPrefix (basePath + "/") modulePath then
                lib.strings.removePrefix (basePath + "/") modulePath
              else
                builtins.throw "provider plan: nodeModules path '${modulePath}' must be under provider root (${basePath})"
            else
              builtins.throw "provider plan: nodeModules entries must be strings or paths.";
        in
        map normalize nodeModules;
      # Detect hostenv project inputs by checking for the presence of evaluated environments.
      projectInputs =
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
        if projectInputs == [ ] then
          builtins.throw ''
            provider plan: no client projects found.

            Each client flake must expose a `lib.hostenv.<system>.environments` output.
            Ensure inputs are named organisation__project, export `outputs.lib.hostenv`,
            and include `hostenv.nix` at each project's flake root (typically by using
            dir=.hostenv).
          ''
        else
          true;

      state =
        let
          rawValues = if builtins.pathExists statePath then lib.importJSON statePath else { };
        in
        lib.filterAttrs (name: _: name != "_description") rawValues;

      lockData =
        if builtins.pathExists lockPath
        then builtins.fromJSON (builtins.readFile lockPath)
        else
          builtins.throw ''
            flake.lock is missing at ${builtins.toString lockPath}.
            Please run: nix flake lock (or nix flake update) at repo root
          '';

      nextUid =
        let
          minUid = 1001;
          maxUid = lib.foldl'
            (a: b:
              let
                value =
                  if builtins.isAttrs b && b ? uid && builtins.typeOf b.uid == "int"
                  then b.uid
                  else minUid;
              in
              if value > a.uid then { uid = value; } else a
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
      # This overrides options set in the user's .hostenv/flake.nix, using
      # values from provider inputs. For example: see `inputNameToProject` and
      # how it is used in this function.
      realEnvs =
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

                minimalHostenv =
                  hostenvMakeHostenv [
                    (inputs.${name} + /hostenv.nix)
                    ({ config, ... }: {
                      hostenv.organisation = lib.mkForce orgAndProject.organisation;
                      hostenv.project = lib.mkForce orgAndProject.project;
                      hostenv.environmentName = lib.mkForce config.defaultEnvironment;
                      hostenv.root = lib.mkForce envRoot;
                      hostenv.hostenvHostname = lib.mkForce cfgHostenvHostname;
                    })
                  ]
                    null;

                migrateEnvExtras =
                  let
                    resticBackups = minimalHostenv.config.services.restic.backups or { };
                    migrateBackupKeys = builtins.filter (n: lib.hasSuffix "-migrate" n) (builtins.attrNames resticBackups);
                  in
                  { migrations = migrateBackupKeys; };

              in
              lib.attrsets.mapAttrsToList
                (
                  envName: envCfg:
                    let
                      evaluatedHostenv =
                        hostenvMakeHostenv [
                          (inputs.${name} + /hostenv.nix)
                          ({ config, ... }: {
                            hostenv.organisation = lib.mkForce orgAndProject.organisation;
                            hostenv.project = lib.mkForce orgAndProject.project;
                            hostenv.environmentName = lib.mkForce envName;
                            hostenv.root = lib.mkForce envRoot;
                            hostenv.hostenvHostname = lib.mkForce cfgHostenvHostname;
                          })
                        ]
                          null;
                      effectiveEnvCfg =
                        if builtins.hasAttr envName evaluatedHostenv.config.environments
                        then evaluatedHostenv.config.environments.${envName}
                        else envCfg;
                      hostenv = effectiveEnvCfg.hostenv;
                      isSelectedServiceEnv =
                        serviceResolution != null
                        && orgAndProject.organisation == serviceResolution.organisation
                        && orgAndProject.project == serviceResolution.project
                        && envName == serviceResolution.environmentName;
                      evaluatedProviderServiceCfg =
                        evaluatedHostenv.config.services.hostenv-provider or { };
                      serviceEnabled = evaluatedProviderServiceCfg.enable or false;
                      webhookHost =
                        if isSelectedServiceEnv && serviceEnabled then
                          normalizeWebhookHost (evaluatedProviderServiceCfg.webhookHost or null)
                        else
                          null;
                      envVirtualHostsRaw = effectiveEnvCfg.virtualHosts or { };
                      envVirtualHosts =
                        if webhookHost != null && !(builtins.hasAttr webhookHost envVirtualHostsRaw) then
                          envVirtualHostsRaw // {
                            ${webhookHost} = {
                              enableLetsEncrypt = true;
                              globalRedirect = null;
                              locations = { };
                            };
                          }
                        else
                          envVirtualHostsRaw;
                      webhookEnableLetsEncrypt =
                        if webhookHost == null then null
                        else
                          (envVirtualHosts.${webhookHost}.enableLetsEncrypt or true);

                      node = nodeFor.${effectiveEnvCfg.type} or nodeFor.default;

                      authorizedKeys =
                        let
                          allUsers = builtins.attrValues effectiveEnvCfg.users;
                        in
                        builtins.concatLists (map (u: u.publicKeys or [ ]) allUsers);

                      # Hostname reservation logic.
                      # Remove current env by username (state keyed by hostenv.userName), not envName.
                      stateVHosts =
                        builtins.concatLists (
                          map (v: v.virtualHosts or [ ])
                            (builtins.attrValues (builtins.removeAttrs state [ hostenv.userName ]))
                        );
                      # All virtualHosts already reserved by other environments.
                      unreservableVHosts = stateVHosts;
                      conflictsWithState = lib.intersectLists (builtins.attrNames envVirtualHosts) unreservableVHosts;
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
                          envVirtualHosts;
                        conflicts = lib.subtractLists
                          (builtins.attrNames envVirtualHosts)
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
                                    extraConfig = mkSecurityHeaders { vhost = vhost; envType = effectiveEnvCfg.type; };
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
                                                proxyPass = "http://${effectiveEnvCfg.hostenv.userName}_upstream";
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
                        envWithMigrations = lib.recursiveUpdate effectiveEnvCfg migrateEnvExtras;
                        projectEnvCfg =
                          if builtins.hasAttr envName projectEnvironments
                          then projectEnvironments.${envName}
                          else effectiveEnvCfg;
                        hostenv' = hostenv // {
                          hostenvHostname = cfgHostenvHostname;
                          backupsRepoHost =
                            if projectEnvCfg ? hostenv && projectEnvCfg.hostenv ? backupsRepoHost
                            then projectEnvCfg.hostenv.backupsRepoHost
                            else hostenv.backupsRepoHost or null;
                        };
                      in
                      envWithMigrations // {
                        inherit node authorizedKeys virtualHosts;
                        hostenv = hostenv';
                        repo = repo // { ref = hostenv'.gitRef; };
                        _providerService = {
                          enabled = serviceEnabled;
                          inherit webhookHost webhookEnableLetsEncrypt;
                        };
                      }
                )
                minimalHostenv.config.environments
            )
            projectInputs);

      # Check for virtualHost collisions.
      allEnvs =
        let
          stateClaims = builtins.concatLists
            (map
              (name:
                let vhosts = lib.unique (state.${name}.virtualHosts or [ ]);
                in map (v: { name = v; owner = "state:${name}"; }) vhosts
              )
              (builtins.attrNames state));

          newClaims = builtins.concatLists
            (map
              (env: map (v: { name = v; owner = env.hostenv.userName; }) (builtins.attrNames (env.virtualHosts or { })))
              realEnvs);

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

          duplicates = lib.filter
            (x: (lib.length x.ownerKeys) > 1)
            (builtins.attrValues claimTable);
        in
        if duplicates != [ ] then
          builtins.throw
            ''
              provider plan: duplicate virtualHosts detected: ${
                lib.concatStringsSep "; "
                  (map (d: "${d.name} claimed by ${lib.concatStringsSep "," d.owners}") duplicates)
              }
            ''
        else realEnvs;

      # Assign unique UIDs to new environments when evaluating; in disk mode keep the
      # UIDs already present in the plan JSON.
      allEnvsWithUid =
        lib.imap0
          (idx: env:
            let
              user = env.hostenv.userName;
              uidFromState = if builtins.hasAttr user state then state.${user}.uid else null;
              stateNodeRaw =
                if builtins.hasAttr user state
                then state.${user}.node or null
                else null;
              stateNode =
                if builtins.isString stateNodeRaw && stateNodeRaw != ""
                then stateNodeRaw
                else null;
              previousNode =
                if stateNode != null && stateNode != env.node
                then stateNode
                else null;
              uid =
                if uidFromState != null then uidFromState
                else nextUid + idx;
            in
            env // { inherit uid previousNode; }
          )
          allEnvs;

      selectedServiceMatches =
        if serviceResolution == null then
          [ ]
        else
          lib.filter
            (env:
              env.hostenv.organisation == serviceResolution.organisation
              && env.hostenv.project == serviceResolution.project
              && env.hostenv.environmentName == serviceResolution.environmentName)
            allEnvsWithUid;

      selectedServiceEnv =
        if serviceResolution == null then
          null
        else if selectedServiceMatches == [ ] then
          builtins.throw ''
            provider plan: provider.serviceResolution points to ${serviceResolution.organisation}/${serviceResolution.project}/${serviceResolution.environmentName}, but no matching enabled environment was found.
          ''
        else
          builtins.head selectedServiceMatches;

      selectedServiceDerived =
        if selectedServiceEnv == null then
          { }
        else
          selectedServiceEnv._providerService or { };

      providerServiceEnabled =
        selectedServiceEnv != null
        && (selectedServiceDerived.enabled or false);

      providerWebhookHost =
        if providerServiceEnabled then
          if selectedServiceDerived.webhookHost or null == null then
            builtins.throw "provider plan: services.hostenv-provider.webhookHost is required when services.hostenv-provider.enable is true"
          else
            selectedServiceDerived.webhookHost
        else
          null;

      providerWebhookEnableLetsEncrypt =
        if providerServiceEnabled then
          selectedServiceDerived.webhookEnableLetsEncrypt or true
        else
          false;

      providerApiBaseUrlDerived =
        if providerServiceEnabled then
          let
            scheme = if providerWebhookEnableLetsEncrypt then "https" else "http";
          in
          "${scheme}://${providerWebhookHost}"
        else
          null;

      nodeAuthTokenFileDerived =
        if providerServiceEnabled then
          "/run/secrets/hostenv/provider_node_token"
        else
          null;

      effectiveDeployProviderApiBaseUrl =
        if deploy.enable
          && isNullOrEmpty deploy.providerApiBaseUrl
          && providerApiBaseUrlDerived != null then
          providerApiBaseUrlDerived
        else
          deploy.providerApiBaseUrl;

      effectiveDeployNodeAuthTokenFile =
        if deploy.enable
          && isNullOrEmpty deploy.nodeAuthTokenFile
          && nodeAuthTokenFileDerived != null then
          nodeAuthTokenFileDerived
        else
          deploy.nodeAuthTokenFile;

      providerCachePublicKey =
        if providerServiceEnabled then
          let
            p = repoRoot + /generated/cache-public-key.txt;
          in
          if builtins.pathExists p then
            trimTrailingNewlines (builtins.readFile p)
          else
            builtins.throw "missing generated/cache-public-key.txt; run nix run .#hostenv-provider -- cache-secrets"
        else
          null;

      providerCacheUrl =
        if providerServiceEnabled then
          joinUrlPath
            (if !isNullOrEmpty deploy.providerApiBaseUrl
            then deploy.providerApiBaseUrl
            else providerApiBaseUrlDerived)
            "cache"
        else
          null;

      assertDeployApiBase =
        if deploy.enable && isNullOrEmpty effectiveDeployProviderApiBaseUrl then
          builtins.throw ''
            provider plan: hostenv-deploy-agent is enabled but provider.deploy.providerApiBaseUrl is empty.
          ''
        else
          true;

      assertDeployTokenFile =
        if deploy.enable
          && isNullOrEmpty effectiveDeployNodeAuthTokenFile
          && (deploy.nodeAuthTokenFiles == { } || builtins.attrNames deploy.nodeAuthTokenFiles == [ ]) then
          builtins.throw ''
            provider plan: hostenv-deploy-agent is enabled but provider.deploy.nodeAuthTokenFile is empty.
          ''
        else
          true;

      generatedFlakeFile =
        let
          envInputSpec = val:
            let
              lockNode = lockData.nodes.${val.hostenv.userName} or null;
              lockedRev = if lockNode != null && lockNode ? locked then lockNode.locked.rev else null;
              lockedNarHash = if lockNode != null && lockNode ? locked then lockNode.locked.narHash else null;
              lockedRef = if lockNode != null && lockNode ? locked then lockNode.locked.ref or val.repo.ref else val.repo.ref;
              repoAttrs =
                {
                  type = val.repo.type;
                  dir = val.repo.dir or ".";
                  ref = lockedRef;
                }
                // (lib.optionalAttrs (lockedRev != null) { rev = lockedRev; })
                // (lib.optionalAttrs (lockedNarHash != null) { narHash = lockedNarHash; })
                // (if val.repo ? url
                then { url = val.repo.url; }
                else { owner = val.repo.owner; repo = val.repo.repo; });
              followsAttrs = lib.mapAttrs (_: v: { follows = v; }) envInputFollows;
              base = repoAttrs // { inputs = followsAttrs; };
              extra = envInputExtra val;
            in
            lib.recursiveUpdate base extra;

          envInputs =
            let
              inputsList = map
                (val: { name = val.hostenv.userName; value = envInputSpec val; })
                allEnvs;
              cfInputs =
                if cloudflare.enable && cloudflare.apiTokenFile != null && cloudflare.zoneId != null then
                  map
                    (val: {
                      name = "${val.hostenv.userName}-cf";
                      value = {
                        type = "path";
                        path = "${cloudflare.apiTokenFile}";
                      };
                    })
                    allEnvs
                else
                  [ ];
            in
            builtins.listToAttrs
              (inputsList ++ cfInputs);

          baseInputs = {
            parent.url = "path:..";
            sops-nix.follows = "parent/sops-nix";
            hostenv.follows = "parent/hostenv";
            nixpkgs.follows = "parent/nixpkgs";
            flake-parts.follows = "parent/flake-parts";
            phps.follows = "parent/phps";
          };
          inputsMerged = baseInputs // generatedFlakeInputs // envInputs;
          inputsText = lib.generators.toPretty
            { }
            inputsMerged;
          nodeModulesText =
            lib.concatMapStringsSep
              "\n"
              (rel: ''            (inputs.parent + "/${rel}")'')
              nodeModulesRel;

        in
        pkgs.writeText
          "flake.nix"
          ''
            {
              inputs = ${inputsText};

              outputs = { self, nixpkgs, ... } @ inputs:
                let
                  config = builtins.removeAttrs
                    (builtins.fromJSON (builtins.readFile ./plan.json))
                    [ "_description" ];
                  localSystem = "x86_64-linux";
                in
                inputs.parent.lib.provider.deployOutputs {
                  inherit config nixpkgs inputs localSystem;
                  nodesPath = ../nodes;
                  secretsPath = ./secrets.merged.yaml;
                  nodeSystems = ${lib.generators.toPretty {} nodeSystems};
                  nodeModules = [
            ${nodeModulesText}
                  ];
                };
            }
          '';

      # JSON representation of every environment returned by each hostenv flake.
      generatedConfig =
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
            nixSigning = {
              trustedPublicKeys = nixSigning.trustedPublicKeys;
            };
            deploy =
              if deploy.enable then
                {
                  enable = true;
                  providerApiBaseUrl = effectiveDeployProviderApiBaseUrl;
                  nodeAuthTokenFile = effectiveDeployNodeAuthTokenFile;
                  nodeAuthTokenFiles = deploy.nodeAuthTokenFiles;
                  reconnectSeconds = deploy.reconnectSeconds;
                }
              else { enable = false; };
            cache = {
              enable = serviceResolution != null;
            };
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
                  ${elem.hostenv.userName} = builtins.removeAttrs elem [ "_providerService" ];
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

                      provider = {
                        service = serviceResolution;
                        nixSigning.trustedPublicKeys = nixSigning.trustedPublicKeys;
                        cache =
                          if providerServiceEnabled then
                            {
                              enable = true;
                              url = providerCacheUrl;
                              publicKey = providerCachePublicKey;
                            }
                          else { };
                        deploy =
                          if deploy.enable then
                            {
                              enable = true;
                              providerApiBaseUrl = effectiveDeployProviderApiBaseUrl;
                              nodeName = nodeName;
                              nodeAuthTokenFile =
                                if builtins.hasAttr nodeName deploy.nodeAuthTokenFiles
                                then deploy.nodeAuthTokenFiles.${nodeName}
                                else effectiveDeployNodeAuthTokenFile;
                              reconnectSeconds = deploy.reconnectSeconds;
                            }
                          else { enable = false; };
                      };

                      users.groups.${elem.hostenv.userName} = {
                        gid = elem.uid;
                      };

                      users.users.${elem.hostenv.userName} = {
                        uid = elem.uid;
                        group = elem.hostenv.userName;
                        openssh.authorizedKeys.keys = elem.authorizedKeys;
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
            base
            allEnvsWithUid;
        in
        pkgs.writers.writeJSON
          "plan.json"
          configAttrs;

      generatedState =
        let
          planState = builtins.listToAttrs
            (builtins.map
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
            } // lib.recursiveUpdate
              state
              planState;
        in
        pkgs.writers.writeJSON
          "state.json"
          mergedState;

    in
    assert (assertProjectInputs && assertDeployApiBase && assertDeployTokenFile);
    {
      flake = generatedFlakeFile;
      plan = generatedConfig;
      state = generatedState;
      environments = map (env: builtins.removeAttrs env [ "_providerService" ]) allEnvs;
    };
in
{
  config.flake.lib.provider.plan = providerPlan;
}
