# CSP submodule for src/modules/environments.nix under `environments.<name>.virtualHosts.<name>.csp`
{ lib, config, ... }:

let
  inherit (lib)
    mkOption mkEnableOption types mkIf mkDefault mkMerge
    concatStringsSep optionalString unique genAttrs mapAttrs optionals;

  # Single source of truth for directive identifiers and their header spellings
  directiveNames = [
    "defaultSrc"
    "scriptSrc"
    "styleSrc"
    "imgSrc"
    "fontSrc"
    "connectSrc"
    "mediaSrc"
    "frameSrc"
    "workerSrc"
    "childSrc"
    "manifestSrc"
    "baseUri"
    "formAction"
    "frameAncestors"
  ];

  headerNameOf = {
    defaultSrc = "default-src";
    scriptSrc = "script-src";
    styleSrc = "style-src";
    imgSrc = "img-src";
    fontSrc = "font-src";
    connectSrc = "connect-src";
    mediaSrc = "media-src";
    frameSrc = "frame-src";
    workerSrc = "worker-src";
    childSrc = "child-src";
    manifestSrc = "manifest-src";
    baseUri = "base-uri";
    formAction = "form-action";
    frameAncestors = "frame-ancestors";
  };

  # Helpers
  esc = s: lib.replaceStrings
    [ "\\" "\"" "\n" "\r" "\t" ]
    [ "\\\\" "\\\"" " " " " " " ]
    s;
  join = xs: concatStringsSep " " xs;
  emit = name: xs: if xs == [ ] then "" else "${name} ${join (unique xs)}";

  # Build Report-To JSON header value
  mkReportToHeader = groups:
    let
      toGroupObj = g: {
        group = g.group;
        max_age = g.maxAge;
        endpoints = map (u: { url = u; }) g.endpoints;
      } // (if g.includeSubdomains then { include_subdomains = true; } else { });
      objs = map toGroupObj groups;
    in
    if objs == [ ] then "" else concatStringsSep ", " (map builtins.toJSON objs);

  # Helper to build CSP header - takes config as argument to avoid infinite recursion
  buildCspHeader = config:
    let
      # Resolve flags (null → false unless profile sets mkDefault)
      flags = mapAttrs (_: v: if v == null then false else v) config.flags;
      inherit (flags)
        allowUnsafeInlineScripts allowUnsafeEval
        objectSrcNone upgradeInsecureRequests blockAllMixedContent;
      useToken = flags.useToken;

      # Final directive lists (merged with mkDefault baselines from profile)
      baseLists = genAttrs
        directiveNames
        (n: config.directives.${n});

      # Tokens derived from flags (always appended; users can disable flags
      # or mkForce their directive if they want total control)
      scriptTokens =
        optionals allowUnsafeInlineScripts [ "'unsafe-inline'" ]
        ++ optionals allowUnsafeEval [ "'unsafe-eval'" ]
        ++ optionals useToken [ "'strict-dynamic'" ("'nonce-$" + config.tokenVar + "'") ];

      styleTokens =
        optionals (config.styleTokenVar != null) [ ("'nonce-$" + config.styleTokenVar + "'") ];

      withTokens = baseLists // {
        scriptSrc = unique (baseLists.scriptSrc ++ scriptTokens);
        styleSrc = unique (baseLists.styleSrc ++ styleTokens);
      };

      # Reporting directives
      reportToNames =
        if config.reporting.reportTo != [ ] then config.reporting.reportTo
        else if config.reporting.useFirstGroupInDirective && config.reporting.reportToGroups != [ ] then
          [ (builtins.elemAt config.reporting.reportToGroups 0).group ]
        else [ ];
      reportUris = config.reporting.reportUri;

      # Emit non-empty directives in a stable, human-friendly order
      directiveParts =
        map (n: emit headerNameOf.${n} (withTokens.${n} or [ ])) directiveNames;

      extraParts = [
        (optionalString objectSrcNone "object-src 'none'")
        (optionalString upgradeInsecureRequests "upgrade-insecure-requests")
        (optionalString blockAllMixedContent "block-all-mixed-content")
        (if reportToNames == [ ] then "" else "report-to ${join (unique reportToNames)}")
        (if reportUris == [ ] then "" else "report-uri ${join (unique reportUris)}")
      ] ++ config.extra.rawDirectives;

      parts = lib.filter (s: s != "") (directiveParts ++ extraParts);
    in
    esc (concatStringsSep "; " parts);

  # --- profile presets (single place) ----------------------------------------
  baseDefaults = {
    defaultSrc = [ "'self'" "https:" "data:" "blob:" ];
    scriptSrc = [ "'self'" "https:" "blob:" ];
    styleSrc = [ "'self'" "https:" ];
    imgSrc = [ "*" "data:" "blob:" ];
    fontSrc = [ "'self'" "https:" "data:" ];
    connectSrc = [ "'self'" "https:" "wss:" ];
    mediaSrc = [ "*" "data:" "blob:" ];
    frameSrc = [ "*" "https:" ];
    workerSrc = [ "'self'" "blob:" ];
    manifestSrc = [ "'self'" "https:" ];
    # baseUri, formAction, frameAncestors default to []
  };
  withFormDefaults = baseDefaults // {
    baseUri = [ "'self'" ];
    formAction = [ "'self'" ];
  };

  mkDefaults = ds: genAttrs directiveNames (n: mkDefault (ds.${n} or [ ]));
  mkFlagDefaults = fs: mapAttrs (_: v: mkDefault v) fs;

  compatFlags = {
    allowUnsafeInlineScripts = true;
    allowUnsafeEval = true;
    useToken = false;
    objectSrcNone = false;
    upgradeInsecureRequests = false;
    blockAllMixedContent = false;
  };
  balancedFlags = compatFlags // {
    allowUnsafeInlineScripts = false;
    allowUnsafeEval = false;
    objectSrcNone = true;
  };
  tokenFlags = balancedFlags // { useToken = true; };

  profileDefs = {
    compat = {
      directives = mkDefaults baseDefaults;
      flags = mkFlagDefaults compatFlags;
    };
    balanced = {
      directives = mkDefaults withFormDefaults;
      flags = mkFlagDefaults balancedFlags;
    };
    token = {
      directives = mkDefaults withFormDefaults;
      flags = mkFlagDefaults tokenFlags;
    };
  };

in
{
  options = {
    enable = mkEnableOption "Content-Security-Policy header generation";

    profile = mkOption {
      type = types.enum [ "compat" "balanced" "token" ];
      default = "compat";
      description = ''
        - compat: permissive (inline/eval allowed), wide HTTPS/CDN
        - balanced: tighter defaults (object-src 'none', base/form 'self')
        - token: like balanced but defaults to token-based scripts

        Note: 'nonce' has very unfortunate connotations in British English
        so 'token' is used instead.
      '';
    };

    reportOnly = mkOption { type = types.bool; default = false; };

    # Names of nginx variables (without $) for token injection
    tokenVar = mkOption { type = types.str; default = "csp_token"; };
    styleTokenVar = mkOption { type = types.nullOr types.str; default = null; };

    # Per-directive options.
    directives = genAttrs directiveNames (name: mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = ''
        Sources for the ${headerNameOf.${name}} directive.
        Omit (or leave default) to inherit from the selected profile.
        Set to `[]` to explicitly omit this directive.
        Override to set custom sources.
        Use `lib.mkForce []` to force-disable a profile's directive.
      '';
      example = [ "'self'" "https://cdn.example.com" ];
    });

    # Token/behaviour flags (converted to CSP tokens; set to false to stop injection)
    flags = {
      useToken = mkOption { type = types.nullOr types.bool; default = null; };
      allowUnsafeInlineScripts = mkOption { type = types.nullOr types.bool; default = null; };
      allowUnsafeEval = mkOption { type = types.nullOr types.bool; default = null; };
      objectSrcNone = mkOption { type = types.nullOr types.bool; default = null; };
      upgradeInsecureRequests = mkOption { type = types.nullOr types.bool; default = null; };
      blockAllMixedContent = mkOption { type = types.nullOr types.bool; default = null; };
    };

    # Rare extras appended verbatim (e.g., "prefetch-src https:")
    extra.rawDirectives = mkOption { type = types.listOf types.str; default = [ ]; };

    reporting = {
      reportTo = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "List of report-to group names to use in CSP directive";
      };

      useFirstGroupInDirective = mkOption {
        type = types.bool;
        default = false;
        description = "Automatically use the first reportToGroups entry in the report-to directive";
      };

      reportToGroups = mkOption {
        type = types.listOf (types.submodule {
          options = {
            group = mkOption { type = types.str; };
            maxAge = mkOption { type = types.int; };
            endpoints = mkOption { type = types.listOf types.str; };
            includeSubdomains = mkOption { type = types.bool; default = false; };
          };
        });
        default = [ ];
        description = "Report-To header group definitions";
      };

      reportUri = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Deprecated report-uri endpoints";
      };
    };

    assertions = lib.mkOption {
      type = types.listOf types.unspecified;
      internal = true;
      default = [ ];
      description = ''
        See `src/modules/top-level/full-env.nix and the
        description of assertions therein.
      '';
    };

  };

  config = mkIf config.enable (mkMerge [
    # Apply profile defaults. Uses mkIf (as opposed to
    # `profileDefs.${config.profile}`) to avoid infinite recursion errors.
    (mkIf (config.profile == "compat") profileDefs.compat)
    (mkIf (config.profile == "balanced") profileDefs.balanced)
    (mkIf (config.profile == "token") profileDefs.token)

    {
      assertions =
        let
          flags = mapAttrs (_: v: if v == null then false else v) config.flags;
        in
        [
          {
            assertion = flags.useToken -> config.tokenVar != "";
            message = "CSP useToken flag requires a non-empty tokenVar";
          }
          {
            assertion = !(flags.allowUnsafeInlineScripts && flags.useToken);
            message = "CSP cannot use both unsafe-inline and token (strict-dynamic)";
          }
        ];
    }
  ]);
}
