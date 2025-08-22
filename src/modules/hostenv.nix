# Basic Hostenv configuration: paths, project hash.
{ lib, config, ... }:
let
  types = lib.types;

  # Replace non-alpha characters with a hyphen
  sanitise = str:
    lib.strings.stringAsChars
      (c:
        if builtins.match "[[:alnum:]]" c != null || c == "-" then c else "-"
      )
      str;

  # Collapse multiple hyphens, trim leading/trailing ones
  cleanDashes = str:
    let
      collapsed = builtins.replaceStrings [ "--" "---" "----" "-----" ] [ "-" "-" "-" "-" ] str;
      trimmed = lib.removeSuffix "-" (lib.removePrefix "-" collapsed);
    in
    trimmed;

  slugify = s:
    let
      lastSegment = lib.last (lib.splitString "/" s);
      sanitised = cleanDashes (sanitise lastSegment);
      words = lib.splitString "-" sanitised;

      buildSlug = maxLen:
        let
          go = idx: acc:
            if idx >= builtins.length words then acc
            else
              let
                next = builtins.elemAt words idx;
                candidate = lib.concatStringsSep "-" (acc ++ [ next ]);
              in
              if lib.stringLength candidate > maxLen
              then acc
              else go (idx + 1) (acc ++ [ next ]);

          resultWords = go 0 [ ];
        in
        if resultWords != [ ]
        then lib.concatStringsSep "-" resultWords
        else lib.substring 0 maxLen sanitised;

      slug = buildSlug 11;
    in
    lib.toLower (
      if slug != "" then slug else lib.concatStringsSep "-" (lib.take 1 words)
    );

in
{
  options = {
    hostenvHostname = lib.mkOption {
      type = types.str;
      description = "Top-level hostname for Hostenv.";
      example = "yourcompany.com";
      default = "hostenv.sh";
      internal = true;
    };
    organisation = lib.mkOption {
      type = types.str;
      description = "Business account name or organisation name of the project. Should be short, lowercase, and with no special characters.";
      example = "fooinc";
    };
    project = lib.mkOption {
      type = types.str;
      description = "Name of the project. Should be short, lowercase, and contain no special characters.";
      example = "coolproject";
    };
    environmentName = lib.mkOption {
      type = types.str;
      description = "Name of the current environment. Usually corresponds to a git branch, but can be something else, e.g. an MR slug or number. Should be short, lowercase, and with no special characters.";
    };
    safeEnvironmentName = lib.mkOption {
      type = types.str;
      description = "Name of the current environment, shortened and with special characters removed.";
    };
    gitRef = lib.mkOption {
      type = types.str;
      description = "Git branch or tag of this environment. Defaults to the `environmentName`.";
      example = "main";
    };
    userName = lib.mkOption {
      type = types.str;
      description = "UNIX username (on server) of this project.";
    };
    hostname = lib.mkOption {
      type = types.str;
      description = "Server hostname for this project and environment, as a fully qualified domain name (FQDN).";
      example = "hostenv-main-7c25553.hostenv.sh";
    };
    root = lib.mkOption {
      type = types.oneOf [ types.str types.path ];
      description = "The application's root directory.";
    };
    runtimeDir = lib.mkOption {
      type = types.str;
      description = "Path (on server) where hostenv sockets and pid files may be found.";
      example = lib.literalExpression "/run/hostenv/user/\${config.hostenv.userName}";
    };
    upstreamRuntimeDir = lib.mkOption {
      type = types.str;
      description = "Path (on server) where upstream reverse proxy socket may be found.";
      example = lib.literalExpression "/run/hostenv/nginx/\${config.hostenv.userName}";
    };
    dataDir = lib.mkOption {
      type = types.str;
      description = "Path (on server) where data may be found at runtime.";
      example = lib.literalExpression "/home/\${config.hostenv.userName}/.local/share";
    };
    stateDir = lib.mkOption {
      type = types.str;
      description = "Path (on server) where state data may be found at runtime.";
      example = lib.literalExpression "/home/\${config.hostenv.userName}/.local/state";
    };
    cacheDir = lib.mkOption {
      type = types.str;
      description = "Path (on server) where cache data may be found at runtime.";
      example = lib.literalExpression "/home/\${config.hostenv.userName}/.cache";
    };
    backupsSecretFile = lib.mkOption {
      type = types.str;
      description = "Path (on server) where secret access key for accessing backups may be found.";
    };
    backupsEnvFile = lib.mkOption {
      type = types.str;
      description = "Path (on server) where an environment file with information related to accessing backups may be found.";
    };
    authorizedKeys = lib.mkOption {
      type = types.listOf types.singleLineStr;
      description = ''
        A list of verbatim OpenSSH public keys that should be added to the
        environment's authorized keys.
      '';
      default = [ ];
    };
    projectNameHash = lib.mkOption {
      type = types.str;
      description = "Hash of organisation, project, and environment names.";
      internal = true;
    };
  };

  config =
    let
      slugHash = builtins.hashString "sha256" (
        # "\n" is so our result matches
        # `echo "org-project-environment" | sha256sum` in bash.
        # This is for historical reasons: I started by creating environments
        # manually, using hashes generated from the command-line, while forgetting
        # the sublty that echo appends "\n" to every string.
        lib.concatStringsSep "-" [ config.organisation config.project config.environmentName ] + "\n"
      );

      shortName = lib.concatStringsSep "-"
        (
          builtins.map slugify [ config.project config.environmentName ]
        ) + "-" + lib.substring 0 7 slugHash;
    in
    {
      userName = lib.mkForce shortName;
      hostname = lib.mkForce "${shortName}.${config.hostenvHostname}";
      safeEnvironmentName = lib.mkForce (cleanDashes (sanitise config.environmentName));
      # Note: we use environmentName and not safeEnvironmentName as the latter
      # is stripped of some characters that are valid in git branch names,
      # '/' and '--' for example.
      gitRef = lib.mkForce (config.environmentName or "main");
      runtimeDir = lib.mkForce "/run/hostenv/user/${config.userName}";
      upstreamRuntimeDir = lib.mkForce "/run/hostenv/nginx/${config.userName}";
      dataDir = lib.mkForce "/home/${config.userName}/.local/share";
      stateDir = lib.mkForce "/home/${config.userName}/.local/state";
      cacheDir = lib.mkForce "/home/${config.userName}/.cache";
      backupsSecretFile = lib.mkForce "/run/secrets/${config.userName}/backups_secret";
      backupsEnvFile = lib.mkForce "/run/secrets/${config.userName}/backups_env";
      authorizedKeys = lib.mkForce config.authorizedKeys;
      projectNameHash = lib.mkForce slugHash;
    };
}
