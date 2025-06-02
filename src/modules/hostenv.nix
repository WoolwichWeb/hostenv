# Basic Hostenv configuration: paths, project hash.
{ config, lib, pkgs, ... }:
let
  cfg = config.hostenv;

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

      slug = buildSlug 12;
    in
    lib.toLower (
      if slug != "" then slug else lib.concatStringsSep "-" (lib.take 1 words)
    );

  slugHash = builtins.hashString "sha256" (
    # "\n" is so our result matches
    # `echo "org-project-environment" | sha256sum` in bash.
    # This is for historical reasons: I started by creating environments
    # manually, using hashes generated from the command-line, while forgetting
    # the sublty that echo appends "\n" to every string.
    lib.concatStringsSep "-" [ cfg.organisation cfg.project cfg.environmentName ] + "\n"
  );
in
{
  config.hostenv = {
    userName = lib.mkForce (
      lib.concatStringsSep "-"
        (
          builtins.map slugify [ cfg.project cfg.environmentName ]
        ) + "-" + lib.substring 0 7 slugHash
    );

    projectNameHash = lib.mkForce slugHash;
    runtimeDir = lib.mkForce "/run/hostenv/user/${cfg.userName}";
    upstreamRuntimeDir = lib.mkForce "/run/hostenv/nginx/${cfg.userName}";
    dataDir = lib.mkForce "/home/${cfg.userName}/.local/share";
    stateDir = lib.mkForce "/home/${cfg.userName}/.local/state";
    cacheDir = lib.mkForce "/home/${cfg.userName}/.cache";
    backupsSecret = lib.mkForce "/run/secrets/${cfg.userName}/backups_secret";
  };
}
