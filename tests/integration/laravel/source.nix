{ pkgs }:
let
  mkLaravelSource = { name, rev, hash, lockFile }:
    let
      upstream = pkgs.fetchFromGitHub {
        owner = "laravel";
        repo = "laravel";
        inherit rev hash;
      };
    in
    pkgs.runCommand "${name}-test-source"
      { nativeBuildInputs = [ pkgs.jq ]; }
      ''
        mkdir -p "$out"
        cp -a ${upstream}/. "$out/"
        chmod -R u+w "$out"

        # Keep fixtures small while retaining the real framework dependency
        # graph and application skeleton for this Laravel major.
        jq 'del(."require-dev", ."autoload-dev")' "$out/composer.json" > "$out/composer.json.tmp"
        mv "$out/composer.json.tmp" "$out/composer.json"
        install -m 0644 ${lockFile} "$out/composer.lock"

        printf '%s\n' "${name} initial storage" > "$out/storage/hostenv-fixture.txt"
      '';
in
{
  laravel10 = mkLaravelSource {
    name = "laravel10";
    rev = "d3287461e15862d1c7a8f10925988b4f1640d92b"; # v10.3.3 skeleton
    hash = "sha256-OdkN7UG3vDsJhqPiMISFjYuaeRMPkHs/RV0XKXuteVg=";
    lockFile = ./locks/laravel10.lock;
  };

  laravel11 = mkLaravelSource {
    name = "laravel11";
    rev = "f9f5e3c3ae0b9e536ddc690aae14032557956449"; # v11.3.1 skeleton
    hash = "sha256-1N2Dtp/0H5pq4L8Sr9TtS/ghx9C7RB7aaw/DD7hf4ls=";
    lockFile = ./locks/laravel11.lock;
  };

  laravel12 = mkLaravelSource {
    name = "laravel12";
    rev = "181249000391597d80b872169680f3921e951928"; # v12.10.1 skeleton
    hash = "sha256-Z+FZevUrL6PBUkPlN9K0iG1aoiuM1oAbKVbiAWv8veo=";
    lockFile = ./locks/laravel12.lock;
  };
}
