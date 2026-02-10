{
  description = "Hostenv: the PaaS you control";

  inputs = {
    nixpkgs.url = "github:liammcdermott/nixpkgs/hotfix/fix-composer-flags";
    flake-parts.url = "github:hercules-ci/flake-parts";
    import-tree.url = "github:vic/import-tree";
    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    search = {
      url = "github:NuschtOS/search";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pog = {
      url = "github:jpetrucciani/pog";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    phps = {
      url = "github:fossar/nix-phps";
    };
    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ flake-parts, ... }:
    let
      modules = inputs.import-tree ./modules;
      moduleList = if builtins.isList modules then modules else [ modules ];
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ inputs.devshell.flakeModule ] ++ moduleList;
    };

    #
    # Hostenv uses flake-parts, which is great since it means everything
    # is a module, and modules are the best thing about Nix! But it also
    # sucks, as it means things we're used to seeing in a flake.nix aren't
    # here, where we expect.
    #
    # So, you may be looking for:
    #
    # devShells → modules/flake/devshells.nix
    # packages → there's no single file for packages, but an example is
    #            modules/flake/docs.nix
    # apps → there's no single file for apps, but an example is
    #        modules/flake/docs.nix
    #
}
