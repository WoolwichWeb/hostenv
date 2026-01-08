{
  description = "Hostenv: the PaaS you control";

  inputs = {
    nixpkgs.url = "github:liammcdermott/nixpkgs/hotfix/fix-composer-flags";
    flake-parts.url = "github:hercules-ci/flake-parts";
    import-tree.url = "github:vic/import-tree";
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
    flake-parts.lib.mkFlake { inherit inputs; }
      (inputs.import-tree ./modules);
}
