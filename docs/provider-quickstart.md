# Provider Quickstart (hostenv)

1) Add hostenv as an input:
```nix
{
  inputs.hostenv.url = "gitlab:woolwichweb/hostenv";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
 outputs = inputs@{ self, flake-parts, hostenv, ... }:
   flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ hostenv.lib.hostenv.providerModule ];
      systems = [ "x86_64-linux" "aarch64-linux" ];
      perSystem = { config, ... }: {
        provider = {
          hostenvHostname = "hostenv.sh";
          nodeSystems = { backend01 = "aarch64-linux"; backend02 = "aarch64-linux"; backend03 = "x86_64-linux"; backend04 = "aarch64-linux"; };
          nodesPath = ./nodes;
          secretsPath = ./secrets/secrets.yaml;
          planSource = "eval"; # or "disk"
        };
      };
    };
}
```

**Important:** each client project flake must export `outputs.hostenv.<system>.environments`
so the provider can discover environments. The shipped project template already does this.
Client inputs should point at the `.hostenv` flake (e.g. `dir=.hostenv`) so `hostenv.nix` is at the flake root.

2) Populate nodes and secrets:
   - Copy `nodes/sample` to `nodes/<node>/` and edit `configuration.nix`/`hardware-configuration.nix`.
   - Create `secrets/secrets.yaml` with sops.
   - Create `generated/state.json` (can be `{}` initially).

3) Generate plan/state (optional if using planSource=eval):
```
nix run .#hostenv-provider plan
```

Why the generated `flake.nix` exists: flake inputs are static, but your client
repos can have many environments (often one per branch/tag). Plan generation
materialises a new flake whose inputs enumerate each environment (repo × env),
so deploy-rs can build the exact activation packages and NixOS systems without
re-evaluating the dynamic hostenv graph. The bundle `generated/{plan.json,
state.json,flake.nix}` is the deployable, auditable snapshot.

4) DNS/ACME safety + Cloudflare (optional):
```
CF_API_TOKEN=... CF_ZONE_ID=... nix run .#hostenv-provider dns-gate [-n node]
```

5) Deploy:
```
nix run .#hostenv-provider deploy [-n node]
```

Outputs:
- `packages.deploy-nodes` / `packages.deploy-envs` per system when a plan exists.
- Deploy specs live at `lib.hostenv.deploySpec` (per flake output). Example:
  `nix eval .#lib.hostenv.deploySpec --json | jq` (planSource=eval recommended).

Optional features to toggle in environments:
- `env.extras.backups` for restic (repo/password/env files, timer, data/state dirs)
- `env.extras.nginx` for HSTS/CSP/headers/aliases
- `hostenv.monitoring.enable` to turn on basic exporters/labels
