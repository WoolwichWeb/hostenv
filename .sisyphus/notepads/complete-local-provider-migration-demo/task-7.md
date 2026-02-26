# Task 7: Generate and distribute comin node tokens

## Summary
Added comin node token generation to the local provider migration demo script.

## Changes Made

### File: examples/local-provider-migration/run-demo.sh

Added token generation step in `prepare_node_a_baseline()` function after `run_provider_plan`:

```bash
log "Generating comin node tokens"
(
  cd "$PROVIDER_DIR"
  nix run .#hostenv-provider -- comin-tokens
)
```

## How It Works

1. The `run_provider_plan` function generates `plan.json` which contains node names (node-a, node-b)
2. The `comin-tokens` command reads plan.json to identify nodes
3. For each node without a token, it generates a unique 64-character hex token using `openssl rand -hex 32`
4. Tokens are stored in `secrets/provider.yaml` under the `comin_node_tokens` key
5. The file is sops-encrypted with the age public key

## Configuration Already Present

The `nodeAuthTokenFile` path is already configured in `write_provider_flake()`:

```nix
comin = {
  enable = true;
  remoteUrl = "file:///mnt/hostenv-shared/provider-repo";
  providerApiBaseUrl = "http://10.0.2.2:${NODE_HTTP_PORT}";
  nodeAuthTokenFile = "/run/secrets/hostenv-provider/comin_node_token";
};
```

## Security

- Each node gets a unique token (not shared between nodes)
- Tokens stored in sops-encrypted YAML file (not in git)
- Tokens have 256 bits of entropy (64 hex characters)
- Provider service validates tokens against node names for authentication

## QA Verification

Tokens can be verified after running the demo:

```bash
# Decrypt and check for comin_node_tokens
sops -d secrets/provider.yaml | grep -q "comin_node_tokens"
sops -d secrets/provider.yaml | grep -q "node-a:"
sops -d secrets/provider.yaml | grep -q "node-b:"
```

## Dependencies

- Blocked by: Task 5 (provider initialization creates plan.json)
- Blocks: Task 9 (Wave 3 - nodes need tokens to authenticate)

## Status

✅ Completed 2025-02-25
