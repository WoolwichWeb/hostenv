# Provider "Just Work™" Initialization Plan

## Goal
Reduce provider setup from ~10+ manual steps to **1 step**: `nix flake init -t hostenv#provider` followed by minimal node configuration (as simple as a basic NixOS install).

## Current State Analysis

### Existing Manual Steps (from README and run-demo.sh)
1. Copy/init template (`nix flake init -t ...`)
2. Configure `provider.hostenvHostname` in flake.nix
3. Configure `provider.nodeFor` mappings
4. Copy/edit node stubs (`nodes/sample/` → `nodes/<node>/`)
5. Create `secrets/provider.yaml` with sops
6. Create `generated/state.json` (can be `{}` initially)
7. Add NixOS node configs with `system.stateVersion`
8. Run `nix run .#hostenv-provider -- plan` to generate `generated/{flake.nix,plan.json,state.json}`
9. **If using comin**: Set `provider.comin.enable = true` + remoteUrl + providerApiBaseUrl + nodeAuthTokenFile
10. **If using comin**: Set `provider.service.{organisation,project,environmentName}`
11. **If using comin**: Run `nix run .#hostenv-provider -- comin-tokens` to generate tokens

### Key Friction Points Identified
1. **Secrets creation is manual**: User must manually create `secrets/provider.yaml` with sops
2. **State file creation**: User must manually create `generated/state.json`
3. **Comin token generation is separate command**: Requires explicit `comin-tokens` invocation
4. **Multiple configuration locations**: Node configs scattered across `nodes/`, `flake.nix`, secrets
5. **No auto-detection of node configs**: User must manually wire node configurations

## Target State: "1-Step Provider"

### User Experience
```bash
# Step 1: Initialize
nix flake init -t gitlab:woolwichweb/hostenv#provider

# Step 2: Configure nodes (as simple as NixOS install)
# - Edit nodes/node-a/configuration.nix (copy from sample, set hostname, stateVersion)
# - Edit nodes/node-b/configuration.nix (if needed)
# - Set provider.hostenvHostname in flake.nix

# Step 3: Enter devshell → everything auto-generates
direnv allow  # or: nix develop
# - secrets/provider.yaml created if missing
# - generated/state.json created if missing  
# - generated/plan.json generated
# - comin tokens auto-generated if provider.comin.enable = true

# Step 4: Deploy
nix run .#hostenv-provider -- plan
# ... deploy commands
```

## Implementation Plan

### Phase 1: Auto-Generate Missing Files on Plan Command

**Goal**: Eliminate manual `secrets/provider.yaml` and `generated/state.json` creation.

**Changes**:
1. **provider/cli.hs** - Modify `runPlan` to:
   - Check if `secrets/provider.yaml` exists
   - If not, auto-generate with `sops --age <generated-key> --encrypt /dev/null` pattern
   - Generate ephemeral age key for initial encryption (user rotates later)
   - Check if `generated/state.json` exists, create `{}` if missing

2. **modules/entrypoints/provider/plan.nix** - Add options:
   - `provider.plan.autoInit = (lib.mkEnableOption "automatically setup necessary config for tracking state and secrets. Disable this if you would like hostenv to stop asking") // { default = true; }`

### Phase 2: Devshell Hook for Comin Token Auto-Generation

**Goal**: Hook comin token generation into provider devshell startup, mirroring `gitlab_token_key` UX.

**Changes**:
1. **modules/flake/devshells.nix** - Add provider-specific devshell hook:
   ```nix
   devshells.provider = {
     devshell.startup.comin-tokens = if config.provider.comin.enable
      then {
        text = ''
            if [[ -f generated/plan.json ]]; then
              # Check if tokens already exist
              if ! sops -d secrets/provider.yaml 2>/dev/null | jq -e '.comin_node_tokens' >/dev/null 2>&1; then
                if [[ -t 0 && -t 1 ]]; then
                  gum confirm --default=true \
                    # @FIXME: check if hostenv-provider comin-tokens opens the file for editing, if so this workflow is no good. If not how is the user supposed to edit their comin tokens?
                    "Generate comin node authentication tokens?" \
                    && nix run .#hostenv-provider -- comin-tokens
                else
                  echo "Run 'nix run .#hostenv-provider -- comin-tokens' to generate node tokens"
                fi
              fi
            fi
        '';
      }
      else {};
   };
   ```

2. **provider/cli.hs** - Enhance `runCominTokens`:
   - Add `--quiet` flag for devshell hook usage
   - Skip if tokens already present (idempotent)

### Phase 3: Simplified Node Configuration

**Goal**: Auto-discover node configs from `nodes/` directory, reduce boilerplate.

**Changes**:
1. **template/provider/flake.nix** - Add auto-discovery:
   ```nix
   provider.nodes = let
     nodeDirs = builtins.attrNames (builtins.readDir ./nodes);
   in lib.genAttrs nodeDirs (node: {
     configuration = ./nodes/${node}/configuration.nix;
     # hardware-configuration.nix is optional, auto-detected
   });
   ```

2. **modules/entrypoints/provider/options.nix** - Add:
   - `provider.nodes` option for declarative node configuration
   - Auto-generate `nodeFor` from nodes directory structure

### Phase 4: Smart Defaults and Validation

**Goal**: Sensible defaults that "just work" for common cases.

**Changes**:
1. **template/provider/flake.nix**:
   - Set `planSource = "eval"` as default (already done)
   - Auto-set `nodeSystems` based on current system if single node
   - Auto-detect `hostenvHostname` from `hostname -f` or git remote

2. **modules/nixos/provider-common.nix**:
   - Better error messages guiding user to missing config
   - Validate node configs exist before plan generation

## Work Breakdown Structure

### Wave 1: Foundation (Foundation Layer)
- Task 1: Auto-generate secrets/provider.yaml in plan command
- Task 2: Auto-generate generated/state.json in plan command
- Task 3: Add provider.plan.autoInit option

### Wave 2: Devshell Integration (UX Layer)
- Task 4: Add comin-tokens startup hook to provider devshell
- Task 5: Enhance CLI with --quiet flag for comin-tokens
- Task 6: Add TTY detection and gum prompts (mirror gitlab_token_key flow)

### Wave 3: Auto-Discovery (Configuration Layer)
- Task 7: Implement provider.nodes auto-discovery from nodes/
- Task 8: Auto-generate nodeFor from nodes directory
- Task 9: Update template/provider/flake.nix with new patterns

### Wave 4: Polish (Documentation & Testing)
- Task 10: Update provider README with new simplified flow
- Task 11: Update provider-quickstart.md
- Task 12: Add integration tests for auto-init flow
- Task 13: Add tests for devshell hook

## Key Design Decisions

### 1. Secrets Auto-Generation Strategy
- **Approach**: Generate ephemeral age key for initial secrets encryption
- **Rationale**: User can rotate to proper recipients later; unblocks immediate usage
- **Security**: Secrets are encrypted at rest from moment of creation

### 2. Devshell Hook vs Explicit Command
- **Approach**: Hook into devshell startup with interactive prompt
- **Rationale**: Mirrors existing `gitlab_token_key` UX pattern users already know
- **Fallback**: Non-TTY shows informational message with manual command

### 3. Node Auto-Discovery
- **Approach**: Read `nodes/` directory at flake evaluation time
- **Rationale**: Zero config for common case; explicit config still possible
- **Limitation**: Requires nodes to follow convention (nodes/<name>/configuration.nix)

## Success Criteria

- [ ] New provider can be initialized with `nix flake init` + 2-3 config edits
- [ ] Entering devshell auto-generates missing secrets/state
- [ ] Comin tokens auto-generate on devshell entry when comin enabled
- [ ] All existing tests pass
- [ ] New integration tests verify auto-init flow
- [ ] Documentation updated with simplified quickstart

## Files to Modify

### Core Implementation
- `provider/cli.hs` - Auto-init logic, --quiet flag
- `modules/flake/devshells.nix` - Provider devshell with hook
- `modules/entrypoints/provider/plan.nix` - Auto-init options
- `modules/entrypoints/provider/options.nix` - provider.nodes option

### Template Updates
- `template/provider/flake.nix` - Auto-discovery patterns
- `template/provider/README.md` - Simplified instructions

### Documentation
- `docs/provider-quickstart.md` - Updated quickstart flow

## Risk Mitigation

| Risk | Mitigation |
|------|------------|
| Auto-generated secrets not secure enough | Document rotation workflow; use ephemeral keys only for bootstrap |
| Devshell hook too intrusive | Make autoInit options toggleable; default to prompting not auto-execution |
| Node auto-discovery breaks custom layouts | Keep explicit `provider.nodes` as override; auto-discovery is default-off initially |
| Backward compatibility | We are early in development; backward compatibility is not a concern |

## Next Steps

1. **Immediate**: Create detailed spec for Phase 1 (auto-init secrets/state)
2. **Parallel**: Draft devshell hook implementation (Phase 2)
3. **Follow-up**: Prototype node auto-discovery (Phase 3)
4. **Finally**: Integration testing and documentation (Phase 4)

---

*Plan created for: feat/oauth-secure-deploy-tokens branch*
*Target: Reduce provider setup to 1 initialization step + minimal node config*
