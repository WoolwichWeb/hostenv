# Task 2: Configure provider-service in demo environment

## Summary
Successfully configured the provider-service module in the demo environment's hostenv.nix configuration file.

## Changes Made

### File Modified
- `examples/local-provider-migration/demo-project/.hostenv/hostenv.nix`

### Configuration Added

1. **PostgreSQL Service** (workdir-relative paths):
   ```nix
   services.postgresql = {
     enable = true;
     user = config.hostenv.userName;
     dataDir = "${config.hostenv.dataDir}/postgresql";
     runtimeDir = config.hostenv.runtimeDir;
     ensureDatabases = [ "hostenv-provider" ];
     ensureUsers = [
       {
         name = config.hostenv.userName;
         ensurePermissions = { "hostenv-provider" = "ALL PRIVILEGES"; };
       }
     ];
   };
   ```

2. **Provider-service Configuration**:
   ```nix
   services.hostenv-provider = {
     enable = true;
     webhookHost = "demo.hostenv.test";
     uiBasePath = "/dashboard";
     # GitLab OAuth disabled for demo - uses dev mode
   };
   ```

## Verification Results

| Check | Status | Details |
|-------|--------|---------|
| Provider-service enabled | PASS | `services.hostenv-provider.enable = true` |
| PostgreSQL workdir-relative | PASS | Uses `config.hostenv.dataDir` |
| Webhook host | PASS | Set to `demo.hostenv.test` |
| UI base path | PASS | Set to `/dashboard` |
| GitLab OAuth | PASS | Not enabled (demo simplicity) |

## Evidence
- Evidence file: `.sisyphus/evidence/task-2-config-check.txt`

## Notes
- Configuration follows patterns from `docs/provider-quickstart.md` lines 143-168
- Uses module options from `modules/features/hostenv-provider-service.nix` lines 156-260
- GitLab OAuth intentionally disabled for demo simplicity (uses dev mode)
- PostgreSQL data directory is ephemeral (in workdir) as required for demo

## Commit Message
```
feat(demo): configure provider-service in demo environment
```
