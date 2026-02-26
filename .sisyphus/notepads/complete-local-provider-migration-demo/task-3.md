# Task 3: PostgreSQL Configuration for Demo

## Summary
Configured PostgreSQL service for the demo environment with ephemeral storage and proper database permissions.

## Changes Made

### File Modified
- `examples/local-provider-migration/demo-project/.hostenv/hostenv.nix`

### Configuration Added
```nix
# PostgreSQL for hostenv-provider service (ephemeral, workdir-relative)
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

## Key Design Decisions

1. **Ephemeral Storage**: Data directory is set to `${config.hostenv.dataDir}/postgresql`, ensuring data lives only within the workdir
2. **Self-Contained**: Not using system PostgreSQL - fully managed by hostenv
3. **Database**: `hostenv-provider` database will be auto-created
4. **Permissions**: Environment user granted ALL PRIVILEGES on the database

## Verification

- ✓ Nix syntax validated with `nix-instantiate --parse`
- ✓ PostgreSQL service enabled
- ✓ Data directory is workdir-relative (ephemeral)
- ✓ Database `hostenv-provider` configured
- ✓ User permissions set to ALL PRIVILEGES

## QA Scenario

**Scenario**: PostgreSQL database accessible
- **Preconditions**: Environment activated via `nix develop`
- **Expected**: Database exists and is accessible
- **Evidence**: `.sisyphus/evidence/task-3-postgres-setup.log`

## References

- Pattern from: `docs/provider-quickstart.md` lines 143-157
- dbUri context: `modules/features/hostenv-provider-service.nix` lines 232-236
