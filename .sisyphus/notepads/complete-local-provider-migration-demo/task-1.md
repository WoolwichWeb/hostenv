# Task 1: Integrate provider-service-dev runner into demo

## Summary
Successfully integrated `hostenv-provider-service-dev` into the local provider migration demo.

## Changes Made

### 1. Modified `examples/local-provider-migration/run-demo.sh`

#### Added wrapper script generation (`write_demo_shell_flake` function)
- Creates `${WRAPPER_DIR}/hostenv-provider-service-dev` script that:
  - Initializes PostgreSQL database in `$HOSTENV_PROVIDER_DEV_DIR/pgdata`
  - Creates dummy GitLab OAuth secrets (`client_id=dev`, `client_secret=dev`)
  - Creates token encryption key
  - Generates provider-config.json with dev mode settings
  - Starts PostgreSQL and the provider service
  - Listens on Unix socket at `$HOSTENV_PROVIDER_DEV_DIR/hostenv-provider.sock`

#### Updated demo shell flake
- Changed from simple `mkShellNoCC` to full flake-parts module
- Imports `hostenv.flakeModules.provider`
- Enables `provider.enable = true`
- Adds `config.packages.hostenv-provider-service` to devShell packages
- Includes additional tools: `postgresql`, `socat`
- Sets environment variables:
  - `HOSTENV_PROVIDER_DEV_DIR=${WORKDIR}/provider-dev`
  - `HOSTENV_PROVIDER_DATA_DIR=${WORKDIR}/provider-dev/data`

## Environment Variables

The following environment variables are configured in the devShell:

| Variable | Default Value | Description |
|----------|---------------|-------------|
| `HOSTENV_PROVIDER_DEV_DIR` | `${WORKDIR}/provider-dev` | Base directory for provider dev mode |
| `HOSTENV_PROVIDER_DATA_DIR` | `${WORKDIR}/provider-dev/data` | Data directory (git repo) |
| `HOSTENV_PROVIDER_LISTEN_SOCKET` | `$base/hostenv-provider.sock` | Unix socket path |
| `HOSTENV_PROVIDER_WEBHOOK_HOST` | `localhost` | Webhook host |
| `HOSTENV_PROVIDER_UI_BASE_URL` | `http://localhost` | UI base URL |
| `HOSTENV_PROVIDER_GITLAB_SECRETS_FILE` | `$base/gitlab_oauth` | OAuth secrets file |
| `HOSTENV_PROVIDER_GITLAB_TOKEN_KEY_FILE` | `$base/gitlab_token_key` | Token encryption key |

## Dummy Secrets (Dev Mode)

The wrapper script automatically creates:

1. **GitLab OAuth secrets** (`gitlab_oauth`):
   ```
   client_id=dev
   client_secret=dev
   ```

2. **Token encryption key** (`gitlab_token_key`):
   ```
   key=0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef
   ```

## QA Results

### Scenario 1: Service starts successfully
- **Status**: PASSED
- **Evidence**: `.sisyphus/evidence/task-1-service-start.log`
- Unix socket created at `$WORKDIR/provider-dev/hostenv-provider.sock`
- Service logs show: `hostenv-provider-service: listening`

### Scenario 2: PostgreSQL initialized
- **Status**: PASSED
- **Evidence**: `.sisyphus/evidence/task-1-postgres.log`
- PG_VERSION: 17
- Database `hostenv-provider` created
- PostgreSQL listening on Unix socket

## Files Modified
- `examples/local-provider-migration/run-demo.sh`

## Verification Commands

```bash
# Enter dev shell
cd $WORKDIR && nix develop

# Start provider service in dev mode
hostenv-provider-service-dev &

# Check Unix socket exists
test -S $WORKDIR/provider-dev/hostenv-provider.sock

# Check PostgreSQL data
cat $WORKDIR/provider-dev/pgdata/PG_VERSION
```

## Notes

- The provider service requires a git repository in the data directory
- PostgreSQL data is ephemeral (stored in workdir)
- No real GitLab OAuth - uses dummy dev mode secrets
- Service binds to Unix socket, not TCP port (as per hostenv constraints)
