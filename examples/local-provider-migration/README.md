# Local Provider Migration Demo

This example is a local, end-to-end Hostenv walkthrough that runs on Linux with KVM and demonstrates:

1. Bootstrapping a Hostenv provider locally.
2. Deploying a Drupal environment to `node-a`.
3. Importing a Drupal seed database via project dev shell tooling.
4. Migrating the same environment to `node-b`.
5. Verifying migrated data on `node-b`.

## Prerequisites

- Linux host with `/dev/kvm`
- Nix with flakes enabled
- Commands: `nix`, `git`, `jq`, `ssh`, `ssh-keygen`, `sops`, `age-keygen`, `curl`, `pv`, `gunzip`, `hostctl`
- Wizard UI uses `gum`; if `gum` or `hostctl` are missing, the script re-runs itself via `nix shell` to provide them.
- `hostctl` needs permission to edit `/etc/hosts` (root or `sudo`).

## Run (Wizard)

From repo root:

```bash
./examples/local-provider-migration/run-demo.sh
```

The script:
- Starts local VM `node-a`.
- Configures a temporary `hostctl` profile for local demo hostnames.
- Creates `./hostenv-demo` symlink to its working directory.
- Generates `./hostenv-demo/flake.nix` so you can enter the demo environment with `cd ./hostenv-demo && nix develop`.
- Pauses between stages and gives copy/paste commands.
- Lets you press `<Enter>` to continue or press `a` to abort and clean up.

## Run (Automated)

```bash
./examples/local-provider-migration/run-demo.sh --automated --cleanup
```

This is the non-interactive end-to-end path suitable for local regression runs.

## Teardown

If a prior run crashed or was interrupted:

```bash
./examples/local-provider-migration/run-demo.sh --teardown
```

To tear down one specific workspace:

```bash
./examples/local-provider-migration/run-demo.sh --teardown --workdir /tmp/hostenv-local-vm-demo-XXXXXX
```

## Notes

- The wizard demonstrates real provider commands (`plan`, `dns-gate`, `deploy`) while keeping setup details in the script.
- Migration in this demo uses an explicit source override (`--migration-source <env>=node-a`) so the flow does not depend on committing `generated/state.json` timing.
- DB seed source file is `examples/local-provider-migration/seed/seed.sql.gz`.
- Teardown/abort removes the temporary `hostctl` profile alongside VMs and demo workspace files.
- On failure, check VM logs under `<workdir>/logs` and open a GitLab issue if needed.
