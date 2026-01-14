# Hostenv deployments missing state

## Problem

When deploying hostenv environments to a new machine those environments lack state. That is things like databases and file uploads. If the environment exists on another machine, that state remains on that machine.

## Solution

This requires multiple changes to hostenv:

1. Support for incremental backups of MySQL/MariaDB needs to be added to hostenv's MySQL and restic features.
2. During the deployment process, incremental backups need to be started for any services supporting them, on the machines those services are running on.
3. During the deployment process, feature activation scripts need to check whether their database(s) have been initialised. If not, they need to trigger restic to download backups for their service, including the incremental one made in (2). Note: there is a nuance here that restic on the newly provisioned machine needs to have data on the incremental backup made in (2), if not we risk not retrieving that incremental backup.
4. Once backups have been obtained on the newly provisioned machine, the feature activation scripts mentioned in (3) need to restore from those backups. Note: the service should be stopped during restore. Also note: incremental backups need special handling to be restored correctly, in order.
5. Once backups have been restored to where the services expect their data to be, the services may be started.
6. If possible, integration and unit tests should be added to ensure correctness.

## Conclusion

Investigate the feasibility and scope of this change using the given source code. If the requirements are impossible to implement or would be fraught with issues, stop there. Othwerise, provide a spec for a Nix implementer who will write the code.

## Current status

### Backups

* `modules/features/restic.nix` sets up **user-level systemd** oneshot services and (optionally) timers named:

  * `restic-backups-<name>.service`
  * `restic-backups-<name>.timer`
    It also generates wrapper scripts `restic-<name>` (via `createWrapper = true`) that source an env file and set `RESTIC_*` variables.

* `modules/features/drupal.nix` and `modules/features/php-app.nix` enable restic backups and (for the DB) run **`mariabackup --backup`** into:

  * `$XDG_STATE_HOME/mariabackup/full`
    and then restic backs up that directory plus any app paths (Drupal also backs up `files/` and `private_files/`).

* There is **no restore path** anywhere (no restic restore, no mariabackup prepare/copy-back), and **no incremental DB backup** (only full `mariabackup --backup`).

### Deploy/activation

* `modules/hostenv/base.nix` builds a single activation script that concatenates `config.activate` snippets from enabled features.
* `modules/features/drupal.nix`’s activation currently runs `drush updatedb` and `drush config:import`, but **does not** check/restore state.
* Provider-side planning/deploy (`modules/entrypoints/provider/plan.nix`, `deploy-rs` config in `modules/entrypoints/provider/options.nix`, and the Haskell provider service/CLI) **does not coordinate state migration** when an environment moves nodes.

This matches the symptom: provisioning a new machine creates the services and empty storage, but it never restores from the existing restic repository.

## Feasibility

Everything listed requirement is implementable with this codebase, but there are a few sharp edges that need to be designed around:

1. **DB incremental backups are not “restic incremental”**
   Restic is already content-addressed/deduplicating, but it still backs up *whatever files you point it at*. Your DB backup today is a fresh physical copy produced by `mariabackup`; that’s why true DB incrementals (either mariabackup/xtrabackup incrementals, or binlog-based PITR) are needed.

2. **Picking the “right” snapshot across machines** is genuinely tricky
   If you select “latest snapshot” by timestamp and you have:

   * clock skew, or
   * concurrent backups, or
   * the new node starts producing its own snapshots quickly,

   you can restore the wrong snapshot (including an empty one). The nuance in (3) is real.

3. **Incremental restore ordering and correctness** is doable but must be precise
   With mariabackup/xtrabackup incrementals you must:

   * prepare base with `--apply-log-only`,
   * apply incrementals in sequence,
   * finalize prepare,
   * copy-back into an empty datadir,
   * start mysqld.

4. **Backwards compatibility with existing repos**
   The current backups include `~/.local/state/mariabackup/full`. If you change directory layout, restores of existing snapshots will break unless you support both layouts.

None of the above makes this impossible; it just means the spec has to force deterministic selection and idempotent restores.

---

## Implementation

### High-level

* Add **physical incremental backups** for MySQL/MariaDB based on:

  * MariaDB: `mariabackup --incremental-basedir …`
  * MySQL: `xtrabackup` (Percona) to support physical incrementals, or explicitly document that MySQL server packages require xtrabackup.
* Add a **“migration backup” restic backup set** per stateful feature (Drupal, php-app at minimum) with:

  * no timer (manual),
  * a `backupPrepareCommand` that produces an incremental DB backup next to the full,
  * a distinct tag/name so the new host can reliably restore the migration snapshot.
* Modify feature activation scripts to:

  * detect uninitialized DB(s),
  * pull the appropriate restic snapshot(s),
  * restore DB and file state with services stopped,
  * start services,
  * mark completion (sentinel file) so it’s safe/idempotent.
* Update the provider deployment flow to:

  * detect env moves (old node → new node),
  * run migration backups on the old node(s) **and wait for completion**,
  * write the exact snapshot IDs into a file on the new node so restore is deterministic.

---

## Concrete Nix implementation spec

### 1. MySQL feature: add incremental backup primitives

**Files to change**

* `modules/features/mysql.nix`

**Goals**

* Provide reusable scripts (and/or config values) so application features don’t embed DB backup logic.
* Support both full and incremental.
* Keep existing path layout for compatibility:

  * base full: `$XDG_STATE_HOME/mariabackup/full`
  * new incremental: `$XDG_STATE_HOME/mariabackup/incremental` (differential)
    (or `incrementals/<ts>` if you want chaining; see “Variant B” below)

**New options**
Add something like:

```nix
services.mysql.backups = {
  enable = mkEnableOption "MySQL/MariaDB physical backups (mariabackup/xtrabackup)";

  # Keep compatible defaults:
  stateDir = mkOption { type = types.path; default = config.hostenv.stateDir + "/mariabackup"; };

  fullDir = mkOption { type = types.path; readOnly = true; };
  incrementalDir = mkOption { type = types.path; readOnly = true; };

  # Tooling:
  tool = mkOption {
    type = types.enum [ "mariabackup" "xtrabackup" ];
    default = if isMariaDB then "mariabackup" else "xtrabackup";
  };
  toolPackage = mkOption {
    type = types.package;
    default = if isMariaDB then cfg.package else pkgs.percona-xtrabackup;
  };

  # Expose scripts to other modules:
  scripts = {
    full = mkOption { type = types.package; readOnly = true; };
    incremental = mkOption { type = types.package; readOnly = true; };
    restore = mkOption { type = types.package; readOnly = true; };
  };

  # Concurrency guard:
  lockFile = mkOption { type = types.path; default = config.hostenv.stateDir + "/mariabackup/.lock"; };
};
```

**Script behaviors**

* `scripts.full`:

  * Acquire lock (e.g., `flock` on lockFile; add `util-linux` to runtime inputs).
  * `rm -rf $stateDir/full $stateDir/incremental` (important: drop incrementals because base changes).
  * Run `${tool} --backup -S ${socket} --target-dir=$fullDir`
* `scripts.incremental`:

  * Acquire lock.
  * If `$fullDir` missing, run the full script first.
  * `rm -rf $incrementalDir && mkdir -p $incrementalDir`
  * Run `${tool} --backup -S ${socket} --target-dir=$incrementalDir --incremental-basedir=$fullDir`
* `scripts.restore <restored-mariabackup-root> <datadir>`:

  * Validate `$root/full` exists.
  * If `$root/incremental` exists, apply incremental correctly:

    * `--prepare --apply-log-only --target-dir=$full`
    * apply incremental **as last** (no `--apply-log-only` on the final step)
  * Ensure datadir is empty, then `--copy-back --datadir=$datadir --target-dir=$full`

**Important restore detail (differential incremental)**
For “one incremental dir” (`incremental/` overwritten each time), the recommended sequence is:

1. `mariabackup --prepare --apply-log-only --target-dir=full`
2. `mariabackup --prepare --target-dir=full --incremental-dir=incremental`
3. copy-back

(for chained incrementals, step 2 repeats with `--apply-log-only` for all but the last.)

---

### 2. Restic feature: expose wrapper scripts + default tagging

**Files to change**

* `modules/features/restic.nix`

**Needed for the restore logic**
Feature activation scripts need an **absolute path** to the wrapper script derivation, so they can run restic without relying on `~/.local/bin` symlinks.

**Add a new read-only attrset**
Expose the wrapper packages:

```nix
services.restic.wrapperScripts = mkOption {
  type = types.attrsOf types.package;
  readOnly = true;
  default = wrapperScriptsList; # the existing let-bound set
};
```

**Add default tags**
To enable robust snapshot selection by backup “role”, make restic always tag snapshots with the backup name unless the user opts out. Two implementation styles:

* simplest: in `runBackup`, prepend `--tag ${name}` to `extraBackupArgs`
* more explicit: add `backup.tags = [ name ]` option and use it to generate `--tag` args

This is especially important once you add a `*-migrate` backup set, because it gives you an unambiguous selector that won’t collide with normal scheduled snapshots.

---

### 3. Drupal feature: add migrate backup + restore-on-first-activate

**Files to change**

* `modules/features/drupal.nix`

#### 3.1 Add the incremental/migration backup set

Keep the existing backup (`services.restic.backups.drupal`) as the scheduled full (daily).

Add a second restic backup, no timer, whose pre-step produces the incremental:

* `services.restic.backups.drupal-migrate`:

  * `timerConfig = null;`
  * `backupPrepareCommand = "${config.services.mysql.backups.scripts.incremental}/bin/…"` (call incremental)
  * `paths =` **same** as current (mariabackup dir + files dirs + private files)
  * `wantsUnits = [ "mysql.service" ];`
  * `extraBackupArgs` should include the auto-tagging anyway, but also include explicit `--tag migrate`.

This creates a unit:

* `restic-backups-drupal-migrate.service`
  that the deploy process can start on the old host.

#### 3.2 Add restore logic to the Drupal activation script

Right now Drupal activation always runs drush tasks.

Add at the *start* of the Drupal activation snippet:

* **Initialization check**: DB “has tables” is better than “DB exists” because MySQL postStart creates empty DBs.

  * Use socket auth:

    * `SELECT COUNT(*) FROM information_schema.tables WHERE table_schema='drupal';`
  * If count > 0, consider initialized.

* **Sentinel file** (idempotence):

  * after a successful restore, write:

    * `$XDG_STATE_HOME/hostenv/restored/drupal`
  * Skip restore if sentinel exists (even if DB check is inconclusive).

* **Restore flow** (when not initialized and no sentinel):

  1. Stop services:

     * `systemctl --user stop nginx.service || true`
     * `systemctl --user stop phpfpm.target || true`
     * `systemctl --user stop mysql.service || true`
  2. Restore restic snapshot into a temp dir:

     * Prefer migration snapshot:

       * run `${resticWrapperDrupalMigrate} restore latest --tag drupal-migrate --target "$tmp" --no-owner`
     * If that fails due to “no snapshot found”, fall back:

       * `${resticWrapperDrupal} restore latest --tag drupal --target "$tmp" --no-owner`
     * Retry/poll for a short window if migrate snapshot isn’t yet visible.
  3. Restore DB:

     * call mysql restore script with:

       * root = `$tmp/home/<user>/.local/state/mariabackup`
       * datadir = `${config.services.mysql.dataDir}`
  4. Restore files:

     * replace `${cfg.filesDir}` and `${cfg.privateFilesDir}` from:

       * `$tmp/home/<user>/.local/share/files`
       * `$tmp/home/<user>/.local/share/private_files`
  5. Start mysql, wait for socket, start phpfpm/nginx.
  6. Write sentinel file.
  7. Clean up temp dir.

Then continue with the existing `drush updatedb` and `drush config:import`.

**Optional improvement (recommended)**
If restore wasn’t possible because there are no snapshots, skip drush commands when the DB is empty, so “brand new env” doesn’t hard-fail deploy. This is a behavior choice; document it.

---

### 4. php-app feature: mirror the same pattern

**Files to change**

* `modules/features/php-app.nix`

* Add `services.restic.backups.php-app-migrate`:

  * `backupPrepareCommand` calls mysql incremental script
  * `paths` already include mariabackup dir; include any app state dirs if relevant
  * no timer

* Add a `config.activate` snippet for php-app:

  * DB init check for database `app` (or make DB name an option and iterate a list)
  * stop mysql + any php-app relevant services
  * restic restore from `php-app-migrate` then fallback to `php-app`
  * run mysql restore
  * start services, write sentinel

---

### 5. Deployment process: trigger incrementals on the “old” node, then deploy the “new” node

#### Strategy: deterministic snapshot IDs

Add a “migration metadata” handoff:

1. **Provider plan output includes previous node**

   * In `modules/entrypoints/provider/plan.nix`, add `previousNode` (nullable) to each environment record based on `state.json`.
   * A migration is `previousNode != node`.

2. **Provider deployment orchestration**

   * In provider-service / provider CLI, before deploying the new host:

     * for each migrating env:

       * ssh to `previousNode` as env user
       * run each `restic-backups-*-migrate.service` and capture the snapshot ID

         * easiest capture method: don’t go through systemd; run the wrapper script directly so it's possible to parse “snapshot <id> saved”.
       * write a file containing the snapshot IDs (JSON) onto the **target host** in a known location owned by the env user, e.g.:

         * `~/.local/state/hostenv/migrations/<timestamp>.json`
         * or `~/.local/state/hostenv/restore-plan.json` (overwrite)

3. **Activation restore uses exact IDs if present**

   * Drupal/php-app activation checks for that file first; if present, restore exactly those snapshot IDs (no “latest”).

This directly addresses requirement (3) nuance: the new machine can restore *the exact incremental snapshot created in (2)*.

### 6. Restore ordering and service control requirements

Enforce in each feature restore code:

* Stop services before restore:

  * MySQL must be stopped before copy-back.
* Restore DB first, then application files, then start services:

  * start MySQL
  * wait for socket/health
  * start PHP-FPM target
  * start nginx
* Use locking so backups and restores can’t overlap:

  * The mysql backup scripts should use `flock` (lockfile in state dir).
  * The restore path should also acquire the same lock.

---

### 7. Tests to add

Given this repo’s current test setup (`tests/integration/*` are Nix eval-style checks), add:

#### Unit-ish eval tests

* **restic module**

  * `services.restic.wrapperScripts` exists and contains expected keys when backups are defined.
  * backup services include `--tag <name>` by default.
* **mysql module**

  * when `services.mysql.backups.enable = true`, the generated scripts exist in the profile (or are exposed via config).
* **drupal/php-app modules**

  * when backups enabled, `services.restic.backups.<name>-migrate` is present with no timer.

#### Script content tests

In existing integration tests (like `tests/integration/drupal/tests.nix`), assert that:

* the activation script contains the restore block (a grep for a stable marker string you add, e.g. `HOSTENV_RESTORE_DRUPAL_BEGIN`).
* the mysql restore script contains the expected `mariabackup --prepare` sequence.

If true runtime correctness tests are required, it would be necessary to add a NixOS VM test harness (not present today), so that's treated as “nice to have later”.

---

## Compatibility and operational notes

* **Do not change** `~/.local/state/mariabackup/full` unless supporting restoring old snapshots. The spec above keeps it.
* The restore logic must be:

  * **idempotent** (sentinel file + DB table-count check),
  * **safe** (never clobber a non-empty DB),
  * **observable** (log clearly what snapshot/tag/ID is being restored).
* For MySQL (non-MariaDB) support, you likely need to add `pkgs.percona-xtrabackup` and switch backup tool accordingly. The current Drupal/php-app backup code hardcodes `pkgs.mariadb` binaries; this should be corrected as part of the mysql backup refactor.

---

## Summary

Nothing in the hostenv code makes the requirements impossible. The scope is “medium” but very contained:

* implement incremental physical DB backups (mysql module),
* add migration backup definitions (drupal/php-app),
* implement restore on first activation (drupal/php-app),
* add a small restic module improvement to make wrappers usable from other modules and tag snapshots,
* add provider orchestration to hand off exact snapshot IDs so migrations are deterministic.
