# Hostenv Tests

This directory contains test environments and test suites for hostenv.

## Directory Structure

- `*/` - Test environments
  - `hostenv.nix` - Hostenv configuration for test Drupal site
  - `tests.nix` - Test suite checking Drupal functionality

## Running Tests Locally

### Run all tests

```bash
nix flake check
```

This will run all checks defined in `checks.*` for your system.

### Run a specific test

```bash
nix build .#checks.x86_64-linux.drupal-test-nginx-config-valid
```

Replace `drupal-test-nginx-config-valid` with any test name. Hint: use tab
completion:

```bash
nix build .#checks.x86_64-linux.<TAB>
```

to see and narrow down the full list of available tests.

### List all available tests

```bash
nix flake show
```

### Inspect test environment output

After building, you can inspect the generated configuration:

```bash
# View nginx configuration
cat result/attachments/env/etc/nginx/nginx.conf

# View PHP-FPM pool configuration
ls result/attachments/env/etc/php-fpm.d/

# View systemd services
ls result/attachments/env/systemd/user/
```

Other files related to the specific test you built will be in
`result/attachements`, while the test script itself will be in
`result/bin`.

## CI/CD Integration

Tests run automatically on GitLab CI for:

- All merge requests
- Pushes to `main` branch
- Scheduled nightly builds (optional)

See `.gitlab-ci.yml` for configuration.

## Writing New Tests

Tests are just Nix derivations that fail (exit non-zero) on test failure.

See files under `tests/*/tests.nix` for examples.

Add new tests to `tests/*/tests.nix` and they'll automatically be included
in `nix flake check`.

To add a new test suite, create `tests/[SUITE NAME]/tests.nix`, then wire
your new `tests.nix` file up in `tests/default.nix`. Your tests will
automatically be included in `nix flake check`.

## Updating Composer Dependencies

If you modify `tests/drupal/composer.json`:

1. Update `composer.lock`:

   ```bash
   cd tests/drupal
   composer update
   ```

2. Set dependency hash in `hostenv.nix` to trigger rebuild:

   ```nix
   services.drupal.composer.dependencyHash = lib.fakeHash;
   ```

3. Build to get the correct hash:

   ```bash
   nix build .#checks.x86-linux.drupal-dev-test-drupal-structure
   #                  ^_______^ ^______________________________^                            
   #                      |             |
   #                      |             | Or some other Drupal test
   #                      | Or your architecture
   ```

4. Copy the hash from the error message into `tests/drupal/hostenv.nix`:

  ```nix
  services.drupal.composer.dependencyHash = "sha256-REMAINDER-OF-THE-HASH"
  ```

## Debugging Test Failures

### Verbose output

```bash
nix build .#checks.x86_64-linux.test-name --show-trace
```

### Keep failed build directory

```bash
nix build .#checks.x86_64-linux.test-name --keep-failed
```

This will print a path like `/tmp/nix-build-test-name.drv-0/` that you can inspect.
