# Hostenv Tests

This directory contains test environments and test suites for hostenv.

## Directory Structure

- `drupal/` - Test environment for Drupal CMS
  - `hostenv.nix` - Hostenv configuration for test Drupal site
  - `tests.nix` - Test suite checking Drupal functionality
  - `composer.json`, `composer.lock` - Drupal dependencies

## Running Tests Locally

### Run all tests

```bash
nix flake check
```

This will run all checks defined in `checks.*` for your system.

### Run a specific test

```bash
nix build .#checks.x86_64-linux.test-nginx-conf-exists
```

Replace `test-nginx-conf-exists` with any test name from `tests/drupal/tests.nix`.

### List all available tests

```bash
nix flake show
```

### Build test packages

```bash
# Production environment (main)
nix build .#testDrupalProduction

# Development environment (dev)
nix build .#testDrupalDev
```

The built packages will be symlinked to `./result`.

### Inspect test environment output

After building, you can inspect the generated configuration:

```bash
# View nginx configuration
cat result/etc/nginx/nginx.conf

# View PHP-FPM pool configuration
ls result/etc/php-fpm.d/

# View systemd services
ls result/systemd/user/
```

## CI/CD Integration

Tests run automatically on GitLab CI for:

- All merge requests
- Pushes to `main` branch
- Scheduled nightly builds (optional)

See `.gitlab-ci.yml` for configuration.

## Writing New Tests

Tests are just Nix derivations that fail (exit non-zero) on test failure.

Example test structure:

```nix
test-example = pkgs.runCommand "test-example" {
  buildInputs = [ pkgs.gnugrep pkgs.coreutils ];
} ''
  echo "Running test..."

  if some-check-fails; then
    echo "FAIL: description of what failed"
    exit 1
  fi

  echo "PASS: test succeeded"
  touch $out
'';
```

Add new tests to `tests/*/tests.nix` and they'll automatically be included in `nix flake check`.

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
   nix build .#testDrupal
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
