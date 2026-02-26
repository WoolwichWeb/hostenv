# Drupal Seed Database

`seed.sql.gz` is a compressed MySQL dump for the local provider migration demo.

It contains:
- A Drupal 11 standard install database.
- A `hostenv_demo_marker` row used by migration verification.

The wizard copies this file into the demo project as `.hostenv/seed.sql.gz` and asks the user to import it with:

```bash
pv .hostenv/seed.sql.gz | gunzip -c | mysql
```

## Refreshing the seed

Regenerate this file from a known-good demo run where Drupal is installed, then replace `seed.sql.gz`.

Recommended constraints:
- Keep it small (compressed size in low MBs or less).
- Preserve the `hostenv_demo_marker` row with value `from-node-a`.
