# Local Provider-Service Project Demo

This example prepares the static fixture layout for a local Hostenv walkthrough where the Drupal app project and the provider-service control-plane project live in separate repositories.

## What This Fixture Contains

1. `demo-app-project/` - a Drupal Hostenv project adapted from the existing local migration demo.
2. `provider-service-project/` - a separate Hostenv project that owns the selected `provider.service` environment and enables in-project provider-service deployment APIs.
3. `seed/seed.sql.gz` - the Drupal seed database used by the app fixture.
4. `trigger-webhook.sh` - the reusable webhook helper used by the demo workflow.

## Intended Topology

The later scripted demo will use these fixtures with a two-project topology:

```text
┌───────────────────────┐        webhook         ┌──────────────────────────┐
│ demo-app-project      │ --------------------> │ provider-service-project │
│ Drupal environment    │                       │ provider-service + cache │
└────────────┬──────────┘                       └────────────┬─────────────┘
             │                                               │
             │ app deployment / migration                    │ deploy intent + cache
             ▼                                               ▼
        ┌──────────┐                                   ┌──────────┐
        │ node-a   │                                   │ node-b   │
        └──────────┘                                   └──────────┘
```

- `demo-app-project` keeps the Drupal service and migration seed data.
- `provider-service-project` enables `services.hostenv-provider.enable = true` and `services.hostenv-provider.deploy.enable = true` in its own environment.
- Harmonia cache support comes from the current provider-service feature defaults; this fixture does not add manual cache wiring.

## Status

This directory currently provides scaffold files only. `run-demo.sh`, provider templates, and integration wiring are added in later deliverables.

## File Structure

```text
examples/local-provider-service-project/
├── README.md
├── trigger-webhook.sh
├── demo-app-project/
│   ├── composer.json
│   ├── composer.lock
│   ├── source.nix
│   └── .hostenv/
│       ├── flake.nix
│       └── hostenv.nix
├── provider-service-project/
│   └── .hostenv/
│       ├── flake.nix
│       └── hostenv.nix
└── seed/
    ├── README.md
    └── seed.sql.gz
```
