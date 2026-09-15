# Redis-compatible service

Hostenv can run a per-environment Redis-compatible service backed by Valkey.
It is disabled by default.

```nix
{ pkgs, ... }:
{
  services.redis = {
    enable = true;

    # Valkey is the default package.
    package = pkgs.valkey;
  };
}
```

The service runs as the environment UNIX user. Plain TCP, TLS TCP, and RDMA
listeners are disabled (`port 0`, `tls-port 0`, and `rdma-port 0`), cluster mode
is disabled, and clients connect through
`<environment runtime directory>/redis.sock`. The socket
defaults to mode `0700`, so it is private to the environment user.

Persistent data defaults to `<environment data directory>/redis`, and
append-only persistence is enabled by default. The main options are:

- `services.redis.package` — systemd-capable Valkey package, default `pkgs.valkey`.
- `services.redis.dataDir` — persistent data directory.
- `services.redis.runtimeDir` — directory containing the Unix socket.
- `services.redis.socket` — read-only resolved socket path.
- `services.redis.socketPermissions` — socket mode, default `700`.
- `services.redis.appendOnly` — append-only persistence, default `true`.
- `services.redis.extraConfig` — additional **non-secret** Valkey
  configuration. It is copied into the Nix store. Hostenv applies `port 0`,
  `tls-port 0`, `rdma-port 0`, and `cluster-enabled no` afterward, together
  with its socket/runtime directives, so project configuration cannot
  accidentally re-enable a network listener or the cluster bus. Do not put
  passwords, ACL credentials, or other secrets directly in this option.

Valkey and its command-line tools are added to the environment profile when the
service is enabled. Hostenv does not currently create backups for Valkey data;
append-only persistence protects normal restarts, not loss of the host or data
directory.
