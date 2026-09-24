{ pkgs, hostenvUserLifecycleModule }:

let
  user = "retired-hostenv-user";
  uid = 2000;
in
pkgs.testers.runNixOSTest {
  name = "retired-linger-user-vm-test";

  nodes.machine = { ... }: {
    imports = [ hostenvUserLifecycleModule ];

    system.stateVersion = "26.05";
    users.mutableUsers = false;

    provider.retiredUsers.${user}.uid = uid;
  };

  testScript = ''
    start_all()
    machine.wait_for_unit("multi-user.target")

    with subtest("create a lingering user manager matching a retired environment"):
        machine.succeed(
            "useradd --uid ${toString uid} --gid users --create-home ${user}"
        )
        machine.succeed("loginctl enable-linger ${user}")
        machine.succeed("systemctl start user@${toString uid}.service")
        machine.wait_for_unit("user@${toString uid}.service")
        machine.wait_until_succeeds("test -S /run/user/${toString uid}/bus")
        machine.succeed("test -e /var/lib/systemd/linger/${user}")

    with subtest("activation stops lingering before immutable user removal"):
        machine.succeed("/run/current-system/activate")
        machine.fail("getent passwd ${user}")
        machine.fail("test -e /var/lib/systemd/linger/${user}")
        machine.fail("systemctl is-active --quiet user@${toString uid}.service")
        machine.fail("test -S /run/user/${toString uid}/bus")

    with subtest("retired-user cleanup remains idempotent"):
        machine.succeed("/run/current-system/activate")
  '';
}
