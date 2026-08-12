{ pkgs, env, hostenvNixosModule }:

let
  cfg = env.config;
  profile = cfg.activatePackage;
  environment = cfg.environments.${cfg.hostenv.environmentName};
  user = cfg.hostenv.userName;
  uid = 2000;
  home = "/home/${user}";
  runtimeDir = cfg.hostenv.runtimeDir;
  nginxSocket = "${cfg.hostenv.upstreamRuntimeDir}/in.sock";
  virtualHost = cfg.hostenv.hostname;
in
pkgs.testers.runNixOSTest {
  name = "drupal6-vm-test";

  nodes.machine = { ... }: {
    imports = [ hostenvNixosModule ];

    provider.plan.environments.${cfg.hostenv.environmentName} = environment // {
      inherit uid;
    };

    users.groups.nginx = { };

    environment.systemPackages = [
      pkgs.curl
      profile
    ];

    virtualisation = {
      cores = 2;
      memorySize = 2048;
    };
  };

  testScript =
    { nodes, ... }:
    let
      activate = pkgs.writeShellScript "activate-drupal6-test-environment" ''
        set -euo pipefail

        export HOME=${home}
        export XDG_CACHE_HOME=${home}/.cache
        export XDG_CONFIG_HOME=${home}/.config
        export XDG_DATA_HOME=${home}/.local/share
        export XDG_RUNTIME_DIR=/run/user/${toString uid}
        export XDG_STATE_HOME=${home}/.local/state

        exec ${profile}/bin/activate
      '';
      curl = pkgs.writeShellScript "drupal6-test-curl" ''
        exec ${pkgs.curl}/bin/curl \
          --fail-with-body \
          --silent \
          --show-error \
          --unix-socket ${nginxSocket} \
          --header ${pkgs.lib.escapeShellArg "Host: ${virtualHost}"} \
          "$@"
      '';
      mysql = pkgs.writeShellScript "drupal6-test-mysql" ''
        exec ${pkgs.util-linux}/bin/runuser -u ${user} -- \
          ${profile}/bin/mysql "$@"
      '';
    in
    ''
      import re
      import shlex
      from datetime import timedelta
      from html.parser import HTMLParser


      class InputParser(HTMLParser):
          def __init__(self):
              super().__init__()
              self.inputs = {}

          def handle_starttag(self, tag, attrs):
              if tag != "input":
                  return
              attributes = dict(attrs)
              if "name" in attributes:
                  self.inputs[attributes["name"]] = attributes.get("value", "")


      def form_inputs(path):
          parser = InputParser()
          parser.feed(machine.succeed(f"cat {shlex.quote(path)}"))
          return parser.inputs


      def data_arg(name, value):
          return f"--data-urlencode {shlex.quote(name + '=' + value)}"


      start_all()
      machine.wait_for_unit("multi-user.target")
      machine.wait_for_unit("user@${toString uid}.service")
      machine.succeed("test -d ${runtimeDir}")

      with subtest("activate the generated Hostenv profile"):
          machine.succeed(
              "${pkgs.util-linux}/bin/runuser -u ${user} -- ${activate}"
          )
          machine.wait_until_succeeds(
              "test -S ${runtimeDir}/mysql.sock", timeout=timedelta(seconds=120)
          )
          machine.wait_until_succeeds(
              "test -S ${runtimeDir}/${cfg.services.drupal.codebase.name}.sock",
              timeout=timedelta(seconds=60),
          )
          machine.wait_until_succeeds(
              "test -S ${nginxSocket}", timeout=timedelta(seconds=60)
          )
          machine.succeed(
              "${pkgs.util-linux}/bin/runuser -u ${user} -- "
              "${profile}/bin/php -r 'exit(str_starts_with(PHP_VERSION, \"8.\") ? 0 : 1);'"
          )

      with subtest("install Drupal through nginx and PHP-FPM"):
          machine.succeed(
              "${curl} --location --cookie-jar /tmp/drupal.cookies "
              "--cookie /tmp/drupal.cookies --output /tmp/install.html "
              "'http://localhost/install.php?profile=default&locale=en'"
          )

          install_page = machine.succeed("cat /tmp/install.html")
          batch_id_match = re.search(r"[?&]id=([0-9]+)", install_page.replace("&amp;", "&"))
          assert batch_id_match, install_page
          batch_id = batch_id_match.group(1)

          for _ in range(30):
              machine.succeed(
                  "${curl} --location --cookie-jar /tmp/drupal.cookies "
                  "--cookie /tmp/drupal.cookies --output /tmp/install.html "
                  f"'http://localhost/install.php?profile=default&locale=en&id={batch_id}&op=do_nojs'"
              )
              install_page = machine.succeed("cat /tmp/install.html")
              if "op=finished" in install_page.replace("&amp;", "&"):
                  machine.succeed(
                      "${curl} --location --cookie-jar /tmp/drupal.cookies "
                      "--cookie /tmp/drupal.cookies --output /tmp/configure.html "
                      f"'http://localhost/install.php?profile=default&locale=en&id={batch_id}&op=finished'"
                  )
                  break
          else:
              raise AssertionError("Drupal installation batch did not finish")

          configure_page = machine.succeed("cat /tmp/configure.html")
          assert "Configure site" in configure_page, configure_page
          inputs = form_inputs("/tmp/configure.html")
          assert inputs.get("form_id") == "install_configure_form", inputs
          assert "form_build_id" in inputs, inputs

          form_data = {
              "site_name": "Hostenv Drupal 6 test",
              "site_mail": "admin@example.com",
              "account[name]": "admin",
              "account[mail]": "admin@example.com",
              "account[pass][pass1]": "hostenv-test-password",
              "account[pass][pass2]": "hostenv-test-password",
              "date_default_timezone": "0",
              "clean_url": "0",
              "update_status_module[1]": "0",
              "op": "Save and continue",
              "form_build_id": inputs["form_build_id"],
              "form_id": inputs["form_id"],
          }
          if "form_token" in inputs:
              form_data["form_token"] = inputs["form_token"]

          post_args = " ".join(data_arg(name, value) for name, value in form_data.items())
          machine.succeed(
              "${curl} --location --cookie-jar /tmp/drupal.cookies "
              "--cookie /tmp/drupal.cookies --output /tmp/finished.html "
              f"{post_args} "
              "'http://localhost/install.php?profile=default&locale=en'"
          )
          finished_page = machine.succeed("cat /tmp/finished.html")
          assert "successfully installed" in finished_page, finished_page

      with subtest("serve and bootstrap the installed site"):
          machine.succeed(
              "${mysql} --batch --skip-column-names "
              "--execute="
              + shlex.quote(
                  "SELECT COUNT(*) FROM system WHERE name = 'system' AND status = 1;"
              )
              + " | grep -qx 1"
          )
          machine.succeed(
              "${curl} --location --cookie-jar /tmp/drupal.cookies "
              "--cookie /tmp/drupal.cookies --output /tmp/home.html http://localhost/"
          )
          home_page = machine.succeed("cat /tmp/home.html")
          assert "Hostenv Drupal 6 test" in home_page, home_page

      with subtest("complete a fresh administrator login"):
          machine.succeed(
              "${curl} --location --cookie-jar /tmp/login.cookies "
              "--cookie /tmp/login.cookies --output /tmp/login.html "
              "http://localhost/user/login"
          )
          inputs = form_inputs("/tmp/login.html")
          assert inputs.get("form_id") == "user_login", inputs

          login_data = {
              "name": "admin",
              "pass": "hostenv-test-password",
              "op": "Log in",
              "form_build_id": inputs["form_build_id"],
              "form_id": inputs["form_id"],
          }
          if "form_token" in inputs:
              login_data["form_token"] = inputs["form_token"]

          post_args = " ".join(data_arg(name, value) for name, value in login_data.items())
          machine.succeed(
              "${curl} --location --cookie-jar /tmp/login.cookies "
              "--cookie /tmp/login.cookies --output /tmp/logged-in.html "
              f"{post_args} http://localhost/user/login"
          )
          logged_in_page = machine.succeed("cat /tmp/logged-in.html")
          assert "Log out" in logged_in_page, logged_in_page

      with subtest("report no PHP runtime compatibility failures"):
          journal = machine.succeed("journalctl _UID=${toString uid} --no-pager")
          assert not re.search(
              r"PHP (Fatal error|Parse error|Deprecated)|Uncaught (Error|Exception)",
              journal,
              re.IGNORECASE,
          ), journal
    '';
}
