{ pkgs }:
let
  spinner = import ../../lib/spinner.nix;

  mkProbe = name: args:
    pkgs.writeShellApplication {
      inherit name;
      runtimeInputs = [ pkgs.coreutils ];
      text = spinner args;
    };

  quietProbe = mkProbe "hostenv-spinner-quiet-probe" {
    title = "Quiet operation";
    variant = "upload";
    command = ''
      printf 'quiet payload\n'
      sleep 0.35
    '';
  };

  outputProbe = mkProbe "hostenv-spinner-output-probe" {
    title = "Visible operation";
    variant = "upload";
    showOutput = true;
    command = ''
      printf 'visible payload\n'
    '';
  };

  failureProbe = mkProbe "hostenv-spinner-failure-probe" {
    title = "Failing operation";
    variant = "build";
    showError = true;
    command = ''
      sleep 0.2
      printf 'failure payload\n' >&2
      exit 23
    '';
  };

  activationProbe = mkProbe "hostenv-spinner-activation-probe" {
    title = "Building main...";
    variant = "build";
    activationMarker = "__HOSTENV_NEKO_ACTIVATE__";
    activationTitle = "Activating main...";
    showOutput = true;
    command = ''
      sleep 0.25
      printf '%s\n' '__HOSTENV_NEKO_ACTIVATE__'
      sleep 0.35
      printf 'activation payload\n'
    '';
  };

  sleepProbe = mkProbe "hostenv-spinner-sleep-probe" {
    title = "Updating local hostenv.nix...";
    variant = "sync";
    finish = "sleep";
    successTitle = "✅ Deploy complete";
    command = ''
      sleep 0.25
    '';
  };

  titleProbe = pkgs.writeShellApplication {
    name = "hostenv-spinner-title-probe";
    runtimeInputs = [ pkgs.coreutils ];
    text = ''
      HOSTENV_SPINNER_TEST_TITLE="$1"
      ${spinner {
        title = "Deploying $HOSTENV_SPINNER_TEST_TITLE...";
        command = "true";
      }}
    '';
  };
in
{
  hostenv-cli-spinner = pkgs.runCommand "hostenv-cli-spinner"
    {
      nativeBuildInputs = [ pkgs.coreutils pkgs.gnugrep pkgs.util-linux ];
    }
    ''
      set -euo pipefail

      fail() {
        printf 'hostenv spinner regression test failed: %s\n' "$1" >&2
        exit 1
      }

      assert_no_capability_queries() {
        file="$1"
        if grep -aF $'\033[?2026$p' "$file" >/dev/null \
          || grep -aF $'\033[?2027$p' "$file" >/dev/null; then
          fail "spinner emitted Bubble Tea DEC mode capability queries"
        fi
      }

      run_pty() {
        transcript="$1"
        command="$2"
        COLUMNS=80 TERM=xterm-256color script -qefc "$command" "$transcript" >/dev/null
      }

      # Keep the original regression guarantees for non-TTY use.
      ${quietProbe}/bin/hostenv-spinner-quiet-probe >quiet.out 2>quiet.err
      ! grep -Fq 'quiet payload' quiet.out \
        || fail "successful command output should stay hidden by default"
      ! grep -Fq 'quiet payload' quiet.err \
        || fail "successful command output should not leak to stderr"
      assert_no_capability_queries quiet.out
      assert_no_capability_queries quiet.err

      ${outputProbe}/bin/hostenv-spinner-output-probe >output.out 2>output.err
      grep -Fq 'visible payload' output.out \
        || fail "showOutput should replay successful command output"
      assert_no_capability_queries output.out
      assert_no_capability_queries output.err

      set +e
      ${failureProbe}/bin/hostenv-spinner-failure-probe >failure.out 2>failure.err
      status=$?
      set -e
      [ "$status" -eq 23 ] \
        || fail "spinner should preserve the wrapped command exit status"
      grep -Fq 'failure payload' failure.out \
        || fail "showError should replay failed command output"
      assert_no_capability_queries failure.out
      assert_no_capability_queries failure.err

      ${titleProbe}/bin/hostenv-spinner-title-probe main >title.out 2>title.err
      grep -Fq 'Deploying main...' title.err \
        || fail "spinner titles should retain runtime shell-variable expansion"
      assert_no_capability_queries title.out
      assert_no_capability_queries title.err

      # Exercise the actual animated path inside a pseudo-terminal. This is the
      # path that used to leak mode-2026/2027 replies via Gum/Bubble Tea.
      run_pty chase.pty '${quietProbe}/bin/hostenv-spinner-quiet-probe'
      grep -aFq '/\_/\' chase.pty \
        || fail "wide TTYs should render the multi-row Neko sprite"
      grep -aFq '♡' chase.pty \
        || fail "Neko should chase the deployment heart"
      grep -Fq '_hostenv_neko_step=$((_hostenv_spinner_frame / 2))' \
        ${quietProbe}/bin/hostenv-spinner-quiet-probe \
        || fail "Neko should advance toward the heart only every other 10 FPS frame"
      grep -Fq '_hostenv_spinner() {' \
        ${quietProbe}/bin/hostenv-spinner-quiet-probe \
        || fail "Hostneko should use a normal function body so animation state stays visible to ShellCheck"
      if grep -Fq '_hostenv_spinner() (' \
        ${quietProbe}/bin/hostenv-spinner-quiet-probe; then
        fail "Hostneko function body must not create a subshell boundary around animation state"
      fi
      assert_no_capability_queries chase.pty

      run_pty activate.pty '${activationProbe}/bin/hostenv-spinner-activation-probe'
      grep -aFq '( O.O )' activate.pty \
        || fail "activation should startle Neko"
      grep -aFq '⚡' activate.pty \
        || fail "activation should render the lightning reaction"
      grep -aFq 'Activating main...' activate.pty \
        || fail "the activation marker should switch the live status"
      grep -aFq 'activation payload' activate.pty \
        || fail "showOutput should still replay real command output"
      ! grep -aFq '__HOSTENV_NEKO_ACTIVATE__' activate.pty \
        || fail "internal animation markers must not leak into command output"
      assert_no_capability_queries activate.pty

      run_pty sleep.pty '${sleepProbe}/bin/hostenv-spinner-sleep-probe'
      grep -aFq '( -O- )' sleep.pty \
        || fail "a completed deploy should make Neko yawn"
      grep -aFq 'zZ' sleep.pty \
        || fail "a completed deploy should leave Neko asleep"
      grep -aFq '✅ Deploy complete' sleep.pty \
        || fail "the sleeping final frame should carry the success status"
      assert_no_capability_queries sleep.pty

      set +e
      run_pty failure.pty '${failureProbe}/bin/hostenv-spinner-failure-probe'
      status=$?
      set -e
      [ "$status" -eq 23 ] \
        || fail "PTY spinner should preserve a failing command's status"
      grep -aFq 'Something went wrong' failure.pty \
        || fail "failures should wake Neko into an alert reaction"
      grep -aFq 'failure payload' failure.pty \
        || fail "failure output should remain visible after the reaction"
      assert_no_capability_queries failure.pty

      # Narrow terminals get a one-line Neko rather than cursor-up drawing.
      COLUMNS=30 TERM=xterm-256color script -qefc \
        '${quietProbe}/bin/hostenv-spinner-quiet-probe' narrow.pty >/dev/null
      grep -aFq '=^.^=' narrow.pty \
        || fail "narrow terminals should use the compact Neko fallback"
      if grep -aF $'\033[3A' narrow.pty >/dev/null; then
        fail "compact mode must not use multi-row cursor movement"
      fi
      assert_no_capability_queries narrow.pty

      echo ok > "$out"
    '';
}
