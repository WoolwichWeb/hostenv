{ pkgs }:
let
  spinner = import ../../lib/spinner.nix;

  mkProbe = name: args:
    pkgs.writeShellApplication {
      inherit name;
      runtimeInputs = [ pkgs.coreutils ];
      # Pog generates the real hostenv CLI with strict mode, which includes
      # Bash noclobber. Keep probes faithful to production: mktemp pre-creates
      # the capture file, so an ordinary `>` redirection would fail here.
      text = ''
        set -o noclobber
        ${spinner args}
      '';
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

  streamProbe = mkProbe "hostenv-spinner-stream-probe" {
    title = "Streaming operation";
    variant = "build";
    showOutput = true;
    command = ''
      printf 'EARLY output\n'
      sleep 1.2
      printf 'LATE output\n'
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
      set -o noclobber
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

      # Non-TTY behaviour matches Gum: command output stays useful in logs even
      # when there is no animated renderer.
      ${quietProbe}/bin/hostenv-spinner-quiet-probe >quiet.out 2>quiet.err
      grep -Fq 'quiet payload' quiet.out \
        || fail "non-TTY commands should pass successful output through"
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
      # In non-TTY mode we deliberately match Gum's passthrough behaviour:
      # the child keeps its real stdout/stderr instead of being captured for a
      # later replay. `showError` replay is exercised separately in the PTY
      # test below, where Hostneko actually owns the terminal renderer.
      grep -Fq 'failure payload' failure.err \
        || fail "non-TTY failures should preserve the command's stderr"
      ! grep -Fq 'failure payload' failure.out \
        || fail "non-TTY failure stderr should not be redirected to stdout"
      assert_no_capability_queries failure.out
      assert_no_capability_queries failure.err

      ${titleProbe}/bin/hostenv-spinner-title-probe main >title.out 2>title.err
      grep -Fq 'Deploying main...' title.err \
        || fail "spinner titles should retain runtime shell-variable expansion"
      assert_no_capability_queries title.out
      assert_no_capability_queries title.err

      # The production CLI has noclobber enabled. The command capture must
      # explicitly override it for the mktemp-created file instead of failing
      # with "cannot overwrite existing file" on the first real deploy.
      grep -Fq '>|"$_hostenv_spinner_output"' \
        ${quietProbe}/bin/hostenv-spinner-quiet-probe \
        || fail "spinner capture should deliberately override noclobber"

      # Exercise the actual animated path inside a pseudo-terminal. This is the
      # path that used to leak mode-2026/2027 replies via Gum/Bubble Tea.
      run_pty chase.pty '${quietProbe}/bin/hostenv-spinner-quiet-probe'
      # Keep the ASCII-art needle double-quoted: a single-quoted string that
      # ends in a backslash is valid Bash, but ShellCheck flags it as SC1003.
      grep -aFq "/\\_/\\" chase.pty \
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

      # showOutput is live, not a replay that appears only after the operation
      # has already finished.
      COLUMNS=80 TERM=xterm-256color script -qefc \
        '${streamProbe}/bin/hostenv-spinner-stream-probe' stream.pty >/dev/null &
      stream_pid=$!
      early_visible=0
      for _ in 1 2 3 4 5 6 7 8; do
        if grep -aFq 'EARLY output' stream.pty; then
          early_visible=1
          break
        fi
        kill -0 "$stream_pid" 2>/dev/null || break
        sleep 0.1
      done
      [ "$early_visible" -eq 1 ] \
        || fail "showOutput should make output visible before the wrapped command exits"
      kill -0 "$stream_pid" 2>/dev/null \
        || fail "EARLY output must be visible while the wrapped command is still running"
      wait "$stream_pid"
      grep -aFq 'LATE output' stream.pty \
        || fail "showOutput should preserve later command output"
      assert_no_capability_queries stream.pty

      run_pty sleep.pty '${sleepProbe}/bin/hostenv-spinner-sleep-probe'
      grep -aFq '( -O- )' sleep.pty \
        || fail "a completed deploy should make Neko yawn"
      grep -aFq 'zZ' sleep.pty \
        || fail "a completed deploy should leave Neko asleep"
      grep -aFq '✅ Deploy complete' sleep.pty \
        || fail "the sleeping final frame should carry the success status"
      assert_no_capability_queries sleep.pty

      # A successful deploy must still announce completion when animation is
      # unavailable (CI/log capture, TERM=dumb, etc.).
      TERM=dumb ${sleepProbe}/bin/hostenv-spinner-sleep-probe >sleep-plain.out 2>sleep-plain.err
      grep -Fq '✅ Deploy complete' sleep-plain.err \
        || fail "plain mode should print the deploy completion status"

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
      grep -aFq 'Quiet operati' narrow.pty \
        || fail "compact mode should truncate its title to avoid terminal wrapping"
      ! grep -aFq 'Quiet operation' narrow.pty \
        || fail "compact mode must not render an over-width title in full"
      if grep -aF $'\033[3A' narrow.pty >/dev/null; then
        fail "compact mode must not use multi-row cursor movement"
      fi
      assert_no_capability_queries narrow.pty

      COLUMNS=30 TERM=xterm-256color script -qefc \
        '${activationProbe}/bin/hostenv-spinner-activation-probe' narrow-activate.pty >/dev/null
      grep -aFq '=O.O=' narrow-activate.pty \
        || fail "compact Neko should react when activation begins"
      grep -aFq 'Activating' narrow-activate.pty \
        || fail "compact mode should switch to the activation title"
      ! grep -aFq '__HOSTENV_NEKO_ACTIVATE__' narrow-activate.pty \
        || fail "compact mode must not leak the internal activation marker"
      assert_no_capability_queries narrow-activate.pty

      COLUMNS=30 TERM=xterm-256color script -qefc \
        '${sleepProbe}/bin/hostenv-spinner-sleep-probe' narrow-sleep.pty >/dev/null
      grep -aFq 'zZ' narrow-sleep.pty \
        || fail "compact successful deploys should leave Neko asleep"
      grep -aFq 'Deploy co' narrow-sleep.pty \
        || fail "compact successful deploys should retain the completion status"
      assert_no_capability_queries narrow-sleep.pty

      echo ok > "$out"
    '';
}
