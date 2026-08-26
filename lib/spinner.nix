{
  title,
  command,
  showOutput ? false,
  showError ? false,
  variant ? "upload",
  activationMarker ? null,
  activationTitle ? "Activating...",
  finish ? "clear",
  successTitle ? "Done",
}:
# Why are we re-inventing bits of `gum spin` here? Gum 2.x uses Bubble Tea for
# its spinner, and Bubble Tea asks the terminal whether it supports modes 2026
# and 2027 even though Gum disables input for the spinner. Nobody is listening
# for the terminal's answers, so fast commands can leave replies such as
# `2026;4$y` behind for the user's shell to print later. See the upstream bug:
# https://github.com/charmbracelet/bubbletea/issues/1590
#
# Once we had to own a tiny renderer anyway, we made the best of it: this is an
# intentionally ridiculous Hostenv-flavoured homage to Masayuki Koba's 1990
# xneko. The heart is Neko's "mouse"; Neko chases it, reacts to the operation,
# gets startled when activation begins, and curls up once a deployment is done.
# Please do not replace this with Gum again without checking the issue above.
let
  bool = value: if value then "1" else "0";
  nullOrString = value: if value == null then "" else value;
  # Titles and markers are source-controlled shell strings and may intentionally
  # reference runtime variables such as $currentBranch.
  shellDoubleQuoted = builtins.replaceStrings [ "\\" "\"" ] [ "\\\\" "\\\"" ];
in
''
  _hostenv_spinner() {
    _hostenv_spinner_title="${shellDoubleQuoted title}"
    _hostenv_spinner_variant="${shellDoubleQuoted variant}"
    _hostenv_spinner_activation_marker="${shellDoubleQuoted (nullOrString activationMarker)}"
    _hostenv_spinner_activation_title="${shellDoubleQuoted activationTitle}"
    _hostenv_spinner_finish="${shellDoubleQuoted finish}"
    _hostenv_spinner_show_output=${bool showOutput}
    _hostenv_spinner_show_error=${bool showError}
    _hostenv_spinner_success_title="${shellDoubleQuoted successTitle}"
    _hostenv_spinner_output=""
    _hostenv_spinner_ready=0
    _hostenv_spinner_command_pid=""
    _hostenv_spinner_multiline=0
    _hostenv_spinner_activation_seen=0

    _hostenv_spinner_replay_output() {
      while IFS= read -r _hostenv_spinner_line || [ -n "$_hostenv_spinner_line" ]; do
        if [ -n "$_hostenv_spinner_activation_marker" ] \
          && [ "$_hostenv_spinner_line" = "$_hostenv_spinner_activation_marker" ]; then
          continue
        fi
        printf '%s\n' "$_hostenv_spinner_line"
      done < "$_hostenv_spinner_output"
    }

    _hostenv_spinner_clear() {
      [ "$_hostenv_spinner_ready" -eq 1 ] || return 0
      if [ "$_hostenv_spinner_multiline" -eq 1 ]; then
        # We finish on row four. Erase all four rows and return to the row where
        # the animation started. These are one-way drawing commands, not terminal
        # capability queries, so the terminal has nothing to reply to.
        printf '\r\033[2K\033[1A\r\033[2K\033[1A\r\033[2K\033[1A\r\033[2K' >&2
      else
        printf '\r\033[2K' >&2
      fi
      _hostenv_spinner_ready=0
    }

    # Keep the EXIT cleanup itself inline. ShellCheck cannot see that a function
    # referenced only through `trap` is invoked indirectly and reports SC2329.
    # `_hostenv_spinner_clear` is also called directly below, so it remains a
    # normal helper without relying on trap reachability analysis.
    trap '
      if [ -n "$_hostenv_spinner_command_pid" ]; then
        kill "$_hostenv_spinner_command_pid" 2>/dev/null || true
        wait "$_hostenv_spinner_command_pid" 2>/dev/null || true
        _hostenv_spinner_command_pid=""
      fi
      _hostenv_spinner_clear
      if [ -n "$_hostenv_spinner_output" ]; then
        rm -f "$_hostenv_spinner_output"
      fi
    ' EXIT

    _hostenv_spinner_columns="''${COLUMNS:-}"
    if ! [[ "$_hostenv_spinner_columns" =~ ^[0-9]+$ ]]; then
      _hostenv_spinner_size="$(stty size </dev/tty 2>/dev/null || true)"
      _hostenv_spinner_columns="''${_hostenv_spinner_size##* }"
    fi
    if ! [[ "$_hostenv_spinner_columns" =~ ^[0-9]+$ ]]; then
      _hostenv_spinner_columns=80
    fi

    if [ -z "''${NO_COLOR:-}" ]; then
      _hostenv_neko_pink=$'\033[38;5;212m'
      _hostenv_neko_cyan=$'\033[38;5;81m'
      _hostenv_neko_yellow=$'\033[38;5;220m'
      _hostenv_neko_red=$'\033[38;5;203m'
      _hostenv_neko_reset=$'\033[0m'
    else
      _hostenv_neko_pink=""
      _hostenv_neko_cyan=""
      _hostenv_neko_yellow=""
      _hostenv_neko_red=""
      _hostenv_neko_reset=""
    fi

    _hostenv_neko_compact_title() {
      _hostenv_neko_compact_title_value="$1"
      # Compact frames reserve sixteen visible columns for the sprite and
      # separator. Keep the full redraw within COLUMNS so a narrow terminal
      # never auto-wraps a frame that we can only erase as one line.
      _hostenv_neko_compact_title_width=$((_hostenv_spinner_columns - 16))
      if [ "$_hostenv_neko_compact_title_width" -lt 4 ]; then
        _hostenv_neko_compact_title_width=4
      fi
      if [ "''${#_hostenv_neko_compact_title_value}" -gt "$_hostenv_neko_compact_title_width" ]; then
        _hostenv_neko_compact_title_value="''${_hostenv_neko_compact_title_value:0:_hostenv_neko_compact_title_width-1}…"
      fi
      printf '%s' "$_hostenv_neko_compact_title_value"
    }

    _hostenv_neko_compact_frame() {
      if [ "$_hostenv_spinner_activation_seen" -eq 1 ]; then
        _hostenv_neko_compact='=O.O= !!⚡  '
        _hostenv_neko_compact_status="$(_hostenv_neko_compact_title "$_hostenv_spinner_activation_title")"
      else
        _hostenv_neko_compact_index=$((_hostenv_spinner_frame % 8))
        case "$_hostenv_neko_compact_index" in
          0) _hostenv_neko_compact='=^.^=  ♡     ' ;;
          1) _hostenv_neko_compact='=^.^=  ♡     ' ;;
          2) _hostenv_neko_compact=' =^.^=  ♡    ' ;;
          3) _hostenv_neko_compact=' =^.^=  ♡    ' ;;
          4) _hostenv_neko_compact='  =^.^=  ♡   ' ;;
          5) _hostenv_neko_compact='  =^.^=  ♡   ' ;;
          6) _hostenv_neko_compact='   =^.^=♡    ' ;;
          7) _hostenv_neko_compact='   =^-^=♡    ' ;;
        esac
        _hostenv_neko_compact_status="$(_hostenv_neko_compact_title "$_hostenv_spinner_title")"
      fi
      printf '\r\033[2K%s%s%s %s·%s %s' \
        "$_hostenv_neko_pink" "$_hostenv_neko_compact" "$_hostenv_neko_reset" \
        "$_hostenv_neko_cyan" "$_hostenv_neko_reset" "$_hostenv_neko_compact_status" >&2
      _hostenv_spinner_ready=1
    }

    _hostenv_neko_draw() {
      if [ "$_hostenv_neko_drawn" -eq 1 ]; then
        printf '\r\033[3A' >&2
      fi
      printf '\r\033[2K%s\n\r\033[2K%s\n\r\033[2K%s\n\r\033[2K%s' \
        "$1" "$2" "$3" "$4" >&2
      _hostenv_neko_drawn=1
      _hostenv_spinner_ready=1
    }

    _hostenv_neko_status() {
      _hostenv_neko_status_title="$1"
      _hostenv_neko_max_title=$((_hostenv_spinner_columns - 12))
      if [ "$_hostenv_neko_max_title" -lt 8 ]; then
        _hostenv_neko_max_title=8
      fi
      if [ "''${#_hostenv_neko_status_title}" -gt "$_hostenv_neko_max_title" ]; then
        _hostenv_neko_status_title="''${_hostenv_neko_status_title:0:_hostenv_neko_max_title-1}…"
      fi
      printf '%s%s%s · %s' \
        "$_hostenv_neko_cyan" 'hostenv' "$_hostenv_neko_reset" "$_hostenv_neko_status_title"
    }

    _hostenv_neko_render_upload() {
      # Neko moves one character every other frame. The paws still animate at
      # the full 10 FPS, giving the deliberately chunky xneko-style gait.
      _hostenv_neko_step=$((_hostenv_spinner_frame / 2))
      _hostenv_neko_pos=$((_hostenv_neko_step % 14))
      _hostenv_neko_gap=$((13 - _hostenv_neko_pos))
      _hostenv_neko_paw='>'
      if [ $((_hostenv_spinner_frame % 2)) -eq 1 ]; then
        _hostenv_neko_paw='/'
      fi
      printf -v _hostenv_neko_l1 "%*s /\\_/\\" "$_hostenv_neko_pos" ""
      _hostenv_neko_face='o.o'
      if [ "$_hostenv_neko_gap" -eq 0 ]; then
        _hostenv_neko_face='^.^'
      fi
      printf -v _hostenv_neko_l2 '%*s( %s )%s%*s%s♡%s' \
        "$_hostenv_neko_pos" "" "$_hostenv_neko_face" "$_hostenv_neko_paw" \
        "$_hostenv_neko_gap" "" "$_hostenv_neko_pink" "$_hostenv_neko_reset"
      printf -v _hostenv_neko_l3 "%*s /   \\" "$_hostenv_neko_pos" ""
      _hostenv_neko_l4="$(_hostenv_neko_status "$_hostenv_spinner_title")"
      _hostenv_neko_draw "$_hostenv_neko_l1" "$_hostenv_neko_l2" "$_hostenv_neko_l3" "$_hostenv_neko_l4"
    }

    _hostenv_neko_render_download() {
      _hostenv_neko_step=$((_hostenv_spinner_frame / 2))
      _hostenv_neko_pos=$((13 - (_hostenv_neko_step % 14)))
      _hostenv_neko_paw='<'
      if [ $((_hostenv_spinner_frame % 2)) -eq 1 ]; then
        _hostenv_neko_paw="\\"
      fi
      printf -v _hostenv_neko_l1 "%s♡%s%*s /\\_/\\" \
        "$_hostenv_neko_pink" "$_hostenv_neko_reset" "$_hostenv_neko_pos" ""
      _hostenv_neko_face='o.o'
      if [ "$_hostenv_neko_pos" -eq 0 ]; then
        _hostenv_neko_face='^.^'
      fi
      printf -v _hostenv_neko_l2 '%*s%s( %s )' \
        "$((_hostenv_neko_pos + 1))" "" "$_hostenv_neko_paw" "$_hostenv_neko_face"
      printf -v _hostenv_neko_l3 "%*s /   \\" "$((_hostenv_neko_pos + 1))" ""
      _hostenv_neko_l4="$(_hostenv_neko_status "$_hostenv_spinner_title")"
      _hostenv_neko_draw "$_hostenv_neko_l1" "$_hostenv_neko_l2" "$_hostenv_neko_l3" "$_hostenv_neko_l4"
    }

    _hostenv_neko_render_prepare() {
      _hostenv_neko_blink='o.o'
      if [ $((_hostenv_spinner_frame % 16)) -eq 15 ]; then
        _hostenv_neko_blink='-.-'
      fi
      _hostenv_neko_tail='~'
      if [ $((_hostenv_spinner_frame % 4)) -ge 2 ]; then
        _hostenv_neko_tail='⌁'
      fi
      _hostenv_neko_l1=' /\_/\                         ⌂'
      _hostenv_neko_l2="( $_hostenv_neko_blink )  $_hostenv_neko_tail · · · · · · · · ·"
      _hostenv_neko_l3=' > ^ <'
      _hostenv_neko_l4="$(_hostenv_neko_status "$_hostenv_spinner_title")"
      _hostenv_neko_draw "$_hostenv_neko_l1" "$_hostenv_neko_l2" "$_hostenv_neko_l3" "$_hostenv_neko_l4"
    }

    _hostenv_neko_render_build() {
      case $((_hostenv_spinner_frame % 8)) in
        0|7) _hostenv_neko_build_thing='·' ;;
        1|6) _hostenv_neko_build_thing='✧' ;;
        2|5) _hostenv_neko_build_thing='✦' ;;
        3|4) _hostenv_neko_build_thing='❄' ;;
      esac
      _hostenv_neko_l1=" /\\_/\\"
      _hostenv_neko_l2="( o.o )     $_hostenv_neko_cyan$_hostenv_neko_build_thing$_hostenv_neko_reset"
      if [ $((_hostenv_spinner_frame % 2)) -eq 0 ]; then
        _hostenv_neko_l3=' > ^ <'
      else
        _hostenv_neko_l3='  >^< '
      fi
      _hostenv_neko_l4="$(_hostenv_neko_status "$_hostenv_spinner_title")"
      _hostenv_neko_draw "$_hostenv_neko_l1" "$_hostenv_neko_l2" "$_hostenv_neko_l3" "$_hostenv_neko_l4"
    }

    _hostenv_neko_render_activate() {
      _hostenv_neko_bolt='⚡'
      if [ $((_hostenv_spinner_frame % 2)) -eq 1 ]; then
        _hostenv_neko_bolt=' ⚡'
      fi
      _hostenv_neko_l1=" /\\_/\\"
      _hostenv_neko_l2="( O.O )  $_hostenv_neko_yellow!! $_hostenv_neko_bolt$_hostenv_neko_reset"
      _hostenv_neko_l3=" /| |\\"
      _hostenv_neko_l4="$(_hostenv_neko_status "$_hostenv_spinner_activation_title")"
      _hostenv_neko_draw "$_hostenv_neko_l1" "$_hostenv_neko_l2" "$_hostenv_neko_l3" "$_hostenv_neko_l4"
    }

    _hostenv_neko_render_sync() {
      case $((_hostenv_spinner_frame % 4)) in
        0) _hostenv_neko_pencil='✎' ;;
        1) _hostenv_neko_pencil=' ✎' ;;
        2) _hostenv_neko_pencil='  ✎' ;;
        3) _hostenv_neko_pencil=' ✎' ;;
      esac
      _hostenv_neko_l1=" /\\_/\\"
      _hostenv_neko_l2="( o.o )っ $_hostenv_neko_pink$_hostenv_neko_pencil$_hostenv_neko_reset  hostenv.nix"
      _hostenv_neko_l3=' > ^ <'
      _hostenv_neko_l4="$(_hostenv_neko_status "$_hostenv_spinner_title")"
      _hostenv_neko_draw "$_hostenv_neko_l1" "$_hostenv_neko_l2" "$_hostenv_neko_l3" "$_hostenv_neko_l4"
    }

    _hostenv_neko_render_failure() {
      _hostenv_neko_draw \
        " /\\_/\\" \
        "( O.O )  $_hostenv_neko_red!! ×$_hostenv_neko_reset" \
        " /| |\\" \
        "$(_hostenv_neko_status "Something went wrong")"
    }

    _hostenv_neko_finish_sleep() {
      # xneko settles down when the mouse stops moving. Our mouse is the pink
      # deployment heart: catch it, yawn, then leave Neko asleep in scrollback.
      _hostenv_neko_drawn=0
      _hostenv_neko_draw \
        " /\\_/\\" \
        "( ^.^ )っ$_hostenv_neko_pink♡$_hostenv_neko_reset" \
        ' > ^ <' \
        "$(_hostenv_neko_status "$_hostenv_spinner_success_title")"
      sleep 0.2
      _hostenv_neko_draw \
        " /\\_/\\" \
        "( -O- )  $_hostenv_neko_pink♡$_hostenv_neko_reset" \
        ' > ^ <' \
        "$(_hostenv_neko_status "$_hostenv_spinner_success_title")"
      sleep 0.2
      _hostenv_neko_draw \
        " /\\_/\\" \
        "( -.- )  zZ  $_hostenv_neko_pink♡$_hostenv_neko_reset" \
        " /     \\" \
        "$(_hostenv_neko_status "$_hostenv_spinner_success_title")"
      # Commit the final frame to scrollback instead of erasing it. The command
      # prompt begins below a sleeping Neko rather than on top of the sprite.
      printf '\n' >&2
      _hostenv_spinner_ready=0
    }

    _hostenv_neko_finish_sleep_compact() {
      _hostenv_neko_compact_status="$(_hostenv_neko_compact_title "$_hostenv_spinner_success_title")"
      printf '\r\033[2K%s%s%s %s·%s %s' \
        "$_hostenv_neko_pink" '=^.^=♡       ' "$_hostenv_neko_reset" \
        "$_hostenv_neko_cyan" "$_hostenv_neko_reset" "$_hostenv_neko_compact_status" >&2
      sleep 0.2
      printf '\r\033[2K%s%s%s %s·%s %s' \
        "$_hostenv_neko_pink" '=-O-= ♡      ' "$_hostenv_neko_reset" \
        "$_hostenv_neko_cyan" "$_hostenv_neko_reset" "$_hostenv_neko_compact_status" >&2
      sleep 0.2
      printf '\r\033[2K%s%s%s %s·%s %s\n' \
        "$_hostenv_neko_pink" '=-.-= zZ ♡   ' "$_hostenv_neko_reset" \
        "$_hostenv_neko_cyan" "$_hostenv_neko_reset" "$_hostenv_neko_compact_status" >&2
      _hostenv_spinner_ready=0
    }

    _hostenv_spinner_filter_output() {
      _hostenv_spinner_filter_line=""
      while IFS= read -r _hostenv_spinner_filter_line || [ -n "$_hostenv_spinner_filter_line" ]; do
        if [ -n "$_hostenv_spinner_activation_marker" ] \
          && [ "$_hostenv_spinner_filter_line" = "$_hostenv_spinner_activation_marker" ]; then
          continue
        fi
        printf '%s\n' "$_hostenv_spinner_filter_line"
      done
    }

    _hostenv_spinner_emit_output_line() {
      _hostenv_spinner_line="$1"
      if [ -n "$_hostenv_spinner_activation_marker" ] \
        && [ "$_hostenv_spinner_line" = "$_hostenv_spinner_activation_marker" ]; then
        _hostenv_spinner_activation_seen=1
        return 0
      fi

      if [ "$_hostenv_spinner_show_output" -eq 1 ]; then
        _hostenv_spinner_clear
        _hostenv_neko_drawn=0
        printf '%s\n' "$_hostenv_spinner_line"
      fi
    }

    _hostenv_spinner_drain_output() {
      while :; do
        _hostenv_spinner_line=""
        if IFS= read -r -u "$_hostenv_spinner_output_fd" _hostenv_spinner_line; then
          _hostenv_spinner_line="$_hostenv_spinner_output_partial$_hostenv_spinner_line"
          _hostenv_spinner_output_partial=""
          _hostenv_spinner_emit_output_line "$_hostenv_spinner_line"
          continue
        fi
        if [ -n "$_hostenv_spinner_line" ]; then
          _hostenv_spinner_output_partial="$_hostenv_spinner_output_partial$_hostenv_spinner_line"
        fi
        break
      done
    }

    _hostenv_spinner_finish_output() {
      _hostenv_spinner_drain_output
      if [ -n "$_hostenv_spinner_output_partial" ]; then
        _hostenv_spinner_emit_output_line "$_hostenv_spinner_output_partial"
        _hostenv_spinner_output_partial=""
      fi
    }

    _hostenv_spinner_can_animate=0
    if [ -t 2 ] && [ "''${TERM:-}" != "dumb" ] && [ "$_hostenv_spinner_columns" -ge 24 ]; then
      _hostenv_spinner_can_animate=1
      if [ "$_hostenv_spinner_columns" -ge 48 ]; then
        _hostenv_spinner_multiline=1
      fi
    fi

    if [ "$_hostenv_spinner_can_animate" -eq 0 ]; then
      printf '%s\n' "$_hostenv_spinner_title" >&2

      # Gum passed child output straight through when it was not rendering on a
      # TTY. Keep that useful logging behaviour; only interpose a tiny line
      # filter when an internal activation marker must be hidden.
      if [ -n "$_hostenv_spinner_activation_marker" ]; then
        if (
          ${command}
        ) | _hostenv_spinner_filter_output; then
          _hostenv_spinner_status=0
        else
          _hostenv_spinner_pipe_status=("''${PIPESTATUS[@]}")
          if [ "''${_hostenv_spinner_pipe_status[0]}" -ne 0 ]; then
            _hostenv_spinner_status="''${_hostenv_spinner_pipe_status[0]}"
          else
            _hostenv_spinner_status="''${_hostenv_spinner_pipe_status[1]}"
          fi
        fi
      elif (
        ${command}
      ); then
        _hostenv_spinner_status=0
      else
        _hostenv_spinner_status=$?
      fi

      if [ "$_hostenv_spinner_status" -eq 0 ] && [ "$_hostenv_spinner_finish" = "sleep" ]; then
        printf '%s\n' "$_hostenv_spinner_success_title" >&2
      fi
      return "$_hostenv_spinner_status"
    fi

    _hostenv_spinner_frame=0
    _hostenv_neko_drawn=0
    _hostenv_spinner_output="$(mktemp)"
    _hostenv_spinner_output_partial=""
    exec {_hostenv_spinner_output_fd}<"$_hostenv_spinner_output"

    # Run the wrapped operation in the background and keep Hostneko's renderer
    # in this shell. Besides making the animation state genuinely persistent,
    # this avoids relying on shell variables crossing a subshell boundary.
    # ShellCheck's SC2030/SC2031 warnings are useful here: a backgrounded
    # renderer really would lose changes to the frame counter and draw state.
    (
      ${command}
    ) >|"$_hostenv_spinner_output" 2>&1 &
    _hostenv_spinner_command_pid=$!

    while kill -0 "$_hostenv_spinner_command_pid" 2>/dev/null; do
      _hostenv_spinner_drain_output
      if [ "$_hostenv_spinner_multiline" -eq 0 ]; then
        _hostenv_neko_compact_frame
      elif [ "$_hostenv_spinner_activation_seen" -eq 1 ]; then
        _hostenv_neko_render_activate
      else
        case "$_hostenv_spinner_variant" in
          prepare) _hostenv_neko_render_prepare ;;
          download) _hostenv_neko_render_download ;;
          build) _hostenv_neko_render_build ;;
          sync) _hostenv_neko_render_sync ;;
          *) _hostenv_neko_render_upload ;;
        esac
      fi
      _hostenv_spinner_frame=$((_hostenv_spinner_frame + 1))
      sleep 0.1
    done

    if wait "$_hostenv_spinner_command_pid"; then
      _hostenv_spinner_status=0
    else
      _hostenv_spinner_status=$?
    fi
    _hostenv_spinner_command_pid=""
    _hostenv_spinner_finish_output
    _hostenv_spinner_clear

    if [ "$_hostenv_spinner_status" -ne 0 ] && [ "$_hostenv_spinner_can_animate" -eq 1 ] \
      && [ "$_hostenv_spinner_multiline" -eq 1 ]; then
      _hostenv_neko_drawn=0
      _hostenv_neko_render_failure
      sleep 0.3
      _hostenv_spinner_clear
    fi

    if [ "$_hostenv_spinner_show_output" -eq 0 ] \
      && [ "$_hostenv_spinner_status" -ne 0 ] \
      && [ "$_hostenv_spinner_show_error" -eq 1 ]; then
      _hostenv_spinner_replay_output
    fi

    if [ "$_hostenv_spinner_status" -eq 0 ] && [ "$_hostenv_spinner_finish" = "sleep" ]; then
      if [ "$_hostenv_spinner_multiline" -eq 1 ]; then
        _hostenv_neko_finish_sleep
      else
        _hostenv_neko_finish_sleep_compact
      fi
    fi

    return "$_hostenv_spinner_status"
  }

  # Run the ordinary function in a subshell rather than making the function
  # body itself a `( ... )` group. The isolation keeps its EXIT trap and helper
  # functions private to this spinner invocation, while the normal function
  # body lets ShellCheck correctly see that Hostneko's frame/draw state is read
  # and written in one shell (avoiding false SC2030/SC2031 subshell warnings).
  (
    _hostenv_spinner
  )
''
