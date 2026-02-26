# Issues

- Config mismatch risk: provider-service comin enabled while provider comin disabled (secrets/token-map expectations).
- Large dirty worktree detected; verify scope carefully per task.

- Nix builtins.tryEval does not expose failure messages; test validates the assertion message by evaluating with `_module.check = false` and inspecting `config.assertions`, while separately using tryEval to ensure the mismatch fails when checks are enabled.
