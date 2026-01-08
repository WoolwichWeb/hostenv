Make a plan for a hostenv provider service. Explaining this is going to be a bit like explaining the plot of the film Inception, so bear with me. It:

1. Is written in Haskell.
2. Provides a REST API using whichever library you're easily able to code against.
3. At first will only support a single route and request type, which will be pinged from a gitlab/github webhook when the user pushes changes to their repo.
4. Has webhook URIs that are obscured using a hash, so it's hard for bad actors to guess what the hook for a project might be and spam it. `environments.<default>.hostenv.projectNameHash` may be used for this if necessary.
5. Is a Hostenv service module under `modules/features/` (with Haskell sources under `modules/services/hostenv-provider-service` and exposed via `flake.lib.provider.service`).
6. Providers may optionally add it to their setup in the same way anyone adds hostenv to their project.
7. Is managed by systemd like many other hostenv services.
8. Looks for provider files, like `flake.nix` and `generated/state.json`, in a given directory under $XDG_DATA_HOME on startup. If those files do not exist, it copies the contents of `environments.<name>.hostenv.root` to the given directory. I believe systemd units can be configured to ensure certain directories exist for service use, if not the service may also create the directory.
9. Has a small warning that secrets *in encrypted form* will end up in the Nix store on the machine where it is deployed somewhere in the docs. Not the same as the usual concern about secrets in the store, but it's notable.
10. Shells out to the command-line, probably using Turtle, to run `nix flake update` after the provider files are in place in the given directory. Then it fails early if that doesn't work, relaying the error message from Nix. Note: this broad update of all inputs should only happen on startup.
11. Begins listening for requests via the nginx Unix socket connecting it to the outside world after startup, then:

    1. When it receives a message from a webhook, the service shells out to the command-line and runs `nix flake update <org>__<project>`.
    2. Then it shells out to run `nix run .#hostenv-provider-plan` which generate `generated/plan.json`.
    3. Then it shells out to run `./provider/cli.hs dns-gate` to upsert hostenv DNS records and ensure any domains not directed at hostenv won't cause deployment failures.
    4. Then it inspects `generated/plan.json` to determine which nodes environments belonging to `<org>` and `<project>` are on.
    5. Then it shells out to run `./provider/cli.hs deploy --node=` providing a single node taken from the list made in step 4.
    6. It repeats step (5) until every node in the list has been deployed, and reports the results (success or failure).
