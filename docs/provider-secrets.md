# Managing Provider Secrets

Hostenv stores provider secrets in a SOPS-encrypted YAML file. By default this
is `secrets/secrets.yaml` in the provider repository.

This guide describes one straightforward setup using `age`: an operator has a
personal age key for editing the file, and each provider node is added as a
recipient using its existing Ed25519 SSH host key.

The same commands work on NixOS and on Ubuntu with Nix installed. Run them from
the provider repository. The provider development shell already includes
`sops` and `age`:

```bash
nix develop
```

This guide intentionally does not try to cover every way SOPS can be used. For
a broader introduction to SOPS and age, see
[Vimjoyer's SOPS/age tutorial](https://www.youtube.com/watch?v=G5f6GC7SnhU).

## 1. Create an operator age key

Create a key for the person who will edit the secrets file:

```bash
mkdir -p ~/.config/sops/age
age-keygen -o ~/.config/sops/age/keys.txt
chmod 600 ~/.config/sops/age/keys.txt
```

Print its public key:

```bash
age-keygen -y ~/.config/sops/age/keys.txt
```

The result starts with `age1`. Keep the private key in
`~/.config/sops/age/keys.txt` private and backed up. The public `age1...` value
is safe to put in the provider repository.

## 2. Get an age recipient for each provider node

Hostenv provider nodes use `/etc/ssh/ssh_host_ed25519_key` to decrypt SOPS
secrets by default. Add the corresponding public host key as a SOPS recipient.

Obtain the public key from the node using a trusted console or SSH connection:

```bash
ssh root@node-a 'cat /etc/ssh/ssh_host_ed25519_key.pub' > /tmp/node-a.pub
```

Convert it to an age recipient:

```bash
nix run nixpkgs#ssh-to-age -- -i /tmp/node-a.pub
```

Repeat this for every provider node that needs to decrypt the provider secrets.

## 3. Create `.sops.yaml`

In the root of the provider repository, create `.sops.yaml` containing the
operator key and each node recipient:

```yaml
keys:
  - &operator age1REPLACE_WITH_OPERATOR_PUBLIC_KEY
  - &node_a age1REPLACE_WITH_NODE_A_RECIPIENT
  - &node_b age1REPLACE_WITH_NODE_B_RECIPIENT

creation_rules:
  - path_regex: ^secrets/secrets\.yaml$
    key_groups:
      - age:
          - *operator
          - *node_a
          - *node_b
```

If you only have one provider node, omit `node_b`.

Commit `.sops.yaml`. It contains public recipients, not private keys.

## 4. Create the secrets file

Create the directory and open the encrypted file with SOPS:

```bash
mkdir -p secrets
sops secrets/secrets.yaml
```

For a provider with public project repositories and one Laravel project, a
minimal useful example could look like this while you are editing it:

```yaml
# Nix reads this file for access tokens when fetching private flake inputs.
# Leave it empty when all project inputs are public.
access_tokens: ""

# Secrets shared by every environment in the acme/website project.
acme_website:
  laravel_env: |
    APP_KEY=base64:replace-with-the-real-application-key
    MAIL_USERNAME=replace-me
    MAIL_PASSWORD=replace-me

  # Include these when backups are enabled for the project.
  backups_secret: replace-with-restic-repository-password
  backups_env: |
    AWS_ACCESS_KEY_ID=replace-me
    AWS_SECRET_ACCESS_KEY=replace-me
```

When you save and close the editor, SOPS encrypts the values before writing
`secrets/secrets.yaml` to disk. Commit the encrypted file to the provider
repository.

If provider builds need access to private GitLab inputs, `access_tokens` is a
Nix configuration fragment rather than just the token itself. For example:

```yaml
access_tokens: |
  access-tokens = gitlab.com=PAT:replace-with-token
```

Use the token format required by the Nix version and Git hosting service you are
using.

If the provider binary cache is enabled, the top-level SOPS file must also
contain its configured cache authentication password:

```yaml
cache_auth_password: replace-with-cache-password
```

## 5. Choose the scope of application secrets

Hostenv looks for application secrets from most specific to least specific:

```text
<environment-user>
<organisation>_<project>
<organisation>
```

For example:

```yaml
# Available only to this environment.
acme-website-main-a1b2c3d:
  laravel_env: |
    APP_KEY=base64:environment-specific-key

# Used by environments in acme/website unless overridden above.
acme_website:
  laravel_env: |
    APP_KEY=base64:project-wide-key

# Used by projects in the acme organisation unless overridden above.
acme:
  laravel_env: |
    APP_KEY=base64:organisation-wide-key
```

Use the narrowest scope that is practical. In particular, application keys or
credentials that should differ between production and testing belong at
environment scope.

Laravel requests `laravel_env` automatically. Other services may request their
own provider-managed secret names through Hostenv configuration. Backups use
`backups_secret` and `backups_env`.

## 6. Edit secrets later

Open the encrypted file normally with SOPS:

```bash
sops secrets/secrets.yaml
```

SOPS decrypts it for the editor and writes the encrypted form back when you
save and exit.

You can check that your operator key can decrypt the file without writing a
plaintext copy:

```bash
sops --decrypt secrets/secrets.yaml > /dev/null
```

Do not commit a manually decrypted copy of the secrets file.

## 7. Add or remove a provider node

When adding a node, convert its Ed25519 SSH host public key to an age recipient
and add that recipient to `.sops.yaml`. Then update the encrypted file's
recipients:

```bash
sops updatekeys secrets/secrets.yaml
```

Do the same after removing a node recipient. Commit both `.sops.yaml` and the
updated encrypted secrets file.

A node that is not a recipient cannot decrypt the provider secrets and should
not be expected to deploy successfully.

## What this guide assumes

This workflow deliberately assumes:

- SOPS with age rather than PGP, KMS, or another SOPS backend;
- an operator age key stored in `~/.config/sops/age/keys.txt`;
- provider nodes with stable Ed25519 SSH host keys;
- the default Hostenv provider secrets path, `secrets/secrets.yaml`;
- direct management of the encrypted file in the provider Git repository.

For more advanced key-management arrangements, use the upstream SOPS and
sops-nix documentation rather than treating this example as a complete SOPS
reference.
