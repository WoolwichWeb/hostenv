{ pkgs, makeHostenv }:
let
  lib = pkgs.lib;
  asserts = (import ../support { inherit pkgs lib; }).asserts;

  mkEnv = modules:
    makeHostenv (
      [
        ({ ... }: {
          hostenv = {
            organisation = "acme";
            project = "canonical";
            hostenvHostname = "hosting.test";
            root = ./.;
          };
          environments.main = {
            enable = true;
            type = "production";
          };
        })
      ]
      ++ modules
    ) "main";

  cfg = env: env.config.environments.main;
  envHost = env: (cfg env).hostenv.hostname;
  envRedirect = env: (cfg env).virtualHosts.${envHost env}.globalRedirect;

  noCustom = mkEnv [ ];

  singleHost = mkEnv [
    {
      environments.main.virtualHosts."www.example.com" = { };
    }
  ];

  apexToWww = mkEnv [
    {
      environments.main.virtualHosts = {
        "example.com".globalRedirect = "www.example.com";
        "www.example.com" = { };
      };
    }
  ];

  wwwToApex = mkEnv [
    {
      environments.main.virtualHosts = {
        "www.example.com".globalRedirect = "example.com";
        "example.com" = { };
      };
    }
  ];

  multipleWithUniqueRedirectTarget = mkEnv [
    {
      environments.main.virtualHosts = {
        "georgina.ca".globalRedirect = "www.georgina.ca";
        "www.georgina.ca" = { };
        "exploregeorgina.ca" = { };
        "www.exploregeorgina.ca" = { };
        "choosegeorgina.ca" = { };
        "www.choosegeorgina.ca" = { };
        "georginavillagemuseum.ca".locations."/".return =
          "301 https://www.georgina.ca/things-do/georgina-village-museum";
        "www.georginavillagemuseum.ca".locations."/".return =
          "301 https://www.georgina.ca/things-do/georgina-village-museum";
      };
    }
  ];

  letsEncryptFallback = mkEnv [
    {
      environments.main.virtualHosts = {
        "a-disabled.example.com".enableLetsEncrypt = false;
        "b-enabled.example.com" = { };
      };
    }
  ];

  noEligibleCustomHost = mkEnv [
    {
      environments.main.virtualHosts."www.example.com".enableLetsEncrypt = false;
    }
  ];

  redirectTargetWithoutLetsEncrypt = mkEnv [
    {
      environments.main.virtualHosts = {
        "alias.example.com".globalRedirect = "www.example.com";
        "www.example.com".enableLetsEncrypt = false;
        "other.example.com" = { };
      };
    }
  ];

  historicalFallback = mkEnv [
    {
      environments.main.virtualHosts = {
        "z.example.com" = { };
        "a.example.com" = { };
      };
    }
  ];

  explicitCanonical = mkEnv [
    {
      environments.main.virtualHosts = {
        "a.example.com" = { };
        "b.example.com" = {
          canonical = true;
          enableLetsEncrypt = false;
        };
      };
    }
  ];

  explicitOptOut = mkEnv [
    {
      environments.main.virtualHosts."www.example.com" = { };
    }
    ({ config, lib, ... }: {
      environments.main.virtualHosts.${config.environments.main.hostenv.hostname}.globalRedirect =
        lib.mkForce null;
    })
  ];

  multipleCanonicalEval = builtins.tryEval (
    builtins.deepSeq
      (cfg (mkEnv [
        {
          environments.main.virtualHosts = {
            "a.example.com".canonical = true;
            "b.example.com".canonical = true;
          };
        }
      ])).canonicalHost
      true
  );

  redirectingCanonicalEval = builtins.tryEval (
    builtins.deepSeq
      (cfg (mkEnv [
        {
          environments.main.virtualHosts = {
            "a.example.com" = {
              canonical = true;
              globalRedirect = "b.example.com";
            };
            "b.example.com" = { };
          };
        }
      ])).canonicalHost
      true
  );
in
{
  canonical-host-no-custom-vhost =
    asserts.assertTrue "canonical-host-no-custom-vhost"
      (
        (cfg noCustom).canonicalHost == envHost noCustom
        && envRedirect noCustom == null
      )
      "an environment with no custom vhost should remain canonical on, and serve, its Hostenv URL";

  canonical-host-single-custom-vhost =
    asserts.assertTrue "canonical-host-single-custom-vhost"
      (
        (cfg singleHost).canonicalHost == "www.example.com"
        && envRedirect singleHost == "www.example.com"
      )
      "a single TLS-enabled custom vhost should become canonical and receive the Hostenv URL redirect";

  canonical-host-apex-to-www =
    asserts.assertTrue "canonical-host-apex-to-www"
      (
        (cfg apexToWww).canonicalHost == "www.example.com"
        && envRedirect apexToWww == "www.example.com"
      )
      "a configured apex-to-www redirect should make the www host canonical";

  canonical-host-www-to-apex =
    asserts.assertTrue "canonical-host-www-to-apex"
      (
        (cfg wwwToApex).canonicalHost == "example.com"
        && envRedirect wwwToApex == "example.com"
      )
      "a configured www-to-apex redirect should make the apex host canonical";

  canonical-host-multiple-unique-redirect-target =
    asserts.assertTrue "canonical-host-multiple-unique-redirect-target"
      (
        (cfg multipleWithUniqueRedirectTarget).canonicalHost == "www.georgina.ca"
        && envRedirect multipleWithUniqueRedirectTarget == "www.georgina.ca"
      )
      "a unique eligible globalRedirect target should win over other serving vhosts";

  canonical-host-lets-encrypt-inference =
    asserts.assertTrue "canonical-host-lets-encrypt-inference"
      (
        (cfg letsEncryptFallback).canonicalHost == "b-enabled.example.com"
        && envRedirect letsEncryptFallback == "b-enabled.example.com"
        && (cfg noEligibleCustomHost).canonicalHost == envHost noEligibleCustomHost
        && envRedirect noEligibleCustomHost == null
        && (cfg redirectTargetWithoutLetsEncrypt).canonicalHost == "other.example.com"
        && envRedirect redirectTargetWithoutLetsEncrypt == "other.example.com"
      )
      "implicit canonical-host inference should ignore custom vhosts and redirect targets without Let's Encrypt";

  canonical-host-historical-fallback =
    asserts.assertTrue "canonical-host-historical-fallback"
      (
        (cfg historicalFallback).canonicalHost == "a.example.com"
        && envRedirect historicalFallback == "a.example.com"
      )
      "ambiguous eligible custom hosts should retain the historical deterministic attr-name fallback";

  canonical-host-explicit =
    asserts.assertTrue "canonical-host-explicit"
      (
        (cfg explicitCanonical).canonicalHost == "b.example.com"
        && envRedirect explicitCanonical == "b.example.com"
      )
      "canonical=true should explicitly select a host even when automatic Let's Encrypt inference would exclude it";

  canonical-host-explicit-hostenv-redirect-opt-out =
    asserts.assertTrue "canonical-host-explicit-hostenv-redirect-opt-out"
      (
        (cfg explicitOptOut).canonicalHost == "www.example.com"
        && envRedirect explicitOptOut == null
      )
      "mkForce null on the generated Hostenv vhost should disable its redirect without changing canonicalHost";

  canonical-host-rejects-multiple-explicit =
    asserts.assertTrue "canonical-host-rejects-multiple-explicit"
      (!multipleCanonicalEval.success)
      "an environment must not allow more than one canonical=true vhost";

  canonical-host-rejects-redirecting-explicit =
    asserts.assertTrue "canonical-host-rejects-redirecting-explicit"
      (!redirectingCanonicalEval.success)
      "a canonical=true vhost must not itself globalRedirect elsewhere";
}
