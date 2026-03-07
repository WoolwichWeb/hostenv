{ ... }:
{
  provider.service = {
    organisation = "demo";
    project = "provider-service";
    environmentName = "provider-service";
  };

  services.hostenv-provider = {
    enable = true;
    deploy.enable = true;
    webhookHost = "provider.demo.hostenv.test";
    uiBasePath = "/dashboard";
  };

  environments."provider-service" = {
    enable = true;
    type = "testing";

    virtualHosts."provider.demo.hostenv.test" = {
      enableLetsEncrypt = false;
      globalRedirect = null;
    };

    users.demo = {
      email = "demo@example.test";
      publicKeys = [
        "SSH_PUBLIC_KEY_PLACEHOLDER"
      ];
    };
  };
}
