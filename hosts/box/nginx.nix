{ ... }:
{
  security.acme = {
    acceptTerms = true;
    defaults.email = "paho@paholg.com";
  };

  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
  };
}
