{
  imports = [
    ./firewall.nix
    ./network.nix
    ./system.nix
    ./wifi.nix
  ];

  terraform.required_providers.routeros.source = "terraform-routeros/routeros";

  # Credentials come from ROS_USERNAME and ROS_PASSWORD.
  provider.routeros = {
    hosturl = "https://10.0.0.1";
    # The router's self-signed certificate.
    ca_certificate = "tofu-api.crt";
  };

  # JSON with keys: admin_mac, ap_macs, ssids, wifi_passphrase.
  variable.router_private = {
    type = "string";
    sensitive = true;
  };

  locals.private = "\${jsondecode(var.router_private)}";
}
