{ lib, ... }:
let
  mainConfig = "link";

  # The APs' radios, in MHz.
  frequencies = {
    main = {
      "2g" = 2412;
      "5g" = 5180;
    };
    up = {
      "2g" = 2462;
      "5g" = 5745;
    };
    garage = {
      "2g" = 2437;
      "5g" = 5180;
    };
  };

  bands = {
    "2g" = "2ghz-ax";
    "5g" = "5ghz-ax";
  };

  configuration =
    {
      name,
      ssid,
      ft ? false,
      channel ? null,
    }:
    {
      inherit name;
      ssid = "\${local.private.ssids[\"${ssid}\"]}";
      chains = [
        0
        1
      ];
      tx_chains = [
        0
        1
      ];
      disabled = false;
      datapath.bridge = "bridge";
      security = {
        authentication_types = "wpa2-psk";
        passphrase = "\${local.private.wifi_passphrase}";
      }
      // lib.optionalAttrs ft { ft = "true"; };
    }
    // lib.optionalAttrs (channel != null) { inherit channel; };

  perRadio = f: lib.concatMapAttrs (ap: lib.mapAttrs' (f ap)) frequencies;

  radioConfigurations = perRadio (
    ap: band: frequency:
    let
      name = "link-${ap}-${band}";
    in
    lib.nameValuePair name (configuration {
      inherit name;
      ssid = mainConfig;
      ft = true;
      channel = {
        frequency = toString frequency;
        skip_dfs_channels = "all";
      };
    })
  );

  apConfigurations = lib.genAttrs (map (ap: "link-${ap}") (lib.attrNames frequencies)) (
    name:
    configuration {
      inherit name;
      ssid = name;
    }
  );

  # RouterOS uses the first match, so `fallback` must stay last. New entries are
  # added after it, and need moving before it.
  radioProvisioning = perRadio (
    ap: band: _:
    lib.nameValuePair "${ap}-${band}" {
      action = "create-dynamic-enabled";
      identity_regexp = "^${ap}$";
      master_configuration = "link-${ap}-${band}";
      slave_configurations = [ "link-${ap}" ];
      supported_bands = [ bands.${band} ];
    }
  );
in
{
  resource = {
    routeros_wifi_configuration = radioConfigurations // apConfigurations // {
      ${mainConfig} = configuration {
        name = mainConfig;
        ssid = mainConfig;
        ft = true;
        channel.skip_dfs_channels = "all";
      };
    };

    routeros_wifi_cap.settings = {
      discovery_interfaces = [ "all" ];
      enabled = true;
    };

    routeros_wifi_capsman.settings = {
      enabled = true;
      interfaces = [ "bridge" ];
      package_path = "";
      require_peer_certificate = false;
      upgrade_policy = "suggest-same-version";
    };

    routeros_wifi_provisioning = radioProvisioning // {
      fallback = {
        action = "create-dynamic-enabled";
        comment = mainConfig;
        master_configuration = mainConfig;
      };
    };
  };
}
