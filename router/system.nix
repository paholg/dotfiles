{
  resource = {
    routeros_disk_settings.settings = {
      auto_media_interface = "bridge";
      auto_media_sharing = true;
      auto_smb_sharing = true;
    };

    routeros_ip_ipsec_profile.default = {
      name = "default";
      dpd_interval = "2m";
      dpd_maximum_failures = 5;
    };

    routeros_ip_neighbor_discovery_settings.settings.discover_interface_list = "LAN";

    routeros_system_note.settings = {
      note = "";
      show_at_login = false;
    };

    routeros_tool_mac_server.settings.allowed_interface_list = "LAN";
    routeros_tool_mac_server_winbox.settings.allowed_interface_list = "LAN";
  };
}
