{ pkgs, ... }:

{
  imports = [
    ../home/common.nix
    ../home/common-linux.nix
    ../home/gui.nix
    ../home/packages-gui.nix
    ../home/packages-gui-linux.nix
  ];

  home = {
    username = "paho";
    homeDirectory = "/home/paho";
    sessionVariables = {
      GOPATH = "$HOME/go";
      ZEROPW = "$GOPATH/src/gitlab.com/zeropw/zero";
      BI_CRYPTO_PROVIDER_HAL = "true";
      AWS_PROFILE = "development";
    };
  };

  programs.git = {
    userEmail = "paho.lurie-gregg@beyondidentity.com";
    signing = {
      gpgPath = "/opt/beyond-identity/bin/gpg-bi";
      key = "DF4EEF201EA52C82651B3015CB98DCF706D022DC";
      signByDefault = true;
    };
  };

  programs.ssh.matchBlocks = {
    enclave = {
      hostname = "172.20.132.57";
      user = "ec2-user";
      identityFile = "/home/paho/.ssh/enclave-key.pem";
    };

    enclave2 = {
      hostname = "172.20.128.135";
      user = "ubuntu";
      identityFile = "/home/paho/.ssh/enclave-key.pem";
    };
  };

  home.packages = with pkgs; [
    awscli2
    docker-compose
    go
    mercurial
    nodejs
    yarn
  ];
}
