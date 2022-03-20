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
      CARGO_REGISTRY_AUTH_URL = "$(cat $HOME/.git-credentials)";
    };
  };

  programs.git = {
    userEmail = "paho.lurie-gregg@beyondidentity.com";
    signing = {
      gpgPath = "/opt/beyond-identity/bin/gpg-bi";
      key = "0A0862F72BBF7DDBEC6DCB6A8B7CC784EDC9150D";
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

  programs.zsh.shellAliases = {
    ns = ''
      function _ns() { nix-shell -p pkgconfig openssl tpm2-tss sqlite --run ""$@"" }; _ns'';
    cb = ''ns "cargo build"'';
    cr = ''ns "cargo run"'';
    ct = ''ns "cargo test"'';
  };

  home.packages = with pkgs; [
    awscli2
    docker-compose
    fpm
    go
    gopls
    mercurial
    nodejs
    sqlitebrowser
    tpm2-tss
    yarn
  ];
}
