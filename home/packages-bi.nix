{ pkgs, ... }:

{
  home.packages = with pkgs;
    [
      cargo-insta
      fpm
      just
      omnisharp-roslyn # c# lsp
      redis
      swagger-cli
      # sourcekit-lsp # swift lsp
    ];
}
