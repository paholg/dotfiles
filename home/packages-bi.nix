{ pkgs, ... }:

{
  home.packages = with pkgs;
    [
      cargo-insta
      fpm
      just
      omnisharp-roslyn # c# lsp
      redis
      # sourcekit-lsp # swift lsp
    ];
}
