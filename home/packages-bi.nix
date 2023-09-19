{ pkgs, ... }:

{
  home.packages = with pkgs;
    [
      cargo-insta
      fpm
      glab # GitLab CLI
      just
      omnisharp-roslyn # c# lsp
      redis
      swagger-cli
      # sourcekit-lsp # swift lsp
    ];
}
