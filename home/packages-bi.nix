{ pkgs, ... }:

{
  home.packages = with pkgs;
    [
      cargo-insta
      just
      omnisharp-roslyn # c# lsp
      redis
      sourcekit-lsp # swift lsp
      swift
    ];
}
