{pkgs, ...}: {
  home.packages = with pkgs; [
    cargo-insta
    fpm
    glab # GitLab CLI
    just
    kubeconform
    omnisharp-roslyn # c# lsp
    redis
    swagger-cli
    # sourcekit-lsp # swift lsp
  ];
}
