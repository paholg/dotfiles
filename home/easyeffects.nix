{
  config,
  pkgs,
  lib,
  ...
}:
{
  services.easyeffects = {
    enable = true;
    # Loaded via --load-preset on service start.
    preset = "zoom-voice";
  };

  # EasyEffects has only a blocklist, no allowlist. So, we turn it off for all
  # inputs and outputs. Only programs that select it specifically will use it.
  home.activation.easyeffectsRouting = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    run ${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 \
      --file "$HOME/.config/easyeffects/db/easyeffectsrc" \
      --group EffectsPipelines --key processAllOutputs false
    run ${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 \
      --file "$HOME/.config/easyeffects/db/easyeffectsrc" \
      --group EffectsPipelines --key processAllInputs false
  '';

  # Playback cleanup for far-end speech recorded on a distant webcam mic:
  # high-pass -> speex dereverb -> EQ (low-mid cut, presence boost) -> limiter.
  xdg.dataFile."easyeffects/output/zoom-voice.json".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home/easyeffects/zoom-voice.json";

}
