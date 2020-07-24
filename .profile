# ------------------------------------------------------------------------------
# Set path
path_dirs="
    $HOME/.cargo/bin
    $HOME/bin
"

echo $path_dirs | tr ' ' '\n' | while read dir; do
    if [ -n "$dir" ]; then
        export PATH=$dir:${PATH}
    fi
done

# ------------------------------------------------------------------------------

if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
    . $HOME/.nix-profile/etc/profile.d/nix.sh
fi
