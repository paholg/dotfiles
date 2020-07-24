# ------------------------------------------------------------------------------
# Set path
path_dirs="
    $HOME/.cargo/bin
    $HOME/bin
"

for dir in $(echo $path_dirs); do
    export PATH=$dir:${PATH}
done

# ------------------------------------------------------------------------------

if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
    . $HOME/.nix-profile/etc/profile.d/nix.sh
fi
