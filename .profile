
# ------------------------------------------------------------------------------
# Set path
path_dirs=(
    $HOME/.cargo/bin
    $HOME/bin
)

for dir in $path_dirs; do
    export PATH=$dir:${PATH}
done
