#!/bin/sh
# export LD_LIBRARY_PATH="`nix-env -q --out-path curl | awk '{print $2}'`/lib:$LD_LIBRARY_PATH"
# echo LDPATH is $LD_LIBRARY_PATH
# exec ./dist/build/vkq/vkq "$@"


LD_LIBRARY_PATH="`nix-env -q --out-path openssl | awk '{print $2}'`/lib:$LD_LIBRARY_PATH"
LD_LIBRARY_PATH="`nix-env -q --out-path zlib | awk '{print $2}'`/lib:$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH

echo $LD_LIBRARY_PATH

exec "$@"
