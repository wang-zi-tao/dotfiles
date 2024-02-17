TEMP=$(mktemp ./target/expand-XXXX.rs)
cargo expand $@ > $TEMP
nvr $TEMP
