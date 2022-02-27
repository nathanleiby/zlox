set -e
for x in `ls examples/*.lox`; do
    echo "=== TEST: $x ==="
    zig run main.zig -- $x
    echo ""
done;

