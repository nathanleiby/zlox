set -e

CYAN='\033[0;36m'
NC='\033[0m' # No Color

zig build

for x in `ls examples/*.lox`; do
    echo "${CYAN}=== EXAMPLE: $x ===${NC}"
    ./zig-out/bin/zlox $x
    echo ""
done;
