set -e

CYAN='\033[0;36m'
NC='\033[0m' # No Color

for x in `ls examples/*.lox`; do
    echo "${CYAN}=== EXAMPLE: $x ===${NC}"
    zig run main.zig -- $x
    echo ""
done;
