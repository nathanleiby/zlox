set -e

CYAN='\033[0;36m'
NC='\033[0m' # No Color

for x in `ls *.zig`; do
    echo "${CYAN}=== TEST: $x ===${NC}"
    zig test $x
    echo ""
done;
