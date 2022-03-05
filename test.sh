set -e

CYAN='\033[0;36m'
NC='\033[0m' # No Color

for x in `ls src/*.zig`; do
    echo "${CYAN}=== UNIT TEST: $x ===${NC}"
    zig test $x
    echo ""
done;
