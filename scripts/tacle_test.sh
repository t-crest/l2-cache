#!/usr/bin/env bash

# This script should be copied to the TACLe bench repo directory of a specific benchmark type.
# E.g. this script can be copied to the kernel benchmarks directory of the TACLe benchmark suite, and it will execute
# all the benchmarks in there. This script is used for the L2 cache CI.

set +e

COMPILER=patmos-clang
COMPILER_OPTIONS="-Wall -Wno-unknown-pragmas -Werror -O2"
EXEC=patemu

PASS=0
FAIL_COMP=0
FAIL_EXEC=0

SKIP_DIRS=()

while [[ $# -gt 0 ]]; do
    case "$1" in
        --skip)
            if [[ -z "$2" ]]; then
                echo "Error: --skip requires an argument."
                exit 1
            fi
            SKIP_DIRS+=("$2")
            shift 2
            ;;
        *)
            echo "Unknown argument: $1"
            exit 1
            ;;
    esac
done

should_skip() {
    local dir="$1"
    for skip in "${SKIP_DIRS[@]}"; do
        [[ "$dir" == "$skip" ]] && return 0
    done
    return 1
}

for BENCH in */ ; do
    if should_skip "$BENCH"; then
        echo "Skipping $BENCH"
        continue
    fi

    printf "Checking %s\n" "$BENCH"
    cd "$BENCH" || continue

    rm -f a.out *.o

    $COMPILER $COMPILER_OPTIONS *.c
    COMPILE_STATUS=$?

    if [[ -f a.out ]]; then
        time $EXEC ./a.out
        RETURNVALUE=$?

        if (( RETURNVALUE == 0 )); then
            printf "passed.\n"
            ((PASS++))
        else
            printf "failed (wrong return value %d).\n" "$RETURNVALUE"
            ((FAIL_EXEC++))
        fi

    else
        printf "failed (compile error or warnings, status %d).\n" "$COMPILE_STATUS"
        ((FAIL_COMP++))
    fi

    cd ..
done

echo "PASS: $PASS, FAIL_COMP: $FAIL_COMP, FAIL_EXEC: $FAIL_EXEC"

if (( FAIL_EXEC > 0 )); then
    exit 1
fi