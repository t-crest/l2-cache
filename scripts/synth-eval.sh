#!/usr/bin/env bash
# Synthesizes the L2 cache for each replacement policy with the Quartus project of the synthesis top level
# (quartus/SharedPipelinedCacheSynthTop) and extracts the resource usage and maximum frequencies used in the paper.
#
# The reports of each policy are kept in <out>/<policy>/, the extracted numbers are written to <out>/synth-results.csv
# and as LaTeX macros to <out>/synth-results.tex (see scripts/synth_report.py). Requires sbt and the Quartus command
# line tools (quartus_map, quartus_fit, quartus_asm, quartus_sta) in the PATH, and python3 for the extraction.

set -euo pipefail

POLICIES="plru,cont,lc,part,lcq,partq"
CORES=4
OUT_DIR="synth-results"
SYNTHESIZE=true

function usage {
	echo "Usage: $0 [--policies plru,cont,lc,part,lcq,partq] [--cores 4] [--out synth-results] [--parse-only]"
	echo "  --policies    Comma separated L2 replacement policies, as in the Patmos configuration: plru, cont, time,"
	echo "                lc, lcr, part, lcq or partq (default: $POLICIES)."
	echo "  --cores       Number of cores sharing the cache (default: $CORES, as in the cont-test evaluation)."
	echo "  --out         Directory for the reports and the results (default: $OUT_DIR)."
	echo "  --parse-only  Do not synthesize, only extract the results from the reports in the output directory."
}

while [[ "$#" -gt 0 ]]; do
	case $1 in
		--policies) POLICIES="$2"; shift ;;
		--cores) CORES="$2"; shift ;;
		--out) OUT_DIR="$2"; shift ;;
		--parse-only) SYNTHESIZE=false ;;
		-h|--help) usage; exit 0 ;;
		*) echo "Unknown option: $1"; usage; exit 1 ;;
	esac
	shift
done

# Run from the repository root, where the Makefile is
cd "$(dirname "$0")/.."

PROJECT_NAME="SharedPipelinedCacheSynthTop"
PROJECT_DIR="quartus/$PROJECT_NAME"
VERILOG_FILE="generated/$PROJECT_NAME.v"

# Replacement policy module that the generated hardware must contain
function policy_module {
	case $1 in
		plru) echo BitPlruReplacementPolicy ;;
		cont) echo ContentionReplacementPolicy ;;
		time) echo TimeoutReplacementPolicy ;;
		lc|lcr|lcq) echo LeastCriticalReplacementPolicy ;;
		part|partq) echo CriticalWayPartitionReplacementPolicy ;;
		*) echo "Unknown L2 replacement policy: $1" >&2; exit 1 ;;
	esac
}

IFS=',' read -r -a POLICY_LIST <<< "$POLICIES"

if [ "$SYNTHESIZE" = true ]; then
	for policy in "${POLICY_LIST[@]}"; do
		module=$(policy_module "$policy")
		echo "=== Synthesizing the L2 cache with the $policy replacement policy and $CORES cores ==="

		# Remove the Verilog of the previous policy, so a failed generation cannot be synthesized instead
		rm -f "$VERILOG_FILE"
		make clean-quartus copy-to-quartus SYNTH_ARGS="$policy $CORES"
		if ! grep -q "^module $module(" "$PROJECT_DIR/$PROJECT_NAME.v"; then
			echo "The hardware generated for the $policy replacement policy does not contain $module"
			exit 1
		fi

		make synth

		policy_dir="$OUT_DIR/$policy"
		rm -rf "$policy_dir"
		mkdir -p "$policy_dir"
		cp "$PROJECT_DIR"/output_files/*.rpt "$PROJECT_DIR"/output_files/*.summary "$policy_dir"/
		cp "$PROJECT_DIR/$PROJECT_NAME.v" "$policy_dir"/
	done
fi

python3 scripts/synth_report.py "$OUT_DIR" "${POLICY_LIST[@]}"
