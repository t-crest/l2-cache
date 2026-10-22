#!/usr/bin/env python3
"""
Extracts the synthesis results of the L2 cache from the Quartus reports collected by synth-eval.sh.

For each replacement policy, the reports are expected in <result dir>/<policy>/. The resource usage of the whole L2
cache (the OcpCacheWrapper instance, without the shift registers of the synthesis top level) and of the replacement
policy module is taken from the "Fitter Resource Utilization by Entity" table of the fitter report. The maximum
frequencies are taken from the slow 1200mV 85C and 0C model Fmax summaries of the timing analyzer report.

Writes <result dir>/synth-results.csv and <result dir>/synth-results.tex. The LaTeX file defines the macros used in
the synthesis results table of the paper, e.g. \\contentionCacheAllLuts and \\contentionFreqLow.

Usage: synth_report.py <result dir> [policy ...]
"""

import csv
import math
import os
import re
import sys

PROJECT_NAME = "SharedPipelinedCacheSynthTop"

# Policies in table order and the prefix of their macros in the paper
POLICY_PREFIXES = {
    "plru": "plru",
    "cont": "contention",
    "time": "timeout",
    "lc": "lc",
    "lcr": "lcr",
    "part": "part",
    "lcq": "lcq",
    "partq": "partq",
}

CACHE_INSTANCE = "l2Cache"  # OcpCacheWrapper instance in the synthesis top level
POLICY_INSTANCE = "repPol"  # Replacement policy instance in SharedPipelinedCache

# LUTs are the logic cells that use their look-up table, i.e. all logic cells except the register-only ones
LUT_COLUMNS = ["LUT-Only LCs", "LUT/Register LCs"]


def read_report(path):
    if not os.path.isfile(path):
        sys.exit(f"Missing Quartus report: {path}")
    with open(path, encoding="utf-8", errors="replace") as f:
        return f.read().splitlines()


def find_table(lines, title):
    """Returns the rows of a Quartus report table as dictionaries from column header to cell text."""
    title_re = re.compile(r"^;\s*" + re.escape(title) + r"\s*;")
    start = next((i for i, line in enumerate(lines) if title_re.match(line)), None)
    if start is None:
        return None

    header = None
    rows = []
    for line in lines[start + 1:]:
        if line.startswith("+"):
            continue
        if not line.startswith(";"):
            break
        cells = [cell.strip() for cell in line.strip().strip(";").split(";")]
        if header is None:
            header = cells
        else:
            rows.append(dict(zip(header, cells)))
    return rows


def first_number(cell):
    """Parses '1234 (56)' or '1,234' as 1234, the total of an entity including its children."""
    match = re.match(r"^\s*([\d,.]+)", cell)
    if match is None:
        raise ValueError(f"Not a number: '{cell}'")
    return float(match.group(1).replace(",", ""))


def entity_usage(rows, instance, path):
    matches = [row for row in rows if re.search(r":" + re.escape(instance) + r"$", row["Full Hierarchy Name"])]
    if len(matches) != 1:
        sys.exit(f"Expected one entity with the instance name {instance} in {path}, found {len(matches)}")
    row = matches[0]
    return {
        "entity": row["Entity Name"],
        "logic_cells": int(first_number(row["Logic Cells"])),
        "luts": int(sum(first_number(row[column]) for column in LUT_COLUMNS)),
        "regs": int(first_number(row["Dedicated Logic Registers"])),
        "mem_bits": int(first_number(row["Memory Bits"])),
        "m9ks": int(first_number(row["M9Ks"])),
    }


def fmax(lines, model, path):
    rows = find_table(lines, f"{model} Model Fmax Summary")
    if not rows:
        sys.exit(f"No Fmax summary for the {model} model in {path}")
    # The synthesis top level has a single clock
    return min(first_number(row["Fmax"]) for row in rows)


def quartus_version(lines):
    for line in lines:
        match = re.search(r"Quartus Prime Version\s*;\s*(.*?)\s*;", line)
        if match:
            return match.group(1)
    return "unknown"


def policy_results(result_dir, policy):
    reports = os.path.join(result_dir, policy)
    fit_path = os.path.join(reports, f"{PROJECT_NAME}.fit.rpt")
    sta_path = os.path.join(reports, f"{PROJECT_NAME}.sta.rpt")
    fit = read_report(fit_path)
    sta = read_report(sta_path)

    rows = find_table(fit, "Fitter Resource Utilization by Entity")
    if not rows:
        sys.exit(f"No resource utilization by entity table in {fit_path}")

    cache = entity_usage(rows, CACHE_INSTANCE, fit_path)
    pol = entity_usage(rows, POLICY_INSTANCE, fit_path)
    return {
        "policy": policy,
        "policy_entity": pol["entity"],
        "cache_logic_cells": cache["logic_cells"],
        "cache_luts": cache["luts"],
        "cache_regs": cache["regs"],
        "cache_mem_bits": cache["mem_bits"],
        "cache_m9ks": cache["m9ks"],
        "policy_logic_cells": pol["logic_cells"],
        "policy_luts": pol["luts"],
        "policy_regs": pol["regs"],
        "policy_mem_bits": pol["mem_bits"],
        "fmax_85c_mhz": fmax(sta, "Slow 1200mV 85C", sta_path),
        "fmax_0c_mhz": fmax(sta, "Slow 1200mV 0C", sta_path),
        "quartus_version": quartus_version(fit),
    }


def write_csv(path, results):
    with open(path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=list(results[0].keys()))
        writer.writeheader()
        writer.writerows(results)


def write_latex(path, results):
    lines = [
        "% Generated by l2-cache/scripts/synth_report.py from the Quartus reports of synth-eval.sh",
        f"% Quartus version: {', '.join(sorted({r['quartus_version'] for r in results}))}",
        "% Cache: OcpCacheWrapper instance, policy: replacement policy instance, LUTs: logic cells without the",
        "% register-only ones, FreqLow/FreqHigh: Fmax of the slow 1200mV 85C/0C models, rounded down",
    ]
    for r in results:
        prefix = POLICY_PREFIXES[r["policy"]]
        lines += [
            "",
            f"% {r['policy']} ({r['policy_entity']})",
            f"\\newcommand{{\\{prefix}CacheAllLuts}}{{{r['cache_luts']}}}",
            f"\\newcommand{{\\{prefix}CacheAllRegs}}{{{r['cache_regs']}}}",
            f"\\newcommand{{\\{prefix}CacheAllMemBits}}{{{r['cache_mem_bits']}}}",
            f"\\newcommand{{\\{prefix}PolicyLuts}}{{{r['policy_luts']}}}",
            f"\\newcommand{{\\{prefix}PolicyRegs}}{{{r['policy_regs']}}}",
            f"\\newcommand{{\\{prefix}PolicyMemBits}}{{{r['policy_mem_bits']}}}",
            f"\\newcommand{{\\{prefix}FreqLow}}{{{math.floor(r['fmax_85c_mhz'])}}}",
            f"\\newcommand{{\\{prefix}FreqHigh}}{{{math.floor(r['fmax_0c_mhz'])}}}",
        ]
    with open(path, "w") as f:
        f.write("\n".join(lines) + "\n")


def main():
    if len(sys.argv) < 2:
        sys.exit(__doc__)

    result_dir = sys.argv[1]
    policies = sys.argv[2:] or [p for p in POLICY_PREFIXES if os.path.isdir(os.path.join(result_dir, p))]
    unknown = [p for p in policies if p not in POLICY_PREFIXES]
    if unknown:
        sys.exit(f"Unknown policies: {', '.join(unknown)}")
    if not policies:
        sys.exit(f"No synthesis results in {result_dir}")

    results = [policy_results(result_dir, policy) for policy in policies]
    write_csv(os.path.join(result_dir, "synth-results.csv"), results)
    write_latex(os.path.join(result_dir, "synth-results.tex"), results)

    print(f"{'policy':<6} {'LUTs':>7} {'regs':>7} {'mem bits':>9} {'pol LUTs':>9} {'pol regs':>9} {'Fmax 85C':>9} {'Fmax 0C':>8}")
    for r in results:
        print(f"{r['policy']:<6} {r['cache_luts']:>7} {r['cache_regs']:>7} {r['cache_mem_bits']:>9} "
              f"{r['policy_luts']:>9} {r['policy_regs']:>9} {r['fmax_85c_mhz']:>9.2f} {r['fmax_0c_mhz']:>8.2f}")
    print(f"Wrote {os.path.join(result_dir, 'synth-results.csv')} and {os.path.join(result_dir, 'synth-results.tex')}")


if __name__ == "__main__":
    main()
