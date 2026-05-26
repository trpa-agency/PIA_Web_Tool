#!/usr/bin/env python3
"""
compare_pia.py — Run test cases through both pia_calculator.py and pia_reference.R,
compare outputs, and report any discrepancies.

Usage:
    python compare_pia.py [--rscript <path>] [--tol <float>] [--csv <output.csv>]

Options:
    --rscript   Path to Rscript executable (default: Rscript)
    --tol       Absolute tolerance for numeric comparison (default: 0.5)
    --csv       Write full results table to this CSV file

Test cases are defined in the CASES list below.  Each case may supply:
    zone_id        — resolves spatial context directly (no REST call needed)
    apn            — resolved to (lon, lat) then to zone_id via TRPA REST services
    in_town_center — bool; required when zone_id is given; inferred from APN parcel
                     fields if available (defaults to False if unknown)
    in_transit     — bool; must be set explicitly (arcpy not available here)
    proj_type      — land use type string exactly as shown in the tool
    quantity       — project size in native units (sq ft for KSF, units for res, etc.)
    analysis_type  — "TRPA" (default) or "SB-743"
    mitigations    — list of mitigation strategy names (default [])
    employee_vmt_pct, eligible_pct, custom_rate   — mitigation parameters
    redevelopment  — bool (default False)
    current_use, current_quantity, current_custom_rate — redevelopment credit params
"""

import argparse
import json
import os
import subprocess
import sys
import urllib.parse
import urllib.request
from pathlib import Path

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
TOOL_DIR = Path(__file__).parent
sys.path.insert(0, str(TOOL_DIR))

PARCELS_URL = "https://maps.trpa.org/server/rest/services/Parcels/FeatureServer/0"
ZONE_URL    = "https://maps.trpa.org/server/rest/services/Transportation_Planning/MapServer/9"

# ---------------------------------------------------------------------------
# Test cases — add / remove as needed
# ---------------------------------------------------------------------------
CASES = [
    # ---- Residential -------------------------------------------------------
    {"label": "Res Market-Rate 10du Zone12 TC+TR",
     "zone_id": "Zone 12", "in_town_center": True,  "in_transit": True,
     "proj_type": "Residential (Market-Rate)",    "quantity": 10},

    {"label": "Res Affordable 20du Zone12 TC+TR (transit screen)",
     "zone_id": "Zone 12", "in_town_center": True,  "in_transit": True,
     "proj_type": "Residential (Affordable)",         "quantity": 20},

    {"label": "Res Market-Rate 200du Zone5 no-TC no-TR (large)",
     "zone_id": "Zone 5",  "in_town_center": False, "in_transit": False,
     "proj_type": "Residential (Market-Rate)",    "quantity": 200},

    {"label": "Res Market-Rate 50du Zone5 TC no-TR",
     "zone_id": "Zone 5",  "in_town_center": True,  "in_transit": False,
     "proj_type": "Residential (Market-Rate)",    "quantity": 50},

    # ---- TAU ---------------------------------------------------------------
    {"label": "Hotel 30 TAU Zone12 TC",
     "zone_id": "Zone 12", "in_town_center": True,  "in_transit": True,
     "proj_type": "Hotel", "quantity": 30},

    {"label": "Hotel 120 TAU Zone5 no-TC (large)",
     "zone_id": "Zone 5",  "in_town_center": False, "in_transit": False,
     "proj_type": "Hotel", "quantity": 120},

    {"label": "Timeshare 50 TAU Zone12",
     "zone_id": "Zone 12", "in_town_center": True,  "in_transit": True,
     "proj_type": "Timeshare", "quantity": 50},

    # ---- Commercial KSF (no SOS) -------------------------------------------
    {"label": "General retail 1000 sqft Zone12",
     "zone_id": "Zone 12", "in_town_center": True,  "in_transit": True,
     "proj_type": "General retail", "quantity": 1_000},

    {"label": "General retail 20000 sqft Zone5 no-TC",
     "zone_id": "Zone 5",  "in_town_center": False, "in_transit": False,
     "proj_type": "General retail", "quantity": 20_000},

    {"label": "Fast Food Restaurant 3000 sqft Zone5",
     "zone_id": "Zone 5",  "in_town_center": False, "in_transit": False,
     "proj_type": "Fast Food Restaurant", "quantity": 3_000},

    {"label": "Supermarket 25000 sqft Zone5",
     "zone_id": "Zone 5",  "in_town_center": False, "in_transit": False,
     "proj_type": "Supermarket", "quantity": 25_000},

    {"label": "General Office 10000 sqft Zone12",
     "zone_id": "Zone 12", "in_town_center": True,  "in_transit": True,
     "proj_type": "General Office Building (GFA of more than 5,000 sf)", "quantity": 10_000},

    # ---- Public service KSF (has SOS) --------------------------------------
    {"label": "Library 2000 sqft Zone12",
     "zone_id": "Zone 12", "in_town_center": True,  "in_transit": True,
     "proj_type": "Library", "quantity": 2_000},

    {"label": "Church 5000 sqft Zone5",
     "zone_id": "Zone 5",  "in_town_center": False, "in_transit": False,
     "proj_type": "Church", "quantity": 5_000},

    {"label": "Hospital 50000 sqft Zone5",
     "zone_id": "Zone 5",  "in_town_center": False, "in_transit": False,
     "proj_type": "Hospital", "quantity": 50_000},

    # ---- Schools -----------------------------------------------------------
    {"label": "Elementary School 300 students Zone5",
     "zone_id": "Zone 5",  "in_town_center": False, "in_transit": False,
     "proj_type": "Elementary School", "quantity": 300},

    {"label": "High School 800 students Zone5",
     "zone_id": "Zone 5",  "in_town_center": False, "in_transit": False,
     "proj_type": "High School", "quantity": 800},

    # ---- Recreation / no-SOS uses ------------------------------------------
    {"label": "Golf Course 18 holes Zone5",
     "zone_id": "Zone 5",  "in_town_center": False, "in_transit": False,
     "proj_type": "Golf Course", "quantity": 18},

    {"label": "Marina 50 berths Zone12",
     "zone_id": "Zone 12", "in_town_center": True,  "in_transit": True,
     "proj_type": "Marina", "quantity": 50},

    {"label": "Bowling Alley 16 lanes Zone5",
     "zone_id": "Zone 5",  "in_town_center": False, "in_transit": False,
     "proj_type": "Bowling Alley", "quantity": 16},

    # ---- Unique Project Type -----------------------------------------------
    {"label": "Unique 100 units rate=5 Zone12",
     "zone_id": "Zone 12", "in_town_center": True,  "in_transit": True,
     "proj_type": "Unique Project Type", "quantity": 100, "custom_rate": 5},

    # ---- With mitigations --------------------------------------------------
    {"label": "Res MR 100du Traffic Calming Zone12",
     "zone_id": "Zone 12", "in_town_center": True,  "in_transit": True,
     "proj_type": "Residential (Market-Rate)", "quantity": 100,
     "mitigations": ["Traffic Calming"]},

    {"label": "Hotel 100 TAU Employee Shuttle 30% Zone5",
     "zone_id": "Zone 5",  "in_town_center": False, "in_transit": False,
     "proj_type": "Hotel", "quantity": 100,
     "mitigations": ["Employee Shuttle"], "employee_vmt_pct": 30},

    {"label": "Hotel 100 TAU Employee+Private Shuttle 50% Zone5",
     "zone_id": "Zone 5",  "in_town_center": False, "in_transit": False,
     "proj_type": "Hotel", "quantity": 100,
     "mitigations": ["Employee Shuttle", "Private Shuttle"], "employee_vmt_pct": 50},

    {"label": "Office 20ksf CTR-Required 60% emp 80% elig Zone5",
     "zone_id": "Zone 5",  "in_town_center": False, "in_transit": False,
     "proj_type": "General Office Building (GFA of more than 5,000 sf)",
     "quantity": 20_000,
     "mitigations": ["Employee Shuttle",
                     "Implement CTR Program -  Required Implementation/Monitoring"],
     "employee_vmt_pct": 60, "eligible_pct": 80},

    {"label": "Retail 10ksf Unbundle+TrafficCalm Zone12",
     "zone_id": "Zone 12", "in_town_center": True,  "in_transit": True,
     "proj_type": "General retail", "quantity": 10_000,
     "mitigations": ["Unbundle Parking Costs from Property Cost", "Traffic Calming"]},

    # ---- Redevelopment -----------------------------------------------------
    {"label": "Retail 10ksf replacing 5ksf Zone12",
     "zone_id": "Zone 12", "in_town_center": True,  "in_transit": True,
     "proj_type": "General retail", "quantity": 10_000,
     "redevelopment": True, "current_use": "General retail", "current_quantity": 5_000},

    {"label": "Res MR 20du replacing 10du Zone5",
     "zone_id": "Zone 5",  "in_town_center": False, "in_transit": False,
     "proj_type": "Residential (Market-Rate)", "quantity": 20,
     "redevelopment": True, "current_use": "Residential (Market-Rate)", "current_quantity": 10},

    # ---- APN-based cases (zone resolved at runtime) ------------------------
    {"label": "APN 026-102-024 Res MR 5du",
     "apn": "026-102-024",
     "in_town_center": True,  "in_transit": True,
     "proj_type": "Residential (Market-Rate)", "quantity": 5},

    {"label": "APN 026-102-024 Hotel 20 TAU",
     "apn": "026-102-024",
     "in_town_center": True,  "in_transit": True,
     "proj_type": "Hotel", "quantity": 20},
]

# ---------------------------------------------------------------------------
# REST helpers (same logic as PIA_Tool.pyt, no arcpy)
# ---------------------------------------------------------------------------

def _rest_get(url: str) -> dict:
    with urllib.request.urlopen(url, timeout=15) as r:
        return json.loads(r.read().decode())


def resolve_apn(apn: str) -> tuple[float, float] | tuple[None, None]:
    """Return (lon, lat) centroid for the given APN from the parcels service."""
    params = {
        "where": f"APN = '{apn.replace(chr(39), '')}'",
        "outFields": "APN",
        "returnGeometry": "true",
        "outSR": "4326",
        "f": "json",
    }
    url = f"{PARCELS_URL}/query?{urllib.parse.urlencode(params)}"
    data = _rest_get(url)
    features = data.get("features", [])
    if not features:
        return None, None
    rings = features[0].get("geometry", {}).get("rings", [])
    coords = [c for ring in rings for c in ring]
    if not coords:
        return None, None
    lon = sum(c[0] for c in coords) / len(coords)
    lat = sum(c[1] for c in coords) / len(coords)
    return lon, lat


def resolve_zone(lon: float, lat: float) -> str | None:
    """Return zone_id string for the given WGS84 point."""
    params = {
        "geometry": json.dumps({"x": lon, "y": lat, "spatialReference": {"wkid": 4326}}),
        "geometryType": "esriGeometryPoint",
        "spatialRel": "esriSpatialRelIntersects",
        "outFields": "zone_id",
        "returnGeometry": "false",
        "f": "json",
    }
    url = f"{ZONE_URL}/query?{urllib.parse.urlencode(params)}"
    data = _rest_get(url)
    features = data.get("features", [])
    return features[0]["attributes"].get("zone_id") if features else None


# ---------------------------------------------------------------------------
# Run pia_calculator.py (Python path)
# ---------------------------------------------------------------------------

def run_python(params: dict) -> dict:
    from pia_calculator import PIACalculator
    calc = PIACalculator(
        data_dir=str(TOOL_DIR),
        analysis_type=params.get("analysis_type", "TRPA"),
    )
    return calc.calculate(params)


# ---------------------------------------------------------------------------
# Run pia_reference.R (R path)
# ---------------------------------------------------------------------------

def run_r(params: dict, rscript: str) -> dict:
    json_str = json.dumps(params)
    r_script = str(TOOL_DIR / "pia_reference.R")
    env = os.environ.copy()
    env["PIA_DATA_DIR"] = str(TOOL_DIR)

    result = subprocess.run(
        [rscript, r_script, json_str],
        capture_output=True, text=True, env=env,
        cwd=str(TOOL_DIR),
    )
    if result.returncode != 0:
        raise RuntimeError(f"R error:\n{result.stderr.strip()}")

    stdout = result.stdout.strip()
    if not stdout:
        raise RuntimeError(f"R produced no output.\nSTDERR: {result.stderr.strip()}")

    return json.loads(stdout)


# ---------------------------------------------------------------------------
# Comparison
# ---------------------------------------------------------------------------

COMPARE_FIELDS = [
    ("proposed_vmt", "proposed_vmt"),
    ("redev_vmt",    "redev_vmt"),
    ("net_vmt",      "net_vmt"),
    ("sos",          "standard_of_significance"),  # R key, Python key
    ("screened",     "screened"),
    ("mob_fee",      "mobility_fee"),
    ("mit_factor",   "mitigation_factor_capped"),
]


def extract_py_values(py_result: dict) -> dict:
    """Flatten single-use Python result to the comparison field set."""
    lu = py_result.get("land_use_results", [{}])[0]
    return {
        "proposed_vmt": lu.get("proposed_vmt", 0),
        "redev_vmt":    lu.get("redev_vmt",    0),
        "net_vmt":      lu.get("net_vmt",       0),
        "standard_of_significance": lu.get("standard_of_significance", 0),
        "screened":     lu.get("screened",     "No"),
        "mobility_fee": lu.get("mobility_fee",  0),
        "mitigation_factor_capped": lu.get("mitigation_factor_capped", 1),
    }


def compare(py_vals: dict, r_vals: dict, tol: float) -> list[dict]:
    """Return list of discrepancy dicts for fields that differ."""
    diffs = []
    for r_key, py_key in COMPARE_FIELDS:
        r_val  = r_vals.get(r_key)
        py_val = py_vals.get(py_key)
        if r_val is None or py_val is None:
            continue
        if isinstance(r_val, str) or isinstance(py_val, str):
            match = str(r_val).strip() == str(py_val).strip()
        else:
            match = abs(float(r_val) - float(py_val)) <= tol
        if not match:
            diffs.append({
                "field": r_key,
                "r_value":  r_val,
                "py_value": py_val,
                "delta": None if isinstance(r_val, str) else round(float(py_val) - float(r_val), 4),
            })
    return diffs


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def build_params(case: dict, zone_id: str) -> dict:
    return {
        "proj_name":           case.get("label", ""),
        "proj_type":           case["proj_type"],
        "zone_id":             zone_id,
        "in_town_center":      bool(case.get("in_town_center", False)),
        "in_transit":          bool(case.get("in_transit",     False)),
        "analysis_type":       case.get("analysis_type", "TRPA"),
        "quantity":            float(case.get("quantity", 0)),
        "mitigations":         list(case.get("mitigations", [])),
        "employee_vmt_pct":    float(case.get("employee_vmt_pct", 0)),
        "eligible_pct":        float(case.get("eligible_pct",     0)),
        "custom_rate":         float(case.get("custom_rate",       0)),
        "redevelopment":       bool(case.get("redevelopment",     False)),
        "current_use":         case.get("current_use",         ""),
        "current_quantity":    float(case.get("current_quantity",  0)),
        "current_custom_rate": float(case.get("current_custom_rate", 0)),
    }


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--rscript", default="Rscript",
                        help="Path to Rscript executable")
    parser.add_argument("--tol", type=float, default=0.5,
                        help="Absolute tolerance for numeric comparison")
    parser.add_argument("--csv", default=None,
                        help="Write full results table to CSV file")
    args = parser.parse_args()

    rows = []
    pass_count = fail_count = skip_count = 0

    print(f"\n{'='*72}")
    print(f"  PIA Comparison: Python vs R  (tol={args.tol})")
    print(f"{'='*72}\n")

    for case in CASES:
        label = case.get("label", case.get("proj_type", "?"))
        print(f">> {label}")

        # Resolve zone_id (either direct or via APN lookup)
        zone_id = case.get("zone_id")
        if not zone_id:
            apn = case.get("apn")
            if not apn:
                print("  SKIP — no zone_id or apn\n")
                skip_count += 1
                continue
            try:
                lon, lat = resolve_apn(apn)
                if lon is None:
                    print(f"  SKIP — APN not found: {apn}\n")
                    skip_count += 1
                    continue
                zone_id = resolve_zone(lon, lat)
                if not zone_id:
                    print(f"  SKIP — zone lookup failed for APN {apn}\n")
                    skip_count += 1
                    continue
                print(f"  APN {apn} -> {zone_id}  ({lon:.5f}, {lat:.5f})")
            except Exception as exc:
                print(f"  SKIP — REST error: {exc}\n")
                skip_count += 1
                continue

        params = build_params(case, zone_id)

        # Python
        try:
            py_result = run_python(params)
            py_vals   = extract_py_values(py_result)
        except Exception as exc:
            print(f"  PYTHON ERROR: {exc}\n")
            skip_count += 1
            continue

        # R
        try:
            r_vals = run_r(params, args.rscript)
            if "error" in r_vals:
                print(f"  R ERROR: {r_vals['error']}\n")
                skip_count += 1
                continue
        except Exception as exc:
            print(f"  R ERROR: {exc}\n")
            skip_count += 1
            continue

        # Compare
        diffs = compare(py_vals, r_vals, args.tol)
        status = "PASS" if not diffs else "FAIL"

        print(f"  zone={zone_id}  "
              f"proposed_vmt(py={py_vals['proposed_vmt']:.1f} r={r_vals.get('proposed_vmt', '?')})  "
              f"net_vmt(py={py_vals['net_vmt']:.1f} r={r_vals.get('net_vmt', '?')})  "
              f"screened(py={py_vals['screened']} r={r_vals.get('screened', '?')})  "
              f"fee(py={py_vals['mobility_fee']:.0f} r={r_vals.get('mob_fee', '?'):.0f})  "
              f"[{status}]")

        if diffs:
            fail_count += 1
            for d in diffs:
                delta_str = f"  delta={d['delta']}" if d['delta'] is not None else ""
                print(f"    DIFF {d['field']:28s}  R={d['r_value']}  PY={d['py_value']}{delta_str}")
        else:
            pass_count += 1
        print()

        # Accumulate rows for CSV
        row = {
            "label":        label,
            "zone_id":      zone_id,
            "proj_type":    params["proj_type"],
            "quantity":     params["quantity"],
            "in_tc":        params["in_town_center"],
            "in_tr":        params["in_transit"],
            "mitigations":  ";".join(params["mitigations"]),
            "status":       status,
        }
        for r_key, py_key in COMPARE_FIELDS:
            row[f"py_{r_key}"] = py_vals.get(py_key, "")
            row[f"r_{r_key}"]  = r_vals.get(r_key, "")
        rows.append(row)

    print(f"{'='*72}")
    print(f"  Results: {pass_count} PASS  {fail_count} FAIL  {skip_count} SKIP  "
          f"({len(CASES)} cases total)")
    print(f"{'='*72}\n")

    if args.csv and rows:
        import csv
        with open(args.csv, "w", newline="") as f:
            writer = csv.DictWriter(f, fieldnames=rows[0].keys())
            writer.writeheader()
            writer.writerows(rows)
        print(f"Results written to {args.csv}\n")


if __name__ == "__main__":
    main()
