"""
pia_client.py — Submit a PIA analysis to the TRPA GP service and print results.

Requires: pip install requests
"""

import time
import json
import requests

SERVICE_URL = "https://maps.trpa.org/server/rest/services/Project_Impact_Analysis/GPServer"
TOOL_NAME   = "Project%20Impact%20Analysis"
TOKEN       = ""   # paste your token here if the service requires authentication

# ---------------------------------------------------------------------------
# Input parameters
# ---------------------------------------------------------------------------
inputs = {
    "proj_name":      "Lakeside Hotel",
    "analysis_type":  "TRPA",
    "apn":            "026-102-024",     # Assessor Parcel Number
    "proj_type":      "Hotel",
    "quantity":       "50",              # TAU units
    "mitigations":    "",                # semicolon-separated if multiple
    "employee_vmt_pct": "0",
    "eligible_pct":   "0",
    "redevelopment":  "false",
    "f":              "json",
    "token":          TOKEN,
}

# ---------------------------------------------------------------------------
# 1. Submit the job
# ---------------------------------------------------------------------------
submit_url = f"{SERVICE_URL}/{TOOL_NAME}/submitJob"
resp = requests.post(submit_url, data=inputs)
resp.raise_for_status()

job = resp.json()
if "error" in job:
    raise RuntimeError(f"Submit failed: {job['error']}")

job_id = job["jobId"]
print(f"Job submitted — ID: {job_id}")

# ---------------------------------------------------------------------------
# 2. Poll until complete
# ---------------------------------------------------------------------------
status_url = f"{SERVICE_URL}/{TOOL_NAME}/jobs/{job_id}"
TERMINAL = {"esriJobSucceeded", "esriJobFailed", "esriJobCancelled", "esriJobTimedOut"}

while True:
    status = requests.get(status_url, params={"f": "json", "token": TOKEN}).json()
    job_status = status.get("jobStatus", "")
    print(f"  {job_status}")
    if job_status in TERMINAL:
        break
    time.sleep(3)

if job_status != "esriJobSucceeded":
    raise RuntimeError(f"Job did not succeed: {status}")

# ---------------------------------------------------------------------------
# 3. Fetch results
# ---------------------------------------------------------------------------
def get_result(param_name):
    url = f"{SERVICE_URL}/{TOOL_NAME}/jobs/{job_id}/results/{param_name}"
    r = requests.get(url, params={"f": "json", "token": TOKEN}).json()
    return r.get("value")

zone_id    = get_result("out_zone_id")
screened   = get_result("out_screened")
total_vmt  = get_result("out_total_vmt")
sos_vmt    = get_result("out_standard_of_significance")
mob_fee    = get_result("out_mobility_fee")
full_json  = get_result("out_results_json")

# ---------------------------------------------------------------------------
# 4. Print summary
# ---------------------------------------------------------------------------
print(f"\nZone ID:               {zone_id}")
print(f"Screened:              {screened}")
print(f"Total Net VMT:         {total_vmt:,.1f}")
print(f"Standard of Sig. VMT:  {sos_vmt:,.1f}")
print(f"Mobility Fee:          ${mob_fee:,.2f}")

if full_json:
    print("\nFull results:")
    print(json.dumps(full_json if isinstance(full_json, dict) else json.loads(full_json), indent=2))