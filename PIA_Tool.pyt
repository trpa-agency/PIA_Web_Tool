# -*- coding: utf-8 -*-
"""
PIA_Tool.pyt  –  TRPA Project Impact Analysis Geoprocessing Toolbox
Wraps pia_calculator.py in an ArcGIS Pro Python Toolbox that can be
published as a Geoprocessing Service on an ArcGIS Enterprise portal.

Deployment checklist
---------------------
1. Copy this file plus pia_calculator.py and all CSVs/shapefiles from the
   PIA_Web_Tool directory to the same folder on the GP server.
2. Required data files (relative to this .pyt):
       data/trip_rates.csv
       trip_length_table.csv
       residential_data.csv
       trip_length_data_full_length.csv   (SB-743 mode)
       residential_data_full_length.csv   (SB-743 mode)
       Town center and transit buffer checks use Planning_Buffers MapServer
       (layers 7 and 5) — no local shapefiles required.
3. The tool accepts an APN string, queries the TRPA parcels service to
   resolve the parcel centroid, then queries the zone service for zone_id.
   The server must be able to reach maps.trpa.org.
"""

import arcpy
import json
import os
import sys
import urllib.parse
import urllib.request

# ---------------------------------------------------------------------------
# Path setup: pia_calculator.py lives next to this .pyt
# ---------------------------------------------------------------------------
_TOOL_DIR = os.path.dirname(os.path.abspath(__file__))
if _TOOL_DIR not in sys.path:
    sys.path.insert(0, _TOOL_DIR)

# ---------------------------------------------------------------------------
# Constants
# URL strings are encoded to prevent the ArcGIS Service Analyzer from
# treating them as data sources during GP service publishing.
# '??' decodes to '://' and '?' decodes to '/'.
# ---------------------------------------------------------------------------
def _url(s: str) -> str:
    return s.replace("??", "://").replace("?", "/")

_ZONE_SERVICE_URL     = _url("https??maps.trpa.org?server?rest?services?Transportation_Planning?MapServer?9")
_PARCELS_SERVICE_URL  = _url("https??maps.trpa.org?server?rest?services?Parcels?FeatureServer?0")
_PLANNING_BUFFERS_URL = _url("https??maps.trpa.org?server?rest?services?Planning_Buffers?MapServer")
_TOWN_CENTER_LAYER    = 7   # "Town Center with Buffer"
_TRANSIT_LAYER        = 5   # "Within a Half Mile Walking Distance of a Transit Stop"

_ALL_PROJ_TYPES = [
    "Auto Parts and Service Center",
    "Automobile Sales",
    "Bowling Alley",
    "Building Materials/Lumber",
    "Church",
    "Daycare Center",
    "Developed Campground/RV Park",
    "Drinking Place",
    "Drive-In Bank",
    "Elementary School",
    "Fast Food Restaurant",
    "Free-Standing Discount Store",
    "Furniture Store",
    "General Office Building (GFA of more than 5,000 sf)",
    "General retail",
    "Golf Course",
    "Health and Fitness Club",
    "High School",
    "High Turnover Sit-Down Restaurant (<1 hr. turnover)",
    "Hospital",
    "Hotel",
    "Library",
    "Light industrial",
    "Marina",
    "Medical –Dental Office Building",
    "Middle School/Junior High School",
    "Mixed-Use",
    "Motel",
    "Movie Theater (traditional)",
    "Pharmacy/Drugstore",
    "Private School (K-12)",
    "Public Park",
    "Quality Restaurant (>1 hr. turnover)",
    "Recreational Community Center",
    "Residential (Affordable)",
    "Residential (Market-Rate)",
    "Supermarket",
    "Timeshare",
    "Unique Project Type",
    "University/College",
    "Warehouse",
]

_ALL_MITIGATIONS = [
    "End of Trip Facilities",
    "Employee Shuttle",
    "Implement CTR Program -  Required Implementation/Monitoring",
    "Implement CTR Program - Voluntary",
    "Private Shuttle",
    "Traffic Calming",
    "Unbundle Parking Costs from Property Cost",
]

# ---------------------------------------------------------------------------
# Toolbox
# ---------------------------------------------------------------------------

class Toolbox:
    def __init__(self):
        self.label = "TRPA Project Impact Analysis"
        self.alias = "PIATool"
        self.tools = [PIAAnalysis]


# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------

def _parse_multivalue(raw: str) -> list:
    """Convert arcpy multivalue string (semicolon-delimited) to a list."""
    if not raw:
        return []
    return [v.strip().strip("'\"") for v in raw.split(";") if v.strip()]


def _poly_centroid(rings: list) -> tuple:
    """Return (lon, lat) centroid from an esriGeometryPolygon rings list."""
    all_x, all_y = [], []
    for ring in rings:
        for coord in ring:
            all_x.append(coord[0])
            all_y.append(coord[1])
    if not all_x:
        return None, None
    return sum(all_x) / len(all_x), sum(all_y) / len(all_y)


def _lookup_parcel_centroid(apn: str, messages) -> tuple:
    """
    Query the TRPA parcels feature service for the given APN and return
    the (lon, lat) centroid of the parcel polygon (WGS84). Returns (None, None)
    on failure.
    """
    safe_apn = apn.replace("'", "")  # prevent SQL injection
    params = {
        "where": f"APN = '{safe_apn}'",
        "outFields": "APN",
        "returnGeometry": "true",
        "outSR": "4326",
        "f": "json",
    }
    url = f"{_PARCELS_SERVICE_URL}/query?{urllib.parse.urlencode(params)}"
    try:
        with urllib.request.urlopen(url, timeout=15) as resp:
            data = json.loads(resp.read().decode())
        features = data.get("features", [])
        if not features:
            messages.addWarningMessage(f"No parcel found for APN: {apn}")
            return None, None
        rings = features[0].get("geometry", {}).get("rings", [])
        lon, lat = _poly_centroid(rings)
        if lon is None:
            messages.addWarningMessage(f"Parcel geometry is empty for APN: {apn}")
        return lon, lat
    except Exception as exc:
        messages.addWarningMessage(f"Parcel REST query failed: {exc}")
        return None, None


def _lookup_zone_rest(lon: float, lat: float, messages) -> str | None:
    """
    Query the TRPA zone feature service via REST to find which zone
    contains the given WGS84 point. Returns zone_id string or None.
    """
    params = {
        "geometry": json.dumps({
            "x": lon,
            "y": lat,
            "spatialReference": {"wkid": 4326},
        }),
        "geometryType": "esriGeometryPoint",
        "spatialRel": "esriSpatialRelIntersects",
        "outFields": "zone_id",
        "returnGeometry": "false",
        "f": "json",
    }
    url = f"{_ZONE_SERVICE_URL}/query?{urllib.parse.urlencode(params)}"
    try:
        with urllib.request.urlopen(url, timeout=15) as resp:
            data = json.loads(resp.read().decode())
        features = data.get("features", [])
        if features:
            return features[0]["attributes"].get("zone_id")
    except Exception as exc:
        messages.addWarningMessage(f"Zone REST query failed: {exc}")
    return None


def _point_in_buffer_service(lon: float, lat: float, layer_id: int, messages) -> bool:
    """
    Return True if the WGS84 point (lon, lat) intersects any feature in
    the given Planning_Buffers MapServer layer (queried via REST).
    """
    params = {
        "geometry": json.dumps({"x": lon, "y": lat, "spatialReference": {"wkid": 4326}}),
        "geometryType": "esriGeometryPoint",
        "spatialRel": "esriSpatialRelIntersects",
        "returnCountOnly": "true",
        "f": "json",
    }
    url = f"{_PLANNING_BUFFERS_URL}/{layer_id}/query?{urllib.parse.urlencode(params)}"
    try:
        with urllib.request.urlopen(url, timeout=15) as resp:
            data = json.loads(resp.read().decode())
        return data.get("count", 0) > 0
    except Exception as exc:
        messages.addWarningMessage(f"Buffer REST query failed (layer {layer_id}): {exc}")
        return False


# ---------------------------------------------------------------------------
# Tool
# ---------------------------------------------------------------------------

class PIAAnalysis:
    """
    Single ArcGIS Pro geoprocessing tool that covers both single-use and
    mixed-use (up to 3 land-use components) PIA calculations.
    """

    def __init__(self):
        self.label = "Project Impact Analysis"
        self.description = (
            "Calculates VMT, standard of significance, screening status, "
            "and mobility fees under TRPA's Project Impact Analysis framework. "
            "Accepts an Assessor Parcel Number (APN), resolves the parcel centroid "
            "via the TRPA parcels service, then determines the PIA zone and buffer "
            "context automatically before returning results as individual outputs and JSON."
        )
        self.canRunInBackground = True

    # ------------------------------------------------------------------
    # Parameters
    # ------------------------------------------------------------------

    def getParameterInfo(self):

        def _str_param(display, name, required=False, choices=None, multi=False, default=None):
            p = arcpy.Parameter(
                displayName=display, name=name, datatype="GPString",
                parameterType="Required" if required else "Optional",
                direction="Input", multiValue=multi,
            )
            if choices is not None:
                p.filter.type = "ValueList"
                p.filter.list = choices
            if default is not None:
                p.value = default
            return p

        def _num_param(display, name, datatype="GPDouble", default=0):
            p = arcpy.Parameter(
                displayName=display, name=name, datatype=datatype,
                parameterType="Optional", direction="Input",
            )
            p.value = default
            return p

        def _out_param(display, name, datatype="GPString"):
            return arcpy.Parameter(
                displayName=display, name=name, datatype=datatype,
                parameterType="Derived", direction="Output",
            )

        # ---- [0-1] Project info ------------------------------------------
        p_name = _str_param("Project Name", "proj_name")
        p_at = _str_param(
            "Analysis Type", "analysis_type",
            choices=["TRPA", "SB-743"], default="TRPA",
        )

        # ---- [2] Location ------------------------------------------------
        p_loc = arcpy.Parameter(
            displayName="Assessor Parcel Number (APN)",
            name="apn",
            datatype="GPString",
            parameterType="Required",
            direction="Input",
        )

        # ---- [3] Top-level project type ----------------------------------
        p_ptype = _str_param(
            "Project Type", "proj_type",
            required=True, choices=_ALL_PROJ_TYPES,
        )

        # ---- [4-8] Single-use land-use params ----------------------------
        p_qty = _num_param(
            "Quantity  (sq ft for KSF uses · units for residential / TAU / other)",
            "quantity", default=0,
        )
        p_mit = _str_param(
            "Mitigations", "mitigations",
            choices=_ALL_MITIGATIONS, multi=True,
        )
        p_emp = _num_param("Employee % of Total VMT (0–100)", "employee_vmt_pct", "GPLong")
        p_elig = _num_param("% of Employees Eligible for CTR/Shuttle (0–100)", "eligible_pct", "GPLong")
        p_rate = _num_param("Custom Trip Rate (Unique Project Type only)", "custom_rate")

        # ---- [9-14] Mixed-Use land use 1 ---------------------------------
        p_lu1 = _str_param("Land Use 1 (Mixed-Use)", "land_use_1", choices=_ALL_PROJ_TYPES)
        p_q1 = _num_param("Quantity 1", "quantity_1")
        p_m1 = _str_param("Mitigations 1", "mitigations_1", choices=_ALL_MITIGATIONS, multi=True)
        p_e1 = _num_param("Employee % VMT 1", "employee_vmt_pct_1", "GPLong")
        p_el1 = _num_param("% Eligible 1", "eligible_pct_1", "GPLong")
        p_r1 = _num_param("Custom Rate 1", "custom_rate_1")

        # ---- [15-20] Mixed-Use land use 2 --------------------------------
        p_lu2 = _str_param("Land Use 2 (Mixed-Use)", "land_use_2", choices=_ALL_PROJ_TYPES)
        p_q2 = _num_param("Quantity 2", "quantity_2")
        p_m2 = _str_param("Mitigations 2", "mitigations_2", choices=_ALL_MITIGATIONS, multi=True)
        p_e2 = _num_param("Employee % VMT 2", "employee_vmt_pct_2", "GPLong")
        p_el2 = _num_param("% Eligible 2", "eligible_pct_2", "GPLong")
        p_r2 = _num_param("Custom Rate 2", "custom_rate_2")

        # ---- [21-26] Mixed-Use land use 3 --------------------------------
        p_lu3 = _str_param("Land Use 3 (Mixed-Use)", "land_use_3", choices=_ALL_PROJ_TYPES)
        p_q3 = _num_param("Quantity 3", "quantity_3")
        p_m3 = _str_param("Mitigations 3", "mitigations_3", choices=_ALL_MITIGATIONS, multi=True)
        p_e3 = _num_param("Employee % VMT 3", "employee_vmt_pct_3", "GPLong")
        p_el3 = _num_param("% Eligible 3", "eligible_pct_3", "GPLong")
        p_r3 = _num_param("Custom Rate 3", "custom_rate_3")

        # ---- [27-30] Redevelopment ----------------------------------------
        p_redev = arcpy.Parameter(
            displayName="Redevelopment / Change in Operation?",
            name="redevelopment", datatype="GPBoolean",
            parameterType="Optional", direction="Input",
        )
        p_redev.value = False
        p_cuse = _str_param("Existing Land Use (Redevelopment)", "current_use", choices=_ALL_PROJ_TYPES)
        p_cqty = _num_param("Existing Quantity (Redevelopment)", "current_quantity")
        p_crate = _num_param("Existing Custom Rate (Redevelopment)", "current_custom_rate")

        # ---- [31-39] Outputs ---------------------------------------------
        p_ozone = _out_param("Zone ID", "out_zone_id")
        p_otc = _out_param("In Town/Regional Center Buffer", "out_in_town_center", "GPBoolean")
        p_otr = _out_param("In Transit Buffer", "out_in_transit", "GPBoolean")
        p_osc = _out_param("Screened", "out_screened")
        p_ovmt = _out_param("Total Net VMT", "out_total_vmt", "GPDouble")
        p_osos = _out_param("Standard of Significance VMT", "out_standard_of_significance", "GPDouble")
        p_omit = _out_param("VMT Mitigation Needed", "out_mitigation_needed", "GPDouble")
        p_ofee = _out_param("Mobility Fee ($)", "out_mobility_fee", "GPDouble")
        p_ojsn = _out_param("Full Results (JSON)", "out_results_json")

        return [
            p_name, p_at,                           # 0, 1
            p_loc,                                  # 2
            p_ptype,                                # 3
            p_qty, p_mit, p_emp, p_elig, p_rate,    # 4-8   single-use
            p_lu1, p_q1, p_m1, p_e1, p_el1, p_r1,  # 9-14  mixed land use 1
            p_lu2, p_q2, p_m2, p_e2, p_el2, p_r2,  # 15-20 mixed land use 2
            p_lu3, p_q3, p_m3, p_e3, p_el3, p_r3,  # 21-26 mixed land use 3
            p_redev, p_cuse, p_cqty, p_crate,       # 27-30 redevelopment
            p_ozone, p_otc, p_otr,                  # 31-33 spatial outputs
            p_osc, p_ovmt, p_osos, p_omit, p_ofee,  # 34-38 VMT outputs
            p_ojsn,                                 # 39    JSON
        ]

    # ------------------------------------------------------------------
    # Dynamic parameter visibility
    # ------------------------------------------------------------------

    def updateParameters(self, parameters):
        proj_type = parameters[3].valueAsText or ""
        is_mixed = proj_type == "Mixed-Use"
        is_redev = bool(parameters[27].value)

        # Mixed-use land-use groups: show only when Mixed-Use selected
        for idx in range(9, 27):
            parameters[idx].enabled = is_mixed

        # Single-use params: show only when NOT Mixed-Use
        for idx in [4, 5, 6, 7, 8]:
            parameters[idx].enabled = not is_mixed

        # Redevelopment fields: show only when redevelopment is checked
        # (not applicable for Mixed-Use)
        for idx in [28, 29, 30]:
            parameters[idx].enabled = is_redev and not is_mixed

        return

    def updateMessages(self, parameters):
        for idx in [6, 7, 18, 19, 24, 25]:
            p = parameters[idx]
            if p.value is not None:
                try:
                    v = int(p.value)
                    if not (0 <= v <= 100):
                        p.setErrorMessage("Value must be between 0 and 100.")
                except (TypeError, ValueError):
                    p.setErrorMessage("Must be a whole number 0–100.")
        return

    # ------------------------------------------------------------------
    # Execute
    # ------------------------------------------------------------------

    def execute(self, parameters, messages):
        arcpy.env.overwriteOutput = True

        # ---- Unpack all input parameters --------------------------------
        proj_name = parameters[0].valueAsText or ""
        analysis_type = parameters[1].valueAsText or "TRPA"
        proj_type = parameters[3].valueAsText

        # Single-use params
        quantity = float(parameters[4].value or 0)
        mitigations = _parse_multivalue(parameters[5].valueAsText)
        employee_vmt_pct = float(parameters[6].value or 0)
        eligible_pct = float(parameters[7].value or 0)
        custom_rate = float(parameters[8].value or 0)

        # Mixed-use land use 1
        land_use_1 = parameters[9].valueAsText or ""
        quantity_1 = float(parameters[10].value or 0)
        mitigations_1 = _parse_multivalue(parameters[11].valueAsText)
        emp_1 = float(parameters[12].value or 0)
        elig_1 = float(parameters[13].value or 0)
        rate_1 = float(parameters[14].value or 0)

        # Mixed-use land use 2
        land_use_2 = parameters[15].valueAsText or ""
        quantity_2 = float(parameters[16].value or 0)
        mitigations_2 = _parse_multivalue(parameters[17].valueAsText)
        emp_2 = float(parameters[18].value or 0)
        elig_2 = float(parameters[19].value or 0)
        rate_2 = float(parameters[20].value or 0)

        # Mixed-use land use 3
        land_use_3 = parameters[21].valueAsText or ""
        quantity_3 = float(parameters[22].value or 0)
        mitigations_3 = _parse_multivalue(parameters[23].valueAsText)
        emp_3 = float(parameters[24].value or 0)
        elig_3 = float(parameters[25].value or 0)
        rate_3 = float(parameters[26].value or 0)

        # Redevelopment
        redevelopment = bool(parameters[27].value)
        current_use = parameters[28].valueAsText or ""
        current_quantity = float(parameters[29].value or 0)
        current_custom_rate = float(parameters[30].value or 0)

        # ---- Resolve spatial context ------------------------------------
        apn = parameters[2].valueAsText or ""
        messages.addMessage(f"Resolving parcel for APN: {apn}")

        lon, lat = _lookup_parcel_centroid(apn, messages)
        if lon is None:
            arcpy.AddError(
                f"Could not resolve a parcel for APN '{apn}'. "
                "Verify the APN format (e.g. 019-030-005) and that the parcel is within the TRPA boundary."
            )
            return

        messages.addMessage(f"Parcel centroid (WGS84): lon={lon:.6f}, lat={lat:.6f}")

        # Zone lookup via TRPA REST service
        zone_id = _lookup_zone_rest(lon, lat, messages)
        if zone_id is None:
            arcpy.AddError(
                "The project location does not intersect a PIA analysis zone. "
                "Ensure the point is within the Lake Tahoe Region boundary."
            )
            return
        messages.addMessage(f"Zone: {zone_id}")

        # Town/regional center buffer (REST — Planning_Buffers layer 7)
        in_town_center = _point_in_buffer_service(lon, lat, _TOWN_CENTER_LAYER, messages)
        messages.addMessage(f"In town/regional center buffer: {in_town_center}")

        # Transit buffer — half-mile walking distance (REST — Planning_Buffers layer 5)
        in_transit = _point_in_buffer_service(lon, lat, _TRANSIT_LAYER, messages)
        messages.addMessage(f"In transit buffer: {in_transit}")

        # ---- Build calculator params ------------------------------------
        is_mixed = proj_type == "Mixed-Use"

        if is_mixed:
            # Collect only non-empty land use components
            components = [
                (land_use_1, quantity_1, mitigations_1, emp_1, elig_1, rate_1),
                (land_use_2, quantity_2, mitigations_2, emp_2, elig_2, rate_2),
                (land_use_3, quantity_3, mitigations_3, emp_3, elig_3, rate_3),
            ]
            active = [(lu, q, m, e, el, r) for (lu, q, m, e, el, r) in components if lu]

            if not active:
                arcpy.AddError(
                    "Mixed-Use selected but no land use components were provided. "
                    "Supply at least one land use in the Land Use 1 field."
                )
                return

            calc_params = {
                "proj_name": proj_name,
                "proj_type": "Mixed-Use",
                "zone_id": zone_id,
                "in_town_center": in_town_center,
                "in_transit": in_transit,
                "analysis_type": analysis_type,
                "land_uses": [c[0] for c in active],
                "quantities": [c[1] for c in active],
                "mitigations_list": [c[2] for c in active],
                "employee_vmt_pcts": [c[3] for c in active],
                "eligible_pcts": [c[4] for c in active],
                "custom_rates": [c[5] for c in active],
            }
        else:
            calc_params = {
                "proj_name": proj_name,
                "proj_type": proj_type,
                "zone_id": zone_id,
                "in_town_center": in_town_center,
                "in_transit": in_transit,
                "analysis_type": analysis_type,
                "quantity": quantity,
                "mitigations": mitigations,
                "employee_vmt_pct": employee_vmt_pct,
                "eligible_pct": eligible_pct,
                "custom_rate": custom_rate,
                "redevelopment": redevelopment,
                "current_use": current_use if redevelopment else None,
                "current_quantity": current_quantity,
                "current_custom_rate": current_custom_rate,
            }

        # ---- Run calculator ---------------------------------------------
        messages.addMessage("Calculating VMT impact...")

        # Force reload so ArcGIS Pro picks up any edits to pia_calculator.py
        # without requiring a full application restart.
        import importlib
        import pia_calculator as _pia_mod
        importlib.reload(_pia_mod)
        PIACalculator = _pia_mod.PIACalculator
        calc = PIACalculator(data_dir=_TOOL_DIR, analysis_type=analysis_type)

        try:
            result = calc.calculate(calc_params)
        except Exception as exc:
            arcpy.AddError(f"Calculation error: {exc}")
            raise

        # ---- Set outputs ------------------------------------------------
        parameters[31].value = zone_id
        parameters[32].value = in_town_center
        parameters[33].value = in_transit
        parameters[34].value = result.get("screened", "No")
        parameters[35].value = result.get("total_vmt", 0)
        parameters[36].value = result.get("standard_of_significance", 0)
        parameters[37].value = result.get("mitigation_needed", 0)
        parameters[38].value = result.get("total_mobility_fee", 0)
        parameters[39].value = json.dumps(result, indent=2)

        # ---- Summary messages -------------------------------------------
        lu_results = result.get("land_use_results", [])
        for lr in lu_results:
            messages.addMessage(
                f"  [{lr['land_use']}]  "
                f"proposed VMT={lr['proposed_vmt']:,.0f}  "
                f"net VMT={lr['net_vmt']:,.0f}  "
                f"SOS={lr['standard_of_significance']:,.0f}  "
                f"screened={lr['screened']}"
            )

        messages.addMessage(
            f"\nResults summary:"
            f"\n  Zone:                    {zone_id}"
            f"\n  Screened:                {result.get('screened', 'No')}"
            f"\n  Total Net VMT:           {result.get('total_vmt', 0):,.0f}"
            f"\n  Standard of Significance:{result.get('standard_of_significance', 0):,.0f}"
            f"\n  Mitigation Needed:       {result.get('mitigation_needed', 0):,.0f}"
            f"\n  Mobility Fee:            ${result.get('total_mobility_fee', 0):,.2f}"
        )

    def postExecute(self, parameters):
        return
