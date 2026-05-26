"""
PIA (Project Impact Analysis) Calculator
Replicates the business logic from the TRPA PIA Web Tool (app.R).
All frontend inputs are accepted as function parameters; lookups use CSVs.
"""

import os
import math
from typing import Optional
import pandas as pd

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

COMMERCIAL_VMT_RATE = 23.75
RESIDENTIAL_VMT_RATE = 213.76

SCREEN_VMT_LOW = 715
SCREEN_VMT_HIGH = 1300

# Mitigation caps: minimum factor allowed after applying mitigations
MIT_CAP_TOWN_CENTER = 0.80   # project inside town/regional center buffer
MIT_CAP_DEFAULT = 0.85       # all other projects

# Persons per dwelling unit assumption used for residential VMT calc
PERSONS_PER_DU = 2.3

# Affordable housing VMT discount
AFFORDABLE_DISCOUNT = 0.90

# ---------------------------------------------------------------------------
# Land-use groupings
# ---------------------------------------------------------------------------

TAU_TYPES = {"Hotel", "Motel", "Timeshare"}

# Uses measured in KSF (user enters square feet; rate is per KSF)
KSF_USES = {
    "Auto Parts and Service Center",
    "Building Materials/Lumber",
    "Church",
    "Daycare Center",
    "Drinking Place",
    "Drive-In Bank",
    "Fast Food Restaurant",
    "Free-Standing Discount Store",
    "Furniture Store",
    "General Office Building (GFA of more than 5,000 sf)",
    "General retail",
    "Health and Fitness Club",
    "High Turnover Sit-Down Restaurant (<1 hr. turnover)",
    "Hospital",
    "Library",
    "Light industrial",
    "Medical –Dental Office Building",
    "Pharmacy/Drugstore",
    "Quality Restaurant (>1 hr. turnover)",
    "Recreational Community Center",
    "Supermarket",
    "Warehouse",
    "Automobile Sales",
}

SCHOOL_USES = {
    "University/College",
    "High School",
    "Middle School/Junior High School",
    "Elementary School",
    "Private School (K-12)",
}

RESIDENTIAL_TYPES = {"Residential (Market-Rate)", "Residential (Affordable)"}

# Commercial/office uses that have NO standard of significance
COMMERCIAL_NO_SOS = {
    "Auto Parts and Service Center",
    "Automobile Sales",
    "Building Materials/Lumber",
    "Drinking Place",
    "Drive-In Bank",
    "Fast Food Restaurant",
    "Free-Standing Discount Store",
    "Furniture Store",
    "General Office Building (GFA of more than 5,000 sf)",
    "General retail",
    "Health and Fitness Club",
    "High Turnover Sit-Down Restaurant (<1 hr. turnover)",
    "Light industrial",
    "Medical –Dental Office Building",
    "Pharmacy/Drugstore",
    "Quality Restaurant (>1 hr. turnover)",
    "Supermarket",
    "Warehouse",
}

# KSF public-service uses that DO get a SOS based on trip-length threshold
PUBLIC_SERVICE_SOS = {"Church", "Daycare Center", "Hospital", "Library", "Recreational Community Center"}

# Uses with no SOS regardless of type
NO_SOS_USES = {
    "Bowling Alley",
    "Developed Campground/RV Park",
    "Golf Course",
    "Marina",
    "Movie Theater (traditional)",
    "Unique Project Type",
}

# Uses where mobility fee uses the residential rate
RESIDENTIAL_FEE_TYPES = RESIDENTIAL_TYPES | TAU_TYPES | {"Developed Campground/RV Park"}

# ---------------------------------------------------------------------------
# Jurisdiction trip-length averages used to derive threshold_15_below
# ---------------------------------------------------------------------------

JUR_TRIP_LENGTH_AVG = {
    "TRPA": {
        "CARSON": 13.1,
        "CSLT": 4.26,
        "DOUGLAS": 6.37,
        "EL DORADO": 6.69,
        "PLACER": 6.51,
        "WASHOE": 5.56,
    },
    "SB-743": {
        "CARSON": 25.4,
        "CSLT": 6.35,
        "DOUGLAS": 11.4,
        "EL DORADO": 10.0,
        "PLACER": 13.2,
        "WASHOE": 12.0,
    },
}

# ---------------------------------------------------------------------------
# Mitigation reduction amounts
# ---------------------------------------------------------------------------

# Fixed (non-parametric) mitigation factors
FIXED_MITIGATION_REDUCTIONS = {
    "Traffic Calming": 0.01,
    "Unbundle Parking Costs from Property Cost": 0.026,
    "End of Trip Facilities": 0.00625,
}


# ---------------------------------------------------------------------------
# Helper: resolve quantity for a land use
# ---------------------------------------------------------------------------

def _resolve_quantity(land_use: str, params: dict) -> float:
    """
    Extract the relevant quantity from *params* for the given *land_use*.
    The caller is expected to pass the right key; this is a convenience
    fallback that also accepts a plain ``quantity`` key for all types.
    """
    if "quantity" in params:
        return float(params["quantity"])
    if land_use in RESIDENTIAL_TYPES:
        return float(params.get("res_units", params.get("units", 0)))
    if land_use in TAU_TYPES:
        return float(params.get("taus", params.get("units", 0)))
    if land_use == "Marina":
        return float(params.get("berths", params.get("units", 0)))
    if land_use == "Public Park":
        return float(params.get("acres", params.get("units", 0)))
    if land_use == "Golf Course":
        return float(params.get("holes", params.get("units", 0)))
    if land_use == "Bowling Alley":
        return float(params.get("lanes", params.get("units", 0)))
    if land_use == "Movie Theater (traditional)":
        return float(params.get("screens", params.get("units", 0)))
    if land_use == "Developed Campground/RV Park":
        return float(params.get("sites", params.get("units", 0)))
    if land_use in SCHOOL_USES:
        return float(params.get("students", params.get("units", 0)))
    if land_use == "Unique Project Type":
        return float(params.get("custom_units", params.get("units", 0)))
    # KSF uses — user enters square feet
    return float(params.get("ksf", params.get("sqft", params.get("units", 0))))


# ---------------------------------------------------------------------------
# Core calculator class
# ---------------------------------------------------------------------------

class PIACalculator:
    """
    Load zone and trip-rate data once, then call ``calculate()`` for each
    project submission from the frontend.

    Parameters
    ----------
    data_dir : str
        Root directory of the PIA Web Tool repository (contains the CSVs).
    analysis_type : str
        "TRPA" (default) or "SB-743" — selects which zone dataset to load.
    """

    def __init__(self, data_dir: str = ".", analysis_type: str = "TRPA"):
        self.data_dir = data_dir
        self.analysis_type = analysis_type
        self._load_data()

    # ------------------------------------------------------------------
    # Data loading
    # ------------------------------------------------------------------

    def _load_data(self):
        data_dir = self.data_dir
        at = self.analysis_type

        # --- Trip rates (shared by both analysis types) ---
        trip_path = os.path.join(data_dir, "data", "trip_rates.csv")
        tr_df = pd.read_csv(trip_path).dropna(subset=["Rate"])
        self.trip_rates: dict[str, float] = tr_df.set_index("use")["Rate"].to_dict()

        # --- Zone data ---
        if at == "TRPA":
            # trip_length_table.csv has pre-computed avg_zone_trip_length and
            # SOS_trip_length (= jurisdiction avg * 0.85) for TRPA thresholds.
            tl_path = os.path.join(data_dir, "trip_length_table.csv")
            tl_df = pd.read_csv(tl_path)[
                ["zone_id", "avg_zone_trip_length", "SOS_trip_length", "Jurisdiction"]
            ].rename(
                columns={
                    "SOS_trip_length": "trip_length_threshold",
                    "Jurisdiction": "county",
                }
            )
            res_path = os.path.join(data_dir, "residential_data.csv")
            res_df = pd.read_csv(res_path)[
                ["zone_id", "zone_vmt_per_capita", "jur_total_vmt_capita"]
            ]
            res_df["residential_sos"] = res_df["jur_total_vmt_capita"] * 0.85

        else:  # SB-743
            tl_path = os.path.join(data_dir, "trip_length_data_full_length.csv")
            tl_raw = pd.read_csv(tl_path)[["zone_id", "avg_zone_trip_length"]]

            res_path = os.path.join(data_dir, "residential_data_full_length.csv")
            res_df = pd.read_csv(res_path)[
                ["zone_id", "zone_vmt_per_capita", "jur_total_vmt_capita", "COUNTY"]
            ].rename(columns={"COUNTY": "county"})
            res_df["residential_sos"] = res_df["jur_total_vmt_capita"] * 0.85

            # Compute trip-length threshold from hardcoded SB-743 jurisdiction averages
            jur_avgs = JUR_TRIP_LENGTH_AVG["SB-743"]
            res_df["trip_length_threshold"] = res_df["county"].map(jur_avgs) * 0.85

            tl_df = tl_raw.merge(
                res_df[["zone_id", "county", "trip_length_threshold"]], on="zone_id"
            )

        zone_df = tl_df.merge(res_df[["zone_id", "zone_vmt_per_capita", "residential_sos"]], on="zone_id")
        self.zone_data: pd.DataFrame = zone_df.set_index("zone_id")

    # ------------------------------------------------------------------
    # Lookup helpers
    # ------------------------------------------------------------------

    def get_zone(self, zone_id: str) -> pd.Series:
        if zone_id not in self.zone_data.index:
            raise ValueError(f"Zone '{zone_id}' not found.")
        return self.zone_data.loc[zone_id]

    def get_trip_rate(self, land_use: str) -> float:
        rate = self.trip_rates.get(land_use)
        if rate is None:
            raise ValueError(f"No trip rate found for land use '{land_use}'.")
        return float(rate)

    # ------------------------------------------------------------------
    # Mitigation factor (composable — no giant if-else chain)
    # ------------------------------------------------------------------

    def compute_mitigation_factor(
        self,
        mitigations: list[str],
        employee_vmt_pct: float = 0.0,
        eligible_pct: float = 0.0,
    ) -> float:
        """
        Return cumulative mitigation multiplier (0–1).

        Each selected mitigation reduces VMT independently:
        - Fixed strategies apply a flat percentage reduction.
        - Employee/CTR strategies scale by employee_vmt_pct and eligible_pct.

        Parameters
        ----------
        mitigations : list[str]
            Strategy names from the frontend checkbox group.
        employee_vmt_pct : float
            Percent of total project VMT attributable to employees (0–100).
        eligible_pct : float
            Percent of employees eligible for CTR / shuttle programs (0–100).
        """
        emp_vmt = employee_vmt_pct / 100.0
        eligible = eligible_pct / 100.0
        factor = 1.0

        for mit in mitigations:
            if mit in FIXED_MITIGATION_REDUCTIONS:
                factor *= 1.0 - FIXED_MITIGATION_REDUCTIONS[mit]
            elif mit == "Employee Shuttle":
                factor *= 1.0 - (0.05 * emp_vmt)
            elif mit == "Private Shuttle":
                factor *= 1.0 - (0.05 * (1.0 - emp_vmt))
            elif mit == "Implement CTR Program - Voluntary":
                factor *= 1.0 - (0.05 * eligible * emp_vmt)
            elif mit == "Implement CTR Program -  Required Implementation/Monitoring":
                factor *= 1.0 - (0.21 * eligible * emp_vmt)

        return factor

    def apply_mitigation_cap(self, factor: float, in_town_center: bool) -> float:
        """
        Enforce the maximum allowed VMT reduction based on project location.
        Factor cannot drop below 0.80 (town center) or 0.85 (all others).
        """
        floor = MIT_CAP_TOWN_CENTER if in_town_center else MIT_CAP_DEFAULT
        return max(factor, floor)

    # ------------------------------------------------------------------
    # Raw VMT for a single land use (before mitigation and redev credit)
    # ------------------------------------------------------------------

    def _raw_vmt(
        self,
        land_use: str,
        quantity: float,
        zone: pd.Series,
        custom_rate: float = 0.0,
    ) -> float:
        """
        Un-mitigated VMT for one land use component.

        *quantity* is always in the native frontend unit:
        - KSF uses: square feet (divided by 1 000 internally)
        - Residential: dwelling units
        - TAU: tourist accommodation units
        - Other: the natural unit shown in the UI (berths, holes, etc.)
        """
        atl = float(zone["avg_zone_trip_length"])
        vmc = float(zone["zone_vmt_per_capita"])

        if land_use == "Residential (Market-Rate)":
            return vmc * PERSONS_PER_DU * quantity

        if land_use == "Residential (Affordable)":
            return vmc * PERSONS_PER_DU * quantity * AFFORDABLE_DISCOUNT

        if land_use == "Unique Project Type":
            return atl * quantity * custom_rate

        if land_use in KSF_USES:
            return atl * (quantity / 1_000.0) * self.get_trip_rate(land_use)

        # All remaining uses (TAU, Marina, Park, Schools, etc.)
        return atl * quantity * self.get_trip_rate(land_use)

    # ------------------------------------------------------------------
    # Standard of significance for a single land use
    # ------------------------------------------------------------------

    def _standard_of_significance(
        self,
        land_use: str,
        quantity: float,
        zone: pd.Series,
    ) -> float:
        """
        Return the VMT standard of significance for this land use.
        Returns 0 when TRPA has not established a numeric SOS for the type.
        """
        res_sos = float(zone["residential_sos"])
        tl_thresh = float(zone["trip_length_threshold"])

        if land_use in COMMERCIAL_NO_SOS or land_use in NO_SOS_USES:
            return 0.0

        if land_use in RESIDENTIAL_TYPES:
            return res_sos * PERSONS_PER_DU * quantity

        if land_use in TAU_TYPES or land_use in SCHOOL_USES:
            return tl_thresh * quantity * self.get_trip_rate(land_use)

        if land_use == "Public Park":
            return tl_thresh * quantity * self.get_trip_rate(land_use)

        if land_use in PUBLIC_SERVICE_SOS:
            return tl_thresh * (quantity / 1_000.0) * self.get_trip_rate(land_use)

        return 0.0

    # ------------------------------------------------------------------
    # Single-component calculation
    # ------------------------------------------------------------------

    def calculate_land_use(
        self,
        land_use: str,
        quantity: float,
        zone_id: str,
        in_town_center: bool,
        in_transit: bool,
        mitigations: Optional[list] = None,
        employee_vmt_pct: float = 0.0,
        eligible_pct: float = 0.0,
        custom_rate: float = 0.0,
        redev_land_use: Optional[str] = None,
        redev_quantity: float = 0.0,
        redev_custom_rate: float = 0.0,
    ) -> dict:
        """
        Calculate VMT impact for one land-use component.

        Parameters
        ----------
        land_use : str
            Project type string exactly as it appears in the frontend dropdown.
        quantity : float
            Size of the project in the unit appropriate for the land use type:
            sq ft for KSF uses, units for residential, TAUs for hotel, etc.
        zone_id : str
            PIA analysis zone (e.g. "Zone 5") — identifies the clicked map zone.
        in_town_center : bool
            True if the project parcel is inside the town/regional center buffer.
        in_transit : bool
            True if the project parcel is inside the transit buffer.
        mitigations : list[str], optional
            Selected mitigation strategy names.
        employee_vmt_pct : float
            Percent of total project VMT from employees (slider 0–100).
        eligible_pct : float
            Percent of employees eligible for CTR/shuttle (slider 0–100).
        custom_rate : float
            Daily trip rate for "Unique Project Type"; ignored otherwise.
        redev_land_use : str, optional
            Existing land use being replaced (redevelopment credit).
        redev_quantity : float
            Size of existing use in its native unit.
        redev_custom_rate : float
            Trip rate for existing "Unique Project Type".

        Returns
        -------
        dict with keys:
            land_use, zone_id, quantity, avg_zone_trip_length,
            zone_vmt_per_capita, residential_sos, trip_length_threshold,
            proposed_vmt, redev_vmt, mitigation_factor, mitigation_factor_capped,
            mitigation_reduction_pct, net_vmt, standard_of_significance,
            screened, mitigation_needed, mobility_fee,
            in_town_center, in_transit, mitigations
        """
        if mitigations is None:
            mitigations = []

        zone = self.get_zone(zone_id)
        mit_factor = self.compute_mitigation_factor(mitigations, employee_vmt_pct, eligible_pct)
        mit_factor_capped = self.apply_mitigation_cap(mit_factor, in_town_center)

        proposed_vmt = self._raw_vmt(land_use, quantity, zone, custom_rate)

        redev_vmt = 0.0
        if redev_land_use:
            redev_vmt = self._raw_vmt(redev_land_use, redev_quantity, zone, redev_custom_rate)

        net_vmt = max((proposed_vmt * mit_factor_capped) - redev_vmt, 0.0)
        sos = self._standard_of_significance(land_use, quantity, zone)

        # Screening logic
        affordable_with_transit = (land_use == "Residential (Affordable)" and in_transit)
        if net_vmt <= SCREEN_VMT_LOW:
            screened = True
        elif SCREEN_VMT_LOW < net_vmt <= SCREEN_VMT_HIGH and in_town_center:
            screened = True
        elif affordable_with_transit:
            screened = True
        else:
            screened = False

        mit_needed = max(net_vmt - sos, 0.0) if not screened else 0.0

        # Mobility fee
        if land_use in RESIDENTIAL_FEE_TYPES:
            mob_fee = round(net_vmt) * RESIDENTIAL_VMT_RATE
        else:
            mob_fee = round(net_vmt) * COMMERCIAL_VMT_RATE

        return {
            "land_use": land_use,
            "zone_id": zone_id,
            "quantity": quantity,
            "avg_zone_trip_length": float(zone["avg_zone_trip_length"]),
            "zone_vmt_per_capita": float(zone["zone_vmt_per_capita"]),
            "residential_sos": float(zone["residential_sos"]),
            "trip_length_threshold": float(zone["trip_length_threshold"]),
            "proposed_vmt": round(proposed_vmt, 2),
            "redev_vmt": round(redev_vmt, 2),
            "mitigation_factor": round(mit_factor, 6),
            "mitigation_factor_capped": round(mit_factor_capped, 6),
            "mitigation_reduction_pct": round((1 - mit_factor_capped) * 100, 2),
            "net_vmt": round(net_vmt, 2),
            "standard_of_significance": round(sos, 0),
            "screened": "Yes" if screened else "No",
            "mitigation_needed": round(mit_needed, 2),
            "mobility_fee": round(mob_fee, 2),
            "in_town_center": in_town_center,
            "in_transit": in_transit,
            "mitigations": mitigations,
        }

    # ------------------------------------------------------------------
    # Top-level project calculation (single or mixed-use)
    # ------------------------------------------------------------------

    def calculate(self, params: dict) -> dict:
        """
        Main entry point — replicates all Shiny server logic.

        Parameters (single land-use project)
        -------------------------------------
        proj_name        str       Display name for the project.
        proj_type        str       Land use type from the frontend dropdown.
        zone_id          str       Selected analysis zone (e.g. "Zone 5").
        in_town_center   bool      True if parcel is in town/regional buffer.
        in_transit       bool      True if parcel is in transit buffer.
        quantity         float     Project size in native units for the land use.
        mitigations      list[str] Selected mitigation strategies (optional).
        employee_vmt_pct float     % of VMT from employees (0–100, default 0).
        eligible_pct     float     % of employees eligible for programs (0–100).
        custom_rate      float     Trip rate for "Unique Project Type" only.
        redevelopment    bool      True if existing use is being replaced.
        current_use      str       Existing land use type (if redevelopment).
        current_quantity float     Existing use size in native units.
        current_custom_rate float  Trip rate for existing "Unique Project Type".

        Additional parameters (Mixed-Use project, proj_type == "Mixed-Use")
        -------------------------------------------------------------------
        land_uses           list[str]        Up to three land use types.
        quantities          list[float]      Sizes matching land_uses.
        mitigations_list    list[list[str]]  Mitigations for each land use.
        employee_vmt_pcts   list[float]      Per-component employee VMT %.
        eligible_pcts       list[float]      Per-component eligible employee %.
        custom_rates        list[float]      Custom rates for Unique types.

        Returns
        -------
        dict with:
            proj_name, proj_type, zone_id, in_town_center, in_transit,
            analysis_type, land_use_results (list of per-component dicts),
            total_vmt, total_mobility_fee, screened (str "Yes"/"No"),
            and for single land use: standard_of_significance, mitigation_needed.
        """
        proj_type = params["proj_type"]
        zone_id = params["zone_id"]
        in_town_center = bool(params.get("in_town_center", False))
        in_transit = bool(params.get("in_transit", False))

        result = {
            "proj_name": params.get("proj_name", ""),
            "proj_type": proj_type,
            "zone_id": zone_id,
            "in_town_center": in_town_center,
            "in_transit": in_transit,
            "analysis_type": params.get("analysis_type", self.analysis_type),
            "land_use_results": [],
        }

        # ---- Mixed-Use -----------------------------------------------
        if proj_type == "Mixed-Use":
            land_uses = params.get("land_uses", [])
            quantities = params.get("quantities", [0.0] * len(land_uses))
            mits_list = params.get("mitigations_list", [[] for _ in land_uses])
            emp_vmts = params.get("employee_vmt_pcts", [0.0] * len(land_uses))
            eligibles = params.get("eligible_pcts", [0.0] * len(land_uses))
            custom_rates = params.get("custom_rates", [0.0] * len(land_uses))

            lu_results = []
            for lu, qty, mits, emp, elig, cr in zip(
                land_uses, quantities, mits_list, emp_vmts, eligibles, custom_rates
            ):
                lu_results.append(
                    self.calculate_land_use(
                        land_use=lu,
                        quantity=qty,
                        zone_id=zone_id,
                        in_town_center=in_town_center,
                        in_transit=in_transit,
                        mitigations=mits,
                        employee_vmt_pct=emp,
                        eligible_pct=elig,
                        custom_rate=cr,
                    )
                )

            result["land_use_results"] = lu_results

            # Internal capture: residential + retail combination gets a 10% VMT
            # reduction. Per app.R behavior, this discount applies to total VMT only;
            # mobility fees are summed from undiscounted per-component values.
            has_res_or_tau = any(
                lu in (RESIDENTIAL_TYPES | TAU_TYPES) for lu in land_uses
            )
            has_retail = "General retail" in land_uses
            total_vmt = sum(r["net_vmt"] for r in lu_results)
            if has_res_or_tau and has_retail:
                total_vmt *= 0.9

            result["total_vmt"] = round(total_vmt, 2)
            result["total_mobility_fee"] = round(sum(r["mobility_fee"] for r in lu_results), 2)

            # Screening for mixed-use
            # Affordable housing on transit is excluded from screen VMT
            affordable_transit_idx = None
            if in_transit:
                for i, lu in enumerate(land_uses):
                    if lu == "Residential (Affordable)":
                        affordable_transit_idx = i
                        break

            if affordable_transit_idx is not None:
                screen_vmt = sum(
                    r["net_vmt"]
                    for i, r in enumerate(lu_results)
                    if i != affordable_transit_idx
                )
            else:
                screen_vmt = total_vmt

            if screen_vmt <= SCREEN_VMT_LOW:
                result["screened"] = "Yes"
            elif SCREEN_VMT_LOW < screen_vmt <= SCREEN_VMT_HIGH and in_town_center:
                result["screened"] = "Yes"
            else:
                result["screened"] = "No"

        # ---- Single land use -----------------------------------------
        else:
            lu_result = self.calculate_land_use(
                land_use=proj_type,
                quantity=_resolve_quantity(proj_type, params),
                zone_id=zone_id,
                in_town_center=in_town_center,
                in_transit=in_transit,
                mitigations=params.get("mitigations", []),
                employee_vmt_pct=params.get("employee_vmt_pct", 0.0),
                eligible_pct=params.get("eligible_pct", 0.0),
                custom_rate=params.get("custom_rate", 0.0),
                redev_land_use=params.get("current_use") if params.get("redevelopment") else None,
                redev_quantity=_resolve_quantity(
                    params.get("current_use", proj_type),
                    {k.replace("current_", ""): v for k, v in params.items()},
                ) if params.get("redevelopment") else 0.0,
                redev_custom_rate=params.get("current_custom_rate", 0.0),
            )

            result["land_use_results"] = [lu_result]
            result["total_vmt"] = lu_result["net_vmt"]
            result["total_mobility_fee"] = lu_result["mobility_fee"]
            result["screened"] = lu_result["screened"]
            result["standard_of_significance"] = lu_result["standard_of_significance"]
            result["mitigation_needed"] = lu_result["mitigation_needed"]

        return result


# ---------------------------------------------------------------------------
# Convenience: build a calculator and call it in one shot
# ---------------------------------------------------------------------------

def calculate_pia(params: dict, data_dir: str = ".") -> dict:
    """
    Stateless wrapper. Loads data on every call — use PIACalculator directly
    when processing multiple projects.
    """
    calc = PIACalculator(data_dir=data_dir, analysis_type=params.get("analysis_type", "TRPA"))
    return calc.calculate(params)


# ---------------------------------------------------------------------------
# Example / smoke test
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    calc = PIACalculator(data_dir=".", analysis_type="TRPA")

    result = calc.calculate({
        "proj_name": "Example Hotel",
        "proj_type": "Hotel",
        "zone_id": "Zone 5",
        "quantity": 50,           # 50 TAUs
        "in_town_center": True,
        "in_transit": False,
        "mitigations": ["Traffic Calming", "Employee Shuttle"],
        "employee_vmt_pct": 30,
        "eligible_pct": 0,
    })

    lu = result["land_use_results"][0]
    print(f"Project:              {result['proj_name']}")
    print(f"Zone:                 {result['zone_id']}")
    print(f"Avg Trip Length:      {lu['avg_zone_trip_length']:.2f} mi")
    print(f"Proposed VMT:         {lu['proposed_vmt']:,.0f}")
    print(f"Mitigation reduction: {lu['mitigation_reduction_pct']:.1f}%")
    print(f"Net VMT:              {lu['net_vmt']:,.0f}")
    print(f"Standard of Sig.:     {lu['standard_of_significance']:,.0f}")
    print(f"Screened:             {lu['screened']}")
    print(f"Mitigation Needed:    {lu['mitigation_needed']:,.0f}")
    print(f"Mobility Fee:         ${lu['mobility_fee']:,.2f}")
