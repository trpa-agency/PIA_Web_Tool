#!/usr/bin/env Rscript
# pia_reference.R — Standalone R reference calculator extracted from app.R.
# No Shiny dependencies. Called by compare_pia.py to produce a ground-truth
# result for each test case.
#
# Usage:
#   Rscript pia_reference.R '<json_params>'
#
# JSON input keys (single land-use):
#   zone_id, in_town_center, in_transit, proj_type, quantity,
#   analysis_type, mitigations (array), employee_vmt_pct, eligible_pct,
#   custom_rate, redevelopment, current_use, current_quantity, current_custom_rate
#
# JSON output keys:
#   proposed_vmt, redev_vmt, net_vmt, sos, screened, mob_fee, mit_factor

suppressMessages({
  library(dplyr)
  library(readr)
  library(jsonlite)
})

# ---------------------------------------------------------------------------
# Paths — PIA_DATA_DIR env var is always set by compare_pia.py
# ---------------------------------------------------------------------------
data_root <- Sys.getenv("PIA_DATA_DIR", getwd())

# ---------------------------------------------------------------------------
# Data loading
# ---------------------------------------------------------------------------
load_zone_data <- function(analysis_type) {
  tr_path  <- file.path(data_root, "data", "trip_rates.csv")
  trip_rates <- read_csv(tr_path, show_col_types = FALSE) %>%
    filter(!use %in% c(
      "Single-Family Detached",
      "Senior Adult Housing – Attached",
      "Congregate Care Facility (Residential Care)",
      "Multi-Family (low-rise, one or two levels)"
    )) %>%
    filter(!is.na(Rate))

  if (analysis_type == "TRPA") {
    tl_path  <- file.path(data_root, "trip_length_table.csv")
    res_path <- file.path(data_root, "residential_data.csv")

    tl  <- read_csv(tl_path, show_col_types = FALSE) %>%
      select(zone_id, avg_zone_trip_length, SOS_trip_length) %>%
      rename(threshold_15_below = SOS_trip_length)

    res <- read_csv(res_path, show_col_types = FALSE) %>%
      select(zone_id, zone_vmt_per_capita, jur_total_vmt_capita) %>%
      mutate(sos = jur_total_vmt_capita * 0.85)

  } else {  # SB-743
    tl_path  <- file.path(data_root, "trip_length_data_full_length.csv")
    res_path <- file.path(data_root, "residential_data_full_length.csv")

    tl  <- read_csv(tl_path, show_col_types = FALSE) %>%
      select(zone_id, avg_zone_trip_length)

    res <- read_csv(res_path, show_col_types = FALSE) %>%
      select(zone_id, zone_vmt_per_capita, jur_total_vmt_capita, COUNTY) %>%
      mutate(sos = jur_total_vmt_capita * 0.85)

    jur_avgs <- c(CARSON=25.4, CSLT=6.35, DOUGLAS=11.4,
                  `EL DORADO`=10.0, PLACER=13.2, WASHOE=12.0)
    res <- res %>%
      mutate(threshold_15_below = jur_avgs[COUNTY] * 0.85)

    tl <- tl %>% left_join(res %>% select(zone_id, threshold_15_below), by = "zone_id")
  }

  zone_data <- tl %>% left_join(res %>% select(zone_id, zone_vmt_per_capita, sos), by = "zone_id")
  list(zone_data = zone_data, trip_rates = trip_rates)
}

# ---------------------------------------------------------------------------
# Land-use type sets (mirrors app.R)
# ---------------------------------------------------------------------------
KSF_USES <- c(
  "Auto Parts and Service Center", "General retail", "Furniture Store",
  "Pharmacy/Drugstore", "Supermarket", "Drive-In Bank",
  "High Turnover Sit-Down Restaurant (<1 hr. turnover)", "Fast Food Restaurant",
  "Quality Restaurant (>1 hr. turnover)", "Drinking Place",
  "Building Materials/Lumber", "Free-Standing Discount Store",
  "General Office Building (GFA of more than 5,000 sf)",
  "Medical –Dental Office Building", "Light industrial", "Warehouse",
  "Automobile Sales", "Health and Fitness Club",
  "Recreational Community Center", "Church", "Daycare Center", "Library", "Hospital"
)

TAU_USES <- c("Hotel", "Motel", "Timeshare")

RESIDENTIAL_FEE_USES <- c(
  "Residential (Market-Rate)", "Residential (Affordable)",
  "Hotel", "Motel", "Timeshare", "Developed Campground/RV Park"
)

COMMERCIAL_NO_SOS <- c(
  "Auto Parts and Service Center", "Automobile Sales", "Building Materials/Lumber",
  "Drinking Place", "Drive-In Bank", "Fast Food Restaurant",
  "Free-Standing Discount Store", "Furniture Store",
  "General Office Building (GFA of more than 5,000 sf)", "General retail",
  "Health and Fitness Club", "High Turnover Sit-Down Restaurant (<1 hr. turnover)",
  "Light industrial", "Medical –Dental Office Building", "Pharmacy/Drugstore",
  "Quality Restaurant (>1 hr. turnover)", "Supermarket", "Warehouse"
)

PUBLIC_SERVICE_SOS <- c("Church", "Daycare Center", "Hospital", "Library",
                         "Recreational Community Center")

NO_SOS_USES <- c("Bowling Alley", "Developed Campground/RV Park", "Golf Course",
                  "Marina", "Movie Theater (traditional)", "Unique Project Type")

SCHOOL_USES <- c("University/College", "High School", "Middle School/Junior High School",
                  "Elementary School", "Private School (K-12)")

# ---------------------------------------------------------------------------
# Mitigation factor  (composable — matches pia_calculator.py)
# ---------------------------------------------------------------------------
calc_mit_factor <- function(mitigations, emp_vmt_pct, elig_pct, in_town_center) {
  emp   <- emp_vmt_pct / 100
  elig  <- elig_pct  / 100
  f     <- 1.0

  for (m in mitigations) {
    f <- switch(m,
      "Traffic Calming"                                               = f * (1 - 0.01),
      "Unbundle Parking Costs from Property Cost"                     = f * (1 - 0.026),
      "End of Trip Facilities"                                        = f * (1 - 0.00625),
      "Employee Shuttle"                                              = f * (1 - 0.05 * emp),
      "Private Shuttle"                                               = f * (1 - 0.05 * (1 - emp)),
      "Implement CTR Program - Voluntary"                             = f * (1 - 0.05 * elig * emp),
      "Implement CTR Program -  Required Implementation/Monitoring"   = f * (1 - 0.21 * elig * emp),
      f  # unrecognised strategy — no change
    )
  }

  floor <- if (isTRUE(in_town_center)) 0.80 else 0.85
  max(f, floor)
}

# ---------------------------------------------------------------------------
# Raw VMT for one land use (before mitigation, before redev credit)
# Mirrors app.R tot_vmt_react_proposed / tot_vmt_react_redev logic
# ---------------------------------------------------------------------------
raw_vmt <- function(lu, qty, zone, trip_rates, custom_rate = 0) {
  atl <- zone$avg_zone_trip_length
  vmc <- zone$zone_vmt_per_capita

  get_rate <- function(use_name) {
    r <- trip_rates %>% filter(use == use_name) %>% pull(Rate)
    if (length(r) == 0) stop(paste("No trip rate found for:", use_name))
    r[1]
  }

  if (lu == "Residential (Market-Rate)") {
    vmc * 2.3 * qty
  } else if (lu == "Residential (Affordable)") {
    vmc * 2.3 * qty * 0.9
  } else if (lu == "Unique Project Type") {
    atl * qty * custom_rate
  } else if (lu %in% KSF_USES) {
    atl * (qty / 1000) * get_rate(lu)
  } else {
    # TAU, Marina, Park, Schools, Campground, etc.
    atl * qty * get_rate(lu)
  }
}

# ---------------------------------------------------------------------------
# Standard of significance (mirrors proj_sos_react in app.R)
# Note: app.R returns 0 when screened; we return the raw threshold here and
# let the caller zero it out, so compare_pia.py can compare both pre- and
# post-screening SOS if needed.
# ---------------------------------------------------------------------------
calc_sos <- function(lu, qty, zone, trip_rates) {
  thresh  <- zone$threshold_15_below   # trip-length SOS
  res_sos <- zone$sos                  # residential per-capita SOS

  get_rate <- function(use_name) {
    trip_rates %>% filter(use == use_name) %>% pull(Rate) %>% `[`(1)
  }

  if (lu %in% COMMERCIAL_NO_SOS || lu %in% NO_SOS_USES) {
    0
  } else if (lu %in% c("Residential (Market-Rate)", "Residential (Affordable)")) {
    round(res_sos * 2.3 * qty, 0)
  } else if (lu %in% TAU_USES || lu %in% SCHOOL_USES) {
    round(thresh * qty * get_rate(lu), 0)
  } else if (lu == "Public Park") {
    round(thresh * qty * get_rate(lu), 0)
  } else if (lu %in% PUBLIC_SERVICE_SOS) {
    round(thresh * (qty / 1000) * get_rate(lu), 0)
  } else {
    0
  }
}

# ---------------------------------------------------------------------------
# Screening logic (mirrors screened() reactive in app.R)
# ---------------------------------------------------------------------------
is_screened <- function(net_vmt, in_town_center, in_transit, lu) {
  if (net_vmt <= 715) return(TRUE)
  if (net_vmt > 715 && net_vmt <= 1300 && isTRUE(in_town_center)) return(TRUE)
  if (isTRUE(in_transit) && lu == "Residential (Affordable)") return(TRUE)
  FALSE
}

# ---------------------------------------------------------------------------
# Mobility fee (mirrors mob_fee_calc in app.R)
# ---------------------------------------------------------------------------
RESIDENTIAL_VMT_RATE <- 213.76
COMMERCIAL_VMT_RATE  <- 23.75

calc_mob_fee <- function(net_vmt, lu) {
  vmt <- round(net_vmt, 0)
  if (lu %in% RESIDENTIAL_FEE_USES) {
    vmt * RESIDENTIAL_VMT_RATE
  } else {
    vmt * COMMERCIAL_VMT_RATE
  }
}

# ---------------------------------------------------------------------------
# Main: parse args, run calculation, output JSON
# ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  cat('{"error": "No JSON argument supplied"}\n')
  quit(status = 1)
}

p <- tryCatch(fromJSON(args[1]), error = function(e) {
  cat(toJSON(list(error = paste("JSON parse error:", e$message)), auto_unbox = TRUE), "\n")
  quit(status = 1)
})

analysis_type    <- if (!is.null(p$analysis_type)) p$analysis_type else "TRPA"
zone_id          <- p$zone_id
in_town_center   <- isTRUE(p$in_town_center)
in_transit       <- isTRUE(p$in_transit)
lu               <- p$proj_type
qty              <- as.numeric(p$quantity)
mitigations      <- if (!is.null(p$mitigations)) p$mitigations else character(0)
emp_vmt_pct      <- if (!is.null(p$employee_vmt_pct)) as.numeric(p$employee_vmt_pct) else 0
elig_pct         <- if (!is.null(p$eligible_pct))     as.numeric(p$eligible_pct)     else 0
custom_rate      <- if (!is.null(p$custom_rate))       as.numeric(p$custom_rate)       else 0
redevelopment    <- isTRUE(p$redevelopment)
current_use      <- if (!is.null(p$current_use))       p$current_use                   else ""
current_qty      <- if (!is.null(p$current_quantity))  as.numeric(p$current_quantity)  else 0
current_cr       <- if (!is.null(p$current_custom_rate)) as.numeric(p$current_custom_rate) else 0

tryCatch({
  dat <- load_zone_data(analysis_type)
  zone_data  <- dat$zone_data
  trip_rates <- dat$trip_rates

  zone_row <- zone_data %>% filter(zone_id == !!zone_id)
  if (nrow(zone_row) == 0) stop(paste("Zone not found:", zone_id))
  zone <- as.list(zone_row[1, ])

  # Proposed VMT (no mitigation)
  proposed_vmt <- raw_vmt(lu, qty, zone, trip_rates, custom_rate)

  # Redevelopment credit
  redev_vmt <- 0
  if (redevelopment && nchar(current_use) > 0) {
    redev_vmt <- raw_vmt(current_use, current_qty, zone, trip_rates, current_cr)
  }

  # Apply mitigation
  mit_factor <- calc_mit_factor(mitigations, emp_vmt_pct, elig_pct, in_town_center)
  net_vmt    <- max(0, proposed_vmt * mit_factor - redev_vmt)

  # SOS and screening
  sos      <- calc_sos(lu, qty, zone, trip_rates)
  screened <- is_screened(net_vmt, in_town_center, in_transit, lu)

  # Mobility fee
  mob_fee <- calc_mob_fee(net_vmt, lu)

  out <- list(
    proposed_vmt = round(proposed_vmt, 2),
    redev_vmt    = round(redev_vmt, 2),
    net_vmt      = round(net_vmt, 2),
    sos          = sos,
    screened     = if (screened) "Yes" else "No",
    mob_fee      = round(mob_fee, 2),
    mit_factor   = round(mit_factor, 6)
  )
  cat(toJSON(out, auto_unbox = TRUE), "\n")

}, error = function(e) {
  cat(toJSON(list(error = e$message), auto_unbox = TRUE), "\n")
  quit(status = 1)
})
