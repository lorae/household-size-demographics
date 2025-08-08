# validation/validate-reg00-reg01-match.R
#
# The purpose of this script is to validate that coefficients from reg00 and reg01
# match after accounting for intercept values

library(tibble)
library(purrr)
library(dplyr)

# Helper functions ----

# Extract proportion from table
extract_prop <- function(svystat_obj, year) {
  # Calculate population proportions, SE on those estimates, and extract the variable
  # "value" (varname concatenated with value of variable, like "AGE_bucket0-4")
  prop <- as.numeric(svystat_obj)
  se <- sqrt(diag(attr(svystat_obj, "var")))
  term <- names(svystat_obj)
  
  # turn output into tibble and name cols using the year
  tibble::tibble(
    term = term,
    !!paste0("prop_", year) := prop,
    !!paste0("prop_", year, "_se") := se
  )
}

# Read in coefficient data from 2000
read_coefs_2000 <- function(path, adjust_by) {
  output <- readRDS(path) |>
    select(term, estimate, std.error) |>
    rename(
      coef_2000 = estimate,
      coef_2000_se = std.error
    )

  return(output)
}

# Read in coefficient data from 2019
read_coefs_2019 <- function(path, adjust_by) {
  output <- readRDS(path) |>
    rename(
      coef_2019 = estimate,
      coef_2019_se = se_estimate
    )

  return(output)
}

# Comparison helpers -----
compare_props <- function(year, reg00_path, reg01_path, tol = 1e-6) {
  r00 <- readRDS(reg00_path) |> purrr::map_dfr(extract_prop, year = year)
  r01 <- readRDS(reg01_path) |> purrr::map_dfr(extract_prop, year = year)
  res <- all.equal(r01, r00, tolerance = tol)
  tibble::tibble(kind = "props",
                 year = year,
                 ok = isTRUE(res),
                 detail = list(res))  # list-col for the all.equal output
}

read_coefs <- function(path, year) {
  out <- readRDS(path)
  if (year == 2000) {
    out |>
      dplyr::select(term, estimate, std.error) |>
      dplyr::rename(!!paste0("coef_", year) := estimate,
                    !!paste0("coef_", year, "_se") := std.error)
  } else if (year == 2019) {
    out |>
      dplyr::rename(!!paste0("coef_", year) := estimate,
                    !!paste0("coef_", year, "_se") := se_estimate)
  } else stop("Unsupported year")
}

compare_coefs <- function(year,
                          reg00_path, reg01_path,
                          intercept_var, ref_level,
                          tol = 1e-6) {
  coef_col <- paste0("coef_", year)
  se_col   <- paste0("coef_", year, "_se")
  
  r00_full <- read_coefs(reg00_path, year) |>
    split_term_column() |>
    add_intercept_v2(variable = intercept_var,
                     reference_value = ref_level,
                     coef_col = coef_col,
                     se_col   = se_col) |>
    dplyr::arrange(term)
  
  r01_full <- read_coefs(reg01_path, year) |>
    split_term_column() |>
    dplyr::arrange(term)
  
  # Compare COEFS ONLY
  r00 <- r00_full |> dplyr::select(term, variable, value, !!coef_col)
  r01 <- r01_full |> dplyr::select(term, variable, value, !!coef_col)
  
  eq <- all.equal(r01, r00, tolerance = tol)
  
  # handy diff metric for debugging (max absolute diff)
  max_abs_diff <- r01 |>
    dplyr::left_join(r00, by = c("term","variable","value"),
                     suffix = c("_01","_00")) |>
    dplyr::mutate(diff = abs(.data[[paste0(coef_col, "_01")]] -
                               .data[[paste0(coef_col, "_00")]])) |>
    dplyr::summarise(max_abs_diff = max(diff, na.rm = TRUE)) |>
    dplyr::pull(max_abs_diff)
  
  tibble::tibble(
    kind   = "coefs",
    year   = year,
    ok     = isTRUE(eq),
    detail = list(eq),
    max_abs_diff = max_abs_diff,
    peek_r00 = list(r00_full |> dplyr::filter(variable %in% c(intercept_var, "(Intercept)"))),
    peek_r01 = list(r01_full |> dplyr::filter(variable %in% c(intercept_var, "(Intercept)")))
  )
}



# PROPS
prop_cfg <- tribble(
  ~year, ~reg00_path,                               ~reg01_path,
  2000,  "throughput/reg00/props00_2000.rds",       "throughput/reg01/2000_prop.rds",
  2019,  "throughput/reg00/props00_2019.rds",       "throughput/reg01/2019_prop.rds"
)

prop_results <- prop_cfg |>
  pmap(compare_props)

print(prop_results)

# COEFS (bedroom now; add more measures later)
coef_cfg <- tribble(
  ~measure,   ~year, ~reg00_path,                                         ~reg01_path,                 ~intercept_var,     ~ref_level,
  "bedroom",  2000,  "throughput/reg00/model00_2000_bedroom_summary.rds", "throughput/reg01/2000_b.rds", "RACE_ETH_bucket", "AAPI",
  "bedroom",  2019,  "throughput/reg00/model00_2019_bedroom_summary-v5.rds","throughput/reg01/2019_b.rds","RACE_ETH_bucket", "AAPI"
  # add rows for "ppr", "ppbr", "persons", "rooms" w/ the correct paths
)

coef_results <- coef_cfg |>
  mutate(res = pmap(list(year, reg00_path, reg01_path, intercept_var, ref_level),
                    ~ compare_coefs(..1, ..2, ..3, ..4, ..5))) |>
  mutate(ok = map_lgl(res, "ok"),
         detail = map(res, "detail")) |>
  select(-res)

print(coef_results)

