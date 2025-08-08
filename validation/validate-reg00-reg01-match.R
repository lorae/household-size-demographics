# validation/validate-reg00-reg01-match.R
#
# The purpose of this script is to validate that coefficients from reg00 and reg01
# match after accounting for intercept values


# props
props_2000_00 <- readRDS("throughput/props00_2000.rds")