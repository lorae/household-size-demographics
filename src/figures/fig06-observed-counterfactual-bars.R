# #src/figures/fig06-observed-counterfactual-bars.R
# The purpose of this script is to define functions that produce bar charts showing
# 2000 observed, 2019 observed, and 2019 and expected (counterfactual) outcomes 
# of number of people in a household, number bedrooms in a household, and bedroom 
# crowding. Also an appendix version that does the same set of 3 bars by room.
#
# Input: various global vars for kob decomposition, such as kob_ppbr, defined in 
#        kob/scripts/kob-control-script.R
# Output: Functions defined below, called in kob/scripts/kob-control-script.R
#
# TODO: write a unit test or example script that does this. Also, have kob-control-script
# save the kob results before producing graphs, rather than relying on global vars.