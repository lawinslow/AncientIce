
library(dplyr)
# ==========
# = Set WD =
# ==========
#argh absolute paths
#setwd("/Users/Battrd/Documents/School&Work/WiscResearch") # for ryan
#ice.new <- read.table("./AncientIce/lib/ice_data_prep/data/suwa_prepared_analysis_data.csv", sep=",", header=TRUE)

#pull from web (could change this to pull from submodule, but I always have trouble with them)
ice.new = read.csv('http://raw.githubusercontent.com/lawinslow/ice_data_prep/master/data/suwa_prepared_analysis_data.csv', header=TRUE, as.is=TRUE)

#subtract our iceon date from Jan 0 of that year
ice.new$doy = as.numeric(
	as.Date(ISOdate(ice.new$iceon_year, ice.new$iceon_month, ice.new$iceon_day)) - 
	as.Date(ISOdate(ice.new$rule_year+1, 1, 1, hour=0))
	, units='days')


ice.new$no.ice = NA
ice.new$no.ice[ice.new$froze == 'Y'] = 0
ice.new$no.ice[ice.new$froze == 'N'] = 1

suwa.old <- read.table("Data/suwa.tsv", sep="\t", header=TRUE)


suwa.new = select(ice.new, year=rule_year, no.ice, doy)

suwa.new = merge(suwa.new, suwa.old[,c('year', 'enso', 'co2', 'sunspots', 'air.t.as', 'aod', 'reff')])


