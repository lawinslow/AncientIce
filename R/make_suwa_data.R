
# ==========
# = Set WD =
# ==========
setwd("/Users/Battrd/Documents/School&Work/WiscResearch") # for ryan

ice.new <- read.table("./AncientIce/lib/ice_data_prep/data/suwa_prepared_analysis_data.csv", sep=",", header=TRUE)
suwa.index <- ice.new[,"lakename"]=="LAKE SUWA" 
suwa.new <- ice.new[]


suwa.old <- read.table("./AncientIce/Data/suwa.tsv", sep="\t", header=TRUE)