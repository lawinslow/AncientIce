
# ==========
# = Set WD =
# ==========
setwd("/Users/Battrd/Documents/School&Work/WiscResearch") # for ryan

# ========================
# = Read in New Data Set =
# ========================
ice.new <- read.table("./AncientIce/lib/ice_data_prep/data/suwa_prepared_analysis_data.csv", sep=",", header=TRUE) # read in new ice data from Luke
suwa.index <- ice.new[,"lakename"]=="LAKE SUWA" # index of rows in new data that belong to suwa
suwa.index.newCols <- c("rule_year","froze","iceon_year","iceon_month","iceon_day") # columns that we need from the new data to reconstruct the old data; i think we only need rule_year (matches to year), froze (can be converted to no.ice), and iceon_year+iceon_month+iceon_day (can be converted to doy)
suwa.new0 <- ice.new[suwa.index,suwa.index.newCols] # subset luke's ice data to just suwa, with columns relevant to Ancient Ice analysis


# =========================
# = Format New Data Dates =
# =========================
# First, create POSIX date system
suwa.new0[,"date"] <- mapply(paste, suwa.new0[,c("iceon_year")], suwa.new0[,c("iceon_month")], suwa.new0[,c("iceon_day")])
suwa.new0[suwa.new0[,"froze"]=="N","date"] <- NA_character_
suwa.new0[,"date"] <- as.POSIXct(suwa.new0[,"date"], format="%Y %m %d", tz="GMT")

# suwa.new0[,"doy.new"] <- format.Date(suwa.new0[,"date"], format="%j")

# Define "day of year" with reference to the rule year (ice on days from years before rule year should be negative)
new.doy <- difftime(suwa.new0[,"date"], as.POSIXct(paste0(suwa.new0[,"rule_year"],"-12-31"), tz="GMT"), units="days") # this result is confusing, because it is the old suwa doy - 1 ... does this mean that we were previously using doy+1 instead of doy? it's mostly important to be consistent with the definitions in the text, and shouldn't matter much to the analysis. I'll do +1 for now, but that doesn't seem right to me

new.doy.matchOld <- as.numeric(new.doy) - 1

suwa.new0[,"doy.new"] <- new.doy.matchOld

# change the "froze" column to factor which, when converted to numeric, will be equal to no.ice
suwa.new0[,"froze"] <- factor(suwa.new0[,"froze"], levels=c("Y","N"))


# =================================================
# = Create a data set that can be merged with old =
# =================================================
# trim to useful columns
suwa.new <- suwa.new0[,c("rule_year", "froze", "doy.new")]
names(suwa.new)[1] <- c("year") # rename to year for merge

# read in old data set
suwa.old <- read.table("./AncientIce/Data/suwa.tsv", sep="\t", header=TRUE)

# merge
suwa0 <- merge(suwa.old, suwa.new, all=TRUE, by="year")

# check for similarity in doy in merge
suwa0[,"doy"] - suwa0[,"doy.new"] # matches; but note the -1 issue in the line where I created new.doy

# check for similar in no.ice/froze merge
suwa0[,"no.ice"] - (as.numeric(suwa0[,"froze"])-1)

sum(suwa0[,"no.ice"], na.rm=TRUE) # previously we'd documented 37 of the years as years that didn't freeze
sum((as.numeric(suwa0[,"froze"])-1), na.rm=TRUE) # but now we only have 33 years that didn't freeze


