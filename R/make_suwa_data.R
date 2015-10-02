
library(plyr)

# ==========
# = Set WD =
# ==========
#argh absolute paths
# setwd("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce") # for ryan
#ice.new <- read.table("./AncientIce/lib/ice_data_prep/data/suwa_prepared_analysis_data.csv", sep=",", header=TRUE)

#pull from web (could change this to pull from submodule, but I always have trouble with them)
ice.new <- read.csv('lib/ice_data_prep/data/suwa_prepared_analysis_data.csv', header=TRUE, as.is=TRUE)


#subtract our iceon date from Jan 0 of that year
ice.new$doy <- as.numeric(
	as.Date(ISOdate(ice.new$iceon_year, ice.new$iceon_month, ice.new$iceon_day)) - 
	as.Date(ISOdate(ice.new$rule_year+1, 1, 1, hour=0)-0) #the minus 1 makes it Jan 0 # removed the -1 b/c we need Dec 31 and Jan 1 to be consecutive integers; I.e., Jan 1 needs to be 0, and Dec 31 needs to be -1.
	, units='days')


ice.new$no.ice <- NA
ice.new$no.ice[ice.new$froze == 'Y'] <- 0
ice.new$no.ice[ice.new$froze == 'N'] <- 1

suwa.old <- read.table("./Data/suwa.old.tsv", sep="\t", header=TRUE)


suwa.new <- ice.new[, c('rule_year', 'no.ice', 'doy')]
names(suwa.new) <- c('year', 'no.ice', 'doy')

suwa.new <- merge(suwa.new, suwa.old[,c('year', 'enso', 'co2', 'sunspots', 'air.t.as', 'aod', 'reff')], all=TRUE)


add.new <- suwa.new[,"year"]%in%c(2005:2014)
add.old <- suwa.old[,"year"]%in%c(2005:2014)
suwa.new[add.new,c("no.ice","doy")] <- suwa.old[add.old,c("no.ice","doy")]


# ==================
# = Uncorrect Suwa =
# ==================
uncorrect.index <- suwa.new[,"year"] <= 1872
suwa.uncorr <- suwa.new
suwa.uncorr[uncorrect.index,"doy"] <- suwa.new[uncorrect.index,"doy"] + 30


# =========================
# = Add latest SUWA years =
# =========================

new_data = data.frame(year=2005:2014, 
                      no.ice=c(0, 1, 0, 1, 1, 0, 0, 0, 1, 1),
                      doy=c(-2, NA, 25, NA, NA, 11, 30, 6, NA, NA))
suwa.uncorr = rbind.fill(suwa.uncorr, new_data)


# ===================================
# = Add in reconstructed Kyoto City =
# ===================================



# ===============
# = Save Output =
# ===============
write.table(suwa.uncorr, file="Data/suwa.tsv", sep="\t", row.names=FALSE)


