#Create R version of Figure 2

suwa <- read.table("Data/suwa.tsv", sep="\t", header=TRUE)
torn = read.table("Data/tornio.tsv", sep='\t', header=TRUE)

#because these are weird, I'm just fixing these

suwa_bin_tops = c(seq(1499, 1949, by=50), 2014)
suwa_bin_bots = c(1443, seq(1500, 1950, by=50))
torn_bin_tops = c(seq(1749, 1949, by=50), 2013)
torn_bin_bots = c(1693, seq(1750, 1950, by=50))

torn_cutoff = 124
suwa_extreme = NA
torn_extreme = NA
torn_names = ''
suwa_names = ''

for(i in 1:length(suwa_bin_tops)){
	bin_i = suwa$year >= suwa_bin_bots[i] & suwa$year <= suwa_bin_tops[i]
	suwa_extreme[i] = sum(suwa[bin_i,]$no.ice == 1 & !is.na(suwa[bin_i,]$no.ice))/nrow(suwa[bin_i,])
	
	suwa_names[i] = paste0('\n ', suwa_bin_bots[i], '-\n', suwa_bin_tops[i])
}

for(i in 1:length(torn_bin_tops)){
	bin_i = torn$year >= torn_bin_bots[i] & torn$year <= torn_bin_tops[i]
	torn_extreme[i] = sum(torn[bin_i, ]$doy <= torn_cutoff)/nrow(torn[bin_i,])
	
	torn_names[i] = paste0('\n ', torn_bin_bots[i], '-\n', torn_bin_tops[i])
}

torn_extreme = c(rep(NA,length(suwa_extreme)-length(torn_extreme)), torn_extreme)
torn_names = c(rep('',length(suwa_names)-length(torn_names)), torn_names)

png('Figures/figure2.png', height=2500, width=2500, res=300)
par(mfrow=c(2,1), mar=c(2.5,0,0,0), oma=c(0, 5, 1, 0.1), family='sans')

barplot(suwa_extreme, names.arg=suwa_names, col='black', ylim=c(0, 0.35))
text(x=0.5, y=0.32, "A", font=2, cex=1.25)
#axis(1,labels=FALSE, lwd.ticks=0)
barplot(torn_extreme, ylim=c(0,0.35), names.arg=torn_names)
text(x=0.5, y=0.32, "B", font=2, cex=1.25)

par(mfrow=c(1,1))
mtext("Proportion of extreme events      ", side=2, line=3, cex = 1.5)
#axis(1,labels=FALSE, lwd.ticks=0)
dev.off()

