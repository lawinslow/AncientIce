## Compare early and late periods

library(data.table)
library(car)
library(plyr)
library(zoo)
library(pracma)

##
torn = fread('../Data/tornio.tsv')
suwa = fread('../Data/suwa.tsv')

tiff('../Figures/variability.calc.check.tiff', res=300, width=2400, height=1800, compression = 'lzw')

par(family="serif")

plot(range(c(torn$year, suwa$year)), c(NA,NA), ylim=c(0,22), ylab="Variability (years)", xlab="Year")

addlinesvar = function(data, windowlen, col){
	
	data$indx = floor(1:nrow(data)/windowlen)
	
	out = ddply(data, 'indx', function(df){
		
						dev = sd(df$doy,na.rm=TRUE)
						year = mean(df$year,na.rm=TRUE)
						return(data.frame(year, dev))
					})
	
	points(out$year, out$dev, col=col, pch=16)
	
}


for(j in 10:40){
	windowlength = j
	
	for(i in 1:windowlength){
		addlinesvar(torn[i:nrow(torn),], windowlength, rgb(0, 0, 1, 0.2))
	}
	
	for(i in 1:windowlength){
		addlinesvar(suwa[i:nrow(suwa),], windowlength, rgb(1, 0, 0, 0.2))
	}
}

legend('topright', pch=16, col=c(rgb(0,0,1,0.6), rgb(1,0,0,0.6)), legend=c('Tornio', 'Suwa'))


dev.off()
