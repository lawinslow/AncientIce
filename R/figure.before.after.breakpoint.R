## Calc variance before and after breakpoint
library(data.table)
library(plyr)
library(zoo)
library(pracma)

##
suwa = fread('../Data/suwa.tsv')
torn = fread('../Data/tornio.tsv')

suwa[,period:=0]
suwa[year <= 1615 & year >=1515 ,period:=1]
suwa[year <= 1997 & year >=1897 ,period:=2]

early.late = suwa[period!=0, list(doy, period)]
early.late = early.late[complete.cases(early.late),]

levene.test(early.late$doy, early.late$period)

##
torn.diff = data.table(year=torn$year[-1], diff=abs(diff(torn$doy)))

torn.roll = rollmean(torn.diff$diff,30, align='center')
torn.roll.year = rollmean(torn.diff$year,30, align='center')

tiff('torn.tiff')
plot(torn.roll.year, torn.roll, type='l')
abline(lm(torn.roll~torn.roll.year), col='red')
dev.off()

suwa.diff = data.table(year=suwa$year[-1], diff=abs(diff(suwa$doy)))

suwa.roll = rollapply(suwa.diff$diff,30, align='center', FUN=mean, na.rm=TRUE)
suwa.roll.year = rollmean(suwa.diff$year,30, align='center')
tiff('suwa.tiff')
plot(suwa.roll.year, suwa.roll, type='l')
abline(lm(suwa.roll~suwa.roll.year), col='red')
dev.off()




##
library(pracma)
nObs = 30
vec = torn$doy
datLabels <- ceiling(seq_along(vec)/nObs)#[rank(vec, ties.method = "first")]
torn[,bin.lab:=datLabels]

sd.ts =ddply(torn,'bin.lab', function(df)
  data.frame(year=median(df$year), std=sd(detrend(df$doy))))

plot(std~year, sd.ts)

nObs = 30
vec = suwa[,list(year,doy)]
#vec = vec[complete.cases(vec),]
datLabels <- ceiling(seq_along(vec$doy)/nObs)#[rank(vec, ties.method = "first")]
vec[,bin.lab:=datLabels]

sd.ts =ddply(vec,'bin.lab', function(df)
  data.frame(year=median(df$year), std=sd(detrend(df$doy))))

suwa[,decade3:=floor(year/30)*30]
sd.ts =ddply(suwa,'decade3', function(df){
  if(sum(is.na(df$doy))< 10){  
    data.frame(year=median(df$year), std=sd(detrend(df$doy),na.rm=TRUE))
  }else{
    data.frame(year=median(df$year), std=NA)
  }
  })


par(mfrow=c(2,1), mar=c(0.4,0,0,0), oma=c(2,2,1,1))
plot(std~year, sd.ts, xlim=c(1400, 2000), xaxt = 'n', 
     ylim=c(5,18))
plot(doy~year, vec, xlim=c(1400, 2000), type='l')


tiff('test.tiff')
par(mfrow=c(2,1))
boxplot(doy~period, suwa[period!=0])

stdevs = c(sd(suwa[period==1]$doy, na.rm=TRUE),
           sd(suwa[period==2]$doy, na.rm=TRUE))
barplot(stdevs)
dev.off()

####
#Compare average no.ice years with mean ice-on date
####

suwa = fread('../Data/suwa.tsv')
torn = fread('../Data/tornio.tsv')

plot(doy~year,suwa, type='l', ylim=c(-50,65))

suwa[,doy.no30:=doy]
suwa[year < 1890, doy.no30:=doy+30]
lines(doy.no30~year,suwa, type='l', col='red')

suwa[,decade2:=floor(year/20)]
mean.doy = ddply(suwa, 'decade2', function(df) mean(df$doy, na.rm=TRUE))
sum.no.ice = ddply(suwa, 'decade2', function(df) sum(df$no.ice, na.rm=TRUE))
plot(mean.doy)
par(new=TRUE)
plot(sum.no.ice, type='l', axes=FALSE, col='red')

