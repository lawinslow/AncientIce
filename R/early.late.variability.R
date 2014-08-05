## Compare early and late periods

library(data.table)
library(car)
library(plyr)
library(zoo)
library(pracma)

##
torn = fread('../Data/tornio.tsv')

#early = suwa[year <= 1615 & year >=1515, list(year, doy)]
#late = suwa[year <= 1997 & year >=1897, list(year, doy)]

early = torn[year <= 1793 & year >=1693, list(year, doy)]
late = torn[year <= 2013 & year >=1913, list(year, doy)]

#early = torn[year <= 1885 & year >=1693, list(year, doy)]
#late = torn[year <= 2013 & year >=1885, list(year, doy)]


cat('Test | Early | Late\n')

cat('AR1 |', ar(early$doy, order.max=1, aic=FALSE)$ar,'|',
    ar(late$doy, order.max=1, aic=FALSE)$ar, '\n')

cat('SD |',sd(detrend(early$doy)), '|', 
    sd(detrend(late$doy)), '\n')

cat('mean(diff) |',mean(abs(diff(early$doy))), '|',
  mean(abs(diff(late$doy))),'\n')

tmp = levene.test(c(detrend(early$doy), detrend(late$doy)), 
                  c(rep(1,nrow(early)), rep(2,nrow(late))))

cat('Levene test p-val:', tmp$p.value, '\n')

suwa = fread('../Data/suwa.tsv')
suwa = suwa[!is.na(doy), ]
#suwa = suwa[is.na(doy), doy:=41]

early = suwa[year <= 1615 & year >=1515, list(year, doy)]
late = suwa[year <= 1997 & year >=1897, list(year, doy)]


cat('Test | Early | Late\n')

cat("AR1 |", ar(early$doy, order.max=1, aic=FALSE)$ar,'|',
    ar(late$doy, order.max=1, aic=FALSE)$ar, '\n')

cat("SD |", sd(detrend(early$doy)), '|', 
    sd(detrend(late$doy)), '\n')

cat("mean(diff) |", mean(abs(diff(early$doy))), '|',
    mean(abs(diff(late$doy))),'\n')

tmp = levene.test(c(detrend(early$doy), detrend(late$doy)), 
            c(rep(1,nrow(early)), rep(2,nrow(late))))

cat('Levene test p-val:', tmp$p.value)







