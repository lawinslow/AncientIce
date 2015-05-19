library(dplyr)

nao_rec = read.table('Data/Other/nao_reconstruction_luterbacher.tsv', sep='\t', header=TRUE, skip=25)


nao_djfm = filter(nao_rec, Mon %in% c('Dec', 'Jan', 'Feb', 'Mar')) %>% 
							group_by(Year) %>% summarise(nao_djfm=mean(NAO))
							
other_nao = read.table('Data/tornio.tsv', header=TRUE, sep='\t')


other_nao2 <- other_nao[,c("nao.djfm","year", "doy")]
names(other_nao2) <- c("torne_nao.djfm", "Year", "doy")
nao <- merge(nao_djfm, other_nao2, all=TRUE)


postBP.nao <- nao[nao[,"Year"]>1867 & !is.na(nao[,"nao_djfm"]),]

(mod.new <- summary(lm(doy~nao_djfm, data=postBP.nao))) # Luterbacher (NAO source used for the first half of the time series)

(mod.orig <- summary(lm(doy~torne_nao.djfm, data=postBP.nao))) # Hurrell




Z.luter.hur.after <- (mod.orig$coef[2,"Estimate"]-mod.new$coef[2,"Estimate"])/sqrt(mod.orig$coef[2,"Std. Error"]+mod.new$coef[2,"Std. Error"])

pnorm(Z.luter.hur.after, lower.tail=FALSE)*2


# =======================================================================================================
# = Do Regressions to see if the before-after conclusion would change if we'd used Luterbacher for both =
# =======================================================================================================
luter.after <- mod.new

preBP.nao <- nao[nao[,"Year"]<1867 & !is.na(nao[,"torne_nao.djfm"]) & !is.na(nao[,"doy"]),]

(luter.before <- summary(lm(doy~torne_nao.djfm, data=preBP.nao)))


Z.luter.before.after <- (luter.before$coef[2,"Estimate"]-luter.after$coef[2,"Estimate"])/sqrt(luter.before$coef[2,"Std. Error"]+luter.after$coef[2,"Std. Error"])

pnorm(Z.luter.before.after, lower.tail=FALSE)*2


# ===============================================
# = Now Use the Full Boostrapping Method to see =
# ===============================================




