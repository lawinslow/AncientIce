## plot reconstructed NAO DJFM from Luterbacher et al 
# retrieved from http://www.cru.uea.ac.uk/cru/data/paleo/naojurg/ on 2015-05-19

library(dplyr)

nao_rec = read.table('Data/Other/nao_reconstruction_luterbacher.tsv', sep='\t', header=TRUE, skip=25)


nao_djfm = filter(nao_rec, Mon %in% c('Dec', 'Jan', 'Feb', 'Mar')) %>% 
							group_by(Year) %>% summarise(nao_djfm=mean(NAO))

png('Figures/NAO_djfm_compare.png', width=2500, height=1800, res=300)

plot(nao_djfm, pch=21, ylim=c(-4,4))
points(filter(nao_djfm, Year > 1850), pch=21, bg=rgb(0,0,0,0.4))


other_nao = read.table('Data/tornio.tsv', header=TRUE, sep='\t')

points(other_nao$year, other_nao$nao.djfm, col=rgb(1,0,0,0.7))

tmp = filter(other_nao, year > 1850)
points(tmp$year, tmp$nao.djfm, pch=21, col=rgb(1,0,0,0.7), bg=rgb(0,0,0,0.4))
dev.off()



# ======================================================
# = Scatter Plot of NAO Indices from Different Sources =
# ======================================================

# Combine data source
other_nao2 <- other_nao[,c("nao.djfm","year")]
names(other_nao2) <- c("torne_nao.djfm", "Year")
nao <- merge(nao_djfm, other_nao2, all=TRUE)


# Make plot
png('Figures/NAO_djfm_compare_scatter.png', width=3.5, height=3.5, res=150, units="in")
par(mar=c(2.5,2.5,0.1,0.1), mgp=c(1.5,0.25,0), tcl=-0.25, ps=10)

plot(nao[,"nao_djfm"], nao[,"torne_nao.djfm"], col=c("black","red")[1+(nao[,"Year"]>=1850)], xlab="Luterbacher DJFM NAO", ylab="Torne data set DJFM NAO", cex=0.85)

legend("topleft", legend=c("Up to 1850", "After 1850"), pch=21, col=c("black","red"))

abline(lm(torne_nao.djfm~nao_djfm, data=nao[nao[,"Year"]<1850,]), col="black")
abline(lm(torne_nao.djfm~nao_djfm, data=nao[nao[,"Year"]>=1850,]), col="red")




dev.off()
