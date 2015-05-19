## plot reconstructed NAO DJFM from Luterbacher et al 
# retrieved from http://www.cru.uea.ac.uk/cru/data/paleo/naojurg/ on 2015-05-19

library(dplyr)

nao_rec = read.table('Data/Other/nao_reconstruction_luterbacher.tsv', sep='\t', header=TRUE, skip=25)


nao_djfm = filter(nao_rec, Mon %in% c('Dec', 'Jan', 'Feb', 'Mar')) %>% 
							group_by(Year) %>% summarise(nao_djfm=mean(NAO))

png('Figures/NAO_djfm_compare.png', width=2500, height=1800, res=300)

plot(nao_djfm, type='l', ylim=c(-4,4))


other_nao = read.table('Data/tornio.tsv', header=TRUE, sep='\t')

lines(other_nao$year, other_nao$nao.djfm, col=rgb(1,0,0,0.7))
dev.off()
