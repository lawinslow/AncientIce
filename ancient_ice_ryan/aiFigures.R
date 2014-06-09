Torn.1803 = read.table("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/ancientIce_ryan_old/Sapna/Torn1803.txt", header=TRUE, sep="\t")

Suwa.Early = read.table("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/ancientIce_ryan_old/Sapna/SuwaEarly101.txt", header=TRUE, sep="\t")


blah1 <- residuals(lm(iceoff_julian~DJFTornio, data=Torn.1803))
blah11 <- residuals(lm(NAO_DJFM~DJFTornio, data=Torn.1803))
plot(blah1, Torn.1803[,"NAO_DJFM"])
plot(blah1, blah11)

blah2 <- residuals(lm(NAO_DJFM~DJFTornio, data=Torn.1803))
plot(blah2, Torn.1803[,"iceoff_julian"])

summary(lm(iceoff_julian~DJFTornio+NAO_DJFM, data=Torn.1803))