#Libraries
#---------

library(ade4)
library(vegan)
library(packfor)
library(rpart)
library(strucchange)

#****************************
# Input Data
#****************************

Torn.1803 = read.table("Torn1803.txt", header=TRUE, sep="\t")
head(Torn.1803)
summary(Torn.1803)

iceoff_year=Torn.1803[,1]
Temp=Torn.1803[,c(7:9)]
Climate=Torn.1803[,c(4:6)]

#*******************
# Data Analysis
#*******************

#Correlations
#-------------
cor(Torn.1803)

#Plots
plot(Torn.1803)

#Breakpoints

breakpoints(Torn.1803[,2] ~ 1)



#Cross-correlations

#1. Ice off and air temp

airice=ccf(Torn.1803[,3], Torn.1803[,2], type=c("correlation"), plot=TRUE)
airice   

airsun=ccf(Torn.1803[,4], Torn.1803[,2], type=c("correlation"), plot=TRUE)
airsun
       
airNAO_DJFM=ccf(Torn.1803[,5], Torn.1803[,2], type=c("correlation"), plot=TRUE)
airNAO_DJFM  

airCO2=ccf(Torn.1803[,6], Torn.1803[,2], type=c("correlation"), plot=TRUE)
airCO2      

#-----------------
#Forward selection  - not really necessary as there are few variables
#------------------


ice.sel=forward.sel(Torn.1803[,2], Torn.1803[,c(4:8)], nperm=9999)
ice.sel

#--------------
#Linear models
#--------------

ice.lm=lm(iceoff_julian ~  MAMTornio + Mean.sunspots  + CO2  , data=Torn.1803)
summary(ice.lm)
#Record the P, R2 adj

#Type III ANOVA

ice.aov=aov(ice.lm)
ice.aov
ice.aov.III=drop1(ice.aov, ~., test="F")
ice.aov.III

#Other linear models

ice.2=lm(iceoff_julian ~   Mean.sunspots  + CO2 + NAO_DJFM, data=Torn.1803)
summary(ice.2)
#Record the P, R2 adj

ice.aov2=aov(ice.2)
ice.aov2
ice.aov.III2=drop1(ice.aov2, ~., test="F")
ice.aov.III2


ice.2=lm(MAMTornio ~   Mean.sunspots  + CO2 + NAO_DJFM, data=Torn.1803)
summary(ice.2)

#Model with Stockholm air temperatures

ice.3=lm(iceoff_julian ~   Stockholm.air.temps + Mean.sunspots  + CO2, data=Torn.1803)
summary(ice.3)



#----------------------------
# Linear models on residuals
#----------------------------


#1. Detrend the response data
#-----------------------------

ice.lm.Torn1803=lm(iceoff_julian~ iceoff_year, data=Torn.1803)
summary(ice.lm.Torn1803)
ice.resid.Torn1803=residuals(ice.lm.Torn1803)

#Linear models on residuals
ice.lm.res=lm(ice.resid.Torn1803 ~  MAMTornio + Mean.sunspots + CO2, data=Torn.1803)
summary(ice.lm.res)
#Record the P, R2 adj

#Type III ANOVA

ice.aovres=aov(ice.lm.res)
ice.aovres
ice.aov.III.res=drop1(ice.aovres, ~., test="F")
ice.aov.III.res

#----------------
#Regression Tree
#-----------------

tree.Torn1803<-rpart(iceoff_julian ~ MAMTornio + Mean.sunspots + NAO_DJFM + CO2 , minsplit = 10, xval=100, data=Torn.1803)
tree.Torn1803
summary(tree.Torn1803)
plot(tree.Torn1803)
text(tree.Torn1803, use.n=TRUE)
rsq.rpart(tree.Torn1803)
plotcp(tree.Torn1803)

tree.Torn1803<-rpart(iceoff_julian ~ MAMTornio + Mean.sunspots + NAO_DJFM + CO2 , minsplit = 10, maxdepth=3, data=Torn.1803)
tree.Torn1803
summary(tree.Torn1803)
plot(tree.Torn1803)
text(tree.Torn1803, use.n=TRUE)
rsq.rpart(tree.Torn1803)


#----------------------------
# PCNM
#----------------------------


#1. Detrend the response data
#-----------------------------

ice.lm.Torn1803=lm(iceoff_julian~ iceoff_year, data=Torn.1803)
summary(ice.lm.Torn1803)
ice.resid.Torn1803=residuals(ice.lm.Torn1803)

#2. Construct PCNM Eigenfucntions
#--------------------------------

time1.dis=dist(Torn.1803[,1])
file.PCNM=pcnm(time1.dis)
file.PCNM$vectors


#How do the PCNM variables relate to ice-off data?
ice.time.Torn1803=rda(ice.resid.Torn1803, file.PCNM$vectors)
ice.time.Torn1803
summary(ice.time.Torn1803)
ice.time.Torn1803_2=varpart(ice.resid.Torn1803, file.PCNM$vectors, file.PCNM$vectors) #To get adjusted R2 values of all PCNM vectors
ice.time.Torn1803_2


#Forward selection of significant PCNM vectors as related to ice-off date
icePCNM.sel.Torn1803=forward.sel(ice.resid.Torn1803, file.PCNM$vectors)
icePCNM.sel.Torn1803
icePCNM.sel.Torn1803$order #Selected PCNM variables in order of explained adjusted R2
sel.PCNM.Torn1803=sort(icePCNM.sel.Torn1803$order)   #Order of variables by PCNM number
sel.PCNM.Torn1803

ice.time.Torn1803_3=varpart(ice.resid.Torn1803, file.PCNM$vectors[,sel.PCNM.Torn1803], file.PCNM$vectors[,sel.PCNM.Torn1803]) #To get adjusted R2 values of all PCNM vectors
ice.time.Torn1803_3



ice.variation.Torn1803=varpart(ice.resid.Torn1803, file.PCNM$vectors[,sel.PCNM.Torn1803], Temp, Climate)
ice.variation.Torn1803
plot(ice.variation.Torn1803)


#Varpart of only significant variables

ice.variation.Torn1803_2=varpart(ice.resid.Torn1803, file.PCNM$vectors[,sel.PCNM.Torn1803], Torn.1803[,c(8)], Torn.1803[,c(4:6)])
ice.variation.Torn1803_2
plot(ice.variation.Torn1803_2)





