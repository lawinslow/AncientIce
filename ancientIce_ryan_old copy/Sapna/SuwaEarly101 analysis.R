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

Suwa.Early = read.table("SuwaEarly101.txt", header=TRUE, sep="\t")
head(Suwa.Early)
summary(Suwa.Early)

iceoff_year=Suwa.Early[,1]
Temp=Suwa.Early[,6]
Climate=Suwa.Early[,c(4:5)]

#*******************
# Data Analysis
#*******************

#Correlations
#-------------
cor(Suwa.Early)

#Plots
plot(Suwa.Early)

#Breakpoints

breakpoints(Suwa.Early[,3] ~ 1)



#Cross-correlations

#1. Ice off and air temp

airice=ccf(Suwa.Early[,6], Suwa.Early[,3], type=c("correlation"), plot=TRUE)
airice   
       
airENSO=ccf(Suwa.Early[,4], Suwa.Early[,3], type=c("correlation"), plot=TRUE)
airENSO 

airCO2=ccf(Suwa.Early[,5], Suwa.Early[,3], type=c("correlation"), plot=TRUE)
airCO2      

#-----------------
#Forward selection  - not really necessary as there are few variables
#------------------


ice.sel=forward.sel(Suwa.Early[,3], Suwa.Early[,c(4:6)], nperm=9999)
ice.sel

#--------------
#Linear models
#--------------

ice.lm=lm(DOY ~  ENSO + CO2 + Airtemp  , data=Suwa.Early)
summary(ice.lm)
#Record the P, R2 adj

#Type III ANOVA

ice.aov=aov(ice.lm)
ice.aov
ice.aov.III=drop1(ice.aov, ~., test="F")
ice.aov.III


#----------------------------
# PCNM
#----------------------------


#1. Detrend the response data
#-----------------------------

ice.lm.SuwaEarly=lm(DOY~ Year, data=Suwa.Early)
summary(ice.lm.SuwaEarly)
ice.resid.SuwaEarly=residuals(ice.lm.SuwaEarly)

#2. Construct PCNM Eigenfucntions
#--------------------------------

time1.dis=dist(Suwa.Early[,1])
file.PCNM=pcnm(time1.dis)
file.PCNM$vectors


#How do the PCNM variables relate to ice-off data?
ice.time.SuwaEarly=rda(ice.resid.SuwaEarly, file.PCNM$vectors)
ice.time.SuwaEarly
summary(ice.time.SuwaEarly)
ice.time.SuwaEarly_2=varpart(ice.resid.SuwaEarly, file.PCNM$vectors, file.PCNM$vectors) #To get adjusted R2 values of all PCNM vectors
ice.time.SuwaEarly_2


#Forward selection of significant PCNM vectors as related to ice-off date
icePCNM.sel.SuwaEarly=forward.sel(ice.resid.SuwaEarly, file.PCNM$vectors)
icePCNM.sel.SuwaEarly
icePCNM.sel.SuwaEarly$order #Selected PCNM variables in order of explained adjusted R2
sel.PCNM.SuwaEarly=sort(icePCNM.sel.SuwaEarly$order)   #Order of variables by PCNM number
sel.PCNM.SuwaEarly

ice.time.SuwaEarly_3=varpart(ice.resid.SuwaEarly, file.PCNM$vectors[,sel.PCNM.SuwaEarly], file.PCNM$vectors[,sel.PCNM.SuwaEarly]) #To get adjusted R2 values of all PCNM vectors
ice.time.SuwaEarly_3



ice.variation.SuwaEarly=varpart(ice.resid.SuwaEarly, file.PCNM$vectors[,sel.PCNM.SuwaEarly], Temp, Climate)
ice.variation.SuwaEarly
plot(ice.variation.SuwaEarly)


#----------------
#Regression Tree
#-----------------

tree.SuwaEarly<-rpart(DOY ~ ENSO + CO2  +Airtemp , minsplit = 10, xval=100, data=Suwa.Early)
tree.SuwaEarly
summary(tree.SuwaEarly)
plot(tree.SuwaEarly)
text(tree.SuwaEarly, use.n=TRUE)
rsq.rpart(tree.SuwaEarly)
plotcp(tree.SuwaEarly)

tree.SuwaEarly<-rpart(DOY ~ ENSO + CO2 +Airtemp , minsplit = 10, maxdepth=2, data=Suwa.Early)
tree.SuwaEarly
summary(tree.SuwaEarly)
plot(tree.SuwaEarly)
text(tree.SuwaEarly, use.n=TRUE)
rsq.rpart(tree.SuwaEarly)





