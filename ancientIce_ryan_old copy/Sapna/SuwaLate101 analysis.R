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

Suwa.Late = read.table("SuwaLate101.txt", header=TRUE, sep="\t")
head(Suwa.Late)
summary(Suwa.Late)

iceoff_year=Suwa.Late[,1]
Temp=Suwa.Late[,6]
Climate=Suwa.Late[,c(3:5)]

#*******************
# Data Analysis
#*******************

#Correlations
#-------------
cor(Suwa.Late)

#Plots
plot(Suwa.Late)

#Breakpoints

breakpoints(Suwa.Late[,2] ~ 1)



#Cross-correlations

#1. Ice off and air temp

airice=ccf(Suwa.Late[,6], Suwa.Late[,2], type=c("correlation"), plot=TRUE)
airice   
       
airENSO=ccf(Suwa.Late[,3], Suwa.Late[,2], type=c("correlation"), plot=TRUE)
airENSO 

airCO2=ccf(Suwa.Late[,4], Suwa.Late[,2], type=c("correlation"), plot=TRUE)
airCO2  

airsun=ccf(Suwa.Late[,5], Suwa.Late[,2], type=c("correlation"), plot=TRUE)
airsun     

#-----------------
#Forward selection  - not really necessary as there are few variables
#------------------


ice.sel=forward.sel(Suwa.Late[,2], Suwa.Late[,c(3:6)], nperm=9999)
ice.sel

#--------------
#Linear models
#--------------

ice.lm=lm(DOY ~  ENSO + CO2 + Sunspots + Airtemp  , data=Suwa.Late)
summary(ice.lm)
#Record the P, R2 adj

ice.lm=lm(DOY ~   CO2   , data=Suwa.Late)
summary(ice.lm)

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

ice.lm.SuwaLate=lm(DOY~ Year, data=Suwa.Late)
summary(ice.lm.SuwaLate)
ice.resid.SuwaLate=residuals(ice.lm.SuwaLate)

#2. Construct PCNM Eigenfucntions
#--------------------------------

time1.dis=dist(Suwa.Late[,1])
file.PCNM=pcnm(time1.dis)
file.PCNM$vectors


#How do the PCNM variables relate to ice-off data?
ice.time.SuwaLate=rda(ice.resid.SuwaLate, file.PCNM$vectors)
ice.time.SuwaLate
summary(ice.time.SuwaLate)
ice.time.SuwaLate_2=varpart(ice.resid.SuwaLate, file.PCNM$vectors, file.PCNM$vectors) #To get adjusted R2 values of all PCNM vectors
ice.time.SuwaLate_2


#Forward selection of significant PCNM vectors as related to ice-off date
icePCNM.sel.SuwaLate=forward.sel(ice.resid.SuwaLate, file.PCNM$vectors)
icePCNM.sel.SuwaLate
icePCNM.sel.SuwaLate$order #Selected PCNM variables in order of explained adjusted R2
sel.PCNM.SuwaLate=sort(icePCNM.sel.SuwaLate$order)   #Order of variables by PCNM number
sel.PCNM.SuwaLate

ice.time.SuwaLate_3=varpart(ice.resid.SuwaLate, file.PCNM$vectors[,sel.PCNM.SuwaLate], file.PCNM$vectors[,sel.PCNM.SuwaLate]) #To get adjusted R2 values of all PCNM vectors
ice.time.SuwaLate_3



ice.variation.SuwaLate=varpart(ice.resid.SuwaLate, file.PCNM$vectors[,sel.PCNM.SuwaLate], Temp, Climate)
ice.variation.SuwaLate
plot(ice.variation.SuwaLate)


#----------------
#Regression Tree
#-----------------

tree.SuwaLate<-rpart(DOY ~ ENSO + CO2 + Sunspots +Airtemp , minsplit = 10, xval=100, data=Suwa.Late)
tree.SuwaLate
summary(tree.SuwaLate)
plot(tree.SuwaLate)
text(tree.SuwaLate, use.n=TRUE)
rsq.rpart(tree.SuwaLate)
plotcp(tree.SuwaLate)

tree.SuwaLate<-rpart(DOY ~ ENSO + CO2 + Sunspots +Airtemp , minsplit = 10, maxdepth=3, data=Suwa.Late)
tree.SuwaLate
summary(tree.SuwaLate)
plot(tree.SuwaLate)
text(tree.SuwaLate, use.n=TRUE)
rsq.rpart(tree.SuwaLate)




