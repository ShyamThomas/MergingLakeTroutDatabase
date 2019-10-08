ibrary(dplyr)
library(corrplot)
library(PerformanceAnalytics)

setwd("C:/Users/thoma/Rprojects/Databases/IsotopeData.csv")

HGdata=read.csv("HGdataMergedBSM_LUT.csv")
head(HGdata)
HGdata.lt=subset(HGdata, HGdata$SPECIES_NAME=="Lake Trout")

HGdata.lt0812=subset(HGdata.lt, HGdata.lt$SAMPLE_YEAR>=2008 & HGdata.lt$SAMPLE_YEAR<=2012)
hist(HGdata.lt0812$SAMPLE_YEAR)

LTisodata=read.csv("LTdata_Tyler.csv")
tail(LTisodata)

LTHGisodata.rightjoin=right_join(HGdata.lt0812,LTisodata, by ="Waterbody_LID")

write.csv(LTHGisodata.rightjoin, "LTHGisodata.rj.csv")

LTHGisodata.rightjoin.narmd=na.omit(LTHGisodata.rightjoin)

########################################################################################################################################

AGG_HG_AGE_LT2LT_TROPHIC_WATERCHEMLAKECHARS=HG_AGE_LT2LT_TROPHIC_WATERCHEMLAKECHARS %>%
group_by(WATERBODY_LID) %>%
summarise(
LAT=max(LATITUDE),
LONG= max(LONGITUDE),
avgHG=mean(VALUE),
samples=length(VALUE),

avgLength=mean(Total_Length ),
avgWeight=mean(WEIGHT),
avgAge=mean( Assessed_Fish_Age),
avgTP = mean(TP),
LakeOrder=mean(Strahler),
LakeArea=mean(AreaCalcm2),
avgDIC=mean(DIC),             ## Disssolved Inorganic Carbon
avgDOC=mean(DOC),             ## Disssolved Organic Carbon
avgSO4=mean(SSO4UR),          ## Is this sulphate levels?
avgDEPTH=mean(Secchi_Depth),
avgPH=mean(pH)
)


AGG_HG_AGE_LT2LT_TROPHIC_WATERCHEMLAKECHARS.DF=as.data.frame(AGG_HG_AGE_LT2LT_TROPHIC_WATERCHEMLAKECHARS)
AGG_HG_AGE_LT2LT_TROPHIC_WATERCHEMLAKECHARS.DF
LTmerged.correlation=chart.Correlation(AGG_HG_AGE_LT2LT_TROPHIC_WATERCHEMLAKECHARS.DF[,c(-1)], histogram=TRUE, pch=19)

################################################################################################################################


