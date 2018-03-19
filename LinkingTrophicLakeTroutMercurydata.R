ibrary(dplyr)
library(corrplot)
library(PerformanceAnalytics)

setwd("C:/Users/thoma/Rprojects/Databases")


WATERCHEM2BSMLAKECHAR=merge(x=WATER_CHEM, y = BSM_LAKE_CHARS, by = "WATERBODY_LID")
head(WATERCHEM2BSMLAKECHAR)
length(WATERCHEM2BSMLAKECHAR[,1])

trout_tl=read.csv("LTdata_Tyler.csv") # Tyler's Lake Trout data with Trophic position


LT_TROPHIC2WATERCHEMLAKECHARS=merge(WATERCHEM2BSMLAKECHAR, trout_tl, by="WATERBODY_LID", all.y = TRUE)
head(LT_TROPHIC2WATERCHEMLAKECHARS)


HG_AGE_LT=read.csv("HG_AGE_LT_Linked.csv") #Mercury data with Lake Trout selected
head(HG_AGE_LT)


HG_AGE_LT2LT_TROPHIC_WATERCHEMLAKECHARS=merge(x=LT_TROPHIC2WATERCHEMLAKECHARS, y = HG_AGE_LT, by = "WATERBODY_LID")
head(HG_AGE_LT2LT_TROPHIC_WATERCHEMLAKECHARS)
length(HG_AGE_LT2LT_TROPHIC_WATERCHEMLAKECHARS[,1])


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

library(ggplot2)

###PH & HG
ggplot(data = AGG_HG_AGE_LT2LT_TROPHIC_WATERCHEMLAKECHARS.DF, aes(x = avgPH, y = avgHG)) +
geom_point(color='blue') +
geom_smooth(method = "lm")

###DOC & HG

