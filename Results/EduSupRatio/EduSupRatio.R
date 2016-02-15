## LOADING LIBRARIES ##
library(MASS)
library(sqldf)
library(entropy)
setwd("D:/PISA_Results_Analysis/Data")

## LOADING AND FILTERING PISA MATHS DB ##
PISAMFile <- "PISA_Maths.csv"
PISAM <- read.csv(PISAMFile)
colnames(PISAM) <- c("CTR", "INDICATOR", "SUBJECT", "MEASURE", "FREQUENCY", "TIME", "Value", "Flag.Codes")
PISAMFilt <- sqldf("select CTR, Value from PISAM where SUBJECT=='GIRL' and TIME=='2012'")
head(PISAMFilt)

## LOADING AND FILTERING PISA SCIENCES DB ##
PISASFile <- "PISA_Sciences.csv"
PISAS <- read.csv(PISASFile)
colnames(PISAS) <- c("CTR", "INDICATOR", "SUBJECT", "MEASURE", "FREQUENCY", "TIME", "Value", "Flag.Codes")
PISASFilt <- sqldf("select CTR, Value from PISAS where SUBJECT=='GIRL' and TIME=='2012'")
head(PISASFilt)

## LOADING CSV FILE ABOUT EDU SUP RATIO ##

STFile <- "ThirdGrade_Edu_Rate.csv"
ST <- read.csv(STFile)
colnames(ST) <- c("CTR", "INDICATOR", "SUBJECT", "MEASURE", "FREQUENCY", "TIME", "Value", "Flag Codes")

# EduSup #

EduSupRatio <- sqldf("select CTR, Value from ST where TIME=='2012'")

STM <- merge(EduSupRatio,PISAMFilt,by = "CTR")			# 37 entries
STM <- na.omit(STM)							# 32 entries
colnames(STM) <- c("LOCATION", "Edu Sup Ratio", "PISA Maths Score")

STS <- merge(EduSupRatio,PISASFilt,by = "CTR")			# 37 entries
STS <- na.omit(STS)							# 32 entries
colnames(STS) <- c("CTR", "Edu Sup Ratio", "PISA Sciences Score")

# Discrete indicators and Fisher tests #

Discrete_ST_M8 <- discretize2d(STM$"Edu Sup Ratio", STM$"PISA Maths Score", numBins1=4, numBins2=4)
Discrete_ST_M12 <- discretize2d(STM$"Edu Sup Ratio", STM$"PISA Maths Score", numBins1=6, numBins2=6)
Discrete_ST_S8 <- discretize2d(STS$"Edu Sup Ratio", STS$"PISA Sciences Score", numBins1=4, numBins2=4)
Discrete_ST_S12 <- discretize2d(STS$"Edu Sup Ratio", STS$"PISA Sciences Score", numBins1=6, numBins2=6)

STPISAM_miM8 <- mi.plugin(Discrete_ST_M8)
STPISAM_entrM8 <- entropy(Discrete_ST_M8)
STDependencyM8 <- (STPISAM_miM8)/(STPISAM_entrM8)

STPISAM_miM12 <- mi.plugin(Discrete_ST_M12)
STPISAM_entrM12 <- entropy(Discrete_ST_M12)
STDependencyM12 <- (STPISAM_miM12)/(STPISAM_entrM12)

STPISAM_miS8 <- mi.plugin(Discrete_ST_S8)
STPISAM_entrS8 <- entropy(Discrete_ST_S8)
STDependencyS8 <- (STPISAM_miS8)/(STPISAM_entrS8)

STPISAM_miS12 <- mi.plugin(Discrete_ST_S12)
STPISAM_entrS12 <- entropy(Discrete_ST_S12)
STDependencyS12 <- (STPISAM_miS12)/(STPISAM_entrS12)

STFisherM8 <- fisher.test(Discrete_ST_M8)
STFisherS8 <- fisher.test(Discrete_ST_S8)

# Plot of PISAMaths = f(Edu Sup Ratio) #
plot(STM$"Edu Sup Ratio", STM$"PISA Maths Score")

# Plot of PISAScience = f(S/T) #
plot(STS$"Edu Sup Ratio", STS$"PISA Sciences Score")

