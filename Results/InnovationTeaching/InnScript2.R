## LOADING LIBRARIES ##
library(MASS)
library(sqldf)
setwd("D:/Ponts/2A/S4/Statistiques/Projet/Data")

## LOADING INNOVATION DB ##
InnFile <- "InnovationEdu.csv"
Inn <- read.csv(InnFile)
colnames(Inn) <- c("CTR", "Country", "IND", "Indicator", "PER", "Period", "GRD", "Grade", "FLD", "Field", "MSR", "Measure", "Value", "Flag.Codes", "Flags")

## CREATING ALL THE USEFUL SUBFRAMES FOR INNOVATION IN TEACHING METHODS ##
InnIndeMath4 <- sqldf("select CTR, Value from Inn where IND=='5CIWT' and GRD=='FOG' and PER=='INI' and FLD=='MAT' and MSR=='MN'")

InnIndeWorkMath8 <- sqldf("select CTR, Value from Inn where IND=='5CIWT' and GRD=='EIG' and PER=='INI' and FLD=='MAT' and MSR=='MN'")

InnExplainMath8 <- sqldf("select CTR, Value from Inn where IND=='6EAAT' and GRD=='EIG' and PER=='INI' and FLD=='MAT' and MSR=='MN'")

InnGroupWorkMath4 <- sqldf("select CTR, Value from Inn where IND=='7SWSG' and GRD=='FOG' and PER=='INI' and FLD=='MAT' and MSR=='MN'")

InnGroupWorkMath8 <- sqldf("select CTR, Value from Inn where IND=='7SWSG' and GRD=='EIG' and PER=='INI' and FLD=='MAT' and MSR=='MN'")

InnIndeSolvingMath8 <- sqldf("select CTR, Value from Inn where IND=='7DOPS' and GRD=='EIG' and PER=='INI' and FLD=='MAT' and MSR=='MN'")

InnGroupWorkScience4<- sqldf("select CTR, Value from Inn where IND=='7SWSG' and GRD=='FOG' and PER=='INI' and FLD=='SCI' and MSR=='MN'")

InnGroupWorkScience8<- sqldf("select CTR, Value from Inn where IND=='7SWSG' and GRD=='EIG' and PER=='INI' and FLD=='SCI' and MSR=='MN'")

InnAbilityGroups <- sqldf("select CTR, Value from Inn where IND=='7FGBA' and PER=='INI' and MSR=='MN'")

## LOADING AND FILTERING PISA MATHS DB ##
PISAMFile <- "PISA_Maths.csv"
PISAM <- read.csv(PISAMFile)
colnames(PISAM) <- c("CTR", "INDICATOR", "SUBJECT", "MEASURE", "FREQUENCY", "TIME", "Value", "Flag.Codes")
PISAMFilt <- sqldf("select CTR, Value from PISAM where SUBJECT=='GIRL' and TIME=='2012'")
head(PISAMFilt)

## MERGING THE FRAMES ##
MergedDB <- merge(InnIndeMath,PISAMFilt,by = "CTR")
colnames(MergedDB) <- c("CTR", "Indep Work %", "PISA Maths Score")