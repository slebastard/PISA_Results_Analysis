## LOADING LIBRARIES ##
library(MASS)
library(sqldf)
library(entropy)
setwd("D:/Ponts/2A/S4/Statistiques/Projet/Data")

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

#								  #
### LOADING ALL FILES AND CREATING THE DATASETS ###
#								  #

## LOADING DIPLOMAS RATES FILES ##
DiploFile <- "ThirdGrade_Edu_Rate.csv"
Diplo <- read.csv(DiploFile)
colnames(Diplo) <- c("CTR", "INDICATOR", "SUBJECT", "MEASURE", "FREQUENCY", "TIME", "Value", "Flag Codes")
Diplo2012 <- sqldf("select CTR, Value from Diplo where TIME=='2012'")

## LOADING CSV FILE ABOUT S/T RATIO ##

STFile <- "Stud_Teach_Ratio.csv"
ST <- read.csv(STFile)
colnames(ST) <- c("CTR", "Country", "LVL", "Level of education", "SCT", "Reference sector", "IND", "Indicator", "YEA", "Year", "UNT", "Unit", "PWC", "PowerCode", "PER", "Reference Period", "Value", "FLG", "Flags")

STRatio <- sqldf("select CTR, Value from ST where IND=='PERS_RATIO_INST' and SCT=='INST_T' and LVL=='L2_3'")

## LOADING TEACHERS SALARIES FILES ##
SlrFile <- "Teachers_Salaries.csv"
Slr <- read.csv(SlrFile)
colnames(Slr) <- c("CTR", "INDICATOR", "SUBJECT", "MEASURE", "FREQUENCY", "TIME", "Value", "Flag Codes")

Slr2012 <- sqldf("select CTR, Value from Slr where MEASURE=='USD' and TIME=='2012'")

#					#
###### COMPLETE DATATABLE #####
#					#

tmp1 <- merge(Diplo2012,PISAMFilt,by = "CTR")
tmp1 <- na.omit(tmp1)								# 36 entries
tmp2 <- merge(STRatio,tmp1,by = "CTR")
tmp2 <- na.omit(tmp2)								# 30 entries
colnames(tmp2) <- c("CTR", "tmp 2", "tmp 1", "PISA Maths Value")
tmp3 <- merge(Slr2012,tmp2,by = "CTR")	
2012PISAM <- na.omit(tmp3)							# 26 entries
colnames(2012PISAM) <- c("CTR", "Teachers salaries, USD", "Student to teaching staff ratio", "Rate of tertiary diplomas","PISA Maths Value")

tmp1 <- merge(Diplo2012,PISASFilt,by = "CTR")
tmp1 <- na.omit(tmp1)								# 36 entries
tmp2 <- merge(STRatio,tmp1,by = "CTR")
tmp2 <- na.omit(tmp2)								# 30 entries
colnames(tmp2) <- c("CTR", "tmp 2", "tmp 1", "PISA Sciences Value")
tmp3 <- merge(Slr2012,tmp2,by = "CTR")	
2012PISAS <- na.omit(tmp3)							# 26 entries
colnames(2012PISAM) <- c("CTR", "Teachers salaries, USD", "Student to teaching staff ratio", "Rate of tertiary diplomas","PISA Sciences Value")
