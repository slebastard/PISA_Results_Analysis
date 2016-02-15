## LOADING LIBRARIES ##
library(MASS)
library(sqldf)
library(entropy)
library(devtools)
install_github("ggbiplot", "vqv") 
library(ggbiplot)

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

## LOADING TEACHING HOURS FILES ##

HoursFile <- "Teaching_Hours.csv"
HoursUnfilt <- read.csv(HoursFile)
colnames(HoursUnfilt) <- c("CTR", "INDICATOR", "SUBJECT", "MEASURE", "FREQUENCY", "TIME", "Value", "Flag Codes")

HoursLow <- sqldf("select CTR, Value from HoursUnfilt where SUBJECT=='LOWSRY' and TIME=='2012'")
HoursUp <- sqldf("select CTR, Value from HoursUnfilt where SUBJECT=='UPPSRY' and TIME=='2012'")

## SPENDINGS ON EDUCATION ##

SpendFile <- "Spendings_GDP.csv"
Spend <- read.csv(SpendFile)
colnames(Spend) <- c("CTR", "INDICATOR", "SUBJECT", "MEASURE", "FREQUENCY", "TIME", "Value", "Flag Codes")

Spendings1_2 <- sqldf("select CTR, Value from Spend where SUBJECT=='PRY_NTRY'")
Spendings3 <- sqldf("select CTR, Value from Spend where SUBJECT=='TRY'")


#					#
###### COMPLETE DATATABLE #####
#					#

tmp1 <- merge(Diplo2012,PISAMFilt,by = "CTR")
tmp1 <- na.omit(tmp1)								# 36 entries
tmp2 <- merge(STRatio,tmp1,by = "CTR")
tmp2 <- na.omit(tmp2)								# 30 entries
colnames(tmp2) <- c("CTR", "tmp 2", "tmp 1", "PISA Maths Value")
tmp3 <- merge(Slr2012,tmp2,by = "CTR")
tmp3 <- na.omit(tmp3)
colnames(tmp3) <- c("CTR", "tmp3", "tmp 2", "tmp 1", "PISA Maths Value")
tmp4 <- merge(HoursUp,tmp3,by = "CTR")
tmp4 <- na.omit(tmp4)
colnames(tmp4) <- c("CTR", "tmp4", "tmp3", "tmp 2", "tmp 1", "PISA Maths Value")
tmp5 <- merge(Spendings1_2,tmp4,by = "CTR")
PISAM12 <- na.omit(tmp5)							# 26 entries
colnames(PISAM12) <- c("CTR", "Spendings on education, B USD", "Teaching time, h", "Teachers salaries, USD", "Student to teaching staff ratio", "Rate of tertiary diplomas","PISA Maths Value")			# 22 entries

tmp1 <- merge(Diplo2012,PISASFilt,by = "CTR")
tmp1 <- na.omit(tmp1)								# 36 entries
tmp2 <- merge(STRatio,tmp1,by = "CTR")
tmp2 <- na.omit(tmp2)								# 30 entries
colnames(tmp2) <- c("CTR", "tmp 2", "tmp 1", "PISA Sciences Value")
tmp3 <- merge(Slr2012,tmp2,by = "CTR")	
tmp3 <- na.omit(tmp3)
colnames(tmp3) <- c("CTR", "tmp3", "tmp 2", "tmp 1", "PISA Sciences Value")
tmp4 <- merge(HoursUp,tmp3,by = "CTR")
tmp4 <- na.omit(tmp4)
colnames(tmp4) <- c("CTR", "tmp4", "tmp3", "tmp 2", "tmp 1", "PISA Sciences Value")
tmp5 <- merge(Spendings1_2,tmp4,by = "CTR")
PISAS12 <- na.omit(tmp5)							# 26 entries
colnames(PISAS12) <- c("CTR", "Spendings on education, B USD", "Teaching time, h", "Teachers salaries, USD", "Student to teaching staff ratio", "Rate of tertiary diplomas","PISA Sciences Value")			# 22 entries

## Principal Components Analysis ##

log.mathsfact <- log(PISAM12[, 2:6])
maths.scores <- PISAM12[, 7]

mathsfact.pca <- prcomp(log.mathsfact,
                 center = TRUE,
                 scale. = TRUE) 

print(mathsfact.pca)
summary(mathsfact.pca)

log.scifact <- log(PISAS12[, 2:6])
scifact.scores <- PISAS12[, 7]

scifact.pca <- prcomp(log.scifact,
                 center = TRUE,
                 scale. = TRUE) 

print(scifact.pca)
summary(scifact.pca)

# Ploting the resulting principal vectors graph #

# g <- ggbiplot(mathsfact.pca, obs.scale = 1, var.scale = 1, 
#              groups = ir.species, ellipse = TRUE, 
#              circle = TRUE)
# g <- g + scale_color_discrete(name = '')
# g <- g + theme(legend.direction = 'horizontal', 
#                legend.position = 'top')
# print(g)
