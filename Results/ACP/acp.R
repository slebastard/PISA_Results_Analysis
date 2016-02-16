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
PISAM12 <- sqldf("select CTR, Value from PISAM where SUBJECT=='GIRL' and TIME=='2012'")
head(PISAM12)

## LOADING AND FILTERING PISA SCIENCES DB ##
PISARFile <- "PISA_Reading.csv"
PISAR <- read.csv(PISARFile)
colnames(PISAR) <- c("CTR", "INDICATOR", "SUBJECT", "MEASURE", "FREQUENCY", "TIME", "Value", "Flag.Codes")
PISAR12 <- sqldf("select CTR, Value from PISAS where SUBJECT=='GIRL' and TIME=='2012'")
head(PISAR12)

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

tmp1 <- merge(Diplo2012,PISAM12,by = "CTR")
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

tmp1 <- merge(Diplo2012,PISAR12,by = "CTR")
tmp1 <- na.omit(tmp1)								# 36 entries
tmp2 <- merge(STRatio,tmp1,by = "CTR")
tmp2 <- na.omit(tmp2)								# 30 entries
colnames(tmp2) <- c("CTR", "tmp 2", "tmp 1", "PISA Reading Value")
tmp3 <- merge(Slr2012,tmp2,by = "CTR")	
tmp3 <- na.omit(tmp3)
colnames(tmp3) <- c("CTR", "tmp3", "tmp 2", "tmp 1", "PISA Reading Value")
tmp4 <- merge(HoursUp,tmp3,by = "CTR")
tmp4 <- na.omit(tmp4)
colnames(tmp4) <- c("CTR", "tmp4", "tmp3", "tmp 2", "tmp 1", "PISA Reading Value")
tmp5 <- merge(Spendings1_2,tmp4,by = "CTR")
PISAS12 <- na.omit(tmp5)							# 26 entries
colnames(PISAS12) <- c("CTR", "Spendings on education, B USD", "Teaching time, h", "Teachers salaries, USD", "Student to teaching staff ratio", "Rate of tertiary diplomas","PISA Reading Value")			# 22 entries

## Principal Components Analysis for mathematics results ##

# Distinguish quantitative from qualitative factors #
quant = PISAM12[,2:7]
qual = PISAM12[,1]
pairs(quant)

# Run PCA, get summary #
acp = princomp(quant, cor = T, scores = T)
summary(acp)
	# Importance of components:
	#                           Comp.1    Comp.2    Comp.3     Comp.4
	# Standard deviation     1.4926975 1.4166800 0.9005918 0.76227419
	# Proportion of Variance 0.3713577 0.3344970 0.1351776 0.09684366
	# Cumulative Proportion  0.3713577 0.7058547 0.8410323 0.93787596
	#                            Comp.5     Comp.6
	# Standard deviation     0.49084269 0.36306706
	# Proportion of Variance 0.04015442 0.02196962
	# Cumulative Proportion  0.97803038 1.00000000

	# Here we'll decide to keep only the four first principal components (which represent 93.8% of the data)

# Compute the eigenvalues, plot them #

valp = acp $ sdev^2
dev.new()
plot(valp, type = "b")

loadings = acp $ loadings
print(loadings)
	# Loadings:
	#                                 Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6
	# Spendings on education, B USD          -0.510  0.496  0.645  0.257       
	# Teaching time, h                -0.187 -0.605 -0.289        -0.675  0.237
	# Teachers salaries, USD           0.356 -0.410 -0.601 -0.127  0.559  0.123
	# Student to teaching staff ratio -0.428 -0.321  0.419 -0.603  0.284  0.308
	# Rate of tertiary diplomas        0.525 -0.282  0.258 -0.446 -0.241 -0.567
	# PISA Maths Value                 0.609  0.151  0.258        -0.167  0.714

# print(scifact.pca)
# summary(scifact.pca)

# Ploting the resulting principal vectors graph #

# g <- ggbiplot(mathsfact.pca, obs.scale = 1, var.scale = 1, 
#              groups = ir.species, ellipse = TRUE, 
#              circle = TRUE)
# g <- g + scale_color_discrete(name = '')
# g <- g + theme(legend.direction = 'horizontal', 
#                legend.position = 'top')
# print(g)

#					 #
## Multiple linear regression ##
#					 #

# Results in mathematics #

MultiRegMaths <- lm(PISAM12$"PISA Maths Value" ~ PISAM12$"Spendings on education, B USD" + PISAM12$"Teaching time, h" + PISAM12$"Teachers salaries, USD" + PISAM12$"Student to teaching staff ratio")
summary(MultiRegMaths)

MultiRegMaths2 <- lm(PISAM12$"PISA Maths Value" ~ PISAM12$"Teaching time, h" + PISAM12$"Student to teaching staff ratio")
summary(MultiRegMaths2)

anova(MultiRegMaths, MultiRegMaths2)

# Results in reading #

MultiRegReading <- lm(PISAR12$"PISA Reading Value" ~ PISAR12$"Spendings on education, B USD" + PISAR12$"Teaching time, h" + PISAR12$"Teachers salaries, USD" + PISAR12$"Student to teaching staff ratio")
summary(MultiRegReading)

