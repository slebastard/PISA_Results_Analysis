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
PISARFile <- "PISA_Reading.csv"
PISAR <- read.csv(PISARFile)
colnames(PISAR) <- c("CTR", "INDICATOR", "SUBJECT", "MEASURE", "FREQUENCY", "TIME", "Value", "Flag.Codes")
PISARFilt <- sqldf("select CTR, Value from PISAR where SUBJECT=='GIRL' and TIME=='2012'")
head(PISARFilt)

## LOADING CSV FILE ABOUT S/T RATIO ##

STFile <- "Stud_Teach_Ratio.csv"
ST <- read.csv(STFile)
colnames(ST) <- c("CTR", "Country", "LVL", "Level of education", "SCT", "Reference sector", "IND", "Indicator", "YEA", "Year", "UNT", "Unit", "PWC", "PowerCode", "PER", "Reference Period", "Value", "FLG", "Flags")

# Ratio students/TeachingStaff #

STRatio <- sqldf("select CTR, Value from ST where IND=='PERS_RATIO_INST' and SCT=='INST_T' and LVL=='L2_3'")

STM <- merge(STRatio,PISAMFilt,by = "CTR")			# 37 entries
STM <- na.omit(STM)							# 32 entries
colnames(STM) <- c("CTR", "S/T ratio", "PISA Maths Score")

STR <- merge(STRatio,PISARFilt,by = "CTR")			# 37 entries
STR <- na.omit(STR)							# 32 entries
colnames(STR) <- c("CTR", "S/T ratio", "PISA Reading Score")

# Discrete indicators and Fisher tests #

Discrete_ST_M8 <- discretize2d(STM$"S/T ratio", STM$"PISA Maths Score", numBins1=8, numBins2=8)
Discrete_ST_M12 <- discretize2d(STM$"S/T ratio", STM$"PISA Maths Score", numBins1=12, numBins2=12)
Discrete_ST_R8 <- discretize2d(STR$"S/T ratio", STR$"PISA Reading Score", numBins1=8, numBins2=8)
Discrete_ST_R12 <- discretize2d(STR$"S/T ratio", STR$"PISA Reading Score", numBins1=12, numBins2=12)

STFisherM8 <- fisher.test(Discrete_ST_M8)
	# Fisher's Exact Test for Count Data
	# data:  Discrete_ST_M8
	# p-value = 0.09221
	# alternative hypothesis: two.sided
STFisherR8 <- fisher.test(Discrete_ST_R8)
	# Fisher's Exact Test for Count Data
	# data:  Discrete_ST_R8
	# p-value = 0.1772
	# alternative hypothesis: two.sided

# Plot of PISAMaths = f(S/T) #
plot(STM$"S/T ratio", STM$"PISA Maths Score")

# Plot of PISARcience = f(S/T) #
plot(STR$"S/T ratio", STR$"PISA Reading Score")

