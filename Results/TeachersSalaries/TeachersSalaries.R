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

## LOADING TEACHERS SALARIES FILES ##
SlrFile <- "Teachers_Salaries.csv"
Slr <- read.csv(SlrFile)
colnames(Slr) <- c("CTR", "INDICATOR", "SUBJECT", "MEASURE", "FREQUENCY", "TIME", "Value", "Flag Codes")

Slr2012 <- sqldf("select CTR, Value from Slr where MEASURE=='USD' and TIME=='2012'")

# Creating and ploting the Salaries to Maths PISA Scores dataframe #

Slr2012PISAM <- merge(Slr2012,PISAMFilt,by = "CTR")			# 33 entries
Slr2012PISAM <- na.omit(Slr2012PISAM)					# 33 entries
colnames(Slr2012PISAM) <- c("CTR", "Teachers Salaries USD", "PISA Maths Score")
plot(Slr2012PISAM$"Teachers Salaries USD", Slr2012PISAM$"PISA Maths Score")		# It looks like there is some interesting correlation

# Creating and ploting the Salaries to Sciences PISA Scores dataframe #

Slr2012PISAS <- merge(Slr2012,PISASFilt,by = "CTR")			# 33 entries
Slr2012PISAS <- na.omit(Slr2012PISAS)					# 33 entries
colnames(Slr2012PISAS) <- c("CTR", "Teachers Salaries USD", "PISA Sciences Score")
plot(Slr2012PISAS$"Teachers Salaries USD", Slr2012PISAS$"PISA Sciences Score")	# That also looks promising

# Creating discrete dataframes #

Discrete_Slr_M10 <- discretize2d(Slr2012PISAM$"Teachers Salaries USD", Slr2012PISAM$"PISA Maths Score", numBins1=10, numBins2=10)
Discrete_Slr_S10 <- discretize2d(Slr2012PISAS$"Teachers Salaries USD", Slr2012PISAS$"PISA Sciences Score", numBins1=10, numBins2=10)

# Fisher Testing #

SlrFisherM10 <- fisher.test(Discrete_Slr_M10)
	# Fisher's Exact Test for Count Data
	# data:  Discrete_Slr_M10
	# p-value = 0.02672
	# alternative hypothesis: two.sided
SlrFisherS10 <- fisher.test(Discrete_Slr_S10)
	#
	# Error in fisher.test(Discrete_Slr_S10): FEXACT error 30
	# Stack length exceeded in f3xact
	#

Slr_miM10 <- mi.plugin(Discrete_Slr_M10)
Slr_entrM10 <- entropy(Discrete_Slr_M10)
SlrDependencyM10 <- (Slr_miM10)/(Slr_entrM10)

Slr_miS10 <- mi.plugin(Discrete_Slr_S10)
Slr_entrS10 <- entropy(Discrete_Slr_S10)
SlrDependencyS10 <- (Slr_miS10)/(Slr_entrS10)

