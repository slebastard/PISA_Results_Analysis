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
PISARFile <- "PISA_Reading.csv"
PISAR <- read.csv(PISARFile)
colnames(PISAR) <- c("CTR", "INDICATOR", "SUBJECT", "MEASURE", "FREQUENCY", "TIME", "Value", "Flag.Codes")
PISARFilt <- sqldf("select CTR, Value from PISAR where SUBJECT=='GIRL' and TIME=='2012'")
head(PISARFilt)

## LOADING DIPLOMAS RATES FILES ##
DiploFile <- "ThirdGrade_Edu_Rate.csv"
Diplo <- read.csv(DiploFile)
colnames(Diplo) <- c("CTR", "INDICATOR", "SUBJECT", "MEASURE", "FREQUENCY", "TIME", "Value", "Flag Codes")

Diplo2012 <- sqldf("select CTR, Value from Diplo where TIME=='2012'")

# Creating and ploting the Diplomas Rate linked to Maths PISA Scores dataframe #

Diplo2012PISAM <- merge(Diplo2012,PISAMFilt,by = "CTR")			# 33 entries
Diplo2012PISAM <- na.omit(Diplo2012PISAM)						# 33 entries
colnames(Diplo2012PISAM) <- c("CTR", "Tertiary diplomas rate", "PISA Maths Score")
plot(Diplo2012PISAM$"PISA Maths Score", Diplo2012PISAM$"Tertiary diplomas rate")		# It looks like there is a strong correlation!
summary(Diplo2012PISAM)
	# CTR     Tertiary diplomas rate PISA Maths Score
 	# AUS    : 1   Min.   :14.51          Min.   :382.9   
 	# AUT    : 1   1st Qu.:30.06          1st Qu.:477.0   
 	# BEL    : 1   Median :40.44          Median :491.5   
 	# BRA    : 1   Mean   :39.44          Mean   :487.6   
 	# CAN    : 1   3rd Qu.:45.47          3rd Qu.:507.2   
 	# CHE    : 1   Max.   :65.68          Max.   :544.2   
 	# (Other):30 

# Creating and ploting the Diplomas Rate linked to Sciences PISA Scores dataframe #

Diplo2012PISAR <- merge(Diplo2012,PISARFilt,by = "CTR")			# 36 entries
Diplo2012PISAR <- na.omit(Diplo2012PISAR)						# 36 entries
colnames(Diplo2012PISAR) <- c("CTR", "Tertiary diplomas rate", "PISA Reading Score")
plot(Diplo2012PISAR$"PISA Reading Score", Diplo2012PISAR$"Tertiary diplomas rate")		# That also looks promising!
summary(Diplo2012PISAR)
	# CTR     Tertiary diplomas rate PISA Sciences Score
 	# AUS    : 1   Min.   :14.51          Min.   :403.9      
 	# AUT    : 1   1st Qu.:30.06          1st Qu.:488.9      
 	# BEL    : 1   Median :40.44          Median :500.3      
 	# BRA    : 1   Mean   :39.44          Mean   :499.0      
 	# CAN    : 1   3rd Qu.:45.47          3rd Qu.:519.3      
 	# CHE    : 1   Max.   :65.68          Max.   :553.9      
 	# (Other):30 

# Creating discrete dataframes #

Discrete_Diplo_M9 <- discretize2d(Diplo2012PISAM$"PISA Maths Score", Diplo2012PISAM$"Tertiary diplomas rate", numBins1=8, numBins2=8)
Discrete_Diplo_R9 <- discretize2d(Diplo2012PISAR$"PISA Reading Score", Diplo2012PISAR$"Tertiary diplomas rate", numBins1=8, numBins2=8)

# Fisher Testing #

DiploFisherM9 <- fisher.test(Discrete_Diplo_M9)
	# Fisher's Exact Test for Count Data
	# data:  Discrete_Diplo_M9
	# p-value = 0.5232
	# alternative hypothesis: two.sided
DiploFisherR9 <- fisher.test(Discrete_Diplo_R9)
	# Fisher's Exact Test for Count Data
	# data:  Discrete_Diplo_S9
	# p-value = 0.6613
	# alternative hypothesis: two.sided

Diplo_miM9 <- mi.plugin(Discrete_Diplo_M9)
Diplo_entrM9 <- entropy(Discrete_Diplo_M9)
DiploDependencyM9 <- (Diplo_miM9)/(Diplo_entrM9)
	# 0.1721299
Diplo_miS9 <- mi.plugin(Discrete_Diplo_S9)
Diplo_entrS9 <- entropy(Discrete_Diplo_S9)
DiploDependencyS9 <- (Diplo_miS9)/(Diplo_entrS9)
	# 0.1545356
