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

