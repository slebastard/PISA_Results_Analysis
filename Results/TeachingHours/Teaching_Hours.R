## LOADING LIBRARIES ##
library(MASS)
library(sqldf)
library(entropy)
setwd("D:/Ponts/2A/S4/Statistiques/Projet/Data")

## LOADING AND FILTERING PISA MATHS DB ##
PISAMFile <- "PISA_Maths.csv"
PISAM <- read.csv(PISAMFile)
colnames(PISAM) <- c("CTR", "INDICATOR", "SUBJECT", "MEASURE", "FREQUENCY", "TIME", "Value", "Flag.Codes")
PISAM12 <- sqldf("select CTR, Value from PISAM where SUBJECT=='GIRL' and TIME=='2012'")
head(PISAM12)
PISAM09 <- sqldf("select CTR, Value from PISAM where SUBJECT=='GIRL' and TIME=='2009'")
head(PISAM09)
PISAM06 <- sqldf("select CTR, Value from PISAM where SUBJECT=='GIRL' and TIME=='2006'")
head(PISAM06)


## LOADING AND FILTERING PISA READING DB ##
PISARFile <- "PISA_Sciences.csv"
PISAR <- read.csv(PISARFile)
colnames(PISAR) <- c("CTR", "INDICATOR", "SUBJECT", "MEASURE", "FREQUENCY", "TIME", "Value", "Flag.Codes")
PISAR12 <- sqldf("select CTR, Value from PISAR where SUBJECT=='GIRL' and TIME=='2012'")
head(PISAR12)
PISAR09 <- sqldf("select CTR, Value from PISAR where SUBJECT=='GIRL' and TIME=='2009'")
head(PISAR09)
PISAR06 <- sqldf("select CTR, Value from PISAR where SUBJECT=='GIRL' and TIME=='2006'")
head(PISAR06)

## LOADING CSV FILE ABOUT TEACHING HOURS ##

THFile <- "Teaching_Hours.csv"
HoursUnfilt <- read.csv(THFile)
colnames(HoursUnfilt) <- c("CTR", "INDICATOR", "SUBJECT", "MEASURE", "FREQUENCY", "TIME", "Value", "Flag Codes")

# TEACHING HOURS #

# Prepare the datasets for the years 206, 2009 and 2012, each time for upper and lower secondary #

HoursLow12 <- sqldf("select CTR, Value from HoursUnfilt where SUBJECT=='LOWSRY' and TIME=='2012'")
HoursUp12 <- sqldf("select CTR, Value from HoursUnfilt where SUBJECT=='UPPSRY' and TIME=='2012'")

HoursLow09 <- sqldf("select CTR, Value from HoursUnfilt where SUBJECT=='LOWSRY' and TIME=='2009'")
HoursUp09 <- sqldf("select CTR, Value from HoursUnfilt where SUBJECT=='UPPSRY' and TIME=='2009'")

HoursLow06 <- sqldf("select CTR, Value from HoursUnfilt where SUBJECT=='LOWSRY' and TIME=='2006'")
HoursUp06 <- sqldf("select CTR, Value from HoursUnfilt where SUBJECT=='UPPSRY' and TIME=='2006'")

# Merge with the corresponding PISA results in mathematics #

HoursMUp12 <- merge(HoursUp12,PISAM12,by = "CTR")			# 37 entries
HoursMUp12 <- na.omit(HoursMUp12)					# 32 entries
colnames(HoursMUp12) <- c("CTR", "Teaching Hours Upper Sec", "PISA Maths Score")
HoursMUp12$"CTR" <- paste(HoursMUp12$"CTR", "12", sep = "")

HoursMUp09 <- merge(HoursUp09,PISAM09,by = "CTR")			# 37 entries
HoursMUp09 <- na.omit(HoursMUp09)					# 32 entries
colnames(HoursMUp09) <- c("CTR", "Teaching Hours Upper Sec", "PISA Maths Score")
HoursMUp09$"CTR" <- paste(HoursMUp09$"CTR", "09", sep = "")

HoursMUp06 <- merge(HoursUp06,PISAM06,by = "CTR")			# 37 entries
HoursMUp06 <- na.omit(HoursMUp06)					# 32 entries
colnames(HoursMUp06) <- c("CTR", "Teaching Hours Upper Sec", "PISA Maths Score")
HoursMUp06$"CTR" <- paste(HoursMUp06$"CTR", "06", sep = "")

HoursMLow12 <- merge(HoursLow12,PISAM12,by = "CTR")			# 37 entries
HoursMLow12 <- na.omit(HoursMLow12)					# 32 entries
colnames(HoursMLow12) <- c("CTR", "Teaching Hours Lower Sec", "PISA Maths Score")
HoursMLow12$"CTR" <- paste(HoursMLow12$"CTR", "12", sep = "")

HoursMLow09 <- merge(HoursLow09,PISAM09,by = "CTR")			# 37 entries
HoursMLow09 <- na.omit(HoursMLow09)					# 32 entries
colnames(HoursMLow09) <- c("CTR", "Teaching Hours Lower Sec", "PISA Maths Score")
HoursMLow09$"CTR" <- paste(HoursMLow09$"CTR", "09", sep = "")

HoursMLow06 <- merge(HoursLow06,PISAM06,by = "CTR")			# 37 entries
HoursMLow06 <- na.omit(HoursMLow06)					# 32 entries
colnames(HoursMLow06) <- c("CTR", "Teaching Hours Lower Sec", "PISA Maths Score")
HoursMLow06$"CTR" <- paste(HoursMLow06$"CTR", "06", sep = "")

# Merge with the corresponding PISA results in reading #

HoursRUp12 <- merge(HoursUp12,PISAR12,by = "CTR")			# 37 entries
HoursRUp12 <- na.omit(HoursRUp12)					# 32 entries
colnames(HoursRUp12) <- c("CTR", "Teaching Hours Upper Sec", "PISA Reading Score")
HoursRUp12$"CTR" <- paste(HoursRUp12$"CTR", "12", sep = "")

HoursRUp09 <- merge(HoursUp09,PISAR09,by = "CTR")			# 37 entries
HoursRUp09 <- na.omit(HoursRUp09)					# 32 entries
colnames(HoursRUp09) <- c("CTR", "Teaching Hours Upper Sec", "PISA Reading Score")
HoursRUp09$"CTR" <- paste(HoursRUp09$"CTR", "09", sep = "")

HoursRUp06 <- merge(HoursUp06,PISAR06,by = "CTR")			# 37 entries
HoursRUp06 <- na.omit(HoursRUp06)					# 32 entries
colnames(HoursRUp06) <- c("CTR", "Teaching Hours Upper Sec", "PISA Reading Score")
HoursRUp06$"CTR" <- paste(HoursRUp06$"CTR", "06", sep = "")

HoursRLow12 <- merge(HoursLow12,PISAR12,by = "CTR")			# 37 entries
HoursRLow12 <- na.omit(HoursRLow12)					# 32 entries
colnames(HoursRLow12) <- c("CTR", "Teaching Hours Lower Sec", "PISA Reading Score")
HoursRLow12$"CTR" <- paste(HoursRLow12$"CTR", "12", sep = "")

HoursRLow09 <- merge(HoursLow09,PISAR09,by = "CTR")			# 37 entries
HoursRLow09 <- na.omit(HoursRLow09)					# 32 entries
colnames(HoursRLow09) <- c("CTR", "Teaching Hours Lower Sec", "PISA Reading Score")
HoursRLow09$"CTR" <- paste(HoursRLow09$"CTR", "09", sep = "")

HoursRLow06 <- merge(HoursLow06,PISAR06,by = "CTR")			# 37 entries
HoursRLow06 <- na.omit(HoursRLow06)					# 32 entries
colnames(HoursRLow06) <- c("CTR", "Teaching Hours Lower Sec", "PISA Reading Score")
HoursRLow06$"CTR" <- paste(HoursRLow06$"CTR", "06", sep = "")

## MERGE THE DATA FROM THE DIFFERENT YEARS ##

HoursMUpTMP <- rbind(HoursMUp12,HoursMUp09)
HoursMUpAll <- rbind(HoursMUpTMP,HoursMUp06)
plot(HoursMUpAll$"Teaching Hours Upper Sec", HoursMUpAll$"PISA Maths Score")

HoursMLowTMP <- rbind(HoursMLow12,HoursMLow09)
HoursMLowAll <- rbind(HoursMLowTMP,HoursMLow06)
plot(HoursMLowAll$"Teaching Hours Lower Sec", HoursMLowAll$"PISA Maths Score")

HoursRUpTMP <- rbind(HoursRUp12,HoursRUp09)
HoursRUpAll <- rbind(HoursRUpTMP,HoursRUp06)
plot(HoursRUpAll$"Teaching Hours Upper Sec", HoursRUpAll$"PISA Reading Score")

HoursRLowTMP <- rbind(HoursRLow12,HoursRLow09)
HoursRLowAll <- rbind(HoursRLowTMP,HoursRLow06)
plot(HoursRLowAll$"Teaching Hours Lower Sec", HoursRLowAll$"PISA Reading Score")

# Discrete indicators and Fisher tests #

Discrete_TH_MUp8 <- discretize2d(HoursMUpAll$"Teaching Hours Upper Sec", HoursMUpAll$"PISA Maths Score", numBins1=8, numBins2=8)
Discrete_TH_RUp8 <- discretize2d(HoursRUpAll$"Teaching Hours Upper Sec", HoursRUpAll$"PISA Reading Score", numBins1=8, numBins2=8)

Discrete_TH_MLow8 <- discretize2d(HoursMLowAll$"Teaching Hours Lower Sec", HoursMLowAll$"PISA Maths Score", numBins1=8, numBins2=8)
Discrete_TH_RLow8 <- discretize2d(HoursRLowAll$"Teaching Hours Lower Sec", HoursRLowAll$"PISA Reading Score", numBins1=8, numBins2=8)

STChiMUp8 <- chisq.test(Discrete_TH_MUp8)
STChiRUp8 <- chisq.test(Discrete_TH_RUp8)

STChiMLow8 <- chisq.test(Discrete_TH_MLow8)
STChiRLow8 <- chisq.test(Discrete_TH_RLow8)

# Plot of PISAMaths = f(S/T) #
plot(HoursMUp12$"Teaching Hours", HoursMUp12$"PISA Maths Score")

# Plot of PISARcience = f(S/T) #
plot(STS$"Teaching Hours", STS$"PISA Reading Score")

