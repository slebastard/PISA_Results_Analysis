## LOADING LIBRARIES ##
library(MASS)
library(sqldf)
library(entropy)
setwd("D:/Ponts/2A/S4/Statistiques/Projet/Data")

## LOADING INNOVATION DB ##
InnFile <- "InnovationEdu.csv"
Inn <- read.csv(InnFile)
colnames(Inn) <- c("CTR", "Country", "IND", "Indicator", "PER", "Period", "GRD", "Grade", "FLD", "Field", "MSR", "Measure", "Value", "Flag.Codes", "Flags")

## CREATING ALL THE USEFUL SUBFRAMES FOR INNOVATION IN TEACHING METHODS ##
InnIndeWorkMath4 <- sqldf("select CTR, Value from Inn where IND=='5CIWT' and GRD=='FOG' and PER=='INI' and FLD=='MAT' and MSR=='MN'")

InnIndeWorkMath8 <- sqldf("select CTR, Value from Inn where IND=='5CIWT' and GRD=='EIG' and PER=='INI' and FLD=='MAT' and MSR=='MN'")

InnExplainMath8 <- sqldf("select CTR, Value from Inn where IND=='6EAAT' and GRD=='EIG' and PER=='INI' and FLD=='MAT' and MSR=='MN'")

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

## LOADING AND FILTERING PISA SCIENCES DB ##
PISASFile <- "PISA_Sciences.csv"
PISAS <- read.csv(PISASFile)
colnames(PISAS) <- c("CTR", "INDICATOR", "SUBJECT", "MEASURE", "FREQUENCY", "TIME", "Value", "Flag.Codes")
PISASFilt <- sqldf("select CTR, Value from PISAS where SUBJECT=='GIRL' and TIME=='2012'")
head(PISASFilt)

## MERGING THE FRAMES ##

tmp1 <- merge(InnIndeWorkMath4,PISAMFilt,by = "CTR")
tmp2 <- merge(InnIndeWorkMath8,tmp1,by = "CTR")
tmp3 <- merge(InnExplainMath8,tmp2,by = "CTR")
tmp4 <- merge(InnGroupWorkMath8,tmp3,by = "CTR")
tmp5 <- merge(InnIndeSolvingMath8,tmp4,by = "CTR")
TestFrame <- merge(InnAbilityGroups,tmp5,by = "CTR")

colnames(TestFrame) <- c("CTR", "Ability Groups %", "Indep Solving Method % 8th", "Group Work % 8th", "Valor Explanations % 8th", "Indep Work % 8th", "Indep Work % 4th", "PISA Maths Score")
# On obtient un Dataframe correct mais de taille trop faible. On va donc commencer à s'intéresser uniquement aux paires (InnovationFeature,PISAMFilt) dans un premier temps #

IndeWork4 <- merge(InnIndeWorkMath4,PISAMFilt,by = "CTR")			# 10 entries
colnames(IndeWork4) <- c("CTR", "Indep Work % 4th", "PISA Maths Score")
IndeWork8 <- merge(InnIndeWorkMath8,PISAMFilt,by = "CTR")			# 12 entries
colnames(IndeWork8) <- c("CTR", "Indep Work % 8th", "PISA Maths Score")
Expl8 <- merge(InnExplainMath8,PISAMFilt,by = "CTR")				# 15 entries
colnames(Expl8) <- c("CTR", "Valor Explanations % 8th", "PISA Maths Score")
GroupWork8 <- merge(InnGroupWorkMath8,PISAMFilt,by = "CTR")			# 15 entries
colnames(GroupWork8) <- c("CTR", "Group Work % 8th", "PISA Maths Score")
IndeSolving8 <- merge(InnIndeSolvingMath8,PISAMFilt,by = "CTR")		# 11 entries
colnames(IndeSolving8) <- c("CTR", "Indep Solving Method % 8th", "PISA Maths Score")
LvlGroups <- merge(InnAbilityGroups,PISAMFilt,by = "CTR")			# 33 entries
colnames(LvlGroups) <- c("CTR", "By group lvl %", "PISA Maths Score")

## DISCRETIZING PISAM FOR DEPENDENCE TESTING ##

PISAMD <- discretize(PISAMFilt)

## LvlGroups to PISAM dependency ##

Discrete_Lvl <- discretize2d(LvlGroups$"By group lvl %", LvlGroups$"PISA Maths Score", numBins1=8, numBins2=8)
LvlPISAM_mi <- mi.plugin(Discrete_Lvl)
LvlPISAM_entr <- entropy(Discrete_Lvl)
LvlDependency <- (LvlPISAM_mi)/(LvlPISAM_entr)

LvlFisher <- fisher.test(Discrete_Lvl)

## Working in groups in 8th grade RELATED TO PISA maths results ##

Discrete_GroupWork8 <- discretize2d(GroupWork8$"Group Work % 8th", GroupWork8$"PISA Maths Score", numBins1=8, numBins2=8)
GW8PISAM_mi <- mi.plugin(Discrete_GroupWork8)
GW8PISAM_entr <- entropy(Discrete_GroupWork8)
GW8Dependency <- (GW8PISAM_mi)/(GW8PISAM_entr)

GW8Fisher <- fisher.test(Discrete_GroupWork8)

## Valorisation of explanations in mathematics, 8th grade ##

Discrete_Expl8 <- discretize2d(Expl8$"Valor Explanations % 8th", Expl8$"PISA Maths Score", numBins1=8, numBins2=8)
Expl8PISAM_mi <- mi.plugin(Discrete_Expl8)
Expl8PISAM_entr <- entropy(Discrete_Expl8)
Expl8Dependency <- (Expl8PISAM_mi)/(Expl8PISAM_entr)

Expl8Fisher <- fisher.test(Discrete_Expl8)


## LOADING STUD/TEACH DB ##
STFile <- "Stud_Teach_Ratio.csv"
ST <- read.csv(STFile)
colnames(ST) <- c("CTR", "Country", "LVL", "Level of education", "SCT", "Reference sector", "IND", "Indicator", "YEA", "Year", "UNT", "Unit", "PWC", "PowerCode", "PER", "Reference Period", "Value", "FLG", "Flags")

# Ratio students/TeachingStaff #

STRatio <- sqldf("select CTR, Value from ST where IND=='PERS_RATIO_INST' and SCT=='INST_T' and LVL=='L2_3'")

STM <- merge(STRatio,PISAMFilt,by = "CTR")			# 37 entries
STM <- na.omit(STM)							# 32 entries
colnames(STM) <- c("CTR", "S/T ratio", "PISA Maths Score")

STS <- merge(STRatio,PISASFilt,by = "CTR")			# 37 entries
STS <- na.omit(STS)							# 32 entries
colnames(STS) <- c("CTR", "S/T ratio", "PISA Sciences Score")

# Discrete indicators and Fisher tests #

Discrete_ST_M8 <- discretize2d(STM$"S/T ratio", STM$"PISA Maths Score", numBins1=8, numBins2=8)
Discrete_ST_M12 <- discretize2d(STM$"S/T ratio", STM$"PISA Maths Score", numBins1=12, numBins2=12)
Discrete_ST_S8 <- discretize2d(STS$"S/T ratio", STS$"PISA Sciences Score", numBins1=8, numBins2=8)
Discrete_ST_S12 <- discretize2d(STS$"S/T ratio", STS$"PISA Sciences Score", numBins1=12, numBins2=12)

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

# Plot of PISAMaths = f(S/T) #
plot(STM$"S/T ratio", STM$"PISA Maths Score")

# Plot of PISAScience = f(S/T) #
plot(STS$"S/T ratio", STS$"PISA Sciences Score")

#						#
####### MULTILINEAR ANALYSIS ########
#						#

# Create a database that regroup every previous table #

tmp1 <- merge(InnIndeWorkMath4,PISAMFilt,by = "CTR")			
colnames(tmp1) <- c("CTR", "Indep Work % 4th", "PISA Maths Score")
tmp2 <- merge(InnIndeWorkMath8,tmp1,by = "CTR")			
colnames(tmp2) <- c("CTR", "Indep Work % 8th", "Indep Work % 4th", "PISA Maths Score")
tmp3 <- merge(InnExplainMath8,tmp2,by = "CTR")				
colnames(tmp3) <- c("CTR", "Valor Explanations % 8th", "Indep Work % 8th", "Indep Work % 4th", "PISA Maths Score")
tmp4 <- merge(InnGroupWorkMath8,tmp3,by = "CTR")			
colnames(tmp4) <- c("CTR", "Group Work % 8th", "Valor Explanations % 8th", "Indep Work % 8th", "Indep Work % 4th", "PISA Maths Score")
tmp5 <- merge(InnIndeSolvingMath8,tmp4,by = "CTR")		
colnames(tmp5) <- c("CTR", "Indep Solving Method % 8th", "Group Work % 8th", "Valor Explanations % 8th", "Indep Work % 8th", "Indep Work % 4th", "PISA Maths Score")
AllInnoMath <- merge(InnAbilityGroups,tmp5,by = "CTR")			
colnames(AllInnoMath) <- c("CTR", "By group lvl %", "Indep Solving Method % 8th", "Group Work % 8th", "Valor Explanations % 8th", "Indep Work % 8th", "Indep Work % 4th", "PISA Maths Score")

# Running the linear regression #

MultiRegMaths <- lm(AllInnoMath$"PISA Maths Score" ~ AllInnoMath$"By group lvl %" + AllInnoMath$"Indep Solving Method % 8th" + AllInnoMath$"Group Work % 8th" + AllInnoMath$"Valor Explanations % 8th" + AllInnoMath$"Indep Work % 8th" + AllInnoMath$"Indep Work % 4th")
summary(MultiRegMaths)