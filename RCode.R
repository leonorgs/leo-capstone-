# getwd()

# Raw Data Set September 2009
raw.data.09 <- read.csv("C:/Users/alber/Desktop/Ryerson/6. Spring-Summer 2020/Datasets Original/pub0909.csv")
head(raw.data.09)
nrow(raw.data.09)
summary(raw.data.09$LFSSTAT)
sum(is.na(raw.data.09$LFSSTAT))
str(raw.data.09$LFSSTAT)

# Only Employed Data
data.09 <- as.data.frame(raw.data.09[raw.data.09$LFSSTAT < 3,])

# Only Public and Private Sector Employees Data
data.09 <- as.data.frame(data.09[data.09$COWMAIN < 3,])


# MATCH VALUES TO 2019

# OCCUPATION Variable to match 2019's NOC_10
data.09$NOC_10 <- data.09$NOCS_01_25
sort(unique(data.09$NOC_10))
table(data.09$NOC_10)
data.09$NOC_10[data.09$NOC_10 == 2] <- 1
data.09$NOC_10[data.09$NOC_10 %in% (3:5)] <- 2
data.09$NOC_10[data.09$NOC_10 == 6] <- 3
data.09$NOC_10[data.09$NOC_10 %in% (7:8)] <- 4
data.09$NOC_10[data.09$NOC_10 %in% (9:10)] <- 5
data.09$NOC_10[data.09$NOC_10 == 11] <- 6
data.09$NOC_10[data.09$NOC_10 %in% (12:17)] <- 7
data.09$NOC_10[data.09$NOC_10 %in% (18:22)] <- 8
data.09$NOC_10[data.09$NOC_10 == 23] <- 9
data.09$NOC_10[data.09$NOC_10 %in% (24:25)] <- 10
sort(unique(data.09$NOC_10))
table(data.09$NOC_10)

# REPLACE EXAMPLE: data.09$noc_10 <- replace(data.09$noc_10, data.09$noc_10 == 2, 1)
# OTHER EXAMPLES:
# iris$Petal.Width [iris$Species == "setosa"]
# train.data$Embarked<-replace(train.data$Embarked,train.data$Embarked == "", NA)

# AGE OF YOUNGEST CHILD
sort(unique(data.09$AGYOWNKN))
table(data.09$AGYOWNKN)
data.09$AGYOWNKN[data.09$AGYOWNKN == 2] <- 1
data.09$AGYOWNKN[data.09$AGYOWNKN == 3] <- 2
data.09$AGYOWNKN[data.09$AGYOWNKN %in% (4:5)] <- 3
data.09$AGYOWNKN[data.09$AGYOWNKN == 6] <- 4
sort(unique(data.09$AGYOWNKN))
table(data.09$AGYOWNKN)

# CURRENT STUDENT STATUS
sort(unique(data.09$SCHOOLN))
table(data.09$SCHOOLN)
data.09$SCHOOLN[data.09$SCHOOLN %in% seq(from = 2, to = 8, by = 2)] <- 2
data.09$SCHOOLN[data.09$SCHOOLN %in% seq(from = 3, to = 9, by = 2)] <- 3
sort(unique(data.09$SCHOOLN))
table(data.09$SCHOOLN)

# Remove Out of Dates Variables
names(data.09)
data.09 <- subset(data.09, select = -c(ED76to89
                   , NAICS_43
                   , SOC80_49
                   , SOC80_21
                   , NOCS_01_25
                   , NOCS_01_47
                   , WHYPTOLD
                   , YNOLKOLD
                   , RELREFN
                   , EFAMSIZE
                   , EFAMEMPL
                   , EFAMUNEM
                   , SP_AGE7
                   , SP_LFSST
                   , SPED7689
                   , SPED1990
                   , SP_SOC80
                   , SP_NOCS01
                   , SP_UHRSM
                   , SP_UHRST
                   , SP_COWM
                   , SCH1624))
names(data.09)

# Remove Unemployment and other Variables specified on main report
data.09 <- subset(data.09, select = -c(AGE_6
                                       , EVERWORK
                                       , FTPTLAST
                                       , YABSENT
                                       , WKSAWAY
                                       , PAYAWAY
                                       , UHRSMAIN
                                       , AHRSMAIN
                                       , ATOTHRS
                                       , HRSAWAY
                                       , YAWAY
                                       , PAIDOT
                                       , UNPAIDOT
                                       , XTRAHRS
                                       , WHYPTNEW
                                       , PREVTEN
                                       , DURUNEMP
                                       , FLOWUNEM
                                       , UNEMFTPT
                                       , WHYLEFTO
                                       , WHYLEFTN
                                       , DURJLESS
                                       , AVAILABL
                                       , LKPUBAG
                                       , LKEMPLOY
                                       , LKRELS
                                       , LKATADS
                                       , LKANSADS
                                       , LKOTHERN
                                       , PRIORACT
                                       , YNOLOOK
                                       , TLOLOOK
                                       , FINALWT))

# Rename Variables
names(data.09)[names(data.09) == "EDUC90"] <- "EDUC"
names(data.09)[names(data.09) == "FILLER1"] <- "IMMIG"
names(data.09)[names(data.09) == "FILLER2"] <- "NOC_40"
names(data.09)[names(data.09) == "WHYPTNEW"] <- "WHYPT"
names(data.09)[names(data.09) == "AGYOWNKN"] <- "AGYOWNK"


# Move variable NOC_10, NAICS_18
noc10.idx <-  grep("NOC_10", names(data.09))
naics18.idx <-  grep("NAICS_18", names(data.09))
data.09 <- data.09[, c(1:13, naics18.idx, noc10.idx, 14, 16:(ncol(data.09)-1))]
names(data.09)

# Raw Data Set September 2019
raw.data.19 <- read.csv("C:/Users/alber/Desktop/Ryerson/6. Spring-Summer 2020/Datasets Original/pub0919.csv")
head(raw.data.19)
nrow(raw.data.19)
summary(raw.data.19$LFSSTAT)
sum(is.na(raw.data.19$LFSSTAT))
str(raw.data.19$LFSSTAT)

# Only Employed
data.19 <- as.data.frame(raw.data.19[raw.data.19$LFSSTAT < 3,])

# Only Public and Private Sector Employees
data.19 <- as.data.frame(data.19[data.19$COWMAIN < 3,])

# MATCH VALUES TO 2009

# CMA
sort(unique(data.19$CMA))
table(data.19$CMA)
data.19$CMA[data.19$CMA == 2] <- 10
data.19$CMA[data.19$CMA == 4] <- 2
data.19$CMA[data.19$CMA == 9] <- 30
data.19$CMA[data.19$CMA %in% c(1,3,5,6,7,8,0)] <- 4
data.19$CMA[data.19$CMA == 10] <- 1
data.19$CMA[data.19$CMA == 30] <- 3
sort(unique(data.19$CMA))
table(data.19$CMA)

# NAICS_21 TO NAICS_18 (2009)
sort(unique(data.19$NAICS_21))
table(data.19$NAICS_21)
data.19$NAICS_21[data.19$NAICS_21 %in% (2:4)] <- 2
data.19$NAICS_21[data.19$NAICS_21 == 5] <- 3
data.19$NAICS_21[data.19$NAICS_21 == 6] <- 4
data.19$NAICS_21[data.19$NAICS_21 == 7] <- 5
data.19$NAICS_21[data.19$NAICS_21 == 8] <- 6
data.19$NAICS_21[data.19$NAICS_21 == 9] <- 7
data.19$NAICS_21[data.19$NAICS_21 == 10] <- 8
data.19$NAICS_21[data.19$NAICS_21 == 11] <- 9
data.19$NAICS_21[data.19$NAICS_21 %in% (12:13)] <- 10
data.19$NAICS_21[data.19$NAICS_21 == 14] <- 11
data.19$NAICS_21[data.19$NAICS_21 == 15] <- 12
data.19$NAICS_21[data.19$NAICS_21 == 16] <- 13
data.19$NAICS_21[data.19$NAICS_21 == 17] <- 14
data.19$NAICS_21[data.19$NAICS_21 == 18] <- 15
data.19$NAICS_21[data.19$NAICS_21 == 19] <- 16
data.19$NAICS_21[data.19$NAICS_21 == 20] <- 17
data.19$NAICS_21[data.19$NAICS_21 == 21] <- 18
names(data.19)[names(data.19) == "NAICS_21"] <- "NAICS_18"
sort(unique(data.19$NAICS_18))
table(data.19$NAICS_18)


# Remove Unemployment and other Variables specified on main report
names(data.19)
data.19 <- subset(data.19, select = -c(AGE_6
                                       , EVERWORK
                                       , FTPTLAST
                                       , YABSENT
                                       , WKSAWAY
                                       , PAYAWAY
                                       , UHRSMAIN
                                       , AHRSMAIN
                                       , ATOTHRS
                                       , HRSAWAY
                                       , YAWAY
                                       , PAIDOT
                                       , UNPAIDOT
                                       , XTRAHRS
                                       , WHYPT
                                       , PREVTEN
                                       , DURUNEMP
                                       , FLOWUNEM
                                       , UNEMFTPT
                                       , WHYLEFTO
                                       , WHYLEFTN
                                       , DURJLESS
                                       , AVAILABL
                                       , LKPUBAG
                                       , LKEMPLOY
                                       , LKRELS
                                       , LKATADS
                                       , LKANSADS
                                       , LKOTHERN 
                                       , PRIORACT
                                       , YNOLOOK
                                       , TLOLOOK
                                       , FINALWT))                      

# Check both data sets have the same variables
names(data.19)
names(data.09)

# Merge the 2 data sets into one
data.all <- rbind(data.09, data.19)
str(data.all)
summary(data.all)

# MISSING VALUES
# SCHOOLN
data.all$SCHOOLN[is.na(data.all$SCHOOLN)] <- 4
summary(data.all$SCHOOLN)
#AGYOWNK
data.all$AGYOWNK[is.na(data.all$AGYOWNK)] <- 5
summary(data.all$AGYOWNK)

# FACTOR LABELS
# Labor Force Status
unique(data.all$LFSSTAT)
str(data.all$LFSSTAT)
data.all$LFSSTAT <- factor(data.all$LFSSTAT,
                           levels = c(1, 2),
                           labels = c("Employed, at work", "Employed, absent from work"))
str(data.all$LFSSTAT)

# Province
data.all$PROV <- factor(data.all$PROV,
                           levels = c(10, 11, 12, 13, 24, 35, 46, 47, 48, 59),
                           labels = c("NL"
                                      , "PEI"
                                      , "NS"
                                      , "NB"
                                      , "Qc"
                                      , "ON"
                                      , "MB"
                                      , "SK"
                                      , "AB"
                                      , "BC"))
str(data.all$PROV)

# CMA
sort(unique(data.all$CMA))
str(data.all$CMA)
data.all$CMA <- factor(data.all$CMA,
                           levels = c(1, 2, 3, 4),
                           labels = c("Montreal", "Toronto", "Vancouver", "Other"))
str(data.all$CMA)

# Age 12 groups
sort(unique(data.all$AGE_12))
str(data.all$AGE_12)
data.all$AGE_12 <- factor(data.all$AGE_12,
                       levels = c(1:12),
                       labels = c("15-19"
                                  , "20-24"
                                  , "25-29"
                                  , "30-34"
                                  , "35-39" 
                                  , "40-44" 
                                  , "45-49" 
                                  , "50-54"
                                  , "55-59" 
                                  , "60-64" 
                                  , "65-69" 
                                  , "70-over"),
                       ordered = TRUE)
str(data.all$AGE_12)

# Sex
sort(unique(data.all$SEX))
str(data.all$SEX)
data.all$SEX <- factor(data.all$SEX,
                          levels = c(1, 2),
                          labels = c("Male", "Female"))
str(data.all$SEX)

# Marital Status
sort(unique(data.all$MARSTAT))
str(data.all$MARSTAT)
data.all$MARSTAT <- factor(data.all$MARSTAT,
                       levels = c(1:6),
                       labels = c("Married"
                                  , "Living in common-law"
                                  , "Widowed"
                                  , "Separated"
                                  , "Divorced"
                                  , "Single, never married"))
str(data.all$MARSTAT)

# Education Attainment
sort(unique(data.all$EDUC))
str(data.all$EDUC)
data.all$EDUC <- factor(data.all$EDUC,
                       levels = c(0:6),
                       labels = c("0 to 8 years"
                                  , "Some high school"
                                  , "High school graduate"
                                  , "Some postsecondary"
                                  , "Postsecondary certificate or diploma"
                                  , "Bachelor's degree"
                                  , "Above bachelor's degree"),
                       ordered = TRUE)
str(data.all$EDUC)

# Single or Multiple Jobholder
sort(unique(data.all$MJH))
str(data.all$MJH)
data.all$MJH <- factor(data.all$MJH,
                           levels = c(1,2),
                           labels = c("Single jobholder", "Multiple jobholder"))

str(data.all$MJH)

# Class of Worker, Main Job
sort(unique(data.all$COWMAIN))
str(data.all$COWMAIN)
data.all$COWMAIN <- factor(data.all$COWMAIN,
                       levels = c(1,2),
                       labels = c("Public sector", "Private sector"))
str(data.all$COWMAIN)

# Immigrant Status
sort(unique(data.all$IMMIG))
str(data.all$IMMIG)
data.all$IMMIG <- factor(data.all$IMMIG,
                       levels = c(1,2,3),
                       labels = c("Immigrant, landed =< 10 years"
                                  , "Immigrant, landed > 10 years"
                                  , "Non-immigrant"))
str(data.all$IMMIG)
summary(data.all$IMMIG)

# Industry of main job
sort(unique(data.all$NAICS_18))
str(data.all$NAICS_18)
data.all$NAICS_18 <- factor(data.all$NAICS_18,
                       levels = c(1:18),
                       labels = c("Agriculture"
                                  , "Forestry, Fishing, Mining, Oil and Gas"
                                  , "Utilities"
                                  , "Construction"
                                  , "Manufacturing durables"
                                  , "Manufacturing non-durables"
                                  , "Wholesale Trade"
                                  , "Retail Trade"
                                  , "Transportation and Warehousing"
                                  , "Finance, Insurance, Real Estate and Leasing"
                                  , "Professional, Scientific and Technical Services"
                                  , "Management, Administrative and Other Support"
                                  , "Educational Services"
                                  , "Health Care and Social Assistance"
                                  , "Information, Culture and Recreation"
                                  , "Accommodation and Food Services"
                                  , "Other Services"
                                  , "Public Administration"))
str(data.all$NAICS_18)

# Occupation at main job (10 categories)
sort(unique(data.all$NOC_10))
str(data.all$NOC_10)
data.all$NOC_10 <- factor(data.all$NOC_10,
                            levels = c(1:10),
                            labels = c("Management"
                                       , "Business, finance and administration"
                                       , "Natural and applied sciences"
                                       , "Health"
                                       , "Education, law and social, community and government services"
                                       , "Art, culture, recreation and sport"
                                       , "Sales and service"
                                       , "Trades, transport and equipment operators"
                                       , "Natural resources and agriculture"
                                       , "Manufacturing and utilities"))
str(data.all$NOC_10)
summary(data.all$NOC_10)

# Occupation at main job (40 categories)
sort(unique(data.all$NOC_40))
str(data.all$NOC_40)
data.all$NOC_40 <- factor(data.all$NOC_40,
                          levels = c(1:40),
                          labels = c("Senior management"
                                     ,"Specialized middle management"
                                     ,"Middle management in retail and wholesale trade and customer services"
                                     ,"Middle management in trades, transportation, production and utilities"
                                     ,"Professional occupations in business and finance"
                                     ,"Administrative and financial supervisors and administrative occupations"
                                     ,"Finance, insurance and related business administrative occupations"
                                     ,"Office support"
                                     ,"Distribution, tracking and scheduling co-ordination"
                                     ,"Professional occupations in natural and applied sciences"
                                     ,"Technical occupations related to natural and applied sciences"
                                     ,"Professional occupations in nursing"
                                     ,"Professional occupations in health (except nursing)"
                                     ,"Technical occupations in health"
                                     ,"Assisting occupations in support of health services"
                                     ,"Professional occupations in education services"
                                     ,"Professional occupations in law and social, community and government services"
                                     ,"Paraprofessional occupations in legal, social, community and education services"
                                     ,"Front-line public protection services"
                                     ,"Care providers and educational, legal and public protection support"
                                     ,"Professional occupations in art and culture"
                                     ,"Technical occupations in art, culture, recreation and sport"
                                     ,"Retail sales supervisors and specialized sales"
                                     ,"Service supervisors and specialized service"
                                     ,"Sales representatives and salespersons - wholesale and retail trade"
                                     ,"Service representatives and other customer and personal services occupations"
                                     ,"Sales support"
                                     ,"Service support and other service occupations, n.e.c."
                                     ,"Industrial, electrical and construction trades"
                                     ,"Maintenance and equipment operation trades"
                                     ,"Other installers, repairers and servicers and material handlers"
                                     ,"Transport and heavy equipment operation and related maintenance occupations"
                                     ,"Trades helpers, construction labourers and related occupations"
                                     ,"Supervisors and technical occupations in natural resources, agriculture and related production"
                                     ,"Workers in natural resources, agriculture and related production"
                                     ,"Harvesting, landscaping and natural resources labourers"
                                     ,"Processing, manufacturing and utilities supervisors and central control operators"
                                     ,"Processing and manufacturing machine operators and related production workers"
                                     ,"Assemblers in manufacturing"
                                     ,"Labourers in processing, manufacturing and utilities"))
str(data.all$NOC_40)
summary(data.all$NOC_40)


# Full time or Part time, main job
sort(unique(data.all$FTPTMAIN))
str(data.all$FTPTMAIN)
data.all$FTPTMAIN <- factor(data.all$FTPTMAIN,
                           levels = c(1,2),
                           labels = c("Full-time", "Part-time"))
str(data.all$FTPTMAIN)
summary(data.all$FTPTMAIN)


# Union
sort(unique(data.all$UNION))
str(data.all$UNION)
data.all$UNION <- factor(data.all$UNION,
                         levels = c(1:3),
                         labels = c("Union member"
                                    , "Not a member but covered by a union contract"
                                    , "Non-unionized"))
str(data.all$UNION)
summary(data.all$UNION)

# Job permanency
sort(unique(data.all$PERMTEMP))
str(data.all$PERMTEMP)
data.all$PERMTEMP <- factor(data.all$PERMTEMP,
                         levels = c(1:4),
                         labels = c("Permanent"
                                    , "Temporary, seasonal"
                                    , "Temporary, term or contract"
                                    , "Temporary, casual or other"))
str(data.all$PERMTEMP)
summary(data.all$PERMTEMP)


# Establishment Size (number of employees)
sort(unique(data.all$ESTSIZE))
str(data.all$ESTSIZE)
data.all$ESTSIZE <- factor(data.all$ESTSIZE,
                            levels = c(1:4),
                            labels = c("<20"
                                       , "20-99"
                                       , "100-500"
                                       , ">500"),
                           ordered = TRUE)
str(data.all$ESTSIZE)
summary(data.all$ESTSIZE)

# Firm Size (number of employees)
sort(unique(data.all$FIRMSIZE))
str(data.all$FIRMSIZE)
data.all$FIRMSIZE <- factor(data.all$FIRMSIZE,
                           levels = c(1:4),
                           labels = c("<20"
                                      , "20-99"
                                      , "100-500"
                                      , ">500"),
                           ordered = TRUE)
str(data.all$FIRMSIZE)
summary(data.all$FIRMSIZE)

# Current Student Status
sort(unique(data.all$SCHOOLN))
str(data.all$SCHOOLN)
data.all$SCHOOLN <- factor(data.all$SCHOOLN,
                            levels = c(1:4),
                            labels = c("Non-student"
                                       , "Full-time student"
                                       , "Part-time student"
                                       , "Unknown"))
str(data.all$SCHOOLN)
summary(data.all$SCHOOLN)

# Type of Economic Family 
# Labels meaning:
# Ind: Unattached individual
# HWDENC: Husband-wife, dual earner couple, no children or none under 25
# HWDE17: Husband-wife, dual earner couple, youngest child 0 to 17
# HWDE24: Husband-wife, dual earner couple, youngest child 18 to 24
# HWSHNC: Husband-wife, single earner couple, husband employed, no children or none under 25
# HWSH17: Husband-wife, single earner couple, husband employed, youngest child 0 to 17
# HWSH24: Husband-wife, single earner couple, husband employed, youngest child 18 to 24
# HWSWNC: Husband-wife, single earner couple, wife employed, no children or none under 25
# HWSW17: Husband-wife, single earner couple, wife employed, youngest child 0 to 17
# HWSW24: Husband-wife, single earner couple, wife employed, youngest child 18 to 24
# HWNENC: Husband-wife, non-earner couple, no children or none under 25
# HWNE17: Husband-wife, non-earner couple, youngest child 0 to 17
# HWNE24: Husband-wife, non-earner couple, youngest child 18 to 24
# SPE17: Single-parent family, parent employed, youngest child 0 to 17
# SPE24: Single-parent family, parent employed, youngest child 18 to 24
# SPN17: Single-parent family, parent not employed, youngest child 0 to 17
# SPN24: Single-parent family, parent not employed, youngest child 18 to 24
# Other: Other families
sort(unique(data.all$EFAMTYPE))
str(data.all$EFAMTYPE)
data.all$EFAMTYPE <- factor(data.all$EFAMTYPE,
                           levels = c(1:18),
                           labels = c("Ind"
                                      , "HWDENC"
                                      , "HWDE17"
                                      , "HWDE24"
                                      , "HWSHNC"
                                      , "HWSH17"
                                      , "HWSH24"
                                      , "HWSWNC"
                                      , "HWSW17"
                                      , "HWSW24"
                                      , "HWNENC"
                                      , "HWNE17"
                                      , "HWNE24"
                                      , "SPE17"
                                      , "SPE24"
                                      , "SPN17"
                                      , "SPN24"
                                      , "Other"))
str(data.all$EFAMTYPE)
summary(data.all$EFAMTYPE)

# Age of Youngest Child (Years)
sort(unique(data.all$AGYOWNK))
str(data.all$AGYOWNK)
data.all$AGYOWNK <- factor(data.all$AGYOWNK,
                            levels = c(1:5),
                            labels = c(">6"
                                       , "6-12"
                                       , "13-17"
                                       , "18-24"
                                       , ">24 or no children"),
                            ordered = TRUE)
str(data.all$AGYOWNK)
summary(data.all$AGYOWNK)


str(data.all)

# NUMERIC VARIABLES REAL VALUES
data.all$UTOTHRS <- data.all$UTOTHRS/10
data.all$HRLYEARN <- data.all$HRLYEARN/100

# NUMERIC VARIABLES SUMMARY
num.vars <- c("UTOTHRS", "TENURE", "HRLYEARN")
summary(data.all[data.all$SURVYEAR == 2009, num.vars])
summary(data.all[data.all$SURVYEAR == 2019, num.vars])
sd(data.all$UTOTHRS[data.all$SURVYEAR == 2009])
sd(data.all$UTOTHRS[data.all$SURVYEAR == 2019])
sd(data.all$TENURE[data.all$SURVYEAR == 2009])
sd(data.all$TENURE[data.all$SURVYEAR == 2019])
sd(data.all$HRLYEARN[data.all$SURVYEAR == 2009])
sd(data.all$HRLYEARN[data.all$SURVYEAR == 2019])

# DESCRIPTIVE STATISTICS BY GENDER
summary(data.all[data.all$SURVYEAR == 2009 & data.all$SEX == "Male", num.vars])
summary(data.all[data.all$SURVYEAR == 2019 & data.all$SEX == "Male", num.vars])
summary(data.all[data.all$SURVYEAR == 2009 & data.all$SEX == "Female", num.vars])
summary(data.all[data.all$SURVYEAR == 2019 & data.all$SEX == "Female", num.vars])

apply(data.all[data.all$SURVYEAR == 2009 & data.all$SEX == "Male", num.vars], 2, sd)
apply(data.all[data.all$SURVYEAR == 2009 & data.all$SEX == "Female", num.vars], 2, sd)
apply(data.all[data.all$SURVYEAR == 2019 & data.all$SEX == "Male", num.vars], 2, sd)
apply(data.all[data.all$SURVYEAR == 2019 & data.all$SEX == "Female", num.vars], 2, sd)

# OUTLIERS IN NUMERIC VARIABLES
data.all[num.vars]

hours <- boxplot(UTOTHRS ~ SURVYEAR + SEX, data = data.all, main = "Usual hours worked")
hours[c("stats", "n", "names")]
table(hours$group)

tenure <- boxplot(TENURE ~ SURVYEAR + SEX, data = data.all, main = "Job tenure with current employer")
tenure[c("stats","n")]
table(tenure$group)

wage <- boxplot(HRLYEARN ~ SURVYEAR + SEX, data = data.all, main = "Usual hourly wages")
wage[c("stats","n")]
table(wage$group)

# DISTRIBUTION OF NUMERIC VARIABLES
table(data.all$SURVYEAR, data.all$SEX)
data.all.09.male <- data.all[data.all$SURVYEAR == 2009 & data.all$SEX == "Male",]
data.all.19.male <- data.all[data.all$SURVYEAR == 2019 & data.all$SEX == "Male",]
data.all.09.fem <- data.all[data.all$SURVYEAR == 2009 & data.all$SEX == "Female",]
data.all.19.fem <- data.all[data.all$SURVYEAR == 2019 & data.all$SEX == "Female",]


# Histogram
# Usual hours worked
par(mfrow=c(2, 2))
# Males
hist(data.all.09.male$UTOTHRS
     , xlim = c(0, 100)
     , ylim = c(0, 0.25)
     , breaks = 50
     , freq   = FALSE
     , col    = "slategray2"
     , main   = "Males 2009"
     , xlab   = "Usual hours worked")
curve(dnorm(x, mean = mean(data.all.09.male$UTOTHRS)
            , sd = sd(data.all.09.male$UTOTHRS))
            , col = "royalblue4"
            , lwd = 2
            , add = TRUE)

hist(data.all.19.male$UTOTHRS
     , xlim = c(0, 100)
     , ylim = c(0, 0.25)
     , breaks = 50
     , freq   = FALSE
     , col    = "slategray2"
     , main   = "Males 2019"
     , xlab   = "Usual hours worked")
curve(dnorm(x, mean = mean(data.all.19.male$UTOTHRS)
            , sd = sd(data.all.19.male$UTOTHRS))
      , col = "royalblue4"
      , lwd = 2
      , add = TRUE)
#Females
hist(data.all.09.fem$UTOTHRS
     , xlim = c(0, 100)
     , ylim = c(0, 0.25)
     , breaks = 50
     , freq   = FALSE
     , col    = "moccasin"
     , main   = "Females 2009"
     , xlab   = "Usual hours worked")
curve(dnorm(x, mean = mean(data.all.09.fem$UTOTHRS)
            , sd = sd(data.all.09.fem$UTOTHRS))
      , col = "darkorange2"
      , lwd = 2
      , add = TRUE)

hist(data.all.19.fem$UTOTHRS
     , xlim = c(0, 100)
     , ylim = c(0, 0.25)
     , breaks = 50
     , freq   = FALSE
     , col    = "moccasin"
     , main   = "Females 2019"
     , xlab   = "Usual hours worked")
curve(dnorm(x, mean = mean(data.all.19.fem$UTOTHRS)
            , sd = sd(data.all.19.fem$UTOTHRS))
      , col = "darkorange2"
      , lwd = 2
      , add = TRUE)

# Tenure
par(mfrow=c(2, 2))
# Males
hist(data.all.09.male$TENURE
     , xlim = c(0, 240)
     , ylim = c(0, 0.03)
     , breaks = 50
     , freq   = FALSE
     , col    = "slategray2"
     , main   = "Males 2009"
     , xlab   = "Tenure with current employer in months")
curve(dnorm(x, mean = mean(data.all.09.male$TENURE)
            , sd = sd(data.all.09.male$TENURE))
      , col = "royalblue4"
      , lwd = 2
      , add = TRUE)

hist(data.all.19.male$TENURE
     , xlim = c(0, 240)
     , ylim = c(0, 0.03)
     , breaks = 50
     , freq   = FALSE
     , col    = "slategray2"
     , main   = "Males 2019"
     , xlab   = "Tenure with current employer in months")
curve(dnorm(x, mean = mean(data.all.19.male$TENURE)
            , sd = sd(data.all.19.male$TENURE))
      , col = "royalblue4"
      , lwd = 2
      , add = TRUE)
# Females
hist(data.all.09.fem$TENURE
     , xlim = c(0, 240)
     , ylim = c(0, 0.03)
     , breaks = 50
     , freq   = FALSE
     , col    = "moccasin"
     , main   = "Females 2009"
     , xlab   = "Tenure with current employer in months")
curve(dnorm(x, mean = mean(data.all.09.fem$TENURE)
            , sd = sd(data.all.09.fem$TENURE))
      , col = "darkorange2"
      , lwd = 2
      , add = TRUE)

hist(data.all.19.fem$TENURE
     , xlim = c(0, 240)
     , ylim = c(0, 0.03)
     , breaks = 50
     , freq   = FALSE
     , col    = "moccasin"
     , main   = "Females 2019"
     , xlab   = "Tenure with current employer in months")
curve(dnorm(x, mean = mean(data.all.19.fem$TENURE)
            , sd = sd(data.all.19.fem$TENURE))
      , col = "darkorange2"
      , lwd = 2
      , add = TRUE)

# Wages
par(mfrow=c(2, 2))
# Males
hist(data.all.09.male$HRLYEARN
     , xlim = c(0, 120)
     , ylim = c(0, 0.085)
     , breaks = 50
     , freq   = FALSE
     , col    = "slategray2"
     , main   = "Males 2009"
     , xlab   = "Usual hourly wages")
curve(dnorm(x, mean = mean(data.all.09.male$HRLYEARN)
            , sd = sd(data.all.09.male$HRLYEARN))
      , col = "royalblue4"
      , lwd = 2
      , add = TRUE)

hist(data.all.19.male$HRLYEARN
     , xlim = c(0, 120)
     , ylim = c(0, 0.085)
     , breaks = 50
     , freq   = FALSE
     , col    = "slategray2"
     , main   = "Males 2019"
     , xlab   = "Usual hourly wages")
curve(dnorm(x, mean = mean(data.all.19.male$HRLYEARN)
            , sd = sd(data.all.19.male$HRLYEARN))
      , col = "royalblue4"
      , lwd = 2
      , add = TRUE)
# Females
hist(data.all.09.fem$HRLYEARN
     , xlim = c(0, 120)
     , ylim = c(0, 0.085)
     , breaks = 50
     , freq   = FALSE
     , col    = "moccasin"
     , main   = "Females 2009"
     , xlab   = "Usual hourly wages")
curve(dnorm(x, mean = mean(data.all.09.fem$HRLYEARN)
            , sd = sd(data.all.09.fem$HRLYEARN))
      , col = "darkorange2"
      , lwd = 2
      , add = TRUE)

hist(data.all.19.fem$HRLYEARN
     , xlim = c(0, 120)
     , ylim = c(0, 0.085)
     , breaks = 50
     , freq   = FALSE
     , col    = "moccasin"
     , main   = "Females 2019"
     , xlab   = "Usual hourly wages")
curve(dnorm(x, mean = mean(data.all.19.fem$HRLYEARN)
            , sd = sd(data.all.19.fem$HRLYEARN))
      , col = "darkorange2"
      , lwd = 2
      , add = TRUE)

# Q-Q Plot
install.packages("ggpubr")
library(ggpubr)
# Usual hours worked
ggqqplot(data.all.09.male$UTOTHRS, title = "Males 2009")
ggqqplot(data.all.19.male$UTOTHRS, title = "Males 2019")
ggqqplot(data.all.09.fem$UTOTHRS, title = "Females 2009")
ggqqplot(data.all.19.male$UTOTHRS, title = "Females 2019")
# Tenure
ggqqplot(data.all.09.male$TENURE, title = "Males 2009")
ggqqplot(data.all.19.male$TENURE, title = "Males 2019")
ggqqplot(data.all.09.fem$TENURE, title = "Females 2009")
ggqqplot(data.all.19.male$TENURE, title = "Females 2019")
# Wages
ggqqplot(data.all.09.male$HRLYEARN, title = "Males 2009")
ggqqplot(data.all.19.male$HRLYEARN, title = "Males 2019")
ggqqplot(data.all.09.fem$HRLYEARN, title = "Females 2009")
ggqqplot(data.all.19.male$HRLYEARN, title = "Females 2019")

# Kolmogorov-Smirnov
# Usual Hours Worked
ks.test(data.all.09.male$UTOTHRS, "pnorm", mean = mean(data.all.09.male$UTOTHRS), sd = sd(data.all.09.male$UTOTHRS))
ks.test(data.all.19.male$UTOTHRS, "pnorm", mean = mean(data.all.19.male$UTOTHRS), sd = sd(data.all.19.male$UTOTHRS))
ks.test(data.all.09.fem$UTOTHRS, "pnorm", mean = mean(data.all.09.fem$UTOTHRS), sd = sd(data.all.09.fem$UTOTHRS))
ks.test(data.all.19.fem$UTOTHRS, "pnorm", mean = mean(data.all.19.fem$UTOTHRS), sd = sd(data.all.19.fem$UTOTHRS))
# Tenure
ks.test(data.all.09.male$TENURE, "pnorm", mean = mean(data.all.09.male$TENURE), sd = sd(data.all.09.male$TENURE))
ks.test(data.all.19.male$TENURE, "pnorm", mean = mean(data.all.19.male$TENURE), sd = sd(data.all.19.male$TENURE))
ks.test(data.all.09.fem$TENURE, "pnorm", mean = mean(data.all.09.fem$TENURE), sd = sd(data.all.09.fem$TENURE))
ks.test(data.all.19.fem$TENURE, "pnorm", mean = mean(data.all.19.fem$TENURE), sd = sd(data.all.19.fem$TENURE))
# Wage
ks.test(data.all.09.male$HRLYEARN, "pnorm", mean = mean(data.all.09.male$HRLYEARN), sd = sd(data.all.09.male$HRLYEARN))
ks.test(data.all.19.male$HRLYEARN, "pnorm", mean = mean(data.all.19.male$HRLYEARN), sd = sd(data.all.19.male$HRLYEARN))
ks.test(data.all.09.fem$HRLYEARN, "pnorm", mean = mean(data.all.09.fem$HRLYEARN), sd = sd(data.all.09.fem$HRLYEARN))
ks.test(data.all.19.fem$HRLYEARN, "pnorm", mean = mean(data.all.19.fem$HRLYEARN), sd = sd(data.all.19.fem$HRLYEARN))



# CORRELATION NUMERIC AND ORDINAL VARIABLES
# Pearson (Only numeric)
cor(data.all[num.vars])
# Spearmen (Numeric and Ordinal)

ord.vars <- c("AGE_12", "EDUC", "ESTSIZE", "FIRMSIZE", "AGYOWNK")
ord <- lapply(data.all[ord.vars], as.numeric)
ord.num <- cbind(data.all[num.vars], ord)
cor(ord.num, method="spearman")

# TRAINING (70%) AND TESTING (30%) SETS
set.seed(1)
index <- sample(1:nrow(data.all), 0.7*nrow(data.all))
train <- data.all[index,]
test <- data.all[-index,]

train.09.male <- train[train$SURVYEAR == 2009 & train$SEX == "Male",]
train.19.male <- train[train$SURVYEAR == 2019 & train$SEX == "Male",]
train.09.fem <- train[train$SURVYEAR == 2009 & train$SEX == "Female",]
train.19.fem <- train[train$SURVYEAR == 2019 & train$SEX == "Female",]

#FEATURE SELECTION - Forward selection algorithm
install.packages('RCurl')
install.packages('MASS')
install.packages('leaps')
library(RCurl) # getURL 
library(MASS) # stepwise regression
library(leaps) # all subsets regression

full <- lm(HRLYEARN ~ LFSSTAT+PROV+CMA+AGE_12+SEX+MARSTAT+EDUC+MJH+COWMAIN
                     +NAICS_18+NOC_10+FTPTMAIN+UTOTHRS+TENURE+UNION+PERMTEMP
                     +ESTSIZE+FIRMSIZE+SCHOOLN+EFAMTYPE+AGYOWNK, data = train)
null <- lm(HRLYEARN ~ 1, data = train)
stepF <- stepAIC(null, scope=list(lower=null, upper=full), direction= "forward", trace=TRUE)
summary(stepF)
train.wage <- train$HRLYEARN
which(names(train)=="HRLYEARN")
names(train)
train.ind <- train[,c(-(1:3), -13, -16, -20)]
names(train.ind)
full <- lm(train.wage~train.ind[,1] + train.ind[,2])
null <- lm(train.wage~1)
stepF <- stepAIC(null, scope=list(lower=null, upper=full), direction= "forward", trace=TRUE)
summary(stepF)
# REGRESSION

model_mlr <- lm(HRLYEARN~ram+speed+screen+hd+ads, data=train) 
prediction <- predict(model_mlr, interval="prediction", newdata =test)