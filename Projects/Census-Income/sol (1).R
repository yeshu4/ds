
#Read csv file

census <- read.csv("C:/Users/Intellipaat-Ashwin/Desktop/data cleaning/census-income.csv")

View(census)

str(census)

#Renaming columns

colnames(census)[colnames(census)=="age"]<-"Age"

library(data.table)
setnames(census, "workclass", "Employment-Type")

library(plyr)
census<-rename(census,c('fnlwgt'='Final-Weight'))

library(gdata)
census <- rename.vars(census, from = "education", to = "Education")

setnames(census, "education.num", "Education-Number")
setnames(census, "marital.status", "Marital-Status")
setnames(census, "occupation", "Occupation")
setnames(census, "relationship", "Relationship")
setnames(census, "race", "Race")
setnames(census, "sex", "Sex")
setnames(census, "capital.gain", "Capital-Gain")
setnames(census, "capital.loss", "Capital-Loss")
setnames(census, "hours.per.week", "Hours-Per-Week")
setnames(census, "native.country", "Native-Country")
setnames(census, "X", "Income")

#Changing the levels
str(census)
table(census$`Employment-Type`)
as.character(census$`Employment-Type`) -> census$`Employment-Type`

census$`Employment-Type`[census$`Employment-Type`==" State-gov"] <- "Government"
census$`Employment-Type`[census$`Employment-Type`==" Federal-gov"] <- "Government"
census$`Employment-Type`[census$`Employment-Type`==" Local-gov"] <- "Government"

census$`Employment-Type`[census$`Employment-Type`==" Self-emp-inc"] <- "Self Employed"
census$`Employment-Type`[census$`Employment-Type`==" Self-emp-not-inc"] <- "Self Employed"


#Changing the levels
table(census$`Marital-Status`)
as.character(census$`Marital-Status`) -> census$`Marital-Status`

census$`Marital-Status`[census$`Marital-Status`== " Married-AF-spouse"] <- "Married"
census$`Marital-Status`[census$`Marital-Status`== " Married-spouse-absent"] <- "Married"
census$`Marital-Status`[census$`Marital-Status`== " Married-civ-spouse"] <- "Married"

#Changing the levels
table(census$`Native-Country`)
as.character(census$`Native-Country`) -> census$`Native-Country`

census$`Native-Country`[census$`Native-Country`== " England"] <- "Europe"
census$`Native-Country`[census$`Native-Country`== " France"] <- "Europe"
census$`Native-Country`[census$`Native-Country`== " Germany"] <- "Europe"
census$`Native-Country`[census$`Native-Country`== " Greece"] <- "Europe"
census$`Native-Country`[census$`Native-Country`== " Ireland"] <- "Europe"
census$`Native-Country`[census$`Native-Country`== " Scotland"] <- "Europe"
census$`Native-Country`[census$`Native-Country`== " Portugal"] <- "Europe"
census$`Native-Country`[census$`Native-Country`== " Italy"] <- "Europe"


census$`Native-Country`[census$`Native-Country`== " India"] <- "Asia"
census$`Native-Country`[census$`Native-Country`== " Vietnam"] <- "Asia"
census$`Native-Country`[census$`Native-Country`== " Taiwan"] <- "Asia"
census$`Native-Country`[census$`Native-Country`== " Japan"] <- "Asia"
census$`Native-Country`[census$`Native-Country`== " Thailand"] <- "Asia"
census$`Native-Country`[census$`Native-Country`== " Iran"] <- "Asia"
census$`Native-Country`[census$`Native-Country`== " China"] <- "Asia"


#Replacing ' ?' with NA

census[census == " ?"] <- NA


#creating function to count number of NA values
na_count <-function (x) sapply(x, function(y) sum(is.na(y)))

na_count(census)

#imputation
census -> census1
census1 <- na.omit(census1)

na_count(census1)

#------------------------------------------------------------------------------------------------------------------------
