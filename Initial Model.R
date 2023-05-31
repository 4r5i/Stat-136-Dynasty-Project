library(readxl)
library(olsrr)

#Importing the entire data set
init.data <- read_xlsx("FinalDataSet_AllVar.xlsx")
str(init.data)

#Getting only the numeric data needed for modelling
init.data_mod <- init.data
str(init.data_mod)

#Making all Columns numeric
init.data_mod$assets2018 <- as.numeric(init.data_mod$assets2018)
init.data_mod$liab2018 <- as.numeric(init.data_mod$liab2018)
init.data_mod$rev2018 <- as.numeric(init.data_mod$rev2018)
init.data_mod$exp2018 <- as.numeric(init.data_mod$exp2018)
str(init.data_mod)
View(init.data_mod)

#Removing all observations with NAs
data_mod <- na.omit(init.data_mod)
str(data_mod)
View(data_mod)

#Fitting initial model
i.mod <- lm(perc_votes_2019~.-region-province-geog_level-city_municip-assets2018-liab2018-rev2018-exp2018 
              -temp_ave_perc_chg-maxtemp_ave_prec_chg, data = data_mod)
summary(i.mod)
plot(i.mod)
round(cor(data_mod[,-c(1,2,3,4,5,7,9,11,13)]),2)

#Variable Selection
plot(ols_step_forward_p(i.mod))


plot(data_mod$assets_ave_perc_chg,data_mod$perc_votes_2019)
par(mfrow=c(2,2))
plot(i.mod)
View(data_mod[c(171, 166,11),])

