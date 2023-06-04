library(readxl)
library(olsrr)
library(tidyr)
library(car)
library(lmtest)

#Importing the entire data set
init.data <- read_xlsx("FinalDataSet_AllVar.xlsx")
str(init.data)

#---------------------------------------------------------
#Removing all observations with NAs
data_mod <- na.omit(init.data)
str(data_mod)
View(data_mod)

#Fitting initial model with 17 Predictors
i.mod <- lm(perc_votes_2019~perc_votes_2016
                +assets_ave_perc_chg
                +liab_ave_perc_chg
                +rev_ave_perc_chg
                +exp_ave_perc_chg
                +pi_chg
                +co2_ave_perc_chg
                +hum_ave_perc_chg
                +prec_ave_perc_chg
                +precmax_ave_prec_chg
                +temp1_ave_perc_chg
                +maxtemp_ave_prec_chg
                +factor(ruling_party)
                +factor(sex)
                +factor(case_inv)
                +factor(executive)
                +factor(legislative)
               ,data = data_mod)


#Model Adequacy
summary(i.mod) #R^2_a = 0.3522
anova(i.mod) #MSE = 142.3


#Variable Selection
ols_step_forward_p(i.mod) #R_a^2 = 0.3596
ols_step_backward_p(i.mod)
ols_step_both_p(i.mod)

#Reduced Model
i.mod.red <- lm(perc_votes_2019~
            +perc_votes_2016
            +hum_ave_perc_chg 
            +factor(executive)
            +co2_ave_perc_chg 
            +factor(legislative)
            +assets_ave_perc_chg
            +factor(case_inv)
            +rev_ave_perc_chg
            +factor(sex)  
            ,data = data_mod)
summary(i.mod.red)
anova(i.mod.red)
par(mfrow = c(2,2))
plot(i.mod.red)

#Diagnostic Analysis
ols_test_normality(i.mod.red)
ols_test_breusch_pagan(i.mod.red)
bptest(i.mod.red, studentize = FALSE)

#------------------------------------------------------------
