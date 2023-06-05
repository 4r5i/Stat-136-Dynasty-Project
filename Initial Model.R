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
i.mod.red <- lm(perc_votes_2019~perc_votes_2016
            +assets_ave_perc_chg
            #+liab_ave_perc_chg
            +rev_ave_perc_chg
            #+exp_ave_perc_chg
            #+pi_chg
            +co2_ave_perc_chg
            +hum_ave_perc_chg
            #+prec_ave_perc_chg
            #+precmax_ave_prec_chg
            +temp1_ave_perc_chg
            +maxtemp_ave_prec_chg
            #+factor(ruling_party)
            +factor(sex)
            +factor(case_inv)
            +factor(executive)
            +factor(legislative)
            ,data = data_mod)
summary(i.mod.red) #R^2_a = 0.3606
anova(i.mod.red) #MSE = 140.5

par(mfrow = c(2,2))
plot(i.mod.red)

#Diagnostic Analysis
ols_test_normality(i.mod.red)
ols_test_breusch_pagan(i.mod.red)
bptest(i.mod.red, studentize = FALSE)


#Detecting Outliers and Influential Observations
#Partial Regression Plot
ols_plot_added_variable(i.mod.red)

#Leverage
sum(ols_leverage(i.mod.red)>2*11/325)

#Reference value
2*(p/n) 

#Studentized Deleted Residuals
ols_plot_resid_stud(i.mod.red)

#Cook's Distance
ols_plot_cooksd_chart(i.mod.red)

#DFFITS
ols_plot_dffits(i.mod.red)

#DFBETAS
ols_plot_dfbetas(i.mod.red)

#Linearity
ols_plot_comp_plus_resid(i.mod.red)

#------------------------------------------------------------
