library(readxl)
library(olsrr)
library(tidyr)
library(car)
library(lmtest)
library(pls)

#Importing the entire data set
init.data <- read_xlsx("FinalDataSet_AllVar.xlsx")
str(init.data)

#---------------------------------------------------------
#Removing all observations with NAs
#---------------------------------------------------------
data_mod <- na.omit(init.data)
str(data_mod)
View(data_mod)

#--------------------------------------------------------
#Fitting initial model with 17 Predictors
#--------------------------------------------------------

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
summary(i.mod) #R^2_a = 0.3527
anova(i.mod) #MSE = 142.2

#Partial Regression and Partial Residual Plots
ols_plot_added_variable(i.mod)
ols_plot_comp_plus_resid(i.mod)

#Detecting Nonnormality and Heteroscedasticity
par(mfrow = c(2,2))
plot(i.mod)

ols_test_normality(i.mod) #Normal
ols_test_breusch_pagan(i.mod, rhs = TRUE) #Homoscedastic
bptest(i.mod, studentize = FALSE)

#Detecting Autocorrelation
durbinWatsonTest(i.mod) #No Autocorrelation

#Detecting Multicollinearity
pairs(data_mod[, c(6:13,15,16,22,23)], lower.panel = NULL) #temp1, maxtemp, and hum have pairwise correlation
as.matrix(vif(i.mod))
cor_matrix <- cor(data_mod[, c(6:13,15,16,22,23)])
eigen(cor_matrix)

#Condition Number
sqrt(max(eigen(cor_matrix)$values)/min(eigen(cor_matrix)$values)) # 10.0174

#Condition Indices
as.matrix(sqrt(max(eigen(cor_matrix)$values)/eigen(cor_matrix)$values)) #max = 10.017401

#PCR
pcr.mod <- pcr(perc_votes_2019~perc_votes_2016
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
            #+factor(ruling_party)
            #+factor(sex)
            #+factor(case_inv)
            #+factor(executive)
            #+factor(legislative)
            ,data = data_mod
            ,scale = TRUE
            ,validation = "CV")
par(mfrow=c(1,3))
validationplot(pcr.mod)
validationplot(pcr.mod, val.type = "MSEP")
validationplot(pcr.mod, val.type = "R2") #9 Components

pcr.mod$projection
pcr.mod$loadings

#Data Matrix and condition number & index
library(dplyr)
col1 <- data.frame(rep(1, 326))
data.mat <- data.frame(data_mod[,c(6:13,15,16,22,23)])
str(data.mat)
data.mat %>% mutate(col_1 = (col1))  %>% relocate(col_1, .before = perc_votes_2016) -> data_matrix
eigen((t(data_matrix)) %*% as.matrix(data_matrix)) -> eigensyst
condition_num <- max(eigensyst$values)/min(eigensyst$values)
condition_num
condition_ind <- as.matrix(sqrt(max(eigensyst$values)/eigensyst$values))
condition_ind

#PCR
library(pls)
pcr.mod <- pcr(perc_votes_2019~perc_votes_2016
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
                       #+factor(ruling_party)
                       #+factor(sex)
                       #+factor(case_inv)
                       #+factor(executive)
                       #+factor(legislative)
                       ,data = data_mod
                       ,scale = TRUE
                       ,validation = "CV")
par(mfrow = c(1,3))
validationplot(pcr.mod)
validationplot(pcr.mod, val.type = "MSEP")
validationplot(pcr.mod, val.type = "R2")
pcr.mod$loadings
PCs <- pcr.mod$scores
pc_mod <- lm(data_mod$perc_votes_2019~PCs[,-c(10,11,12)])
summary(pc_mod)
anova(pc_mod)
vif(pc_mod)



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
            #+temp1_ave_perc_chg
            #+maxtemp_ave_prec_chg
            #+factor(ruling_party)
            #+factor(sex)
            +factor(case_inv)
            +factor(executive)
            +factor(legislative)
            ,data = data_mod)
summary(i.mod.red) #R^2_a = 0.3573
anova(i.mod.red) #MSE = 141.2
ols_plot_added_variable(i.mod.red)
par(mfrow = c(2,2))
plot(i.mod.red)

#Diagnostic Analysis
ols_test_normality(i.mod.red)
ols_test_breusch_pagan(i.mod.red, rhs = TRUE)
bptest(i.mod.red, studentize = FALSE)


#Detecting Outliers and Influential Observations
#Partial Regression Plot
ols_plot_added_variable(i.mod.red)

#Leverage
which(ols_leverage(i.mod.red)>2*8/326)

#Reference value
2*(p/n) 

View(data_mod[which(ols_leverage(i.mod.red)>2*8/326),c(2,4)])

i.mod.red.out <- lm(perc_votes_2019~perc_votes_2016
                                 +assets_ave_perc_chg
                                 #+liab_ave_perc_chg
                                 +rev_ave_perc_chg
                                 #+exp_ave_perc_chg
                                 #+pi_chg
                                 +co2_ave_perc_chg
                                 +hum_ave_perc_chg
                                 #+prec_ave_perc_chg
                                 #+precmax_ave_prec_chg
                                 #+temp1_ave_perc_chg
                                 #+maxtemp_ave_prec_chg
                                 #+factor(ruling_party)
                                 +factor(sex)
                                 +factor(case_inv)
                                 +factor(executive)
                                 +factor(legislative)
                                 ,data = data_mod)

predict(i.mod.red.out,data_mod[c(6,8,9,21,28,36,38,43,44,47,
                                 209,211,297,301,311,313,315,316),c(6,7,9,12,16,17,18,19,20,21,22,23)], interval = "prediction")


#Studentized Deleted Residuals
ols_plot_resid_stud(i.mod.red)
str(data_mod)
data_mod[319,5]
#Cook's Distance
ols_plot_cooksd_chart(i.mod.red)

#DFFITS
ols_plot_dffits(i.mod.red)

#DFBETAS
ols_plot_dfbetas(i.mod.red)

#Linearity
ols_plot_comp_plus_resid(i.mod.red)
#Multicollinearity
str(data_mod)

round(cor(data_mod[,c(6,7,9,12,23)]),3)
pairs(data_mod[,c(6,7,9,12,23)], lower.panel = NULL)



#------------------------------------------------------------
