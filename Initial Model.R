library(readxl)
library(olsrr)
library(tidyr)
library(car)
library(lmtest)
library(pls)
library(dplyr)

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
#Fitting initial model with 18 Predictors
#--------------------------------------------------------
i.mod <- lm(perc_votes_2019~perc_votes_2016
                +assets_ave_perc_chg
                +liab_ave_perc_chg
                +rev_ave_perc_chg
                +exp_ave_perc_chg
                +pi_diff
                +co2_ave_perc_chg
                +hum_ave_perc_chg
                +prec_ave_perc_chg
                +precmax_ave_prec_chg
                +temp_ave_perc_chg
                +maxtemp_ave_prec_chg
                +total_dpwh
                +factor(ruling_party)
                +factor(sex)
                +factor(case_inv)
                +factor(executive)
                +factor(legislative)
               ,data = data_mod)

#Model Adequacy
summary(i.mod) #R^2_a = 0.3557
anova(i.mod) #MSE = 141.5

#Partial Regression and Partial Residual Plots
ols_plot_added_variable(i.mod)
ols_plot_comp_plus_resid(i.mod)

#--------------------------------------------------------
#Diagnostic Analysis
#--------------------------------------------------------
#Detecting Nonnormality and Heteroscedasticity
par(mfrow = c(2,2))
plot(i.mod)

ols_test_normality(i.mod) #Normal
ols_test_breusch_pagan(i.mod, rhs = TRUE) #Homoscedastic
bptest(i.mod, studentize = FALSE)

#Detecting Autocorrelation
durbinWatsonTest(i.mod) #No Autocorrelation

#Detecting Multicollinearity
as.matrix(vif(i.mod))

pairs(data_mod[, c(5:17)], lower.panel = NULL) 
cor_matrix <- cor(data_mod[, c(5:17)])
eigen(cor_matrix)

#Condition Number
max(eigen(cor_matrix)$values)/min(eigen(cor_matrix)$values) #84.56099

#Condition Indices
as.matrix(sqrt(max(eigen(cor_matrix)$values)/eigen(cor_matrix)$values)) #max = 9.195705

#------------------------------------------------------
#Outliers and Influential Observations
#------------------------------------------------------
which(ols_leverage(i.mod)>(2*19/326))
# 2   8   9  21  28  29  43  59 166 290 313 315

#1 NCR                TAGUIG CITY             
#2 ABRA               PILAR                   
#3 ABRA               SALLAPADAN              
#4 LANAO DEL SUR      BUADIPOSO-BUNTONG       
#5 LANAO DEL SUR      MAGUING                 
#6 LANAO DEL SUR      MAROGONG                
#7 MAGUINDANAO        SHARIFF SAYDONA MUSTAPHA
#8 Ilocos Norte       Bacarra                 
#9 Rizal              Antipolo City           
#10 Misamis Occidental Concepcion              
#11 Agusan del Sur     La Paz                  
#12 Agusan del Sur     San Luis   

which(!is.na(ols_plot_resid_stud(i.mod)[[1]][,5]))
# Null

which(!is.na(ols_plot_cooksd_chart(i.mod)[[1]][,5]))
# 2   8  11  29  43  46  47 166 168 202 243 286 309 311 316 319
which(!is.na(ols_plot_dffits(i.mod)[[1]][,5]))
# 2   8  11  29  43  46  47 166 168 202 243 286 309 311 316 319

ols_plot_dfbetas(i.mod)


i.mod_out1 <- lm(perc_votes_2019~perc_votes_2016
                +assets_ave_perc_chg
                +liab_ave_perc_chg
                +rev_ave_perc_chg
                +exp_ave_perc_chg
                +pi_diff
                +co2_ave_perc_chg
                +hum_ave_perc_chg
                +prec_ave_perc_chg
                +precmax_ave_prec_chg
                +temp_ave_perc_chg
                +maxtemp_ave_prec_chg
                +total_dpwh
                +factor(ruling_party)
                +factor(sex)
                +factor(case_inv)
                +factor(executive)
                +factor(legislative)
                ,data = data_mod[-c(2,8,9,21,28,29,43,59,166,290,313,315),])
data_mod[c(2,8,9,21,28,29,43,59,166,290,313,315), 4]
predict(i.mod_out1, data_mod[c(2,8,9,21,28,29,43,59,166,290,313,315), c(5:22)], interval = "prediction")

#perc_votes_2019
#1            59.3
#2            60.4
#3            81.5
#4            83.5
#5            86.5
#6            63.7
#7            84.6
#8            63.7
#9            82.3
#10           63.4
#11           57.3
#12           78.6

#fit        lwr       upr
#1   76.55370  50.163002 102.94440
#2  113.18874 -50.709782 277.08726
#3   71.07208  43.455335  98.68882
#4  134.73895  66.243920 203.23397
#5   61.43161  33.390197  89.47303
#6  358.46667 -20.010016 736.94335
#7  204.85882   8.710368 401.00728
#8   68.52072  43.039720  94.00173
#9   66.40808  39.969830  92.84633
#10  59.62180  34.290195  84.95341
#11  49.16868  23.601611  74.73576
#12  61.92967  36.232617  87.62673

#------------------------------------------------------
#All outlying observations can be removed. 
#Fitting a model with removed outliers and influential obs
#------------------------------------------------------
summary(i.mod_out1) #R^2_a = 0.3585
anova(i.mod_out1) #MSE = 141.6

#Detecting Nonnormality and Heteroscedascticity
plot(i.mod_out1)
ols_plot_added_variable(i.mod_out1)
ols_plot_comp_plus_resid(i.mod_out1)

ols_test_normality(i.mod_out1) #Normal
ols_test_breusch_pagan(i.mod_out1, rhs = TRUE) #Homoscedastic
bptest(i.mod_out1, studentize = FALSE)

#Detecting Autocorrelation
durbinWatsonTest(i.mod_out1) #No Autocorrelation

#Detecting Multicollinearity

pairs(data_mod[-c(2,8,9,21,28,29,43,59,166,290,313,315), c(5:17)], lower.panel = NULL) 
as.matrix(vif(i.mod_out1))
cor_matrix1 <- cor(data_mod[-c(2,8,9,21,28,29,43,59,166,290,313,315), c(5:17)])
eigen(cor_matrix1)

#Condition Number
(max(eigen(cor_matrix1)$values)/min(eigen(cor_matrix1)$values)) #97.00832

#Condition Indices
as.matrix(sqrt(max(eigen(cor_matrix1)$values)/eigen(cor_matrix1)$values)) #max = 9.849280


#------------------------------------------------------
#Variable Selection
#------------------------------------------------------
ols_step_forward_p(i.mod_out1) #R_a^2 = 0.3721
ols_step_backward_p(i.mod_out1) #R_a^2 = 0.3721
ols_step_both_aic(i.mod_out1) #R_a^2 = 0.37061

i.mod1_red <- lm(perc_votes_2019~perc_votes_2016
             #+assets_ave_perc_chg
             +liab_ave_perc_chg
             #+rev_ave_perc_chg
             +exp_ave_perc_chg
             +pi_diff
             +co2_ave_perc_chg
             +hum_ave_perc_chg
             #+prec_ave_perc_chg
             +precmax_ave_prec_chg
             #+temp_ave_perc_chg
             #+maxtemp_ave_prec_chg
             #+total_dpwh
             #+factor(ruling_party)
             +factor(sex)
             +factor(case_inv)
             +factor(executive)
             +factor(legislative)
             ,data = data_mod[-c(2,8,9,21,28,29,43,59,166,290,313,315),])

summary(i.mod1_red) #R_a^2 = 0.3721
anova(i.mod1_red) #MSE = 138.6

#Detecting Nonnormality and Heteroscedascticity
plot(i.mod1_red)
ols_plot_added_variable(i.mod1_red)

ols_test_normality(i.mod1_red) #Normal
ols_test_breusch_pagan(i.mod1_red, rhs = TRUE) #Homoscedastic
bptest(i.mod1_red, studentize = FALSE)

#Detecting Autocorrelation
durbinWatsonTest(i.mod1_red) #No Autocorrelation

#Detecting Multicollinearity
as.matrix(vif(i.mod1_red))
cor_matrix2 <- cor(data_mod[-c(2,8,9,21,28,29,43,59,166,290,313,315), c(5,7,9,10,11,14,16)])
eigen(cor_matrix2)


#Condition Number
max(eigen(cor_matrix2)$values)/min(eigen(cor_matrix2)$values) #3.742283

#Condition Indices
as.matrix(sqrt(max(eigen(cor_matrix2)$values)/eigen(cor_matrix2)$values)) #max = 1.934498
















#---------------------------------------------------------
#pi_diff
#---------------------------------------------------------

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
i.mod_pi <- lm(perc_votes_2019~perc_votes_2016
            +assets_ave_perc_chg
            +liab_ave_perc_chg
            +rev_ave_perc_chg
            +exp_ave_perc_chg
            +pi_diff
            +co2_ave_perc_chg
            +hum_ave_perc_chg
            +prec_ave_perc_chg
            +precmax_ave_prec_chg
            +temp_ave_perc_chg
            +maxtemp_ave_prec_chg
            +factor(ruling_party)
            +factor(sex)
            +factor(case_inv)
            +factor(executive)
            +factor(legislative)
            ,data = data_mod)

#Model Adequacy
summary(i.mod_pi) #R^2_a = 0.3575
anova(i.mod_pi) #MSE = 141.1

#Partial Regression and Partial Residual Plots
ols_plot_added_variable(i.mod_pi)
ols_plot_comp_plus_resid(i.mod_pi)

#--------------------------------------------------------
#Diagnostic Analysis
#--------------------------------------------------------
#Detecting Nonnormality and Heteroscedasticity
par(mfrow = c(2,2))
plot(i.mod_pi)

ols_test_normality(i.mod_pi) #Normal
ols_test_breusch_pagan(i.mod_pi, rhs = TRUE) #Homoscedastic
bptest(i.mod_pi, studentize = FALSE)

#Detecting Autocorrelation
durbinWatsonTest(i.mod_pi) #No Autocorrelation

#Detecting Multicollinearity
as.matrix(vif(i.mod_pi))

pairs(data_mod[, c(6:16)], lower.panel = NULL) 
cor_matrix_pi <- cor(data_mod[, c(6:10, 12:17, 24)])
eigen(cor_matrix_pi)

#Condition Number
max(eigen(cor_matrix_pi)$values)/min(eigen(cor_matrix_pi)$values) #82.79064

#Condition Indices
as.matrix(sqrt(max(eigen(cor_matrix_pi)$values)/eigen(cor_matrix_pi)$values)) #max = 9.098936

#------------------------------------------------------
#Outliers and Influential Observations
#------------------------------------------------------
which(ols_leverage(i.mod_pi)>(2*18/326))
# 8   9  21  28  29  43  47 290 313 315 322


which(!is.na(ols_plot_resid_stud(i.mod_pi)[[1]][,5]))
# null


i.mod_pi.out1 <- lm(perc_votes_2019~perc_votes_2016
                 +assets_ave_perc_chg
                 +liab_ave_perc_chg
                 +rev_ave_perc_chg
                 +exp_ave_perc_chg
                 +pi_diff
                 +co2_ave_perc_chg
                 +hum_ave_perc_chg
                 +prec_ave_perc_chg
                 +precmax_ave_prec_chg
                 +temp_ave_perc_chg
                 +maxtemp_ave_prec_chg
                 +factor(ruling_party)
                 +factor(sex)
                 +factor(case_inv)
                 +factor(executive)
                 +factor(legislative)
                 ,data = data_mod[-c(8,9,21,28,29,43,47,290,313,315,322),])
data_mod[c(8,9,21,28,29,43,47,290,313,315,322), 5]
predict(i.mod_pi.out1, data_mod[c(8,9,21,28,29,43,47,290,313,315,322), c(6:10, 12:22, 24)], interval = "prediction")


#------------------------------------------------------
#Fitting a model with removed outliers and influential obs
#------------------------------------------------------
summary(i.mod_pi.out1) #R^2_a = 0.3466
anova(i.mod_pi.out1) #MSE = 142.0


#------------------------------------------------------
#Variable Selection
#------------------------------------------------------
ols_step_forward_p(i.mod_pi.out1) #R_a^2 = 0.3586
ols_step_backward_p(i.mod_pi.out1) #R_a^2 = 0.3586
ols_step_both_p(i.mod_pi.out1) #R_a^2 = 0.3560

i.mod_pi.red <- lm(perc_votes_2019~perc_votes_2016
                    #+assets_ave_perc_chg
                    #+liab_ave_perc_chg
                    #+rev_ave_perc_chg
                    +exp_ave_perc_chg
                    +pi_diff
                    +co2_ave_perc_chg
                    +hum_ave_perc_chg
                    #+prec_ave_perc_chg
                    #+precmax_ave_prec_chg
                    #+temp_ave_perc_chg
                    #+maxtemp_ave_prec_chg
                    #+factor(ruling_party)
                    +factor(sex)
                    +factor(case_inv)
                    +factor(executive)
                    +factor(legislative)
                    ,data = data_mod[-c(8,9,21,28,29,43,47,290,313,315,322),])
summary(i.mod_pi.red)
anova(i.mod_pi.red)


#--------------------------------------------------------
#Diagnostic Analysis
#--------------------------------------------------------
#Detecting Nonnormality and Heteroscedasticity
par(mfrow = c(2,2))
plot(i.mod_pi.red)

ols_test_normality(i.mod_pi.red) #Normal
ols_test_breusch_pagan(i.mod_pi.red, rhs = TRUE) #Homoscedastic
bptest(i.mod_pi.red, studentize = FALSE)

#Detecting Autocorrelation
durbinWatsonTest(i.mod_pi.red) #No Autocorrelation

#Detecting Multicollinearity
as.matrix(vif(i.mod_pi.red))

pairs(data_mod[, c(6:16)], lower.panel = NULL) 
cor_matrix_pi <- cor(data_mod[, c(6:10, 12:17, 24)])
eigen(cor_matrix_pi)

#Condition Number
max(eigen(cor_matrix_pi)$values)/min(eigen(cor_matrix_pi)$values) #82.79064

#Condition Indices
as.matrix(sqrt(max(eigen(cor_matrix_pi)$values)/eigen(cor_matrix_pi)$values)) #max = 9.098936







#------------------------------------------------------
#NO TEMP
#------------------------------------------------------
i.mod_noTemp <- lm(perc_votes_2019~perc_votes_2016
            +assets_ave_perc_chg
            +liab_ave_perc_chg
            +rev_ave_perc_chg
            +exp_ave_perc_chg
            +pi_chg
            +co2_ave_perc_chg
            +hum_ave_perc_chg
            +prec_ave_perc_chg
            +precmax_ave_prec_chg
            #+temp_ave_perc_chg
            #+maxtemp_ave_prec_chg
            +factor(ruling_party)
            +factor(sex)
            +factor(case_inv)
            +factor(executive)
            +factor(legislative)
            ,data = data_mod)


#Model Adequacy
summary(i.mod_noTemp) #R^2_a = 0.3527
anova(i.mod_noTemp) #MSE = 142.2

#Partial Regression and Partial Residual Plots
ols_plot_added_variable(i.mod_noTemp)
ols_plot_comp_plus_resid(i.mod_noTemp)

#--------------------------------------------------------
#Diagnostic Analysis
#--------------------------------------------------------
#Detecting Nonnormality and Heteroscedasticity
par(mfrow = c(2,2))
plot(i.mod_noTemp)

ols_test_normality(i.mod_noTemp) #Normal
ols_test_breusch_pagan(i.mod_noTemp, rhs = TRUE) #Homoscedastic
bptest(i.mod_noTemp, studentize = FALSE)

#Detecting Autocorrelation
durbinWatsonTest(i.mod_noTemp) #No Autocorrelation

#Detecting Multicollinearity
pairs(data_mod[, c(6:16)], lower.panel = NULL) 
as.matrix(vif(i.mod_noTemp))
cor_matrix_noTemp <- cor(data_mod[, c(6:13, 15, 17)])
eigen(cor_matrix_noTemp)

#Condition Number
(max(eigen(cor_matrix_noTemp)$values)/min(eigen(cor_matrix_noTemp)$values)) #14.52325

#Condition Indices
as.matrix(sqrt(max(eigen(cor_matrix_noTemp)$values)/eigen(cor_matrix_noTemp)$values)) #max = 3.763925

#------------------------------------------------------
#Outliers and Influential Observations
#------------------------------------------------------
which(ols_leverage(i.mod_noTemp)>(2*16/326))
# 6   8   9  21  28  29  43  47 148 297 322
which(!is.na(ols_plot_resid_stud(i.mod_noTemp)[[1]][,5]))
# 319

which(!is.na(ols_plot_cooksd_chart(i.mod_noTemp)[[1]][,5]))
# 11  28  29  39  43  46  47  55  56  66 101 173 243 309 319
which(!is.na(ols_plot_dffits(i.mod)[[1]][,5]))
# 11  29  43  46  47  56  66 101 168 202 243 309 311 316 319

i.mod_out <- lm(perc_votes_2019~perc_votes_2016
                +assets_ave_perc_chg
                +liab_ave_perc_chg
                +rev_ave_perc_chg
                +exp_ave_perc_chg
                +pi_chg
                +co2_ave_perc_chg
                +hum_ave_perc_chg
                +prec_ave_perc_chg
                +precmax_ave_prec_chg
                #+temp_ave_perc_chg
                #+maxtemp_ave_prec_chg
                +factor(ruling_party)
                +factor(sex)
                +factor(case_inv)
                +factor(executive)
                +factor(legislative)
                ,data = data_mod[-c(6,8,9,21,28,29,43,47,148,297,322),])
data_mod[c(6,8,9,21,28,29,43,47,148,297,322), 5]
predict(i.mod_out, data_mod[c(6,8,9,21,28,29,43,47,148,297,322), c(6:13,15,17:22)], interval = "prediction")


#perc_votes_2019
#1            62.7
#2            60.4
#3            81.5
#4            83.5
#5            86.5
#6            63.7
#7            84.6
#8            96.8
#9            66.8
#10           80.9
#11           52.5
#fit       lwr       upr
#1   68.87290  42.60267  95.14314
#2  110.57355 -52.30308 273.45018
#3   71.81471  43.25908 100.37034
#4  134.96448  56.82669 213.10226
#5   61.50082  33.05928  89.94235
#6  358.42780 -70.53540 787.39101
#7  170.95008 -22.53835 364.43851
#8   84.79906  55.65894 113.93917
#9   54.21730  28.95265  79.48195
#10  78.01145  52.50462 103.51828
#11  61.17140  36.05570  86.28710


summary(i.mod_out) #R^2_a = 0.3414
anova(i.mod_out) #MSE = 143.0

#Detecting Nonnormality and Heteroscedascticity
plot(i.mod_out)
ols_plot_added_variable(i.mod_out)
ols_plot_comp_plus_resid(i.mod_out)

ols_test_normality(i.mod_out) #Normal
ols_test_breusch_pagan(i.mod_out, rhs = TRUE) #Homoscedastic
bptest(i.mod_out, studentize = FALSE)

#Detecting Autocorrelation
durbinWatsonTest(i.mod_out) #No Autocorrelation

#Detecting Multicollinearity

pairs(data_mod[-c(6,8,9,21,28,29,43,47,148,297,322), c(6:13,15,17:22)], lower.panel = NULL) 
as.matrix(vif(i.mod_out))
cor_matrix.out <- cor(data_mod[-c(6,8,9,21,28,29,43,47,148,297,322), c(6:13,15,17)])
eigen(cor_matrix.out)

#Condition Number
(max(eigen(cor_matrix.out)$values)/min(eigen(cor_matrix.out)$values)) #7.069268

#Condition Indices
as.matrix(sqrt(max(eigen(cor_matrix.out)$values)/eigen(cor_matrix.out)$values)) #max = 2.658810



#------------------------------------------------------
#Variable Selection
#------------------------------------------------------














#--------------------------------------------------------
#-----With DPWH-------------------------------------
i <- lm(perc_votes_2019~perc_votes_2016
            +assets_ave_perc_chg
            +liab_ave_perc_chg
            +rev_ave_perc_chg
            +exp_ave_perc_chg
            +pi_diff
            +co2_ave_perc_chg
            +hum_ave_perc_chg
            +prec_ave_perc_chg
            +precmax_ave_prec_chg
            +temp_ave_perc_chg
            +maxtemp_ave_prec_chg
            +total_dpwh
            +factor(ruling_party)
            +factor(sex)
            +factor(case_inv)
            +factor(executive)
            +factor(legislative)
            ,data = data_mod)
summary(i)
anova(i)
ols_plot_added_variable(i)

ols_step_forward_p(i)
ols_step_backward_p(i)

#--------------------------------------------------------
#Diagnostic Analysis
#--------------------------------------------------------
#Detecting Nonnormality and Heteroscedasticity
par(mfrow = c(2,2))
plot(i)

ols_test_normality(i) #Normal
ols_test_breusch_pagan(i, rhs = TRUE) #Homoscedastic
bptest(i, studentize = FALSE)

#Detecting Autocorrelation
durbinWatsonTest(i) #No Autocorrelation

#Detecting Multicollinearity
as.matrix(vif(i))

pairs(data_mod[, c(6:16)], lower.panel = NULL) 
cor_matrix <- cor(data_mod[, c(6:17)])
eigen(cor_matrix)

#Condition Number
max(eigen(cor_matrix)$values)/min(eigen(cor_matrix)$values) #82.73437

#Condition Indices
as.matrix(sqrt(max(eigen(cor_matrix)$values)/eigen(cor_matrix)$values)) #max = 7.826570

#------------------------------------------------------
#Outliers and Influential Observations
#------------------------------------------------------
which(ols_leverage(i)>(2*19/326))
# 2   8   9  21  28  29  43  59 166 290 313 315
which(!is.na(ols_plot_resid_stud(i)[[1]][,5]))
# Null

which(!is.na(ols_plot_cooksd_chart(i)[[1]][,5]))
# 11  28  29  39  43  46  47  55  56  66 101 173 243 309 319
which(!is.na(ols_plot_dffits(i.mod)[[1]][,5]))
# 11  29  43  46  47  56  66 101 168 202 243 309 311 316 319

i.out <- lm(perc_votes_2019~perc_votes_2016
                +assets_ave_perc_chg
                +liab_ave_perc_chg
                +rev_ave_perc_chg
                +exp_ave_perc_chg
                +pi_diff
                +co2_ave_perc_chg
                +hum_ave_perc_chg
                +prec_ave_perc_chg
                +precmax_ave_prec_chg
                +temp_ave_perc_chg
                +maxtemp_ave_prec_chg
                +total_dpwh
                +factor(ruling_party)
                +factor(sex)
                +factor(case_inv)
                +factor(executive)
                +factor(legislative)
                ,data = data_mod[-c(2,8,9,21,28,29,43,59,166,290,313,315),])
data_mod[c(2,8,9,21,28,29,43,59,166,290,313,315), 5]
predict(i.out, data_mod[c(2,8,9,21,28,29,43,59,166,290,313,315), c(6:10,12:24)], interval = "prediction")

summary(i.out)
anova(i.out)


#Detecting Nonnormality and Heteroscedasticity
par(mfrow = c(2,2))
plot(i.out)

ols_test_normality(i) #Normal
ols_test_breusch_pagan(i, rhs = TRUE) #Homoscedastic
bptest(i, studentize = FALSE)

#Detecting Autocorrelation
durbinWatsonTest(i) #No Autocorrelation

#Detecting Multicollinearity
as.matrix(vif(i.out))

pairs(data_mod[, c(6:16)], lower.panel = NULL) 
cor_matrix <- cor(data_mod[, c(6:10, 12:17,23,24)])
eigen(cor_matrix)

#Condition Number
max(eigen(cor_matrix)$values)/min(eigen(cor_matrix)$values) #84.56099

#Condition Indices
as.matrix(sqrt(max(eigen(cor_matrix)$values)/eigen(cor_matrix)$values)) #max = 9.195705



ols_step_forward_p(i.out)
ols_step_backward_p(i.out)
ols_step_both_p(i.out)
i.out_red <- lm(perc_votes_2019~perc_votes_2016
            #+assets_ave_perc_chg
            +liab_ave_perc_chg
            #+rev_ave_perc_chg
            +exp_ave_perc_chg
            +pi_diff
            +co2_ave_perc_chg
            +hum_ave_perc_chg
            #+prec_ave_perc_chg
            +precmax_ave_prec_chg
            #+temp_ave_perc_chg
            #+maxtemp_ave_prec_chg
            #+total_dpwh
            #+factor(ruling_party)
            +factor(sex)
            +factor(case_inv)
            #+factor(executive)
            +factor(legislative)
            ,data = data_mod[-c(2,8,9,21,28,29,43,59,166,290,313,315),])
summary(i.out_red)
anova(i.out_red)
ols_plot_added_variable(i.out_red)























#--------------------------------------------------------
#-----PLAYGRUND char-------------------------------------
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
            +temp_ave_perc_chg
            +maxtemp_ave_prec_chg
            +factor(ruling_party)
            +factor(sex)
            +factor(case_inv)
            +factor(executive)
            +factor(legislative)
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
