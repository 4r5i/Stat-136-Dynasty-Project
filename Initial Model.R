library(readxl)
library(olsrr)
library(tidyr)
library(car)
library(lmtest)
library(pls)
library(dplyr)
library(psych)
library(ggplot2)
library(corrplot)
library(RColorBrewer)

#Importing the entire data set
init.data <- read_xlsx("FinalDataSet_AllVar.xlsx")
str(init.data)

#---------------------------------------------------------
#Removing all observations with NAs
#---------------------------------------------------------
data_mod <- na.omit(init.data)
str(data_mod)
View(data_mod)

#---------------------------------------------------------
#Preliminary Analysis
#---------------------------------------------------------
describe(data_mod[,4:17])

#Dummy Variable Charts
df1 <- data.frame(Make=c("YES", "NO"),
                Cnt=c(sum(data_mod$ruling_party=="YES"), sum(data_mod$ruling_party=="NO")))
df1 %>%
  group_by(Make) %>%
  summarise(volume = sum(Cnt)) %>%
  mutate(share=volume/sum(volume)) %>%
  ggplot(aes(x="", y= share, fill=reorder(Make, volume))) +
  geom_col() +
  geom_text(aes(label = scales::percent(round(share,3))), position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") + 
  theme_void()+ scale_fill_discrete(name = "Ruling Party")

df2 <- data.frame(Make=c("YES", "NO"),
                 Cnt=c(sum(data_mod$case_inv=="YES"), sum(data_mod$case_inv=="NO")))
df2 %>%
  group_by(Make) %>%
  summarise(volume = sum(Cnt)) %>%
  mutate(share=volume/sum(volume)) %>%
  ggplot(aes(x="", y= share, fill=reorder(Make, volume))) +
  geom_col() +
  geom_text(aes(label = scales::percent(round(share,3))), position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") + 
  theme_void()+ scale_fill_discrete(name = "Case Involvement")

df3 <- data.frame(Make=c("YES", "NO"),
                  Cnt=c(sum(data_mod$executive=="YES"), sum(data_mod$executive=="NO")))
df3 %>%
  group_by(Make) %>%
  summarise(volume = sum(Cnt)) %>%
  mutate(share=volume/sum(volume)) %>%
  ggplot(aes(x="", y= share, fill=reorder(Make, volume))) +
  geom_col() +
  geom_text(aes(label = scales::percent(round(share,3))), position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") + 
  theme_void()+ scale_fill_discrete(name = "Executive")

df4 <- data.frame(Make=c("YES", "NO"),
                  Cnt=c(sum(data_mod$legislative=="YES"), sum(data_mod$legislative=="NO")))
df4 %>%
  group_by(Make) %>%
  summarise(volume = sum(Cnt)) %>%
  mutate(share=volume/sum(volume)) %>%
  ggplot(aes(x="", y= share, fill=reorder(Make, volume))) +
  geom_col() +
  geom_text(aes(label = scales::percent(round(share,3))), position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") + 
  theme_void()+ scale_fill_discrete(name = "Legislative")

df5 <- data.frame(Make=c("MALE", "FEMALE"),
                  Cnt=c(sum(data_mod$sex=="MALE"), sum(data_mod$sex=="FEMALE")))
df5 %>%
  group_by(Make) %>%
  summarise(volume = sum(Cnt)) %>%
  mutate(share=volume/sum(volume)) %>%
  ggplot(aes(x="", y= share, fill=reorder(Make, volume))) +
  geom_col() +
  geom_text(aes(label = scales::percent(round(share,3))), position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") + 
  theme_void()+ scale_fill_discrete(name = "Sex")

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
ols_regress(i.mod)#R^2_a = 0.3557; MSE = 142.199 
summary(i.mod)

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
cor_matrix1 <- cor(data_mod[, c(5:17)])


eigen(cor_matrix1)
cordata1<-data_mod[, c(5:17)]
colnames(cordata1)<-c("Successor", "Assets", "Liabilities",
                                 "Revenue", "Expenses", "Poverty", "CO2",
                                 "Precipitation", "Max Precipitation", 
                                 "Temperature", "Max Temperature",
                                 "Humidity", "Infrastracture")
corrplot(cor(cordata1), method = "color", type = "upper",
         col = brewer.pal(n=8, name="RdYlBu"))

#Condition Number
max(eigen(cor_matrix1)$values)/min(eigen(cor_matrix1)$values) #84.56099

#Condition Indices
as.matrix(sqrt(max(eigen(cor_matrix1)$values)/eigen(cor_matrix1)$values)) #max = 9.195705

#------------------------------------------------------
#Outliers and Influential Observations
#------------------------------------------------------
which(ols_leverage(i.mod)>(2*19/326))
# 2   8   9  21  28  29  43  59 166 290 313 315
data_mod[c(2,8,9,21,28,29,43,59,166,290,313,315),c(2,3)]
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

ols_leverage(i.mod)[c(2,8,9,21,28,29,43,59,166,290,313,315)]

which(!is.na(ols_plot_resid_stud(i.mod)[[1]][,5]))
# Null

which(!is.na(ols_plot_cooksd_chart(i.mod)[[1]][,5]))
# 2   8  11  29  43  46  47 166 168 202 243 286 309 311 316 319
which(!is.na(ols_plot_dffits(i.mod)[[1]][,5]))
# 2   8  11  29  43  46  47 166 168 202 243 286 309 311 316 319

#Influential Outliers
#2 8 29 43 166

which(abs(dfbetas(i.mod)[,1])>2/sqrt(326))
# 10  11  43  46  55  61  66  72  75  76  77  78 155 243 286 298 311 315 316 
which(abs(dfbetas(i.mod)[,2])>2/sqrt(326))
#22  24  43  44  46  47  55  66  77  78  85  97 115 140 145 162 177 
#187 202 204 206 209 286 287 288 289 309 312 319 
which(abs(dfbetas(i.mod)[,3])>2/sqrt(326))
# 6   9  17  21  28  30  33  43  44  47  56 242 286 319 
which(abs(dfbetas(i.mod)[,4])>2/sqrt(326))
#28 29
which(abs(dfbetas(i.mod)[,5])>2/sqrt(326))
#8 43 66 77
which(abs(dfbetas(i.mod)[,6])>2/sqrt(326))
#6   8  28  43  47  56 242 319 
which(abs(dfbetas(i.mod)[,7])>2/sqrt(326))
#16  17  24  25  39  43  44  45  46  49  50  51  52 101 219 243 268 
#284 287 288 289 315 319 325
which(abs(dfbetas(i.mod)[,8])>2/sqrt(326))
#10  11  43  46  55  66  72  75  76  77  78 155 243 286 298 311 315 316 
which(abs(dfbetas(i.mod)[,9])>2/sqrt(326))
#11  77  97 150 162 168 177 201 202 209 211 242 243 300 310 311 315 316 319 325 
which(abs(dfbetas(i.mod)[,10])>2/sqrt(326))
#16  46  47  55  56  80  82  83  85 101 162 172 173 243 288 298 309 318 321 
which(abs(dfbetas(i.mod)[,11])>2/sqrt(326))
#17  32  43  47 119 125 131 140 155 173 194 202 206 242 243 303 309
which(abs(dfbetas(i.mod)[,12])>2/sqrt(326))
#56  66  77  82  97 101 114 123 129 131 140 162 168 173 202 271 296 
#302 310 311 315 316 317 319 
which(abs(dfbetas(i.mod)[,13])>2/sqrt(326))
#11  66 101 114 116 131 140 162 168 173 209 296 302 309 311 313 315 
#316 317 319 325
which(abs(dfbetas(i.mod)[,14])>2/sqrt(326))
# 2  52 125 135 140 143 166 286 292 323 
which(abs(dfbetas(i.mod)[,15])>2/sqrt(326))
#11  39  49  55  75  78 119 125 145 155 162 168 173 202 208 219 276 
#286 310 317 319
which(abs(dfbetas(i.mod)[,16])>2/sqrt(326))
#33  83 120 125 140 152 166 208 213 216 219 226 243 284 301 309 319 
which(abs(dfbetas(i.mod)[,17])>2/sqrt(326))
#11  39  46  47  66  76  80  85 119 129 194 213 216 226 243 271 286 326
which(abs(dfbetas(i.mod)[,18])>2/sqrt(326))
# 20  39  43  46  47  51  78  82 114 135 166 173 202 206 208 264 286 
# 295 317 319 323 
which(abs(dfbetas(i.mod)[,19])>2/sqrt(326))
#2  44  46  47  66  78  82 115 123 155 168 173 187 206 208 288 309 
#310 

ols_plot_dfbetas(i.mod)

#Fitting the Model for Prediction Interval
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
View(data_mod[c(2,8,9,21,28,29,43,59,166,290,313,315), c(2,3,4:22)])
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
ols_regress(i.mod_out1) #R^2_a = 0.3585 #MSE = 141.6
summary(i.mod_out1)

#Detecting Nonnormality and Heteroscedascticity
plot(i.mod_out1)
ols_plot_added_variable(i.mod_out1)
ols_plot_comp_plus_resid(i.mod_out1)

ols_plot_resid_hist(i.mod_out1)
ols_test_normality(i.mod_out1) #Normal
ols_test_breusch_pagan(i.mod_out1, rhs = TRUE) #Homoscedastic
bptest(i.mod_out1, studentize = FALSE)

#Detecting Autocorrelation
durbinWatsonTest(i.mod_out1) #No Autocorrelation

#Detecting Multicollinearity

eigen(cor_matrix2)
cordata2<-data_mod[-c(2,8,9,21,28,29,43,59,166,290,313,315), c(5:17)]
colnames(cordata2)<-c("Successor", "Assets", "Liabilities",
              "Revenue", "Expenses", "Poverty", "CO2",
              "Precipitation", "Max Precipitation", 
              "Temperature", "Max Temperature",
              "Humidity", "Infrastracture")
corrplot(cor(cordata2), method = "color", type = "upper",
         col = brewer.pal(n=8, name="RdYlBu"))

pairs(data_mod[-c(2,8,9,21,28,29,43,59,166,290,313,315), c(5:17)], lower.panel = NULL) 
as.matrix(vif(i.mod_out1))
cor_matrix2 <- cor(data_mod[-c(2,8,9,21,28,29,43,59,166,290,313,315), c(5:17)])
eigen(cor_matrix2)

#Condition Number
(max(eigen(cor_matrix2)$values)/min(eigen(cor_matrix2)$values)) #97.00832

#Condition Indices
as.matrix(sqrt(max(eigen(cor_matrix2)$values)/eigen(cor_matrix2)$values)) #max = 9.849280


#------------------------------------------------------
#Variable Selection
#------------------------------------------------------
ols_step_forward_p(i.mod_out1) #R_a^2 = 0.3721
ols_step_backward_p(i.mod_out1) #R_a^2 = 0.3721
ols_step_both_p(i.mod_out1) #R_a^2 = 0.37061

ols_step_forward_aic(i.mod_out1) 
ols_step_backward_aic(i.mod_out1)
ols_step_both_aic(i.mod_out1) 

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

ols_regress(i.mod1_red)
summary(i.mod1_red) #R_a^2 = 0.3721 #MSE = 138.6

#Detecting Nonnormality and Heteroscedascticity
plot(i.mod1_red)
ols_plot_resid_hist(i.mod1_red)

ols_plot_added_variable(i.mod1_red)
ols_plot_comp_plus_resid(i.mod1_red)

ols_test_normality(i.mod1_red) #Normal
ols_test_breusch_pagan(i.mod1_red, rhs = TRUE) #Homoscedastic
bptest(i.mod1_red, studentize = FALSE)

#Detecting Autocorrelation
durbinWatsonTest(i.mod1_red) #No Autocorrelation

#Detecting Multicollinearity
cordata3<-data_mod[-c(2,8,9,21,28,29,43,59,166,290,313,315), c(5,7,9,10,11,14,16)]
colnames(cordata3)<-c("Successor", "Liabilities",
                     "Expenses", "Poverty", "CO2",
                     "Humidity",
                     "Max Precipitation" 
                     )
corrplot(cor(cordata3), method = "color", type = "upper",
         col = brewer.pal(n=8, name="RdYlBu"))

as.matrix(vif(i.mod1_red))
cor_matrix3 <- cor(data_mod[-c(2,8,9,21,28,29,43,59,166,290,313,315), c(5,7,9,10,11,14,16)])
eigen(cor_matrix3)

#Condition Number
max(eigen(cor_matrix3)$values)/min(eigen(cor_matrix3)$values) #3.742283

#Condition Indices
as.matrix(sqrt(max(eigen(cor_matrix3)$values)/eigen(cor_matrix3)$values)) #max = 1.934498

#------------------------------------------------------
#Adding Regionality
#------------------------------------------------------

dataNew <- data_mod[-c(2,8,9,21,28,29,43,59,166,290,313,315),]
i<- lm(perc_votes_2019~perc_votes_2016
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
                 +factor(region)
                 ,data = dataNew)
summary(i)
#factor(region)IX (Zamboanga Peninsula) *
#factor(region)VII (Central Visayas)    * 
#factor(region)XI (Davao Region)        *
#factor(region)XII (SOCCSKSARGEN)       ** 


#Fitting a model with reg = regions significant in the model with factor(region)
i1<- lm(perc_votes_2019~perc_votes_2016
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
       +factor(reg)
       ,data = dataNew)
ols_regress(i1)
summary(i1) #R^2_a = 0.3862
anova(i1) #MSE = 135.5

ols_step_forward_p(i1) #R^2_a = 0.3862

#Final Model Without Regions VII, IX, XI, XII
i2<- lm(perc_votes_2019~perc_votes_2016
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
        #+factor(reg)
        ,data = dataNew[dataNew$reg==0,])
summary(i2) #R^2_a = 0.3892
anova(i2) #MSE = 141.6
ols_step_forward_p(i2) #R^2_a = 0.3911

#Final Model Only With Regions VII, IX, XI, XII
i3<- lm(perc_votes_2019~perc_votes_2016
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
        #+factor(reg)
        ,data = dataNew[dataNew$reg==1,])
summary(i3) #R^2_a = 0.2662
anova(i3) #MSE = 108.03
ols_step_forward_p(i3) #R^2_a = 0.3004
