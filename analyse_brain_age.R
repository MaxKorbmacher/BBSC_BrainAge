# BBSC brain age data analysis
# Max Korbmacher (max.korbmacher@gmail.com)
#####################################
#### PREPARE DATA AND PACKAGES ######
#####################################

# load results data from CNN predictions (predictions are described in Leonardsen et al., 2022 in NeuroImage)
# BAG = brain age gap = predicted age MINUS chronological age
data = read.csv("/home/max/Documents/Projects/Brain_Age/DL/BBSC/df.csv")

# QC data
QC = read.delim("/home/max/Documents/Projects/BBSC/Brain_Age_paper/group_T1w.tsv")

# load packages
#install.packages("lm.beta")
library(lm.beta)
library(dplyr)
#install.packages("Metrics")
library(Metrics)
library(ggplot2)
library(ggstatsplot)
library(lme4)
#install.packages("metan")
library(metan)
#install.packages("stargazer")
library(stargazer)
library(stringr)

###################################
####### CHECK MODEL FIT ###########
###################################
# calc BAG
data$BAG = data$pred_age - data$age
# make ID variable
data$ID = c(replicate(38,1), replicate(40,2), replicate(25,3))

################## AGE & PREDICTION CORRELATIONS


######## CORRELATIONS
COR = data %>% corr_ci(age, pred_age)
COR$ID = c("All")
COR = COR %>% rename("LowerCI"="LL", "UpperCI"="UL") %>% select(!c(V1,V2,CI))

COR2 = data.frame(matrix(nrow = 3, ncol = 4))
for (i in 1:3){
  COR2[i,] = data %>% filter(ID == i) %>% corr_ci(age, pred_age)%>% select(!c(V1,V2,CI))
}
COR2$ID = c("Subject 1", "Subject 2", "Subject 3")
colnames(COR2) = colnames(COR)
CORs = rbind(COR, COR2)

######## p-vals
# can be obtained like this:
cor.test(data$age, data$pred_age)

################## MAE & RMSE
# Error (MAE & RMSE) for all subjects pooled
MAE = mae(data$age, data$pred_age)
RMSE = rmse(data$age, data$pred_age)
err_all = data.frame(MAE, RMSE)

# and for single single subjects
data$ID = c(replicate(38,"sub-1"), replicate(40, "sub-2"), replicate(25, "sub-3"))## first give subject-specific ID vals
## Sub 1
err1 = data %>% filter(ID == "sub-1") %>% summarize(MAE = mae(age, pred_age),RMSE = rmse(age, pred_age))
## Sub 2
err2 = data %>% filter(ID == "sub-2") %>% summarize(MAE = mae(age, pred_age),RMSE = rmse(age, pred_age))
## Sub 3
err3 = data %>% filter(ID == "sub-3") %>% summarize(MAE = mae(age, pred_age),RMSE = rmse(age, pred_age))
Subject = c("All","1", "2", "3") # define IDs for output table
err_tab = rbind(err_all, err1, err2, err3) # bind tables
err_tab = data.frame(Subject, err_tab) # bind tables

#################################
####### SUMMARY STATS ###########
#################################
# Summary Stats on age
tab0 = data %>% summarize(Age_Mean = mean(age), Age_SD = sd(age))
tab1 = data %>% filter(ID == "sub-1") %>% summarize(Age_Mean = mean(age), Age_SD = sd(age))
tab2 = data %>% filter(ID == "sub-2") %>% summarize(Age_Mean = mean(age), Age_SD = sd(age))
tab3 = data %>% filter(ID == "sub-3") %>% summarize(Age_Mean = mean(age), Age_SD = sd(age))
Age_tab = rbind(tab0, tab1, tab2, tab3)
Age_tab = data.frame(Subject, Age_tab)

# Summary Stats for predicted age
tab0 = data %>% summarize(Pred_Age_Mean = mean(pred_age), Pred_Age_SD = sd(pred_age))
tab1 = data %>% filter(ID == "sub-1") %>% summarize(Pred_Age_Mean = mean(pred_age), Pred_Age_SD = sd(pred_age))
tab2 = data %>% filter(ID == "sub-2") %>% summarize(Pred_Age_Mean = mean(pred_age), Pred_Age_SD = sd(pred_age))
tab3 = data %>% filter(ID == "sub-3") %>% summarize(Pred_Age_Mean = mean(pred_age), Pred_Age_SD = sd(pred_age))
Pred_Age_tab = rbind(tab0, tab1, tab2, tab3)
Pred_Age_tab = data.frame(Subject, Pred_Age_tab)

# Summary Stats on brain age gap
tab0 = data %>% summarize(BAG_Mean = mean(BAG), BAG_SD = sd(BAG))
tab1 = data %>% filter(ID == "sub-1") %>% summarize(BAG_Mean = mean(BAG), BAG_SD = sd(BAG))
tab2 = data %>% filter(ID == "sub-2") %>% summarize(BAG_Mean = mean(BAG), BAG_SD = sd(BAG))
tab3 = data %>% filter(ID == "sub-3") %>% summarize(BAG_Mean = mean(BAG), BAG_SD = sd(BAG))
BAG_tab = rbind(tab0, tab1, tab2, tab3)
BAG_tab = data.frame(Subject, BAG_tab)

sumstats = merge(Age_tab,Pred_Age_tab)
sumstats = merge(sumstats,BAG_tab)
sumstats = merge(sumstats, CORR)
summary_table = merge(sumstats, err_tab)
write.csv(summary_table, "/home/max/Documents/Projects/BBSC/Brain_Age_paper/Table1.csv")

#####################################
########### AGE-BRAIN AGE RELATIONSHIPS ###########
#####################################

####################### CORRELATIONS AGE & BRAIN AGE
# rename variable levels
data$ID = c(replicate(38,"Subject 1"), replicate(40, "Subject 2"), replicate(25, "Subject 3"))

# scatter plot for all subjectes
scatter_all = ggscatterstats(
  data  = data,
  x     = age,
  y     = pred_age,
  ylab   = "Predicted Age",
  xlab   = "Age",
)

ggsave("/home/max/Documents/Projects/BBSC/Brain_Age_paper/scatter_all.pdf", scatter_all, width = 10, height = 8)
# grouped scatter plots
subject_scatter = grouped_ggscatterstats(
  data             = data,
  x                = age,
  y                = pred_age,
  grouping.var     = ID,
  ylab             = "Predicted Age",
  xlab   = "Age",
  ggtheme          = ggplot2::theme_bw(),
  #ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(2, 9, 1), limits = (c(2, 9)))),
  #plotgrid.args    = list(nrow = 1),
  #annotation.args  = list(title = "Relationship between movie length and IMDB ratings")
)
ggsave("/home/max/Documents/Projects/BBSC/Brain_Age_paper/subject_scatter.pdf", subject_scatter, width = 19, height = 8)

########################################
####### QC METRICS, AGE, AND BRAIN AGE ########
#########################################
#################### ALL SUBJECTS TOGETHER ####
QC$id = QC$bids_name
data = data %>% transform(id=str_replace(id, "anat_",""))
QC_BAG = merge(data, QC, by = "id")
QC_reg = QC_BAG %>% select(pred_age,efc, wm2max,snr_total, fber, fwhm_avg, cjv, cnr, snrd_total, age)
QC_model_0 = lm(pred_age~., data = QC_reg)

################### BACKWARDS LINEAR HIERARCHICAL MODELLING
# start removing meaningful predictors (backwards hierarchy)
QC_model_1 = lm(pred_age~age + cjv+ cnr+ fber+ snr_total+ snrd_total+ fwhm_avg + wm2max, data = QC_reg) # remove efc
QC_model_2 = lm(pred_age~age + cjv+ cnr+ fber+ snr_total+ snrd_total+ fwhm_avg, data = QC_reg) # remove wm2max
QC_model_3 = lm(pred_age~age + cjv+ cnr+ fber+ snrd_total+ fwhm_avg, data = QC_reg) # remove snr
QC_model_4 = lm(pred_age~age + cjv+ cnr+ snrd_total+ fwhm_avg, data = QC_reg) # remove fber
QC_model_5 = lm(pred_age~age + cjv+ cnr+ snrd_total, data = QC_reg) # remove fwhm
QC_model_6 = lm(pred_age~age, data = QC_reg)

# make table
stargazer(QC_model_0,QC_model_1,QC_model_2,QC_model_3,QC_model_4,QC_model_5,QC_model_6, type = "html",  #also possible to get "html" or "latex" output
          title = "Braina age predicted from QC metrics", out = "/home/max/Documents/Projects/BBSC/Brain_Age_paper/Table2.doc")
# compare models step-wise (removing each of these variables leads to significantly lower model performance, i.e. less predicted age var expl)
anova(QC_model_0,QC_model_1,QC_model_2,QC_model_3,QC_model_4, QC_model_5, QC_model_6)
# compare each model against the null-model
anova(QC_model_0, QC_model_6)
anova(QC_model_1, QC_model_6)
anova(QC_model_2, QC_model_6)
anova(QC_model_3, QC_model_6)
anova(QC_model_4, QC_model_6)
anova(QC_model_5, QC_model_6) # (we did this above already)
#
# These results suggests that most of the variability can be attributed to the model's age bias and unknown noise (adjusted R2 = .666).
# However, additional R2 = 0.787 - 0.666 = 0.121 are due to data quality.
# This can also be tested from null models:
QC_model_00 = lm(pred_age~age, data = QC_reg)
QC_model_01 = lm(pred_age~age +efc + wm2max+snr_total+fber+fwhm_avg, data = QC_reg)
anova(QC_model_00,QC_model_01)
summary(QC_model_00)
summary(QC_model_01)

# In our data this seems to be subject-dependent, which links :
QC_reg = QC_BAG %>% select(pred_age, age, ID)
diff_mod = lm(pred_age~age*ID, data = QC_reg)
summary(diff_mod)


#################### FOLLOW UP: SINGLE PREDICTOR IMPACT
QC_reg = QC_BAG %>% select(pred_age,efc, wm2max,snr_total, fber, fwhm_avg, cjv, cnr, snrd_total, age)
# follow up: can single metrics explain pred_age?

wm2max_model = lm(pred_age~age+wm2max, data = QC_reg)
snr_model = lm(pred_age~age+snr_total, data = QC_reg)

fwhm_avg_model = lm(pred_age~age+fwhm_avg, data = QC_reg)
cjv_model = lm(pred_age~age+cjv, data = QC_reg)
stargazer(wm2max_model, fwhm_avg_model, snr_model, cjv_model, type = "text",  #also possible to get "html" or "latex" output
          title = "Single QC metrics and pred_age")


#################### SUBJECT 1 ####
QC_reg = QC_BAG %>% filter(ID == "Subject 1") %>% select(pred_age,efc, wm2max, snr_total, fber,fwhm_avg, cnr,cjv,  snrd_total, age)
QC_model_0 = lm(pred_age~., data = QC_reg)
# shows no particularly good model fit
summary(QC_model_0)
# check other models
#QC_model_1 = lm(pred_age~age+cjv+ cnr+ fber+ snr_total+ snrd_total+ fwhm_avg+wm2max, data = QC_reg) # remove efc
#QC_model_2 = lm(pred_age~age+cjv+ cnr+ fber+ snr_total+ snrd_total+ fwhm_avg, data = QC_reg) # remove wm2max
#QC_model_3 = lm(pred_age~age+cjv+ cnr+ fber+ fwhm_avg+ snrd_total, data = QC_reg) # remove snr
#QC_model_4 = lm(pred_age~age+cjv+ cnr+ fwhm_avg+ snrd_total, data = QC_reg) # remove fber
#QC_model_5 = lm(pred_age~age+cjv+ cnr+ snrd_total, data = QC_reg)# remove fwhm

#stargazer(QC_model_0,QC_model_1,QC_model_2,QC_model_3,QC_model_4,QC_model_5, type = "text",  #also possible to get "html" or "latex" output
#          title = "QC metrics and pred_age")
# compare models (removing each of these variables leads to significantly lower model performance, i.e. less pred_age var expl)
#anova(QC_model_0,QC_model_1,QC_model_2,QC_model_3,QC_model_4,QC_model_5)

#QC_model_00 = lm(pred_age~age, data = QC_reg)
#QC_model_01 = lm(pred_age~age +efc + wm2max+snr_total+fber+fwhm_avg, data = QC_reg)
#anova(QC_model_00,QC_model_01)
#summary(QC_model_00)
#summary(QC_model_01)

#################### SUBJECT 2 ####
QC_reg = QC_BAG %>% filter(ID == "Subject 2") %>% select(pred_age,wm2max, fwhm_avg, snr_total, cjv, cnr, efc, fber, snrd_total, age)
QC_model_0 = lm(pred_age~., data = QC_reg)
summary(QC_model_0)

################### BACKWARDS LINEAR HIERARCHICAL MODELLING
# start removing meaningful predictors (backwards hierarchy)
QC_model_1 = lm(pred_age~age+cjv+ cnr+ efc+ fber+ snr_total+ snrd_total+ fwhm_avg, data = QC_reg) # remove wm2max
# from here, no more significant predictors, but one can keep removing.
summary(QC_model_1)
QC_model_2 = lm(pred_age~age+cjv+ cnr+ fber+ snr_total+ snrd_total+ fwhm_avg, data = QC_reg) # remove efc
summary(QC_model_2)
QC_model_3 = lm(pred_age~age+cjv+ cnr+ snr_total+ snrd_total+ fwhm_avg, data = QC_reg) # remove fber
summary(QC_model_3)
QC_model_4 = lm(pred_age~age+cjv+ cnr+ snr_total+ fwhm_avg, data = QC_reg) # remove snrd
summary(QC_model_4)
QC_model_5 = lm(pred_age~age+cjv+ cnr+ fwhm_avg, data = QC_reg) # remove snr
summary(QC_model_5)
QC_model_6 = lm(pred_age~age, data = QC_reg) # remove snr
summary(QC_model_6)

# make table
stargazer(QC_model_0,QC_model_1,QC_model_2,QC_model_3,QC_model_4, QC_model_5, type = "text",  #also possible to get "html" or "latex" output
          title = "QC metrics and pred_age")
# compare models (removing each of these variables leads to significantly lower model performance, i.e. less pred_age var expl)
anova(QC_model_0,QC_model_1,QC_model_2,QC_model_3,QC_model_4, QC_model_5)


#################### FOLLOW UP: SINGLE PREDICTOR IMPACT

# follow up: can single metrics explain pred_age?
wm2max_model = lm(pred_age~wm2max + age, data = QC_reg)
efc_model = lm(pred_age~efc + age, data = QC_reg)
fber_model = lm(pred_age~fber + age, data = QC_reg)
snrd_model = lm(pred_age~snrd_total + age, data = QC_reg)
snr_model = lm(pred_age~snr_total + age, data = QC_reg)
  stargazer(wm2max_model, efc_model, fber_model, snrd_model, snr_model, type = "text",  #also possible to get "html" or "latex" output
          title = "Single QC metrics and pred_age")



#################### SUBJECT 3 ####
QC_reg = QC_BAG %>% filter(ID == "Subject 3") %>% select(pred_age,wm2max, fwhm_avg, snr_total, cjv, cnr, efc, fber, snrd_total, age)
QC_model_0 = lm(pred_age~., data = QC_reg)
# predicting brain age from age + QC metrics does not seem to work well for both sub1 & sub3 
summary(QC_model_0)

########################################
####### QC METRICS AND BRAIN AGE ########
#########################################
#################### ALL SUBJECTS TOGETHER ####
QC$id = QC$bids_name
data = data %>% transform(id=str_replace(id, "anat_",""))
QC_BAG = merge(data, QC, by = "id")
QC_reg = QC_BAG %>% select(pred_age,efc, wm2max,snr_total, fber, fwhm_avg, cjv, cnr, snrd_total, age)
QC_model_0 = lm(pred_age~., data = QC_reg)

################### BACKWARDS LINEAR HIERARCHICAL MODELLING
# start removing meaningful predictors (backwards hierarchy)
QC_model_1 = lm(pred_age~age + cjv+ cnr+ fber+ snr_total+ snrd_total+ fwhm_avg + wm2max, data = QC_reg) # remove efc
QC_model_2 = lm(pred_age~age + cjv+ cnr+ fber+ snr_total+ snrd_total+ fwhm_avg, data = QC_reg) # remove wm2max
QC_model_3 = lm(pred_age~age + cjv+ cnr+ fber+ snrd_total+ fwhm_avg, data = QC_reg) # remove snr
QC_model_4 = lm(pred_age~age + cjv+ cnr+ snrd_total+ fwhm_avg, data = QC_reg) # remove fber
QC_model_5 = lm(pred_age~age + cjv+ cnr+ snrd_total, data = QC_reg) # remove fwhm
QC_model_6 = lm(pred_age~age, data = QC_reg)

# make table
stargazer(QC_model_0,QC_model_1,QC_model_2,QC_model_3,QC_model_4,QC_model_5,QC_model_6, type = "html",  #also possible to get "html" or "latex" output
          title = "Braina age predicted from QC metrics", out = "/home/max/Documents/Projects/BBSC/Brain_Age_paper/Table2.doc")
# compare models step-wise (removing each of these variables leads to significantly lower model performance, i.e. less predicted age var expl)
anova(QC_model_0,QC_model_1,QC_model_2,QC_model_3,QC_model_4, QC_model_5, QC_model_6)
# compare each model against the null-model
anova(QC_model_0, QC_model_6)
anova(QC_model_1, QC_model_6)
anova(QC_model_2, QC_model_6)
anova(QC_model_3, QC_model_6)
anova(QC_model_4, QC_model_6)
anova(QC_model_5, QC_model_6) # (we did this above already)
#
# These results suggests that most of the variability can be attributed to the model's age bias and unknown noise (adjusted R2 = .666).
# However, additional R2 = 0.787 - 0.666 = 0.121 are due to data quality.
# This can also be tested from null models:
QC_model_00 = lm(pred_age~age, data = QC_reg)
QC_model_01 = lm(pred_age~age +efc + wm2max+snr_total+fber+fwhm_avg, data = QC_reg)
anova(QC_model_00,QC_model_01)
summary(QC_model_00)
summary(QC_model_01)

# In our data this seems to be subject-dependent, which links :
QC_reg = QC_BAG %>% select(pred_age, age, ID)
diff_mod = lm(pred_age~age*ID, data = QC_reg)
summary(diff_mod)


#################### FOLLOW UP: SINGLE PREDICTOR IMPACT
QC_reg = QC_BAG %>% select(pred_age,efc, wm2max,snr_total, fber, fwhm_avg, cjv, cnr, snrd_total, age)
# follow up: can single metrics explain pred_age?

wm2max_model = lm(pred_age~age+wm2max, data = QC_reg)
snr_model = lm(pred_age~age+snr_total, data = QC_reg)

fwhm_avg_model = lm(pred_age~age+fwhm_avg, data = QC_reg)
cjv_model = lm(pred_age~age+cjv, data = QC_reg)
stargazer(wm2max_model, fwhm_avg_model, snr_model, cjv_model, type = "text",  #also possible to get "html" or "latex" output
          title = "Single QC metrics and pred_age")


#################### SUBJECT 1 ####
QC_reg = QC_BAG %>% filter(ID == "Subject 1") %>% select(pred_age,efc, wm2max, snr_total, fber,fwhm_avg, cnr,cjv,  snrd_total)
QC_model_0 = lm(pred_age~., data = QC_reg)
# shows no particularly good model fit
summary(QC_model_0)
# check other models
QC_model_1 = lm(pred_age~cjv+ cnr+ fber+ snr_total+ snrd_total+ fwhm_avg+wm2max, data = QC_reg) # remove efc
QC_model_2 = lm(pred_age~cjv+ cnr+ fber+ snr_total+ snrd_total+ fwhm_avg, data = QC_reg) # remove wm2max
QC_model_3 = lm(pred_age~cjv+ cnr+ fber+ fwhm_avg+ snrd_total, data = QC_reg) # remove snr
QC_model_4 = lm(pred_age~cjv+ cnr+ fwhm_avg+ snrd_total, data = QC_reg) # remove fber
QC_model_5 = lm(pred_age~cjv+ cnr+ snrd_total, data = QC_reg)# remove fwhm

stargazer(QC_model_0,QC_model_1,QC_model_2,QC_model_3,QC_model_4,QC_model_5, type = "text",  #also possible to get "html" or "latex" output
          title = "QC metrics and pred_age")
# compare models (removing each of these variables leads to significantly lower model performance, i.e. less pred_age var expl)
anova(QC_model_0,QC_model_1,QC_model_2,QC_model_3,QC_model_4,QC_model_5)

QC_model_00 = lm(pred_age~1, data = QC_reg)
QC_model_01 = lm(pred_age~1 +efc + wm2max+snr_total+fber+fwhm_avg, data = QC_reg)
anova(QC_model_00,QC_model_01)
summary(QC_model_00)
summary(QC_model_01)
#
#
#
#
#################### FOLLOW UP: SINGLE PREDICTOR IMPACT

# follow up: can single metrics explain pred_age?
wm2max_model = lm(pred_age~wm2max, data = QC_reg)
fwhm_avg_model = lm(pred_age~fwhm_avg, data = QC_reg)
snr_model = lm(pred_age~snr_total, data = QC_reg)
cjv_model = lm(pred_age~cjv, data = QC_reg)
stargazer(wm2max_model, fwhm_avg_model, snr_model, cjv_model, type = "text",  #also possible to get "html" or "latex" output
          title = "Single QC metrics and pred_age")




#################### SUBJECT 2 ####
QC_reg = QC_BAG %>% filter(ID == "Subject 2") %>% select(pred_age,wm2max, fwhm_avg, snr_total, cjv, cnr, efc, fber, snrd_total)
QC_model_0 = lm(pred_age~., data = QC_reg)
summary(QC_model_0)

################### BACKWARDS LINEAR HIERARCHICAL MODELLING
# start removing meaningful predictors (backwards hierarchy)
QC_model_1 = lm(pred_age~cjv+ cnr+ efc+ fber+ snr_total+ snrd_total+ fwhm_avg, data = QC_reg) # remove wm2max
QC_model_2 = lm(pred_age~cjv+ cnr+ efc+ fber+ snr_total+ snrd_total, data = QC_reg) # remove fwhm_avg
QC_model_3 = lm(pred_age~cjv+ cnr+ efc+ fber+ snrd_total, data = QC_reg) # remove snr
QC_model_4 = lm(pred_age~ cnr+ efc+ fber+ snrd_total, data = QC_reg) # remove cjv
QC_model_5 = lm(pred_age~ cnr+ fber+ snrd_total, data = QC_reg) # remove efc

# make table
stargazer(QC_model_0,QC_model_1,QC_model_2,QC_model_3,QC_model_4, QC_model_5, type = "text",  #also possible to get "html" or "latex" output
          title = "QC metrics and pred_age")
# compare models (removing each of these variables leads to significantly lower model performance, i.e. less pred_age var expl)
anova(QC_model_0,QC_model_1,QC_model_2,QC_model_3,QC_model_4, QC_model_5)

# compare models for most sign predictors
QC_model_00 = lm(pred_age~age, data = QC_reg)
QC_model_01 = lm(pred_age~age + wm2max+fwhm_avg+snr, data = QC_reg)
anova(QC_model_00,QC_model_01)
summary(QC_model_00)
summary(QC_model_01)


#################### FOLLOW UP: SINGLE PREDICTOR IMPACT

# follow up: can single metrics explain pred_age?
wm2max_model = lm(pred_age~wm2max, data = QC_reg)
fwhm_avg_model = lm(pred_age~fwhm_avg, data = QC_reg)
snr_model = lm(pred_age~snr_total, data = QC_reg)
cjv_model = lm(pred_age~cjv, data = QC_reg)
stargazer(wm2max_model, fwhm_avg_model, snr_model, cjv_model, type = "text",  #also possible to get "html" or "latex" output
          title = "Single QC metrics and pred_age")



#################### SUBJECT 3 ####
QC_reg = QC_BAG %>% filter(ID == "Subject 3") %>% select(pred_age,wm2max, fwhm_avg, snr_total, cjv, cnr, efc, fber, snrd_total)
QC_model_0 = lm(pred_age~., data = QC_reg)
summary(QC_model_0)

################### BACKWARDS LINEAR HIERARCHICAL MODELLING
# start removing meaningful predictors (backwards hierarchy)
QC_model_1 = lm(pred_age~cjv+ cnr+ wm2max+ fber+ snr_total+ fwhm_avg, data = QC_reg) # remove efc
QC_model_2 = lm(pred_age~cjv+ cnr + fber+ snr_total+ fwhm_avg, data = QC_reg) # wm2max
QC_model_3 = lm(pred_age~cjv+ cnr + fber+ fwhm_avg, data = QC_reg) # snr
QC_model_4 = lm(pred_age~cjv+ cnr + fwhm_avg, data = QC_reg) # fber
QC_model_5 = lm(pred_age~cjv+ cnr, data = QC_reg) # fwhm_avg

# make table
stargazer(QC_model_0,QC_model_1,QC_model_2,QC_model_3, QC_model_4, QC_model_5, type = "text",  #also possible to get "html" or "latex" output
          title = "QC metrics and pred_age")
# compare models (removing each of these variables leads to significantly lower model performance, i.e. less pred_age var expl)
anova(QC_model_0,QC_model_1,QC_model_2,QC_model_3, QC_model_4, QC_model_5)

QC_model_00 = lm(pred_age~1, data = QC_reg)
QC_model_01 = lm(pred_age~1 + wm2max+fwhm_avg+snr_total+efc+fber, data = QC_reg)
anova(QC_model_00,QC_model_01)
summary(QC_model_00)
summary(QC_model_01)

#################### FOLLOW UP: SINGLE PREDICTOR IMPACT

# follow up: can single metrics explain pred_age?
cnr_model = lm(pred_age~cnr, data = QC_reg)
efc_model = lm(pred_age~efc, data = QC_reg)
wm2max_model = lm(pred_age~wm2max, data = QC_reg)
stargazer(cnr_model, efc_model, wm2max_model, type = "text",  #also possible to get "html" or "latex" output
          title = "Single QC metrics and pred_age")

## ## ## ## ## ##
## CONCLUSION ## 
## ## ## ## ## ##
# ! No surprise !
# The quality of the scans can has effects on brain age estimates but those seem to predict individual aspects of brain age, hence I estimate bivariate relationships.

#########################################
##### BIVARIATE RELATIONSHIPS ###########
#########################################

#################### extract beta values
# prep data frames
SUB1 = QC_BAG %>% filter(ID == "Subject 1")
SUB2 = QC_BAG %>% filter(ID == "Subject 2")
SUB3 = QC_BAG %>% filter(ID == "Subject 3")
data_list = list(QC_BAG,SUB1,SUB2,SUB3)
predictors = c("cjv", "cnr", "wm2max","fber","snr_total", "fwhm_avg", "efc", "snrd_total")
out = data.frame(matrix(nrow = length(predictors),ncol = length(data_list)))
pvals = data.frame(matrix(nrow = length(predictors),ncol = length(data_list)))
# loop over them extracting beta values
for (i in 1:4){
  m1 = lm(pred_age~age + cjv, data = data_list[[i]])
  m2 = lm(pred_age~age + cnr, data = data_list[[i]])
  m3 = lm(pred_age~age + wm2max, data = data_list[[i]])
  m4 = lm(pred_age~age + fber, data = data_list[[i]])
  m5 = lm(pred_age~age + snr_total, data = data_list[[i]])
  m6 = lm(pred_age~age + fwhm_avg, data = data_list[[i]])
  m7 = lm(pred_age~age + efc, data = data_list[[i]])
  m8 = lm(pred_age~age + snrd_total, data = data_list[[i]])
  model_list = list(m1,m2,m3,m4,m5,m6,m7,m8)
  for (m in 1:length(predictors)){
    out[m,i] = lm.beta(model_list[[m]])$coefficients[3]
    pvals[m,i] = summary(model_list[[m]])$coefficients[3,4]
  }
}
# name columns 
colnames(out) = c("all", "sub1","sub2","sub3")
# correct for multiple comparison
pvals = pvals*8*4
# rename cols
colnames(pvals) = colnames(out)
# put in a vector showing which predictor is which
out$predictors = predictors
pvals$predictors = predictors

# check surviving p-vals
pvals %>% filter(all < 0.05)
# for all: wm2max, fwhm_avg, efc
pvals %>% filter(sub1 < 0.05)
# for sub1: none
pvals %>% filter(sub2 < 0.05)
# for sub 2: fber, efc, snrd
pvals %>% filter(sub3 < 0.05)
# for sub 3: none

# print standardised betas
out

#################### CORRELATION STRUCTURE OF THE DATA
QC_reg = QC_BAG %>% select(ID,pred_age,wm2max, fwhm_avg, snr_total, cjv, cnr, efc, fber, snrd_total)
ggcorrmat(
  data         = QC_reg,  #dplyr::filter(data, ID %in% c("Subject 1")),
  type         = "robust",
  colors       = c("#cbac43", "white", "#550000"),
  matrix.type  = "lower"
)
# by subject
grouped_ggcorrmat(
  data         = QC_reg,  #dplyr::filter(data, ID %in% c("Subject 1")),
  type         = "robust",
  colors       = c("#cbac43", "white", "#550000"),
  grouping.var = ID,
  matrix.type  = "lower"
)








#########################################################
### EXPLAINING BAG FROM ADDIONAL MEANINGFUL MEASURES #####
########################################################
# relode data for correct id column
data = read.csv("/home/max/Documents/Projects/Brain_Age/DL/BBSC/df.csv")

# behavioural data
morning_final = read.csv("/home/max/Documents/Projects/BBSC/Brain_Age_paper/morning_final.csv")
before_final = read.csv("/home/max/Documents/Projects/BBSC/Brain_Age_paper/before_final.csv")
after_final = read.csv("/home/max/Documents/Projects/BBSC/Brain_Age_paper/after_final.csv")

# rename id column in behavioural data frames
after_final = after_final %>% rename("id" = "scan")
before_final = before_final %>% rename("id" = "scan")
morning_final = morning_final %>% rename("id" = "scan")

# remove suffix
after_final = after_final %>% transform(id=str_replace(id, ".nii.gz","")) %>% select(-age)
before_final = before_final %>% transform(id=str_replace(id, ".nii.gz","")) %>% select(-age)
morning_final = morning_final %>% transform(id=str_replace(id, ".nii.gz","")) %>% select(-age)

# merge data frames
abeh_BAG = merge(data, after_final, by = "id")
bbeh_BAG = merge(abeh_BAG, before_final, by = "id")
morning_BAG = merge(bbeh_BAG, morning_final, by = "id")

# modelling
## null model (age and ID) (we use three different models, later specified based on missingness in each model)
mod0.1 <- lm(formula = pred_age ~ age + ID, data = morning_BAG)

## sleep behaviour (from morning questionnaire)
mod1 <- lm(formula = pred_age ~ age + ID + Soreness + sleep_qual + sleep_hours, data = morning_BAG)
stargazer(mod0.1, mod1, type = "text",  #also possible to get "html" or "latex" output
          title = "")
anova(mod0.1, mod1) # not different from null model
#ggcoefstats(mod1)

## whether (from before scanning questionnaire)
whether = bbeh_BAG %>% select(pred_age, age, ID.x, outside_humid, outside_temp, control_room_temp, control_room_humid, scanner_room_temp, scanner_room_humid)
mod2 <- lm(formula = pred_age ~ age + ID.x + outside_humid + outside_temp + control_room_temp + control_room_humid + scanner_room_temp + scanner_room_humid, data = whether)
mod0.2 <- lm(formula = pred_age ~ age + ID.x, data = na.omit(whether))
stargazer(mod0.2, mod2, type = "text",  #also possible to get "html" or "latex" output
          title = "")
anova(mod0.2, mod2) # not different from null model
#ggcoefstats(mod2)

## body measures (from before scanning questionnaire)
body_measures = bbeh_BAG %>% select(pred_age, age, ID.x, body_temp, pulse_pressure, eaten_today, coffe_today)
mod3 <- lm(formula = pred_age ~ ., data = body_measures)
mod0.3 <- lm(formula = pred_age ~ age + ID.x, data = na.omit(body_measures))
stargazer(mod0.3, mod3, type = "text",  #also possible to get "html" or "latex" output
          title = "")
anova(mod0.3, mod3) # not different from null model

#
#
#
# CONCLUSION: whether (temperature and humidity), sleep, body temperature, blood pressure and food or caffeine consumption have no effect on BAG
#
#


#####################################
############# DIFFERENCES ###########
#####################################

# pre covid, long covid
#
# escitalopram ON vs OFF
# 13.05.2021
data$ID = c(replicate(38,"Subject 1"), replicate(40, "Subject 2"), replicate(25, "Subject 3"))
data %>% filter(ID=="Subject 1")



######## OLD ############


############# SIMPLE SCATTER PLOTS

## ALL subjects
data %>% 
  ggplot(aes(x=Age, 
             y=pred_age, 
             color=ID))+ geom_point()+
  geom_smooth(method="lm",se = T) + theme_bw() +
  ylab("Predicted Age") + xlab("Age") + scale_colour_discrete("Models") + ggtitle("Predicted Age for each Subject over time")

## Fitting one line for all subject
data %>% 
  ggplot(aes(x=Age, 
             y=pred_age))+ geom_point(group = ID)+
  geom_smooth(method="lm",se = T) + theme_bw() +
  ylab("Predicted Age") + xlab("Age") + scale_colour_discrete("Models") + ggtitle("Predicted Age for all Subjects over time")


## Subject 1
data %>% filter(ID == "Subject 1") %>%
  ggplot(aes(x=Age, 
             y=pred_age))+ geom_point()+
  geom_smooth(method="lm",se = T) + theme_bw() +
  ylab("Predicted Age") + xlab("Age") + scale_colour_discrete("Models") + ggtitle("Predicted Age for Subject 1")

## Subject 2
data %>% filter(ID == "Subject 2") %>%
  ggplot(aes(x=Age, 
             y=pred_age))+ geom_point()+
  geom_smooth(method="lm",se = T) + theme_bw() +
  ylab("Predicted Age") + xlab("Age") + scale_colour_discrete("Models") + ggtitle("Predicted Age for Subject 2")

## Subject 3
data %>% filter(ID == "Subject 3") %>%
  ggplot(aes(x=Age, 
             y=pred_age))+ geom_point()+
  geom_smooth(method="lm",se = T) + theme_bw() +
  ylab("Predicted Age") + xlab("Age") + scale_colour_discrete("Models") + ggtitle("Predicted Age for Subject 1")
