# analysis of brain age densly sampled from 4 individuals
# Max Korbmacher, 18.02.2022

# PREPARATION ####

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
library(ggpubr)
library(psych)
library(lmerTest)
library(forcats)
library(car)

# load & clean data
## data set 1: BBSC data
data = read.csv("/home/max/Documents/Projects/Brain_Age/DL/BBSC/df.csv")
## QC data for data set 1
QC = read.delim("/home/max/Documents/Projects/BBSC/Brain_Age_paper/group_T1w.tsv")
# make ID variable
data$ID = c(replicate(38,1), replicate(40,2), replicate(25,3))
# merge and fix data frame for dataset 1
QC$id = QC$bids_name
data = data %>% transform(id=str_replace(id, "anat_",""))
dataset1 = merge(data, QC, by = "id")
dataset1$fieldStrength = replicate(nrow(dataset1), 3.0)

## data set 2: the travelling human phantom
# load MRIQC metrics table
MRIQC = read.csv("/home/max/Documents/Projects/Brain_Age/DL/Travelling_Human_Phantom/out.csv")
# load brain age predictions
predictions = read.csv("/home/max/Documents/Projects/Brain_Age/DL/Travelling_Human_Phantom/predictions.csv")
# load inclusion list
inclusion = read.csv("/home/max/Documents/Projects/Brain_Age/DL/Travelling_Human_Phantom/inclusion_list.csv")

# first give MRIQC scans a counter vector (are in the right order from 1 to n)
MRIQC$scanID = seq(1:nrow(MRIQC))

# NOTE: In the first run, we DID NOT REMOVE volumes which had the same acquisition place, time, parameters...
# If that is of interest, exclude by inclusion list

# then merge QC metrics with brain age prediction data
data2 = merge(MRIQC, predictions, by = "scanID")
data2$ID = replicate(nrow(data2),4)

# create data frames for merging
QC_reg = dataset1 %>% select(efc, wm2max,snr_total, fber, fwhm_avg, cjv, cnr, age,pred_age, fieldStrength, ID)
data2$pred_age = data2$prediction
QC_reg2 = data2 %>% select(efc, wm2max,snr_total, fber, fwhm_avg, cjv, cnr, age, pred_age,fieldStrength, ID)

# merge dat
dat = rbind(QC_reg,QC_reg2)
#dat$fieldStrength = factor(dat$fieldStrength)
dat$ID = factor(dat$ID)

# RMSE & MAE ####
## Sub 1
err1 = dat %>% filter(ID == "1") %>% summarize(MAE = mae(age, pred_age),RMSE = rmse(age, pred_age))
## Sub 2
err2 = dat %>% filter(ID == "2") %>% summarize(MAE = mae(age, pred_age),RMSE = rmse(age, pred_age))
## Sub 3
err3 = dat %>% filter(ID == "3") %>% summarize(MAE = mae(age, pred_age),RMSE = rmse(age, pred_age))
## Sub 4
err4 = dat %>% filter(ID == "4", fieldStrength == 3) %>% summarize(MAE = mae(age, pred_age),RMSE = rmse(age, pred_age))
Subject = c("sub-1", "sub-2", "sub-3","sub-4") # define IDs for output table
err_tab = rbind(err1, err2, err3,err4) # bind tables
err_tab = data.frame(Subject, err_tab) # bind tables
err_tab

# CORRELATIONS ####
dat$Subject = c(replicate(38, "Subject 1 (Bergen Breakfast Scanning Club Data)"),replicate(40, "Subject 2 (Bergen Breakfast Scanning Club Data)"),replicate(25, "Subject 3 (Bergen Breakfast Scanning Club Data)"),replicate(551, "Subject 4 (Travelling Human Phantom Data)"))
# grouped scatter plots
subject_scatter = grouped_ggscatterstats(
  data             = dat,
  x                = age,
  y                = pred_age,
  grouping.var     = Subject,
  ylab             = "Predicted Age",
  xlab   = "Age",
  ggtheme          = ggplot2::theme_bw(),
)
ggsave("/home/max/Documents/Projects/Brain_Age/DL/BBSC/BBSC_BrainAge/Figure1.pdf", subject_scatter, width = 19, height = 8)



# MODELLING ASSUMPTIONS ####

# check potential multicollinearity & homoscedasticity

## 1) for all participants in BBSC dataset (dataset 1)
BBSC = dat %>% filter(ID != 4)
BBSC[1:38,1:7] = BBSC %>% filter(ID == 1) %>% select(c(efc, wm2max,snr_total, fber, fwhm_avg, cjv, cnr))%>% scale()
BBSC[39:78,1:7] = BBSC %>% filter(ID == 2) %>% select(c(efc, wm2max,snr_total, fber, fwhm_avg, cjv, cnr))%>% scale()
BBSC[79:nrow(BBSC),1:7] = BBSC %>% filter(ID == 3) %>% select(c(efc, wm2max,snr_total, fber, fwhm_avg, cjv, cnr))%>% scale()
THP = dat %>% filter(ID == 4)
dat_cor = BBSC %>% select(-ID, -fieldStrength, -Subject)
corPlot(dat_cor)
# we can see a number of correlations of r > .4
# We hence further check multicollinearity by calculating the variance inflation factor of a model including all the beforementioned variables for all participants
 
# model data
model = lmer(pred_age ~ efc + wm2max +snr_total + fber + fwhm_avg + cjv + cnr + age + (1|ID),data = BBSC)
vif(model)
# concerning vif values (VIF > 5) across the board, except fwhm_avg. Hence, a better approach would be to model each QC parameter for iself.

model = lm(pred_age ~ efc + wm2max +snr_total + fber + fwhm_avg + cjv + cnr + age + ID,data = BBSC)

# check linearity & homosced
par(mfrow = c(2, 2))
plot(model)
# looks fine, but multicollinearity still applies!

# we can see age dependencies for the predictions. This does also influence the variability in predictions 

## 2) for participant 1
dat_cor1 = dat %>% filter(ID == 1) %>%select(-ID, -fieldStrength, -Subject)
corPlot(dat_cor1)
dat_cor1 = dat %>% filter(ID == 1) %>%select(-ID, -fieldStrength, -Subject)
# here most QC metrics are strongly related
model1 = lm(pred_age ~ efc + wm2max +snr_total + fber + fwhm_avg + cjv + cnr + age,data = dat_cor1)
vif(model1)
# here we should only observe associations between FWHM and predicted age, while controlling for age

# checking homoscedasticity
model = lm(pred_age ~ efc + wm2max +snr_total + fber + fwhm_avg + cjv + cnr + age,data = dat_cor1)
par(mfrow = c(2, 2))
plot(model)
# Within individual 1, linearity and homoscedasticity assumptions are met.

## 2) for participant 2
plotdat = dat %>% filter(ID == 2) %>%select(-ID, -fieldStrength, -Subject)
corPlot(plotdat, cluster = TRUE)
dat_cor2 = dat %>% filter(ID == 2) %>%select(-ID, -fieldStrength, -Subject)
# here most QC metrics are strongly related
model1 = lm(pred_age ~ efc + wm2max +snr_total + fber + fwhm_avg + cjv + cnr + age,data = dat_cor2)
vif(model1)
# Strong multicollinearity across QC metrics (all vifs > 5)

# checking homoscedasticity
model = lm(pred_age ~ efc + wm2max +snr_total + fber + fwhm_avg + cjv + cnr + age,data = dat_cor2)
par(mfrow = c(2, 2))
plot(model)
# Within individual 2, linearity and homoscedasticity assumptions are satisfied (although not perfectly).

## 3) for participant 3
dat %>% filter(ID == 3) %>%select(-ID, -fieldStrength, -Subject) %>% corPlot(cluster = TRUE)
dat_cor3 = dat %>% filter(ID == 3) %>%select(-ID, -fieldStrength, -Subject)
# here most QC metrics are strongly related
model1 = lm(pred_age ~ efc + wm2max +snr_total + fber + fwhm_avg + cjv + cnr + age,data = dat_cor3)
vif(model1)
# here we could include wm2max, fber, and fwhm in the model

# checking homoscedasticity
model = lm(pred_age ~ efc + wm2max +snr_total + fber + fwhm_avg + cjv + cnr + age,data = dat_cor3)
par(mfrow = c(2, 2))
plot(model)
# Within individual 3, linearity and homoscedasticity assumptions are met (although not perfectly as in subject 2).

## 4) for participant 4
dat %>% filter(ID == 4) %>%select(-ID, -fieldStrength, -Subject) %>% corPlot(cluster = TRUE)
dat_cor4.1 = dat %>% filter(ID == 4) %>% select(-ID)
dat_cor4 = dat %>% filter(ID == 4) %>%select(-ID, -fieldStrength, -Subject)
# here most QC metrics are strongly related
model1 = lm(pred_age ~ efc + wm2max +snr_total + fber + fwhm_avg + cjv + cnr + age,data = dat_cor4)
vif(model1)
# here we would need to exclude cjv and cnr as in subject 1.

# checking homoscedasticity
model = lm(pred_age ~ efc + wm2max +snr_total + fber + fwhm_avg + cjv + cnr + age,data = dat_cor4)
par(mfrow = c(2, 2))
plot(model)
# Within individual 4, linearity and homoscedasticity assumptions are met.

# MIXED MODELLING ####
# due to strong multicollinearity (high correlations between QC metrics), we model dependencies of predicted age with QC metrics per QC metric
## BBSC data
QCmetrics = colnames(dat[1:7])
BBSC_model = list()
RIb = c()
RIstde = c()
RIt = c()
RIp = c()
for ( i in 1:length(QCmetrics)){
  f = formula(paste("pred_age~",QCmetrics[i],"+age+(1|ID)"))
  BBSC_model[[i]] = lmer(f,data = BBSC)
  RIb[i] = as.numeric(fixef(BBSC_model[[i]])[2])
  RIstde[i] = summary(BBSC_model[[i]])$coefficients[2,2]
  RIt[i]= summary(BBSC_model[[i]])$coefficients[2,4]
  RIp[i]= summary(BBSC_model[[i]])$coefficients[2,5]
}
out = data.frame(QCmetrics,RIb,RIstde,RIt,RIp)
out$p_corrected = p.adjust(out$RIp,method = "holm")
colnames(out) = c("Metric", "Std_Beta", "Std_Err","T_val", "p_val","p_corr")
out

## THP data
# model field strength, and scanner type (for that some prep is needed)
data2$manufacturer = factor(data2$manufacturer)
data2$manufacturer = fct_collapse(data2$manufacturer,  Siemens = c("Siemens","SIEMENS","Siemens HealthCare GmbH"), GE = c("GE MEDICAL SYSTEMS"), Philips = c("Philips Healthcare", "Philips","Philips Medical Systems"))
#data2$fieldStrength = factor(data2$fieldStrength)
THP_reg = data2 %>% dplyr::select(efc, wm2max,snr_total, fber, fwhm_avg, cjv, cnr, age,pred_age, fieldStrength, manufacturer,sliceThickness,siteID)
THP_reg[1:7] = scale(THP_reg[1:7])
THP_reg$siteID = dactor(THP_reg$siteID)
THP_model = list()
for ( i in 1:length(QCmetrics)){
  f = formula(paste("pred_age~",QCmetrics[i],"+age+(1|fieldStrength)+(1|sliceThickness)+(1|siteID)")) 
  THP_model[[i]] = lmer(f,data = THP_reg)
  # coefficients for the different QC metrics
  RIb[i] = as.numeric(fixef(THP_model[[i]])[2])
  RIstde[i] = summary(THP_model[[i]])$coefficients[2,2]
  RIt[i]= summary(THP_model[[i]])$coefficients[2,4]
  RIp[i]= summary(THP_model[[i]])$coefficients[2,5]
}

# coefficients of the QC metrics
out2 = data.frame(QCmetrics,RIb,RIstde,RIt,RIp)
out2$p_corrected = p.adjust(out2$RIp,method = "holm")
colnames(out2) = c("Predictor", "Std_Beta", "Std_Err","T_val", "p_val","p_corr")
out2

### POST HOC ANALYSIS ####

# 1) analyse 

# there was strong variability for field strength and some for manufacturer
# we check the influence of both on subject 4:
manu_mod = lmer(pred_age~age+manufacturer+(1|fieldStrength)+(1|sliceThickness)+(1|siteID),THP_reg)
summary(manu_mod)
fieldS_mod = lmer(pred_age~age+fieldStrength+(1|fieldStrength)+(1|sliceThickness)+(1|siteID),THP_reg)
summary(fieldS_mod)
Thick_mod = lmer(pred_age~age+sliceThickness+(1|fieldStrength)+(1|sliceThickness)+(1|siteID),THP_reg)
summary(Thick_mod)

# We could analyse all data together. However, data set 2 has many more observations, giving unproportionally much weighting to subject 4
dat[1:38,1:7] = dat %>% filter(ID == 1) %>% select(c(efc, wm2max,snr_total, fber, fwhm_avg, cjv, cnr))%>% scale()
dat[39:78,1:7] = dat %>% filter(ID == 2) %>% select(c(efc, wm2max,snr_total, fber, fwhm_avg, cjv, cnr))%>% scale()
dat[79:103,1:7] = dat %>% filter(ID == 3) %>% select(c(efc, wm2max,snr_total, fber, fwhm_avg, cjv, cnr))%>% scale()
dat[104:nrow(dat),1:7] = dat %>% filter(ID == 4) %>% select(c(efc, wm2max,snr_total, fber, fwhm_avg, cjv, cnr))%>% scale()

dat$sliceThickness = c(replicate(nrow(dataset1),1), THP_reg$sliceThickness)
dat$siteID = c(replicate(nrow(dataset1),222),THP_reg$siteID)
dat$manufacturer = c(replicate(nrow(dataset1),1), THP_reg$manufacturer)
dat$manufacturer
levels(dat$manufacturer) = levels(THP_reg$manufacturer)

# we check the effect of acquisition parameters
final_mod = lmer(pred_age~age+manufacturer+fieldStrength+sliceThickness+(1|ID)+(1|siteID),dat)
summary(final_mod)


# further down: untick for single QC metrics effects on pooled data.
# 
# full_model = list()
# for ( i in 1:length(QCmetrics)){
#   f = formula(paste("pred_age~",QCmetrics[i],"+age+(1|fieldStrength)+(1|ID)+(1|siteID)"))
#   full_model[[i]] = lmer(f,data = dat)
#   # coefficients for the different QC metrics
#   RIb[i] = as.numeric(fixef(THP_model[[i]])[2])
#   RIstde[i] = summary(THP_model[[i]])$coefficients[2,2]
#   RIt[i]= summary(THP_model[[i]])$coefficients[2,4]
#   RIp[i]= summary(THP_model[[i]])$coefficients[2,5]
# }
# 
# # coefficients of the QC metrics
# out3 = data.frame(QCmetrics,RIb,RIstde,RIt,RIp)
# out3$p_corrected = p.adjust(out3$RIp,method = "holm")
# colnames(out3) = c("Predictor", "Std_Beta", "Std_Err","T_val", "p_val","p_corr")
# out3


### EXCLUDE REPEAT SCANS ####

# include only scans on inclusion list 
exclusion = inclusion %>% filter(exclude == "exclude")
exclusion$scanID = noquote(sub("Scan0", "", exclusion$scanName))
dat2 = data2[!data2$scanID %in% exclusion$scanID,]
dat2[1:nrow(dat2),1:7] = dat2 %>% select(c(efc, wm2max,snr_total, fber, fwhm_avg, cjv, cnr))%>% scale()

# QC params
for ( i in 1:length(QCmetrics)){
  f = formula(paste("pred_age~",QCmetrics[i],"+age+(1|fieldStrength)+(1|sliceThickness)+(1|siteID)")) 
  THP_model[[i]] = lmer(f,data = dat2)
  # coefficients for the different QC metrics
  RIb[i] = as.numeric(fixef(THP_model[[i]])[2])
  RIstde[i] = summary(THP_model[[i]])$coefficients[2,2]
  RIt[i]= summary(THP_model[[i]])$coefficients[2,4]
  RIp[i]= summary(THP_model[[i]])$coefficients[2,5]
}

# coefficients of the QC metrics
out2 = data.frame(QCmetrics,RIb,RIstde,RIt,RIp)
out2$p_corrected = p.adjust(out2$RIp,method = "holm")
colnames(out2) = c("Predictor", "Std_Beta", "Std_Err","T_val", "p_val","p_corr")
out2

# Acquisition params
manu_mod = lmer(pred_age~age+manufacturer+(1|fieldStrength)+(1|sliceThickness)+(1|siteID),dat2)
summary(manu_mod)
fieldS_mod = lmer(pred_age~age+fieldStrength+(1|fieldStrength)+(1|sliceThickness)+(1|siteID),dat2)
summary(fieldS_mod)
Thick_mod = lmer(pred_age~age+sliceThickness+(1|fieldStrength)+(1|sliceThickness)+(1|siteID),dat2)
summary(Thick_mod)
