# Travelling human phantom brain age data analysis
# by Max Korbmacher (max.korbmacher@gmail.com)
# code originally run on R version 4.1.2 (2021-11-01)

# PREP ####
# load MRIQC metrics table
MRIQC = read.csv("/home/max/Documents/Projects/Brain_Age/DL/Travelling_Human_Phantom/out.csv")
# load brain age predictions
predictions = read.csv("/home/max/Documents/Projects/Brain_Age/DL/Travelling_Human_Phantom/predictions.csv")
# load inclusion list
inclusion = read.csv("/home/max/Documents/Projects/Brain_Age/DL/Travelling_Human_Phantom/inclusion_list.csv")

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
#install.packages("ppcor")
library(ppcor)
#install.packages("cocor")
library(cocor)

# first give MRIQC scans a counter vector (are in the right order from 1 to n)
MRIQC$scanID = seq(1:nrow(MRIQC))

# NOTE: In the first run, we DID NOT REMOVE volumes which had the same acquisition place, time, parameters... If that is of interest, check the end of the script.

# then merge QC metrics with brain age prediction data
data = merge(MRIQC, predictions, by = "scanID")

#### MAIN ANALYSIS (RESULTS SECTION) ####
# BrainAge-Age Correlations ####

# correlate true with predicted age
cor.test(data$prediction, data$age)

# check the effect of scannerID, fieldStrength, and sliceThickness
model = lm(prediction ~ age + fieldStrength + scannerID , data = data)
summary(model)
lm.beta(model) # for standardised betas

# correlations for different field strength (1.5T & 3T), if of interest:
T1_5 = data %>% filter(fieldStrength == 1.5)
T3 = data %>% filter(fieldStrength == 3)
pcor_ci.test(T1_5$age, T1_5$prediction, T1_5$scannerID)
pcor_ci.test(T3$age, T3$prediction, T3$scannerID)

# check correlation when holding scanner site and field strength constant
# create function for that
pcor_ci.test <-
  function (x, y, z, method = c("pearson", "kendall", "spearman"), conf.level = 0.95, ...) {
    d1 <- deparse(substitute(x))
    d2 <- deparse(substitute(y))
    d3 <- deparse(substitute(z))
    data.name <- paste0(d1, " and ", d2, "; controlling: ", d3)
    method <- match.arg(method)
    Method <- paste0("Partial correlation (", method, ")")
    alternative <- "true partial correlation is not equal to 0"
    
    x <- as.vector(x)
    y <- as.vector(y)
    z <- as.data.frame(z)
    xyz <- data.frame(x, y, z)
    pcor <- ppcor::pcor(xyz, method = method)
    estimate <- pcor$est[1, 2]
    p.value <- pcor$p.value[1, 2]
    parameter <- c(n = pcor$n, gp = pcor$gp)
    statistic <- c(Stat = pcor$statistic[1, 2])
    
    fit1 <- lm(x ~ z, data = xyz)
    fit2 <- lm(y ~ z, data = xyz)
    cortest <- cor.test(resid(fit1), resid(fit2), method = method, conf.level = conf.level, ...)
    ci <- cortest$conf.int
    
    ht <- list(
      statistic = statistic,
      parameter = parameter,
      p.value = p.value,
      estimate = c(partial.cor = estimate),
      alternative = alternative,
      method = Method,
      data.name = data.name,
      conf.int = ci
    )
    class(ht) <- "htest"
    ht
  }

# calculate confidence interval around correlation coefficient
pcor_ci.test(data$age, data$prediction, data$fieldStrength)

# calculate error for data
mae(data$age, data$prediction)
rmse(data$age, data$prediction)

# MRIQC, age and brain age (and covars) ####
data2 = data %>% dplyr::select(prediction,efc, wm2max, snr_total, fber,fwhm_avg, cnr,cjv,  snrd_total, age, scannerID, fieldStrength,sliceThickness)
QC_model_1 = lm(prediction~., data = data2)
summary(QC_model_1)
lm.beta(QC_model_1)

# observing bivariate relationship of QC metrics of interest as defined by data set 1 analyses
bivar1 = lm(prediction~wm2max+age+scannerID +fieldStrength + sliceThickness, data = data)
bivar2 = lm(prediction~fwhm_avg+age+scannerID +fieldStrength + sliceThickness, data = data)
bivar3 = lm(prediction~efc+age+scannerID +fieldStrength + sliceThickness, data = data)
bivar4 = lm(prediction~fber+age+scannerID +fieldStrength + sliceThickness, data = data)
bivar5 = lm(prediction~snr_total+age+scannerID +fieldStrength + sliceThickness, data = data)
summary(bivar5)
lm.beta(bivar5)

model0 = lm(prediction~age+scannerID +fieldStrength, data = data2)
summary(model0)
anova(model0,model)

#### SUPPLEMENTARY ANALYSIS ####
# Analyses with exclusions ####

# apply exclusions and create new 'data' df
exclusion = inclusion %>% filter(exclude == "exclude")
exclusion$scanID = noquote(sub("Scan0", "", exclusion$scanName))
predictions = predictions[!predictions$scanID %in% exclusion$scanID,]
MRIQC = MRIQC[!MRIQC$scanID %in% exclusion$scanID,]
data = merge(MRIQC, predictions, by = "scanID")

# correlate true with predicted age
cor.test(data$prediction, data$age)

# check the effect of field strength
model = lm(prediction ~ age + fieldStrength + scannerID, data = data)
summary(model)
lm.beta(model) # for standardised betas


# correlations for different field strength (1.5T & 3T), if of interest:
T1_5 = data %>% filter(fieldStrength == 1.5)
T3 = data %>% filter(fieldStrength == 3)
pcor_ci.test(T1_5$age, T1_5$prediction, T1_5$scannerID)
pcor_ci.test(T3$age, T3$prediction, T3$scannerID)

# calculate confidence interval around correlation coefficient
pcor_ci.test(data$age, data$prediction, data$fieldStrength)

# calculate error for data
mae(data$age, data$prediction)
rmse(data$age, data$prediction)

# MRIQC, age and brain age ####
data1 = data %>% dplyr::select(prediction,age, scannerID, fieldStrength, sliceThickness)
QC_model_1 = lm(prediction~., data = data1)
summary(QC_model_1)
lm.beta(QC_model_1)

data2 = data %>% dplyr::select(prediction,efc, wm2max, snr_total, fber,fwhm_avg, cnr,cjv,  snrd_total, age, scannerID, fieldStrength, sliceThickness)
QC_model_1 = lm(prediction~., data = data2)
summary(QC_model_1)
lm.beta(QC_model_1)

bivar1 = lm(prediction~wm2max+age+scannerID +fieldStrength + sliceThickness, data = data)
bivar2 = lm(prediction~fwhm_avg+age+scannerID +fieldStrength + sliceThickness, data = data)
bivar3 = lm(prediction~efc+age+scannerID +fieldStrength + sliceThickness, data = data)
bivar4 = lm(prediction~fber+age+scannerID +fieldStrength + sliceThickness, data = data)
bivar5 = lm(prediction~snr_total+age+scannerID +fieldStrength + sliceThickness, data = data)
summary(bivar2)
lm.beta(bivar2)
