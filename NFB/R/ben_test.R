library(tidyverse)
library(readxl)
library(here)

datpth <- file.path(here(), "data")

#### TABLE 1 ----
tab <- read.csv(file.path(datpth, 'ctsem_paper', 'table1.csv'))

rgm_age_aov <- aov(Age ~ Regime, tab)
summary(rgm_age_aov)
TukeyHSD(rgm_age_aov)

lrn_age_aov <- aov(Age ~ Status, tab)
summary(lrn_age_aov)
TukeyHSD(lrn_age_aov)

#### NFB OUTCOMES ----
nfb <- read_excel(file.path(datpth, "NFBTrainingMasterDatasheet_2013-08-21.xlsx"), sheet = "TOVA_diff")

df <- nfb %>% 
  dplyr::select(c(1:51, ends_with('T'))) %>% 
  filter(!is.na(TOVA_SESNUM)) %>% # this filters out the dropouts
  mutate(ASRS_diff = case_when(Learner == "PWL" ~ post_ASRS_total - pre_ASRS_total, 
                               Learner == "Learner" | Learner == "Non-Learner" ~ s30_ASRS_total - pre_ASRS_total), 
         ASRSi_diff = case_when(Learner == "PWL" ~ post_ASRS_inattention_score - pre_ASRS_inattention_score, 
                                Learner == "Learner" | Learner == "Non-Learner" ~ s30_ASRS_inattention_score - pre_ASRS_inattention_score), 
         ASRSh_diff = case_when(Learner == "PWL" ~ post_ASRS_hyperactivity_impulsivity_score - pre_ASRS_hyperactivity_impulsivity_score,
                                Learner == "Learner" | Learner == "Non-Learner" ~ s30_ASRS_hyperactivity_impulsivity_score - pre_ASRS_hyperactivity_impulsivity_score))

#### NON-STANDARDISED TOVA VARS ----
dpaov <- aov(DPRIMET ~ Learner, df)
summary(dpaov)
TukeyHSD(dpaov)

omaov <- aov(OMERRT ~ Learner, df)
summary(omaov)
TukeyHSD(omaov)

cmaov <- aov(COMERRT ~ Learner, df)
summary(cmaov)
TukeyHSD(cmaov)

rtaov <- aov(RTMEANT ~ Learner, df)
summary(rtaov)
TukeyHSD(rtaov)

rvaov <- aov(RTVART ~ Learner, df)
summary(rvaov)
TukeyHSD(rvaov)


#### STANDARDISED TOVA VARS ----
dpaov <- aov(DPRSST ~ Learner, df)
summary(dpaov)
TukeyHSD(dpaov)

omaov <- aov(OMSST ~ Learner, df)
summary(omaov)
TukeyHSD(omaov)

cmaov <- aov(COMSST ~ Learner, df)
summary(cmaov)
TukeyHSD(cmaov)

rtaov <- aov(RTMSST ~ Learner, df)
summary(rtaov)
TukeyHSD(rtaov)

rvaov <- aov(VARSST ~ Learner, df)
summary(rvaov)
TukeyHSD(rvaov)

#### EXGUASS TOVA VARS (PREVIOUSLY NOT TESTED) ----
XMaov <- aov(EXGMUT ~ Learner, df)
summary(XMaov)
TukeyHSD(XMaov)

XSaov <- aov(EXGSIGT ~ Learner, df)
summary(XSaov)
TukeyHSD(XSaov)

XTaov <- aov(EXGTAUT ~ Learner, df)
summary(XTaov)
TukeyHSD(XTaov)


#### ASRS ----
ASRSaov <- aov(ASRS_diff ~ Learner, df)
summary(ASRSaov)
TukeyHSD(ASRSaov)

ASRSIaov <- aov(ASRSi_diff ~ Learner, df)
summary(ASRSIaov)
TukeyHSD(ASRSIaov)

ASRSHaov <- aov(ASRSh_diff ~ Learner, df)
summary(ASRSHaov)
TukeyHSD(ASRSHaov)




#### Looking at sessions data ----
ssnpth <- file.path(datpth, "CENT_DB_2013-08-27", "Sessions")

ssn <- read.csv(file.path(ssnpth, "daily_session.csv"), sep = ",", header = TRUE, dec = ".")
blk <- read.csv(file.path(ssnpth, 'auto_block.csv'), sep=";", header=1)

gudf <- blk[blk$reject_filter==0,]

boxplot(score ~ patient, data=df)
boxplot(score ~ patient, data=gudf)
boxplot(adj_score ~ patient, data=gudf)

boxplot(score ~ gametype, data=df)
boxplot(score ~ gametype, data=gudf)
boxplot(adj_score ~ gametype, data=gudf)

boxplot(score ~ trialtype, data=df)
boxplot(score ~ trialtype, data=gudf)
boxplot(adj_score ~ trialtype, data=gudf)


#### CTSEM testing, never started! ----
# install.packages("ctsem")
# library(ctsem)

