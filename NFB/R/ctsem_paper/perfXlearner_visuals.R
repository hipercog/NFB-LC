library(tidyverse)
library(patchwork)
library(viridis)
library(RColorBrewer)
source('R/znbnVisuals.R')
source('R/LC_analysis_functions.R')
source('R/LC_visuals_functions.R')

odir <- "/home/bcowley/Dropbox/project_CENT/Publications/WIP - NFB Learning Curves/Figures/"

par(mfrow = c(2, 2))

dat <- read.csv(file.path('data', 'ctsem_paper', 'scoreXregimeXlearner.csv'), sep = '\t')
df <- dat %>% 
  filter(session_num > 2) %>%
  group_by(Regime) %>%
  mutate(z.norm.score = scale(norm_score)) %>%
  ungroup()

aggv <- 'z.norm.score'
byv <- 'session_num'
colv <- brewer.pal(n = 12, name = "Paired")
colv <- paste0(colv, "70")

lrnTB <- getTrialsMCI(filter(df, Regime == "TBR", Learner == "Learner"), byv, aggv)
lrnSMR <- getTrialsMCI(filter(df, Regime == "SMR", Learner == "Learner"), byv, aggv)
nlrnTB <- getTrialsMCI(filter(df, Regime == "TBR", Learner == "NonLearner"), byv, aggv)
nlrnSMR <- getTrialsMCI(filter(df, Regime == "SMR", Learner == "NonLearner"), byv, aggv)

# plot_4_meanCI(x1s, ml1, ci1, 
#               x2s, ml2, ci2, 
#               x3s, ml3, ci3, 
#               x4s, ml4, ci4, 
#               xlabel, ylabel, lgdstr, colgrp)
ylbl <- "adjusted score"
xs <- seq(3,40)
plot.scoreTB <-
plot_2_meanCI(xs, lrnTB$M[[aggv]], lrnTB$CI, 
              xs, nlrnTB$M[[aggv]], nlrnTB$CI, 
              "Session", ylbl,
              c("Learner:TB", "nonLearner:TB"),
              colv[c(2, 1, 4, 3)])

plot.scoreSMR <-
plot_2_meanCI(xs, lrnSMR$M[[aggv]], lrnSMR$CI, 
              xs, nlrnSMR$M[[aggv]], nlrnSMR$CI, 
              "Session", ylbl,
              c("Learner:SMR", "nonLearner:SMR"),
              colv[c(10, 9, 12, 11)])
# plot.score <-
# plot_4_meanCI(xs, lrnTB$M[[aggv]], lrnTB$CI, 
#               xs, nlrnTB$M[[aggv]], nlrnTB$CI, 
#               xs, lrnSMR$M[[aggv]], lrnSMR$CI, 
#               xs, nlrnSMR$M[[aggv]], nlrnSMR$CI, 
#               "Session", ylbl, 
#               c("Learner:TB", "nonLearner:TB", "Learner:SMR", "nonLearner:SMR"), 
#               colv[c(2, 1, 4, 3, 10, 9, 12, 11)],
#               SMTH = TRUE, smthk = 3)
ggsave(paste0(odir, "learnerXregimeXperf.svg"))


#### TRY ANOTHER APPROACH WITH TBR
daily.ssn <- read.csv(file.path('data', 'CENT_DB_2013-08-27', 'Sessions', 'daily_session.csv'), sep = ';')
df1 <- dat %>% select(patient, Regime, Learner) %>% 
  unique() %>% 
  inner_join(daily.ssn %>% filter(session_num > 2, session_num < 40))

aggv1 <- 'TB_ratio'

lrnTB.1 <- getTrialsMCI(filter(df1, Regime == "TBR", Learner == "Learner"), byv, aggv1)
lrnSMR.1 <- getTrialsMCI(filter(df1, Regime == "SMR", Learner == "Learner"), byv, aggv1)
nlrnTB.1 <- getTrialsMCI(filter(df1, Regime == "TBR", Learner == "NonLearner"), byv, aggv1)
nlrnSMR.1 <- getTrialsMCI(filter(df1, Regime == "SMR", Learner == "NonLearner"), byv, aggv1)

# plot_4_meanCI(x1s, ml1, ci1, 
#               x2s, ml2, ci2, 
#               x3s, ml3, ci3, 
#               x4s, ml4, ci4, 
#               xlabel, ylabel, lgdstr, colgrp)
ylb1 <- "theta-beta ratio"
xs1 <- seq(3,39)
plot.tbr <-
plot_4_meanCI(xs1, lrnTB.1$M[[aggv1]], lrnTB.1$CI, 
              xs1, nlrnTB.1$M[[aggv1]], nlrnTB.1$CI, 
              xs1, lrnSMR.1$M[[aggv1]], lrnSMR.1$CI, 
              xs1, nlrnSMR.1$M[[aggv1]], nlrnSMR.1$CI, 
              "Session", ylb1, 
              c("Learner:TB", "nonLearner:TB", "Learner:SMR", "nonLearner:SMR"), 
              colv[c(2, 1, 4, 3, 10, 9, 12, 11)],
              SMTH = TRUE, smthk = 3)

dev.copy(png, file.path(odir, 'ssn-wise.png'))
dev.off()


#### DO IT THE MODERN WAY ----
scores <- rbind(
  rbind(
    inner_join(lrnTB$M, lrnTB$CI) %>% mutate(Learner = "Learner", Regime = "TBR"),
    inner_join(nlrnTB$M, nlrnTB$CI) %>% mutate(Learner = "nonLearner", Regime = "TBR")),
  rbind(
    inner_join(lrnSMR$M, lrnSMR$CI) %>% mutate(Learner = "Learner", Regime = "SMR"),
    inner_join(nlrnSMR$M, nlrnSMR$CI) %>% mutate(Learner = "nonLearner", Regime = "SMR")))

scores <- scores %>%
  group_by(Learner, Regime) %>%
  mutate(smu.scor = runmed(z.norm.score, k = 3), smu.loCI = runmed(V4, k = 3), smu.hiCI = runmed(V5, k = 3)) %>%
  ungroup()

tbres <- rbind(
  rbind(
    inner_join(lrnTB.1$M, lrnTB.1$CI) %>% mutate(Learner = "Learner", Regime = "TBR"),
    inner_join(nlrnTB.1$M, nlrnTB.1$CI) %>% mutate(Learner = "nonLearner", Regime = "TBR")),
  rbind(
    inner_join(lrnSMR.1$M, lrnSMR.1$CI) %>% mutate(Learner = "Learner", Regime = "SMR"),
    inner_join(nlrnSMR.1$M, nlrnSMR.1$CI) %>% mutate(Learner = "nonLearner", Regime = "SMR"))) 

tbres <- tbres %>%
  group_by(Learner, Regime) %>%
  mutate(smu.tbr = runmed(TB_ratio, k = 3), smu.loCI = runmed(V4, k = 3), smu.hiCI = runmed(V5, k = 3)) %>%
  ungroup()

plot.score <-
  ggplot(data = scores, aes(x = session_num, y = smu.scor, line = Learner, color = Learner)) +
  geom_ribbon(aes(ymin = smu.loCI, ymax = smu.hiCI, fill = Learner), alpha = 0.2, size = 0) +
  geom_line(size = 1, alpha = 0.5) +
  geom_smooth(aes(fill = Learner)) +
  facet_grid(~Regime) +
  xlab(NULL) +
  ylab("standardised\nscore") +
  theme_minimal() +
  theme(axis.text.x=element_blank())

plot.TBR <-
  ggplot(data = tbres, aes(x = session_num, y = smu.tbr, line = Learner, color = Learner)) +
  geom_ribbon(aes(ymin = smu.loCI, ymax = smu.hiCI, fill = Learner), alpha = 0.2, size = 0) +
  geom_line(size = 1, alpha = 0.5) +
  geom_smooth(aes(fill = Learner)) +
  facet_wrap(~Regime) +
  xlab("Session number") +
  ylab(ylb1) +
  theme_minimal() +
  theme(strip.text.x = element_blank())

# plot.score <-
#   ggplot(data = scores, aes(x = session_num, y = z.norm.score, line = Learner, color = Learner)) +
#   geom_ribbon(aes(ymin = V4, ymax = V5, fill = Learner), alpha = 0.2, size = 0) +
#   geom_line(size = 1, alpha = 0.5) +
#   geom_smooth(aes(fill = Learner)) +
#   facet_grid(~Regime) +
#   xlab(NULL) +
#   ylab("standardised\nscore") +
#   theme_minimal() +
#   theme(axis.text.x=element_blank())
# 
# plot.TBR <-
# ggplot(data = tbres, aes(x = session_num, y = TB_ratio, line = Learner, color = Learner)) +
#   geom_ribbon(aes(ymin = V4, ymax = V5, fill = Learner), alpha = 0.2, size = 0) +
#   geom_line(size = 1, alpha = 0.5) +
#   geom_smooth(aes(fill = Learner)) +
#   facet_wrap(~Regime) +
#   xlab("Session number") +
#   ylab(ylb1) +
#   theme_minimal() +
#   theme(strip.text.x = element_blank())

(plot.score / plot.TBR) + 
  plot_layout(guides="collect") + 
  plot_annotation(tag_levels = 'A', tag_suffix = '.')
