library(tidyverse)
library(patchwork)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(RColorBrewer)
source('R/znbnVisuals.R')

odir <- "/home/bcowley/Dropbox/project_CENT/Publications/WIP - NFB Learning Curves/Figures"

colv <- brewer.pal(n = 12, name = "Paired")
defred = "#D6544B"
defblu = "#00AEB3"

#---- FIGURE FOR DES x LC SLOPE
des <- read.csv(file.path('data', 'ctsem_paper', 'DESxLC.csv'), sep = '\t')
dat <- read.csv(file.path('data', 'ctsem_paper', 'selfrepXLC.csv'))


## BIS x LC ----
eqn <- lm_eqn(dat, lm(slope.score ~ BIS, data = dat))
eqTB <- lm_eqn(dat, lm(slope.score ~ BIS, data = filter(dat, regime == "TBR")))
eqSM <- lm_eqn(dat, lm(slope.score ~ BIS, data = filter(dat, regime == "SMR")))

# ggplot(dat, aes(x = BIS, y = slope.score)) +
#   geom_point(aes(pch = regime)) +
#   geom_smooth(method = "lm") +
#   ylab("Slope of learning curve") +
#   ylim(-0.25, 0.6) +
#   annotate("text",
#            x = max(dat$BIS),
#            y = max(dat$slope.score),
#            hjust = 1,
#            label = eqn,
#            parse = TRUE) +
#   theme_minimal()
# ggsave(file.path(odir, "LCxBIS.svg"))

# ggplot(dat, aes(x = BIS, y = slope.score, color = learner)) +
#   geom_point(aes(pch = learner)) +
#   geom_smooth(method = "lm") +
#   ylab("Slope of learning curve") +
#   scale_colour_manual(values = colv[c(1, 2)]) +
#   theme_minimal()

lc.bis <- 
ggplot(dat, aes(x = BIS, y = slope.score, color = regime)) +
  geom_point(aes(pch = regime)) +
  geom_smooth(method = "lm") +
  ylab("Slope of learning curve") +
  ylim(-0.25, 0.7) +
  annotate("text",
           x = max(dat$BIS),
           y = max(dat$slope.score) + 0.17,
           hjust = 1,
           label = eqSM,
           parse = TRUE, 
           color = defred) +
  annotate("text",
           x = max(dat$BIS),
           y = max(dat$slope.score) + 0.1,
           hjust = 1,
           label = eqTB,
           parse = TRUE, 
           color = defblu) +
  theme_minimal()
# ggsave(file.path(odir, "LCxBISxRegime.svg"))


## DES x LC ----
eqn <- lm_eqn(dat, lm(slope.score ~ DES, data = dat))
eqTB <- lm_eqn(dat, lm(slope.score ~ DES, data = filter(dat, regime == "TBR")))
eqSM <- lm_eqn(dat, lm(slope.score ~ DES, data = filter(dat, regime == "SMR")))

# ggplot(dat, aes(x = DES, y = slope.score)) +
#   geom_point(aes(pch = regime)) +
#   geom_smooth(method = "lm") +
#   ylab("Slope of learning curve") +
#   ylim(-0.25, 0.6) +
#   annotate("text",
#            x = max(dat$DES),
#            y = max(dat$slope.score),
#            hjust = 1,
#            label = eqn,
#            parse = TRUE) +
#   theme_minimal()
# ggsave(file.path(odir, "LCxDES.svg"))

# ggplot(dat, aes(x = DES, y = slope.score, color = learner)) +
#   geom_point(aes(pch = learner)) +
#   geom_smooth(method = "lm") +
#   ylab("Slope of learning curve") +
#   scale_colour_manual(values = colv[c(1, 2)]) +
#   theme_minimal()

lc.des <- 
ggplot(dat, aes(x = DES, y = slope.score, color = regime)) +
  geom_point(aes(pch = regime)) +
  geom_smooth(method = "lm") +
  ylab("Slope of learning curve") +
  ylim(-0.25, 0.7) +
  ylab(NULL) +
  annotate("text",
           x = max(dat$DES),
           y = max(dat$slope.score) + 0.17,
           hjust = 1,
           label = eqSM,
           parse = TRUE, 
           color = defred) +
  annotate("text",
           x = max(dat$DES),
           y = max(dat$slope.score) + 0.1,
           hjust = 1,
           label = eqTB,
           parse = TRUE, 
           color = defblu) +
  theme_minimal()
# ggsave(file.path(odir, "LCxDESxRegime.svg"))


## GAD x LC ----
eqn <- lm_eqn(dat, lm(slope.score ~ GAD, data = dat))
eqTB <- lm_eqn(dat, lm(slope.score ~ GAD, data = filter(dat, regime == "TBR")))
eqSM <- lm_eqn(dat, lm(slope.score ~ GAD, data = filter(dat, regime == "SMR")))

# ggplot(dat, aes(x = GAD, y = slope.score)) +
#   geom_point(aes(pch = regime)) +
#   geom_smooth(method = "lm") +
#   ylab("Slope of learning curve") +
#   ylim(-0.25, 0.6) +
#   annotate("text",
#            x = max(dat$GAD),
#            y = max(dat$slope.score),
#            hjust = 1,
#            label = eqn,
#            parse = TRUE) +
#   theme_minimal()
# ggsave(file.path(odir, "LCxGAD.svg"))

# ggplot(dat, aes(x = GAD, y = slope.score)) +
#   geom_point(aes(pch = learner)) +
#   geom_smooth(method = "lm") +
#   ylab("Slope of learning curve") +
#   scale_colour_manual(values = colv[c(1, 2)]) +
#   theme_minimal()

lc.gad <-
ggplot(dat, aes(x = GAD, y = slope.score, color = regime)) +
  geom_point(aes(pch = regime)) +
  geom_smooth(method = "lm") +
  ylab("Slope of learning curve") +
  ylim(-0.25, 0.7) +
  ylab(NULL) +
  annotate("text",
           x = max(dat$GAD),
           y = max(dat$slope.score) + 0.17,
           hjust = 1,
           label = eqSM,
           parse = TRUE, 
           color = defred) +
  annotate("text",
           x = max(dat$GAD),
           y = max(dat$slope.score) + 0.1,
           hjust = 1,
           label = eqTB,
           parse = TRUE, 
           color = defblu) +
  theme_minimal()
# ggsave(file.path(odir, "LCxGADxRegime.svg"))

(lc.bis + lc.des + lc.gad) + 
  plot_layout(guides="collect") + 
  plot_annotation(title="Slope of learning curve by BIS, DES, and GAD reports", tag_levels = 'A', tag_suffix = '.')


ggsave(file.path(odir, "LCxBIS-DES-GADxRegime.svg"), width = 12, height = 4)
ggsave(file.path(odir, "LCxBIS-DES-GADxRegime.png"), width = 12, height = 4)




#### ASRS DATA ----
asrs <- read.csv(file.path('data', 'ctsem_paper', 'ASRSxLC.csv'), sep = '\t')
asrs$DES <- des$DES < 25
asrs$Learner <- as.factor(asrs$Learner)
levels(asrs$Learner) <- c("nonLearner", "Learner")
asrs$DES <- as.factor(asrs$DES)
levels(asrs$DES) <- c("DES≥25", "DES<25")

df <- asrs %>%
  pivot_longer(cols = contains(c("inattention", "hyper")), names_to = "ASRS")
df$ASRS <- as.factor(df$ASRS)
df$status.ASRS <- with(df, Learner:ASRS)
df$DES.ASRS <- with(df, DES:ASRS)

df0 <- asrs %>%
  pivot_longer(cols = contains("asrs"), 
               names_to = c("deficit", "time"),
               names_pattern = "asrs_(.*)_(.*)",
               values_to = "ASRS")
asrs$deficit <- as.factor(asrs$deficit)
asrs$time <- as.factor(asrs$time)


#---- FIGURE FOR ASRS x LEARNER STATUS ----
# ggplot(df, aes(x = status.ASRS, y = value)) +
#   geom_jitter(aes(fill=Regime), width = 0.2, height = 0.1, size=2, shape=21, stroke=0) +
#   geom_boxplot(width=0.2, outlier.shape = NA, alpha = 0.4) +
#   labs(title="ASRS factors before and after 30 sessions of treatment", x="ASRS", y = "Score") +
#   theme_minimal() +
#   theme(
#     axis.title.x = element_text(size=11),
#     axis.title.y= element_text(size=11),
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )
asrs.lc <-
ggplot(df0, aes(x = time, y = ASRS)) +
  geom_jitter(aes(fill=Regime), width = 0.2, height = 0.1, size=2, shape=21, stroke=0) +
  geom_boxplot(width=0.2, outlier.shape = NA, alpha = 0.4) +
  facet_wrap(deficit~Learner, ncol = 4, strip.position = "bottom") +
  labs(x=NULL, y = "ASRS score") +
  theme_minimal() +
  theme(strip.placement = "outside")
# ggsave(file.path(odir, "LCxASRS.svg"))


#---- FIGURE FOR ASRS x DES LEVEL ----
# ggplot(df, aes(x = DES.ASRS, y = value)) +
#   geom_jitter(aes(fill=Regime), width = 0.2, height = 0.1, size=2, shape=21, stroke=0) +
#   geom_boxplot(width=0.2, outlier.shape = NA, alpha = 0.4) +
#   labs(title="ASRS factors before and after 30 sessions of treatment", x="ASRS", y = "Score") +
#   theme_minimal() +
#   theme(
#     axis.title.x = element_text(size=11),
#     axis.title.y= element_text(size=11),
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )
asrs.des <-
ggplot(df0, aes(x = time, y = ASRS)) +
  geom_jitter(aes(fill=Regime), width = 0.2, height = 0.1, size=2, shape=21, stroke=0) +
  geom_boxplot(width=0.2, outlier.shape = NA, alpha = 0.4) +
  facet_wrap(deficit~DES, ncol = 4, strip.position = "bottom") +
  labs(x=NULL, y = NULL) +
  theme_minimal() +
  theme(strip.placement = "outside")
# ggsave(file.path(odir, "DESxASRS.svg"))

(asrs.lc + asrs.des) + 
  plot_layout(guides="collect") + 
  plot_annotation(title="ASRS factors before and after 30 sessions of treatment", tag_levels = 'A', tag_suffix = '.')

ggsave(file.path(odir, "ASRSxLC-DESxRegime.svg"), width = 12, height = 4)
ggsave(file.path(odir, "ASRSxLC-DESxRegime.png"), width = 12, height = 4)
