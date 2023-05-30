library(tidyverse)
library(ggplot2)
library(MASS)
library(gam)
library(HLMdiag)
library(merTools)
library(party)
library(LMERConvenienceFunctions)
library(lme4)
library(lmerTest)
library(glmnet)
library(spm2)
library(HLMdiag)
library(glmmLasso)
library(ggparty)
library(patchwork)

setwd("/Users/Nathan/iCloudDrive/STAT 998/Federico/code/")
load("eda.image")

gg_df <- meta_df %>%
  mutate(idx=1:nrow(meta_df))

p1 <- ggplot(gg_df, aes(x=idx, y=Number_of_cattle_owned)) +
  geom_point(color=ifelse(gg_df$Number_of_cattle_owned > 40, "red", "black")) +
  geom_text(aes(label=ifelse(Number_of_cattle_owned > 40, idx, "")), nudge_x=40) +
  labs(x="Index", y="Number of Cattle Owned")
p2 <- ggplot(gg_df, aes(x=idx, y=Number_of_donkeys_owned)) +
  geom_point(color=ifelse(gg_df$Number_of_donkeys_owned > 10, "red", "black")) +
  geom_text(aes(label=ifelse(Number_of_donkeys_owned > 10, idx, "")), nudge_x=40) +
  labs(x="Index", y="Number of Donkeys Owned")
p3 <- ggplot(gg_df, aes(x=idx, y=shannon_entropy)) +
  geom_point(color=ifelse(gg_df$shannon_entropy < 3, "red", "black")) +
  geom_text(aes(label=ifelse(shannon_entropy < 3, idx, "")), nudge_x=40) +
  labs(x="Index", y="Shannon Entropy")

p1 + p2 + p3



p1 <- ggplot(influence, aes(x=id, y=cooksd)) +
  geom_point(color=influence$gg_cooks_color) +
  labs(x="Index", y="MDFFITS / Cooks Distance") +
  geom_text(aes(label=gg_cooks_label), nudge_x=50) +
  guides(color=F) +
  geom_hline(yintercept=0.019, color="red", linetype="dashed")
p2 <- ggplot(influence, aes(x=id, y=leverage.overall)) +
  geom_point(color=influence$gg_lev_color) +
  geom_text(aes(label=gg_lev_label), nudge_x=50) +
  labs(x="Index", y="Leverage") +
  geom_hline(yintercept=0.0465, color="red", linetype="dashed")
p3 <- ggplot(influence, aes(x=id, y=dfbeta)) +
  geom_point(color=influence$gg_dfbeta_color) +
  geom_text(aes(label=gg_dfbeta_label), nudge_x=50) +
  guides(color=F) +
  geom_hline(yintercept=c(0.004, -0.004), color="red", linetype="dashed") +
  labs(x="Index", y="DFBETA")

p1 + p2 + p3

