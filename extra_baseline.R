#get summary stats 
library(emmeans)
library(car)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(jitter_ddp, type="common")
model1<-lm(jitter_ddp ~ age_screening + sex + testing_location+ age_learned_english + factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[5]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = jitter_ddp, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "F0 Group", x ="", y = "F0",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp

#get summary stats 
library(emmeans)
library(car)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(medium_pause_duration, type="common")
model1<-lm(medium_pause_duration ~ age_screening + sex + testing_location + age_learned_english+ factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = medium_pause_duration, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "1-2 second pause duration", x ="", y = "Seconds",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=FALSE, 
              annotations = c("*"))
bxp
eta_squared <- ancova[["Sum Sq"]][6] / sum(ancova[["Sum Sq"]])

WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(jitter_ddp, type="common")
model1<-lm(jitter_ddp ~ age_screening + sex + testing_location+ factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[5]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = jitter_ddp, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "F0 Group", x ="", y = "F0 Variance",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp

#### JITTER 
#(ddp)

WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(jitter_ddp, type="common")
model1<-lm(jitter_ddp ~ age_screening + sex + testing_location+age_learned_english+factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = jitter_ddp, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Average change in jitter", x ="", y = "%",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp
eta_squared <- ancova[["Sum Sq"]][6] / sum(ancova[["Sum Sq"]])

jitterddp.lm <- lm(jitter_ddp ~ Baseline_HAMD + sex + age_screening + testing_location + age_learned_english, data = MDD_feeling_BL)
# Create plot
jitterddp.scatter <- ggplot(MDD_feeling_BL, aes(y = jitter_ddp, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(data = data, method = "lm", formula = y ~ x, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "%", title = "Average change in jitter") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
jitterddp.scatter <- jitterddp.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(jitterddp.lm)$coefficients[2,1], 4),
                         ", p = ", signif(summary(jitterddp.lm)$coefficients[2, 4], 2)))
jitterddp.scatter


#Local

WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(jitter_local, type="common")
model1<-lm(jitter_local ~ age_screening + sex + testing_location+ age_learned_english+ factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = jitter_local, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Local jitter", x ="", y = "%",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp
eta_squared <- ancova[["Sum Sq"]][6] / sum(ancova[["Sum Sq"]])

jitterlocal.lm <- lm(jitter_local ~ Baseline_HAMD + sex + age_screening + testing_location+age_learned_english, data = MDD_feeling_BL)
# Create plot
jitterlocal.scatter <- ggplot(MDD_feeling_BL, aes(y = jitter_local, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "%", title = "Local jitter") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
jitterlocal.scatter <- jitterlocal.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(jitterlocal.lm)$coefficients[2,1], 5),
                         ", p = ", signif(summary(jitterlocal.lm)$coefficients[2, 4], 2)))
jitterlocal.scatter
summary(jitterlocal.lm)


#Local absolute

WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(jitter_local_absolute, type="common")
model1<-lm(jitter_local_absolute ~ age_screening + sex + testing_location+ age_learned_english+ factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = jitter_local_absolute, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Local absolute jitter", x ="", y = "%",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp
eta_squared <- ancova[["Sum Sq"]][6] / sum(ancova[["Sum Sq"]])

jitterlocal.lm <- lm(jitter_local ~ Baseline_HAMD + sex + age_screening + testing_location+age_learned_english, data = MDD_feeling_BL)
# Create plot
jitterlocal.scatter <- ggplot(MDD_feeling_BL, aes(y = jitter_local, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "%", title = "Local jitter") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
jitterlocal.scatter <- jitterlocal.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(jitterlocal.lm)$coefficients[2,1], 5),
                         ", p = ", signif(summary(jitterlocal.lm)$coefficients[2, 4], 2)))
jitterlocal.scatter
summary(jitterlocal.lm)



#Relative distal jitter

WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(jitter_ppq5, type="common")
model1<-lm(jitter_ppq5 ~ age_screening + sex + testing_location+ age_learned_english+factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = jitter_ppq5, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Relative distal jitter", x ="", y = "%",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=FALSE, 
              annotations = c("*"))
bxp
eta_squared <- ancova[["Sum Sq"]][6] / sum(ancova[["Sum Sq"]])


jitterppq5.lm <- lm(jitter_ppq5 ~ Baseline_HAMD + sex + age_screening + testing_location+ age_learned_english, data = MDD_feeling_BL)
# Create plot
jitterppq5.scatter <- ggplot(MDD_feeling_BL, aes(y = jitter_ppq5, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "%", title = "Relative distal jitter") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
jitterppq5.scatter <- jitterppq5.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(jitterlocal.lm)$coefficients[2,1], 5),
                         ", p = ", signif(summary(jitterlocal.lm)$coefficients[2, 4], 2)))
jitterppq5.scatter
summary(jitterppq5.lm)


# Jitter rap (relative proximal jitter)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(jitter_rap, type="common")
model1<-lm(jitter_rap ~ age_screening + sex + testing_location+ age_learned_english +factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = jitter_rap, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Relative Proximal Jitter", x ="", y = "%",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp
eta_squared <- ancova[["Sum Sq"]][6] / sum(ancova[["Sum Sq"]])


jitterrap.lm <- lm(jitter_rap ~ Baseline_HAMD + sex + age_screening + testing_location + age_learned_english, data = MDD_feeling_BL)
# Create plot
jitterrap.scatter <- ggplot(MDD_feeling_BL, aes(y = jitter_rap, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "%", title = "Relative Proxmial Jitter") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
jitterrap.scatter <- jitterrap.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(jitterrap.lm)$coefficients[2,1], 5),
                         ", p = ", signif(summary(jitterrap.lm)$coefficients[2, 4], 2)))
jitterrap.scatter
summary(jitterrap.lm)

#### SHIMMER

# Shimmer apq11 (relative distal shimmer)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(shimmer_apq11, type="common")
model1<-lm(shimmer_apq11 ~ age_screening + sex + testing_location+ age_learned_english + factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = shimmer_apq11, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Relative distal shimmer", x ="", y = "apq",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp
eta_squared <- ancova[["Sum Sq"]][6] / sum(ancova[["Sum Sq"]])

shimmerapq11.lm <- lm(shimmer_apq11 ~ Baseline_HAMD + sex + age_screening + testing_location+ age_learned_english, data = MDD_feeling_BL)
# Create plot
shimmerapq11.scatter <- ggplot(MDD_feeling_BL, aes(y = shimmer_apq11, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "%", title = "Relative distal shimmer") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
shimmerapq11.scatter <- shimmerapq11.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(shimmerapq11.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(shimmerapq11.lm)$coefficients[2, 4], 2)))
shimmerapq11.scatter
summary(shimmerapq11.lm)

# Shimmer apq3 (relative proximal shimmer)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(shimmer_apq3, type="common")
model1<-lm(shimmer_apq3 ~ age_screening + sex + testing_location+ age_learned_english+factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = shimmer_apq3, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Relative Proximal Shimmer", x ="", y = "apq",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp
eta_squared <- ancova[["Sum Sq"]][6] / sum(ancova[["Sum Sq"]])

shimmerapq3.lm <- lm(shimmer_apq3 ~ Baseline_HAMD + sex + age_screening + testing_location+ age_learned_english, data = MDD_feeling_BL)
# Create plot
shimmerapq3.scatter <- ggplot(MDD_feeling_BL, aes(y = shimmer_apq3, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "%", title = "Relative Distal Shimmer") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
shimmerapq3.scatter <- shimmerapq3.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(shimmerapq3.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(shimmerapq3.lm)$coefficients[2, 4], 2)))
shimmerapq3.scatter
summary(shimmerapq3.lm)

# Shimmer apq5 (relative  shimmer)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(shimmer_apq5, type="common")
model1<-lm(shimmer_apq5 ~ age_screening + sex + testing_location + age_learned_english +factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = shimmer_apq5, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Relative Shimmer", x ="", y = "apq",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp

shimmerapq5.lm <- lm(shimmer_apq5 ~ Baseline_HAMD + sex + age_screening + testing_location+ age_learned_english, data = MDD_feeling_BL)
# Create plot
shimmerapq3.scatter <- ggplot(MDD_feeling_BL, aes(y = shimmer_apq5, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "%", title = "Relative shimmer") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
shimmerapq5.scatter <- shimmerapq3.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(shimmerapq5.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(shimmerapq5.lm)$coefficients[2, 4], 2)))
shimmerapq5.scatter
summary(shimmerapq5.lm)

# Shimmer dda (average change in shimmer)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(shimmer_dda, type="common")
model1<-lm(shimmer_dda ~ age_screening + sex + testing_location + age_learned_english+factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = shimmer_dda, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Relative Shimmer", x ="", y = "apq",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp
eta_squared <- ancova[["Sum Sq"]][6] / sum(ancova[["Sum Sq"]])

shimmerdda.lm <- lm(shimmer_dda ~ Baseline_HAMD + sex + age_screening + testing_location + age_learned_english, data = MDD_feeling_BL)
# Create plot
shimmerdda.scatter <- ggplot(MDD_feeling_BL, aes(y = shimmer_dda, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "%", title = "Relative Shimmer") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
shimmerapq5.scatter <- shimmerdda.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(shimmerdda.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(shimmerdda.lm)$coefficients[2, 4], 2)))
shimmerapq5.scatter
summary(shimmerdda.lm)

# Shimmer local 
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(shimmer_local, type="common")
model1<-lm(shimmer_local ~ age_screening + sex + testing_location + age_learned_english+factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = shimmer_loca, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Local Shimmer", x ="", y = "shimmer",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp
eta_squared <- ancova[["Sum Sq"]][6] / sum(ancova[["Sum Sq"]])

shimmerlocal.lm <- lm(shimmer_local ~ Baseline_HAMD + sex + age_screening + testing_location+ age_learned_english, data = MDD_feeling_BL)
# Create plot
shimmerdda.scatter <- ggplot(MDD_feeling_BL, aes(y = shimmer_local, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "%", title = "Relative Shimmer") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
shimmerapq5.scatter <- shimmerdda.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(shimmerlocal.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(shimmerlocal.lm)$coefficients[2, 4], 2)))
shimmerapq5.scatter
summary(shimmerlocal.lm)

# Shimmer local absolute 
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(shimmer_local_db, type="common")
model1<-lm(shimmer_local_db ~ age_screening + sex + testing_location+ age_learned_english+factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = shimmer_loca, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Absolute Local Shimmer", x ="", y = "dB",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp

shimmerlocaldb.lm <- lm(shimmer_local_db ~ Baseline_HAMD + sex + age_screening + testing_location+ age_learned_english, data = MDD_feeling_BL)
# Create plot
shimmerdda.scatter <- ggplot(MDD_feeling_BL, aes(y = shimmer_local, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "%", title = "Relative Shimmer") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
shimmerapq5.scatter <- shimmerdda.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(shimmerlocaldb.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(shimmerlocaldb.lm)$coefficients[2, 4], 2)))
shimmerapq5.scatter
summary(shimmerlocaldb.lm)

# HNR max (ac)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(hnr_ac_max, type="common")
model1<-lm(hnr_ac_max ~ age_screening + sex + testing_location+ factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[5]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = shimmer_loca, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Max Harmonics to Noise Ratio", x ="", y = "dB",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp

hnracmax.lm <- lm(hnr_ac_max ~ Baseline_HAMD + sex + age_screening + testing_location, data = MDD_feeling_BL)
# Create plot
shimmerdda.scatter <- ggplot(MDD_feeling_BL, aes(y = shimmer_loca, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "%", title = "Relative Shimmer") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
shimmerapq5.scatter <- shimmerdda.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(hnracmax.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(hnracmax.lm)$coefficients[2, 4], 2)))
shimmerapq5.scatter
summary(hnracmax.lm)

# HNR mean (ac)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(hnr_ac_mean, type="common")
model1<-lm(hnr_ac_mean ~ age_screening + sex + testing_location+ age_learned_english+factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = hnr_ac_mean, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Max Harmonics to Noise Ratio", x ="", y = "dB",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp
eta_squared <- ancova[["Sum Sq"]][6] / sum(ancova[["Sum Sq"]])


hnracmean.lm <- lm(hnr_ac_mean ~ Baseline_HAMD + sex + age_screening + testing_location+ age_learned_english, data = MDD_feeling_BL)
# Create plot
hnracmean.scatter <- ggplot(MDD_feeling_BL, aes(y = hnr_ac_mean, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "dB", title = "Mean HNR") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
hnracmean.scatter <- hnracmean.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(hnracmean.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(hnracmean.lm)$coefficients[2, 4], 2)))
hnracmean.scatter




# Create plot
shimmerdda.scatter <- ggplot(MDD_feeling_BL, aes(y = hnr_ac_mean, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "%", title = "Relative shimmer") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
shimmerapq5.scatter <- shimmerdda.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(hnracmean.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(hnracmean.lm)$coefficients[2, 4], 2)))
shimmerapq5.scatter
summary(hnracmean.lm)

# HNR variance (ac)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(hnr_ac_variance, type="common")
model1<-lm(hnr_ac_variance ~ age_screening + sex + testing_location+ age_learned_english+factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = hnr_ac_variance, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Max Harmonics to Noise Ratio", x ="", y = "dB",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp
eta_squared <- ancova[["Sum Sq"]][6] / sum(ancova[["Sum Sq"]])


hnracvar.lm <- lm(hnr_ac_variance ~ Baseline_HAMD + sex + age_screening + testing_location+ age_learned_english, data = MDD_feeling_BL)
# Create plot
shimmerdda.scatter <- ggplot(MDD_feeling_BL, aes(y = shimmer_loca, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "%", title = "Relative Shimmer") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
shimmerapq5.scatter <- shimmerdda.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(hnracvar.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(hnracvar.lm)$coefficients[2, 4], 2)))
shimmerapq5.scatter
summary(hnracvar.lm)

# HNR mean (cc)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(hnr_cc_mean, type="common")
model1<-lm(hnr_cc_mean ~ age_screening + sex + testing_location+ age_learned_english+factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[5]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = hnr_cc_mean, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Mean Harmonics to Noise Ratio", x ="", y = "dB",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp

hnrccmean.lm <- lm(hnr_cc_mean ~ Baseline_HAMD + sex + age_screening + testing_location+ age_learned_english, data = MDD_feeling_BL)
# Create plot
shimmerdda.scatter <- ggplot(MDD_feeling_BL, aes(y = shimmer_loca, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "%", title = "Relative Shimmer") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
shimmerapq5.scatter <- shimmerdda.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(hnrccmean.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(hnrccmean.lm)$coefficients[2, 4], 2)))
shimmerapq5.scatter
summary(hnrccmean.lm)

# HNR variance (cc)
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(hnr_cc_variance, type="common")
model1<-lm(hnr_cc_variance ~ age_screening + sex + testing_location+ age_learned_english+factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[5]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = hnr_cc_variance, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Mean Harmonics to Noise Ratio", x ="", y = "dB",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp

hnrccvar <- lm(hnr_cc_variance ~ Baseline_HAMD + sex + age_screening + testing_location+ age_learned_english, data = MDD_feeling_BL)
# Create plot
shimmerdda.scatter <- ggplot(MDD_feeling_BL, aes(y = shimmer_loca, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "%", title = "Relative Shimmer") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
shimmerapq5.scatter <- shimmerdda.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(hnrccvar)$coefficients[2,1], 2),
                         ", p = ", signif(summary(hnrccvar)$coefficients[2, 4], 2)))
shimmerapq5.scatter
summary(hnrccvar)

# Personal pronouns
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(tag_PRP, type="common")
model1<-lm(tag_PRP ~ age_screening + sex + testing_location+ age_learned_english+factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = tag_PRP, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Personal pronoun count", x ="", y = "dB",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp
eta_squared <- ancova[["Sum Sq"]][6] / sum(ancova[["Sum Sq"]])


tagpronouns.lm <- lm(tag_PRP ~ Baseline_HAMD + sex + age_screening + testing_location+ age_learned_english, data = MDD_feeling_BL)
# Create plot
tagpronouns.scatter <- ggplot(MDD_feeling_BL, aes(y = tag_PRP, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Raw count", title = "Personal pronoun count") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
tagpronouns.scatter <- tagpronouns.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(tagpronouns.lm)$coefficients[2,1], 3),
                         ", p = ", signif(summary(tagpronouns.lm)$coefficients[2, 4], 2)))
tagpronouns.scatter


# Create plot
shimmerdda.scatter <- ggplot(MDD_feeling_BL, aes(y = shimmer_loca, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "%", title = "Relative Shimmer") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
shimmerapq5.scatter <- shimmerdda.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(tagpronouns.lm)$coefficients[2,1], 2),
                         ", p = ", signif(summary(tagpronouns.lm)$coefficients[2, 4], 2)))
shimmerapq5.scatter
summary(tagpronouns.lm)

# Gerund verbs
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(tag_VBG, type="common")
model1<-lm(tag_VBG ~ age_screening + sex + testing_location+ age_learned_english+factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = tag_VBG, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Gerund Verb by Group", x ="", y = "Raw count gerund",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp

gerundverbs.lm <- lm(tag_VBG ~ Baseline_HAMD + sex + age_screening + testing_location+ age_learned_english, data = MDD_feeling_BL)
# Create plot
gerundverbs.scatter <- ggplot(MDD_feeling_BL, aes(y = tag_VBG, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Raw count", title = "Gerund verb count") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
gerundverbs.scatter <- gerundverbs.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(gerundverbs.lm)$coefficients[2,1], 3),
                         ", p = ", signif(summary(gerundverbs.lm)$coefficients[2, 4], 2)))
gerundverbs.scatter
summary(gerundverbs.lm)

# Adverb
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(tag_RB, type="common")
model1<-lm(tag_RB ~ age_screening + sex + testing_location+ age_learned_english+factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = tag_RB, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Adverb use", x ="", y = "Raw count adverbs",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp

gerundverbs.lm <- lm(tag_RB ~ Baseline_HAMD + sex + age_screening + testing_location+ age_learned_english, data = MDD_feeling_BL)
# Create plot
gerundverbs.scatter <- ggplot(MDD_feeling_BL, aes(y = tag_RB, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Raw count", title = "Gerund verb count") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
gerundverbs.scatter <- gerundverbs.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(gerundverbs.lm)$coefficients[2,1], 3),
                         ", p = ", signif(summary(gerundverbs.lm)$coefficients[2, 4], 2)))
gerundverbs.scatter
summary(gerundverbs.lm)

# Determiner 
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(pos_DET, type="common")
model1<-lm(pos_DET ~ age_screening + sex + testing_location+ age_learned_english+factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = pos_DET, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Determiner count", x ="", y = "Raw count determiners",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp

determiners.lm <- lm(pos_DET ~ Baseline_HAMD + sex + age_screening + testing_location+ age_learned_english, data = MDD_feeling_BL)
# Create plot
determiners.scatter <- ggplot(MDD_feeling_BL, aes(y = pos_DET, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Raw count", title = "Determiner count") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
determiners.scatter <- determiners.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(determiners.lm)$coefficients[2,1], 3),
                         ", p = ", signif(summary(determiners.lm)$coefficients[2, 4], 2)))
determiners.scatter
summary(determiners.lm)

#  Universal dependency morphology feature - Number, plur 
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(morph_number_plur, type="common")
model1<-lm(morph_number_plur ~ age_screening + sex + testing_location+ age_learned_english+factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = morph_number_plur, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "UD - Number, plur", x ="", y = "Count",subtitle = paste0("p = ", round(p_value, 3))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp

morhnumplur.lm <- lm(morph_number_plur ~ Baseline_HAMD + sex + age_screening + testing_location+ age_learned_english, data = MDD_feeling_BL)
# Create plot
morphnumplur.scatter <- ggplot(MDD_feeling_BL, aes(y = morph_number_plur, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Raw count", title = "UD - Number, plur") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
morphnumplur.scatter <- morphnumplur.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(morhnumplur.lm)$coefficients[2,1], 4),
                         ", p = ", signif(summary(morhnumplur.lm)$coefficients[2, 4], 2)))
morphnumplur.scatter
summary(morhnumplur.lm)

#  Universal dependency morphology feature - Aspect, prog 
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(morph_aspect_prog, type="common")
model1<-lm(morph_aspect_prog ~ age_screening + sex + testing_location+ age_learned_english+factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = morph_aspect_prog, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "UD - Aspect, prog", x ="", y = "Count",subtitle = paste0("p = ", round(p_value, 5))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp

morphaspprg.lm <- lm(morph_aspect_prog ~ Baseline_HAMD + sex + age_screening + testing_location+ age_learned_english, data = MDD_feeling_BL)
# Create plot
morphaspprg.scatter <- ggplot(MDD_feeling_BL, aes(y = morph_aspect_prog, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Raw count", title = "UD - Aspect, prog") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
morphaspprg.scatter <- morphaspprg.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(morphaspprg.lm)$coefficients[2,1], 4),
                         ", p = ", signif(summary(morphaspprg.lm)$coefficients[2, 4], 2)))
morphaspprg.scatter
summary(morphaspprg.lm)

#  Minimum utterance length
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(min_utt_len, type="common")
model1<-lm(min_utt_len ~ age_screening + sex + testing_location+ age_learned_english+factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = min_utt_len, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "UD - Aspect, prog", x ="", y = "Count",subtitle = paste0("p = ", round(p_value, 5))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp

minuttlenght.lm <- lm(min_utt_len ~ Baseline_HAMD + sex + age_screening + testing_location+ age_learned_english, data = MDD_feeling_BL)
# Create plot
minuttlength.scatter <- ggplot(MDD_feeling_BL, aes(y = min_utt_len, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Raw count", title = "UD - Aspect, prog") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
minuttlength.scatter <- minuttlength.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(minuttlenght.lm)$coefficients[2,1], 4),
                         ", p = ", signif(summary(minuttlenght.lm)$coefficients[2, 4], 2)))
minuttlength.scatter
summary(minuttlenght.lm)

#  MFCC 31 variance 
WL_feeling_BL %>% group_by(participant_group) %>%  get_summary_stats(mfcc_var_31, type="common")
model1<-lm(mfcc_var_31 ~ age_screening + sex + testing_location+ age_learned_english+factor(participant_group), data = WL_feeling_BL)
ancova <- Anova(model1, type ="III")
plot(model1)
p_value <- ancova$`Pr(>F)`[6]

bxp <- ggplot(WL_feeling_BL, aes(x = participant_group, y = mfcc_var_31, fill = participant_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "MFCC 31 variance", x ="", y = "Variance",subtitle = paste0("p = ", round(p_value, 5))) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size=20),
        axis.title.y = element_text(size = 20), axis.text.y = element_text(size=20), title = element_text(size=15)) +
  scale_fill_manual(values = c("#CCFFFF", "#D783C6")) +
  theme(legend.position = "none") +
  geom_signif(comparisons = list(c("Control", "MDD")), 
              map_signif_level=TRUE)
bxp

mfccvar31.lm <- lm(mfcc_var_31 ~ Baseline_HAMD + sex + age_screening + testing_location+ age_learned_english, data = MDD_feeling_BL)
# Create plot
mfccvar31.scatter <- ggplot(MDD_feeling_BL, aes(y = mfcc_var_31, x = Baseline_HAMD)) +
  geom_point(color = "#9F2B68") +
  geom_smooth(method = lm, color = "#702963", fill = "#C3B1E1") +
  labs(x = "HAM-D", y = "Variance", title = "MFCC 31") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    plot.subtitle = element_text(size = 16))
# Add R^2 to and p to plot
mfccvar31.scatter <- mfccvar31.scatter +
  labs(subtitle = paste0("\U03B2= ", round(summary(mfccvar31.lm)$coefficients[2,1], 4),
                         ", p = ", signif(summary(mfccvar31.lm)$coefficients[2, 4], 2)))
mfccvar31.scatter
summary(mfccvar31.lm)



