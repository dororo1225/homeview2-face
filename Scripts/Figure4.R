library(here)
library(tidyverse)
library(modelbased)
library(ggsignif)
library(ggpubr)
library(knitr)
library(lme4)
library(lmerTest)
library(car)
library(effectsize)
library(emmeans)
library(circular)


# 5 min bin
bin_length <- 5 * 60 # seconds

here("Data", "ImageList_Homeview2_master.csv") %>%  
  read_csv(col_types = "cccdcdd") %>%
  arrange(AgeCategory, Participant, image_name) %>% 
  group_by(AgeCategory) %>% 
  mutate(time = row_number(),
         BinID = time %/% bin_length + 1) %>% 
  ungroup() %>% 
  group_by(AgeCategory, BinID) %>% 
  mutate(Bin_name = str_c(AgeCategory, str_pad(BinID, pad = "0", side = "left", width = 3), sep = "_"),
         time_bin = row_number(),
         time_bin_min = time_bin/60) %>% 
  ungroup() %>%
  group_by(AgeCategory) %>% 
  mutate(Bin_num = as.numeric(as.factor(Bin_name))) %>% 
  ungroup() %>% 
  mutate(AgeCategory = str_replace_all(AgeCategory, pattern = "0", replacement = "")) -> df_bin

# Figure 4
## Proportion of images with faces
df_bin %>% 
  group_by(AgeCategory, Bin_name) %>% 
  summarise(N_img = n_distinct(image_name),
            N_img_face = sum(FaceInView),
            Prop = N_img_face/N_img,
            .groups = "drop") -> df_prop

fit <- lm(Prop ~AgeCategory, data = df_prop)
Anova(fit)
eta_squared(fit)
estimate_contrasts(fit, contrast = "AgeCategory", p_adjust = "holm", effectsize = "emmeans", backend = "emmeans")

df_prop %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean = mean(Prop),
            SD = sd(Prop),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Prop)$conf.int[1],
            .upper = t.test(Prop)$conf.int[2]) %>% 
  kable()

df_prop %>% 
  mutate(AgeCategory = case_when(AgeCategory == "2-3" ~ "2-3\n(n = 526)",
                                 AgeCategory == "5-6" ~ "5-6\n(n = 520)",
                                 AgeCategory == "8-9" ~ "8-9\n(n = 514)")) -> df_prop

df_prop %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean = mean(Prop),
            SD = sd(Prop),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Prop)$conf.int[1],
            .upper = t.test(Prop)$conf.int[2]) %>% 
  ggplot(aes(x = AgeCategory, y = Mean, fill = AgeCategory)) +
  geom_bar(stat = "identity", alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymax = .lower, ymin = .upper), width = 0.5) +
  geom_signif(y_position = c(1.05, 1.05, 1.15), xmin = c(1, 2.05, 1), xmax = c(1.95, 3, 3),
              annotations = c("***", "**", "***"), tip_length = 0.01, textsize = 5, vjust = 0.5) +
  labs(x = "Age in Months", y = "Proportion of Images with Faces", tag = "A") +
  geom_jitter(data = df_prop, aes(color = AgeCategory, y = Prop), height = 0, width = 0.125, alpha = 0.075, size = 1) +
  guides(color = "none", fill = "none") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(breaks = seq(0, 1.25, by = 0.25)) +
  theme_bw() +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 7, color = "black"),
        strip.text = element_text(size = 10, face = "bold")) -> gp1
print(gp1)

## Define short/long bouts
thr_bout <- 5

df_bin %>% 
  group_by(AgeCategory, Bin_name) %>% 
  mutate(FaceInView_lag = lag(FaceInView, default = 0),
         .after = "FaceInView") %>% 
  mutate(Onset = if_else(FaceInView == 1 & FaceInView_lag == 0, 1, 0),
         Bout_tmp = cumsum(Onset),
         Bout = if_else(FaceInView == 0, 0, Bout_tmp),
         .after = "FaceInView_lag") %>% 
  select(!Bout_tmp) %>% 
  filter(Bout != 0) %>% 
  group_by(AgeCategory, Bin_name, Bout) %>% 
  mutate(BoutLength = n_distinct(image_name)) %>% 
  ungroup() %>% 
  select(Bin_name, Bout, BoutLength, image_name) -> df_bout_img

df_bin %>% 
  group_by(AgeCategory, Bin_name) %>% 
  mutate(FaceInView_lag = lag(FaceInView, default = 0),
         .after = "FaceInView") %>% 
  mutate(Onset = if_else(FaceInView == 1 & FaceInView_lag == 0, 1, 0),
         Bout_tmp = cumsum(Onset),
         Bout = if_else(FaceInView == 0, 0, Bout_tmp),
         .after = "FaceInView_lag") %>% 
  select(!Bout_tmp) %>% 
  filter(Bout != 0) %>% 
  group_by(AgeCategory, Bin_name, Bout) %>% 
  summarise(BoutLength = n_distinct(image_name),
            .groups = "drop") -> df_bout

df_bout %>%
  mutate(BoutCategory = if_else(BoutLength <= thr_bout, "short", "long"),
         AgeCategory = str_c(AgeCategory, " mo")) %>%
  ggplot(aes(x = BoutLength, fill = BoutCategory)) +   
  geom_histogram(binwidth = 2, color = "black") +
  facet_wrap(~AgeCategory, ncol = 1) +
  geom_vline(aes(xintercept = thr_bout), linetype = "dashed") +
  guides(fill = "none") +
  labs(x = "Bout Length (sec)", y = "Count", fill = "Bout Category", tag = "B") +
  theme_bw() +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 7, color = "black"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 7)) -> gp2
print(gp2)

## Proportion of long bouts
here("Data", "ImageList_face_master.csv") %>% 
  read_csv(col_types = "ccccdddddddddddddddddddd") %>% 
  left_join(select(df_bin, image_name, Bin_name), by = "image_name") %>% 
  left_join(df_bout_img, by = c("Bin_name", "image_name")) %>% 
  relocate(Bin_name, Bout, BoutLength, .after = "AgeCategory") %>% 
  mutate(roll_abs = abs(roll)) -> df

df %>% 
  group_by(AgeCategory, Bin_name, Bout, BoutLength) %>% 
  summarise(N_face = n(),
            .groups = "drop") %>%
  mutate(BoutCategory = if_else(BoutLength <= thr_bout, "short", "long")) %>%
  group_by(AgeCategory, Bin_name, BoutCategory) %>% 
  summarise(N_bout = n(),
            .groups = "drop") %>%
  group_by(AgeCategory) %>% 
  complete(Bin_name, BoutCategory) %>% 
  replace_na(list(N_bout = 0)) %>% 
  group_by(AgeCategory, Bin_name) %>% 
  mutate(Total_bout = sum(N_bout),
         Prop = N_bout/Total_bout) %>%
  ungroup() %>% 
  filter(BoutCategory == "long") -> df_bout_long

df_bout_long %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean = mean(Prop),
            SD = sd(Prop),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Prop)$conf.int[1],
            .upper = t.test(Prop)$conf.int[2],
            .groups = "drop") %>% 
  kable()

df_bout_long %>% 
  mutate(AgeCategory = case_when(AgeCategory == "2-3" ~ "2-3\n(n = 486)",
                                 AgeCategory == "5-6" ~ "5-6\n(n = 457)",
                                 AgeCategory == "8-9" ~ "8-9\n(n = 441)")) -> df_bout_long2

df_bout_long %>% 
  lm(Prop ~ AgeCategory, data = .) -> fit
Anova(fit)
eta_squared(fit)
estimate_contrasts(fit, contrast = "AgeCategory", p_adjust = "holm", effectsize = "emmeans", backend = "emmeans")

df_bout_long2 %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean = mean(Prop),
            SD = sd(Prop),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Prop)$conf.int[1],
            .upper = t.test(Prop)$conf.int[2],
            .groups = "drop") %>%
  ggplot(aes(x = AgeCategory, y = Mean)) +
  geom_bar(aes(fill = AgeCategory), stat = "identity", position = position_dodge(0.9), color = "black", alpha = 0.5) +
  geom_errorbar(aes(ymax = .upper, ymin = .lower, group = AgeCategory), width = 0.5) +
  geom_jitter(data = df_bout_long2, aes(color = AgeCategory, y = Prop), alpha = 0.075, size = 1, width = 0.25, height = 0) +
  geom_signif(y_position = c(1.05, 1.05, 1.15), xmin = c(1, 2.05, 1), xmax = c(1.95, 3, 3),
              annotations = c("***", "***", "***"), tip_length = 0.01, textsize = 5, vjust = 0.5) +
  scale_y_continuous(breaks = seq(0, 1.25, by = 0.25)) +
  scale_fill_viridis_d(option = "D") +
  scale_color_viridis_d(option = "D") +
  labs(x = "Age in Months", y = "Proportion of Long Bouts", fill = "Age in Months", color = "Age in Months", tag = "C") +
  guides(fill = "none", color = "none") +
  theme_bw() +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 7, color = "black"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 7)) -> gp3
print(gp3)

## define overall domain-general saliency scores and overallface-template scores
thr_quantile <- 4
df %>% 
  # filter(N_face == 1) %>% 
  summarise(thr_saliency = quantile(Saliency_max_face, na.rm = TRUE)[thr_quantile],
            thr_size = quantile(size)[thr_quantile],
            thr_centering_x = quantile(centering_x)[thr_quantile],
            thr_centering_y = quantile(centering_y)[thr_quantile],
            thr_roll = quantile(roll_abs, na.rm = TRUE)[5 - thr_quantile + 1]) -> df_thr

df %>% 
  mutate(saliency_flag = if_else(Saliency_max_face > df_thr$thr_saliency, 1, 0, 0),
         size_flag = if_else(size >= df_thr$thr_size, 1, 0),
         centering_x_flag = if_else(centering_x >= df_thr$thr_centering_x, 1, 0),
         centering_y_flag = if_else(centering_y >= df_thr$thr_centering_y, 1, 0),
         upright_flag = if_else(roll_abs <= df_thr$thr_roll, 1, 0, missing = 0),
         frontal_flag = if_else(eyes == 2 & nose == 1 & mouth == 1, 1, 0),
         GS = saliency_flag + size_flag + centering_x_flag + centering_y_flag,
         TemplateS = upright_flag + frontal_flag) %>% 
  select(AgeCategory, Bin_name, Bout, image_name, face_id, N_face, starts_with("BoutLength"), GS, TemplateS) %>%
  group_by(AgeCategory, Bin_name, Bout, BoutLength) %>% 
  summarise(N_face = n(),
            MeanGS = mean(GS),
            MeanTemplate = mean(TemplateS),
            .groups = "drop") %>%
  pivot_longer(starts_with("Mean"), names_to = "Score", values_to = "Mean") %>% 
  mutate(Score = case_when(Score == "MeanGS" ~ "Overall Domain-General Saliency",
                           Score == "MeanTemplate" ~ "Overall Face Template"),
         Score = fct_relevel(Score, "Overall Domain-General Saliency", "Overall Face Template"),
         AgeCategory = factor(AgeCategory, levels = c("2-3", "5-6", "8-9"))) -> df_score

df_score %>% 
  mutate(BoutCategory = if_else(BoutLength <= thr_bout, "short", "long")) %>%
  filter(Score == "Overall Domain-General Saliency") %>% 
  group_by(AgeCategory, BoutCategory) %>% 
  summarise(N_Bout = n(),
            .groups = "drop_last") %>% 
  mutate(Total_Bout = sum(N_Bout),
         Prop_Bout = N_Bout/Total_Bout) %>% 
  ungroup() %>% 
  kable()

df_score %>% 
  mutate(BoutCategory = if_else(BoutLength <= thr_bout, "short", "long"),
         BoutCategory = fct_rev(BoutCategory)) %>% 
  group_by(AgeCategory, Bin_name, BoutCategory, Score) %>% 
  summarise(Mean_Bin = mean(Mean),
            .groups = "drop") -> df_score_bin

# for visualization
ages <- levels(df_score_bin$AgeCategory)
dodge_pos <- function(x, age, ages, width = 0.9) {
  i <- match(age, ages)
  n <- length(ages)
  x + (-width/2) + (i - 0.5) * (width / n)
}

# Overall Domein-general Saliency Scores
df_score_bin %>% 
  filter(Score == "Overall Domain-General Saliency") %>% 
  group_by(AgeCategory, BoutCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean = mean(Mean_Bin),
            SD = sd(Mean_Bin),
            .groups = "drop") %>% 
  kable()

df_score_bin %>% 
  filter(Score == "Overall Domain-General Saliency") %>% 
  lmer(Mean_Bin ~ AgeCategory * BoutCategory + (1 | Bin_name), data = .) -> fit_gs
Anova(fit_gs, test.statistic = "F")
eta_squared(fit_gs)
estimate_contrasts(fit_gs, contrast = "AgeCategory", by = "BoutCategory", p_adjust = "holm", backend = "emmeans")

## effect size
emm <- emmeans(fit_gs, ~ AgeCategory | BoutCategory)
contr <- contrast(emm, method = "pairwise", adjust = "holm")
contr_df <- as.data.frame(summary(contr, infer = TRUE))
s <- sigma(fit_gs)
edf <- df.residual(fit_gs)
contr_df %>%
  mutate(d = estimate / s,
         d_lower = d - qt(0.975, edf) * (SE / s),
         d_upper = d + qt(0.975, edf) * (SE / s),
         sig = if_else(p.value < 0.05, 1, 0)) %>% 
  kable()

tibble(
  Score = "Overall Domain-General Saliency",
  xmin = c(dodge_pos(1, "2-3", ages), dodge_pos(2, "2-3", ages), dodge_pos(2, "2-3", ages), 1),
  xmax = c(dodge_pos(1, "5-6", ages), dodge_pos(2, "5-6", ages), dodge_pos(2, "8-9", ages), 2),
  y_position = c(4.05, 4.05, 4.25, 4.45),
  annotations = c("***", "***", "***", "***")) -> df_sig_gs

# Overall Face-Template Scores
df_score_bin %>% 
  filter(Score == "Overall Face Template") %>% 
  group_by(AgeCategory, BoutCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean = mean(Mean_Bin),
            SD = sd(Mean_Bin),
            .groups = "drop") %>% 
  kable()

df_score_bin %>% 
  filter(Score == "Overall Face Template") %>% 
  lmer(Mean_Bin ~ AgeCategory * BoutCategory + (1 | Bin_name), data = .) -> fit_tmp
Anova(fit_tmp, test.statistic = "F")
eta_squared(fit_tmp)
estimate_contrasts(fit_tmp, contrast = "AgeCategory", by = "BoutCategory", p_adjust = "holm",  backend = "emmeans")

df_score_bin %>% 
  filter(Score == "Overtall Face Template") %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean = mean(Mean_Bin),
            SD = sd(Mean_Bin),
            .groups = "drop") %>% 
  kable()
estimate_contrasts(fit_tmp, contrast = "AgeCategory", p_adjust = "holm",  backend = "emmeans")
## effect size
emm <- emmeans(fit_tmp, ~ AgeCategory | BoutCategory)
contr <- contrast(emm, method = "pairwise", adjust = "holm")
contr_df <- as.data.frame(summary(contr, infer = TRUE))
s <- sigma(fit_tmp)
edf <- df.residual(fit_tmp)
contr_df %>%
  mutate(d = estimate / s,
         d_lower = d - qt(0.975, edf) * (SE / s),
         d_upper = d + qt(0.975, edf) * (SE / s),
         sig = if_else(p.value < 0.05, 1, 0)) %>% 
  kable()

## effect size
emm <- emmeans(fit_tmp, ~ AgeCategory | BoutCategory)
contr <- contrast(emm, method = "pairwise", adjust = "holm")
contr_df <- as.data.frame(summary(contr, infer = TRUE))
s <- sigma(fit_tmp)
edf <- df.residual(fit_tmp)
contr_df %>%
  mutate(d = estimate / s,
         d_lower = d - qt(0.975, edf) * (SE / s),
         d_upper = d + qt(0.975, edf) * (SE / s),
         sig = if_else(p.value < 0.05, 1, 0)) %>% 
  kable()

tibble(
  Score = "Overall Face Template",
  xmin = c(dodge_pos(1, "2-3", ages), dodge_pos(1, "5-6", ages), 1, dodge_pos(2, "2-3", ages), dodge_pos(2, "5-6", ages)),
  xmax = c(dodge_pos(1, "8-9", ages), dodge_pos(1, "8-9", ages), 2, dodge_pos(2, "8-9", ages), dodge_pos(2, "8-9", ages)),
  y_position = c(4.25, 4.05, 4.45, 4.25, 4.05),
  annotations = c("***", "***", "***", "*", "*")) -> df_sig_tmp

bind_rows(df_sig_gs, df_sig_tmp) %>% 
  mutate(Score = fct_relevel(Score, "Overall Domain-General Saliency", "Overall Face Template")) -> df_sig

df_score_bin %>% 
  group_by(AgeCategory, BoutCategory, Score) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean = mean(Mean_Bin),
            SD = sd(Mean_Bin),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Mean_Bin)$conf.int[1],
            .upper = t.test(Mean_Bin)$conf.int[2],
            .groups = "drop") %>%
  ggplot(aes(x = BoutCategory, y = Mean)) +
  geom_bar(aes(fill = AgeCategory), stat = "identity", position = position_dodge(0.9), color = "black", alpha = 0.5) +
  geom_errorbar(aes(ymax = .upper, ymin = .lower, group = AgeCategory), position = position_dodge(0.9), width = 0.5) +
  geom_point(data = df_score_bin, aes(color = AgeCategory, y = Mean_Bin), alpha = 0.075, size = 1,
             position = position_jitterdodge(jitter.height = 0, jitter.width = 0.125, dodge.width  = 0.9)) +
  geom_segment(data = df_sig,
               aes(x = xmin, xend = xmax, y = y_position, yend = y_position),
               inherit.aes = FALSE, linewidth = 0.6) +
  geom_segment(data = df_sig,
               aes(x = xmin, xend = xmin, y = y_position, yend = y_position - 0.04),
               inherit.aes = FALSE, linewidth = 0.6) +
  geom_segment(data = df_sig,
               aes(x = xmax, xend = xmax, y = y_position, yend = y_position - 0.04),
               inherit.aes = FALSE, linewidth = 0.6) +
  geom_text(data = df_sig,
            aes(x = (xmin + xmax)/2, y = y_position + 0.03, label = annotations),
            inherit.aes = FALSE, size = 5) +
  facet_grid(~Score) +
  scale_fill_viridis_d(option = "D") +
  scale_color_viridis_d(option = "D") +
  guides(fill = "none", color = "none") +
  scale_y_continuous(breaks = seq(0, 4.5, by = 0.5), limits = c(0, 4.5)) +
  labs(x = "Bout Category", y = "Bin-level Mean Score", fill = "Age in Months", color = "Age in Months", tag = "D") +
  theme_bw() +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 7, color = "black"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 7))  -> gp4
print(gp4)

ggarrange(gp1, gp2, gp3, ncol = 3, widths = c(1, 2, 1)) -> gp_upper 
ggarrange(gp4, ncol = 1) -> gp_lower

ggarrange(gp_upper, gp_lower, ncol = 1, heights = c(1, 1))
ggarrange(gp_upper, gp_lower, ncol = 1, heights = c(1, 1)) %>% 
  ggexport(filename = here("Figures", "Figure4.jpg"), res = 300, width = 2400, height = 2400)