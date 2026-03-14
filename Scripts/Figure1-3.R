library(here)
library(tidyverse)
library(modelbased)
library(ggsignif)
library(ggpubr)
library(knitr)
library(car)
library(effectsize)
library(circular)

# Figure 1A (5-min bins)
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

df_bin %>% 
  group_by(AgeCategory) %>% 
  summarise(N_img = n_distinct(image_name),
            N_DNC = sum(DNC),
            N_hr = N_img/60/60) %>% 
  mutate(Total_hr = sum(N_hr),
         Total_img = sum(N_img)) %>% 
  kable()

df_bin %>% 
  mutate(AgeCategory = str_c(AgeCategory, " mo")) %>%
  ggplot(aes(x = time_bin_min, y = Bin_num)) +
  geom_tile(aes(fill = FaceInView)) +
  facet_wrap(~AgeCategory) +
  labs(x = "Time (min)", y = "5-min Bins", tag = "A") +
  guides(fill = "none") +
  theme_bw() +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 7, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 10, face = "bold")) -> gp1
print(gp1)
ggsave(here("Figures", "Figure1A.jpg"), dpi = 300, units = "px", width = 1800, height = 900)


# Figure S3 (Number of Faces per Image)

here("Data", "ImageList_face_master.csv") %>% 
  read_csv(col_types = "ccccdddddddddddddddddddd") %>% 
  left_join(select(df_bin, image_name, Bin_name), by = "image_name") %>% 
  relocate(Bin_name, .after = "AgeCategory") -> df_face

df_face %>% 
  filter(N_face == 1) -> df_one

# Supplementary figure S3
df_face %>% 
  mutate(AgeCategory = str_c(AgeCategory, " mo")) %>%
  group_by(AgeCategory, N_face) %>% 
  summarise(N_img = n_distinct(image_name),
            .groups = "drop_last") %>% 
  mutate(Total_img = sum(N_img),
         Prop = N_img/Total_img) %>% 
  ungroup() %>% 
  ggplot(aes(x = N_face, y = Prop, fill = AgeCategory)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.5) +
  facet_wrap(~AgeCategory, ncol = 1) +
  labs(x = "Number of Faces per Image", y = "Proportion of Images") +
  guides(fill = "none") +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 7, color = "black"),
        strip.text = element_text(size = 10, face = "bold")) -> gps3
print(gps3)
ggsave(here("Figures", "FigureS3.jpg"), dpi = 300, units = "px", width = 2400, height = 2400)

# Figure 2(Domain-general Saliency)
## Size
df_one %>% 
  select(AgeCategory, Bin_name, image_name, face_id, size, contains("saliency"), contains("centering")) %>% 
  group_by(AgeCategory, Bin_name) %>% 
  summarise(Mean_bin = mean(size, na.rm = TRUE),
            .groups = "drop") -> df_size

fit <- lm(Mean_bin ~AgeCategory, data = df_size)
Anova(fit)
eta_squared(fit)
estimate_contrasts(fit, contrast = "AgeCategory", p_adjust = "holm", effectsize = "emmeans", backend = "emmeans")

df_size %>%
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Mean_bin),
            SD = sd(Mean_bin),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Mean_bin)$conf.int[1],
            .upper = t.test(Mean_bin)$conf.int[2]) %>% 
  kable()

df_size %>% 
  mutate(AgeCategory = case_when(AgeCategory == "2-3" ~ "2-3\n(n = 484)",
                                 AgeCategory == "5-6" ~ "5-6\n(n = 455)",
                                 AgeCategory == "8-9" ~ "8-9\n(n = 441)")) -> df_size

df_size %>%
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Mean_bin),
            SD = sd(Mean_bin),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Mean_bin)$conf.int[1],
            .upper = t.test(Mean_bin)$conf.int[2]) %>% 
  ggplot(aes(x = AgeCategory, y = Mean_group, fill = AgeCategory)) +
  geom_bar(stat = "identity", alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymax = .upper, ymin = .lower), width = 0.5) +
  geom_jitter(data = df_size, aes(color = AgeCategory, y = Mean_bin), height = 0, width = 0.125, alpha = 0.075, size = 1) +
  geom_signif(y_position = c(1.05, 1.15), xmin = c(1, 1), xmax = c(2, 3),
              annotations = c("***", "***"), tip_length = 0.01, textsize = 5, vjust = 0.5) +
  scale_y_continuous(breaks = seq(0, 1.25, by = 0.25)) +
  labs(x = "Age in Months", y = "Mean Face Size", tag = "A") + 
  guides(color = "none", fill = "none") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw() +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 7, color = "black"),
        strip.text = element_text(size = 10, face = "bold")) -> gp1
print(gp1)


## Saliency (all features)
df_one %>% 
  group_by(AgeCategory, Bin_name) %>% 
  summarise(Mean_bin = mean(Saliency_max_face, na.rm = TRUE),
            .groups = "drop") -> df_saliency

fit <- lm(Mean_bin ~ AgeCategory, data = df_saliency)
Anova(fit)
eta_squared(fit)
estimate_contrasts(fit, contrast = "AgeCategory", p_adjust = "holm", effectsize = "emmeans", backend = "emmeans")

df_saliency %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Mean_bin),
            SD = sd(Mean_bin),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Mean_bin)$conf.int[1],
            .upper = t.test(Mean_bin)$conf.int[2]) %>% 
  kable()

df_saliency %>% 
  mutate(AgeCategory = case_when(AgeCategory == "2-3" ~ "2-3\n(n = 484)",
                                 AgeCategory == "5-6" ~ "5-6\n(n = 455)",
                                 AgeCategory == "8-9" ~ "8-9\n(n = 441)")) -> df_saliency

df_saliency %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Mean_bin),
            SD = sd(Mean_bin),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Mean_bin)$conf.int[1],
            .upper = t.test(Mean_bin)$conf.int[2]) %>% 
  ggplot(aes(x = AgeCategory, y = Mean_group, fill = AgeCategory)) +
  geom_bar(stat = "identity", alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymax = .upper, ymin = .lower), width = 0.5) +
  geom_jitter(data = df_saliency, aes(color = AgeCategory, y = Mean_bin), height = 0, width = 0.125, alpha = 0.075, size = 1) +
  geom_signif(y_position = c(1.05, 1.15), xmin = c(1, 1), xmax = c(2, 3),
              annotations = c("***", "***"), tip_length = 0.01, textsize = 5, vjust = 0.5) +
  scale_y_continuous(breaks = seq(0, 1.25, by = 0.25)) +
  labs(x = "Age in Months", y = "Mean Saliency Map Value (All Features)",  tag = "B") + 
  guides(color = "none", fill = "none") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw() +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 7, color = "black"),
        strip.text = element_text(size = 10, face = "bold")) -> gp2
print(gp2)

## Centering_y
df_one %>% 
  group_by(AgeCategory, Bin_name) %>% 
  summarise(Mean_bin = mean(centering_y, na.rm = TRUE),
            .groups = "drop") -> df_centering_y

fit <- lm(Mean_bin ~ AgeCategory, data = df_centering_y)
Anova(fit)
eta_squared(fit)
estimate_contrasts(fit, contrast = "AgeCategory", p_adjust = "holm", effectsize = "emmeans", backend = "emmeans")

df_centering_y %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Mean_bin),
            SD = sd(Mean_bin),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Mean_bin)$conf.int[1],
            .upper = t.test(Mean_bin)$conf.int[2]) %>% 
  kable()

df_centering_y %>% 
  mutate(AgeCategory = case_when(AgeCategory == "2-3" ~ "2-3\n(n = 484)",
                                 AgeCategory == "5-6" ~ "5-6\n(n = 455)",
                                 AgeCategory == "8-9" ~ "8-9\n(n = 441)")) -> df_centering_y

df_centering_y %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Mean_bin),
            SD = sd(Mean_bin),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Mean_bin)$conf.int[1],
            .upper = t.test(Mean_bin)$conf.int[2]) %>% 
  ungroup() %>% 
  ggplot(aes(x = AgeCategory, y = Mean_group, fill = AgeCategory)) +
  geom_bar(stat = "identity", alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymax = .upper, ymin = .lower), width = 0.5) +
  geom_jitter(data = df_centering_y, aes(color = AgeCategory, y = Mean_bin), height = 0, width = 0.125, alpha = 0.075, size = 1) +
  geom_signif(y_position = c(1.15), xmin = c(1), xmax = c(2),
              annotations = c("**"), tip_length = 0.01, textsize = 5, vjust = 0.5) +
  scale_y_continuous(breaks = seq(0, 1.05, by = 0.25)) +
  labs(x = "Age in Months", y = "Mean Face Centering (Vertical)", tag = "C") + 
  guides(color = "none", fill = "none") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw() +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 7, color = "black"),
        strip.text = element_text(size = 10, face = "bold")) -> gp3
print(gp3)

## Centering_x
df_one %>% 
  group_by(AgeCategory, Bin_name) %>% 
  summarise(Mean_bin = mean(centering_x, na.rm = TRUE),
            .groups = "drop") -> df_centering_x

fit <- lm(Mean_bin ~AgeCategory, data = df_centering_x)
Anova(fit)
eta_squared(fit)
estimate_contrasts(fit, contrast = "AgeCategory", p_adjust = "holm", effectsize = "emmeans", backend = "emmeans")

df_centering_x %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Mean_bin),
            SD = sd(Mean_bin),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Mean_bin)$conf.int[1],
            .upper = t.test(Mean_bin)$conf.int[2]) %>% 
  kable()

df_centering_x %>% 
  mutate(AgeCategory = case_when(AgeCategory == "2-3" ~ "2-3\n(n = 484)",
                                 AgeCategory == "5-6" ~ "5-6\n(n = 455)",
                                 AgeCategory == "8-9" ~ "8-9\n(n = 441)")) -> df_centering_x

df_centering_x %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Mean_bin),
            SD = sd(Mean_bin),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Mean_bin)$conf.int[1],
            .upper = t.test(Mean_bin)$conf.int[2]) %>% 
  ungroup() %>% 
  ggplot(aes(x = AgeCategory, y = Mean_group, fill = AgeCategory)) +
  geom_bar(stat = "identity", alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymax = .upper, ymin = .lower), width = 0.5) +
  geom_jitter(data = df_centering_x, aes(color = AgeCategory, y = Mean_bin), height = 0, width = 0.125, alpha = 0.075, size = 1) +
  geom_signif(y_position = c(1.05, 1.15), xmin = c(1, 1), xmax = c(2, 3),
              annotations = c("*", "*"), tip_length = 0.01, textsize = 5, vjust = 0.5) +
  scale_y_continuous(breaks = seq(0, 1.05, by = 0.25)) +
  labs(x = "Age in Months", y = "Mean Face Centering (Horizontal)", tag = "D") + 
  guides(color = "none", fill = "none") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw() +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 7, color = "black"),
        strip.text = element_text(size = 10, face = "bold")) -> gp4
print(gp4)

ggarrange(gp1, gp2, gp3, gp4, ncol = 4, nrow = 1)
ggarrange(gp1, gp2, gp3, gp4, ncol = 4, nrow = 1) %>% 
  ggexport(filename = here("Figures", "Figure2.jpg"), res = 300, width = 2400, height = 1200)

# Figure S1
## Saliency (all features)
df_one %>% 
  group_by(AgeCategory, Bin_name) %>% 
  summarise(Mean_bin = mean(Saliency_max_face, na.rm = TRUE),
            .groups = "drop") -> df_saliency

fit <- lm(Mean_bin ~AgeCategory, data = df_saliency)
Anova(fit)
eta_squared(fit)
estimate_contrasts(fit, contrast = "AgeCategory", p_adjust = "holm", effectsize = "emmeans", backend = "emmeans")

df_saliency %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Mean_bin),
            SD = sd(Mean_bin),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Mean_bin)$conf.int[1],
            .upper = t.test(Mean_bin)$conf.int[2]) %>% 
  kable()

df_saliency %>% 
  mutate(AgeCategory = case_when(AgeCategory == "2-3" ~ "2-3\n(n = 484)",
                                 AgeCategory == "5-6" ~ "5-6\n(n = 455)",
                                 AgeCategory == "8-9" ~ "8-9\n(n = 441)")) -> df_saliency

df_saliency %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Mean_bin),
            SD = sd(Mean_bin),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Mean_bin)$conf.int[1],
            .upper = t.test(Mean_bin)$conf.int[2]) %>% 
  ggplot(aes(x = AgeCategory, y = Mean_group, fill = AgeCategory)) +
  geom_bar(stat = "identity", alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymax = .upper, ymin = .lower), width = 0.5) +
  geom_jitter(data = df_saliency, aes(color = AgeCategory, y = Mean_bin), height = 0, width = 0.125, alpha = 0.075, size = 1) +
  geom_signif(y_position = c(1.05, 1.15), xmin = c(1, 1), xmax = c(2, 3),
              annotations = c("***", "***"), tip_length = 0.01, textsize = 5, vjust = 0.5) +
  scale_y_continuous(breaks = seq(0, 1.25, by = 0.25)) +
  labs(x = "Age in Months", y = "Mean Saliency Map Value (All Features)",  tag = "A") + 
  guides(color = "none", fill = "none") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw() +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 7, color = "black"),
        strip.text = element_text(size = 10, face = "bold")) -> gp1
print(gp1)

## Saliency (DKL color)
df_one %>% 
  group_by(AgeCategory, Bin_name) %>% 
  summarise(Mean_bin = mean(Saliency_DKL, na.rm = TRUE),
            .groups = "drop") -> df_saliency_DKL

fit <- lm(Mean_bin ~AgeCategory, data = df_saliency_DKL)
Anova(fit)
eta_squared(fit)
estimate_contrasts(fit, contrast = "AgeCategory", p_adjust = "holm", effectsize = "emmeans", backend = "emmeans")

df_saliency_DKL %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Mean_bin),
            SD = sd(Mean_bin),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Mean_bin)$conf.int[1],
            .upper = t.test(Mean_bin)$conf.int[2]) %>% 
  kable()

df_saliency_DKL %>% 
  mutate(AgeCategory = case_when(AgeCategory == "2-3" ~ "2-3\n(n = 484)",
                                 AgeCategory == "5-6" ~ "5-6\n(n = 455)",
                                 AgeCategory == "8-9" ~ "8-9\n(n = 441)")) -> df_saliency_DKL

df_saliency_DKL %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Mean_bin),
            SD = sd(Mean_bin),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Mean_bin)$conf.int[1],
            .upper = t.test(Mean_bin)$conf.int[2]) %>% 
  ggplot(aes(x = AgeCategory, y = Mean_group, fill = AgeCategory)) +
  geom_bar(stat = "identity", alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymax = .upper, ymin = .lower), width = 0.5) +
  geom_jitter(data = df_saliency_DKL, aes(color = AgeCategory, y = Mean_bin), height = 0, width = 0.125, alpha = 0.075, size = 1) +
  geom_signif(y_position = c(1.05, 1.15), xmin = c(1, 1), xmax = c(2, 3),
              annotations = c("***", "***"), tip_length = 0.01, textsize = 5, vjust = 0.5) +
  scale_y_continuous(breaks = seq(0, 1.25, by = 0.25)) +
  labs(x = "Age in Months", y = "Mean Saliency Map Value (DKL Color)",  tag = "B") + 
  guides(color = "none", fill = "none") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw() +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 7, color = "black"),
        strip.text = element_text(size = 10, face = "bold")) -> gp2
print(gp2)


## Saliency (Intensity)
df_one %>% 
  group_by(AgeCategory, Bin_name) %>% 
  summarise(Mean_bin = mean(Saliency_Intensity, na.rm = TRUE),
            .groups = "drop") -> df_saliency_Intensity

fit <- lm(Mean_bin ~AgeCategory, data = df_saliency_Intensity)
Anova(fit)
eta_squared(fit)
estimate_contrasts(fit, contrast = "AgeCategory", p_adjust = "holm", effectsize = "emmeans", backend = "emmeans")

df_saliency_Intensity %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Mean_bin),
            SD = sd(Mean_bin),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Mean_bin)$conf.int[1],
            .upper = t.test(Mean_bin)$conf.int[2]) %>% 
  kable()

df_saliency_Intensity %>% 
  mutate(AgeCategory = case_when(AgeCategory == "2-3" ~ "2-3\n(n = 484)",
                                 AgeCategory == "5-6" ~ "5-6\n(n = 455)",
                                 AgeCategory == "8-9" ~ "8-9\n(n = 441)")) -> df_saliency_Intensity

df_saliency_Intensity %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Mean_bin),
            SD = sd(Mean_bin),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Mean_bin)$conf.int[1],
            .upper = t.test(Mean_bin)$conf.int[2]) %>% 
  ggplot(aes(x = AgeCategory, y = Mean_group, fill = AgeCategory)) +
  geom_bar(stat = "identity", alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymax = .upper, ymin = .lower), width = 0.5) +
  geom_jitter(data = df_saliency_DKL, aes(color = AgeCategory, y = Mean_bin), height = 0, width = 0.125, alpha = 0.075, size = 1) +
  geom_signif(y_position = c(1.05, 1.15), xmin = c(1, 1), xmax = c(2, 3),
              annotations = c("***", "***"), tip_length = 0.01, textsize = 5, vjust = 0.5) +
  scale_y_continuous(breaks = seq(0, 1.25, by = 0.25)) +
  labs(x = "Age in Months", y = "Mean Saliency Map Value (Intensity)",  tag = "C") + 
  guides(color = "none", fill = "none") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw() +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 7, color = "black"),
        strip.text = element_text(size = 10, face = "bold")) -> gp3
print(gp3)

## Saliency (Orientation)
df_one %>% 
  group_by(AgeCategory, Bin_name) %>% 
  summarise(Mean_bin = mean(Saliency_Orientation, na.rm = TRUE),
            .groups = "drop") -> df_saliency_Orientation

fit <- lm(Mean_bin ~AgeCategory, data = df_saliency_Orientation)
Anova(fit)
eta_squared(fit)
estimate_contrasts(fit, contrast = "AgeCategory", p_adjust = "holm", effectsize = "emmeans", backend = "emmeans")

df_saliency_Orientation %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Mean_bin),
            SD = sd(Mean_bin),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Mean_bin)$conf.int[1],
            .upper = t.test(Mean_bin)$conf.int[2]) %>% 
  kable()

df_saliency_Orientation %>% 
  mutate(AgeCategory = case_when(AgeCategory == "2-3" ~ "2-3\n(n = 484)",
                                 AgeCategory == "5-6" ~ "5-6\n(n = 455)",
                                 AgeCategory == "8-9" ~ "8-9\n(n = 441)")) -> df_saliency_Orientation

df_saliency_Orientation %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Mean_bin),
            SD = sd(Mean_bin),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Mean_bin)$conf.int[1],
            .upper = t.test(Mean_bin)$conf.int[2]) %>% 
  ggplot(aes(x = AgeCategory, y = Mean_group, fill = AgeCategory)) +
  geom_bar(stat = "identity", alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymax = .upper, ymin = .lower), width = 0.5) +
  geom_jitter(data = df_saliency_DKL, aes(color = AgeCategory, y = Mean_bin), height = 0, width = 0.125, alpha = 0.075, size = 1) +
  geom_signif(y_position = c(1.05, 1.15), xmin = c(1, 1), xmax = c(2, 3),
              annotations = c("***", "***"), tip_length = 0.01, textsize = 5, vjust = 0.5) +
  scale_y_continuous(breaks = seq(0, 1.25, by = 0.25)) +
  labs(x = "Age in Months", y = "Mean Saliency Map Value (Orientation)",  tag = "D") + 
  guides(color = "none", fill = "none") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw() +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 7, color = "black"),
        strip.text = element_text(size = 10, face = "bold")) -> gp4
print(gp4)

## Saliency (Flicker)
df_one %>% 
  group_by(AgeCategory, Bin_name) %>% 
  summarise(Mean_bin = mean(Saliency_Flicker, na.rm = TRUE),
            .groups = "drop") -> df_saliency_Flicker

fit <- lm(Mean_bin ~AgeCategory, data = df_saliency_Flicker)
Anova(fit)
eta_squared(fit)
estimate_contrasts(fit, contrast = "AgeCategory", p_adjust = "holm", effectsize = "emmeans", backend = "emmeans")

df_saliency_Flicker %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Mean_bin),
            SD = sd(Mean_bin),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Mean_bin)$conf.int[1],
            .upper = t.test(Mean_bin)$conf.int[2]) %>% 
  kable()

df_saliency_Flicker %>% 
  mutate(AgeCategory = case_when(AgeCategory == "2-3" ~ "2-3\n(n = 484)",
                                 AgeCategory == "5-6" ~ "5-6\n(n = 455)",
                                 AgeCategory == "8-9" ~ "8-9\n(n = 441)")) -> df_saliency_Flicker

df_saliency_Flicker %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Mean_bin),
            SD = sd(Mean_bin),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Mean_bin)$conf.int[1],
            .upper = t.test(Mean_bin)$conf.int[2]) %>% 
  ggplot(aes(x = AgeCategory, y = Mean_group, fill = AgeCategory)) +
  geom_bar(stat = "identity", alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymax = .upper, ymin = .lower), width = 0.5) +
  geom_jitter(data = df_saliency_DKL, aes(color = AgeCategory, y = Mean_bin), height = 0, width = 0.125, alpha = 0.075, size = 1) +
  geom_signif(y_position = c(1.05, 1.15), xmin = c(1, 1), xmax = c(2, 3),
              annotations = c("***", "***"), tip_length = 0.01, textsize = 5, vjust = 0.5) +
  scale_y_continuous(breaks = seq(0, 1.25, by = 0.25)) +
  labs(x = "Age in Months", y = "Mean Saliency Map Value (Flicker)",  tag = "E") + 
  guides(color = "none", fill = "none") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw() +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 7, color = "black"),
        strip.text = element_text(size = 10, face = "bold")) -> gp5
print(gp5)

## Saliency (Motion)
df_one %>% 
  group_by(AgeCategory, Bin_name) %>% 
  summarise(Mean_bin = mean(Saliency_Motion, na.rm = TRUE),
            .groups = "drop") -> df_saliency_Motion

fit <- lm(Mean_bin ~AgeCategory, data = df_saliency_Motion)
Anova(fit)
eta_squared(fit)
estimate_contrasts(fit, contrast = "AgeCategory", p_adjust = "holm", effectsize = "emmeans", backend = "emmeans")

df_saliency_Motion %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Mean_bin),
            SD = sd(Mean_bin),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Mean_bin)$conf.int[1],
            .upper = t.test(Mean_bin)$conf.int[2]) %>% 
  kable()

df_saliency_Motion %>% 
  mutate(AgeCategory = case_when(AgeCategory == "2-3" ~ "2-3\n(n = 484)",
                                 AgeCategory == "5-6" ~ "5-6\n(n = 455)",
                                 AgeCategory == "8-9" ~ "8-9\n(n = 441)")) -> df_saliency_Motion

df_saliency_Motion %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Mean_bin),
            SD = sd(Mean_bin),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Mean_bin)$conf.int[1],
            .upper = t.test(Mean_bin)$conf.int[2]) %>% 
  ggplot(aes(x = AgeCategory, y = Mean_group, fill = AgeCategory)) +
  geom_bar(stat = "identity", alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymax = .upper, ymin = .lower), width = 0.5) +
  geom_jitter(data = df_saliency_DKL, aes(color = AgeCategory, y = Mean_bin), height = 0, width = 0.125, alpha = 0.075, size = 1) +
  geom_signif(y_position = c(1.05, 1.15), xmin = c(1, 1), xmax = c(2, 3),
              annotations = c("***", "***"), tip_length = 0.01, textsize = 5, vjust = 0.5) +
  scale_y_continuous(breaks = seq(0, 1.25, by = 0.25)) +
  labs(x = "Age in Months", y = "Mean Saliency Map Value (Motion)",  tag = "F") + 
  guides(color = "none", fill = "none") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw() +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 7, color = "black"),
        strip.text = element_text(size = 10, face = "bold")) -> gp6
print(gp6)

ggarrange(gp1, gp2, gp3, gp4, gp5, gp6, ncol = 3, nrow = 2)
ggarrange(gp1, gp2, gp3, gp4, gp5, gp6, ncol = 3, nrow = 2) %>% 
  ggexport(filename = here("Figures", "FigureS1.jpg"), res = 300, width = 1800, height = 2400)


# Figure 3 (Face-Template features)
## Number of Visible Face Parts
df_one %>% 
  mutate(category = case_when(N_fp == 4 ~ "complete frontal face",
                              N_fp == 3 & eyes == 2 & nose == 1 ~ "2 eyes & nose",
                              N_fp == 3 & eyes == 2 & mouth == 1 ~ "2 eyes & mouth",
                              N_fp == 3 & eyes == 1 & nose == 1 & mouth == 1 ~ "eye, nose, & mouth",
                              N_fp == 2 & nose == 1 & mouth == 1 ~ "nose & mouth",
                              N_fp == 2 & eyes == 2 ~ "2 eyes",
                              N_fp == 2 & eyes == 1 & nose == 1 ~ "eye & nose",
                              N_fp == 2 & eyes == 1 & mouth == 1 ~ "eye & mouth",
                              N_fp == 1 & eyes == 1 ~ "eye",
                              N_fp == 1 & nose == 1 ~ "nose",
                              N_fp == 1 & mouth == 1 ~ "mouth",
                              N_fp == 0 ~ "no face parts"),
         category = fct_reorder(category, N_fp)) -> df_fp2

df_fp2 %>% 
  mutate(AgeCategory = str_c(AgeCategory, " mo")) %>% 
  group_by(AgeCategory, N_fp, category) %>% 
  summarise(N_img = n_distinct(image_name),
            .groups = "drop") %>% 
  group_by(AgeCategory) %>% 
  mutate(Total = sum(N_img),
         Prop = N_img/Total) %>% 
  ggplot(aes(x = category, y = N_img, fill = N_fp)) +
  geom_bar(stat = "identity", color = "black") +
  facet_grid(~AgeCategory) +
  scale_fill_viridis_c(option = "F") +
  coord_flip() +
  labs(y = "Number of Images with Faces", x = "Visible Face Parts", tag = "C") +
  theme_bw() +
  guides(fill = "none") +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 7, color = "black"),
        strip.text = element_text(size = 10, face = "bold")) -> gp1
print(gp1)

## Proportion of complete frontal faces
df_fp2 %>% 
  select(N_fp, category) %>% 
  distinct() -> df_fp_category

df_fp2 %>%
  group_by(AgeCategory, Bin_name, category) %>% 
  summarise(N_img = n_distinct(image_name),
            .groups = "drop_last") %>% 
  complete(category, fill = list(N_img = 0)) %>%
  ungroup() %>% 
  left_join(df_fp_category, by = "category") %>% 
  group_by(AgeCategory, Bin_name) %>% 
  mutate(Total = sum(N_img),
         Prop = N_img/Total) %>% 
  filter(N_fp == 4) -> df_frontal

fit <- lm(Prop ~AgeCategory, data = df_frontal)
Anova(fit)
eta_squared(fit)
estimate_contrasts(fit, contrast = "AgeCategory", p_adjust = "holm", effectsize = "emmeans", backend = "emmeans")

df_frontal %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Prop),
            SD = sd(Prop),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Prop)$conf.int[1],
            .upper = t.test(Prop)$conf.int[2]) %>% 
  kable()

df_frontal %>% 
  mutate(AgeCategory = case_when(AgeCategory == "2-3" ~ "2-3\n(n = 484)",
                                 AgeCategory == "5-6" ~ "5-6\n(n = 455)",
                                 AgeCategory == "8-9" ~ "8-9\n(n = 441)")) -> df_frontal

df_frontal %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Prop),
            SD = sd(Prop),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Prop)$conf.int[1],
            .upper = t.test(Prop)$conf.int[2]) %>% 
  ggplot(aes(x = AgeCategory, y = Mean_group, fill = AgeCategory)) +
  geom_bar(stat = "identity", alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymax = .upper, ymin = .lower), width = 0.5) +
  geom_signif(y_position = c(1.05, 1.15), xmin = c(2, 1), xmax = c(3, 3),
              annotations = c("**", "*"), tip_length = 0.01, textsize = 5, vjust = 0.5) +
  labs(x = "Age in Months", y = "Proportion of Complete Frontal Faces", tag = "D") +
  geom_jitter(data = df_frontal, aes(color = AgeCategory, y = Prop), height = 0, width = 0.125, alpha = 0.075, size = 1) +
  guides(color = "none", fill = "none") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(breaks = seq(0, 1.25, by = 0.25)) +
  theme_bw() +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 7, color = "black"),
        strip.text = element_text(size = 10, face = "bold")) -> gp2
print(gp2)

## face orientation histogram
df_one %>%
  filter(N_fp == 4 & !is.na(roll)) %>% 
  mutate(roll_circ = circular(roll, units = "degrees", template = "none", modulo = "2pi")) %>% 
  group_by(AgeCategory, Bin_name) %>% 
  summarise(N_FrontalFace = n_distinct(image_name),
            Mean = mean.circular(roll_circ),
            SD = sd.circular(roll_circ),
            R = rho.circular(roll_circ),
            .groups = "drop") %>%
  mutate(Mean = if_else(Mean > 180, Mean - 360, Mean)) -> df_angle_bin

# geom_hex
df_angle_bin %>% 
  mutate(AgeCategory = str_c(AgeCategory, " mo")) %>%
  ggplot(aes(x = Mean, y = SD)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_hex() +
  scale_x_continuous(limits = c(-185, 185), 
                     breaks = seq(-180, 180, 45)) +
  facet_grid(AgeCategory~.) +
  scale_fill_viridis_c(option = "B") +
  labs(x = "Circular Mean (degrees)", y = "Circular SD (degrees)", tag = "E", fill = "Count") +
  guides(fill = "none") +
  theme_bw() +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 7, color = "black"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 7, color = "black")) -> gp3
print(gp3)

## Absolute Circular Mean
df_angle_bin %>% 
  mutate(Deviation = abs(as.numeric(Mean))) -> df_dev

fit <- lm(Deviation ~ AgeCategory, data = df_dev)
Anova(fit)
eta_squared(fit)
estimate_contrasts(fit, contrast = "AgeCategory", p_adjust = "holm", effectsize = "emmeans", backend = "emmeans")

df_dev %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Deviation),
            SD = sd(Deviation),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Deviation)$conf.int[1],
            .upper = t.test(Deviation)$conf.int[2]) %>% 
  kable()

df_dev %>% 
  mutate(AgeCategory = case_when(AgeCategory == "2-3" ~ "2-3\n(n = 445)",
                                 AgeCategory == "5-6" ~ "5-6\n(n = 404)",
                                 AgeCategory == "8-9" ~ "8-9\n(n = 392)")) -> df_dev

df_dev %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(Deviation),
            SD = sd(Deviation),
            SE = SD/sqrt(N_bin),
            .lower = t.test(Deviation)$conf.int[1],
            .upper = t.test(Deviation)$conf.int[2]) %>% 
  ggplot(aes(x = AgeCategory, y = Mean_group, fill = AgeCategory)) +
  geom_bar(stat = "identity", alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymax = .upper, ymin = .lower), width = 0.5) +
  geom_signif(y_position = c(185, 185, 195), xmin = c(1, 2.05, 1), xmax = c(1.95, 3, 3),
              annotations = c("**", "***", "***"), tip_length = 0.01, textsize = 5, vjust = 0.5) +
  labs(x = "Age in Months", y = "Absolute Circular Mean (degrees)", tag = "F") +
  geom_jitter(data = df_dev, aes(color = AgeCategory, y = Deviation), height = 0, width = 0.125, alpha = 0.075, size = 1) +
  guides(color = "none", fill = "none") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(breaks = seq(0, 180, by = 45)) +
  theme_bw() +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 7, color = "black"),
        strip.text = element_text(size = 10, face = "bold")) -> gp4
print(gp4)

## Analysis of Circular SD
fit <- lm(SD ~AgeCategory, data = df_angle_bin)
Anova(fit)
eta_squared(fit)
estimate_contrasts(fit, contrast = "AgeCategory", p_adjust = "holm", effectsize = "emmeans", backend = "emmeans")

df_angle_bin %>% 
  rename(CircularSD = "SD") %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(CircularSD),
            SD = sd(CircularSD),
            SE = SD/sqrt(N_bin),
            .lower = t.test(CircularSD)$conf.int[1],
            .upper = t.test(CircularSD)$conf.int[2]) %>% 
  kable()

df_angle_bin %>% 
  mutate(AgeCategory = case_when(AgeCategory == "2-3" ~ "2-3\n(n = 445)",
                                 AgeCategory == "5-6" ~ "5-6\n(n = 404)",
                                 AgeCategory == "8-9" ~ "8-9\n(n = 392)")) -> df_angle_bin

df_angle_bin %>% 
  rename(CircularSD = "SD") %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = n_distinct(Bin_name),
            Mean_group = mean(CircularSD),
            SD = sd(CircularSD),
            SE = SD/sqrt(N_bin),
            .lower = t.test(CircularSD)$conf.int[1],
            .upper = t.test(CircularSD)$conf.int[2]) %>% 
  ggplot(aes(x = AgeCategory, y = Mean_group, fill = AgeCategory)) +
  geom_bar(stat = "identity", alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymax = .upper, ymin = .lower), width = 0.5) +
  geom_signif(y_position = c(2.55, 2.7), xmin = c(1, 1), xmax = c(2, 3),
              annotations = c("**", "***"), tip_length = 0.01, textsize = 5, vjust = 0.5) +
  labs(x = "Age in Months", y = "Circular SD (degrees)", tag = "G") +
  geom_jitter(data = df_angle_bin, aes(color = AgeCategory, y = SD), height = 0, width = 0.125, alpha = 0.075, size = 1) +
  guides(color = "none", fill = "none") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0, 2.7)) +
  theme_bw() +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 7, color = "black"),
        strip.text = element_text(size = 10, face = "bold")) -> gp5
print(gp5)

load_image_as_ggplot <- function(path) {
  img <- magick::image_read(path)
  ggplot() +
    ggpubr::background_image(img) +
    theme_void()
}

# load_image_as_ggplot(here("P012_26000_Cam57_MOVI0020_056.jpg")) + 
#   labs(tag = "A") +
#   theme(plot.tag = element_text(size = 10, face = "bold")) -> jpg1
# load_image_as_ggplot(here("P037_26000_Cam55_MOVI0028_293.jpg")) -> jpg2
# 
# load_image_as_ggplot(here("P012_26000_Cam57_MOVI0022_001.jpg")) + 
#   labs(tag = "B") +
#   theme(plot.tag = element_text(size = 10, face = "bold")) -> jpg3
# load_image_as_ggplot(here("P037_26000_Cam55_MOVI0031_101.jpg")) -> jpg4
# 
# ggarrange(jpg1, jpg2, jpg3, jpg4, ncol = 4, align = "h") -> gp_upper

gp_middle <- ggarrange(gp1, gp2, widths = c(3, 1))
gp_lower <- ggarrange(gp3, ggarrange(gp4, gp5, ncol = 1), ncol = 2, widths = c(3, 1))
gp_lower <- ggarrange(gp3, gp4, gp5, ncol = 3, widths = c(2, 1, 1))

# ggarrange(gp_upper, gp_middle, gp_lower, ncol = 1, heights = c(1, 2, 2))
# ggarrange(gp_upper, gp_middle, gp_lower, ncol = 1, heights = c(3, 10, 10), align = "h") %>% 
#   ggexport(filename = here("Figures", "Figure3.jpg"), res = 300, width = 2400, height = 2760)


ggarrange(gp_middle, gp_lower, ncol = 1, heights = c(1, 1), align = "h") %>% 
   ggexport(filename = here("Figures", "Figure3C-G.jpg"), res = 300, width = 2400, height = 2400)
