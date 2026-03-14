library(here)
library(tidyverse)
library(modelbased)
library(ggsignif)
library(ggpubr)
library(knitr)

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

# read data
here("Data", "ImageList_face_master.csv") %>% 
  read_csv(col_types = "ccccdddddddddddddddddddd") %>% 
  left_join(select(df_bin, image_name, Bin_name), by = "image_name") %>% 
  relocate(Bin_name, .after = "AgeCategory") -> df_face
nrow(df_face)

df_face %>% 
  filter(N_face == 1) -> df_one

# Observed Saliency
df_one %>% 
  group_by(AgeCategory, Bin_name) %>% 
  summarise(Mean_bin = mean(Saliency_max_face, na.rm = TRUE),
            .groups = "drop") -> df_saliency

df_saliency %>% 
  group_by(AgeCategory) %>% 
  summarise(N_bin = sum(!is.na(Mean_bin)),
            Mean_group = mean(Mean_bin, na.rm = TRUE),
            SD = sd(Mean_bin, na.rm = TRUE),
            SE = SD/sqrt(N_bin)) %>%
  ungroup() -> df_saliency_obs

# read Surrogation data
file_rnd <- list.files(here("Data"), pattern = "FaceSaliency2_rnd1_")
tibble(file = here("Data", file_rnd)) %>%
  filter(!str_detect(file, pattern = ".csv.zip")) %>% 
  rowwise() %>% 
  mutate(csv = map(file, ~read_csv(.x, col_types = "ccddd"))) %>% 
  ungroup() %>% 
  unnest(csv) %>% 
  select(!file) %>% 
  left_join(select(df_bin, AgeCategory, image_name, Bin_name), c("AgeCategory", "image_name")) %>% 
  group_by(AgeCategory, Bin_name, Rep) %>% 
  summarise(Mean_bin = mean(saliency, na.rm = TRUE),
            .groups = "drop") %>% 
  group_by(AgeCategory, Rep) %>% 
  summarise(N_bin = sum(!is.na(Mean_bin)),
            Mean_group = mean(Mean_bin, na.rm = TRUE),
            .groups = "drop") -> df_saliency_rnd

df_saliency_obs %>% 
  mutate(AgeCategory = str_c(AgeCategory, " mo")) -> df_saliency_obs

df_saliency_rnd %>% 
  group_by(AgeCategory) %>% 
  mutate(BootstrapMean = mean(Mean_group),
         .groups = "drop") %>% 
  mutate(AgeCategory = str_c(AgeCategory, " mo")) %>% 
  ggplot(aes(x = Mean_group)) +
  geom_histogram(binwidth = 0.0025, color = "black", fill = "white") +
  geom_vline(aes(xintercept = BootstrapMean), linetype = 2) +
  facet_wrap(~AgeCategory, ncol = 1) +
  geom_vline(data = df_saliency_obs, aes(xintercept = Mean_group, color = AgeCategory), linewidth = 1) +
  scale_color_viridis_d() +
  labs(x = "Group Mean Saliency Map Value", y = "Count", color = "Age in Months", tag = "B") +
  guides(color = "none") +
  theme_bw() +
  theme(plot.tag = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 15, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 15)) -> gp2
print(gp2)

# Diffrence between age based on Bootstrap samples
df_saliency_obs %>% 
  select(AgeCategory, Mean_group) %>% 
  pivot_wider(names_from = "AgeCategory", values_from = "Mean_group") %>% 
  mutate(diff12 = `2-3 mo` - `5-6 mo`,
         diff13 = `2-3 mo` - `8-9 mo`,
         diff23 = `5-6 mo` - `8-9 mo`) %>% 
  select(starts_with("diff")) %>% 
  pivot_longer(starts_with("diff"), values_to = "diff") %>% 
  mutate(Pair = c("2-3 mo - 5-6 mo", "2-3 mo - 8-9 mo", "5-6 mo - 8-9 mo")) %>% 
  select(!name) -> df_diff_obs

df_saliency_rnd %>% 
  filter(AgeCategory == "2-3") %>% 
  pull(Mean_group) -> age1

df_saliency_rnd %>% 
  filter(AgeCategory == "5-6") %>% 
  pull(Mean_group) -> age2

df_saliency_rnd %>% 
  filter(AgeCategory == "8-9") %>% 
  pull(Mean_group) -> age3

expand_grid(age1 = age1,
            age2 = age2) %>% 
  mutate(diff = age1 - age2,
         Pair = "2-3 mo - 5-6 mo") %>% 
  select(Pair, diff) -> diff12

expand_grid(age1 = age1,
            age3 = age3) %>% 
  mutate(diff = age1 - age3,
         Pair = "2-3 mo - 8-9 mo") %>% 
  select(Pair, diff) -> diff13

expand_grid(age2 = age2,
            age3 = age3) %>% 
  mutate(diff = age2 - age3,
         Pair = "5-6 mo - 8-9 mo") %>% 
  select(Pair, diff) -> diff23

bind_rows(diff12, diff13, diff23) %>%
  group_by(Pair) %>% 
  mutate(upr = quantile(diff, probs = 0.975),
         lwr = quantile(diff, probs = 0.025)) %>% 
  ggplot(aes(x = diff)) +
  geom_histogram(binwidth = 0.0025, fill = "white", color = "black") +
  geom_vline(aes(xintercept = upr), linetype = 2) +
  geom_vline(aes(xintercept = lwr), linetype = 2) +
  geom_vline(data = df_diff_obs, aes(xintercept = diff, color = Pair), linewidth = 1) +
  facet_wrap(~Pair, ncol = 1) +
  labs(x = "Diffrence in Group Mean Saliency Map Value", y = "Count", tag = "C") +
  guides(color = "none") +
  theme_bw() +
  theme(plot.tag = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 15, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 18, face = "bold")) -> gp3
print(gp3)

ggarrange(gp2, gp3) %>% 
  ggexport(filename = here("Figures", "FigureS2bc.jpg"), res = 300, width = 4000, height = 3000)