# Date: 8.14.2024
# Code for: A. cantonensis encounter study
# Author: Randi Rollins

# Load libraries
library(stats)
library(readr)
library(readxl)
library(FSA) # Essentially a wrapper for 'dunn.test' in library(dunn.test). FSA provides a nicer output.
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(stringr)

# Import data
df <- read_excel("Food pref data.xlsx")

# Convert to factor
df$species <- as.factor(df$species)
df$food_type <- as.factor(df$food_type)


################  Unhealthy (fat) vs healthy diet rat feces - t-test
# Filter the data where food_type is 'feces'
filtered_data <- subset(df, food_type == 'feces')

# Group by feces_type and calculate summary statistics for feces RIR
sum_stats <- filtered_data %>%
  group_by(feces_type) %>%
  summarize(
    mean_rir = mean(rir, na.rm = TRUE),
    median_rir = median(rir, na.rm = TRUE),
    sd_rir = sd(rir, na.rm = TRUE),
    var_rir = var(rir, na.rm = TRUE),
    n = n()
  )
sum_stats

# Separate fat and healthy groups
fat <- filtered_data[filtered_data$feces_type == 'fat', 'rir']
healthy <- filtered_data[filtered_data$feces_type == 'healthy', 'rir']

# Independent two-sample t-test
t.test(fat, healthy, var.equal = FALSE)


################  Food preference differences among species
# Change data frame format - include columns for each food type, with RIR values
data_filtered <- df %>%
  select(`snail_#`, snail_ID, species, food_type, rir)
data_wide <- data_filtered %>%
  pivot_wider(names_from = food_type, values_from = rir)
data <- data_wide %>%
  rename(papaya = `papaya`, romaine = `romaine`, hibiscus = `hibiscus`, feces = `feces`)

# Kruskal-wallis tests   
kruskal.test(data$romaine, data$species)
kruskal.test(data$feces, data$species)
kruskal.test(data$hibiscus, data$species)
kruskal.test(data$papaya, data$species)

# Post hoc Dunn's test for feces only
dunnTest(data$feces, data$species, method = "bonferroni")


################  Correlation between feces preference and published A. cantonensis prevalences
# Perform Spearman rank correlation between feces RIR and prevalence
cor.test(filtered_data$rir, filtered_data$prevalence, method = "spearman", exact = F)


################  Proportion and RIR values for Table 2
# For RIR values
aggregate(cbind(papaya, romaine, hibiscus, feces) ~ species, data = data, FUN = mean)
# For proportions

data_individual_total <- df %>%
  group_by(snail_ID) %>%
  summarise(total_intake = sum(ate, na.rm = TRUE))

data_with_proportions <- df %>%
  left_join(data_individual_total, by = "snail_ID") %>%
  mutate(proportion = ate / total_intake) %>%
  select(snail_ID, species, food_type, proportion)

data_with_proportions %>%
  group_by(species, food_type) %>%
  summarise(average_proportion = mean(proportion, na.rm = TRUE)) %>%
  spread(key = food_type, value = average_proportion)


################  Create boxplots for Figure 2
# Change 'romaine' to 'LETTUCE'lettuce'
df.plot <- df %>%
  mutate(food_type = recode(food_type, romaine = 'lettuce'))

ggplot(df.plot, aes(x = species, y = rir, fill = species)) +
  geom_boxplot(notch = F) +
  facet_grid(cols = vars(food_type), scales = "free_y", switch = "x", 
             labeller = labeller(food_type = str_to_title)) +  
  labs(x = NULL,  # Remove x-axis title
       y = "Relative Intake Ratio (RIR)",
       fill = "Species") +  # Capitalize "Species" in legend
  scale_fill_manual(values = c(
    "P. martensi" = viridis(4)[4],  # Bright red shade for Parmarion martensi
    "L. alte" = viridis(4)[5],  # Earth tone brown
    "L. fulica" = viridis(4)[2],  # Earth tone sienna
    "V. cubensis" = viridis(4)[3]   # Earth tone olive green
  )) +  # Customize colors here
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    strip.background = element_blank(),
    strip.placement = "outside",  # Move strip to outside the plot
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.title.x = element_blank(),  # Remove x-axis title
    panel.spacing = unit(0.5, "lines"),
    panel.background = element_rect(fill = NA, color = "black"),
    legend.text = element_text(face= "italic")
  )

# END
