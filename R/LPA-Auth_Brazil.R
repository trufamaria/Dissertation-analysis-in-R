library(haven)
library(tidyverse)
library(tidyLPA)

# Import your .sav file — adjust the path to where your file is saved
data <- read_sav("/Users/marinapetric/Library/Mobile Documents/com~apple~CloudDocs/Documents/Dissertation/Data surveys/Final/Brazil_March26.sav")

# Select only the items from your scale
# Replace item1, item2 etc. with your actual variable names
scale_items <- data %>%
  select(AT1, AT2, AT3, AT4 ) %>%
  na.omit()  # removes missing values

head(scale_items)


lpa_results <- scale_items %>%
  scale() %>%
  estimate_profiles(1:4, package = "mclust")


tidyLPA::get_fit(lpa_results)

best_model <- scale_items %>%
  scale() %>%
  estimate_profiles(3, package = "mclust")

# Plot the profiles
plot_profiles(best_model)

# See the profile means
tidyLPA::get_estimates(best_model)

#best_model <- scale_items %>%
scale() %>%
  estimate_profiles(3, package = "mclust")

# Plot the profiles
plot_profiles(best_model)

# See the profile means
tidyLPA::get_estimates(best_model)


# Plot

library(ggplot2)
library(tidyverse)

# Get the estimates
estimates <- tidyLPA::get_estimates(best_model)

# Filter only the item means (not variances)
plot_data <- estimates %>%
  filter(Category == "Means") %>%
  mutate(Class = factor(Class, labels = c("Low Auth", "Medium Auth", "High Auth")))


best_model

tidyLPA::get_estimates(best_model)

estimates <- tidyLPA::get_estimates(best_model)
head(estimates)

plot_data <- estimates %>%
  filter(Category == "Means") %>%
  mutate(Class = factor(Class, labels = c("Low Auth", "Medium Auth", "High Auth")))

ggplot(plot_data, aes(x = Parameter, y = Estimate, 
                      group = Class, color = Class)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Estimate - se, ymax = Estimate + se), 
                width = 0.2, linewidth = 0.8) +
  scale_color_manual(values = c("#2AA119", "#2C7BB6","#D7191C")) +
  labs(
    title = "Latent Profiles of Authoritarian Attitutdes - Brazil",
    x = "Scale Item",
    y = "Standardized Mean",
    color = "Profile"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )


