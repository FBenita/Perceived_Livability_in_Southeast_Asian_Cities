library(lavaan)
library(semPlot)
library(psych)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(classInt)
library(sf)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)

cat("\014") 
rm(list = ls())
graphics.off()

# Read the data
data <- read.csv("C:\\Users\\all_responses.csv", stringsAsFactors = FALSE)
data <- data[data$Age >= 18, ]

# Select survey items grouped by conceptual domains 
qol_items <- data %>%
  select(
    # SAFETY & SECURITY
    q1_2,  # Q2: I feel safe in this neighborhood
    q1_6,  # Q6: There is good street lighting in this neighborhood
    
    # SOCIAL COHESION
    q4_6,  # Q26: In our neighbourhood neighbours look out for each other
    
    # FAMILY FRIENDLINESS
    q1_3,  # Q3: This neighbourhood is a good place for older people
    q1_4,  # Q4: This neighbourhood is a good place for children
    
    # URBAN MOBILITY & INFRASTRUCTURE
    q1_5,  # Q5: The neighborhood has adequate road and parking facilities
    q2_3,  # Q13: There is affordable public transport service to/from my neighbourhood
    q3_3,  # Q19: The footpaths in this neighbohood are adequate
    
    # HOUSING STABILITY & QUALITY
    q4_2,  # Q22: This is a well-maintained housing area
    q4_4,  # Q24: I don't feel overcrowded in my house
    q4_5,  # Q25: The sub-letting in the neighbourhood doesn't cause issues with the neighbours
    
    # BASIC URBAN SERVICES
    q3_1,  # Q17: This neighbourhood has effective sanitary system
    q3_2,  # Q18: This residential neighbourhood has an effective waste management system
    
    # AMENITIES & WELL-BEING
    q2_1,  # Q11: The neighborhood has good rec./leisure facilities
    q2_2,  # Q12: There are enough shops where I can obtain basic necessities
    
    # HEALTH SERVICES ACCESS
    q4_7   # Q27: The healthcare facilities in my neighborhood are of good quality
  )

# Recode Likert values as per your scheme
# (Original: 1-5 → New: 1,2,3→4,4→5,5→3)
qol_items <- qol_items %>%
  mutate_all(~ recode(., `1` = 1, `2` = 2, `3` = 4, `4` = 5, `5` = 3))

# Convert to numeric 
qol_items <- mutate_all(qol_items, as.numeric)

# Count complete cases (rows with no missing values)
sum(complete.cases(qol_items))


# EFA with 3–4 factors
setwd("C:\\Users\\Results")
png("efa_parallel_plot.png", width = 6, height = 6, units = "in", res = 300)
par(family = "sans", cex.lab = 1.5, cex.axis = 1.2, cex.main = 1.5) # Adjust font and size
fa.parallel(qol_items, fa = "fa")
dev.off()


# Try n-factor EFA
efa_result <- fa(qol_items, nfactors = 5, rotate = "oblimin", fm = "ml")
print(efa_result, cut = 0.3)

# Scree plot
#plot(efa_result)

#Compute Factor Scores
efa_scores <- factor.scores(qol_items, efa_result, method = "regression")$scores

#Add them back to the dataset
data_with_scores <- cbind(data, efa_scores)


# Rename the 5 factors from ML3, ML1, ML5, ML2, ML4 in that order
colnames(data_with_scores)[(ncol(data_with_scores)-4):ncol(data_with_scores)] <- c(
  "Basic_Urban_Services",      # ML3: q1_2, q3_1, q3_2, q4_2, q4_6
  "Family_Friendliness",       # ML1: q1_3, q1_4
  "Accessibility_Provisions",  # ML5: q2_2, q2_3, q4_7
  "Public_Space_Mobility",     # ML2: q1_6, q1_5, q3_3, q2_1
  "Housing_Stability"          # ML4: q4_4, q4_5
)



# Transform to long format for all five factors
data_long <- data_with_scores %>%
  select(City, Basic_Urban_Services, Family_Friendliness, Accessibility_Provisions,
         Public_Space_Mobility, Housing_Stability) %>%
  pivot_longer(-City, names_to = "Domain", values_to = "Score")


#Visualization
ggplot(data_long, aes(x = Domain, y = Score, fill = City)) +
  geom_boxplot(position = position_dodge(0.8)) +
  theme_minimal() +
  labs(title = "Distribution of Livability Factors by City",
       y = "Factor Score (centered)", x = "Domain") +
  coord_flip()

#Custom Domain Labels & Order
plot_data <- data_long %>%
  mutate(
    Domain = factor(Domain, levels = c(
      "Basic_Urban_Services",
      "Family_Friendliness",
      "Accessibility_Provisions",
      "Public_Space_Mobility",
      "Housing_Stability"
    )),
    Domain_label = recode(Domain,
                          "Basic_Urban_Services" = "Basic Urban Services",
                          "Family_Friendliness" = "Family-Friendliness",
                          "Accessibility_Provisions" = "Accessibility & Services",
                          "Public_Space_Mobility" = "Public Space & Mobility",
                          "Housing_Stability" = "Cleanliness & Amenities"
    )
  )

#Faceted Boxplot (3 × 2 layout, last cell blank)
setwd("C:\\Users\\L03565094\\Dropbox\\Francisco\\Papers2023\\Surveys_Comparative_analysis\\Results")
png("Distribution_QoL_factors.png", width = 6, height = 8, units = 'in', res = 300)
# Create the plot
a <- ggplot(plot_data, aes(x = City, y = Score, fill = City)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8) +
  facet_wrap(~ Domain_label, ncol = 1, nrow = 5) +
  coord_flip() + # Flip axes for horizontal boxplots
  labs(
    y = "Factor Score (centered)"
  ) +
  theme_bw(base_size = 18) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.title.y = element_blank(), # Remove y-axis title (was x-axis before flip)
    axis.text.y = element_blank(), # Remove y-axis labels (city names)
    axis.ticks.y = element_blank(), # Remove y-axis ticks
    legend.position = "bottom"
  )
print(a)
dev.off()
graphics.off()



#Long Boxplot vertical
setwd("C:\\Users\\Results")
png("Long_boxplot_QoL_factors.png", width = 8, height = 8, units = 'in', res = 300)
# Create the plot
a <- ggplot(plot_data, aes(x = Domain, y = Score, fill = City)) +
  geom_boxplot(position = position_dodge(0.8)) +
  theme_minimal() +
  labs(title = "",
       y = "Factor Score (centered)", x = "") +
  coord_flip()
print(a)
dev.off()
graphics.off()


# Composite QoL Index (based on 5 factors)
data_with_scores$QoL_Index <- rowMeans(data_with_scores %>%
                                         select(Basic_Urban_Services, Family_Friendliness, Accessibility_Provisions,
                                                Public_Space_Mobility, Housing_Stability), na.rm = TRUE)


###########ANOVA TESTS######################
# Define factors
factors <- c("QoL_Index", "Basic_Urban_Services", "Family_Friendliness", 
             "Accessibility_Provisions", "Public_Space_Mobility", "Housing_Stability")

# ---------- 1. Run ANOVAs ----------
anova_models <- lapply(factors, function(f) {
  aov(as.formula(paste(f, "~ City")), data = data_with_scores)
})
names(anova_models) <- factors

# ---------- 2. Extract F and p-values ----------
anova_summary_df <- do.call(rbind, lapply(names(anova_models), function(f) {
  model <- summary(anova_models[[f]])
  f_value <- model[[1]]["City", "F value"]
  p_value <- model[[1]]["City", "Pr(>F)"]
  data.frame(
    Domain = f,
    F_value = round(f_value, 2),
    p_value = format.pval(p_value, digits = 3, eps = .001),
    Significance = symnum(p_value, corr = FALSE, na = FALSE,
                          cutpoints = c(0, .001, .01, .05, .1, 1),
                          symbols = c("***", "**", "*", ".", ""))
  )
}))
rownames(anova_summary_df) <- NULL

# ---------- 3. Tukey HSD Post-Hoc ----------
tukey_results <- lapply(anova_models, TukeyHSD)

tukey_df <- bind_rows(lapply(names(tukey_results), function(f) {
  result <- as.data.frame(tukey_results[[f]]$City)
  result$Comparison <- rownames(result)
  result$Domain <- f
  rownames(result) <- NULL
  result
}), .id = "id") %>%
  select(Domain, Comparison, diff, lwr, upr, `p adj`) %>%
  rename(
    Mean_Diff = diff,
    CI_Lower = lwr,
    CI_Upper = upr,
    P_Adj = `p adj`
  ) %>%
  mutate(Significance = symnum(P_Adj, corr = FALSE, na = FALSE,
                               cutpoints = c(0, .001, .01, .05, .1, 1),
                               symbols = c("***", "**", "*", ".", "")))

# ---------- 5. Export Tables ----------
write.csv(anova_summary_df, "ANOVA_Summary_Table.csv", row.names = FALSE)
write.csv(tukey_df, "Tukey_Pairwise_Comparisons.csv", row.names = FALSE)



# Use `tukey_df` table
heatmap_data <- tukey_df %>%
  mutate(Comparison = factor(Comparison)) %>%
  mutate(Signif = case_when(
    P_Adj < 0.01 ~ "***",
    P_Adj < 0.05 ~ "**",
    P_Adj < 0.1 ~ "*",
    TRUE ~ ""
  ))

# Relabel and order the domains (factors)
domain_labels <- c(
  "QoL_Index" = "Livability Index",
  "Basic_Urban_Services" = "Basic Urban Services",
  "Family_Friendliness" = "Family-Friendliness",
  "Accessibility_Provisions" = "Accessibility & Services",
  "Public_Space_Mobility" = "Public Space & Mobility",
  "Housing_Stability" = "Cleanliness & Amenities"
)

# Apply labels and enforce factor order
heatmap_data <- heatmap_data %>%
  mutate(Domain = factor(domain_labels[Domain],
                         levels = domain_labels))  # Factor order = label order

#Figure Tukey Pairwise Comparisons (Mean Differences)
setwd("C:\\Users\\Results")
png("Tukey_Pairwise_Comparisons.png", width = 8, height = 8, units = 'in', res = 300)
# Create the plot

a <- ggplot(heatmap_data, aes(x = Comparison, y = Domain, fill = Mean_Diff)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Signif), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "", x = "", y = "", fill = "Mean Difference") +
  theme_bw(base_size = 18) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
    legend.position = "bottom",
    legend.text = element_text(size = 10), # Adjust legend text size
    legend.title = element_text(size = 12) # Adjust legend title size
  )
# Print and save the plot
print(a)
dev.off()
graphics.off()






##########Prepare for regression 
########## Prepare cleaned_data for regression ##########
cleaned_data <- data_with_scores %>%
  # Define factors with appropriate labels
  mutate(
    Gender = factor(Gender, levels = c("M", "F")),  # M = reference
    Marital_status = factor(Marital_status,
                            levels = c(1, 2, 3, 4),
                            labels = c("Not_Married", "Married", "Others", "No_Response")),
    Income = factor(Income,
                    levels = c(1, 2, 3, 4),
                    labels = c("Low", "Middle", "High", "No_Response")),
    Education_level = factor(Education_level,
                             levels = c(1, 2, 3),
                             labels = c("Low_Ed", "Medium_Ed", "High_Ed")),
    Property_ownership = factor(Property_ownership,
                                levels = c(1, 2, 3, 97),
                                labels = c("Rented", "Owned", "Temporal", "Other"))
  ) %>%
  # Filter out invalid responses
  filter(
    Income != "No_Response",
    Property_ownership != "Other",
    Marital_status != "No_Response"
  ) %>%
  # Set reference levels for modeling
  mutate(
    Gender = relevel(Gender, ref = "M"),
    Marital_status = relevel(Marital_status, ref = "Married"),
    Income = relevel(Income, ref = "Middle"),
    Education_level = relevel(Education_level, ref = "Medium_Ed"),
    Property_ownership = relevel(Property_ownership, ref = "Owned")
  )

# Create Age_Group and set reference
cleaned_data <- cleaned_data %>%
  mutate(Age_Group = factor(cut(Age,
                                breaks = c(18, 34, 49, Inf),
                                labels = c("Young (18–34)", "Middle-aged (35–49)", "Older (50+)")))) %>%
  mutate(Age_Group = relevel(Age_Group, ref = "Middle-aged (35–49)"))



# Updated function to assign significance stars
get_stars <- function(p) {
  if (p < 0.01) return("***")
  else if (p < 0.05) return("**")
  else if (p < 0.1) return("*")
  else return("")
}

# List of dependent variables
outcomes <- c("QoL_Index", 
              "Basic_Urban_Services", "Family_Friendliness", 
              "Accessibility_Provisions", "Public_Space_Mobility", 
              "Housing_Stability")

# Run all regressions and extract results
all_models <- lapply(outcomes, function(outcome) {
  model <- lm(as.formula(paste(outcome, "~ Age_Group + Gender + Marital_status + Income + 
                                Education_level + Property_ownership + factor(City)")),
              data = cleaned_data)
  
  # Extract coefficients
  coef_tbl <- tidy(model) %>%
    mutate(
      Outcome = outcome,
      Estimate = round(estimate, 3),
      Std_Error = round(std.error, 3),
      Stars = sapply(p.value, get_stars),
      Coef_Line = paste0(Estimate, " ", Stars),
      SE_Line = paste0("(", Std_Error, ")")
    ) %>%
    select(Outcome, term, Coef_Line, SE_Line)
  
  # Extract R-squared and append as final row
  r2_value <- summary(model)$adj.r.squared
  r2_row <- data.frame(
    Outcome = outcome,
    term = "R-squared",
    Coef_Line = as.character(round(r2_value, 3)),
    SE_Line = ""
  )
  
  bind_rows(coef_tbl, r2_row)
})

# Combine all models into one dataframe
regression_results_all <- bind_rows(all_models)

# Export to CSV
write.csv(regression_results_all, "Regression_Results_All_Factors.csv", row.names = FALSE)

# Format regression results with SE in parentheses under coefficient
regression_results_formatted <- regression_results_all %>%
  mutate(
    # For regular terms: show coef and std err in new line
    Coef_SE = ifelse(term %in% c("Adjusted R-squared", "R-squared"),
                     paste0(term, ": ", Coef_Line),
                     paste0(Coef_Line, "\n(", SE_Line, ")"))
  ) %>%
  select(Outcome, term, Coef_SE)

write.csv(regression_results_formatted, "Regression_Results_Formatted_Coef_SE.csv", row.names = FALSE)







################################################################
################################################################

# Create a transformed copy for analysis
data_clean <- data_with_scores %>%
  mutate(
    Marital_status = factor(Marital_status,
                            levels = c(1, 2, 3, 4),
                            labels = c("Not_Married", "Married", "Others", "No_Response")),
    Income = factor(Income,
                    levels = c(1, 2, 3, 4),
                    labels = c("Low", "Middle", "High", "No_Response")),
    Education_level = factor(Education_level,
                             levels = c(1, 2, 3),
                             labels = c("Low_Ed", "Medium_Ed", "High_Ed")),
    Property_ownership = case_when(
      Property_ownership == 1 ~ "Rented",
      Property_ownership == 2 ~ "Owned",
      Property_ownership == 3 ~ "Temporal",
      Property_ownership > 3 ~ "Other",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Rented", "Owned", "Temporal", "Other"))
  )


# List of categorical variables to summarize (in data_clean)
cat_vars <- c("Gender", "Marital_status", "Income", 
              "Education_level", "Property_ownership")

# 1. AGE: numerical summary as string "Mean (SD)"
age_summary <- data_clean %>%
  group_by(City) %>%
  summarise(
    stat = sprintf("%.2f (%.2f)", mean(Age, na.rm = TRUE), sd(Age, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = City, values_from = stat) %>%
  mutate(Variable = "Age", Category = "") %>%
  select(Variable, Category, everything())

# 2. CATEGORICALS: summary as "N (P%)"
cat_summary <- lapply(cat_vars, function(var) {
  data_clean %>%
    group_by(City, !!sym(var)) %>%
    summarise(N = n(), .groups = "drop_last") %>%
    mutate(Percentage = round(100 * N / sum(N), 1)) %>%
    ungroup() %>%
    rename(Category = !!sym(var)) %>%
    mutate(
      Variable = var,
      stat = paste0(N, " (", Percentage, "%)")
    ) %>%
    select(Variable, Category, City, stat)
}) %>%
  bind_rows()

# 3. Reshape categorical summary to wide format
cat_summary_wide <- cat_summary %>%
  pivot_wider(names_from = City, values_from = stat)

# 4. Combine age + categoricals
summary_formatted <- bind_rows(age_summary, cat_summary_wide)

# 5. Add total observations per city, formatted as characters
n_obs <- data_clean %>%
  group_by(City) %>%
  summarise(n = n()) %>%
  mutate(n = as.character(n)) %>%  # <- convert to character
  pivot_wider(names_from = City, values_from = n) %>%
  mutate(Variable = "Obs", Category = "") %>%
  select(Variable, Category, everything())

summary_formatted <- bind_rows(summary_formatted, n_obs)

# 6. Export to CSV
write.csv(summary_formatted, "Formatted_Summary_By_City.csv", row.names = FALSE)

##################################################################

table(data$City, data$Area...District)

##################################################################
# Maps of livability indicators
##################################################################

# Step 1: Define the score variables to summarize and classify
score_vars <- c("Basic_Urban_Services", "Family_Friendliness", 
                "Accessibility_Provisions", "Public_Space_Mobility", 
                "Housing_Stability", "QoL_Index")

# Step 2: Compute mean score per region within each city
regional_means <- data_with_scores %>%
  group_by(City, `Area...District`) %>%
  summarise(across(all_of(score_vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

# Optional: rename for clarity
regional_means <- regional_means %>%
  rename(Region = `Area...District`)

# Step 3: Classify regional mean values using Jenks breaks (based on regions)
jenks_breaks_region <- list()  # Store cutoffs

for (var in score_vars) {
  # Compute Jenks breaks on regional means
  jenks <- classIntervals(regional_means[[var]], n = 3, style = "jenks", na.rm = TRUE)
  jenks_breaks_region[[var]] <- jenks$brks  # Save breakpoints
  
  # Apply classification using cut()
  regional_means[[paste0(var, "_Level")]] <- cut(
    regional_means[[var]],
    breaks = jenks$brks,
    labels = c("Low", "Medium", "High"),
    include.lowest = TRUE,
    right = TRUE
  )
}


################Plotting map for PP
#Selecting PH
regional_phnom_penh <- regional_means %>%
  filter(City == "Phnom Penh")

# Replace with your actual file path
phnom_penh_shape <- st_read("C:\\Users\\Commune-2011.shp")

# Check column names to find the one matching 'Region' (e.g., 'Commune_Name')
names(phnom_penh_shape)

# Join classification data to the spatial layer
map_data <- phnom_penh_shape %>%
  left_join(regional_phnom_penh, by = c("COMM_NAME" = "Region"))

#Define a plotting funtion for one variable
plot_livability_map <- function(data, variable, title) {
  ggplot(data) +
    geom_sf(aes_string(fill = variable), color = "white") +
    scale_fill_manual(values = c("Low" = "#d73027", "Medium" = "#fee08b", "High" = "#1a9850"),
                      na.value = "grey90") +
    labs(title = title, fill = "Level") +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank())
}


# Plot each map
p1 <- plot_livability_map(map_data, "Basic_Urban_Services_Level", "Basic Urban Services")
p2 <- plot_livability_map(map_data, "Family_Friendliness_Level", "Family Friendliness")
p3 <- plot_livability_map(map_data, "Accessibility_Provisions_Level", "Accessibility & Services")
p4 <- plot_livability_map(map_data, "Public_Space_Mobility_Level", "Public Space & Mobility")
p5 <- plot_livability_map(map_data, "Housing_Stability_Level", "Cleanliness & Amenities")
p6  <- plot_livability_map(map_data, "QoL_Index_Level", "Livability index")

# Display or arrange them
(p1 | p2) / (p3 | p4) / (p5| p6)

#Explort as pdf
setwd("C:\\Users\\Results")
ggsave("Phnom_Penh_Livability_Maps.pdf", width = 10, height = 10)


# Combine maps into a single plot object
livability_maps <- (p1 | p2) / (p3 | p4) / (p5 | p6)
# Export as PNG
ggsave("Phnom_Penh_Livability_Maps.png", 
       plot = livability_maps,
       width = 8, height = 10, dpi = 600, units = "in")



############Map all cities

# Step 1: Load world country boundaries
world <- ne_countries(scale = "medium", returnclass = "sf")

# Step 2: Filter Southeast Asian countries of interest
highlight_countries <- c("Cambodia", "Vietnam", "Indonesia", "Philippines")
sea_countries <- world %>% filter(admin %in% highlight_countries)

# Step 3: Define city coordinates and labels
cities <- data.frame(
  City = c("Phnom Penh", "Ho Chi Minh City", "Jakarta", "Manila"),
  Longitude = c(104.918, 106.6297, 106.8456, 120.9842),
  Latitude = c(11.5449, 10.8231, -6.2088, 14.5995)
)

# Step 4: Plot map
ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray80") +
  geom_sf(data = sea_countries, fill = "lightblue", color = "black", size = 0.5) +
  geom_point(data = cities, aes(x = Longitude, y = Latitude), color = "red", size = 2.5) +
  geom_text_repel(data = cities, aes(x = Longitude, y = Latitude, label = City), 
                  size = 4, fontface = "bold", color = "black", segment.color = "gray30") +
  coord_sf(xlim = c(95, 125), ylim = c(-12, 25)) +
  theme_minimal() +
  labs(title = "",
       caption = "")

setwd("C:\\Users\\Results")
ggsave("SE_Asia_Cities_Location_Map.png", width = 10, height = 8, dpi = 600)