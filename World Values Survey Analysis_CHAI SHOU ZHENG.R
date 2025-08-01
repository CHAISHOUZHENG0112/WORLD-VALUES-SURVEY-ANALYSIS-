# --------------------------------------------------
# R script: World Values Survey Analysis
# Project: World Values Survey Analysis
#
# Date: 16/4/2025
# Time: 7:20pm
# Author: CHAI SHOU ZHENG
# Assigned Country: JOR (Jordan)
# --------------------------------------------------

# Create individual data set
rm(list = ls())
set.seed(34035958) 
VCData = read.csv("WVSExtract.csv")
VC = VCData[sample(1:nrow(VCData),50000, replace=FALSE),]
VC = VC[,c(1:6, sort(sample(7:46,17, replace = FALSE)), 47:53,
           sort(sample(54:69,10, replace = FALSE)))]

# Save the extracted data set as a new CSV file
write.csv(VC, "WVSExtract_34035958.csv", row.names = FALSE)

# Load libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(knitr)
library(tidyr)
library(broom)
library(janitor)
library(corrplot)
options(contrasts = c("contr.treatment", "contr.poly"))
setwd("FIT3152/Week1/Assignment1")
getwd()

# Read the extracted data set
wvs_data <- read.csv("WVSExtract_34035958.csv", stringsAsFactors = FALSE)

# --------------------------------------------------
# Q1.Descriptive analysis
# --------------------------------------------------
# 1(a).

# Dimensions
dim(wvs_data)

# Data Types
str(wvs_data)

# Missing Values
sum(is.na(wvs_data))

# Count of negative survey responses (missing value) per column
coded_missing <- function(x) {
  sum(x %in% c(-1, -2, -3, -4, -5))
}
missing_counts <- sapply(wvs_data, coded_missing)
missing_counts

# Convert to data frame for plotting
missing_df <- data.frame(
  Variable = names(missing_counts),
  Missing = as.numeric(missing_counts)
)

# --------------------------------------------------
# Plot the missing values graph
# --------------------------------------------------
ggplot(missing_df, aes(x = reorder(Variable, -Missing), y = Missing)) +
  geom_bar(stat = "identity", fill = "#9ecae1") +
  geom_text(aes(label = Missing), vjust = -0.5, size = 3.5) +  # Add numbers above bars
  labs(title = "Missing Values of Each Column",
       x = "Variable",
       y = "Count of Missing Values") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# --------------------------------------------------
# Data Pre-Processing and Data Cleaning
# --------------------------------------------------
# Replace coded missing values in all numeric columns only
wvs_data_clean <- wvs_data
# Loop through columns and replace coded values with NA
wvs_data_clean[] <- lapply(wvs_data_clean, function(col) {
  if (is.numeric(col)) {
    col[col %in% c(-1, -2, -3, -4, -5)] <- NA
  }
  return(col)
})

# Summary before cleaning
cat("Summary of CUnions BEFORE cleaning:\n")
summary(wvs_data$CUnions)

# Summary after cleaning
cat("\nSummary of CUnions AFTER cleaning:\n")
summary(wvs_data_clean$CUnions)

# --------------------------------------------------
# Distribution of Numerical Attributes (Age)
# --------------------------------------------------
# Plot a graph of Distribution of Numerical Attributes (Age)
ggplot(wvs_data_clean, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "lightblue1", color = "black") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +  # custom x-axis ticks
  labs(title = "Distribution of Age",
       x = "Age",
       y = "Count") +
  theme_minimal()

# --------------------------------------------------
# Description of 'coded_country'
# --------------------------------------------------

# Identify rows where 'Country' is NA or an empty string
missing_country <- wvs_data[is.na(wvs_data$Country) | wvs_data$Country == "", ]
nrow(missing_country)

# Convert Country column to a factor
wvs_data$Country <- as.factor(wvs_data$Country)

# Compute the frequency table for coded_country in descending order
country_counts <- sort(table(wvs_data$Country), decreasing = TRUE)
country_counts

# --------------------------------------------------
# Draw the plot of Distribution of Age
# --------------------------------------------------
# Calculate country counts
country_counts <- wvs_data %>%
  group_by(Country) %>%
  summarise(count = n())

# Determine min, max, average, and the count for JOR
max_count <- max(country_counts$count)

min_count <- min(country_counts$count)
avg_count <- mean(country_counts$count)
jor_count <- country_counts$count[country_counts$Country == "JOR"]

# Create the bar chart
p_country <- ggplot(country_counts, aes(x = reorder(Country, -count), y = count)) +
  geom_col(fill = "darkslategray1", color = "black") +
  
  # Add a horizontal dashed line for the average count
  geom_hline(yintercept = avg_count, linetype = "dashed", color = "red") +
  
  # Annotate the average count on the plot
  annotate("text",
           x = nrow(country_counts) / 2,  # roughly center the text horizontally
           y = avg_count,
           label = paste0("Avg: ", round(avg_count, 1)),
           vjust = -0.5,
           color = "red") +
  
  # Label the bar with the maximum count
  geom_text(
    data = filter(country_counts, count == max_count),
    aes(label = paste0(count)),
    vjust = -0.5,
    color = "blue",
    size = 3
  ) +
  
  # Label the bar with the minimum count
  geom_text(
    data = filter(country_counts, count == min_count),
    aes(label = paste0(count)),
    vjust = -0.5,
    color = "purple",
    size = 3
  ) +
  
  # Annotate the count for the focus country JOR
  geom_text(
    data = filter(country_counts, Country == "JOR"),
    aes(label = paste0("JOR: ", count)),
    vjust = -0.5,
    color = "darkgreen",
    size = 3
  ) +
  
  ggtitle("Distribution of Countries") +
  xlab("Country") +
  ylab("Number of Respondents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))  # Center the title

# Print the plot
print(p_country)

# --------------------------------------------------
# multivariate scatter plot (interaction between Age,Edu,HSatLife and Gender (MF))
# --------------------------------------------------

wvs_data$MF <- factor(wvs_data$MF, 
                      levels = c(1, 2), 
                      labels = c("Male", "Female"))

wvs_sample$Edu <- factor(wvs_sample$Edu)

wvs_data$HSatLife <- ifelse(wvs_data$HSatLife %in% c(-1, -2, -3, -4, -5), NA, wvs_data$HSatLife)

# Filter to remove NA values
wvs_clean <- wvs_data %>%
  filter(!is.na(MF), !is.na(Age), !is.na(HSatLife))

# Sample 1000 rows
set.seed(123)
wvs_sample <- wvs_clean[sample(nrow(wvs_clean), 1000), ]

ggplot(wvs_sample, aes(x = Age, y = HSatLife, color = MF)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, size = 1.2) +
  labs(title = "Life Satisfaction by Age and Gender")


ggplot(wvs_sample, aes(x = Age, y = HSatLife)) +
  geom_point(alpha = 0.5, size = 2, color = "steelblue") +
  geom_smooth(method = "loess", se = TRUE, color = "darkblue", size = 1) +
  facet_wrap(~ MF) +
  labs(
    title = "Life Satisfaction by Age (Faceted by Gender)",
    x = "Age",
    y = "Life Satisfaction (1–10)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30"),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )




Q2(a).
# --------------------------------------------------
# Q2.Focus country vs all other countries as a group.
# --------------------------------------------------
# 2(a).

# Read the extracted data set
wvs_data <- read.csv("WVSExtract_34035958.csv", stringsAsFactors = FALSE)
# Create a new categorical column 'Focus' to distinguish the focus country (JOR)  
# Assign "JOR" to rows where the country is Jordan, and "Others_Country" otherwise  
wvs_data <- wvs_data %>%
  mutate(Focus = recode(Country, 
                        "JOR" = "JOR", 
                        .default = "Others_Country") %>%
           factor(levels = c("JOR", "Others_Country")))

# --------------------------------------------------
# Means Comparison of Participant Responses using JOR and Others_Country using graph
# --------------------------------------------------
predictor_vars <- c(
  "TPeople", "TFamily", "TNeighbourhood", "TKnow", "TMeet",
  "VFamily", "VFriends",
  "HSatLife", "HSatFin", "HFood", "HMedicine",
  "EEquality", "ECompetition", "SSecure", "SJob",
  "PDemImp", "PDemCurrent", "PSatisfied",
  "STFaith", "STRight", "STWorld","PMobile",
  "PEmail", "PFriends","Age", "Edu"
)

# Summarize means by group
basic_summary <- wvs_data %>%
  select(Focus, all_of(predictor_vars)) %>%
  group_by(Focus) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

summary_long <- basic_summary %>%
  pivot_longer(-Focus, names_to = "Variable", values_to = "Mean")

ggplot(summary_long, aes(x = reorder(Variable, -Mean), y = Mean, fill = Focus)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = round(Mean, 2)),
            position = position_dodge(width = 0.9),
            vjust = 0.3, hjust = -0.1, size = 3) +
  labs(
    title = "Mean Comparison of Participant Responses(Jordan vs Other Countries)",
    x = "Variable",
    y = "Mean Score"
  ) +
  scale_fill_manual(values = c("JOR" = "#4E5B6E", "Others_Country" = "#E6B800")) +
  coord_flip(clip = "off") +
  theme_minimal() +
  theme(plot.margin = margin(5.5, 30, 5.5, 5.5))


# --------------------------------------------------
# Means Comparison of Participant Responses using JOR and Others_Country using boxplot
# --------------------------------------------------
# Clean negative values in predictors
wvs_data[predictor_vars] <- lapply(wvs_data[predictor_vars], function(x) {
  x[x < 0] <- NA
  return(x)
})

wvs_long <- wvs_data %>%
  select(Focus, all_of(predictor_vars)) %>%
  pivot_longer(cols = -Focus, names_to = "Attribute", values_to = "Value")

ggplot(wvs_long, aes(x = Focus, y = Value, fill = Focus)) +
  geom_boxplot() +
  facet_wrap(~Attribute, scales = "free_y") +
  scale_fill_manual(values = c("JOR" = "#A7BED3", "Others_Country" = "#FFDAC1")) +
  theme_minimal() +
  labs(
    title = "Comparison of Participant Responses(Jordan vs Other Countries)",
    x = "Country Group",
    y = "Value"
  )

# --------------------------------------------------
# Boxplot for TMeet, TPeople, HFood, HMedicine, and PDemImp
# --------------------------------------------------

# Define the selected variables for plotting
selected_vars <- c("TMeet", "TPeople", "HFood", "HMedicine", "PDemImp")

# Clean negative values for selected variables
wvs_data[selected_vars] <- lapply(wvs_data[selected_vars], function(x) {
  x[x < 0] <- NA
  return(x)
})

# Convert to long format for ggplot
wvs_long_selected <- wvs_data %>%
  select(Focus, all_of(selected_vars)) %>%
  pivot_longer(cols = -Focus, names_to = "Attribute", values_to = "Value")

# Create boxplot
ggplot(wvs_long_selected, aes(x = Focus, y = Value, fill = Focus)) +
  geom_boxplot() +
  facet_wrap(~Attribute, scales = "free_y") +
  scale_fill_manual(values = c("JOR" = "#A7BED3", "Others_Country" = "#FFDAC1")) +
  theme_minimal() +
  labs(
    title = "Boxplot of TMeet, TPeople, HFood, HMedicine, PDemImp (Jordan vs Other Countries)",
    x = "Country Group",
    y = "Response Value"
  )

# --------------------------------------------------
# Boxplot for PMobile and SJob
# --------------------------------------------------

# Define the selected variables for plotting
selected_vars <- c("PMobile","SJob")




# Clean negative values for selected variables
wvs_data[selected_vars] <- lapply(wvs_data[selected_vars], function(x) {
  x[x < 0] <- NA
  return(x)
})

# Convert to long format for ggplot
wvs_long_selected <- wvs_data %>%
  select(Focus, all_of(selected_vars)) %>%
  pivot_longer(cols = -Focus, names_to = "Attribute", values_to = "Value")

# Create boxplot
ggplot(wvs_long_selected, aes(x = Focus, y = Value, fill = Focus)) +
  geom_boxplot() +
  facet_wrap(~Attribute, scales = "free_y") +
  scale_fill_manual(values = c("JOR" = "#A7BED3", "Others_Country" = "#FFDAC1")) +
  theme_minimal() +
  labs(
    title = "Boxplot of PMobile, SJob (Jordan vs Other Countries)",
    x = "Country Group",
    y = "Response Value"
  )

# --------------------------------------------------
# Boxplot for TFamily and VFamily
# --------------------------------------------------

# Define the selected variables for plotting
selected_vars <- c("TFamily","VFamily")

# Clean negative values for selected variables
wvs_data[selected_vars] <- lapply(wvs_data[selected_vars], function(x) {
  x[x < 0] <- NA
  return(x)
})

# Convert to long format for ggplot
wvs_long_selected <- wvs_data %>%
  select(Focus, all_of(selected_vars)) %>%
  pivot_longer(cols = -Focus, names_to = "Attribute", values_to = "Value")

# Create boxplot
ggplot(wvs_long_selected, aes(x = Focus, y = Value, fill = Focus)) +
  geom_boxplot() +
  facet_wrap(~Attribute, scales = "free_y") +
  scale_fill_manual(values = c("JOR" = "#A7BED3", "Others_Country" = "#FFDAC1")) +
  theme_minimal() +
  labs(
    title = "Boxplot of TFamily, VFamily (Jordan vs Other Countries)",
    x = "Country Group",
    y = "Response Value"
  )

# --------------------------------------------------
# Comparison of Participant Responses using T-test and present as a graph
# --------------------------------------------------
t_test_results <- lapply(predictor_vars, function(var) {
  formula <- as.formula(paste(var, "~ Focus"))
  test <- t.test(formula, data = wvs_data)
  
  data.frame(
    Variable = var,
    Mean_JOR = mean(wvs_data[[var]][wvs_data$Focus == "JOR"], na.rm = TRUE),
    Mean_Others = mean(wvs_data[[var]][wvs_data$Focus == "Others_Country"], na.rm = TRUE),
    p_value = test$p.value,
    Significant = ifelse(test$p.value < 0.05, "Yes", "No")
  )
})

# Combine into one data frame
t_test_summary <- do.call(rbind, t_test_results)

ggplot(t_test_summary, aes(x = reorder(Variable, -Mean_JOR))) +
  geom_bar(aes(y = Mean_JOR, fill = "Jordan"), stat = "identity", alpha = 0.7, width = 0.4, position = position_nudge(x = -0.2)) +
  geom_bar(aes(y = Mean_Others, fill = "Others Countries"), stat = "identity", alpha = 0.7, width = 0.4, position = position_nudge(x = 0.2)) +
  geom_text(aes(y = pmax(Mean_JOR, Mean_Others) + 0.1, label = ifelse(Significant == "Yes", "*", "")), size = 5) +
  scale_fill_manual(values = c("Jordan" = "#4E79A7", "Others Countries" = "#F28E2B")) +
  coord_flip() +
  labs(
    title = "Comparison of Participant Responses using T-Test (Jordan vs Other Countries)",
    x = "Variable", y = "Mean Score",
    fill = "Group"
  ) +
  theme_minimal()

print(t_test_summary)


# --------------------------------------------------
# Convert categorical nominal to factor
# --------------------------------------------------

# Convert numeric categorical variables into properly labeled factors 
# to allow R to treat them as categorical and create dummy variables in regression
wvs_data <- wvs_data %>%
  mutate(
    PIAB = factor(PIAB, levels = 1:4,
                  labels = c("Economic Growth", "Defense", "Jobs and Communities", "Beauty")),
    
    Employment = factor(Employment, levels = 1:8,
                        labels = c("Full time", "Part time", "Self employed", "Retired",
                                   "Spouse/not employed", "Student", "Unemployed", "Other")),
    
    MF = factor(MF, levels = 1:2, labels = c("Male", "Female"))
  )

# ---------------------------
# Comparison of Categorical Nominal Variables Between Jordan and Other Countries using Mode
# ---------------------------
# Function to get mode
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

nominal_vars <- c("PIAB", "Employment", "MF")

# Recompute the mode for each nominal variable by group (after labeling)
mode_table <- wvs_data %>%
  group_by(Focus) %>%
  summarise(across(all_of(nominal_vars), get_mode))

mode_table

# --------------------------------------------------
# Plot the graph of distribution of categorical nominal variables
# --------------------------------------------------
nominal_long <- wvs_data %>%
  select(Focus, all_of(nominal_vars)) %>%
  pivot_longer(cols = -Focus, names_to = "Variable", values_to = "Response") %>%
  filter(!is.na(Response)) %>%
  group_by(Focus, Variable, Response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Variable, Focus) %>%
  mutate(Percent = Count / sum(Count) * 100)

# Plot a faceted bar chart with side-by-side percentages
ggplot(nominal_long, aes(x = Response, y = Percent, fill = Focus)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free_x") +
  scale_fill_manual(values = c("JOR" = "#4E5B6E", "Others_Country" = "#E6B800")) +
  labs(
    title = "Distribution of Categorical Nominal Variables (Jordan vs Other Countries)",
    x = "Response Category",
    y = "Percentage of Respondents"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Q2(b).
# --------------------------------------------------
# Q2B. Linear and Stepwise Regression for Jordan (JOR)
# --------------------------------------------------
wvs_data <- read.csv("WVSExtract_34035958.csv", stringsAsFactors = FALSE)

wvs_data <- wvs_data %>%
  mutate(Focus = recode(Country, 
                        "JOR" = "JOR", 
                        .default = "Others_Country") %>%
           factor(levels = c("JOR", "Others_Country")))

# Filter dataset for the focus country: Jordan (JOR)
wvs_jordan <- wvs_data %>% filter(Focus == "JOR")

# Convert categorical variables with numeric codes into factors with labels
wvs_jordan <- wvs_jordan %>%
  mutate(
    PIAB = factor(PIAB, levels = 1:4,
                  labels = c("Economic Growth", "Defense", "Jobs and Communities", "Beauty")),
    
    Employment = factor(Employment, levels = 1:8,
                        labels = c("Full time", "Part time", "Self employed", "Retired",
                                   "Spouse/not employed", "Student", "Unemployed", "Other")),
    
    MF = factor(MF, levels = 1:2, labels = c("Male", "Female"))
  )

# Remove the redundant Country column
wvs_jor <- wvs_jordan %>% select(-Country)

# --------------------------------------------------
# 2. Define response variables and predictors
# --------------------------------------------------
# List of all confidence variables (dependent variables)
target_vars <- c("CUniversities", "CArmedForces", "CPress", "CTelevision", 
                 "CUnions", "CCourts", "CPParties", "CCivilService", 
                 "CMajCompanies", "CEnvOrg")

# All independent variables (predictors)
predictors <- paste(c("TPeople", "TFamily", "TNeighbourhood", "TKnow", "TMeet",
                      "VFamily", "VFriends",
                      "HSatLife", "HSatFin", "HFood", "HMedicine",
                      "EEquality", "ECompetition", "SSecure", "SJob",
                      "PIAB", "STFaith", "STRight", "STWorld",
                      "PMobile", "PEmail", "PFriends",
                      "PDemImp", "PDemCurrent", "PSatisfied",
                      "MF", "Age", "Edu", "Employment"), collapse = " + ")

# --------------------------------------------------
# 3. Fit linear and stepwise regression models
# --------------------------------------------------
# Initialize lists to store models
lm_models <- list()
stepwise_models <- list()

# Loop through each dependent variable and fit models
for (var in target_vars) {
  # Create formula dynamically
  f <- as.formula(paste(var, "~", predictors))
  
  # Fit full linear model
  model <- lm(f, data = na.omit(wvs_jor))
  lm_models[[var]] <- model
  
  # Fit stepwise model
  step_model <- step(model, direction = "both", trace = 0)
  stepwise_models[[var]] <- step_model
}

# --------------------------------------------------
# 4. Extract R-squared values for model comparison
# --------------------------------------------------
# Function to get R-squared
get_r_squared <- function(model) summary(model)$r.squared

# Function to extract significant predictors
get_significant_predictors <- function(model) {
  sig <- summary(model)$coefficients
  sig_vars <- rownames(sig)[sig[, 4] < 0.05]
  sig_vars[sig_vars != "(Intercept)"]
}

# R-squared tables
r_squared_df <- data.frame(
  CVariable = names(lm_models),
  R_squared = sapply(lm_models, get_r_squared)
)

# Data frame of R-squared values for stepwise models
stepwise_r_squared_df <- data.frame(
  CVariable = names(stepwise_models),
  R_squared = sapply(stepwise_models, get_r_squared)
)

# Predictor lists
lm_predictors_df <- data.frame(
  CVariable = names(lm_models),
  Best_Predictors = sapply(lm_models, function(m) paste(get_significant_predictors(m), collapse = ", "))
)

# Data frame of R-squared values for stepwise models
stepwise_predictors_df <- data.frame(
  CVariable = names(stepwise_models),
  Best_Predictors = sapply(stepwise_models, function(m) paste(get_significant_predictors(m), collapse = ", "))
)
# --------------------------------------------------
# 6. Count most common predictors across all models
# --------------------------------------------------
lm_top_predictors <- sort(table(unlist(sapply(lm_models, get_significant_predictors))), decreasing = TRUE)
step_top_predictors <- sort(table(unlist(sapply(stepwise_models, get_significant_predictors))), decreasing = TRUE)


# --------------------------------------------------
# 7. Display Results
# --------------------------------------------------
cat("----- Linear Regression R-squared Values (JOR) -----\n")
print(r_squared_df)

cat("----- Stepwise Regression R-squared Values (JOR) -----\n")
print(stepwise_r_squared_df)

cat("----- Significant Predictors from Linear Models (JOR) -----\n")
print(lm_predictors_df)

cat("----- Significant Predictors from Stepwise Model (JOR)s -----\n")
print(stepwise_predictors_df)

cat("----- Top 5 Predictors from Linear Models (JOR) -----\n")
print(head(lm_top_predictors, 5))

cat("----- Bottom 3 Predictors from Linear Models (JOR) -----\n")
print(tail(lm_top_predictors, 3))

cat("----- Top 5 Predictors from Stepwise Models (JOR) -----\n")
print(head(step_top_predictors, 5))

cat("----- Bottom 3 Predictors from Stepwise Models (JOR) -----\n")
print(tail(step_top_predictors, 3))

# -------------------------------
# correlation
# -------------------------------
# 1. Select only numeric columns
numeric_data <- wvs_jordan[, sapply(wvs_jordan, is.numeric)]

# 2. Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# 3. Extract correlations between confidence variables (C*) and predictors
cor_confidence <- cor_matrix[grepl("^C", rownames(cor_matrix)), !grepl("^C", colnames(cor_matrix))]

# 4. Plot the heatmap with values included
corrplot(cor_confidence,
         method = "color",
         col = colorRampPalette(c("red", "white", "blue"))(200),
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", 
         number.cex = 0.6,        # Text size of the numbers
         title = "Correlation between Predictors and Confidence Variables (JOR)",
         mar = c(0, 0, 1, 0),
         cl.pos = "r")


Q2(c).
# -------------------------------
# Q2C. Linear and Stepwise Regression for Others_Country
# -------------------------------

wvs_data <- read.csv("WVSExtract_34035958.csv", stringsAsFactors = FALSE)

wvs_data <- wvs_data %>%
  mutate(Focus = recode(Country, 
                        "JOR" = "JOR", 
                        .default = "Others_Country") %>%
           factor(levels = c("JOR", "Others_Country")))


# Step 1: Filter dataset for other countries
wvs_others <- wvs_data %>% filter(Focus == "Others_Country")

# Convert numeric categorical variables into properly labeled factors
wvs_others <- wvs_others %>%
  mutate(
    PIAB = factor(PIAB, levels = 1:4,
                  labels = c("Economic Growth", "Defense", "Jobs and Communities", "Beauty")),
    
    Employment = factor(Employment, levels = 1:8,
                        labels = c("Full time", "Part time", "Self employed", "Retired",
                                   "Spouse/not employed", "Student", "Unemployed", "Other")),
    
    
    MF = factor(MF, levels = 1:2, labels = c("Male", "Female"))
  )

# Drop the 'Country' column since we group all the countries into Others_Country
wvs_other <- wvs_others %>% select(-Country)
str(wvs_others)

# --------------------------------------------------
# 2. Define response variables and predictors
# --------------------------------------------------
target_vars <- c("CUniversities", "CArmedForces", "CPress", "CTelevision",
                 "CUnions", "CCourts", "CPParties", "CCivilService",
                 "CMajCompanies", "CEnvOrg")

# Shared predictors
predictors <- paste(c("TPeople", "TFamily", "TNeighbourhood", "TKnow", "TMeet",
                      "VFamily", "VFriends",
                      "HSatLife", "HSatFin", "HFood", "HMedicine",
                      "EEquality", "ECompetition", "SSecure", "SJob",
                      "PIAB", "STFaith", "STRight", "STWorld",
                      "PMobile", "PEmail", "PFriends",
                      "PDemImp", "PDemCurrent", "PSatisfied",
                      "MF", "Age", "Edu", "Employment"), collapse = " + ")

# --------------------------------------------------
# 3. Fit linear and stepwise regression models
# --------------------------------------------------
# Initialize model storage
lm_models_others <- list()
stepwise_models_others <- list()

# Build full and stepwise models
for (var in target_vars) {
  formula_str <- as.formula(paste(var, "~", predictors))
  
  model <- lm(formula_str, data = na.omit(wvs_others))
  step_model <- step(model, direction = "both", trace = 0)
  
  lm_models_others[[var]] <- model
  stepwise_models_others[[var]] <- step_model
}

# --------------------------------------------------
# 4. Extract R-squared values for model comparison
# --------------------------------------------------

# R-squared
get_r_squared <- function(model) summary(model)$r.squared

r_squared_df <- data.frame(
  CVariable = names(lm_models_others),
  R_squared = sapply(lm_models_others, get_r_squared)
)
cat("----- Linear Regression R-squared Values (Others Country) -----\n")
print(r_squared_df)

stepwise_r_squared_df <- data.frame(
  CVariable = names(stepwise_models_others),
  R_squared = sapply(stepwise_models_others, get_r_squared)
)
cat("----- Stepwise Regression R-squared Values (Others Country) -----\n")
print(stepwise_r_squared_df)

# --------------------------------------------------
# 5. Extract significant predictors (p < 0.05)
# --------------------------------------------------
get_significant_predictors <- function(model) {
  coef_summary <- summary(model)$coefficients
  sig_vars <- rownames(coef_summary)[coef_summary[, 4] < 0.05]
  sig_vars[sig_vars != "(Intercept)"]
}

lm_predictors_df <- data.frame(
  CVariable = names(lm_models_others),
  Best_Predictors = sapply(lm_models_others, function(m) paste(get_significant_predictors(m), collapse = ", "))
)

stepwise_predictors_df <- data.frame(
  CVariable = names(stepwise_models_others),
  Best_Predictors = sapply(stepwise_models_others, function(m) paste(get_significant_predictors(m), collapse = ", "))
)


# --------------------------------------------------
# 6. Count frequency of predictors across all models
# --------------------------------------------------

lm_top_predictors <- sort(table(unlist(sapply(lm_models_others, get_significant_predictors))), decreasing = TRUE)
cat("----- Top 5 Predictors from Linear Models (Others Country) -----\n")
print(head(lm_top_predictors, 5))

cat("----- Bottom 3 Predictors from Linear Models (Others Country) -----\n")
print(tail(lm_top_predictors, 3))

step_top_predictors <- sort(table(unlist(sapply(stepwise_models_others, get_significant_predictors))), decreasing = TRUE)
cat("----- Top 5 Predictors from Stepwise Models (Others Country) -----\n")
print(head(step_top_predictors, 5))

cat("----- Bottom 3 Predictors from Stepwise Models (Others Country) -----\n")
print(tail(step_top_predictors, 3))

# -------------------------------
# correlation
# -------------------------------
# 1. Select only numeric columns
numeric_data <- wvs_other[, sapply(wvs_other, is.numeric)]

# 2. Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# 3. Extract correlations between confidence variables (C*) and predictors
cor_confidence <- cor_matrix[grepl("^C", rownames(cor_matrix)), !grepl("^C", colnames(cor_matrix))]

# 4. Plot the heatmap with values included
corrplot(cor_confidence,
         method = "color",
         col = colorRampPalette(c("red", "white", "blue"))(200),
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", 
         number.cex = 0.6,        # Text size of the numbers
         title = "Correlation between Predictors and Confidence Variables (Other Country)",
         mar = c(0, 0, 1, 0),
         cl.pos = "r")




Q3(a).

# --------------------------------------------------
# Q3.Focus country vs cluster of similar countries. 
# --------------------------------------------------
# Q3(a).

# Read the dataset
external_country_data <- read.csv("country_profile_variables.csv")

# List of countries included in the WVS Country (in our assignment)
assignment_countries <- c("Andorra", "Argentina", "Armenia", "Australia", "Bangladesh", "Bolivia", "Brazil",
                          "Canada", "Chile", "China", "Colombia", "Cyprus", "Czech Republic", "Germany",
                          "Ecuador", "Egypt", "Ethiopia", "Great Britain", "Greece", "Guatemala", "Hong Kong",
                          "Indonesia", "India", "Iran", "Iraq", "Jordan", "Japan", "Kazakhstan", "Kenya",
                          "Kyrgyzstan", "South Korea", "Lebanon", "Libya", "Macedonia", "Morocco", "Moldova",
                          "Mexico", "Myanmar", "Mongolia", "Malaysia", "Nigeria", "Nicaragua", "Netherlands",
                          "New Zealand", "Pakistan", "Peru", "Philippines", "Puerto Rico", "Romania", "Russia",
                          "Singapore", "Serbia", "Slovakia", "Thailand", "Tajikistan", "Tunisia", "Turkey",
                          "Taiwan", "Ukraine", "Uruguay", "United States", "Uzbekistan", "Venezuela", "Viet Nam", "Zimbabwe")

# Filter the external dataset to keep only the list of countries included in the WVS Country (in our assignment)
filtered_data <- external_country_data %>%
  filter(country %in% assignment_countries)





# Select relevant columns (update these column names as per the dataset)
selected_data <- filtered_data %>%
  select(
    Country = country,
    `Population_thousands_2017` = Population.in.thousands..2017.,
    `GDP_per_capita` = `GDP.per.capita..current.US..`,
    `Fertility_rate` = `Fertility.rate..total..live.births.per.woman.`,
    `Health_exp_pct_GDP` = `Health..Total.expenditure....of.GDP.`,
    `Unemployment_rate` = `Unemployment....of.labour.force.`,
    `Internet_users_per_100` = `Individuals.using.the.Internet..per.100.inhabitants.`,
    `Edu_primary_gross_enrol` = Education..Primary.gross.enrol..ratio..f.m.per.100.pop..
  )

# --------------------------------------------------
# data cleaning
# --------------------------------------------------

# Convert string like "102.3/101.7" to numeric average
selected_data$Edu_primary_gross_enrol <- sapply(strsplit(as.character(selected_data$Edu_primary_gross_enrol), "/"), function(x) {
  mean(as.numeric(x))
})

# Find and print rows with at least one column containing -99
rows_with_99 <- selected_data[apply(selected_data, 1, function(row) any(row == -99, na.rm = TRUE)), ]
print(rows_with_99)

# Convert all -99 values in numeric columns to NA
selected_data[selected_data == -99] <- NA

# Drop rows with any NA values
selected_data_clean <- na.omit(selected_data)

# Check for remaining NA values in each column
na_counts <- sapply(selected_data_clean[, -1], function(col) sum(is.na(col)))
print(na_counts)

# Replace "..." strings with NA
selected_data_clean[selected_data_clean == "..."] <- NA

# Remove rows with any NA values
selected_data_clean <- na.omit(selected_data_clean)

# --------------------------------------------------
# data normalization
# --------------------------------------------------
# Exclude the "Country" column for normalization
numeric_data <- selected_data_clean[, -1]
# Convert all columns to numeric
numeric_data <- data.frame(lapply(numeric_data, function(x) as.numeric(as.character(x))))

# Normalize using scale() — each column becomes mean=0, sd=1
normalized_data <- as.data.frame(scale(numeric_data))

# Add country names back for reference
normalized_data$Country <- selected_data_clean$Country

# View the normalized data
print(normalized_data)

# --------------------------------------------------
# hirerchical clustering
# --------------------------------------------------

# Set seed for reproducibility
set.seed(34035958)

# Compute Euclidean distance matrix
distance_matrix <- dist(normalized_data, method = "euclidean")

# Perform hierarchical clustering using Ward's method
hc <- hclust(distance_matrix, method = "ward.D2")

# Plot the dendrogram
plot(hc, labels = selected_data_clean$Country, main = "Dendrogram of Countries", xlab = "", sub = "", cex = 0.7)

# Cut the tree to get 5 clusters
clusters <- cutree(hc, k = 5)

# Add cluster rectangles
rect.hclust(hc, k = 5, border = "red") 

# Add cluster labels to the cleaned data
selected_data_clean$Cluster <- clusters

# Sort the data by cluster number
selected_data_sorted <- selected_data_clean[order(selected_data_clean$Cluster), ]

# View countries by cluster
print(selected_data_sorted[, c("Country", "Cluster")])



Q3(b).
# --------------------------------------------------
# Q3B. Linear and Stepwise Regression for Cluster 4 (Similar Countries to JOR)
# --------------------------------------------------
wvs_data <- read.csv("WVSExtract_34035958.csv", stringsAsFactors = FALSE)

# Filter data for countries in cluster 4 with JOR
cluster4_countries <- c("EGY", "BOL", "KEN", "NGA", "PAK", "ETH", "ZWE")
cluster4_data <- wvs_data %>% filter(Country %in% cluster4_countries)

# Convert numeric categorical variables into properly labeled factors
# This allows R to treat them as categorical and create dummy variables in regression
cluster4_data <- cluster4_data %>%
  mutate(
    PIAB = factor(PIAB, levels = 1:4,
                  labels = c("Economic Growth", "Defense", "Jobs and Communities", "Beauty")),
    Employment = factor(Employment, levels = 1:8,
                        labels = c("Full time", "Part time", "Self employed", "Retired",
                                   "Spouse/not employed", "Student", "Unemployed", "Other")),
    MF = factor(MF, levels = 1:2, labels = c("Male", "Female"))
  )
cluster4_data_clean <- cluster4_data %>% select(-Country)

# --------------------------------------------------
# 2. Define response variables and predictors
# --------------------------------------------------
# List of the target variables
outcomes <- c("CUniversities", "CArmedForces", "CPress", "CTelevision", "CUnions",
              "CCourts", "CPParties", "CCivilService", "CMajCompanies", "CEnvOrg")

# Independent variables
predictors <- c("TPeople", "TFamily", "TNeighbourhood", "TKnow", "TMeet",
                "VFamily", "VFriends",
                "HSatLife", "HSatFin", "HFood", "HMedicine",
                "EEquality", "ECompetition", "SSecure", "SJob",
                "PIAB", "STFaith", "STRight", "STWorld",
                "PMobile", "PEmail", "PFriends",
                "PDemImp", "PDemCurrent", "PSatisfied",
                "MF", "Age", "Edu", "Employment")

# Full lm models
lm_models_cluster4 <- list()
for (outcome in outcomes) {
  formula_text <- paste(outcome, "~", paste(predictors, collapse = " + "))
  lm_models_cluster4[[outcome]] <- lm(as.formula(formula_text), data = na.omit(cluster4_data_clean))
}

# Stepwise models
step_models_cluster4 <- list()
for (outcome in outcomes) {
  step_models_cluster4[[outcome]] <- step(lm_models_cluster4[[outcome]], direction = "both", trace = 0)
}

cat("----- Full Linear Model Summaries (Cluster 4) -----\n")
for (outcome in outcomes) {
  cat("\n---", outcome, "---\n")
  print(summary(lm_models_cluster4[[outcome]]))
}

cat("\n----- Stepwise Linear Model Summaries (Cluster 4) -----\n")
for (outcome in outcomes) {
  cat("\n---", outcome, "---\n")
  print(summary(step_models_cluster4[[outcome]]))
}

# -------------------------------
# Extract and Print R-squared
# -------------------------------

get_r_squared <- function(model) summary(model)$r.squared

r_squared_lm <- sapply(lm_models_cluster4, get_r_squared)
r_squared_step <- sapply(step_models_cluster4, get_r_squared)

r_squared_df_lm <- data.frame(
  CVariable = names(r_squared_lm),
  R_squared = unname(r_squared_lm),
  row.names = NULL
)

r_squared_df_step <- data.frame(
  CVariable = names(r_squared_step),
  R_squared = unname(r_squared_step),
  row.names = NULL
)

cat("----- Linear Regression R-squared Values (Cluster 4) -----\n")
print(r_squared_df_lm)

cat("----- Stepwise Regression R-squared Values (Cluster 4) -----\n")
print(r_squared_df_step)


# -------------------------------
# Extract Significant Predictors
# -------------------------------

get_significant_predictors <- function(model) {
  coef_summary <- summary(model)$coefficients
  sig_vars <- rownames(coef_summary)[coef_summary[, 4] < 0.05]
  sig_vars <- sig_vars[sig_vars != "(Intercept)"]
  paste(sig_vars, collapse = ", ")
}

# Full models
best_lm_predictors_cluster4 <- sapply(lm_models_cluster4, get_significant_predictors)
best_lm_df_cluster4 <- data.frame(
  CVariable = names(best_lm_predictors_cluster4),
  Best_Predictors = unname(best_lm_predictors_cluster4),
  row.names = NULL
)

# Stepwise models
best_step_predictors_cluster4 <- sapply(step_models_cluster4, get_significant_predictors)
best_step_df_cluster4 <- data.frame(
  CVariable = names(best_step_predictors_cluster4),
  Best_Predictors = unname(best_step_predictors_cluster4),
  row.names = NULL
)

print(best_lm_df_cluster4)
print(best_step_df_cluster4)
# -------------------------------
# Top 5 and Bottom 3 Predictors from Full Linear Models
# -------------------------------

all_lm_predictors_cluster4 <- unlist(strsplit(best_lm_df_cluster4$Best_Predictors, ",\\s*"))
top_lm_predictor_counts <- sort(table(all_lm_predictors_cluster4), decreasing = TRUE)

top_5_lm <- head(top_lm_predictor_counts, 5)
bottom_3_lm <- tail(top_lm_predictor_counts, 3)

cat("----- Top 5 Best Predictors from Linear Regression (Cluster 4) -----\n")
print(top_5_lm)
cat("----- Bottom 3 Least Frequent Predictors from Linear Regression (Cluster 4) -----\n")
print(bottom_3_lm)

# -------------------------------
# Top 5 and Bottom 3 Predictors from Stepwise Models
# -------------------------------

all_stepwise_predictors_cluster4 <- unlist(strsplit(best_step_df_cluster4$Best_Predictors, ",\\s*"))
top_stepwise_predictor_counts <- sort(table(all_stepwise_predictors_cluster4), decreasing = TRUE)

top_5_step <- head(top_stepwise_predictor_counts, 5)
bottom_3_step <- tail(top_stepwise_predictor_counts, 3)

cat("----- Top 5 Best Predictors from Stepwise Regression (Cluster 4) -----\n")
print(top_5_step)

cat("----- Bottom 3 Least Frequent Predictors from Stepwise Regression (Cluster 4) -----\n")
print(bottom_3_step)
