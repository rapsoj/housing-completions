#### PREPARE WORKSPACE ####

# Set working directory
setwd("~/Desktop")

# Load in data manipulation libraries
library(plyr)
library(dplyr)
library(tidyr)

# Load in time series library
library(zoo)

# Load in data visualization libraries
library(ggplot2)
library(gridExtra)
library(extrafont)
loadfonts()


#### PREPARE DATA ####

# Read in data for annual housing completions
df_housing <- read.csv("completions_ca.csv", skip = 2) %>%
  # Set column for region
  mutate(GEO = "Canada") %>%
  # Rename first column
  rename(REF_DATE = X) %>%
  # Drop last column
  select(-X.1) %>%
  # Remove last row
  filter(REF_DATE != "Source") %>%
  # Edit columns
  mutate(
    # Format reference date as date
    REF_DATE = as.yearqtr(gsub("/", " ", REF_DATE)),
    # Replace thousands separator
    across(c(-GEO, -REF_DATE), ~as.numeric(gsub(",","", .))),
    # Calculate cumulative sum of annual housing completions
    completions_cumsum_all = cumsum(All),
    # Calculate cumulative sum of annual single housing completions
    completions_cumsum_single = cumsum(Single),
    # Calculate cumulative sum of all annual semi-detached completions
    completions_cumsum_semi = cumsum(Semi.Detached),
    # Calculate cumulative sum of all annual row housing completions
    completions_cumsum_row = cumsum(Row),
    # Calculate cumulative sum of all annual apartment completions
    completions_cumsum_apt = cumsum(Apartment))

# Read in data for annual population estimates
df_pop <- read.csv("population_estimates.csv") %>%
  # Rename column
  rename(pop = VALUE) %>%
  # Group by region
  group_by(GEO) %>%
  # Edit columns
  mutate(
    # Format reference date as date
    REF_DATE = as.yearqtr(as.yearmon(REF_DATE)),
    # Calculate annual change in population
    pop_change = pop - lag(pop),
    # Calculate cumulative sum of annual population change
    pop_change_cumsum = cumsum(ifelse(!is.na(pop_change), pop_change, 0))) %>%
  # Remove dates beyond available housing data
  filter(REF_DATE >= min(df_housing$REF_DATE) - 1/4 &
           REF_DATE <= max(df_housing$REF_DATE)) %>%
  # Ungroup data
  ungroup() %>%
  # Select columns of interest
  select(GEO, REF_DATE, pop, pop_change, pop_change_cumsum)

# Join population and housing completion data
df <- df_pop %>%
  # Merge housing completion data
  merge(df_housing, by = c("GEO", "REF_DATE"), all.x = TRUE) %>%
  # Group by year and geography
  group_by(year = year(as.Date(as.yearmon(REF_DATE))), GEO) %>%
  # Add columns for yearly totals
  summarise_if(is.numeric, mean, na.rm = T) %>%
  # Group by geography
  group_by(GEO) %>%
    # Add columns for ratio of housing completions to population change
    mutate(
      # Convert date to date type
      year = as.Date(as.character(year), format = "%Y"),
      
      # Calculate ratio of all housing completions to population change
      completion_per_1000_all = All / pop_change * 1000,
      # Calculate cumulative ratio of all housing completions to pop change
      completion_per_1000_cumsum_all =
        completions_cumsum_all / pop_change_cumsum * 1000,
      
      # Calculate ratio of single housing completions to population change
      completion_per_1000_single = Single / pop_change * 1000,
      # Calculate cumulative ratio of single housing completions to pop change
      completion_per_1000_cumsum_single =
        completions_cumsum_single / pop_change_cumsum * 1000,
      
      # Calculate ratio of semi-detached completions to population change
      completion_per_1000_semi = Semi.Detached / pop_change * 1000,
      # Calculate cumulative ratio of semi-detached completions to pop change
      completion_per_1000_cumsum_semi =
        completions_cumsum_semi / pop_change_cumsum * 1000,
      
      # Calculate ratio of row housing completions to population change
      completion_per_1000_row = Row / pop_change * 1000,
      # Calculate cumulative ratio of row housing completions to pop change
      completion_per_1000_cumsum_row =
        completions_cumsum_row / pop_change_cumsum * 1000,
      
      # Calculate ratio of apartment completions to population change
      completion_per_1000_apt = Apartment / pop_change * 1000,
      # Calculate cumulative ratio of apartment completions to pop change
      completion_per_1000_cumsum_apt =
        completions_cumsum_apt / pop_change_cumsum * 1000,
      
      # Calculate rolling mean of all housing completions to population change
      completion_per_1000_all_roll = rollmean(
        completion_per_1000_all, k = 3, fill = NA),
      # Replace end values in all housing rolling mean with original value
      completion_per_1000_all_roll = ifelse(
        is.na(completion_per_1000_all_roll),
        completion_per_1000_all, completion_per_1000_all_roll),
      
      # Calculate rolling mean of single housing completions to population change
      completion_per_1000_single_roll = rollmean(
        completion_per_1000_single, k = 3, fill = NA),
      # Replace end values in single housing rolling mean with original value
      completion_per_1000_single_roll = ifelse(
        is.na(completion_per_1000_single_roll),
        completion_per_1000_single, completion_per_1000_single_roll),
      
      # Calculate rolling mean of semi-detatched completions to population change
      completion_per_1000_semi_roll = rollmean(
        completion_per_1000_semi, k = 3, fill = NA),
      # Replace end values in semi-detatched rolling mean with original value
      completion_per_1000_semi_roll = ifelse(
        is.na(completion_per_1000_semi_roll),
        completion_per_1000_semi, completion_per_1000_semi_roll),
      
      # Calculate rolling mean of row housing completions to population change
      completion_per_1000_row_roll = rollmean(
        completion_per_1000_row, k = 3, fill = NA),
      # Replace end values in row housing rolling mean with original value
      completion_per_1000_row_roll = ifelse(
        is.na(completion_per_1000_row_roll),
        completion_per_1000_row, completion_per_1000_row_roll),
      
      # Calculate rolling mean of apartment completions to population change
      completion_per_1000_apt_roll = rollmean(
        completion_per_1000_apt, k = 3, fill = NA),
      # Replace end values in apartment rolling mean with original value
      completion_per_1000_apt_roll = ifelse(
        is.na(completion_per_1000_apt_roll),
        completion_per_1000_apt, completion_per_1000_apt_roll)) %>%
  # Ungroup data
  ungroup() %>%
  # Remove missing values
  filter(!is.na(All))


#### CREATE VISUALIZATIONS ####

# Create graph for ratio of all housing completions to population change
graph1 <- df %>%
  # Pivot data
  pivot_longer(
    !c("GEO", "year", "pop", "pop_change", "pop_change_cumsum"),
    names_to = "housing_metric", values_to = "value") %>%
  # Filter data to Canada only
  filter(GEO == "Canada") %>%
  # Filter metrics for ratio of housing completions to population change
  filter(housing_metric == "completion_per_1000_all_roll") %>%
  # Plot data
  ggplot(aes(x = year, fill = housing_metric)) +
  # Plot ratio of housing completions to population change as a line
  geom_line(aes(y = value), lwd = 2, color = "#59594A") +
  # Add graph title
  labs(title = "Have Canadian housing completions kept up with population growth?",
       subtitle = "Annual housing completions per 1,000 population increase (Canada, 1990-2021)",
       caption = "
       Population: Statistics Canada. (2021). Population estimates, quarterly.
       Completions: Canada Mortgage and Housing Corporation. (2021). New Housing Construction, Completions.") +
  # Remove x-axis title
  xlab("") +
  # Add y-axis title
  ylab("Housing Completions per 1,000 Population Increase") +
  # Change x-axis formatting
  scale_x_date(date_breaks = "4 years" , date_labels = "%y") +
  # Add average line
  geom_hline(yintercept = mean(df$completion_per_1000_all_roll),
             color = "#59594A", linetype = 'dashed') +
  # Add annotation for average line
  annotate("text", x = as.Date("1994-01-01"), y = 480, vjust = -0.5,
           label = "1990-2021 Average", family = "CMU Typewriter Text Bold") +
  # Set graph theme
  theme_minimal() +
  # Customize theme elements
  theme(text = element_text(family = "CMU Typewriter Text Bold"),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 8, hjust = 0, vjust = 0))

# Create graph for ratio of housing completions to population change facets
graph2 <- df %>%
  # Rename columns of interest
  rename(
    `Single-Detatched Houses` = completion_per_1000_single_roll,
    `Semi-Detached Houses` = completion_per_1000_semi_roll,
    `Row Houses` = completion_per_1000_row_roll,
    `Apartments` = completion_per_1000_apt_roll) %>%
  # Pivot data
  pivot_longer(
    !c("GEO", "year", "pop", "pop_change", "pop_change_cumsum"),
    names_to = "housing_metric", values_to = "value") %>%
  # Filter data to Canada only
  filter(GEO == "Canada") %>%
  # Filter metrics for ratio of housing completions to population change
  filter(housing_metric %in%
           c("Single-Detatched Houses", "Semi-Detached Houses",
             "Row Houses", "Apartments")) %>%
  # Convert labels to factors
  mutate(housing_metric = factor(
    housing_metric,levels = c("Single-Detatched Houses", "Semi-Detached Houses",
                              "Row Houses", "Apartments"))) %>%
  # Plot data
  ggplot(aes(x = year, color = housing_metric)) +
  # Plot ratio of housing completions to population change as a line
  geom_line(aes(y = value, color = housing_metric), lwd = 2) +
  # Specify colours
  scale_color_manual(
    values = c("Single-Detatched Houses" = "#BE6E46",
               "Semi-Detached Houses" = "#CDE7B0",
               "Row Houses" = "#A3BFA8",
               "Apartments" = "#7286A0")) +
  # Remove graph title
  ggtitle("") +
  # Remove x-axis title
  xlab("") +
  # Remove y-axis title
  ylab("") +
  # Change x-axis formatting
  scale_x_date(date_breaks = "4 years" , date_labels = "%y") +
  # Set graph theme
  theme_minimal() +
  # Customize theme elements
  theme(text = element_text(family = "CMU Typewriter Text Bold"),
        panel.grid.minor = element_blank(), legend.position = c(.2,.85),
        legend.title = element_blank())

# Combine two graphs
png("housing_completions.png", units="in", width = 12, height = 5, res = 1000)
grid.arrange(graph1, graph2, ncol = 2)
dev.off()






######









# Visualize cumulative completions vs. cumulative population growth
df %>% 
  # Pivot data
  pivot_longer(
    !c("GEO", "REF_DATE", "pop", "pop_change", "pop_change_cumsum"),
    names_to = "housing_metric", values_to = "value") %>%
  # Filter data to Canada only
  filter(GEO == "Canada") %>%
  # Filter metrics for ratio of housing completions to population change
  filter(housing_metric %in%
           c("completion_per_1000_cumsum_single",
             "completion_per_1000_cumsum_semi",
             "completion_per_1000_cumsum_row",
             "completion_per_1000_cumsum_apt")) %>%
  # Plot data
  ggplot(aes(x = REF_DATE, fill = housing_metric)) +
  # Set graph theme
  theme_minimal() +
  # Plot ratio of housing completions to population change as a line
  geom_area(aes(y = value)) +
  # Add graph title
  ggtitle("Cumulative Housing Completions per 1,000 Population Growth")

# Visualize cumulative lines
df %>% 
  # Pivot data
  pivot_longer(
    !c("GEO", "REF_DATE", "pop", "pop_change"),
    names_to = "housing_metric", values_to = "value") %>%
  # Filter data to Canada only
  filter(GEO == "Canada") %>%
  # Filter metrics for ratio of housing completions to population change
  filter(housing_metric %in%
           c("completions_cumsum_all", "pop_change_cumsum")) %>%
  # Plot data
  ggplot(aes(x = REF_DATE, color = housing_metric)) +
  # Set graph theme
  theme_minimal() +
  # Plot ratio of housing completions to population change as a line
  geom_line(aes(y = value), lwd = 1) +
  # Add graph title
  ggtitle("Cumulative Housing Completions per 1,000 Population Growth")

# Visualize cumulative lines
df %>% 
  # Pivot data
  pivot_longer(
    !c("GEO", "REF_DATE", "pop", "pop_change"),
    names_to = "housing_metric", values_to = "value") %>%
  # Filter data to Canada only
  filter(GEO == "Canada") %>%
  # Filter metrics for ratio of housing completions to population change
  filter(housing_metric %in%
           c("completions_cumsum_single", "pop_change_cumsum")) %>%
  # Plot data
  ggplot(aes(x = REF_DATE, color = housing_metric)) +
  # Set graph theme
  theme_minimal() +
  # Plot ratio of housing completions to population change as a line
  geom_line(aes(y = value), lwd = 1) +
  # Add graph title
  ggtitle("Cumulative Housing Completions per 1,000 Population Growth")