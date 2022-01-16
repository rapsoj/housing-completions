#### PREPARE WORKSPACE ####

# Set working directory
setwd(paste0("~/Desktop/Projects/Personal/Website/Posts/",
             "2021-01-15-canada-housing-supply/analysis"))

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
library(scales)
loadfonts()


#### CLEAN DATA ####

# Read in data for annual housing completions
df_housing <- read.csv("./data/housing.csv") %>%
  # Filter data
  filter(
    # Filter to housing completion data
    Housing.estimates == "Housing completions",
    # Filter out multiples unit type
    Type.of.unit != "Multiples") %>%
  # Rename unit types
  mutate(Type.of.unit =
           mapvalues(Type.of.unit,
                     from = c("Total units", "Single-detached", "Semi-detached",
                              "Row",  "Apartment and other unit type"),
                     to = c("All", "Single", "Semi", "Row", "Apartment"))) %>%
  # Rename completions columns
  rename(completions = VALUE) %>%
  # Group by region and housing type
  group_by(GEO, Type.of.unit) %>%
  # Edit columns
  mutate(
    # Format reference date as date
    REF_DATE = as.yearqtr(as.yearmon(REF_DATE))) %>%
  # Ungroup data
  ungroup() %>%
  # Select columns of interest
  select(REF_DATE, GEO, Type.of.unit, completions)

# Read in data for annual population estimates
df_pop <- read.csv("./data/population.csv") %>%
  # Rename column
  rename(pop = VALUE) %>%
  # Group by region
  group_by(GEO) %>%
  # Remove dates beyond available housing data
  #filter(as.yearqtr(as.yearmon(REF_DATE)) >= min(df_housing$REF_DATE) - 1/4 &
  #         as.yearqtr(as.yearmon(REF_DATE)) <= max(df_housing$REF_DATE)) %>%
  # Edit columns
  mutate(
    # Format reference date as date
    REF_DATE = as.yearqtr(as.yearmon(REF_DATE)),
    # Calculate quarterly change in population
    pop_change = pop - lag(pop)) %>%
  # Ungroup data
  ungroup() %>%
  # Remove rows with missing data
  filter(!is.na(pop_change)) %>%
  # Select columns of interest
  select(GEO, REF_DATE, pop_change)

# Join population and housing completion data
df <- df_housing %>%
  # Merge housing completion data
  merge(df_pop, by = c("GEO", "REF_DATE")) %>%
  # Group by year, geography, and housing type
  group_by(year = year(as.Date(as.yearmon(REF_DATE))), GEO, Type.of.unit) %>%
  # Add columns for yearly totals
  summarise_if(is.numeric, sum, na.rm = T) %>%
  # Group by geography and housing type
  group_by(GEO, Type.of.unit) %>%
    # Add columns for ratio of housing completions to population change
    mutate(
      # Convert year to date type
      year = as.Date(paste0(year, "-01-01")),
      # Calculate cumulative sum of annual population change
      pop_change_cumsum = cumsum(ifelse(!is.na(pop_change), pop_change, 0)),
      # Calculate cumulative sum of annual housing completions
      completions_cumsum = cumsum(completions),
      # Calculate number of people per housing completion
      pop_per_completion = pop_change / completions,
      # Calculate cumulative number of people per housing completion
      pop_per_completion_cumsum = pop_change_cumsum / completions_cumsum,
      # Calculate rolling mean of number of people per housing completion
      pop_per_completion_roll = rollmean(pop_per_completion, k = 3, fill = NA),
      # Replace end values in people rolling mean with original value
      pop_per_completion_roll = ifelse(
        is.na(pop_per_completion_roll),
        pop_per_completion, pop_per_completion_roll),
      # Calculate ratio of all housing completions to population change
      completion_per_1000 = completions / pop_change * 1000,
      # Calculate cumulative ratio of housing completions to pop change
      completions_per_1000_cumsum =
        completions_cumsum / pop_change_cumsum * 1000, 
      # Calculate rolling mean of housing completions to population change
      completion_per_1000_roll = rollmean(
        completion_per_1000, k = 3, fill = NA),
      # Replace end values in completions rolling mean with original value
      completion_per_1000_roll = ifelse(
        is.na(completion_per_1000_roll),
        completion_per_1000, completion_per_1000_roll)) %>%
  # Ungroup data
  ungroup()


#### CREATE VISUALIZATION OF COMPLETIONS TO POPULATION CHANGE ####

# Create graph for ratio of all housing completions to population change
graph1 <- df %>%
  # Filter data to Canada and all housing types only
  filter(GEO == "Canada" & Type.of.unit == "All") %>%
  # Plot data
  ggplot(aes(x = year, fill = completion_per_1000_roll)) +
  # Plot ratio of housing completions to population change as a line
  geom_line(aes(y = completion_per_1000_roll), lwd = 2, color = "#59594A") +
  # Add graph title
  labs(title = paste("Have Canadian housing completions", 
                     "kept up with population growth?"),
       subtitle = paste("Annual housing completions per 1,000 population",
                        "increase,", "3-year rolling average",
                        "(Canada, 1948-2021)\n")) +
  # Remove x-axis title
  xlab("") +
  # Remove y-axis title
  ylab("") +
  # Change x-axis formatting
  scale_x_date(date_breaks = "4 years" , date_labels = "%y",
               limit = c(as.Date("1948-01-01"), as.Date("2021-01-01"))) +
  # Add average line
  geom_hline(yintercept = mean(
    df[df$GEO == "Canada" & df$Type.of.unit == "All",]$completion_per_1000_roll,
    na.rm = T), color = "#59594A", linetype = 'dashed') +
  # Add annotation for average line
  annotate("text", x = as.Date("1956-01-01"), y = 530, vjust = -0.4,
           label = "1948-2021 Average", family = "CMU Bright") +
  # Set graph theme
  theme_minimal() +
  # Customize theme elements
  theme(text = element_text(family = "CMU Bright", size = 16),
        plot.title = element_text(family = "CMU Bright SemiBold"),
        plot.subtitle = element_text(face = "plain"),
        plot.margin = unit(c(1, 1, 0.2, 1.2), "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#CCCCCC"),
        plot.background = element_rect(fill = "#E6E6E6", color = NA))

# Create graph for ratio of housing completions to population change facets
graph2 <- df %>%
  # Filter data to Canada and housing subtypes only
  filter(GEO == "Canada" & Type.of.unit != "All") %>%
  # Edit columns
  mutate(
    # Rename housing types
    Type.of.unit = mapvalues(
      Type.of.unit, from = c("Single", "Semi", "Row", "Apartment"),
      to = c("Single-Detached Houses", "Semi-Detached Houses",
             "Row Houses", "Apartments")), 
    # Set housing types as factors with specified order
    Type.of.unit = factor(
      Type.of.unit, levels = c("Single-Detached Houses",
                                   "Semi-Detached Houses",
                                   "Row Houses", "Apartments"))) %>%
  # Plot data
  ggplot(aes(x = year, color = Type.of.unit)) +
  # Plot ratio of housing completions to population change as a line
  geom_line(aes(y = completion_per_1000_roll, color = Type.of.unit), lwd = 2) +
  # Specify colours
  scale_color_manual(
    values = c("Single-Detached Houses" = "#BE6E46",
               "Semi-Detached Houses" = "#CDE7B0",
               "Row Houses" = "#A3BFA8",
               "Apartments" = "#7286A0")) +
  # Remove graph title
  ggtitle("") +
  # Add caption
  labs(caption = paste("\nStatistics Canada. (2021). Population estimates,",
                       "quarterly", "\nStatistics Canada. (2021). Canada",
                       "Mortgage and Housing Corporation, housing starts,",
                       "under construction and completions, all areas,",
                       "quarterly.")) +
  # Remove x-axis title
  xlab("") +
  # Add y-axis title
  ylab("Housing Completions per 1,000 Population Increase") +
  # Change x-axis formatting
  scale_x_date(date_breaks = "4 years" , date_labels = "%y",
               limit = c(as.Date("1948-01-01"), as.Date("2021-01-01"))) +
  # Make legend single row
  guides(colour = guide_legend(nrow = 1)) +
  # Add average line
  geom_hline(yintercept = mean(
    df[df$GEO == "Canada" & df$Type.of.unit == "Single",]$completion_per_1000_roll,
    na.rm = T), color = "#BE6E46", linetype = 'dashed') +
  # Add annotation for average line
  annotate("text", x = as.Date("1956-01-01"), y = 254, vjust = -0.4,
           label = "1955-2021 Single-Detatched Average", color = "#BE6E46",
           family = "CMU Bright") +
  # Add average line
  geom_hline(yintercept = mean(
    df[df$GEO == "Canada" & df$Type.of.unit == "Apartment",]$completion_per_1000_roll,
    na.rm = T), color = "#7286A0", linetype = 'dashed') +
  # Add annotation for average line
  annotate("text", x = as.Date("1954-09-01"), y = 158, vjust = -0.4,
           label = "1955-2021 Apartments Average", color = "#7286A0",
           family = "CMU Bright") +
  # Set graph theme
  theme_minimal() +
  # Customize theme elements
  theme(text = element_text(family = "CMU Bright", size = 16),
        legend.position = c(.5, 1.05), legend.title = element_blank(),
        axis.title.y = element_text(hjust = -0.5, vjust = 4),
        plot.caption = element_text(size = 10, hjust = 0, vjust = 4),
        plot.margin = unit(c(0.2, 1, 1, 1.2), "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#CCCCCC"),
        plot.background = element_rect(fill = "#E6E6E6", color = NA))

# Combine two graphs
png("./images/annual_completions_to_pop.png", units = "in",
    width = 12, height = 10, res = 1000)
grid.arrange(graph1, graph2, ncol = 1)
dev.off()

#### CREATE VISUALIZATION OF POPULATION CHANGE TO COMPLETIONS ####

# Create graph for ratio of population change to all housing completions
graph1 <- df %>%
  # Filter data to Canada and all housing types only
  filter(GEO == "Canada" & Type.of.unit == "All") %>%
  # Plot data
  ggplot(aes(x = year, fill = pop_per_completion_roll)) +
  # Plot ratio of housing completions to population change as a line
  geom_line(aes(y = pop_per_completion_roll), lwd = 2, color = "#59594A") +
  # Add graph title
  labs(title = paste("Have Canadian housing completions",
                     "kept up with population growth?"),
       subtitle = paste("Annual population increase per housing completion,",
                        "3-year rolling average", "(Canada, 1948-2021)\n")) +
  # Remove x-axis title
  xlab("") +
  # Remove y-axis title
  ylab("") +
  # Change x-axis formatting
  scale_x_date(date_breaks = "4 years" , date_labels = "%y",
               limit = c(as.Date("1948-01-01"), as.Date("2021-01-01"))) +
  # Add average line
  geom_hline(yintercept = mean(
    df[df$GEO == "Canada" & df$Type.of.unit == "All",]$pop_per_completion_roll,
    na.rm = T), color = "#59594A", linetype = 'dashed') +
  # Add annotation for average line
  annotate("text", x = as.Date("1956-01-01"), y = 2.3, vjust = 2.1,
           label = "1948-2021 Average", family = "CMU Bright") +
  # Set graph theme
  theme_minimal() +
  # Customize theme elements
  theme(text = element_text(family = "CMU Bright", size = 16),
        plot.title = element_text(family = "CMU Bright SemiBold"),
        plot.subtitle = element_text(face = "plain"),
        plot.margin = unit(c(1, 1, 0.2, 1.2), "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#CCCCCC"),
        plot.background = element_rect(fill = "#E6E6E6", color = NA))

# Create graph for ratio of housing completions to population change facets
graph2 <- df %>%
  # Filter data to Canada and housing subtypes only
  filter(GEO == "Canada" & !Type.of.unit %in% c("All", "Row", "Semi")) %>%
  # Edit columns
  mutate(
    # Rename housing types
    Type.of.unit = mapvalues(
      Type.of.unit, from = c("Single", "Apartment"),
      to = c("Single-Detached Houses", "Apartments")), 
    # Set housing types as factors with specified order
    Type.of.unit = factor(
      Type.of.unit, levels = c("Single-Detached Houses", "Apartments"))) %>%
  # Plot data
  ggplot(aes(x = year, color = Type.of.unit)) +
  # Plot ratio of housing completions to population change as a line
  geom_line(aes(y = pop_per_completion_roll, color = Type.of.unit), lwd = 2) +
  # Specify colours
  scale_color_manual(
    values = c("Single-Detached Houses" = "#BE6E46",
               "Apartments" = "#7286A0")) +
  # Remove graph title
  ggtitle("") +
  # Add caption
  labs(caption = paste("\nStatistics Canada. (2021). Population estimates,",
                       "quarterly", "\nStatistics Canada. (2021). Canada",
                       "Mortgage and Housing Corporation, housing starts,",
                       "under construction and completions, all areas,",
                       "quarterly.")) +
  # Remove x-axis title
  xlab("") +
  # Add y-axis title
  ylab("Population Increase per Housing Completion") +
  # Change x-axis formatting
  scale_x_date(date_breaks = "4 years" , date_labels = "%y",
               limit = c(as.Date("1948-01-01"), as.Date("2021-01-01"))) +
  # Make legend single row
  guides(colour = guide_legend(nrow = 1)) +
  # Add average line
  geom_hline(yintercept = mean(
    df[df$GEO == "Canada" & df$Type.of.unit == "Single",]$pop_per_completion_roll,
    na.rm = T), color = "#BE6E46", linetype = 'dashed') +
  # Add annotation for average line
  annotate("text", x = as.Date("1956-01-01"), y = 2.9, vjust = -0.4,
           label = "1955-2021 Single-Detatched Average", color = "#BE6E46",
           family = "CMU Bright") +
  # Add average line
  geom_hline(yintercept = mean(
    df[df$GEO == "Canada" & df$Type.of.unit == "Apartment",]$pop_per_completion_roll,
    na.rm = T), color = "#7286A0", linetype = 'dashed') +
  # Add annotation for average line
  annotate("text", x = as.Date("1954-09-01"), y = 5.7, vjust = -0.4,
           label = "1955-2021 Apartments Average", color = "#7286A0",
           family = "CMU Bright") +
  # Set graph theme
  theme_minimal() +
  # Customize theme elements
  theme(text = element_text(family = "CMU Bright", size = 16),
        axis.title.y = element_text(hjust = -1.6, vjust = 4), 
        legend.position = c(.5, 1.05), legend.title = element_blank(),
        plot.caption = element_text(size = 10, hjust = 0, vjust = 4),
        plot.margin = unit(c(0.2, 1, 1, 1.2), "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#CCCCCC"),
        plot.background = element_rect(fill = "#E6E6E6", color = NA))

# Combine two graphs
png("./images/annual_pop_to_completions.png", units = "in",
    width = 12, height = 10, res = 1000)
grid.arrange(graph1, graph2, ncol = 1)
dev.off()



#### CREATE VISUALIZATION OF CUMULATIVE COMPLETIONS TO POPULATION CHANGE ####

# Create graph for ratio of all housing completions to population change
graph1 <- df %>%
  # Filter data to Canada and all housing types only
  filter(GEO == "Canada" & Type.of.unit == "All") %>%
  # Plot data
  ggplot(aes(x = year, fill = completions_per_1000_cumsum)) +
  # Plot ratio of housing completions to population change as a line
  geom_line(aes(y = completions_per_1000_cumsum), lwd = 2, color = "#59594A") +
  # Add graph title
  labs(title = paste("Have Canadian housing completions",
                     "kept up with population growth?"),
       subtitle = paste("Cumulative housing completions per 1,000",
                        "population increase (Canada, 1948-2021)\n")) +
  # Remove x-axis title
  xlab("") +
  # Remove y-axis title
  ylab("") +
  # Change x-axis formatting
  scale_x_date(date_breaks = "4 years" , date_labels = "%y",
               limit = c(as.Date("1948-01-01"), as.Date("2021-01-01"))) +
  # Add average line
  geom_hline(yintercept = mean(
    df[df$GEO == "Canada" & df$Type.of.unit == "All",]$completions_per_1000_cumsum,
    na.rm = T), color = "#59594A", linetype = 'dashed') +
  # Add annotation for average line
  annotate("text", x = as.Date("1956-01-01"), y = 400, vjust = -0.4,
           label = "1948-2021 Average", family = "CMU Bright") +
  # Set graph theme
  theme_minimal() +
  # Customize theme elements
  theme(text = element_text(family = "CMU Bright", size = 16),
        plot.title = element_text(family = "CMU Bright SemiBold"),
        plot.subtitle = element_text(face = "plain"),
        plot.margin = unit(c(1, 1, 0.2, 1.2), "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#CCCCCC"),
        plot.background = element_rect(fill = "#E6E6E6", color = NA))

# Create graph for ratio of housing completions to population change facets
graph2 <- df %>%
  # Filter data to Canada and housing subtypes only
  filter(GEO == "Canada" & Type.of.unit != "All") %>%
  # Edit columns
  mutate(
    # Rename housing types
    Type.of.unit = mapvalues(
      Type.of.unit, from = c("Single", "Semi", "Row", "Apartment"),
      to = c("Single-Detached Houses", "Semi-Detached Houses",
             "Row Houses", "Apartments")), 
    # Set housing types as factors with specified order
    Type.of.unit = factor(
      Type.of.unit, levels = c("Single-Detached Houses",
                               "Semi-Detached Houses",
                               "Row Houses", "Apartments"))) %>%
  # Plot data
  ggplot(aes(x = year, color = Type.of.unit)) +
  # Plot ratio of housing completions to population change as a line
  geom_line(aes(y = completions_per_1000_cumsum, color = Type.of.unit), lwd = 2) +
  # Specify colours
  scale_color_manual(
    values = c("Single-Detached Houses" = "#BE6E46",
               "Semi-Detached Houses" = "#CDE7B0",
               "Row Houses" = "#A3BFA8",
               "Apartments" = "#7286A0")) +
  # Remove graph title
  ggtitle("") +
  # Add caption
  labs(caption = paste("\nStatistics Canada. (2021). Population estimates,",
                       "quarterly", "\nStatistics Canada. (2021). Canada",
                       "Mortgage and Housing Corporation, housing starts,",
                       "under construction and completions, all areas,",
                       "quarterly.")) +
  # Remove x-axis title
  xlab("") +
  # Add y-axis title
  ylab("Cumulative Housing Completions per 1,000 Population Increase") +
  # Change x-axis formatting
  scale_x_date(date_breaks = "4 years" , date_labels = "%y",
               limit = c(as.Date("1948-01-01"), as.Date("2021-01-01"))) +
  # Make legend single row
  guides(colour = guide_legend(nrow = 1)) +
  # Set graph theme
  theme_minimal() +
  # Customize theme elements
  theme(text = element_text(family = "CMU Bright", size = 16),
        axis.title.y = element_text(hjust = -0.1, vjust = 4), 
        legend.position = c(.5, 1.05), legend.title = element_blank(),
        plot.caption = element_text(size = 10, hjust = 0, vjust = 4),
        plot.margin = unit(c(0.2, 1, 1, 1.2), "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#CCCCCC"),
        plot.background = element_rect(fill = "#E6E6E6", color = NA))

# Combine two graphs
png("./images/cumulative_completions_to_pop.png", units = "in",
    width = 12, height = 10, res = 1000)
grid.arrange(graph1, graph2, ncol = 1)
dev.off()


#### CREATE VISUALIZATION OF CUMULATIVE POPULATION CHANGE TO COMPLETIONS ####

# Create graph for ratio of population change to all housing completions 
graph1 <- df %>%
  # Filter data to Canada and all housing types only
  filter(GEO == "Canada" & Type.of.unit == "All") %>%
  # Plot data
  ggplot(aes(x = year, fill = pop_per_completion_cumsum)) +
  # Plot ratio of housing completions to population change as a line
  geom_line(aes(y = pop_per_completion_cumsum), lwd = 2, color = "#59594A") +
  # Add graph title
  labs(title = paste("Have Canadian housing completions",
                     "kept up with population growth?"),
       subtitle = paste("Cumulative population increase per housing",
                        "completion (Canada, 1948-2021)\n")) +
  # Remove x-axis title
  xlab("") +
  # Remove y-axis title
  ylab("") +
  # Change x-axis formatting
  scale_x_date(date_breaks = "4 years" , date_labels = "%y",
               limit = c(as.Date("1948-01-01"), as.Date("2021-01-01"))) +
  # Add average line
  geom_hline(yintercept = mean(
    df[df$GEO == "Canada" & df$Type.of.unit == "All",]$pop_per_completion_cumsum,
    na.rm = T), color = "#59594A", linetype = 'dashed') +
  # Add annotation for average line
  annotate("text", x = as.Date("1956-01-01"), y = 2.8, vjust = -0.4,
           label = "1948-2021 Average", family = "CMU Bright") +
  # Set graph theme
  theme_minimal() +
  # Customize theme elements
  theme(text = element_text(family = "CMU Bright", size = 16),
        plot.title = element_text(family = "CMU Bright SemiBold"),
        plot.subtitle = element_text(face = "plain"),
        plot.margin = unit(c(1, 1, 0.2, 1.2), "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#CCCCCC"),
        plot.background = element_rect(fill = "#E6E6E6", color = NA))

# Create graph for ratio of housing completions to population change facets
graph2 <- df %>%
  # Filter data to Canada and housing subtypes only
  filter(GEO == "Canada" & !Type.of.unit %in% c("All", "Row", "Semi")) %>%
  # Edit columns
  mutate(
    # Rename housing types
    Type.of.unit = mapvalues(
      Type.of.unit, from = c("Single", "Apartment"),
      to = c("Single-Detached Houses", "Apartments")), 
    # Set housing types as factors with specified order
    Type.of.unit = factor(
      Type.of.unit, levels = c("Single-Detached Houses", "Apartments"))) %>%
  # Plot data
  ggplot(aes(x = year, color = Type.of.unit)) +
  # Plot ratio of housing completions to population change as a line
  geom_line(aes(y = pop_per_completion_cumsum, color = Type.of.unit), lwd = 2) +
  # Specify colours
  scale_color_manual(
    values = c("Single-Detached Houses" = "#BE6E46",
               "Apartments" = "#7286A0")) +
  # Remove graph title
  ggtitle("") +
  # Add caption
  labs(caption = paste("\nStatistics Canada. (2021). Population estimates,",
                       "quarterly", "\nStatistics Canada. (2021). Canada",
                       "Mortgage and Housing Corporation, housing starts,",
                       "under construction and completions, all areas,",
                       "quarterly.")) +
  # Remove x-axis title
  xlab("") +
  # Add y-axis title
  ylab("Cumulative population Increase per Housing Completion") +
  # Change x-axis formatting
  scale_x_date(date_breaks = "4 years" , date_labels = "%y",
               limit = c(as.Date("1948-01-01"), as.Date("2021-01-01"))) +
  # Make legend single row
  guides(colour = guide_legend(nrow = 1)) +
  # Set graph theme
  theme_minimal() +
  # Customize theme elements
  theme(text = element_text(family = "CMU Bright", size = 16),
        axis.title.y = element_text(hjust = -0.25, vjust = 4), 
        legend.position = c(.5, 1.05), legend.title = element_blank(),
        plot.caption = element_text(size = 10, hjust = 0, vjust = 4),
        plot.margin = unit(c(0.2, 1, 1, 1.2), "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#CCCCCC"),
        plot.background = element_rect(fill = "#E6E6E6", color = NA))

# Combine two graphs
png("./images/cumulative_pop_to_completions.png", units = "in",
    width = 12, height = 10, res = 1000)
grid.arrange(graph1, graph2, ncol = 1)
dev.off()


#### CREATE VISUALIZATION OF COMPLETIONS TO POPULATION CHANGE TREND ####

# Conduct linear regression over time for all housing completions to population
trend <- df %>%
  #Filter data to Canada and all housing types only
  filter(GEO == "Canada" & Type.of.unit == "All") %>%
  # Run linear regression on time
  lm(completion_per_1000_roll ~ year, .) %>%
  # Summarize model results
  summary()

# Plot regression trend line for housing completions to population growth
png("./images/trend_completions_to_pop.png", units = "in",
    width = 12, height = 7, res = 1000)
df %>%
  # Filter data to Canada and all housing types only
  filter(GEO == "Canada" & Type.of.unit == "All") %>%
  # Plot data
  ggplot(aes(x = year, fill = completion_per_1000_roll)) +
  # Plot ratio of housing completions to population change as a line
  geom_line(aes(y = completion_per_1000_roll), lwd = 2, color = "#59594A") +
  # Add graph title
  labs(title = paste("Have Canadian housing completions",
                     "kept up with population growth?"),
       subtitle = paste("Annual housing completions per 1,000 population",
                        "increase (Canada, 1948-2021)\n"),
       caption = paste("\nStatistics Canada. (2021). Population estimates,",
                       "quarterly", "\nStatistics Canada. (2021). Canada",
                       "Mortgage and Housing Corporation, housing starts,",
                       "under construction and completions, all areas,",
                       "quarterly.")) +
  # Remove x-axis title
  xlab("") +
  # Add y-axis title
  ylab("Housing Completions per 1,000 Population Increase") +
  # Change x-axis formatting
  scale_x_date(date_breaks = "4 years" , date_labels = "%y",
               limit = c(as.Date("1948-01-01"), as.Date("2021-01-01"))) +
  # Add trend line
  geom_abline(intercept = trend$coefficients[1], slope = trend$coefficients[2],
              color = "#59594A", linetype = 'dashed') +
  # Add annotation for average line
  annotate("text", x = as.Date("1956-01-01"), y = 500, vjust = 3, angle = 10,
           label = "1948-2021 Trend", family = "CMU Bright") +
  # Set graph theme
  theme_minimal() +
  # Customize theme elements
  theme(text = element_text(family = "CMU Bright", size = 16),
        plot.title = element_text(family = "CMU Bright SemiBold"),
        plot.subtitle = element_text(face = "plain"),
        axis.title.y = element_text(hjust = 0.2, vjust = 4), 
        plot.caption = element_text(size = 10, hjust = 0, vjust = 4),
        plot.margin = unit(c(1, 1, 0.2, 1.2), "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#CCCCCC"),
        plot.background = element_rect(fill = "#E6E6E6", color = NA))
dev.off()


#### CREATE VISUALIZATION OF POPULATION CHANGE TO COMPLETIONS TREND ####

# Conduct linear regression over time for population all housing completions
trend <- df %>%
  #Filter data to Canada and all housing types only
  filter(GEO == "Canada" & Type.of.unit == "All") %>%
  # Run linear regression on time
  lm(pop_per_completion_roll ~ year, .) %>%
  # Summarize model results
  summary()

# Plot regression trend line for population growth to housing completions
png("./images/trend_pop_to_completions.png", units = "in",
    width = 12, height = 7, res = 1000)
df %>%
  # Filter data to Canada and all housing types only
  filter(GEO == "Canada" & Type.of.unit == "All") %>%
  # Plot data
  ggplot(aes(x = year, fill = pop_per_completion_roll)) +
  # Plot ratio of housing completions to population change as a line
  geom_line(aes(y = pop_per_completion_roll), lwd = 2, color = "#59594A") +
  # Add graph title
  labs(title = paste("Have Canadian housing completions",
                     "kept up with population growth?"),
       subtitle = paste("Annual population increase per housing",
                        "completion (Canada, 1948-2021)\n"),
       caption = paste("\nStatistics Canada. (2021). Population estimates,",
                       "quarterly", "\nStatistics Canada. (2021). Canada",
                       "Mortgage and Housing Corporation, housing starts,",
                       "under construction and completions, all areas,",
                       "quarterly.")) +
  # Remove x-axis title
  xlab("") +
  # Add y-axis title
  ylab("Population Increase per Housing Completion") +
  # Change x-axis formatting
  scale_x_date(date_breaks = "4 years" , date_labels = "%y",
               limit = c(as.Date("1948-01-01"), as.Date("2021-01-01"))) +
  # Add trend line
  geom_abline(intercept = trend$coefficients[1], slope = trend$coefficients[2],
              color = "#59594A", linetype = 'dashed') +
  # Add annotation for average line
  annotate("text", x = as.Date("1954-01-01"), y = 3, vjust = 2.5, angle = -10,
           label = "1948-2021 Trend", family = "CMU Bright") +
  # Set graph theme
  theme_minimal() +
  # Customize theme elements
  theme(text = element_text(family = "CMU Bright", size = 16),
        plot.title = element_text(family = "CMU Bright SemiBold"),
        plot.subtitle = element_text(face = "plain"),
        axis.title.y = element_text(hjust = 0.2, vjust = 4), 
        plot.caption = element_text(size = 10, hjust = 0, vjust = 4),
        plot.margin = unit(c(1, 1, 0.2, 1.2), "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#CCCCCC"),
        plot.background = element_rect(fill = "#E6E6E6", color = NA))
dev.off()


#### CONDUCT REGRESSION OF HOUSING STARTS TO POPULATION CHANGE PER DECADE ####

# Create table of trends in housing starts to population change by decade
trend <- data.frame(year = character(), trend = numeric(), sig = numeric())
# Analyze trend for every decade in data set
for(i in seq(as.Date("1950-01-01"), as.Date("2010-01-01"), "10 years")){
  # Conduct linear regression over time for selected decade
  subtrend <- df %>%
    # Filter data to Canada and all housing types only
    filter(GEO == "Canada" & Type.of.unit == "All") %>%
    # Filter data to dates past 2010
    filter(year >= as.Date(i) & year <= as.Date(i) + years(10)) %>%
    # Run linear regression on time
    lm(completion_per_1000_roll ~ year, .) %>%
    # Summarize model results
    summary()
  # Add row to table of trends
  row <- data.frame(
    paste(year(as.Date(i)), "'s", sep = ""),
    subtrend$coefficients[2] * 365.25,
    subtrend$coefficients[8])
  names(row) = c("year", "trend", "sig")
  trend <- rbind(trend, row)
}


#### CREATE VISUALIZATION OF CUMULATIVE POP TO COMPLETIONS RATIO BY REGION ####

# Create graph for cumulative ratio of population to completions by region
graph1 <- df %>%
  # Filter to regional areas, all housing types, and 2021 only
  filter(GEO != "Canada" & Type.of.unit == "All" & year(year) == 2021) %>%
  # Edit columns
  mutate(
    # Rename housing types
    GEO = mapvalues(
      GEO, from = c("Alberta", "British Columbia", "Manitoba", "Ontario",
                    "New Brunswick", "Newfoundland and Labrador", "Nova Scotia",
                    "Quebec", "Prince Edward Island", "Saskatchewan"),
      to = c("AB", "BC", "MB", "ON", "NB", "NL", "NS", "QC", "PE", "SK"))) %>%
  # Plot data
  ggplot(aes(x = reorder(GEO, -pop_per_completion_cumsum),
             y = pop_per_completion_cumsum)) +
  # Plot ratio of cumulative population change to housing completions as bars
  geom_bar(stat = "identity") +
  # Add graph title
  labs(title = paste("Have Canadian housing completions",
                     "kept up with population growth?"),
       subtitle = paste("Total cumulative population increase per",
                        "housing completion (Canada, 1948-2021)\n")) +
  # Remove x-axis title
  xlab("") +
  # Remove y-axis title
  ylab("") +
  # Change y-axis formatting
  scale_y_continuous(limit = c(0, 3.5)) +
  # Add average line
  geom_hline(
    yintercept = df[df$GEO == "Canada" & df$Type.of.unit == "All" &
                      year(df$year) == 2021, ]$pop_per_completion_cumsum,
    color = "#59594A", linetype = 'dashed') +
  # Add annotation for average line
  annotate("text", x = "SK", y = 1.9, vjust = -1.5,
           label = "Canada Total", family = "CMU Bright") +
  # Set graph theme
  theme_minimal() +
  # Customize theme elements
  theme(text = element_text(family = "CMU Bright", size = 16),
        plot.title = element_text(family = "CMU Bright SemiBold"),
        plot.subtitle = element_text(face = "plain"),
        plot.margin = unit(c(1, 1, 0.2, 1.2), "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#CCCCCC"),
        plot.background = element_rect(fill = "#E6E6E6", color = NA))

# Create graph for ratio of housing completions to population change facets
graph2 <- df %>%
  # Filter to regional areas and 2021 only
  filter(GEO != "Canada" & year(year) == 2021) %>%
  # Filter to select housing subtypes
  filter(Type.of.unit %in% c("Single", "Apartment")) %>%
  # Edit columns
  mutate(
    # Rename housing types
    Type.of.unit = mapvalues(
      Type.of.unit, from = c("Single", "Apartment"),
      to = c("Single-Detached Houses", "Apartments")), 
    # Set housing types as factors with specified order
    Type.of.unit = factor(
      Type.of.unit, levels = c("Single-Detached Houses", "Apartments")),
    # Rename housing types
    GEO = mapvalues(
      GEO, from = c("Alberta", "British Columbia", "Manitoba", "Ontario",
                    "New Brunswick", "Newfoundland and Labrador", "Nova Scotia",
                    "Quebec", "Prince Edward Island", "Saskatchewan"),
      to = c("AB", "BC", "MB", "ON", "NB", "NL", "NS", "QC", "PE", "SK")),
    # Set housing types as factors with specified order
    GEO = factor(
      GEO, levels = c("ON", "AB", "BC", "QC", "MB",
                      "PE", "NB", "NS", "SK", "NL"))) %>%
  # Plot data
  ggplot(aes(x = GEO, y = pop_per_completion_cumsum,
         fill = factor(Type.of.unit), )) +
  # Plot ratio of cumulative population change to housing completions as bars
  geom_bar(stat = "identity", position = "dodge") +
  # Specify colours
  scale_fill_manual(
    values = c("Single-Detached Houses" = "#BE6E46",
               "Apartments" = "#7286A0")) +
  # Remove graph title
  ggtitle("") +
  # Add caption
  labs(caption = paste("\nStatistics Canada. (2021). Population estimates,",
                       "quarterly", "\nStatistics Canada. (2021). Canada",
                       "Mortgage and Housing Corporation, housing starts,",
                       "under construction and completions, all areas,",
                       "quarterly.")) +
  # Remove x-axis title
  xlab("") +
  # Add y-axis title
  ylab("Total cumulative Population Increase per Housing Completion") +
  # Change y-axis formatting
  scale_y_continuous(limit = c(0, 11), breaks = seq(0, 10, 2)) +
  # Make legend single row
  guides(fill = guide_legend(nrow = 1)) +
  # Set graph theme
  theme_minimal() +
  # Customize theme elements
  theme(text = element_text(family = "CMU Bright", size = 16),
        axis.title.y = element_text(hjust = -0.3, vjust = 4), 
        legend.position = c(.5, 1.05), legend.title = element_blank(),
        plot.caption = element_text(size = 10, hjust = 0, vjust = 4),
        plot.margin = unit(c(0.2, 1, 1, 1.2), "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#CCCCCC"),
        plot.background = element_rect(fill = "#E6E6E6", color = NA))

# Combine two graphs
png("./images/region_pop_to_completions_total.png", units = "in",
    width = 12, height = 10, res = 1000)
grid.arrange(graph1, graph2, ncol = 1)
dev.off()


#### CREATE VISUALIZATION OF RECENT CUMULATIVE POP TO HOUSING RATIO BY REGION ####

# Create graph for cumulative ratio of population to completions by region
graph1 <- df %>%
  # Filter to regional areas, all housing types, and years after 2016 only
  filter(GEO != "Canada" & Type.of.unit == "All" & year(year) >= 2016) %>%
  # Group by region
  group_by(GEO) %>%
  # Edit grouped columns
  mutate(
    # Create column for recent cumulative population change
    sum_pop_change = sum(pop_change),
    # Create column for recent cumulative completions
    sum_completions = sum(completions),
    # Create column for recent cumulative pop change to completion ratio
    recent_pop_to_completions = sum_pop_change / sum_completions) %>%
  # Ungroup data
  ungroup() %>%
  # Select single year
  filter(year(year) == 2021) %>%
  # Edit columns
  mutate(
    # Rename housing types
    GEO = mapvalues(
      GEO, from = c("Alberta", "British Columbia", "Manitoba", "Ontario",
                    "New Brunswick", "Newfoundland and Labrador", "Nova Scotia",
                    "Quebec", "Prince Edward Island", "Saskatchewan"),
      to = c("AB", "BC", "MB", "ON", "NB", "NL", "NS", "QC", "PE", "SK")),
    # Replace negative values with zero
    recent_pop_to_completions = ifelse(
      recent_pop_to_completions < 0, 0, recent_pop_to_completions)) %>%
  # Plot data
  ggplot(aes(x = reorder(GEO, -recent_pop_to_completions),
             y = recent_pop_to_completions)) +
  # Plot ratio of cumulative population change to housing completions as bars
  geom_bar(stat = "identity") +
  # Add graph title
  labs(title = paste("Have Canadian housing completions",
                     "kept up with population growth?"),
       subtitle = paste("Total cumulative population increase",
                        "per housing completion (Canada, 2016-2021)\n")) +
  # Remove x-axis title
  xlab("") +
  # Remove y-axis title
  ylab("") +
  # Change y-axis formatting
  scale_y_continuous(limit = c(0, 3.5)) +
  # Add average line
  geom_hline(
    yintercept =
      sum(df[df$GEO == "Canada" & df$Type.of.unit == "All" &
               year(df$year) >= 2016, ]$pop_change) /
      sum(df[df$GEO == "Canada" & df$Type.of.unit == "All" &
               year(df$year) >= 2016, ]$completions),
    color = "#59594A", linetype = 'dashed') +
  # Add annotation for average line
  annotate("text", x = "QC", y = 2.1, vjust = -0.8,
           label = "Canada Total", family = "CMU Bright") +
  # Set graph theme
  theme_minimal() +
  # Customize theme elements
  theme(text = element_text(family = "CMU Bright", size = 16),
        plot.title = element_text(family = "CMU Bright SemiBold"),
        plot.subtitle = element_text(face = "plain"),
        plot.margin = unit(c(1, 1, 0.2, 1.2), "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#CCCCCC"),
        plot.background = element_rect(fill = "#E6E6E6", color = NA))

# Create graph for ratio of housing completions to population change facets
graph2 <- df %>%
  # Filter to regional areas and years after 2016 only
  filter(GEO != "Canada" & year(year) >= 2016) %>%
  # Filter to select housing subtypes
  filter(Type.of.unit %in% c("Single", "Apartment")) %>%
  # Group by region and unit type
  group_by(GEO, Type.of.unit) %>%
  # Edit grouped columns
  mutate(
    # Create column for recent cumulative population change
    sum_pop_change = sum(pop_change),
    # Create column for recent cumulative completions
    sum_completions = sum(completions),
    # Create column for recent cumulative pop change to completion ratio
    recent_pop_to_completions = sum_pop_change / sum_completions) %>%
  # Ungroup data
  ungroup() %>%
  # Select single year
  filter(year(year) == 2021) %>%
  # Edit columns
  mutate(
    # Rename housing types
    Type.of.unit = mapvalues(
      Type.of.unit, from = c("Single", "Apartment"),
      to = c("Single-Detached Houses", "Apartments")), 
    # Set housing types as factors with specified order
    Type.of.unit = factor(
      Type.of.unit, levels = c("Single-Detached Houses", "Apartments")),
    # Rename housing types
    GEO = mapvalues(
      GEO, from = c("Alberta", "British Columbia", "Manitoba", "Ontario",
                    "New Brunswick", "Newfoundland and Labrador", "Nova Scotia",
                    "Quebec", "Prince Edward Island", "Saskatchewan"),
      to = c("AB", "BC", "MB", "ON", "NB", "NL", "NS", "QC", "PE", "SK")),
    # Set housing types as factors with specified order
    GEO = factor(
      GEO, levels = c("PE", "ON", "NS", "MB", "SK",
                      "NB", "BC", "AB", "QC", "NL")),
    # Replace negative values with zero
    recent_pop_to_completions = ifelse(
      recent_pop_to_completions < 0, 0, recent_pop_to_completions)) %>%
  # Plot data
  ggplot(aes(x = GEO, y = recent_pop_to_completions,
             fill = factor(Type.of.unit), )) +
  # Plot ratio of cumulative population change to housing completions as bars
  geom_bar(stat = "identity", position = "dodge") +
  # Specify colours
  scale_fill_manual(
    values = c("Single-Detached Houses" = "#BE6E46",
               "Apartments" = "#7286A0")) +
  # Remove graph title
  ggtitle("") +
  # Add caption
  labs(caption = paste("\nStatistics Canada. (2021). Population estimates,",
                       "quarterly", "\nStatistics Canada. (2021). Canada",
                       "Mortgage and Housing Corporation, housing starts,",
                       "under construction and completions, all areas,",
                       "quarterly.")) +
  # Remove x-axis title
  xlab("") +
  # Add y-axis title
  ylab("Total cumulative Population Increase per Housing Completion") +
  # Change y-axis formatting
  scale_y_continuous(limit = c(0, 11), breaks = seq(0, 10, 2)) +
  # Make legend single row
  guides(fill = guide_legend(nrow = 1)) +
  # Set graph theme
  theme_minimal() +
  # Customize theme elements
  theme(text = element_text(family = "CMU Bright", size = 16),
        axis.title.y = element_text(hjust = -0.3, vjust = 4), 
        legend.position = c(.5, 1.05), legend.title = element_blank(),
        plot.caption = element_text(size = 10, hjust = 0, vjust = 4),
        plot.margin = unit(c(0.2, 1, 1, 1.2), "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#CCCCCC"),
        plot.background = element_rect(fill = "#E6E6E6", color = NA))

# Combine two graphs
png("./images/region_pop_to_completions_recent.png", units = "in",
    width = 12, height = 10, res = 1000)
grid.arrange(graph1, graph2, ncol = 1)
dev.off()


#### CREATE VISUALATION OF POP CHANGE AND COMPLETIONS ####

# Create graph for population change
graph1 <- df_pop %>%
  # Filter data to Canada only
  filter(GEO == "Canada") %>%
  # Group data by year
  group_by(year = year(as.Date(as.yearmon(REF_DATE)))) %>%
  # Get total population change per year
  summarise_if(is.numeric, sum, na.rm = T) %>%
  # Ungroup data
  ungroup() %>%
  # Edit columns
  mutate(
    # Format reference date as date
    year = as.Date(paste0(year, "-01-01")),
    # Create column for three year rolling average population change
    pop_change_roll = rollmean(pop_change, k = 3, fill = NA),
    # Replace end values in people rolling mean with original value
    pop_change_roll = ifelse(
      is.na(pop_change_roll), pop_change, pop_change_roll)) %>%
  # Plot data
  ggplot(aes(x = year)) +
  # Plot ratio of housing completions to population change as a line
  geom_line(aes(y = pop_change_roll), lwd = 2, color = "#BE6E46") +
  # Add graph title
  labs(title = paste("Have Canadian housing completions", 
                     "kept up with population growth?"),
       subtitle = paste("Annual population change, 3-year rolling average,",
                        "(Canada, 1948-2021)\n")) +
  # Remove x-axis title
  xlab("") +
  # Remove y-axis title
  ylab("Population Change") +
  # Change x-axis formatting
  scale_x_date(date_breaks = "4 years" , date_labels = "%y",
               limit = c(as.Date("1948-01-01"), as.Date("2021-01-01"))) +
  # Change y-axis formatting
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) +
  # Add average line
  geom_hline(yintercept = 344404.5, color = "#BE6E46", linetype = 'dashed') +
  # Add annotation for average line
  annotate("text", x = as.Date("1956-01-01"), y = 328404.5, vjust = 0.2,
           label = "1948-2021 Average", family = "CMU Bright",
           color = "#BE6E46") +
  # Set graph theme
  theme_minimal() +
  # Customize theme elements
  theme(text = element_text(family = "CMU Bright", size = 16),
        plot.title = element_text(family = "CMU Bright SemiBold"),
        plot.subtitle = element_text(face = "plain"),
        axis.title.y = element_text(hjust = 0.5, vjust = 4),
        plot.margin = unit(c(1, 1, 0.2, 1.2), "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#CCCCCC"),
        plot.background = element_rect(fill = "#E6E6E6", color = NA))

# Create graph for ratio of housing completions to population change facets
graph2 <- df %>%
  # Filter data to Canada and all housing types only
  filter(GEO == "Canada" & Type.of.unit == "All" & year(year) <= 2020) %>%
  # Edit columns
  mutate(
    # Create column for three year rolling average population change
    completions_roll = rollmean(completions, k = 3, fill = NA),
    # Replace end values in people rolling mean with original value
    completions_roll = ifelse(
      is.na(completions_roll), completions, completions_roll)) %>%
  # Plot data
  ggplot(aes(x = year)) +
  # Plot ratio of housing completions to population change as a line
  geom_line(aes(y = completions_roll), lwd = 2, color = "#59594A") +
  # Remove graph title
  labs(title = "",
       subtitle = paste("Annual housing completions, 3-year rolling average",
                        "(Canada, 1948-2020)\n")) +
  # Remove x-axis title
  xlab("") +
  # Remove y-axis title
  ylab("Housing Completions") +
  # Change x-axis formatting
  scale_x_date(date_breaks = "4 years" , date_labels = "%y",
               limit = c(as.Date("1948-01-01"), as.Date("2021-01-01"))) +
  # Change y-axis formatting
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) +
  # Add average line
  geom_hline(yintercept = mean(
    df[df$GEO == "Canada" & df$Type.of.unit == "All",]$completions,
    na.rm = T), color = "#59594A", linetype = 'dashed') +
  # Add annotation for average line
  annotate("text", x = as.Date("1956-01-01"), y = 159000, vjust = 0.2,
           label = "1948-2021 Average", family = "CMU Bright") +
  # Set graph theme
  theme_minimal() +
  # Customize theme elements
  theme(text = element_text(family = "CMU Bright", size = 16),
        legend.position = c(.5, 1.05), legend.title = element_blank(),
        axis.title.y = element_text(hjust = 0.5, vjust = 4),
        plot.caption = element_text(size = 10, hjust = 0, vjust = 4),
        plot.margin = unit(c(0.2, 1, 1, 1.2), "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#CCCCCC"),
        plot.background = element_rect(fill = "#E6E6E6", color = NA))

# Combine two graphs
png("./images/pop_and_completions.png", units = "in",
    width = 12, height = 10, res = 1000)
grid.arrange(graph1, graph2, ncol = 1)
dev.off()