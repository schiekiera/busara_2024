# remove data from working space
rm(list = ls())

# libraries
library(tidyverse)
library(report)
library(svglite)

##############
# read data  #
##############
url <- "https://raw.githubusercontent.com/schiekiera/busara_2024/refs/heads/main/data/busara_2024_data.csv"
data <- read.csv(url)

##############################
##                          ##
## 1. Global South Analysis ##
##                          ##
##############################

# Descriptive statistics: Count instances of 'is_global_south' categories in 'data'.
table(data$is_global_south)

# Data aggregation and proportion calculation:
# Group data by 'publication_year' and 'is_global_south', then calculate the count of records and proportions within groups.
data_by_year_global_south <- data %>%
  group_by(publication_year, is_global_south) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count, na.rm = TRUE)) %>%
  mutate(proportion = count / total)


# Function to filter data for specified global south category and year range, and calculate mean and standard deviation of proportions.
# Args:
#   data_by_year_global_south: A dataframe containing publication years, category flags, and proportions.
#   is_global_south: Category flag to filter the data on ('all_gn', 'all_gs', or 'mix_gn_gs').
#   year_start: Starting year of the period for analysis.
#   year_end: Ending year of the period for analysis.
# Returns:
#   A string representation of mean and standard deviation of the 'proportion' for the specified category and period.
filter_mean_and_sd <-
  function(data_by_year_global_south,
           is_global_south,
           year_start,
           year_end) {
    data_by_year_global_south <-
      data_by_year_global_south[data_by_year_global_south$is_global_south == is_global_south, ]
    data_by_year_global_south <-
      data_by_year_global_south[data_by_year_global_south$publication_year >= year_start &
                            data_by_year_global_south$publication_year <= year_end, ]
    data_by_year_global_south$proportion <-
      round(data_by_year_global_south$proportion, 2)
    mean <- round(mean(data_by_year_global_south$proportion), 2)
    sd <- round(sd(data_by_year_global_south$proportion), 2)
    m_sd <- cat("(M =", mean, ", SD = ", sd, ") \n")
    return(m_sd)
  }


# Retrieve names of the 'is_global_south' categories.
names(table(data$is_global_south))

# Define categories and decades for analysis.
is_global_souths <- c("all_gn" ,   "all_gs" ,   "mix_gn_gs")
decades <- list(c(1990,1999), c(2000,2009), c(2010,2019), c(2020,2022))

# Loop through each category and decade, print the results of mean and standard deviation calculations.
for(is_global_south in is_global_souths) {
  for(decade in decades) {
    print(paste("is_global_south:", is_global_south, "Decade:", decade[1], "-", decade[2]))
    filter_mean_and_sd(data_by_year_global_south, is_global_south, decade[1], decade[2])
  }
}

# Summarize citation counts by 'is_global_south' and 'publication_year'.
# Calculates mean, standard deviation, and count of citations.
df_gs_summarized <- data %>%
  group_by(publication_year,is_global_south) %>%
  summarise(mean_cited_by_count = mean(cited_by_count, na.rm = TRUE),
            sd_cited_by_count = sd(cited_by_count, na.rm = TRUE),
            n = n())


###########
## ggplot #
###########

###################################################################
# Relative Representation of Global South in Academic Affiliation #
###################################################################

# expression
subtitle<-expression(paste(italic("n"), " = 17,095 First Authorships from 1990 to 2022"))

# give labels other names
ggplot(data_by_year_global_south, aes(x = publication_year, y = proportion, color = is_global_south, group = is_global_south)) +
  geom_line() +
  theme_minimal() +
  labs(
    #title = "Proportion of Academic Affiliation in Psychotherapy RCTs by World Region Over Time",
    #subtitle = subtitle,
    x = "Publication Year",
    y = "Proportion") +
  scale_color_discrete(name = "UNCTAD: Academic Affiliation",labels = c("Institutions in Global North",
                                                                        "Institutions in Global South",
                                                                        "Institutions in Global South and Global North")) + # make legend at bottom
  theme(legend.position = "bottom") # make legend at bottom

# Additionally, saving the plot as a PDF & jpg
#ggsave("Plots/UNCTAD_plot_region.jpg", width = 10, height = 6, dpi = 300)
#ggsave("Plots/UNCTAD_plot_region.pdf", width = 10, height = 6)
#ggsave("Plots/UNCTAD_plot_region.svg", width = 10, height = 6)


####################################################
# Cited by Count as a Function of Publication Year #
##################ä#################################
ggplot(df_gs_summarized, aes(x = publication_year, y = mean_cited_by_count, color = is_global_south, group = is_global_south)) +
  geom_line() +
  #geom_ribbon(aes(ymin = mean_cited_by_count - sd_cited_by_count, ymax = mean_cited_by_count + sd_cited_by_count, fill = is_global_south), alpha = 0.2) +
  theme_minimal() +
  labs(
    #title = "Proportion of Academic Affiliation in Psychotherapy RCTs by World Region Over Time",
    #subtitle = subtitle,
    x = "Publication Year",
    y = "Mean Cited by Count") +
  scale_color_discrete(name = "UNCTAD: Academic Affiliation",labels = c("Institutions in Global North",
                                                                        "Institutions in Global South",
                                                                        "Institutions in Global South and Global North")) + # make legend at bottom
  theme(legend.position = "bottom") # make legend at bottom



######################
##                  ##
## 2. World region  ##
##                  ##
######################

# Calculate the count and proportion of publications by year and region.
# This section groups the dataset by 'publication_year' and 'region', calculates the count of records for each group,
# and then computes the proportion of each count relative to the total publications in that year.
data_by_year_region <- data %>%
  group_by(publication_year, region) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count, na.rm = TRUE)) %>%
  mutate(proportion = count / total)

# Define a function to filter data by region and specified year range, then calculate the mean and standard deviation of the proportion.
# Args:
#   data_by_year_region: Data frame containing yearly data grouped by region.
#   region: The specific region to filter by.
#   year_start: The beginning year of the time range for analysis.
#   year_end: The end year of the time range for analysis.
# Returns:
#   A string output with the calculated mean and standard deviation for the proportion of publications in the specified range.
filter_mean_and_sd <-
  function(data_by_year_region,
           region,
           year_start,
           year_end) {
    data_by_year_region <-
      data_by_year_region[data_by_year_region$region == region, ]
    data_by_year_region <-
      data_by_year_region[data_by_year_region$publication_year >= year_start &
                            data_by_year_region$publication_year <= year_end, ]
    data_by_year_region$proportion <-
      round(data_by_year_region$proportion, 2)
    mean <- round(mean(data_by_year_region$proportion), 2)
    sd <- round(sd(data_by_year_region$proportion), 2)
    m_sd <- cat("(M =", mean, ", SD = ", sd, ") \n")
    return(m_sd)
  }


# List of world regions for analysis.
regions <- c("North America", "Europe", "Asia", "Latin America and the Caribbean", "Africa", "Oceania")

# Time ranges to be analyzed, segmented into decades.
decades <- list(c(1990,1999), c(2000,2009), c(2010,2019), c(2020,2022))

# Iterate through each region and decade, applying the filter_mean_and_sd function and printing the results.
for(region in regions) {
  for(decade in decades) {
    print(paste("Region:", region, "Decade:", decade[1], "-", decade[2]))
    filter_mean_and_sd(data_by_year_region, region, decade[1], decade[2])
  }
}

###############################
# Further Count data analyses #
###############################

# Step 1: Count of Publications Per Region and Year
count_data <- data %>%
  group_by(publication_year, region) %>%
  summarise(count = n(), .groups = 'drop')

# Step 2: Total Publications Per Year
total_per_year <- data %>%
  group_by(publication_year) %>%
  summarise(total = n(), .groups = 'drop')

# Step 3: Join and Calculate Proportion
avg_data_by_year <- count_data %>%
  left_join(total_per_year, by = "publication_year") %>%
  mutate(percent = count / total *100,
         proportion = count / total)


############
## ggplot ##
############

# expression
subtitle<-expression(paste(italic("n"), " = 17,095 First Authorships from 1990 to 2022"))

ggplot(data_by_year_region, aes(x = publication_year, y = proportion, color = region, group = region)) +
  geom_line() +
  theme_minimal() +
  labs(
        #title = "Proportion of Academic Affiliation in Psychotherapy RCTs by World Region Over Time",
       #subtitle = subtitle,
       x = "Publication Year",
       y = "Proportion") +
  scale_color_discrete(name = "World Region") 
 # add legend title

# save plot to disk
#ggsave("Plots/plot_region.jpg", width = 10, height = 6, dpi = 300)
#ggsave("Plots/plot_region.pdf", width = 10, height = 6)
#ggsave("Plots/plot_region.svg", width = 10, height = 6)

