# Packages 
library(purrr) # used for merging isco(s) df into one file
library(readxl) # used for reading the excel file 
library(stringr) # used for working with strings 
library(dplyr) # used for combining the data
library(Hmisc) # used for statistic calculations 

# Sets the path to the parent directory of RR classes
setwd("/Users/yuriisemenov/Documents/GitHub/RRcourse2023/6. Coding and documentation/Data")

task_data = read.csv("onet_tasks.csv") # reading the csv file

isco1 <- read_excel("Eurostat_employment_isco.xlsx", sheet="ISCO1")
isco2 <- read_excel("Eurostat_employment_isco.xlsx", sheet="ISCO2")
isco3 <- read_excel("Eurostat_employment_isco.xlsx", sheet="ISCO3")
isco4 <- read_excel("Eurostat_employment_isco.xlsx", sheet="ISCO4")
isco5 <- read_excel("Eurostat_employment_isco.xlsx", sheet="ISCO5")
isco6 <- read_excel("Eurostat_employment_isco.xlsx", sheet="ISCO6")
isco7 <- read_excel("Eurostat_employment_isco.xlsx", sheet="ISCO7")
isco8 <- read_excel("Eurostat_employment_isco.xlsx", sheet="ISCO8")
isco9 <- read_excel("Eurostat_employment_isco.xlsx", sheet="ISCO9")

# Define what country do you want to check
country <- "Sweden"

# This function calculates worker totals in each of the chosen countries.

total_isco_by_country <- function(country) {
  total <- 0
  for (i in 1:9) {
    col_name <- paste0("isco", i, "$", country)
    total <- total + eval(parse(text=col_name))
  }
  return(total)
}

totals <- total_isco_by_country(country)
print(totals)

# Create a list of all the data frames
isco_list <- list(isco1, isco2, isco3, isco4, isco5, isco6, isco7, isco8, isco9)

# Merge all the data frames
all_data_1 <- reduce(isco_list, rbind)


# Function that calculates totals for the country totals 
calculate_totals <- function(country_totals, num_repeats = 9) {
  rep(totals, num_repeats)
}

all_data_1$totals <- calculate_totals(totals)

# Function that calculates shares for the country totals 
calculate_share <- function(country, data) {
    share <- data[[country]] / data$totals
  return(share)
}

all_data_1$shares <- calculate_share(country = country, data = all_data_1)

# Now let's look at the task data. We want the first digit of the ISCO variable only

task_data$isco08_1dig <- str_sub(task_data$isco08, 1, 1) %>% as.numeric()

# And we'll calculate the mean task values at a 1-digit level 
# (more on what these tasks are below)

aggdata_1 <-aggregate(task_data, by=list(task_data$isco08_1dig),
                    FUN=mean, na.rm=TRUE)
aggdata$isco08 <- NULL

combined_1 <- left_join(all_data_1, aggdata_1, by = c("ISCO" = "isco08_1dig"))


general <- function(combined_1){
  
  temp_mean_a <- wtd.mean(combined_1$t_4A2a4, combined_1$shares)
  temp_sd_a <- wtd.var(combined_1$t_4A2a4, combined_1$shares) %>% sqrt()
  std_country_t_4A2a4 = (combined_1$t_4A2a4-temp_mean_a)/temp_sd_a
  
  temp_mean_b <- wtd.mean(combined_1$t_4A2b2, combined_1$shares)
  temp_sd_b <- wtd.var(combined_1$t_4A2b2, combined_1$shares) %>% sqrt()
  std_country_t_4A2b2 = (combined_1$t_4A2b2-temp_mean_b)/temp_sd_b
  
  temp_mean_c <- wtd.mean(combined_1$t_4A4a1 , combined_1$shares)
  temp_sd_c <- wtd.var(combined_1$t_4A4a1 , combined_1$shares) %>% sqrt()
  std_country_4A4a1  = (combined_1$t_4A4a1 - temp_mean_c)/temp_sd_c
  country_NRCA <- std_country_t_4A2a4 + std_country_t_4A2b2 + std_country_4A4a1
  
  temp_mean_d <- wtd.mean(combined_1$NRCA, combined_1$shares)
  temp_sd_d <- wtd.var(combined_1$NRCA, combined_1$shares) %>% sqrt()
  country_NRCA_adj = (combined_1$NRCA-temp_mean_d)/temp_sd_d
  
  multip_NRCA <- (combined_1$NRCA_adjusted*combined_1$shares)
  
  agg_country <-aggregate(combined_1$NRCA_multiple, by=list(combined_1$TIME),
                          FUN=sum, na.rm=TRUE)
  
  combined_1$std_t_4A2a4 <- std_country_t_4A2a4
  combined_1$std_4A2b2 <- std_country_t_4A2b2
  combined_1$std_4A4a1 <- std_country_4A4a1
  combined_1$NRCA <- country_NRCA
  combined_1$NRCA_adjusted <- country_NRCA_adj
  combined_1$NRCA_multiple <- multip_NRCA
  agg_country <- agg_country
 
  return(list(combined_1=combined_1, agg_country = agg_country))
  
}

combined_1 = general(combined_1)





