# imports - not base R, require installation
library(dplyr)
library(tidyverse)
library(lubridate)
library(readr)

# from defined starting date (week 0), calculate each date given in raw data as week value
aggregate_week <- function(data, age_group, sex, date, start_date){
  data$sexfactor <- as.factor(sex)
  data$agefactor <- as.factor(age_group)

  data$week <- floor(as.numeric(difftime(date, start_date, units = "weeks")))
  
  return(data)
}

# subset all cases and deaths based on raw data, return list of two tibbles
# first tibble contains only cases, second contains only deaths
separate_cases_deaths <- function(data, deaths){
  death_data <- data[deaths == 1, ]
  case_data <- data[deaths == 0, ]
  return(list(cases = case_data, deaths = death_data))
}

# subset data into male and female gender, returning list of two tibbles
# first tibble is the first factor level (usually female); second is second factor level (usually male)
# depending on the factor levels in your data, change the naming of the returned list to reflect your levels
# function currently limited to 2 factor levels reflecting biological sex
separate_gender <- function(data, gender){
  fct <- levels(gender)
  if (length(fct) > 2){
    stop("Too many factor levels. Restructure biologically to male and female only.")
  }
  cat(sprintf("First level of gender factor is: %s\n", fct[1]))
  
  first_gender <- data[gender == fct[1], ]
  second_gender <- data[gender == fct[2], ]
  return(list(female = first_gender, male = second_gender))
}

# aggregate cases based on week and age bins
# input intended to be already stratified by sex status and death status (see the two functions immediately above)
# ie., if looking to find female deaths, input should only contain "Female" in gender column and "1" or "True" in deaths column
# IMPT: change sum(cases) depending on your raw data's column name for COVID-19 cases
# ex., if column name in raw data is "Cases", rename to cases = sum(Cases)
# output is a tibble with only the week, age groups, and number of COVID-19 cases in that age group for the week
aggregate <- function(data){
  data <- data %>%
    group_by(week, agefactor) %>%
    summarise(cases = sum(cases))
  return(data)
}

### EXAMPLE USE ###

# sample use file attached in data with the name "covid_de.csv"
# change path to your file location on local machine
stats <- read_csv("C:\\Users\\...\\covid_de.csv", col_types=cols(`date` = col_datetime(format = "%Y-%m-%d")))
head(stats)

# drop NA values from data
stats <- stats[!is.na(stats$gender),]
stats <- stats[!is.na(stats$age_group),]
stats <- stats[!is.na(stats$deaths),]

# calculate weeks from sample reference date 2020-01-28 which COVID-19 case/death was documented
weekly_stats <- aggregate_week(stats, stats$age_group, stats$gender, stats$date, ymd("2020-01-28"))

# separate cases and deaths from the data into their own data frames
weekly_cases_deaths <- separate_cases_deaths(weekly_stats, weekly_stats$deaths)

# calculate COVID-19 cases for each gender
cases <- weekly_cases_deaths$cases
gender_case_data <- separate_gender(cases, as.factor(cases$gender))

# calculate COVID-19 cases in each week by age groups
aggregated_weekly_stats <- aggregate(gender_case_data$female)

# write data into CSV file to view
write.csv(aggregated_weekly_stats, "C:\\Users\\...\\example.csv", row.names = TRUE)