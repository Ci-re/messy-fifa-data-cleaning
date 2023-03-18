setwd("fifa data cleaning/")

# install.packages("janitor")
# install.packages("tidyr") 
# install.packages("plyr")
# install.packages("scales")
# install.packages("data.cube")
# install.packages("data.cube", repos = paste0("https://", c(
#   "jangorecki.gitlab.io/data.cube",
#   "cloud.r-project.org"
# )))

library(readxl)
library(tidyr)
library(tidyverse)
library(plyr)
library(scales)
library(data.cube)

FIFA1 <- read_csv("fifa21_raw_data.csv")
FIFA2 <- read_csv("fifa21 raw data v2.csv")

head(FIFA2)
fifa_cleaned <- FIFA2 |> janitor::clean_names()
colnames(fifa_cleaned)

## Data overview
glimpse(fifa_cleaned)

fifa_cleaned <- fifa_cleaned |> 
  mutate(club = str_replace_all(club,"\\n","")) |> 
  separate(contract, c("contract_start","contract_end"), " ~ ", convert = TRUE)

fifa_cleaned <- fifa_cleaned |> 
  separate(contract_start , c("contract_start","contract_status"), " On ", convert = TRUE) |> 
  mutate(contract_status = ifelse(str_detect(contract_start, "Free"), contract_start, contract_status )) |> 
  mutate(contract_status = ifelse(is.na(contract_status), "Contract", contract_status)) |> 
  mutate(contract_start = ifelse(str_detect(contract_start, ","), lubridate::year(as.Date(contract_start, "%b %d, %Y")), as.numeric(contract_start)))

## Clean dates that aren't standardized to years
c <- fifa_cleaned |> mutate(
  contract_date = lubridate::year(as.Date(contract_start, "%b %d, %Y"))
)

c$contract_date
fifa_cleaned$contract_start
fifa_cleaned$contract_status
head(fifa_cleaned)

## Clean the weight column -> function
weight_cleaner <- function(weight){

  weight_in_lbs <- weight |> 
    str_trim() |> 
    str_extract_all("[0-9]+") |> 
    unlist()
  
  weight_in_kg <- round(as.numeric(weight_in_lbs) * 0.453592, 0)
  return(paste0(weight_in_kg,"kg"))
}

## Clean the height column -> function
height_cleaner <- function(height){
  
  height_in_inches <- height |> 
    str_trim() |> 
    str_extract_all("[0-9]'[0-9]*") |> 
    unlist() |> 
    str_split("'")
  
  height_in_cm <- round((as.numeric(height_in_inches[[1]][1]) * 30.48) + (as.numeric(height_in_inches[[1]][2]) * 2.54), 0)
  return(paste0(height_in_cm,"cm"))
}

## Implement both functions -> height_cleaner and weight_cleaner
fifa_cleaned2 <- fifa_cleaned |> 
  rowwise() |> 
  mutate(
    weight = ifelse(str_detect(weight, "kg"),weight, weight_cleaner(weight))
  ) |> 
  mutate(
    height = ifelse(str_detect(height, "cm"),height, height_cleaner(height))
  )

## Convert values in both height and weight column to numeric -> remove all characters
fifa_cleaned2 <- fifa_cleaned2 |> 
  mutate(height = str_replace_all(height,"cm","")) |> 
  mutate(height = as.numeric(height)) |> 
  mutate(weight = str_replace_all(weight, "kg","")) |> 
  mutate(weight = as.numeric(weight)) |> 
  as.data.frame()

## Clean  column money columns -> function + case_when
clean_currency <- function(currency_list){
  numeric_val <- as.numeric(gsub("[^0-9.-]", "", currency_list))
  standard <- gsub("[^A-Z]","",currency_list)
  standard <- case_when(
    standard == "M" ~ 1000000,
    standard == "K" ~ 1000,
    TRUE ~ 1
  )
  cleaned_value <- numeric_val*standard
  return(cleaned_value)
}

## Implement clean_currency function in the columns -> value, wage, release_clause
fifa_cleaned3 <- fifa_cleaned2 |> 
  mutate(value = clean_currency(value)) |> 
  mutate(wage = clean_currency(wage)) |> 
  mutate(release_clause = clean_currency(release_clause))

## Add the currency to 
fifa_cleaned3 <- fifa_cleaned3 |> 
  mutate(value = currency.format(value, currency.sym = "€")) |> 
  mutate(wage = currency.format(wage, currency.sym = "€")) |> 
  mutate(release_clause = currency.format(release_clause, currency.sym = "€"))

## Remove stars symbol from rating columns -> function
remove_stars <- function(char_list){
  numeric_val <- as.numeric(gsub("[^0-9.-]", "", char_list))
  return(numeric_val)
}

## Implement remove_stars function for columns -> ir, w_f, and sm
fifa_cleaned5 <- fifa_cleaned3 |> 
  mutate(ir = remove_stars(ir)) |> 
  mutate(w_f = remove_stars(w_f)) |> 
  mutate(sm = remove_stars(sm))

## Cleaning and standardizing the date columns -> Joined and loan_date_end
fifa_cleaned6 <- fifa_cleaned5 |> 
  mutate(joined = as.Date(joined, "%B %d, %Y")) |> 
  mutate(loan_date_end = as.Date(loan_date_end, "%B %d, %Y"))

## Convert ratings to percentage -> OVA, BOV, POT
fifa_cleaned7 <- fifa_cleaned6 |> 
  mutate(ova = percent(ova/100, suffix = "%")) |> 
  mutate(bov = percent(bov/100, suffix = "%")) |> 
  mutate(pot = percent(pot/100, suffix = "%"))

## cleaning the hits column
## We can use the clean_currency function
fifa_cleaned8 <- fifa_cleaned7 |> 
  mutate(hits = clean_currency(hits))


glimpse(fifa_cleaned8)
class(fifa_cleaned8$wage)
fifa_cleaned8
