setwd("fifa data cleaning/")

# install.packages("data.cube", repos = paste0("https://", c(
#   "jangorecki.gitlab.io/data.cube",
#   "cloud.r-project.org"
# )))

library(tidyr)
library(tidyverse)
library(scales)
library(data.cube)
library(stringi)

FIFA1 <- read_csv("fifa21_raw_data.csv")
FIFA2 <- read_csv("fifa21 raw data v2.csv")

head(FIFA2)
fifa_cleaned <- FIFA2 |> janitor::clean_names()

## Data overview
glimpse(fifa_cleaned)

## Check for duplicate entries
fifa_cleaned[duplicated(fifa_cleaned$id),]

## Clean the club name -> remove special characters in club name and players long name
fifa_cleaned <- fifa_cleaned |> 
  mutate(club = str_replace_all(club,"\\n","") |> stri_trans_general("Latin-ASCII") |> str_trim()) |> 
  separate(contract, c("contract_start","contract_end"), " ~ ", convert = TRUE) |> 
  mutate(long_name = stri_trans_general(long_name, "Latin-ASCII") |> str_trim())

## Clean dates that aren't standardized to years -> also add contract_type column
fifa_cleaned <- fifa_cleaned |> 
  separate(
    contract_start , c("contract_start","contract_type"), " On ", convert = TRUE
  ) |> 
  mutate(
    contract_type = ifelse(
      str_detect(contract_start, "Free"), contract_start, contract_type )
    ) |> 
  mutate(
    contract_type = ifelse(
      is.na(contract_type), "Contract", contract_type)
    ) |> 
  mutate(
    contract_start = ifelse(
      str_detect(contract_start, ","), lubridate::year(as.Date(contract_start, "%b %d, %Y")), as.numeric(contract_start))
    ) |> 
  mutate(
    contract_end = ifelse(is.na(contract_end) & contract_type == "Loan", lubridate::year(as.Date(loan_date_end, "%b %d, %Y")), contract_end))


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
  mutate(height = str_replace_all(height,"cm","") |> as.numeric(height)) |> 
  mutate(weight = str_replace_all(weight, "kg","") |> as.numeric(weight)) |> 
  as.data.frame()


## Clean column money columns -> function + case_when
clean_currency <- function(amount){
  numeric_val <- as.numeric(gsub("[^0-9.-]", "", amount))
  standard <- gsub("[^A-Z]","",amount)
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

## Add the currency to wage, value and release_clause if necessary 
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
fifa_cleaned4 <- fifa_cleaned3 |> 
  mutate(ir = remove_stars(ir)) |> 
  mutate(w_f = remove_stars(w_f)) |> 
  mutate(sm = remove_stars(sm))

## Cleaning and standardizing the date columns -> Joined and loan_date_end
fifa_cleaned5 <- fifa_cleaned4 |> 
  mutate(joined = as.Date(joined, "%B %d, %Y")) |> 
  mutate(loan_date_end = as.Date(loan_date_end, "%B %d, %Y"))

## Convert ratings to percentage -> OVA, BOV, POT
fifa_cleaned6 <- fifa_cleaned5 |> 
  mutate(ova = percent(ova/100, suffix = "%")) |> 
  mutate(bov = percent(bov/100, suffix = "%")) |> 
  mutate(pot = percent(pot/100, suffix = "%"))

## cleaning the hits column
## We can use the clean_currency function
fifa_cleaned7 <- fifa_cleaned6 |> 
  mutate(hits = clean_currency(hits))


glimpse(fifa_cleaned7)

## Drop irrelevant columns
fifa_cleaned8 <- fifa_cleaned7 |> 
  select(-c("positions")) |> 
  select(id, long_name, name, nationality, club, contract_start, contract_end, contract_type, everything(), player_url, photo_url)

openxlsx::write.xlsx(x = fifa_cleaned8, file = "fifa2.xlsx")
