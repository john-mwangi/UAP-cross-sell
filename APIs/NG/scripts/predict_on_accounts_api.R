library(httr)
library(jsonlite)
library(recommenderlab)
library(DBI)
library(RSQLite)
library(tidyverse)

# Database connections ====
ng_sqlite_path <- "../../../App/uap_cross_sell/db/ng_cs.db"
con_ng <- dbConnect(drv = SQLite(), ng_sqlite_path)

# Load required objects ====
ng_rec_model <- readRDS(file = "../../../App/uap_cross_sell/objects/models/Nigeria.rds")

ROUND_OFF <- 3

# API description ====

#* @apiTitle Cross-selling Tool

#* @get /predict-on-accounts

#* @post /predict-on-accounts

#* @param account_num Customers' account numbers separated by a comma.

#* @param recomm_limit Number of desired products per business line.

#* @apiDescription This API serves product recommendations for cross selling purposes when provided with a customer's account number.


# API function ====
function(account_num, recomm_limit){
  
  account_num <- str_remove_all(string = account_num, pattern = " ")
  records <- strsplit(x = account_num, split = ",")[[1]]
  
  chosen_customers <-
    tbl(src = con_ng, "recommendations_detailed") %>% 
    filter(ACCOUNT_NO %in% records) %>%
    group_by(ACCOUNT_NO, BUSINESS_LINE) %>% 
    arrange(desc(rating)) %>% 
    collect() %>% 
    slice(1:recomm_limit) %>% 
    arrange(desc(max_rating)) %>% 
    mutate(max_rating = round(max_rating, ROUND_OFF),
           rating = round(rating, ROUND_OFF))
  
  return(chosen_customers)
}