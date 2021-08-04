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

#* @get /fetch-top-customers

#* @post /fetch-top-customers

#* @param ownership The business lines that owns the customer separated by comma.

#* @param intermediated Whether or not the customer is intermediated (Yes/No).

#* @param min_value Minimum product value.

#* @param max_value Maximum product value.

#* @param recomm_limit Number of desired products per business line.

#* @apiDescription This API returns a list of customers most likely to purchase a product based on chosen criteria.


# API function ====
function(ownership, intermediated, min_value, max_value, recomm_limit){
  
  top_customers <-
    tbl(src = con_ng, "recommendations_detailed") %>%
    group_by(ACCOUNT_NO, BUSINESS_LINE) %>% 
    filter(intermediated == !!intermediated) %>% 
    filter(ownership == !!ownership) %>% 
    filter(product_value >= min_value) %>% 
    filter(product_value <= max_value) %>% 
    collect() %>% 
    slice(1:recomm_limit)
  
  return(top_customers)
}