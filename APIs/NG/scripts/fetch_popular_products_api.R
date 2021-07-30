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

#* @get /fetch-popular-products

#* @post /fetch-popular-products

#* @param recomm_limit Number of desired products per business line.

#* @apiDescription This API the most popular products.


# API function ====
function(recomm_limit){
  
  popular_products <-
    left_join(x = tbl(src = con_ng, "popular_products"),
              y = tbl(src = con_ng, "product_values"),
              by = "PRODUCT") %>% 
    group_by(BUSINESS_LINE) %>% 
    collect() %>% 
    slice(1:recomm_limit) %>% 
    rename(PURCHASES = n) %>% 
    select(-accounts)
  
  return(popular_products)
}