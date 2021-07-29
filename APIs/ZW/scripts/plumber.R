library(httr)
library(jsonlite)
library(recommenderlab)
library(DBI)
library(RSQLite)
library(tidyverse)

# Database connections ====
zw_sqlite_path <- "../../App/uap_cross_sell/db/zw_cs.db"
con_zw <- dbConnect(drv = SQLite(), zw_sqlite_path)

# Load required objects ====
zw_rec_model <- readRDS(file = "../../App/uap_cross_sell/objects/models/Zimbabwe.rds")
zw_products_rating_matrix <- readRDS(file = "../../App/uap_cross_sell/objects/rating_matrix/Zimbabwe.rds")


#* @apiTitle Cross-selling Tool

#* @get /predict-on-accounts

#* @param account_num Customers' account numbers separated by a comma..

#* @param recomm_limit Number of desired products per business line.

#* @apiDescription This API serves product recommendations for cross selling purposes when provided with a customer's account number.


function(account_num, recomm_limit){
  
  table_order <- c("UNIQUE_ID", "PRODUCT", "BUSINESS_LINE", "rating", "max_rating", "ownership", "intermediated", "product_value", "customer_details")
  
  # Clean & vectorize input values
  input_account_num <- str_remove_all(string = account_num, 
                                      pattern = " ")
  input_account_num <- str_split(string = input_account_num, 
                                 pattern = ",")[[1]]
  
  # Retrieve unique identifiers
  res_tbl <- tibble()
  
  for(account in seq_along(input_account_num)){
    
    query_stmt <-
      paste0("SELECT * FROM uniqueid_accounts WHERE ACCOUNT_NO LIKE '%",input_account_num[account],"%'") %>% 
      SQL()
    
    query_result <- 
      dbSendStatement(conn = con_zw, 
                      statement = query_stmt)
    
    res_tbl <- rbind(res_tbl, dbFetch(res = query_result))
    dbClearResult(res = query_result)
  }
  
  cust_unique_ids <- res_tbl %>% pull(UNIQUE_ID)
  
  # Predict & merge
  user_account_recommendations <-
    predict(object = zw_rec_model, 
            newdata = zw_products_rating_matrix[cust_unique_ids,], 
            type = "ratings") %>% 
    as("data.frame") %>% 
    rename(UNIQUE_ID = user, PRODUCT = item) %>% 
    left_join(tbl(src = con_zw, "product_businesslines"), by = "PRODUCT", copy = TRUE) %>% 
    left_join(tbl(src = con_zw, "uniqueid_accounts"), by = "UNIQUE_ID", copy = TRUE) %>% 
    left_join(tbl(src = con_zw, "product_values"), by = "PRODUCT", copy = TRUE) %>% 
    left_join(tbl(src = con_zw, "customer_details_final"), by = "UNIQUE_ID", copy = TRUE) %>% 
    select(any_of(c("ACCOUNT_NO",table_order))) %>% 
    select(-UNIQUE_ID) %>% 
    group_by(ACCOUNT_NO, BUSINESS_LINE) %>% 
    mutate(max_rating = max(rating)) %>% 
    relocate(max_rating, .after = rating) %>% 
    arrange(desc(rating)) %>% 
    slice(1:recomm_limit)
  
  return(user_account_recommendations)
}