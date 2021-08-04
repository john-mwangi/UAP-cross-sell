library(httr)
library(jsonlite)
library(recommenderlab)
library(DBI)
library(RSQLite)
library(tidyverse)

# Database connections ====
ng_sqlite_path <- "../../../App/uap_cross_sell/db/ng_cs.db"
con_ng <- dbConnect(drv = SQLite(), ng_sqlite_path)

supp_sqlite_path <- "../../../App/uap_cross_sell/db/support.db"
con_supp <- dbConnect(drv = SQLite(), supp_sqlite_path)

# Load required objects ====
ng_rec_model <- readRDS(file = "../../../App/uap_cross_sell/objects/models/Nigeria.rds")

ROUND_OFF <- 3

# API description ====

#* @apiTitle Cross-selling Tool

#* @get /predict-on-choices

#* @post /predict-on-choices

#* @param user_choices Products selected by the customer.

#* @param recomm_limit Number of desired products per business line.

#* @apiDescription This API returns additional products on top of what a customer has purchased or shown interest in.


# API function ====
function(user_choices, recomm_limit){
  
  #Products not selected
  not_user_choices <-
    tbl(src = con_supp, "support") %>% 
    filter(country=="Nigeria") %>% 
    filter(!products %in% user_choices & !is.na(products)) %>%
    collect() %>% 
    pull(products)
  
  
  #Convert to a table
  choices <- 
    tibble(choice = rep(x = c(1,0), 
                        times = c(length(user_choices), 
                                  length(not_user_choices))
    ))
  
  user_choices_df <-
    choices %>% 
    mutate(user_choices = c(user_choices, not_user_choices)) %>% 
    distinct(user_choices, .keep_all = TRUE)
  
  #Convert to wide format
  user_choices_df <- 
    pivot_wider(data = user_choices_df,
                names_from = user_choices, 
                values_from = choice)
  
  #Convert to binaryRatingMatrix
  user_choices_mat <- as.matrix(user_choices_df)
  user_choices_ratmat <- as(user_choices_mat,"binaryRatingMatrix")
  
  #Predict using model
  user_choice_recommendations <-
    tryCatch(expr = predict(object = ng_rec_model, 
                            newdata = user_choices_ratmat, 
                            type="ratings"),
             error = function(e){
               stop("Ensure the products specified match the selected country.")
             })
  
  #Convert prediction to dataframe
  user_choice_recommendations_df <-
    as(user_choice_recommendations, "data.frame") %>% 
    rename(PRODUCT = item) %>%
    left_join(tbl(src = con_ng, "product_businesslines"),
              by = "PRODUCT", 
              copy = TRUE) %>% 
    left_join(tbl(src = con_ng, "product_values"),
              by = "PRODUCT",
              copy = TRUE) %>% 
    relocate(BUSINESS_LINE, .after = PRODUCT) %>% 
    group_by(BUSINESS_LINE) %>% 
    arrange(desc(rating), .by_group = TRUE) %>% 
    slice(1:recomm_limit) %>% 
    select(-accounts) %>% 
    mutate(rating = round(rating, ROUND_OFF))
  
  return(user_choice_recommendations_df)
}