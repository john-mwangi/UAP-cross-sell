#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

load("Showcase - dummy.RData")

library(shiny)
library(recommenderlab)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
dfScenario1 = reactive({
        if(input$submit>0){
            
            #Get customer ids
            req_user_id <- c(input$customer_ids)
            
            records <- c()
            for(id in seq_along(req_user_id)){
                records <- append(records,paste0("^",req_user_id,"$"))
            }
            
            records <- unique(records)
            
            rec_nums <- which(grepl(x = rownames(uap_dummy_data),
                                    pattern = paste0(records, collapse = "|")))
            
            #Obtain recommendations
            pred_ratings <- predict(ke_pop, uap_dummy_data[rec_nums], type="ratings")
            
            #Format into dataframe
            pred_ratings_list <- as(pred_ratings,"list")
            
            item_name_vec <- c()
            for(user in seq_along(pred_ratings_list)){
                item_names <- names(pred_ratings_list[[user]])
                item_name_vec <- append(item_name_vec, item_names)
            }
            
            preds_all <- pred_ratings_list %>% 
                tibble(user_id = names(.)) %>% 
                unnest(cols = ".") %>% 
                mutate(item = item_name_vec) %>% 
                rename(score = ".") %>% 
                mutate(score = round(score,3)) %>% 
                left_join(uap_products, by = c("item"="product")) %>% 
                select(user_id,item,business_line,score) %>% 
                arrange(desc(score)) %>% 
                group_by(user_id, business_line) %>% 
                slice(1:input$recomm_limit) %>% 
                rename(customer_num = user_id,
                       product = item)
            
            return(list(recommendations=preds_all))
            
        }
    })


dfScenario3 = reactive({
    
    if(input$submit>0){
    #Capture user selections
    user_choices <- c(input$chosen_products)
    
    #Products not selected
    not_user_choices <- colnames(uap_dummy_data)[!colnames(uap_dummy_data) %in% user_choices]
    
    #Collect choices and selections into a table
    choices <- tibble(choice = 
                          rep(x = c(1,0), 
                              times = c(length(user_choices), 
                                        length(not_user_choices))
                          ))
    
    user_choices_df <-
        choices %>% 
        mutate(user_choices = c(user_choices, not_user_choices))
    
    #One-hot encode user choices
    user_choices_df <- 
        pivot_wider(data = user_choices_df,
                    names_from = user_choices, 
                    values_from = choice)
    
    #Convert to rating matrix
    user_choices_mat <- as.matrix(user_choices_df)
    
    colnames(user_choices_mat)[1:10]
    
    user_choices_ratmat <- as(user_choices_mat,"binaryRatingMatrix")
    
    #Recommend products
    pre_opt3 <- predict(object = ke_ubcf, 
                        newdata = user_choices_ratmat, 
                        type="ratings")
    
    #Convert recommendations to dataframe
    pre_opt3_df <- 
    as(pre_opt3, "list") %>% 
        data.frame() %>% 
        rename(score = X1) %>% 
        mutate(score = round(score,3)) %>% 
        rownames_to_column("item") %>% 
        left_join(uap_products, by = c("item"="product")) %>% 
        group_by(business_line) %>% 
        arrange(desc(score), .by_group = TRUE) %>% 
        slice(1:input$recomm_limit) %>% 
        rename(product = item)
    
    return(list(recommendations=pre_opt3_df))
    }
})


#Scenario 1 output
output$customer_recomms <- DT::renderDataTable(rownames = FALSE, {
    dfScenario1()$recommendations
})

#Scenario 2 output
output$target_list <- renderDataTable(rownames = FALSE,
                                      
                                      all_cust_recs_df %>% 
                                          group_by(user_id, business_line) %>% 
                                          arrange(desc(score)) %>% 
                                          mutate(score = round(score,3)) %>% 
                                          slice(1:input$recomm_limit) %>% 
                                          ungroup() %>% 
                                          group_by(user_id) %>% 
                                          mutate(max_score = max(score)) %>%  
                                          ungroup() %>% 
                                          group_by(user_id, max_score) %>% 
                                          arrange(desc(max_score)) %>% 
                                          ungroup() %>% 
                                          rename(customer_num = user_id,
                                                 product = item))

#Scenario 3
output$chosen_recomms <- renderDataTable(rownames = FALSE, {
    dfScenario3()$recommendations
})

#Scenario 4 output
output$popular_products <- renderDataTable(rownames = FALSE,
    
   as(uap_dummy_data,"data.frame") %>% 
       count(item) %>% 
       left_join(uap_products, by = c("item"="product")) %>% 
       select(business_line,item,n) %>% 
       arrange(desc(n)) %>% 
       filter(!is.na(business_line)) %>% 
       group_by(business_line) %>% 
       slice(1:input$recomm_limit) %>% 
       rename(products_bought = n) %>% 
       ungroup() %>% 
       rename(product = item))



})
