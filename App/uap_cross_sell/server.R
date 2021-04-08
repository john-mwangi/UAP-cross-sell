load("07Apr_Models.RData")

library(shiny)
library(recommenderlab)
library(tidyverse)
library(DT)


shinyServer(function(input, output, session) {

    
##========REACTIVES===============
        
    dfScenario1 = reactive({

        records <- strsplit(x = input$customer_ids, split = ",")[[1]]

        chosen_customers <- recommendations_df %>%
            filter(ACCOUNT_NO %in% records) %>%
            arrange(desc(rating)) %>%
            slice(1:input$recomm_limit) %>%
            arrange(desc(max_rating))

        return(list(recommendations=chosen_customers))

    })
    
    dfScenario3 = reactive({
        
        if(input$submit>0){
            #Capture user selections
            user_choices <- c(input$chosen_products)
            
            #Products not selected
            not_user_choices <- colnames(products_rating_matrix)[!colnames(products_rating_matrix) %in% user_choices]
            
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
            
            user_choices_ratmat <- as(user_choices_mat,"binaryRatingMatrix")
            
            #Recommend products
            user_choice_recommendations <- predict(object = ke_pop, 
                                newdata = user_choices_ratmat, 
                                type="ratings")
            
            #Convert recommendations to dataframe
            user_choice_recommendations_df <-
            as(user_choice_recommendations, "data.frame") %>% 
                rename(PRODUCT = item) %>%
                left_join(product_businesslines, by = "PRODUCT") %>% 
                relocate(BUSINESS_LINE, .after = PRODUCT) %>% 
                group_by(BUSINESS_LINE) %>% 
                arrange(desc(rating), .by_group = TRUE) %>% 
                slice(1:input$recomm_limit)
            
            return(list(recommendations=user_choice_recommendations_df))
        }
    })
    
    
    dfScenario5 = reactive({

        # Prevent reactive from executing unless a file is uploaded
        req(input$data_upload)

        # File validation
        ext <- tools::file_ext(input$data_upload$datapath)
        validate(need(ext == "xlsx", "Please use the provided Excel (.xlsx) template"))
        
        acc_nums_upload <- readxl::read_excel(path = input$data_upload$datapath,
                                              sheet = "customer_acc") %>% pull(ACCOUNT_NO)
        
        cust_prods_upload <- readxl::read_excel(path = input$data_upload$datapath, 
                                                sheet = "customer_prod") %>% pull(PRODUCT)
        
        # Recommendations using uploaded customer numbers
        recommendations_df %>% 
            filter(ACCOUNT_NO %in% acc_nums_upload) %>% 
            slice(1:input$recomm_limit) %>% 
            arrange(desc(max_rating),
                    desc(rating), 
                    .by_group = TRUE) -> acc_upload
        
        # Recommendations using uploaded product names
        user_choices_up <- cust_prods_upload
        
        not_user_choices_up <- colnames(products_rating_matrix)[!colnames(products_rating_matrix) %in% user_choices_up]
        
        choices_up <- tibble(choice_up = 
                              rep(x = c(1,0), 
                                  times = c(length(user_choices_up), 
                                            length(not_user_choices_up))
                              ))
        
        user_choices_df_up <-
            choices_up %>% 
            mutate(user_choices_up = c(user_choices_up, not_user_choices_up))
        
        user_choices_df_up <- 
            pivot_wider(data = user_choices_df_up,
                        names_from = user_choices_up, 
                        values_from = choice_up)
        
        user_choices_mat_up <- as.matrix(user_choices_df_up)
        
        user_choices_ratmat_up <- as(user_choices_mat_up,"binaryRatingMatrix")
        
        user_choice_recommendations_up <- 
            predict(object = ke_pop, 
                    newdata = user_choices_ratmat_up, 
                    type="ratings")
        
        prod_upload <- as(user_choice_recommendations_up, "data.frame") %>% 
            rename(PRODUCT = item) %>%
            left_join(product_businesslines, by = "PRODUCT") %>% 
            relocate(BUSINESS_LINE, .after = PRODUCT) %>% 
            group_by(BUSINESS_LINE) %>% 
            arrange(desc(rating), .by_group = TRUE) %>% 
            slice(1:input$recomm_limit)
        
        return(list(recommendations_acc=acc_upload, 
                    recommendations_prod=prod_upload,
                    checks=acc_nums_upload))

    })

    
    
##=======OUTPUTS==============
    
    # #Scenario 1 output
    output$customer_recomms <- renderDataTable(rownames = FALSE,
                                               extensions = "Buttons",
                                               options = list(dom = "Bfrtip",
                                                              buttons = c("excel")), {
        dfScenario1()$recommendations
    })
    
    #Scenario 2 output
    output$target_list <- renderDataTable(rownames = FALSE,
                                          extensions = "Buttons",
                                          options = list(dom = "Bfrtip",
                                                         buttons = c("excel")),
                                          
                                          recommendations_df_sample %>% 
                                              arrange(desc(max_rating), 
                                                      desc(rating), 
                                                      .by_group = TRUE) %>%
                                              slice(1:input$recomm_limit))
    
    #Scenario 3
    output$chosen_recomms <- renderDataTable(rownames = FALSE,
                                             extensions = "Buttons",
                                             options = list(dom = "Bfrtip",
                                                            buttons = c("excel")), {
        dfScenario3()$recommendations
    })
    
    #Scenario 4 output
    output$popular_products <- renderDT(rownames = FALSE,
                                               extensions = "Buttons",
                                               options = list(dom = "Bfrtip",
                                                              buttons = c("excel")),
                                               
                                               popular_products %>% 
                                                   slice(1:input$recomm_limit))
    
    #Scenario 5 output
    output$accounts_upload <- renderDataTable({
        dfScenario5()$recommendations_acc
    })
    
    output$products_upload <- renderDataTable({
        dfScenario5()$recommendations_prod
    })
    
    output$checks <- renderText({
        dfScenario5()$checks
    })
    
})
