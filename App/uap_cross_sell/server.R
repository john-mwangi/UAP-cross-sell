product_businesslines <- readRDS("./objects/product_businesslines.rds")
products_rating_matrix <- readRDS("./objects/products_rating_matrix.rds")

library(shiny)
library(recommenderlab)
library(DT)
library(DBI)
library(RSQLite)
library(tidyverse)

shinyServer(function(input, output, session) {
  
  ##======== FIREBASE AUTHENTICATION============

  ##### Switch Views ------------------
  # if user click link to register, go to register view
  observeEvent(input$go_to_register, {
    shinyjs::show("register_panel", anim = TRUE, animType = "fade")
    shinyjs::hide("sign_in_panel")
  }, ignoreInit = TRUE)

  observeEvent(input$go_to_sign_in, {
    shinyjs::hide("register_panel")
    shinyjs::show("sign_in_panel", anim = TRUE, animType = "fade")
  }, ignoreInit = TRUE)

  # switch between auth sign in/registration and app for signed in user
  observeEvent(session$userData$current_user(), {
    current_user <- session$userData$current_user()

    if (is.null(current_user)) {
      shinyjs::show("sign_in_panel")
      shinyjs::hide("main")
      shinyjs::hide("side_panel")
      shinyjs::hide("verify_email_view")
    } else {
      shinyjs::hide("sign_in_panel")
      shinyjs::hide("register_panel")

      if (current_user$emailVerified == TRUE) {
        shinyjs::show("main")
        shinyjs::show("side_panel")
      } else {
        shinyjs::show("verify_email_view")
      }
      
    }

  }, ignoreNULL = FALSE)




  # Signed in user --------------------
  # the `session$userData$current_user()` reactiveVal will hold information about the user
  # that has signed in through Firebase.  A value of NULL will be used if the user is not
  # signed in
  session$userData$current_user <- reactiveVal(NULL)

  # input$sof_auth_user comes from front end js in "www/sof-auth.js"
  observeEvent(input$sof_auth_user, {

    # set the signed in user
    session$userData$current_user(input$sof_auth_user)

  }, ignoreNULL = FALSE)



  ##### App for signed in user
  signed_in_user_df <- reactive({
    req(session$userData$current_user())

    out <- session$userData$current_user()
    out <- unlist(out)

    data.frame(
      name = names(out),
      value = unname(out)
    )
  })


  output$user_out <- DT::renderDT({
    datatable(
      signed_in_user_df(),
      rownames = FALSE,
      options = list(
        dom = "tp",
        scrollX = TRUE
      )
    )
  })
  
  
  
  ##====== Create db connection as soon as user logs in successfully
  sqlite_path <- "./db/ke_cs.db"
  con <- dbConnect(drv = SQLite(), sqlite_path)
  
  
  
  ###============== REACTIVES========
  
  dfScenario1 = reactive({
    
    records <- strsplit(x = input$customer_ids, split = ",")[[1]]
    
    chosen_customers <-
    tbl(src = con, "recommendations_detailed") %>%
      filter(ACCOUNT_NO %in% records) %>%
      group_by(ACCOUNT_NO, BUSINESS_LINE) %>% 
      arrange(desc(rating)) %>% 
      collect() %>% 
      slice(1:input$recomm_limit) %>% 
      arrange(desc(max_rating)) %>% 
      rename(inter = intermediated,
             value = product_value) %>% 
      setNames(str_to_upper(colnames(.)))
    
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
      ke_pop <- readRDS("./objects/ke_pop.rds")
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
        slice(1:input$recomm_limit) %>% 
        setNames(str_to_upper(colnames(.)))
      
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
                                          sheet = "customer_acc") %>% 
      distinct(ACCOUNT_NO) %>% 
      mutate(ACCOUNT_NO = as.character(ACCOUNT_NO)) %>% 
      pull(ACCOUNT_NO)
    
    cust_prods_upload <- readxl::read_excel(path = input$data_upload$datapath, 
                                            sheet = "customer_prod") %>% 
      distinct(PRODUCT) %>% 
      pull(PRODUCT)
    
    # Recommendations using uploaded customer numbers
    acc_upload <-
    tbl(src = con, "recommendations_detailed") %>%
      filter(ACCOUNT_NO %in% acc_nums_upload) %>%
      group_by(ACCOUNT_NO, BUSINESS_LINE) %>% 
      arrange(desc(rating)) %>% 
      collect() %>% 
      slice(1:input$recomm_limit) %>% 
      arrange(desc(max_rating)) %>% 
      rename(inter = intermediated,
             value = product_value) %>% 
      setNames(str_to_upper(colnames(.)))
    
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
    
    ke_pop <- readRDS("./objects/ke_pop.rds")
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
      slice(1:input$recomm_limit) %>% 
      setNames(str_to_upper(colnames(.)))
    
    return(list(recommendations_acc=acc_upload, 
                recommendations_prod=prod_upload,
                checks=acc_nums_upload))
    
  })
  
  
  
  ##=======OUTPUTS==============
  
  # #Scenario 1 output
  output$customer_recomms <- renderDataTable(server = FALSE,
                                             rownames = FALSE,
                                             extensions = "Buttons",
                                             options = list(dom = "Bfrtip",
                                                            buttons = "excel",
                                                            columnDefs = list(list(visible=FALSE, targets=c(-1))),
                                                            rowCallback = JS(
                                                              "function(row, data) {",
                                                              "var full_text = 'Customer details: ' + data[8]",
                                                              "$('td', row).attr('title', full_text);",
                                                              "}"
                                                            )), {
                                                              dfScenario1()$recommendations
                                                            })
  
  #Scenario 2 output
  output$target_list <- renderDataTable(server = FALSE,
                                        rownames = FALSE,
                                        extensions = "Buttons",
                                        options = list(dom = "Bfrtip",
                                                       buttons = "excel",
                                                       columnDefs = list(list(visible=FALSE, targets=c(-1))),
                                                       rowCallback = JS(
                                                         "function(row, data) {",
                                                         "var full_text = 'Customer details: ' + data[8]",
                                                         "$('td', row).attr('title', full_text);",
                                                         "}"
                                                       )),
                                        
                                        
                                        tbl(src = con, "recommendations_detailed_sample") %>% 
                                          group_by(ACCOUNT_NO, BUSINESS_LINE) %>% 
                                          filter(intermediated==!!input$intermediated) %>% 
                                          filter(ownership==!!input$ownership) %>% 
                                          filter(product_value>=!!input$min_prod_value & 
                                                 product_value<=!!input$max_prod_value) %>%
                                          arrange(desc(max_rating),
                                                  desc(rating),
                                                  .by_group = TRUE) %>%
                                          collect() %>% 
                                          slice(1:input$recomm_limit)%>% 
                                          rename(inter = intermediated,
                                                 value = product_value) %>% 
                                          setNames(str_to_upper(colnames(.)))
                                        )
  
  #Scenario 3 output
  output$chosen_recomms <- renderDataTable(server = FALSE,
                                           rownames = FALSE,
                                           extensions = "Buttons",
                                           options = list(dom = "Bfrtip",
                                                          buttons = "excel"), {
                                                            dfScenario3()$recommendations
                                                          })
  
  #Scenario 4 output
  output$popular_products <- renderDT(server = FALSE,
                                      rownames = FALSE,
                                      extensions = "Buttons",
                                      options = list(dom = "Bfrtip",
                                                     buttons = "excel"),
                                      
                                      tbl(src = con, "popular_products") %>% 
                                        collect() %>% 
                                        group_by(BUSINESS_LINE) %>% 
                                        slice(1:input$recomm_limit) %>% 
                                        rename(PURCHASES = n)
                                      )
  
  #Scenario 5 output
  output$accounts_upload <- renderDataTable(server = FALSE,
                                            rownames = FALSE, 
                                            extensions = "Buttons",
                                            options = list(dom = "Bfrtip",
                                                           buttons = "excel",
                                                           columnDefs = list(list(visible=FALSE, targets=c(-1))),
                                                           rowCallback = JS(
                                                             "function(row, data) {",
                                                             "var full_text = 'Customer details: ' + data[8]",
                                                             "$('td', row).attr('title', full_text);",
                                                             "}"
                                                           )), {
                                                             dfScenario5()$recommendations_acc
                                                           })
  
  #Bulk upload output
  output$products_upload <- renderDataTable(server = FALSE,
                                            rownames = FALSE,
                                            extensions = "Buttons",
                                            options = list(dom = "Bfrtip",
                                                           buttons = "excel"),{
                                                             dfScenario5()$recommendations_prod
                                                           })
  
  output$checks <- renderText({
    dfScenario5()$checks
  })

  
  ### END
})
