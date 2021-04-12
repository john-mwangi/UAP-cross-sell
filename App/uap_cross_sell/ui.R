load("07Apr_Models.RData")

library(shiny)
library(shinythemes)
library(data.table)
library(DT)

shinyUI(fluidPage(
    
    theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
    
    # Application title
    titlePanel("UAP-OM Cross Selling Tool"),
    
    # Sidebar for selecting key parameters
    sidebarLayout(
        sidebarPanel(width = 3,
                     selectInput(inputId = "country", 
                                 label = "Country", 
                                 choices = c("Kenya","Uganda","Zimbabwe"), 
                                 selected = "Kenya", 
                                 multiple = FALSE),
                     numericInput(inputId = "recomm_limit", 
                                  label = "Recommendations per business line", 
                                  value = 2, min = 1, max = 10),
                     actionButton(inputId = "submit", label = "Submit")),
        
        # Main panel for recommendation results
        mainPanel(
            tabsetPanel(type = "pills",
                        tabPanel(title = "ACCOUNT NO.", 
                                 tags$p("Use this tab when you already have a customer in mind 
                                        that you'd want to target and you have their account number."),
                                 textInput(inputId = "customer_ids", 
                                           label = "Enter account numbers separated by commas",
                                           value = paste0(rownames(products_rating_matrix)[1:2], 
                                                          collapse = ",")),
                                 dataTableOutput(outputId = "customer_recomms")),
                        tabPanel(title = "CUSTOMERS",
                                 tags$p("Use this tab when you don't have a specific customer in mind but
                                you want a list of customers who are most likely to make a purchase. If you
                                have a specific account number in mind, you can as well search it here."),
                                tags$p("The table below displays only the first few records. Click the link below
                                       to download recommendations for all UAP-OM customers in Excel format."),
                                tags$a(href="recommendations_df.csv", "Download all records!"),
                                tags$p(),
                                dataTableOutput(outputId = "target_list")),
                        tabPanel(title = "PRODUCTS",
                                 tags$p("Use this tab when you want to recommend additional products on
                                top of what a customer has already chosen."),
                                tags$p(),
                                selectInput(inputId = "chosen_products", 
                                            label = "Products chosen by the customer", 
                                            choices = colnames(products_rating_matrix),
                                            multiple = TRUE),
                                dataTableOutput(outputId = "chosen_recomms")),
                        tabPanel(title = "POPULAR",
                                 tags$p("These are the most popular products at UAP-OM. It is a good place 
                                to start if you don't have any information about a customer's preferences"),
                                tags$p(),
                                DTOutput(outputId = "popular_products")),
                        tabPanel(title = "BULK",
                                 tags$p("Use this tab for obtaining product recommendations in bulk by providing
                                        customer account numbers or a list of products they hold or have expressed
                                        interest it. Use the template provided on the link below."),
                                 tags$a(href="cs_upload_template.xlsx", "Bulk upload template."),
                                 fileInput(inputId = "data_upload", 
                                           label = "", 
                                           multiple = FALSE,
                                           accept = ".xlsx"),
                                 tags$hr(),
                                 tags$h6("PRODUCT NAMES"),
                                 tags$p("Recommendations based on product names provided will appear here."),
                                 dataTableOutput(outputId = "products_upload"),
                                 tags$hr(),
                                 tags$h6("ACCOUNT NUMBERS"),
                                 tags$p("Recommendations based on account numbers provided will appear here."),
                                 #textOutput(outputId = "checks"),
                                 dataTableOutput(outputId = "accounts_upload"))
            )
        )
    )
))
