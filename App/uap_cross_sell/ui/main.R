library(shiny)
library(DT)
library(shinycssloaders)

fluidPage(
  
titlePanel(""),
  
theme = bslib::bs_theme(version = 4, bootswatch = "minty"),

sidebarLayout(
  sidebarPanel(width = 2,
    hidden(
    fluidRow(id = "side_panel",
    selectInput(inputId = "country", 
                label = "Select your country", 
                choices = c("Kenya","Uganda","Zimbabwe","Nigeria","Ghana"), 
                selected = "Kenya", 
                multiple = FALSE),
    numericInput(inputId = "recomm_limit", 
                 label = "Recommendations", 
                 value = 2, min = 1, max = 5),
    actionButton(inputId = "submit", label = "Submit")
    ))),
  
  # Main content
  mainPanel(width = 10,
    hidden(
    fluidRow(
      id = "main",
      column(
        12,
        tags$h3("UAP-OM Cross Selling Tool"),
        tags$button(
          id = "submit_sign_out",
          type = "button",
          "Sign Out",
          class = "btn-danger pull-right",
          style = "float: right;"
        )
      ),
      column(
        12,
        tabsetPanel(type = "pills",
                    tabPanel(title = "ACCOUNT NO", 
                             tags$p("Use this tab when you already have a customer in mind 
                                    that you'd want to target and you have their account number.
                                    Click search to begin or query"),
                             fluidRow(
                               column(width = 4,
                                      textInput(inputId = "customer_ids", 
                                                label = "Enter account numbers separated by commas",
                                                value = "1337,100003,0000013188,3013656028,AG002068,AS000883", 
                                                width = "100%")),
                               column(width = 2,
                                      tags$br(),
                                      actionButton(inputId = "acc_search", label = "Search"))),
                             withSpinner(ui_element = dataTableOutput(outputId = "customer_recomms"), 
                                         color = "#78C2AD")),
                    
                    tabPanel(title = "CUSTOMERS",
                             tags$p("Use this tab when you don't have a specific customer in mind but
                                you want a list of customers who are most likely to make a purchase."),
                             tags$p("Use the provided filters to refine your criteria. Results are
                                    limited to a maximum of 1000 records."),
                             tags$p(),
                             fluidRow(width = 10,
                                      column(width = 3,
                                             selectInput(inputId = "intermediated",
                                                         label = "Intermediated", 
                                                         choices = c("Yes","No"), 
                                                         selected = "No",
                                                         multiple = FALSE)),
                                      column(width = 3,
                                             selectInput(inputId = "ownership", 
                                                         label = "Select account ownership", 
                                                         choices = "Account Ownership", 
                                                         #selected = "General",
                                                         multiple = FALSE)),
                                      column(width = 3,
                                             numericInput(inputId = "min_prod_value", 
                                                          label = "Minimum product value", 
                                                          value = 50000, 
                                                          min = 0)),
                                      column(width = 3,
                                             numericInput(inputId = "max_prod_value", 
                                                          label = "Maximum product value", 
                                                          value = 200000))),
                             tags$hr(),
                             withSpinner(ui_element = dataTableOutput(outputId = "target_list"), 
                                         color = "#78C2AD")),
                    
                    tabPanel(title = "PRODUCTS",
                             tags$p("Use this tab when you want to recommend additional products on
                                top of what a customer has already chosen."),
                             tags$p(),
                             selectInput(inputId = "chosen_products", 
                                         label = "Products chosen by the customer", 
                                         choices = "Chosen Products",
                                         multiple = TRUE),
                             withSpinner(ui_element = dataTableOutput(outputId = "chosen_recomms"), 
                                         color = "#78C2AD")),
                    
                    tabPanel(title = "POPULAR",
                             tags$p("These are the most popular products at UAP-OM. It is a good place 
                                to start if you don't have any information about a customer's preferences"),
                             tags$p(),
                             withSpinner(DTOutput(outputId = "popular_products"), color = "#78C2AD")),
                    
                    tabPanel(title = "BULK",
                             tags$p("Use this tab for obtaining product recommendations in bulk by providing
                                        customer account numbers or a list of products they hold or have expressed
                                        interest it. Use the template provided on the link below."),
                             tags$a(href="files/cs_upload_template.xlsx", "Bulk upload template."),
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
                             dataTableOutput(outputId = "accounts_upload")))
        )
      )
    ))
  )
)