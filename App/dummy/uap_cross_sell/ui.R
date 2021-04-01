#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

load("Showcase - dummy.RData")

library(shiny)
library(DT)
library(shinythemes)

shinyUI(fluidPage(
    
    theme = bslib::bs_theme(version = 4, bootswatch = "minty"),

    # Application title
    titlePanel("UAP-OM Cross Selling Tool [DEMO]"),

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
                tabPanel(title = "1. I have a customer no.", 
                         tags$p("Use this tab when you already have a customer in mind that you'd 
                                want to target and you have their customer number."),
                         selectInput(inputId = "customer_ids",
                                     label = "Select a customer number",
                                     choices = rownames(uap_dummy_data), #TODO: Add customer names
                                     multiple = TRUE),
                         dataTableOutput(outputId = "customer_recomms")),
                tabPanel(title = "2. I want a list of customers",
                         tags$p("Use this tab when you don't have a specific customer in mind but
                                you want a list of customers who are most likely to make a purchase."),
                         tags$p(),
                         dataTableOutput(outputId = "target_list")),
                tabPanel(title = "3. Additional products",
                         tags$p("Use this tab when you want to recommend additional products on
                                top of what a customer has already chosen."),
                         tags$p(),
                         selectInput(inputId = "chosen_products", 
                                     label = "Products held by the customer", 
                                     choices = colnames(uap_dummy_data),
                                     multiple = TRUE),
                         dataTableOutput(outputId = "chosen_recomms")),
                tabPanel(title = "4. Our most popular products",
                         tags$p("These are the most popular products at UAP-OM. It is a good place 
                                to start if you don't have any information about a customer's 
                                preferences"),
                         tags$p(),
                         dataTableOutput(outputId = "popular_products")
                         )
            )
        )
    )
))
