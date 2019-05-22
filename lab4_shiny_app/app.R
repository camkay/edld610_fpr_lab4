#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(forcats)

gss_cat

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("General Social Science Survey: TV Data"),
    sidebarLayout(
        sidebarPanel(
            selectInput("var",
                         "Variable:", #label that appers on rendered html
                         choices = c(
                             "None" = "none",
                             "Marital status" = "marital", 
                             "Age" = "age", 
                             "Identified race" = "race",
                             "Reported income" = "rincome",
                             "Political affiliation" = "partyid",
                             "Identified religion" = "relig",
                             "Identified denomination" = "denom"),
                         selected = "none")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"), #renders/produces "displot" (see below)
            dataTableOutput("dist_smry")
        )
    )
)

# Define server logic required to draw a histogram
# input comes from ui

server <- function(input, output) {
    output$distPlot <- renderPlot({
        p <- ggplot(gss_cat, aes(tvhours)) +
            geom_histogram(colour = "#633974",
                           fill = "#D2B4DE",
                           alpha = 0.7) +
            theme_minimal(base_size = 16) +
            theme(axis.title = element_text(face = "bold"),
                  strip.text = element_text(face = "bold"),
                  axis.line = element_line(colour = "gray60")) +
            scale_y_continuous(expand = c(0, 0)) +
            labs(x = "TV Hours",
                 y = "Count")
        if(input$var != "none") {
            p <- p + facet_wrap(input$var) # with strings we do not use ~
        }
        p
    })
    
    output$dist_smry <- renderDataTable({
        if(input$var == "none") {
            gss_cat %>%
                summarize(Mean = mean(tvhours, na.rm = TRUE),
                          SD   = sd(tvhours, na.rm = TRUE),
                          Min  = min(tvhours, na.rm = TRUE),
                          Max  = max(tvhours, na.rm = TRUE)) %>%
                mutate_if(is.numeric, round, 2) %>%
                datatable(rownames = FALSE)
        }
        else {
            gss_cat %>% 
                group_by(!!sym(input$var)) %>% 
                summarize(Mean = mean(tvhours, na.rm = TRUE),
                          SD   = sd(tvhours, na.rm = TRUE),
                          Min  = min(tvhours, na.rm = TRUE),
                          Max  = max(tvhours, na.rm = TRUE)) %>% 
                mutate_if(is.numeric, round, 2) %>% 
                datatable(rownames = FALSE)
        }
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
