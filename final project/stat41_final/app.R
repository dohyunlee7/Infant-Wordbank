#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load Libraries
library(shiny)
library(tidyverse)
library(ggrepel)
library(dplyr)
library(magrittr)
library(plotly)
library(rsconnect)

# Load and Merge Data

wordbylang = read_csv("wordbylang.csv")

wordbylang <- wordbylang %>%
    mutate(language = ifelse(as.character(language) == 
                                 "American Sign Language", 
                             "ASL", as.character(language))) %>%
    mutate(language = ifelse(as.character(language) == 
                                 "British Sign Language", 
                             "BSL", as.character(language))) %>%
    mutate(language = ifelse(as.character(language) == 
                                 "English (American)", 
                             "English (USA)", as.character(language))) %>%
    mutate(language = ifelse(as.character(language) == 
                                 "English (Australian)", 
                             "English (AUS)", as.character(language))) %>%
    mutate(language = ifelse(as.character(language) == 
                                 "English (British)", 
                             "English (UK)", as.character(language))) %>%
    mutate(language = ifelse(as.character(language) == 
                                 "French (French)", 
                             "French (FRA)", as.character(language))) %>%
    mutate(language = ifelse(as.character(language) == 
                                 "French (Quebecois)", 
                             "French (QUE)", as.character(language))) %>% 
    filter(age >= 8 & age <= 20)


uni_lemma_list <- sort(unique(wordbylang$uni_lemma))

# Define UI 
ui <- fluidPage(
    
    # Application title
    titlePanel("Vocabulary Production and Comprehension by Word and Language"),
    
    # Sidebar layout with a input and output definitions
    sidebarLayout(
        # Inputs: Select variables to plot
        sidebarPanel(
            
            
            selectInput(inputId = "measure", 
                        label = "Measure of Vocabulary:",
                        choices = c("Production " = "produces",
                                    "Comprehension" = "understands")),
            
            
            selectInput(inputId = 'uni_lemma', 
                        label = 'Word:', 
                        choices = uni_lemma_list,
                        selected = "dog"),
            
            sliderInput(inputId = "alpha", 
                        label = "Alpha:", 
                        min = 0, max = 1, 
                        value = 0.5)
            
            
        ),
        
        #Output
        
        mainPanel(
            
            plotlyOutput(outputId = "plot"),
            br(),        # a little bit of visual separation
            
        )
    )
)

# Define server function --------------------------------------------
server <- function(input, output) {
    
    wordbylang2 <- reactive ({
        req(input$uni_lemma)
        wordbylang %>%
            group_by(language) %>%
            filter(uni_lemma == input$uni_lemma &
                       measure == input$measure &
                       n >= 3 ) 
    })
    
    wbl_subset <- reactive({
        wordbylang %>%
            select(language, measure, words) %>%
            distinct()
    })
    
    # Create scatterplot object the plotOutput function is expecting
    output$plot <- renderPlotly({
        
        # Creates base plot 
        
        p1 <- ggplot(wordbylang2(), aes(x = age, y = prop, color = language)) +
            geom_point(size = 0.3, alpha = input$alpha) +
            geom_smooth(method = "loess", se = FALSE) +
            facet_wrap(vars(language)) +
            labs(title = "Growth Curve for a Word by Language") +
            scale_x_continuous(name = "Age (months)", limits = c(8, 18),
                               breaks = seq(8, 18, 2)) +
            scale_y_continuous(name = "\nProportion of children", limits = c(0, 1)) +
            theme(legend.position = "none",
                  panel.background = element_blank()) 
        
        
        ggplotly(p1, tooltip = "text")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)


