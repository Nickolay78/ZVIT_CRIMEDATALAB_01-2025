
load ("crimes_l.RData")
load ("field_rule.RData")

library(shiny)
library (tidyverse)
library(labelled)
menu<-var_label(crimes_l)


ui <- fluidPage(

    # Application title
    titlePanel("111"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            
            selectInput("gr", "Групи для дослідження", selectize = TRUE, 
                        choices = n_field$nfield[6:375], multiple = TRUE,
                        selected = n_field$nfield[6])
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          DT::dataTableOutput(outputId = "construct")
          #tableOutput("construct")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  #output$construct <- DT::renderDataTable({
  # DT::datatable(crimes_l, filter = "top")})  
  crimes_lr <- reactive({
    req(input$gr, crimes_l)
    selected_cols <- c("Article", "nazva","Year","Chapter","ChapterTXT", n_field$field[n_field$nfield %in% input$gr])
    selected_data <- select(crimes_l, selected_cols)
    colnames(selected_data) <- var_label(selected_data)
    selected_data
  })
  
  output$construct <- DT::renderDT({
    req(crimes_lr())
    DT::datatable(crimes_lr(), filter = "top")
  })
  

  
  
    }


# Run the application 
shinyApp(ui = ui, server = server)
