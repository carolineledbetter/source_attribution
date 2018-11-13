#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
load(file = 'knnnmodelobj.rda')
load(file = 'FileSkeleton.rda')
NonSpecific <- c('Agona', 'Anatum','Berta', 'Mbandaka', 'Muenchen', 'Saintpaul', 
                 'Stanley', 'Thompson')
PrimaryAnimal <- c('Derby', 'Group B', 'Hadar', 'Infantis', 'Johannesburg', 
                   'Montevideo', 'Oranienburg', 'Reading', 'Sandiego', 
                   'Typhimurium var Cope', 'Uganda')
PrimaryPlant <- c('Cubana', 'Poona', 'Senftenberg', 'Virchow')
Serogroups <- c(levels(input_skeleton$Agent), NonSpecific, PrimaryAnimal, 
                PrimaryPlant)
names(Serogroups) <- Serogroups
Serogroups[Serogroups %in% NonSpecific] <- 'NonSpecific Sero group'
Serogroups[Serogroups %in% PrimaryAnimal] <- "Primary Animal Sero group"
Serogroups[Serogroups %in% PrimaryPlant] <- "Primary Animal Sero group"
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Source Prediction"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("TotalCases",
                         "Total Cases:",
                         min = 0,
                         max = Inf, 
                         value = 0), 
            selectInput('Season', 'Season', 
                        choices = levels(input_skeleton$Season)), 
            selectInput('test', 'test', 
                        choices = 1:10), 
            actionButton('Submit', 'Submit')
        ),

        # Show a plot of the generated distribution
        mainPanel(
           verbatimTextOutput("text")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    observeEvent(input$Submit, output$text <- renderPrint({
        paste('You have selected', isolate(input$TotalCases), 'and', 
              isolate(input$Season), 
              'action is at', input$Submit)
    }))
    observeEvent(input$TotalCases, 
                 updateSelectInput(session, 'test', choices = input$TotalCases))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
