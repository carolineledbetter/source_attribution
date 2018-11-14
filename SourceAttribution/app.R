#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
load(file = 'knnnmodelobj.rda')
load(file = 'FileSkeleton.rda')
NonSpecific <- c('Agona', 'Anatum','Berta', 'Mbandaka', 'Muenchen', 'Saintpaul', 
                 'Stanley', 'Thompson')
PrimaryAnimal <- c('Derby', 'Group B', 'Hadar', 'Infantis', 'Johannesburg', 
                   'Montevideo', 'Oranienburg', 'Reading', 'Sandiego', 
                   'Typhimurium var Cope', 'Uganda')
PrimaryPlant <- c('Cubana', 'Poona', 'Senftenberg', 'Virchow')
SalmSeroTypes <- levels(input_skeleton$Agent)
SalmSeroTypes <- SalmSeroTypes[!SalmSeroTypes %in% c('NonSpecific Sero group', 
                                    "Primary Animal Sero group", 
                                    "Primary Plant Sero group")]
Serogroups <- c(SalmSeroTypes, NonSpecific, PrimaryAnimal, 
                PrimaryPlant)
names(Serogroups) <- Serogroups
names(Serogroups)[Serogroups %in% 'Salm unk sero'] <- 'Unknown'
names(Serogroups)[Serogroups %in% 'Rare'] <- "Other"
names(Serogroups)[Serogroups %in% 'STEC'] <- " "
Serogroups <- as.list(Serogroups)
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
                        choices = c('Choose one' = '', 
                                    levels(input_skeleton$Season))), 
            radioButtons('bacteria', 'Infectious Agent', 
                         selected =  character(0), 
                         choiceNames = c('STEC', 'Salmonella'), 
                         choiceValues = 1:2), 
            selectInput('Agent', 'Salmonella Serotype', 
                        choices = c(list('Choose one' = ""), 
                                    Serogroups)), 
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

    observeEvent(input$Submit, {
        isolate({
            Agent <- input$Agent
            if(Agent %in% NonSpecific) Agent <- 'NonSpecific Sero group'
            if(Agent %in% PrimaryAnimal) Agent <- "Primary Animal Sero group"
            if(Agent %in% PrimaryPlant) Agent <- "Primary Plant Sero group"
        })
        output$text <- renderPrint({
        paste('You have selected', Agent, 'and', 
              isolate(input$Season))
    })})
    observeEvent(input$bacteria, {
        if(input$bacteria == 1) choice <- Serogroups[9]
        if(input$bacteria == 2) choice <- c(list('Choose one' = ""), 
                                            Serogroups[-9])
        updateSelectInput(session, 'Agent', choices = choice)
    })
                    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
