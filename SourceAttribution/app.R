#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(kknn)
library(tidyr)
library(ggplot2)
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
        sidebarPanel(splitLayout(
            wellPanel(
                numericInput("TotalCases",
                             "Total Cases:",
                             min = 1,
                             max = Inf, 
                             value = 0), 
                selectInput('Season', 'Season', 
                            choices = c('Choose one' = '', 
                                        levels(input_skeleton$Season))), 
                radioButtons('Geography', 'Geography', 
                             choices = levels(input_skeleton$Geography)), 
                radioButtons('.bacteria', 'Infectious Agent', 
                             selected =  character(0), 
                             choiceNames = c('STEC', 'Salmonella'), 
                             choiceValues = 1:2), 
                selectInput('Agent', 'Salmonella Serotype', 
                            choices = c('Choose STEC or Salmonella' = "")), 
                sliderInput('GenderMale', 'Male', 
                            min = 0, 
                            max = 0, 
                            value = 0, 
                            ticks = F, step = 1),
                sliderInput('GenderFemale', 'Female', 
                            min = 0, 
                            max = 0, 
                            value = 0, 
                            ticks = F, step = 1),
                sliderInput('GenderSexUnknown', 'Unknown', 
                            min = 0, 
                            max = 0, 
                            value = 0, 
                            ticks = F, step = 1),
                sliderInput('HospPercent2', 'Percent Hospitalized', 
                            min = 0, 
                            max = 100, 
                            post = '%', 
                            ticks = F, 
                            value = 0)
                ), 
            
            wellPanel(
                numericInput('AgeUnder1', 'Under 1 Year', 
                             min = 0, 
                             max = 0, 
                             value = 0), 
                numericInput('Age1to4', '1 yr to 4 yrs', 
                             min = 0, 
                             max = 0, 
                             value = 0), 
                numericInput('Age5to9', '5 yrs to 9 yrs', 
                             min = 0, 
                             max = 0, 
                             value = 0), 
                numericInput('Age10to19', '10 yrs to 19 yrs', 
                             min = 0, 
                             max = 0, 
                             value = 0),             
                numericInput('Age20to49', '20 yrs to 49 yrs', 
                             min = 0, 
                             max = 0, 
                             value = 0), 
                numericInput('Age50to74', '50 yrs to 74 yrs', 
                             min = 0, 
                             max = 0, 
                             value = 0), 
                numericInput('Age75plus', '75 yrs or older', 
                             min = 0, 
                             max = 0, 
                             value = 0),
                numericInput('AgeUnknown', 'AgeUnknown', 
                             min = 0, 
                             max = 0, 
                             value = 0), 
                actionButton('.Submit', 'Submit')
                )
            )
            ), 
    
        mainPanel(plotOutput("Results"))
    )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {

    observeEvent(input$.Submit, {
        output$Results <- renderPlot({
            isolate({
                total <- sum(input$AgeUnder1, 
                             input$Age1to4, 
                             input$Age5to9, 
                             input$Age10to19, 
                             input$Age20to49, 
                             input$Age50to74, 
                             input$Age75plus)
                validate(
                    need(input$TotalCases > 0,
                         'You must have at least one case'),
                    need(input$Season != "",
                         'You must choose a season'),
                    need(input$Agent != "",
                         paste0('You must choose an an infectious agent ',
                                'and a salmonella serotype if the agent is ',
                                'salmonella')),
                    need(total <= input$TotalCases, 
                         paste0('The sum of number of cases in each age group ', 
                                'can not exceed the total number of cases'))
                )
                Inputs <- reactiveValuesToList(input)
                Agent <- Inputs$Agent
                if(Agent %in% NonSpecific) Agent <- 'NonSpecific Sero group'
                if(Agent %in% PrimaryAnimal) Agent <-
                    "Primary Animal Sero group"
                if(Agent %in% PrimaryPlant) Agent <- "Primary Plant Sero group"
                Inputs$Agent <- Agent
                Inputs <- data.frame(Inputs)
                Inputs <- subset(Inputs,
                                 select = -c(`Season.selectized`,
                                             `Agent.selectized`))
                SexVars <- grep('^Gender', names(Inputs), value = T)
                AgeVars <- grep('^Age[^n]', names(Inputs), value = T)
                PercentSex <- paste0('Percent', SexVars)
                PercentSex <- gsub('Gender', "", PercentSex)
                PercentAge <- paste0('Percent', AgeVars)
                Inputs[, PercentSex] <- lapply(Inputs[, SexVars], 
                                               function(x){
                                                   y <- x/Inputs$TotalCases*100
                                                   return(y)
                                               })
                Inputs[, PercentAge] <- lapply(Inputs[, AgeVars], 
                                               function(x){
                                                   y <- x/Inputs$TotalCases*100
                                                   return(y)
                                               })
                Inputs <- subset(Inputs,
                                 select = names(input_skeleton))
                input_skeleton[1, ] <- Inputs[1, ]
                pred <- predict(finalchoice, newdata = input_skeleton,
                                type = 'prob')
                pred <- gather(pred, 1:4, key = 'source', value = 'prob')
                ggplot(data = pred, aes(x = source, y = prob)) +
                    geom_col(aes(fill = source)) +
                    theme_classic() +
                    labs(x = 'Source',
                         y = 'Predicted Probability',
                         title = 'Predicted Probability of Potential Sources') +
                    guides(fill = 'none') + 
                    geom_label(aes(label = round(prob, 2)))
            })
        }, width = 800, height = 800, res = 200)
        # output$test <- renderPrint({
        #     Inputs <- reactiveValuesToList(input)
        #     Agent <- Inputs$Agent
        #     if(Agent %in% NonSpecific) Agent <- 'NonSpecific Sero group'
        #     if(Agent %in% PrimaryAnimal) Agent <- 
        #         "Primary Animal Sero group"
        #     if(Agent %in% PrimaryPlant) Agent <- "Primary Plant Sero group"
        #     Inputs$Agent <- Agent
        #     Inputs <- data.frame(Inputs)
        #     Inputs <- subset(Inputs,
        #                      select = -c(`Season.selectized`,
        #                                  `Agent.selectized`))
        #     SexVars <- grep('^Gender', names(Inputs), value = T)
        #     AgeVars <- grep('^Age[^n]', names(Inputs), value = T)
        #     PercentSex <- paste0('Percent', SexVars)
        #     PercentSex <- gsub('Gender', "", PercentSex)
        #     PercentAge <- paste0('Percent', AgeVars)
        #     Inputs[, PercentSex] <- lapply(Inputs[, SexVars], 
        #                                    function(x){
        #                                        y <- x/Inputs$TotalCases*100
        #                                        return(y)
        #                                    })
        #     Inputs[, PercentAge] <- lapply(Inputs[, AgeVars], 
        #                                    function(x){
        #                                        y <- x/Inputs$TotalCases*100
        #                                        return(y)
        #                                    })
        #     Inputs <- subset(Inputs,
        #                      select = names(input_skeleton))
        #     str(Inputs)
        #     
        # })
    })
    

    observeEvent(input$.bacteria, {
        if(input$.bacteria == 1) choice <- Serogroups[9]
        if(input$.bacteria == 2) choice <- c(list('Choose one' = ""), 
                                            Serogroups[-9])
        updateSelectInput(session, 'Agent', choices = choice)
    })
    observeEvent(input$TotalCases, {
        tot <- input$TotalCases
        updateSliderInput(session, 'GenderMale', 
                          min = 0, 
                          max = tot)
        updateSliderInput(session, 'GenderFemale', 
                          min = 0, 
                          max = tot - input$GenderMale)
        updateSliderInput(session, 'GenderSexUnknown', 
                          min = 0, 
                          max = tot - input$GenderMale - input$GenderFemale, 
                          val = tot - input$GenderMale - input$GenderFemale)
        total <- sum(input$AgeUnder1, 
                     input$Age1to4, 
                     input$Age5to9, 
                     input$Age10to19, 
                     input$Age20to49, 
                     input$Age50to74, 
                     input$Age75plus)
        updateNumericInput(session, 'AgeUnknown', 
                           max = input$TotalCases - total, 
                           value = input$TotalCases - total)
        # updateNumericInput(session, 'AgeUnder1', 
        #                    max = input$TotalCases)
        # updateNumericInput(session, 'Age1to4', 
        #                    max = input$TotalCases)
        # updateNumericInput(session, 'Age5to9', 
        #                    max = input$TotalCases)
        # updateNumericInput(session, 'Age10to19', 
        #                    max = input$TotalCases)        
        # updateNumericInput(session, 'Age20to49', 
        #                    max = input$TotalCases)       
        # updateNumericInput(session, 'Age50to74', 
        #                    max = input$TotalCases)
        # updateNumericInput(session, 'Age75plus', 
        #                    max = input$TotalCases)
    })
    
    observeEvent(input$GenderMale, {
        updateSliderInput(session, 'GenderFemale', 
                          min = 0, 
                          max = input$TotalCases - input$GenderMale)
    })
    
    observeEvent(input$GenderFemale|input$GenderMale, {
        val <- input$TotalCases - input$GenderMale - input$GenderFemale
        updateSliderInput(session, 'GenderSexUnknown', 
                          min = 0, 
                          max = val, 
                          val = val)
    })
    observeEvent(input$AgeUnder1|input$Age1to4|
                     input$Age5to9|input$Age10to19|
                     input$Age20to49|input$Age50to74|
                     input$Age75plus, {
                         total <- sum(input$AgeUnder1, 
                                      input$Age1to4, 
                                      input$Age5to9, 
                                      input$Age10to19, 
                                      input$Age20to49, 
                                      input$Age50to74, 
                                      input$Age75plus)
                         updateNumericInput(session, 'AgeUnknown', 
                                            max = input$TotalCases - total, 
                                            value = input$TotalCases - total)
                     })
                    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
