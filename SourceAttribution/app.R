###########################################
# Project: P1330White
# Author: David Weitzenkamp & Caroline Ledbetter
# Date: 11/16/2018
# #########################################

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(kknn) # necessary to predict with algorithm we chose
library(tidyr) # for gather
library(ggplot2) # for plotting

# load prediction algortithm
load(file = 'knnnmodelobj.rda')

# load empty data frame with required predictors and possible levels
load(file = 'FileSkeleton.rda')

# classify rare serotypes according to primary source
NonSpecific <- c('Agona', 'Anatum','Berta', 'Mbandaka', 'Muenchen', 'Saintpaul', 
                 'Stanley', 'Thompson')
PrimaryAnimal <- c('Derby', 'Group B', 'Hadar', 'Infantis', 'Johannesburg', 
                   'Montevideo', 'Oranienburg', 'Reading', 'Sandiego', 
                   'Typhimurium var Cope', 'Uganda')
PrimaryPlant <- c('Cubana', 'Poona', 'Senftenberg', 'Virchow')

# format salmonella serotypes for tidy appearance in webplatform
SalmSeroTypes <- levels(input_skeleton$Agent)
SalmSeroTypes <- SalmSeroTypes[!SalmSeroTypes %in% c('NonSpecific Sero group', 
                                    "Primary Animal Sero group", 
                                    "Primary Plant Sero group")]
Serogroups <- c(SalmSeroTypes, NonSpecific, PrimaryAnimal, 
                PrimaryPlant)
Serogroups <- Serogroups[order(Serogroups)]
names(Serogroups) <- Serogroups
names(Serogroups)[Serogroups %in% 'Salm unk sero'] <- 'Unknown'
names(Serogroups)[Serogroups %in% 'Rare'] <- "Other"
names(Serogroups)[Serogroups %in% 'STEC'] <- " "

Serogroups <- as.list(Serogroups)

# format Geography for tidy appearance in webplatform
Geography <- as.list(levels(input_skeleton$Geography))
names(Geography) <- Geography
names(Geography)[Geography == 'Missing'] <- 'Unknown'

# Set up named month list
Months <- as.list(1:12)
names(Months) <- month.name

# Define User Interface
ui <- fluidPage(
    # make required flags red
    tags$style(type = "text/css", 
               ".required{color: red;}"), 
    # format title
    tags$style(type = 'text/css', 
               ".title{color: #006600; 
                       text-align:center;
                       }"),
    # format logo image
    tags$style(type = 'text/css', 
               ".logo{position: fixed; 
                      right: 5%;
                      bottom: 5%; 
                      width: 10%;
                      }"), 
    # format instuctions hover
    tags$style(type = 'text/css', 
               'span.inst:hover + div {display: block;
                            }'), 
    tags$style(type = 'text/css', 
               'div.inst{display: none;
                         color: #ff8000;
                            }'), 
    # Application title
    div(class = 'title', 
        titlePanel("Foodbourne Outbreaks: Source Attribution Prediction")),

    # Sidebar with inputs for model and instructions
    sidebarLayout(
        sidebarPanel(splitLayout(
            # Left side of sidebar
            wellPanel(
                    numericInput("TotalCases",
                             label = span(
                                 span(class = 'inst', 'Total Cases', 
                                      div(class = 'required', 
                                          '*required')),  
                                 div(class = 'inst', 
                                     'All suspected and confirmed cases') 
                                 ),
                             min = 1,
                             max = Inf, 
                             value = 0),
                    
                selectInput('Month', 
                            span("Month of first illness onset", 
                                 div(class = 'required', 
                                     '*required')), 
                            choices = c('Choose one' = '', 
                                        Months)), 
                radioButtons('Geography', 
                             label = span(
                                 span(class = 'inst', 
                                      "Geography of exposures", 
                                      div(class = 'required', 
                                          '*required')),  
                                 div(class = 'inst', 
                                     'The geography of the suspected exposures,',  
                                     br(), 
                                     'not the residence of the cases.')), 
                             choices = Geography, 
                             selected = character(0)),
                radioButtons('.bacteria', 
                             span('Infectious Agent', 
                                  div(class = 'required', 
                                      '*required')), 
                             selected =  character(0), 
                             choiceNames = c('STEC', 'Salmonella'), 
                             choiceValues = 1:2), 
                selectInput('Agent', 
                            label = span(
                                span(class = 'inst', 
                                     'Salmonella Serotype', 
                                     div(class = 'required', 
                                         '*required for Salmonella')),  
                                div(class = 'inst', 
                                    'If the suspected agent is Salmonella,', 
                                    br(), '
                                    you must select a serotype. Other, ', br(), 
                                    'Unknown, and Multiple are options.')),
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
                sliderInput('Hosp', 'Cases Hospitalized', 
                            min = 0, 
                            max = 0, 
                            ticks = F, 
                            value = 0, step = 1)
                , style = "width:350px;"
                ), 
            # Right side of Sidebar Age inputs and submit button
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
                numericInput('AgeUnknown', 'Age Unknown', 
                             min = 0, 
                             max = 0, 
                             value = 0), 
                actionButton('.Submit', 'Submit')
                , style = "width:150px;"
                ), cellWidths = c('350px', '150px')
            ), 
            # instuction paragraph at the bottom of the sidebar
            # spans both models
            p(paste0('Instructions: This is a tool designed to help guide an ', 
                     'investigation of a foodbourne outbreak caused by ', 
                     'STEC or Salmonella. This is only ', 
                     'meant to be a guide and can only predict some of the ', 
                     'most common food sources. The more information you can ', 
                     'provide the better the prediction will preform. ', 
                     '**Additional instructions to be provided by Alice. **')), 
            style = 'width:550px;'
                     
            ), 
        
        # output results
        div(mainPanel(
            div(plotOutput("Results"), style = 'align:right'), 
            img(src = 'CoE logo no bckgrnd.png', class = 'logo')), 
            style = 'margin-left:550px')
    )
)


# Define server logic required to process inputs and produce results
server <- function(input, output, session) {
    
    # run model only once submit is pressed
    observeEvent(input$.Submit, {
        output$Results <- renderPlot({
            # isolate so that after submit is pressed, model is not rerun
            # until it is pressed again (prevent dynamic updating)
            isolate({
                total <- sum(input$AgeUnder1,
                             input$Age1to4,
                             input$Age5to9,
                             input$Age10to19,
                             input$Age20to49,
                             input$Age50to74,
                             input$Age75plus)
                # confirm all required fields are valid
                validate(
                    need(input$TotalCases > 0,
                         'You must have at least one case'),
                    need(length(input$Geography) > 0, 
                         'You must select a geography of expoure'), 
                    need(input$Month != "",
                         'You must choose a month'),
                    need(input$Agent != "",
                         paste0('You must choose an an infectious agent ',
                                'and a salmonella serotype if the agent is ',
                                'salmonella')),
                    need(total <= input$TotalCases,
                         paste0('The sum of number of cases in each age group ',
                                'can not exceed the total number of cases'))
                )
                # process inputs into a dataframe
                Inputs <- reactiveValuesToList(input)
                
                # convert misc salmonella serotypes to a format recognizable 
                # to the model
                Agent <- Inputs$Agent
                if(Agent %in% NonSpecific) Agent <- 'NonSpecific Sero group'
                if(Agent %in% PrimaryAnimal) Agent <-
                    "Primary Animal Sero group"
                if(Agent %in% PrimaryPlant) Agent <- "Primary Plant Sero group"
                Inputs$Agent <- Agent
                Inputs <- data.frame(Inputs)
                Inputs$Season <- cut(as.numeric(Inputs$Month),
                                     breaks = c(seq(1, 12, 3), 12),
                                     include.lowest = T, right = F,
                                     labels = c('Winter', 'Spring', 'Summer',
                                                'Fall'))
                # coverts raw numbers to percentages
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
                Inputs$HospPercent2 <- Inputs$Hosp/Inputs$TotalCases*100
                
                # keep only predictors in the model
                Inputs <- subset(Inputs,
                                 select = names(input_skeleton))
                
                # paste predictors into the empty data frame so levels/structure
                # is preserved
                input_skeleton[1, ] <- Inputs[1, ]
                pred <- predict(finalchoice, newdata = input_skeleton,
                                type = 'prob')
                # convert probabilites from 4 columns to 2 (one with source,
                # one with probality) for ggplot
                pred <- gather(pred, 1:4, key = 'source', value = 'prob')
                
                # plot
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
    })
    
    # update salmonella serotype dropdown based on STEC/salmonella selection
    observeEvent(input$.bacteria, {
        if(input$.bacteria == 1) choice <- list(" " = 'STEC' )
        if(input$.bacteria == 2) {
            excl <- which(names(Serogroups) == ' ')
            choice <- c(list('Choose one' = ""), 
                                            Serogroups[-excl])
            }
        updateSelectInput(session, 'Agent', choices = choice)
    })
    
    # update possibe gender, age amd hospilization cases based on total cases
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
        updateSliderInput(session, 'Hosp', 
                          max = input$TotalCases)
        ### Max doesn't really need to be updated as it doesn't prevent you
        ### from enteriing a higher value
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
    
    # update Female and Unknown Gender cases based on Male/Female cases
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
    
    # update unknown age to be difference of total cases and all age groups
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
