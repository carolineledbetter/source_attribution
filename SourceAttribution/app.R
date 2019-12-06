###########################################
# Project: P1330White
# Author: Caroline Ledbetter
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
library(ranger) # necessary to predict with algorithm we chose
library(tidyverse)
library(recipes) # used recipe to bake input
library(caret)

# load prediction algortithm
load(file = 'ranger_model_obj.rda')

# load empty data frame with required predictors and possible levels
load(file = 'file_skeleton.rda')

# classify rare serotypes according to primary source
load(file = "sero_groupings.rda")

load(file = 'recipe.rda')

# format salmonella serotypes for tidy appearance in webplatform\

sero_groupings <- sero_groupings %>% 
    arrange(serotype) %>% 
    add_row(serogroup = 'rare', serotype = "Other") %>% 
    add_row(serogroup = '(missing)', serotype = 'Unknown') 

salm_sero <- sero_groupings %>% 
    filter(serotype != 'STEC') %>% 
    mutate(serogroup = serotype) %>% 
    deframe() %>% 
    as.list()



# format Geography for tidy appearance in webplatform
geography <- as.list(levels(skeleton$geography))
names(geography) <- str_to_title(str_replace(levels(skeleton$geography), 
                                  pattern = '_', 
                                  replacement = ' '))

# Set up named month list
months <- as.list(1:12)
names(months) <- month.name

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
                    
                selectInput('month', 
                            span("Month of first illness onset", 
                                 div(class = 'required', 
                                     '*required')), 
                            choices = c('Choose one' = '', 
                                        months)), 
                radioButtons('geography', 
                             label = span(
                                 span(class = 'inst', 
                                      "Geography of exposures", 
                                      div(class = 'required', 
                                          '*required')),  
                                 div(class = 'inst', 
                                     'The geography of the suspected exposures,',  
                                     br(), 
                                     'not the residence of the cases.')), 
                             choices = geography, 
                             selected = character(0)),
                radioButtons('.bacteria', 
                             span('Infectious Agent', 
                                  div(class = 'required', 
                                      '*required')), 
                             selected =  character(0), 
                             choiceNames = c('STEC', 'Salmonella'), 
                             choiceValues = 1:2), 
                selectInput('serotype', 
                            label = span(
                                span(class = 'inst', 
                                     'Salmonella Serotype', 
                                     div(class = 'required', 
                                         '*required for Salmonella')),  
                                div(class = 'inst', 
                                    'If the suspected agent is Salmonella,', 
                                    br(), '
                                    you must select a serotype. Other, ', br(), 
                                    'and Unknown are options.')),
                            choices = c('Choose STEC or Salmonella First' = "")), 
                actionButton('.cal_graph', 'Calibration Graph'), 
                style = "width:350px;"
                ), 
            # Right side of Sidebar Age inputs and submit button
            wellPanel(
                numericInput('male', 'Male', 
                                  min = 0, 
                                  max = 0, 
                                  value = 0),
                numericInput('female', 'Female', 
                                  min = 0, 
                                  max = 0, 
                                  value = 0)
                      ,
                numericInput('age_under1', 'Under 1 Year', 
                             min = 0, 
                             max = Inf, 
                             value = 0), 
                numericInput('age1to4', '1 yr to 4 yrs', 
                             min = 0, 
                             max = Inf, 
                             value = 0), 
                numericInput('age5to19', '5 yrs to 19 yrs', 
                             min = 0, 
                             max = Inf, 
                             value = 0),             
                numericInput('age20to49', '20 yrs to 49 yrs', 
                             min = 0, 
                             max = Inf, 
                             value = 0), 
                numericInput('age50plus', '50 yrs or older', 
                             min = 0, 
                             max = Inf, 
                             value = 0),
                actionButton('.Submit', 'Submit')
                , style = "width:150px;"
                ), cellWidths = c('350px', '150px')
            ), 
            # instuction paragraph at the bottom of the sidebar
            # spans both models
            p('Outbreak Source Prediction Tool ',
              'Developed by the Colorado Integrated Food Safety ', 
              'Center of Excellence, the Outbreak Source Prediction ', 
              'Tool is a resource for public health professionals to ', 
              'help with hypothesis generation during an enteric ', 
              'disease outbreak investigation. The tool was ', 
              'developed using statistical prediction methods ', 
              "(code can be found ", 
              a(href = "https://github.com/ledbettc/p1330_white", 
                "here"), "). ", 
              'and historical Salmonella and shiga ', 
              'toxin-producing E.coli outbreak data from the ', 
              a(href = "https://www.cdc.gov/nors/index.html", 
                "National Outbreak Reporting System at the Centers ", 
                "for Disease Control and Prevention"), ".",  
              br(), 
              'When using this tool to help with hypothesis ', 
              'generation during an outbreak investigation ', 
              'outbreak, simply enter the most current ', 
              'information available. The more information you ', 
              'have available, the better the prediction tool ', 
              'will perform. Total cases, month of first illness ', 
              "onset, geography, and agent are required fields.", br(), 
              "After entering available information, click ‘Submit’ ", 
              "and a graph will appear with common sources ", 
              "and their ", 
              "associated probabilities. The numbers above the bars ", 
              "are the probabilities that your outbreak is associated ", 
              "with each of the sources based on the information ", 
              "you entered.", br(), 
              "The tool is intended to be used, along with other ", 
              "resources, as a guide during hypothesis generation. ", 
              "This and other hypothesis generation resources should ", 
              "not be used in replace of an epidemiological study or ", 
              "other outbreak investigation activities.", br(), 
              "For feedback, questions, or comments, please contact ", 
              "us.", br(), 
              "For additional resources on foodborne outbreak",  
              "investigation, visit ", 
              a(href = "http://www.COFoodSafety.org", 
                "www.COFoodSafety.org")
            ), 
        style = 'width:550px;'
                     
            ), 
        
        # output results
        div(mainPanel(
            div(plotOutput("Results"), style = 'align:right'), 
            a(href = "http://www.COFoodSafety.org", 
              img(src = 'CoE logo no bckgrnd.png', class = 'logo'))), 
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
                total <- sum(input$age_under1,
                             input$age1to4,
                             input$age5to19,
                             input$age20to49,
                             input$age50plus)

                # confirm all required fields are valid
                validate(
                    need(input$TotalCases > 0,
                         'You must have at least one case'),
                    need(length(input$geography) > 0,
                         'You must select a geography of expoure'),
                    need(input$month != "",
                         'You must choose a month'),
                    need(input$serotype != "",
                         paste0('You must choose an an infectious agent ',
                                'and a salmonella serotype if the agent is ',
                                'salmonella')),
                    need(total <= input$TotalCases,
                         paste0('The sum of number of cases in each age group ',
                                'can not exceed the total number of cases'))
                )
                # process inputs into a dataframe
                inputs <- reactiveValuesToList(input)
                
                inputs <- as_tibble(inputs) %>%
                    mutate_at(vars(starts_with('age')),
                              ~ ./(total)) %>%
                    mutate(female = female/(male + female),
                           attr_source = NA,
                           month = as.numeric(month)) %>%
                    rename_at(vars(female, starts_with('age')),
                              ~ paste0('percent_', .)) %>% 
                    left_join(sero_groupings)
               
                # bake and run
                data_to_predict <- recipe %>%
                    bake(inputs)
                
                pred <- predict(final_model, newdata = data_to_predict,
                                type = 'prob') %>%
                    pivot_longer(everything(), names_to = 'predicted_cat',
                                 values_to = 'predicted_value') %>% 
                    mutate(predicted_cat = str_to_title(
                        str_replace(predicted_cat, 
                                    pattern = '_', 
                                    replacement = ' ')
                        ))

                # plot
                ggplot(data = pred, aes(x = predicted_cat,
                                        y = predicted_value)) +
                    geom_col(aes(fill = predicted_cat)) +
                    theme_classic() +
                    labs(x = 'Potential Source',
                         y = 'Predicted Probability',
                         title = 'Predicted Probability of Potential Sources') +
                    guides(fill = 'none') +
                    geom_label(aes(label = round(predicted_value, 2))) + 
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))
            })
        }, width = 800, height = 800, res = 200)
    })

    # update salmonella serotype dropdown based on STEC/salmonella selection
    observeEvent(input$.bacteria, {
        if(input$.bacteria == 1) choice <- list(" " = 'STEC')
        if(input$.bacteria == 2) choice <- salm_sero
        updateSelectInput(session, 'serotype', choices = choice)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
