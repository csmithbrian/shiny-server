#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rhandsontable)
question_dropdown <- c("Overall Rating","Likelihood to Recommend","Employee Satisfaction","Physician Satisfaction","Job Engagement","Organization Engagement","Other")

surveys <- c('Inpatient','Outpatient','ED','OP SDS/ASC','CGCAHPS(Phone 12M)','CGCAHPS(Phone Visit)','CGCAHPS(eSurvey 12M)','CGCAHPS(eSurvey Visit)','CG-CAHPS Visit Child','CG-CAHPS ACO','Adult PCMH','Child PCMH','Home Health','OP Behavioral','IP Behavioral','IP Rehab','OP Rehab','ED Peds','OP Peds','IP Peds','NICU','PCA','Hemodialysis','Urgent Care','Walk-In Clinic','IP Long term Care','Employee Insights','Physician Insights')
surveygroups <- data.frame(survey=surveys,group=c('Inpatient','OP / SDS','ED','OP / SDS','CG','CG','CG','CG','CG','CG','CG','CG','Other Patient','Other Patient','Other Patient','Inpatient','OP / SDS','ED','OP / SDS','Inpatient','Other Patient','Other Patient','Other Patient','ED','Other Patient','Other Patient','Employee','Physician'))

goals_lookup <- read.csv("lookup.csv")



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Goal Setting Tool"),
   
   # Sidebar with a slider input for number of bins 
   # sidebarLayout(
   #    sidebarPanel(
   #      
   #      selectInput("surveytype",'Survey Type:',
   #                  c('Inpatient','Outpatient','ED','OP SDS/ASC','CGCAHPS(Phone 12M)','CGCAHPS(Phone Visit)','CGCAHPS(eSurvey 12M)','CGCAHPS(eSurvey Visit)','CG-CAHPS Visit Child','CG-CAHPS ACO','Adult PCMH','Child PCMH','Home Health','OP Behavioral','IP Behavioral','IP Rehab','OP Rehab','ED Peds','OP Peds','IP Peds','NICU','PCA','Hemodialysis','Urgent Care','Walk-In Clinic','IP Long term Care','Employee Insights','Physician Insights')),
   #      
   #      wellPanel(
   #        h3("Table options"),
   #        radioButtons("useType", "Use Data Types", c("TRUE", "FALSE"))
   #    )
   #   
   #    
   #    ),
      
      # Show a plot of the generated distribution
      mainPanel(
         #plotOutput("distPlot"),
        selectInput(inputId = 'survey_type',label = "Survey Type",choices = surveys),
        rHandsontableOutput("hospitalInfo")
      )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  values <- reactiveValues(DF = data.frame('Survey' = rep(factor(surveys)[1],10),
                                           'Question' = rep(factor(question_dropdown)[1],10),
                                           'Facility.Name' = rep("Name",10),
                                           'Top.Box' = rep(0.00,10),
                                           Percentile = rep(integer(1), 10), 
                                           Standard.Goal = rep(0.0,10),
                                           Aggressive.Goal = rep(0.0,10),
                                           Stretch.Goal = rep(0.0,10),
                                           stringsAsFactors = FALSE))
  
  observe({
    if (!is.null(input$hospitalInfo)) {
      DF = hot_to_r(input$hospitalInfo)
    } else {
      if (is.null(values[["DF"]]))
        DF <- DF
      else
        DF <- values[["DF"]]
    }
    #new <- DF
    #DF[,5] <- goals_lookup$Goal_Standard[match(DF[,4],goals_lookup$LowerBound)]#new[] <- 
    DF <- merge(DF[,c(1,2,3,4,5)],goals_lookup,all.x = TRUE,all.y = FALSE,by.x = c("Question","Survey","Percentile"),by.y = c("Question","SurveyType","Percentile"),sort = FALSE)
    DF <- DF[,c(1,2,4,5,3,6,7,8)]
    values[["DF"]] <- DF
  })
  
  observe({
    if(!is.null(input$hospitalInfo))
      values$data <- hot_to_r(input$hospitalInfo)
  })
  
  
  output$hospitalInfo <- renderRHandsontable({
    DF <- values[["DF"]]
    if(!is.null(DF))
      rhandsontable(DF,stretchH = "none") %>% #,stretchH = "all"
      hot_col("Top.Box", format = "0.0%")
  }) 
  
  output$addingrowstext <- renderText("To add or remove rows, right click anywhere on the table and select either Insert Row above/below or Remove Row")

}

# Run the application 
shinyApp(ui = ui, server = server)

