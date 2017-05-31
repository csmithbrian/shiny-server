

library(shiny)
library(networkD3)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("eSurvey Expected Completes Calculator"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("surveytype",
                  "Select Your Survey Type:",
                  c("CGCAHPS","ED","Outpatient")),
      textInput("sample","Enter Patient Volume (Number of Visits) for Facility:",value = 20000),
      #textOutput("patientvolumespiel"),
      #HTML(paste0("<br/>")),
      selectInput("methodology",
                  "Select Your Data Collection Methodology:",
                  c("eSurvey","eSurvey+SMS")),
      textOutput("surveytypetalk"),
      HTML(paste0("<br/>")),
      #sliderInput("emailcapturerate",
      #             "Email Capture Rate:",
      #             min = 1,
      #             max = 99,
      #             value = 55),
      uiOutput("starting_email_capture_rate"),
      htmlOutput("typical_emails"),
      textOutput("emailratespiel"),
      HTML(paste0("<br/>")),
      conditionalPanel(
        condition = "input.methodology == 'eSurvey+SMS'",
        uiOutput("starting_sms_capture_rate"),
        #sliderInput("mobilecapturerate",
        #          "Percentage of Phones Capable of Receiving SMS:",
        #          min = 1,
        #          max = 99,
        #          value = 65),
        htmlOutput("typical_mobiles"),
        textOutput("mobileratespiel")),
      downloadButton("downloadData", label = "Download an Excel version of this tool (with multiple facilities)")
        
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      sankeyNetworkOutput("sankeyPlot"),
      
      h3(htmlOutput("completesinfo")),
      conditionalPanel(
        condition = "input.methodology == 'eSurvey+SMS'",
        plotOutput("completesBar"))
      #conditionalPanel(
        #condition = "input.methodology == 'eSurvey+SMS'",
        #plotlyOutput("completesBar"))
    )
  )
))


##
## Completes volume chart
##
#plotDF <- data.frame('Sample'=c('Patients','Patients'),'Category'=c("Expected Non-Completes","Expected Completes"),"Volume"=c(non_completes,completes))
#ggplot(plotDF, aes(Sample,Volume,fill=Category)) +
#  +     geom_bar(stat = "identity") + coord_flip() + scale_fill_manual(values=c("#66CC99","#CC6666"))+ theme(legend.position = "right",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),legend.title = element_blank(),axis.title.x = element_blank(),axis.title.y=element_blank())

