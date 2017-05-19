#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#need a new way to get average ineligibles. Too much variation between esurvey and sms because in the former, dedupes are calculated after bad emails
#this gives the impression that ineligibles are much higher for sms, which is not the case.

library(shiny)
#install.packages('networkD3')
library(networkD3)
library(ggplot2)

indefinite_articles <- data.frame('SurvType'=c('CGCAHPS','Outpatient','ED'),'article'=c('a','an','an'))
ave_email_rates <- data.frame('SurvType'=c('CGCAHPS','Outpatient','ED'),'Rate'=c('56%','59%','57%'))
ave_mobile_rates <- data.frame('SurvType'=c('CGCAHPS','Outpatient','ED'),'Rate'=c('61%','55%','72%'))
ineligibles <- data.frame('SurvType'=rep(c('CGCAHPS','Outpatient','ED'),2),'Methodology'=c(rep('eSurvey',3),rep('eSurvey+SMS',3)),'Rate'=c(0.62,0.66,0.56,0.52,0.40,0.38))
response_rate_table <- data.frame('SurvType'=rep(c('CGCAHPS','Outpatient','ED'),2),'Methodology'=c(rep('eSurvey',3),rep('eSurvey+SMS',3)),'Rate'=c(0.22,0.15,0.07,0.15,0.07,0.04))
# Define server logic
shinyServer(function(input, output) {
  sms_dataset <- reactive({
    totalvolume <- as.integer(input$sample)
    emailrate <- input$emailcapturerate
    mobilerate <- input$mobilecapturerate
    ineligiblerate <- ineligibles$Rate[which(ineligibles$SurvType==input$surveytype & ineligibles$Methodology==input$methodology)]
    eligibles <- round(as.numeric(totalvolume)*(1-as.numeric(ineligiblerate)))
    ineligible_count <- round(as.numeric(totalvolume)*as.numeric(ineligiblerate))
    email <- round(eligibles*(.01*as.numeric(emailrate)))
    responserate <- response_rate_table$Rate[which(response_rate_table$SurvType==input$surveytype & response_rate_table$Methodology==input$methodology)]
    email_completes <- round(email*as.numeric(responserate))
    email_noncompletes <- round(email-(email_completes))
    sms <- round((eligibles-email)*(0.01*as.numeric(mobilerate)))
    no_contact <- round(eligibles-email-sms)
    sms_completes <- round(sms*as.numeric(responserate))
    sms_noncompletes <- round(sms-sms_completes)
    non_completes <- totalvolume-(email_completes-sms_completes)
    
  })
  email_dataset <- reactive({
    totalvolume <- as.integer(input$sample)
    emailrate <- input$emailcapturerate
    mobilerate <- input$mobilecapturerate
    ineligiblerate <- ineligibles$Rate[which(ineligibles$SurvType==input$surveytype & ineligibles$Methodology==input$methodology)]
    eligibles <- round(as.numeric(totalvolume)*(1-as.numeric(ineligiblerate)))
    ineligible_count <- round(as.numeric(totalvolume)*as.numeric(ineligiblerate))
    email <- round(eligibles*(.01*as.numeric(emailrate)))
    responserate <- response_rate_table$Rate[which(response_rate_table$SurvType==input$surveytype & response_rate_table$Methodology==input$methodology)]
    email_completes <- round(email*as.numeric(responserate))
    email_noncompletes <- round(email-(email_completes))
    no_contact <- round(eligibles-email)
    non_completes <- totalvolume-as.numeric(email_completes)
  })
  output$sankeyPlot <- renderSankeyNetwork({
    totalvolume <- input$sample
    emailrate <- input$emailcapturerate
    mobilerate <- input$mobilecapturerate
    ineligiblerate <- ineligibles$Rate[which(ineligibles$SurvType==input$surveytype & ineligibles$Methodology==input$methodology)]
    eligibles <- round(as.numeric(totalvolume)*(1-as.numeric(ineligiblerate)))
    ineligible_count <- round(as.numeric(totalvolume)*as.numeric(ineligiblerate))
    email <- round(eligibles*(.01*as.numeric(emailrate)))
    responserate <- response_rate_table$Rate[which(response_rate_table$SurvType==input$surveytype & response_rate_table$Methodology==input$methodology)]
    email_completes <- round(email*as.numeric(responserate))
    email_noncompletes <- round(email-(email_completes))
    if(input$methodology=='eSurvey+SMS'){
      sms <- round((eligibles-email)*(0.01*as.numeric(mobilerate)))
      no_contact <- round(eligibles-email-sms)
      sms_completes <- round(sms*as.numeric(responserate))
      sms_noncompletes <- round(sms-sms_completes)
      nodes <- data.frame("name"=c(paste0('Total (',totalvolume,')'),'','Eligible','Ineligible to participate in survey','Eligible patients with email','Eligible patients with mobile phone #',
                                   'No contact info','Expected email completes','Expected sms completes','No response to email invite','No response to sms invite'))
      links <- data.frame('source'=c(0,1,1,2,2,2,4,4,5,5),
                        'target'=c(1,2,3,4,5,6,7,9,10,8),
                        'value'=c(totalvolume,eligibles,ineligible_count,email,sms,no_contact,email_completes,email_noncompletes,sms_noncompletes,sms_completes))
    }
    else{ #eSurvey only
      no_contact <- round(eligibles-email)
      nodes <- data.frame("name"=c(paste0('Total (',totalvolume,')'),'','Eligible','Ineligible to participate in survey','Eligible patients with email',
                          'No contact info','Expected email completes','No response to email invite'))
      links <- data.frame('source'=c(0,1,1,2,2,4,4),
                          'target'=c(1,2,3,4,5,6,7),
                          'value'=c(totalvolume,eligibles,ineligible_count,email,no_contact,email_completes,email_noncompletes))
    
    }  
  completes <- list(nodes=nodes,links=links)
  sankeyNetwork(Links = completes$links, Nodes = completes$nodes, Source = "source",
                Target = "target", Value = "value", NodeID = "name",
                units = "patients", fontSize = 12, nodeWidth = 30,nodePadding = 20)
  
  })
  output$completesinfo <- renderUI({
    totalvolume <- input$sample
    emailrate <- input$emailcapturerate
    mobilerate <- input$mobilecapturerate
    ineligiblerate <- ineligibles$Rate[which(ineligibles$SurvType==input$surveytype & ineligibles$Methodology==input$methodology)]
    eligibles <- round(as.numeric(totalvolume)*(1-as.numeric(ineligiblerate)))
    ineligible_count <- round(as.numeric(totalvolume)*as.numeric(ineligiblerate))
    email <- round(eligibles*(.01*as.numeric(emailrate)))
    sms <- round((eligibles-email)*(0.01*as.numeric(mobilerate)))
    if(input$methodology=='eSurvey+SMS'){no_contact <- round(eligibles-email-sms)}
    else{
      no_contact <- round(eligibles-email)
    }
    responserate <- response_rate_table$Rate[which(response_rate_table$SurvType==input$surveytype & response_rate_table$Methodology==input$methodology)]
    email_completes <- round(email*as.numeric(responserate))
    email_noncompletes <- round(email-(email_completes))
    sms_completes <- round(sms*as.numeric(responserate))
    sms_noncompletes <- round(sms-sms_completes)
    if(input$methodology=='eSurvey+SMS'){
      HTML(paste0("Expected Total Completes: ",sms_completes+email_completes,"  (Expected Email Completes: ",email_completes,";"," Expected SMS Completes: ",sms_completes,")"))
    }
    else{
      HTML(paste0("Expected Total Completes: ",email_completes))
    }
    
  })
  
  output$completesBar <- renderPlot({
    totalvolume <- as.integer(input$sample)
    emailrate <- input$emailcapturerate
    mobilerate <- input$mobilecapturerate
    ineligiblerate <- ineligibles$Rate[which(ineligibles$SurvType==input$surveytype & ineligibles$Methodology==input$methodology)]
    eligibles <- round(as.numeric(totalvolume)*(1-as.numeric(ineligiblerate)))
    ineligible_count <- round(as.numeric(totalvolume)*as.numeric(ineligiblerate))
    email <- round(eligibles*(.01*as.numeric(emailrate)))
    responserate <- response_rate_table$Rate[which(response_rate_table$SurvType==input$surveytype & response_rate_table$Methodology==input$methodology)]
    email_completes <- round(email*as.numeric(responserate))
    email_noncompletes <- round(email-(email_completes))
    if(input$methodology=='eSurvey+SMS'){
      sms <- round((eligibles-email)*(0.01*as.numeric(mobilerate)))
      no_contact <- round(eligibles-email-sms)
      sms_completes <- round(sms*as.numeric(responserate))
      sms_noncompletes <- round(sms-sms_completes)
      non_completes <- totalvolume-(email_completes-sms_completes)
      colors1 <- c("#66CC99","#0eaf5f")
      plotDF <- data.frame('Sample'=c('Patients','Patients'),'Category'=c("Expected Total Email Completes","Expected Total SMS Completes"),"Volume"=c(email_completes,sms_completes))
    }
    else{ #eSurvey only
      no_contact <- round(eligibles-email)
      non_completes <- totalvolume-as.numeric(email_completes)
      colors1 <- c("#CC6666","#66CC99")
      plotDF <- data.frame('Sample'=c('Patients','Patients'),'Category'=c("Expected Non-Completes","Expected Total Completes"),"Volume"=c(non_completes,email_completes))
    }  
    #PLOT
    ggplot(plotDF, aes(Sample,Volume,fill=Category)) +
             geom_bar(stat = "identity",width = .2) + coord_flip() + scale_fill_manual(values=colors1)+ theme(legend.position= "right",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),legend.title = element_blank(),axis.title.x = element_blank(),axis.title.y=element_blank())
  })
  
  
  
  
  
  output$downloadData <- downloadHandler(
    filename <- function() {
      paste("output", "zip", sep=".")
    },
    content <- function(file) {
      file.copy("out.zip", file)
    },
    contentType = "application/zip"
  )
  output$mobileratespiel <- renderText({"This is the percentage of phone numbers you collect that are mobile phones. This is an estimation. Data collected by HealthStream has shown this number to typically range from 60-75%. Rural hospitals tend to have lower numbers, urban hospitals tend to have higher numbers. ED facilities tend to have higher rates than outpatient facilities and provider offices (CGCAHPS)."})
  output$emailratespiel <- renderText({"The email capture rate is the percentage of patient records with a valid email address. Non-valid emails (eg Declined@YourHosp.com) will give the illusion of a bigger sample frame, but ultimately will have no effect on completes, and will even be detrimental if you select SMS - patients with email addresses that are not valid will be sent emails they will never receive instead of receiving an SMS."})
  output$typical_emails <- renderUI({
    HTML(paste0("<b>",'The typical email capture rate for ',indefinite_articles$article[which(indefinite_articles$SurvType==input$surveytype)],
                                              ' ',input$surveytype,' facility is ',ave_email_rates$Rate[which(ave_email_rates$SurvType==input$surveytype)],'.',"</b>"))
    })
  output$typical_mobiles <- renderUI({
    HTML(paste0("<b>",'The typical percentage of phones that are capable of receiving SMS for ',indefinite_articles$article[which(indefinite_articles$SurvType==input$surveytype)],
                                              ' ',input$surveytype,' facility is ',ave_mobile_rates$Rate[which(ave_mobile_rates$SurvType==input$surveytype)],'.',"</b>"))
    })
  output$patientvolumespiel <- renderText({"This is the typical number of patients that visit your hospital each year. This is NOT limited to unique visits. For instance, if the same person visits your hospital five times, it would count as five visits."})
  output$surveytypetalk <- renderText({paste0("The survey type and methodology you choose affects how your sample is divided up. For ",
                                              indefinite_articles$article[which(indefinite_articles$SurvType==input$surveytype)],' ',input$surveytype,
                                              ' facility using the ',input$methodology,' methodology, typically ',as.character(100*ineligibles$Rate[which(ineligibles$SurvType==input$surveytype & ineligibles$Methodology==input$methodology)]),'% of records will be ineligible for data collection due to duplication, age restrictions, or other restrictions.')})
})
