library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(sqldf)

healthcare<-read.csv("ABC Healthcare Company Claims Dataset (1).csv")

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Claims Dashboard")  
#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    radioButtons(inputId = "claim",
                label = "Claim:",
                c("Total Paid insurance" = 1,
                  "Patient copayment" = 2),
                selected = 1)
  )
  
)

frow1 <- fluidRow(
  valueBoxOutput("value1"),
  valueBoxOutput("value2"),
  valueBoxOutput("value3")
  
)
frow2 <- fluidRow( 
    box(
      title = "CLAIM PAID"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE
      ,height = "570px"
      ,width="570px"
      ,plotOutput("CLAIM", height = "450px",width = "1300px")
    )
)
  
  

# combine the two fluid rows to make the body
body <- dashboardBody(frow1,frow2)

ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='red')

server<-shinyServer(function(input, output) {
  a<-sqldf("SELECT PATIENT_STATE,SUM(TOTAL_PAID_BY_INSURANCE) FROM healthcare GROUP BY PATIENT_STATE")
  names(a)<-c("patientstate","Insurance")
  b<-sqldf("SELECT PROVIDER_SPECIALTY,SUM(TOTAL_PAID_BY_INSURANCE) FROM healthcare GROUP BY PROVIDER_SPECIALTY")
  names(b)<-c("providerspeciality","Insurance")
  c<-sqldf("SELECT PATIENT_STATE,SUM(PATIENT_COPAYMENT) FROM healthcare GROUP BY PATIENT_STATE")
  names(c)<-c("patientstate","copayment")
  d<-sqldf("SELECT PROVIDER_SPECIALTY,SUM(PATIENT_COPAYMENT) FROM healthcare GROUP BY PROVIDER_SPECIALTY")
  names(d)<-c("providerspeciality","copayment")
  total.insurance <- sum(healthcare$TOTAL_PAID_BY_INSURANCE)
  insurance.state <- healthcare %>% group_by(PATIENT_STATE) %>% summarise(value = sum(TOTAL_PAID_BY_INSURANCE)) %>% filter(value==max(value))
  insurance.speciality <- healthcare %>% group_by(PROVIDER_SPECIALTY) %>% summarise(value = sum(TOTAL_PAID_BY_INSURANCE)) %>% filter(value==max(value))
  total.copayment <- sum(healthcare$PATIENT_COPAYMENT)
  copayment.state <- healthcare %>% group_by(PATIENT_STATE) %>% summarise(value = sum(PATIENT_COPAYMENT)) %>% filter(value==max(value))
  copayment.speciality <- healthcare %>% group_by(PROVIDER_SPECIALTY) %>% summarise(value = sum(PATIENT_COPAYMENT)) %>% filter(value==max(value))
  output$value1 <- renderValueBox({
    if(input$claim==1){
    valueBox(
      formatC(insurance.state$value, format="d", big.mark=',')
      ,paste('Top state:',insurance.state$PATIENT_STATE)
      ,icon = icon("dollar")
      ,color = "orange")
    }
      else{
        valueBox(
          formatC(copayment.state$value, format="d", big.mark=',')
          ,paste('Top state:',copayment.state$PATIENT_STATE)
          ,icon = icon("dollar")
          ,color = "orange") 
    }    
  })
  output$value2 <- renderValueBox({ 
    if(input$claim==1){
    valueBox(
      formatC(total.insurance, format="d", big.mark=',')
      ,'Total Insurance'
      ,icon = icon("dollar")
      ,color = "blue")
    }
    else{
      valueBox(
        formatC(total.copayment, format="d", big.mark=',')
        ,'Total Insurance'
        ,icon = icon("dollar")
        ,color = "blue")
    }
  })
  output$value3 <- renderValueBox({
    if(input$claim==1){
    valueBox(
      formatC(insurance.speciality$value, format="d", big.mark=',')
      ,paste('Top Provider Speciality:',insurance.speciality$PROVIDER_SPECIALTY)
      ,icon = icon("dollar")
      ,color = "green")
    }
    else{
      valueBox(
        formatC(copayment.speciality$value, format="d", big.mark=',')
        ,paste('Top Provider Speciality:',insurance.speciality$PROVIDER_SPECIALTY)
        ,icon = icon("dollar")
        ,color = "green")
    } 
  })

  output$CLAIM<-renderPlot({
    if(input$claim==1){
    gridExtra::grid.arrange(
      ggplot(a,aes(x=patientstate, y=Insurance,fill=patientstate,color=patientstate,label=Insurance))+geom_bar(stat="identity")+labs(x="State",y="Total Paid insurance")+geom_text(nudge_y = 1,hjust=0.5,vjust=-0.25)+theme(legend.position = "none")+theme(axis.title=element_text(size=15,face="bold"))+theme(axis.text=element_text(size=9,face = "bold"))+ theme(plot.title = element_text(size = 15,face = "bold")), 
      ggplot(b,aes(x=providerspeciality, y=Insurance,fill=providerspeciality,color=providerspeciality,label=Insurance))+geom_bar(stat="identity")+labs(x="Provider Specialty",y="Total Paid Insurance")+geom_text(nudge_y = 1,hjust=0.5,vjust=-0.25)+theme(legend.position = "none")+ylim(0,250000)+theme(axis.title=element_text(size=15,face="bold"))+theme(axis.text=element_text(size=9,face = "bold"))+theme(plot.title = element_text(size = 15,face = "bold")),
      nrow = 1
    )
    } 
    if(input$claim==2){
      gridExtra::grid.arrange(
        ggplot(c,aes(x=patientstate, y=copayment,fill=patientstate,color=patientstate,label=copayment))+geom_bar(stat = "identity")+labs(x="State",y="Patient Copayment")+geom_text(nudge_y = 1,hjust=0.5,vjust=-0.25)+theme(legend.position = "none")+theme(axis.title=element_text(size=15,face="bold"))+theme(axis.text=element_text(size=9,face = "bold"))+ theme(plot.title = element_text(size = 15,face = "bold")), 
        ggplot(d,aes(x=providerspeciality, y=copayment,fill=providerspeciality,color=providerspeciality,label=copayment))+geom_bar(stat="identity")+labs(x="Provider Specialty",y="Patient Copayment")+geom_text(nudge_y = 1,hjust=0.5,vjust=-0.25)+theme(legend.position = "none")+ylim(0,6000)+theme(axis.title=element_text(size=15,face="bold"))+theme(axis.text=element_text(size=9,face = "bold"))+theme(plot.title = element_text(size = 15,face = "bold")),
        nrow = 1
      )
    }  
  })
})  
shinyApp(ui,server)