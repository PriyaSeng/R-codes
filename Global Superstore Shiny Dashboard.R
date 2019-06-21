
library("shiny")
library("ggplot2")
library("shinyWidgets")


storedata<-read.csv("Global Superstore 2016 Orders(1) (1).csv")

ui<-shinyUI(fluidPage(
  setBackgroundColor(color = "lightblue", gradient = c("linear",
                                                       "radial"), direction = c("bottom", "top", "right", "left")),
  titlePanel("Boxplot of Global Superstore 2016 Orders by Segment and Category"),
  sidebarPanel(
    conditionalPanel(
      'input.stuff === "Segment"',
     helpText("Boxplot displaying the relationship between Sales,shipping cost and Discount by Segment"),
      selectInput("x", "Select the variables of storedataset:",
                  list("Sales"='a', "Shipping.cost"='b',"Discount"='c'))
      
      
      ),
    conditionalPanel(
      'input.stuff === "Category"',
      helpText("Boxplot displaying the relationship between Sales,shipping cost and Discount by Category"),
      selectInput("z", "Select the variables of storedataset:",
                  list("Sales"='d', "Shipping.cost"='e',"Discount"='f'))
      
      )
    
    
  ),
  
  mainPanel(
    tabsetPanel(
     id='stuff',
      tabPanel('Segment', plotOutput("box",height = "600px")),
      tabPanel('Category', plotOutput("box1",height="600px"))
    
    )
  )
  )
  )


server<-function(input,output){
  output$box <- renderPlot({
    storedata_reduced<- storedata[,c("Sales","Shipping.Cost","Discount","Segment","Category")]
    
    
    if(input$x=='a'){
      i<-1
    }
    
    if(input$x=='b'){
      i<-2
    }
    if(input$x=='c'){
      i<-3
    }
    
   p<-ggplot(storedata_reduced, aes(x=storedata_reduced$Segment,y=storedata_reduced[[i]],color=Segment))+geom_boxplot(outlier.color = "blue",outlier.shape = 8,outlier.size=4)+labs(title = "Selected Variable vs Segment",x="Segment",y="Quantity")+theme(legend.position = "none")+theme(axis.title.x=element_blank())+ theme(axis.title.y=element_blank())+theme(axis.title=element_text(size=25,face="bold"))+theme(axis.text=element_text(size=15,face = "bold"))+ theme(plot.title = element_text(size = 20))
   p
  })
   
  
  output$box1 <- renderPlot({
    storedata_reduced<- storedata[,c("Sales","Shipping.Cost","Discount","Segment","Category")]
    
    
 
    if(input$z=='d'){
      i<-1
    }
    
    if(input$z=='e'){
      i<-2
    }
    if(input$z=='f'){
      i<-3
    }
    
   q<-ggplot(storedata_reduced, aes(x=storedata_reduced$Category,y=storedata_reduced[[i]],color=Category))+geom_boxplot(outlier.color = "blue",outlier.shape = 8,outlier.size=4)+labs(title = "Selected Variable vs Category",x="Segment",y="Numeric Quantity")+theme(legend.position = "none")+ theme(axis.title.x=element_blank())+ theme(axis.title.y=element_blank())+theme(axis.title=element_text(size=25,face="bold"))+theme(axis.text=element_text(size=15,face = "bold"))+theme(plot.title = element_text(size = 20))
   q
  })
  
  
}



shinyApp(ui, server)    

