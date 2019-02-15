#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny);require(ggplot2);require(ggimage)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("vizMort: visualizing mortality stats"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(width=2,
        fluidRow(
          column(12, uiOutput("outOf"))
            ),
        fluidRow(
          checkboxInput("scalepts","Autoscale Points?",value=T),
          sliderInput("pointcex","Manual point scale",min=.001,max=.8,value=.01,step=.001),
          sliderInput("grafsize","Plot size",min=300,max=1000,value=400,step=20),
          actionButton("update","Update")
            
          )
        ),
    
      
      # Show a vizualization
      mainPanel(width=10,
         uiOutput("graf"),#plotOutput("viz",height=600),
        downloadButton("down",label="Download Plot")
      )
   )
)




 #--------------------------------------------------------
# Define server logic 
server <- function(input, output) {
   #Render input sliders that actually respect min/max values
   N=100#init value
  N <-  eventReactive(input$update,valueExpr=input$numba)
  G<-reactiveValues() #Used to access graph outside renderPlot
   
   
   output$outOf <- renderUI({
    numericInput(inputId = "numba", label = "Number of deaths (integer)", min = 1, max = 1e9,value=100) #2
    })
   
   output$viz<- renderPlot({
     ico<-"person.png" #"vizMort/person.png" #(2nd for local ref)
     
print(N())
     #Autoscale vs Manual point scaling
     if(input$scalepts==F){pts<-input$pointcex}else{
       #slightly different models for pt size before &after 9999
           if(N()<500){pts=1.9*exp(-1*N())+.1}else{pts=(.011-.045)/(10e3-500)*N()+.046789}
     }
       
       
       
     #        #slightly different models for pt size before &after 9999
     #       if(N>9999){pts=(30*exp(-.006*N)+.01)/200}else{pts=.8*exp(-.02*N)+.0001}}#pts=(20*exp(-.002*N)+.5)/200} }
     
     root<-sqrt(N())
     lim<-ceiling(root) #What's the nearest integer length of root N
     remainder<-ifelse(root==lim,0,(lim^2-N())) #if square axes don't equal root N, calculate how many points to remove
     d<-expand.grid(x=1:lim,y=1:lim)
     d<-d[order(d$x),]
     if(remainder!=0){ #If the grid isn't square, remove the remainder values
       remove.indx<-(nrow(d)-remainder):nrow(d)
       d<-d[-remove.indx[-1],]#remove[-1] first element bc of greedy remove.indx definition (remainder=1 should just remove 1 element)
     }
     mytheme<-theme_void()
     g<-ggplot(d,aes(x=jitter(x),y=jitter(y)))+geom_image(image=ico,alpha=1,size=pts) +xlab("")+ylab("")+xlim(min(d$x)-1,max(d$x)+1)+ylim(min(d$y)-1,max(d$y)+1)+mytheme+ guides(fill = guide_legend(title = "LEFT", title.position = "left"))+coord_fixed()
     
     print(pts)
     #####The tricky part: if N>100k
     if(N()>1000000){
       binmagnitude<-round(log10(N()))-3
       g <- g+stat_summary_2d()
     }
     
     
     # ttl<-substitute( paste(frac(k,N),warn),list(k=format(k, big.mark=",", scientific=FALSE),N=format(N, big.mark=",", scientific=FALSE),warn=warn))
     ttl<-paste(N(),"Deaths")
     
     g<-g+ ggtitle(ttl)+theme(plot.title = element_text(hjust = 0.5,size=34,face="bold",colour="#011627",family="serif"))#,plot.background = element_rect(colour = '#011627',size=1)
     G$plot<-g #assign reactive value
     g
     
      })#end renderPlot
   
   output$down<-downloadHandler("vizN.jpeg",
     content=function(file){
       ggsave(file,plot=G$plot,width=8,height=9)
       }
   )

  output$graf<-renderUI({
    plotOutput("viz",height=input$grafsize,width=input$grafsize)
  })   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

