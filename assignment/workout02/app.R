#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Visualization of growth rate"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
    column(4,
         sliderInput("amount",
                     "initial amount:",
                     min = 0,
                     max = 100000,
                     value = 1000,
                     step = 500),
         sliderInput("contrib",
                     "Annual contribution",
                     min=0,
                     max=50000,
                     value=2000,
                     step=500)
      ),
    column(4,
           sliderInput("return",
                       "Return Rate (in %)",
                       min=0,
                       max=20,
                       step = 0.1,
                       value = 5),
           sliderInput("growth",
                       "Growth Rate (in %)",
                       min=0,
                       max=20,
                       step=0.1,
                       value=2)
      ),
    column(4,
           sliderInput("years",
                       "Years",
                       min=0,
                       max=50,
                       step = 1,
                       value=20),
           selectInput("facet",
                       "Facet?",
                       c("No","Yes"))
    ),
    
      
      # Show a plot of the generated distribution
      mainPanel(
        h3("Timelines"),
         plotOutput("distPlot"),
        h3("Balances"),
         verbatimTextOutput("table")
      )
   )
)

# functions that need to use
#' @title Future Value Function
#' @description compute the future value of an investment
#' @param amount: the intial invested amount
#' @param rate: annual rate of return
#' @param years: number of years
#' @return the computed future value of an investment

future_value <- function(amount, rate, years) {
  if (!is.numeric(c(amount,rate,years))) {
    stop("the inputs should be numeric")
  }
  return(amount*(1+rate)^years)
}
#' @title Future Value of Annuity
#' @description compute the future value of annuity
#' @param contrib: contributed amount
#' @param rate: annual rate of return
#' @param years: number of years
#' @return the computed future value of annuity

annuity <- function(contrib, rate, years) {
  return(contrib*((1+rate)^years-1)/rate)
}
#' @title Future Value of Growing Annuity
#' @description compute the future value of the growing annuity
#' @param contrib: contributed amount
#' @param rate: annual rate of return
#' @param growth: annual growth rate
#' @param years: number of years

growing_annuity <- function(contrib, rate, growth, years) {
  return(contrib*(((1+rate)^years-(1+growth)^years)/(rate-growth)))
}

# Define server logic required to draw a histogram
server <- function(input, output) {
   modalities <- reactive({
     modalities <- matrix(NA,nrow = input$years+1,ncol = 3)
     r=input$return/100
     g=input$growth/100
      
   for (j in 1:ncol(modalities)) {
     for (i in 1:nrow(modalities)) {
       if (j==1) {
         modalities[i,j] <- future_value(input$amount,r,i-1)
       } else if (j==2) {
         modalities[i,j] <- future_value(input$amount,r,i-1)+annuity(input$contrib,r,i-1)
       } else {
         modalities[i,j] <- future_value(input$amount,r,i-1)+growing_annuity(input$contrib,r,g,i-1)
       }
     }
   }
   modalities <- data.frame(year=0:input$years,no_contrib=modalities[,1],fixed_contrib=modalities[,2],growing_contrib=modalities[,3])
   return(modalities)
   
   })
   output$distPlot <- renderPlot({ 
     if (input$facet=="No") {
       gg <- ggplot(modalities())+geom_line(aes(x=year,y=no_contrib,color="no_contrib"))+
         geom_point(aes(x=year,no_contrib),colour="black")+
         geom_line(aes(x=year,y=fixed_contrib,color="fixed_contrib"))+
         geom_point(aes(x=year,y=fixed_contrib),colour="blue")+
         geom_line(aes(x=year,y=growing_contrib,color="growing_contrib"))+
         geom_point(aes(x=year,y=growing_contrib),colour="red")+
         scale_color_manual(name="variable",values = c("no_contrib"="black","fixed_contrib"="blue","growing_contrib"="red"))+
         xlab("number of years")+ylab("future value of investment")+
         ggtitle("Three modes of investing")+
         xlim(0,input$years)+theme_bw()
     } else {
     modalities <- modalities()
     modalities <- melt(modalities,id.vars = "year")
     gg <- ggplot(data = modalities,aes(x=year,y=value))+geom_line(aes(color=variable))+
       geom_area(aes(fill=variable),alpha=0.5)+
       geom_point(aes(color=variable))+facet_grid(~variable)+ggtitle("Three modes of investing")
     }
     gg
   })
   output$table <- renderPrint({
     modalities <- modalities()
     modalities
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

