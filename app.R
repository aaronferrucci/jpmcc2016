  #
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(gridExtra)

source("jpmcc2016.R")
data <- getData()
data <- cleanData(data)
company_table <- table(data$Company)
company_levels <- names(company_table)[order(company_table)]
company_levels <- names(company_table)[order(company_table, decreasing=TRUE)]
data$Company.Sort <- factor(data$Company, levels=company_levels)
top20 <- data %>% filter(Company %in% company_levels[1:20])
company_names <- unique(top20$Company)

time.breaks <- seq(0, 4800, 60 * 10)


# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("JP Morgan Chase Corporate Challenge - SF 2016 - Company Comparison, Top 20"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       selectInput('co1', 'Company #1', company_names, "SALESFORCE"),
       selectInput('co2', 'Company #2', company_names, "J.P. MORGAN CHASE"),
       selectInput('co3', 'Company #3', company_names, "GENENTECH, INC."),
       checkboxInput('smooth', 'Enable Smoother', FALSE),
       checkboxInput('jitter', 'Enable Jitter', FALSE)
     ),
     
      # Show a plot of the generated distribution
      mainPanel(
         tabsetPanel(
           tabPanel("Plot",
             br(),
             column(12, plotOutput("plot")),
             br(),
             tableOutput("summary_table")        
           )
         )
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
   output$plot <- renderPlot({
     sel <- top20 %>% filter(Company %in% input$co1 | Company %in% input$co2 | Company %in% input$co3)
     sel$Company.Sort <- factor(sel$Company, levels=c(input$co1, input$co2, input$co3))
     p <- ggplot(sel, aes(Company.Sort, Time.Seconds)) +
       geom_jitter(width=0.5, aes(color=Gender)) +
       xlab("Company") +
       scale_y_continuous(breaks = time.breaks, labels = secondsToTimestr(time.breaks), name="elapsed time (hh:mm:ss)") +
       expand_limits(y=0)
     print(p)
   })
   output$summary_table <- renderTable({
     co1 <- top20 %>% filter(Company %in% input$co1)
     sum1 <- summary(co1$Time.Seconds)
     co2 <- top20 %>% filter(Company %in% input$co2)
     sum2 <- summary(co2$Time.Seconds)
     co3 <- top20 %>% filter(Company %in% input$co3)
     sum3 <- summary(co3$Time.Seconds)

     data.frame(
       company = c(input$co1, input$co2, input$co3),
       min = c(sum1["Min."], sum2["Min."], sum3["Min."]),
       mean = c(sum1["Mean"], sum2["Mean"], sum3["Mean"]),
       median = c(sum1["Median"], sum2["Median"], sum3["Median"]),
       max = c(sum1["Max."], sum2["Max."], sum3["Max."])
     )
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

