library(shiny)

 
ui <- fluidPage(
  
  titlePanel("ADHD Prevalence Among Children in the U.S. Between 2009 and 2011"),

  sidebarLayout(
    sidebarPanel(
        p("Click the button to get a summary of the scatterplot."),
        actionButton("sumButton", "Summary")
      
    ),
    

    mainPanel(
      tabsetPanel(id = "tabChoice", type = "tabs", 
                  tabPanel("Age", plotOutput("agePlot")), 
                  tabPanel("Sex", plotOutput("sexPlot")), 
                  tabPanel("Race", plotOutput("racePlot")), 
                  tabPanel("Percent of Poverty Level", plotOutput("povertyPlot")),
                  tabPanel("Health Insurance Status", plotOutput("insurancePlot")),
      
      verbatimTextOutput("summary"))
      )
    )
  )

server <- function(input, output) {
  
  data <- reactive({  switch(input$tabChoice,
                      "Age" = ageADHD$X2009.2011,
                      "Sex" = sexADHD$X2009.2011,
                      "Race" = raceADHD$X2009.2011,
                      "Percent of Poverty Level" = povertyADHD$X2009.2011,
                      "Health Insurance Status" = insuranceADHD$X2009.2011)
  })
  
  
  output$agePlot <- renderPlot({
    
    ageGroup <- c( "5-17 years", "5-9 years", "10-17 years")
    barplot(data(), names.arg = ageGroup, xlab = "Age Range",
            ylab = "Frequency (percent)", main = "Percent of U.S. Children with ADHD by Age", col = "red" )
    
  })
  
  output$sexPlot <- renderPlot({
    
    gender <- c( "Male", "Female")
    barplot(data(), names.arg = gender, xlab = "Gender",
            ylab = "Frequency (percent)", main = "Percent of U.S. Children with ADHD by Sex", col = "darkgreen" )
    
  })
  
  output$racePlot <- renderPlot({
    
    raceGroup <- c( "White", "Black", "American Indian", "Asian",
                   "Pacific-Islander", "Two or More Races")
    barplot(data(), names.arg = raceGroup, xlab = "Race",
            ylab = "Frequency (percent)", main = "Percent of U.S. Children with ADHD by Race", col = "blue" )
    
  })
  
  output$povertyPlot <- renderPlot({
    
    povertyGroup <- c( "Below 100%", "100-199%", "200-399%","400% or More")
    barplot(data(), names.arg = povertyGroup, xlab = "Percent of Poverty Level",
            ylab = "Frequency (percent)", main = "Percent of U.S. Children with ADHD by Poverty Level", col = "orange")
    
  })
  
  output$insurancePlot <- renderPlot({
    
    insuranceGroup <- c( "Insured","Insured-Private","Insured-Medicaid","Uninsured")
    barplot(data(), names.arg = insuranceGroup, xlab = "Health Insurance Status",
            ylab = "Frequency (percent)", main = "Percent of U.S. Children with ADHD by Health Insurance Status", col = "pink")
    
  })
  
  
  sumGraph <- observeEvent(input$sumButton, {
        print(summary(data()))
  })

  output$summary <- renderPrint({
     sumGraph()
  })
  
  
}

shinyApp( ui = ui, server = server )

