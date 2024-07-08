library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(randomForest)
library(party)
library(partykit)
library(caret)


modell <- randomForest(type ~ ., data = gdata, ntree = 500, mtry = 8, importance = TRUE)

myf2 <- type ~ DMC+DC+ISI+Temperature+RH+Wind+Rain+FFMC
model2 <- ctree(myf2, data=gdata)

saveRDS(modell, "modell.rds")
modell <- readRDS("modell.rds")
saveRDS(model2, "model2.rds")
model2 <- readRDS("model2.rds")


ui <- fluidPage(theme = shinytheme("cyborg"),
                
                
          headerPanel('Forest Fire Prediction'),
               
          sidebarPanel(
                  HTML("<h3>Input parameters</h3>"),
                  
                  numericInput(inputId = "Temperature",
                               label = "Temperature", 
                               value = 32,),
                  numericInput(inputId = "RH",
                               label = "RH", 
                               value = 62),
                  numericInput(inputId = "Wind",
                               label = "Wind", 
                               value = 15.5),
                  numericInput(inputId = "Rain",
                               label = "Rain", 
                               value = 0.7),
                  numericInput(inputId = "FFMC",
                               label = "FFMC", 
                               value = 78),
                  numericInput(inputId = "DMC",
                               label = "DMC", 
                               value = 15),
                  numericInput(inputId = "DC",
                               label = "DC", 
                               value = 49),
                  numericInput(inputId = "ISI",
                               label = "ISI", 
                               value = 5),
                  
                actionButton("Predict", "Predict", class = "btn btn-primary")
                ),
                
                mainPanel(
                  
                  tabsetPanel(type = "tab",
                  tabPanel("Random Forest", 
                  tableOutput("tabledata"),
                  tags$label(h3('Result')),
                  verbatimTextOutput('contents')),
                  tabPanel("Decision Tree",
                  tableOutput("tabledata2"),
                  tags$label(h3('Result')),
                  verbatimTextOutput('contents2')),
                  tabPanel("Information",verbatimTextOutput('info')))
                  
                  
                  ))



server <- function(input, output, session) {
  

  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("Temperature",
               "RH",
               "Wind",
               "Rain",
               "FFMC",
               "DMC",
               "DC",
               "ISI"),
      Value = as.numeric(c(input$Temperature,
                             input$RH,
                             input$Rain,
                             input$Wind,
                             input$FFMC,
                             input$DMC,
                             input$DC,
                             input$ISI)),
      stringsAsFactors = FALSE)
    
    type <- "type"
    df <- rbind(df, type)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    set.seed(225)
    Output <- data.frame(Prediction=predict(modell,test), round(predict(modell,test,type="prob"), 3))
    print(Output)
  })
  
  datasetInput2 <- reactive({  
    
    df2 <- data.frame(
      Name = c("Temperature",
               "RH",
               "Wind",
               "Rain",
               "FFMC",
               "DMC",
               "DC",
               "ISI"),
      Value = as.numeric(c(input$Temperature,
                           input$RH,
                           input$Rain,
                           input$Wind,
                           input$FFMC,
                           input$DMC,
                           input$DC,
                           input$ISI)),
      stringsAsFactors = FALSE)
    
    type <- "type"
    df2 <- rbind(df2, type)
    input <- transpose(df2)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Output2  <- data.frame(Prediction=predict(model2,test), round(predict(model2,test,type="prob"), 3))
    print(Output2)
  })
  
  output$contents <- renderPrint({
    if (input$Predict>0) { 
      isolate("Complete") 
    } else {
      return("Server is ready")
    }
  })
  
  output$contents2 <- renderPrint({
    if (input$Predict>0) { 
      isolate("Complete") 
    } else {
      return("Server is ready")
    }
  })
  
  output$tabledata <- renderTable({
    if (input$Predict>0) { 
      isolate(datasetInput()) 
    } 
  })
  
  output$tabledata2 <- renderTable({
    if (input$Predict>0) { 
      isolate(datasetInput2()) 
    } 
  })
  
  output$info <- renderText({
"The app estimates the risk of fire by using the current weather conditions. 
It uses random forest algorithm for prediction.
      
App uses weather parameters determined according to Fire Weather Index (FWI) System:
      
1- Temp : temperature noon (temperature max) in Celsius degrees
2- RH : Relative Humidity in
3- Ws : Wind speed in km/h
4- Rain: total day in mm
5- Fine Fuel Moisture Code (FFMC) index from the FWI system
6- Duff Moisture Code (DMC) index from the FWI system
7- Drought Code (DC) index from the FWI system
8- Initial Spread Index (ISI) index from the FWI system

To use it, enter the above-mentioned parameters according to your region and press the Predict button.
The results will come out as N and F. N indicates low fire risk, F indicates high risk.
There are two prediction models: Random Forest and Decision Tree.
If you want to change the model, you don't need to re-enter the parameters. The panel works for both."
  })
  
  
}

shinyApp(ui = ui, server = server)
