###R Shiny App for Water Quality Data from the North Fork Palouse River
# By Gabby Hannen, Last Edited 2023-04-24


library(tidyr)
library(dplyr)
library(shiny)
library(rsconnect)
library(plotly)
library(shinyWidgets)
library(shinythemes)
library(lubridate)
library(anytime)
library(gt)
library(kableExtra)
library(here)

########

### CREATING DATAFRAME ###
#when running in RStudio use:
#wqdata <- read.csv(here("shiny/NF_App/Data", "NFPRshinyappdata.csv"))

#when publishing to ShinyIO use:
wqdata <- read.csv(here("Data", "NFPRshinyappdata.csv"))
wqdata$Datetime <- mdy(wqdata$Datetime)

### ADDING TMDLS TO DATAFRAME ###
#TMDLs according to ECY
#DO_Min = 8.5
#pH_Min = 6.5
#pH_Max = 8.5
#Temp_Max = 17.5
#FC_Max = 100

wqdata$Criteriamax<- ifelse(wqdata$Parameter == "Dissolved Oxygen (mg/L)", 8.5,
                            ifelse(wqdata$Parameter == "Water Temperature (C)", 17.5,
                                   ifelse(wqdata$Parameter == "pH", 8.5,
                                          ifelse(wqdata$Parameter == "Fecal Coliform (cfu)", 100, NA))))
wqdata$Criteriamin<- ifelse(wqdata$Parameter == "pH", 6.5, NA)




### RSCONNECT TO SERVER ###

rsconnect::setAccountInfo(name='pcdrm',
                          token= '22F8B9DB932C3F6582F2B3E5DDDE6A14',
                          secret= 'T0EH/V2HUtckmKizHDYSeLK4Q3pCxOyYZ0bY+Qzj')

## Define UI

ui <- fluidPage(theme = shinytheme("cosmo"),
                
                sidebarLayout(position = "left",
                              sidebarPanel(id = "sidebar", width = 2, height = "500px",
                                           h3(strong("Graphing Options"), align = 'center'),
                                           #dropdown selection for water quality parameters
                                           selectInput("para", 
                                                       label = "Select Desired Parameter:",
                                                       choices = c( "Ammonia (mg/L)",
                                                                    "Nitrate/Nitrite (mg/L)",
                                                                    "Orthophosphate (mg/L)",
                                                                    "Total Phosphorus (mg/L)",
                                                                    "Suspended Sediment Concentration (mg/L)",
                                                                    "Water Temperature (C)",
                                                                    "Dissolved Oxygen (%)",
                                                                    "Dissolved Oxygen (mg/L)",
                                                                    "Specific Conductivity (uS/cm)",
                                                                    "pH",
                                                                    "Turbidity (NTU)",
                                                                    "Fecal Coliform (cfu)",
                                                                    "Flow (cfs)",
                                                                    "Stage (ft)")),
                                           
                                           #dropdown selection for monitoring sites
                                           selectInput("sta", 
                                                       label = "Select Desired Water Quality Monitoring Site:",
                                                       choices = c("Duffield Creek (NFPR12)",
                                                                   "Clear Creek (NFPR9)",
                                                                   "Silver Creek (NFPR6)",
                                                                   "Cedar Creek (NFPR3)",
                                                                   "Palouse River (NFPR8)",
                                                                   "Silver Creek (NFPR5A)",
                                                                   "Palouse River (PAL112.4)")),
                                         
                              ),
                              
                              
                              mainPanel(width =10,
                                        fluidRow(
                                          column(12,
                                                 align = "left",
                                                 h2("Ambient Water Quality Monitoring for the North Fork Palouse River Tributaries"))
                                        ),
                                        
                                        fluidRow(
                                          column(10,
                                                 align = "center",
                                                 plotlyOutput("line", width = "100%", height = "100%",
                                                 )),
                                          column(10,
                                                  align = "right",
                                                  textOutput("warnings1"),
                                                 tags$head(tags$style("#warnings1{color: red;
                                                                      font-size: 17px;
                                                                      font-style: bold;
                                                                      }"))),
                              ),
                                        
                                        fluidRow(
                                          column(3,
                                                 align = "right",
                                                 tableOutput("table")))
                                        
                              )))



server = function(input, output){
  
  ### SETTING UP DATAFRAMES FOR GRAPH AND SUMMARY TABLE ###
  
  data <-reactive({
    new_data <-filter(wqdata,Station == input$sta & Parameter == input$para)
    return(new_data)
  }) 
  
  
  tdata<-reactive({
    ntdata<- data() %>%
      select('Value') %>%
      summarise(Min = min(Value, na.rm = TRUE), Mean=mean(Value, na.rm = TRUE),Max = max(Value, na.rm = TRUE))
    return(ntdata)
  })
  
  
  
  ### PLOTTING GRAPH ###
  # This displays the trace of parameter data and TMDLs if there is one for that parameter
  output$line <- renderPlotly({
    plot_ly(data(), x = ~Datetime)%>%
      add_trace(y = ~Value,
                type = 'scatter',
                mode = 'lines+markers',
                connectgaps = TRUE,
                name = input$sta)%>%
      add_lines(y = ~Criteriamax,
                line = list(color = 'red'),
                connectgaps = TRUE,
                name = "Water Quality Criteria Max")%>%
      add_lines(y = ~Criteriamin,
                line = list(color = 'red'),
                connectgaps = TRUE,
                name = "Water Quality Criteria Min")%>%
      layout(
        plot_bgcolor='#e5ecf6',
        xaxis = list(title ='Date'),
        yaxis = list(title = input$para),
        legend = list(orientation = 'h'))
     ##(x = 0.05, y = 0.95) 
  })

  # Warnings about flow sampling
  output$warnings1 <- renderText({
    if(input$para == "Stage (ft)" & input$sta == "Palouse River (PAL112.4)") {
      "Note: Stage was not collected at this site"
    } else {
      if(input$para == "Stage (ft)" & input$sta == "Silver Creek (NFPR6)") {
        "Note: Stage was not collected at this site"
      } else {
      if(input$para == "Stage (ft)" & input$sta == "Palouse River (NFPR8)") {
        "Note: Stage was not collected at this site"
      }
        }
          }
  })
  
  
  
   ### OUTPUTTING TABLE ###
  # This outputs a table with min, max and mean
  output$table <- renderTable({
    tdata()},
    striped = TRUE, bordered = TRUE, width = '300', align = 'c')

  
}



shinyApp(ui = ui, server = server)



