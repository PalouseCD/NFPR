###R Shiny App for Water Quality Data from the North Fork Palouse River
# By Gabby Hannen, Last Edited 2022-08-03


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

### CLEANING DATA ###


my_data <- read.csv("PRtribs_2020-02-19_to_2022-07-25_.csv")
my_data$datetime <- ymd_hms(my_data$datetime)
my_data[,17] <- as.numeric(as.integer(my_data[,17]))
my_data[,7] <- as.numeric(as.integer(my_data[,7]))
my_data[,8] <- as.numeric(as.integer(my_data[,8]))
my_data[,9] <- as.numeric(as.integer(my_data[,9]))



colnames(my_data) = c("Station",
                      "Datetime",
                      "AirTemp",
                      "Pressure",
                      "Dissolved Oxygen",
                      "Dissolved Oxygen %",
                      "Fecal Coliform",
                      "Fecal Coliform Down",
                      "Fecal Coliform Up",
                      "Nitrate/Nitrite",
                      "Ammonia",
                      "Orthophosphate",
                      "pH",
                      "Flow",
                      "Stage",
                      "Specific Conductivity",
                      "Suspended Sediment Concentration",
                      "Total Phosphorus",
                      "Turbidity",
                      "Water Temp")
                      

### CREATING DATAFRAME AND CHANGING PARAMETER & STATION NAMES ###

wqdata <- my_data%>%
  pivot_longer(cols=!Station & !Datetime, names_to = "Parameter",values_to="Value")
unique(wqdata$Parameter)  

wqnew <- mutate(wqdata, Station=recode(Station,
                                       "NFPR12" = "Duffield Creek (NFPR12)",
                                       "NFPR9" = "Clear Creek (NFPR9)",
                                       "NFPR6" ="Silver Creek (NFPR6)",
                                       "NFPR3" = "Cedar Creek (NFPR3)",
                                       "NFPR8" = "Palouse River (NFPR8)",
                                       "NFPR5A" = "Silver Creek (NFPR5A)",
                                       "PAL112.4" = "Palouse River (PAL112.4)"))

wqnew2<-filter(wqnew,Station %in% c("Duffield Creek (NFPR12)",
                                    "Clear Creek (NFPR9)",
                                    "Silver Creek (NFPR6)",
                                    "Cedar Creek (NFPR3)",
                                    "Palouse River (NFPR8)",
                                    "Silver Creek (NFPR5A)",
                                    "Palouse River (PAL112.4)"))
unique(wqnew2$Parameter)
wqnew3<-mutate(wqnew2,Parameter=recode(Parameter,
                                       "Nitrate/Nitrite" = "Nitrate/Nitrite.(mg/L)",
                                       "Orthophosphate" = "Orthophosphate.(mg/L)",
                                       "Total Phosphorus" = "Total_Phosphorus.(mg/L)",
                                       "Suspended Sediment Concentration" = "Suspended_Sediment_Concentration.(mg/L)",
                                       "Water Temp" = "Water_Temperature.(C)",
                                       "Dissolved Oxygen %" = "Dissolved_Oxygen.(%)",
                                       "Dissolved Oxygen" = "Dissolved_Oxygen.(mg/L)",
                                       "Specific Conductivity" = "Specific_Conductivity.(uS/cm)",
                                       "pH" = "pH",
                                       "Turbidity" = "Turbidity.(NTU)",
                                       "Fecal Coliform" = "Fecal_Coliform.(cfu)",
                                       "Flow" = "Flow.(cfs)",
))

#wqnew3<-na.omit(wqnew3)



### ADDING TMDLS TO DATAFRAME ###

### TMDLs according to ECY ##
#DO_Min = 8.5
#pH_Min = 6.5
#pH_Max = 8.5
#Temp_Max = 17.5
#FC_Max = 100

wqnew3$Criteriamax<- ifelse(wqnew3$Parameter == "Dissolved_Oxygen.(mg/L)", 8.5,
                            ifelse(wqnew3$Parameter == "Water_Temperature.(C)", 17.5,
                                   ifelse(wqnew3$Parameter == "pH", 8.5,
                                          ifelse(wqnew3$Parameter == "Fecal_Coliform.(cfu)", 100, NA))))
wqnew3$Criteriamin<- ifelse(wqnew3$Parameter == "pH", 6.5, NA)




### RSCONNECT TO SERVER ###

rsconnect::setAccountInfo(name='pcdrm',
                          token= '22F8B9DB932C3F6582F2B3E5DDDE6A14',
                          secret= 'T0EH/V2HUtckmKizHDYSeLK4Q3pCxOyYZ0bY+Qzj')

## Define UI

ui <- fluidPage(theme = shinytheme("cosmo"),
                
                headerPanel(title = "", windowTitle = "Palouse CD Research & Monitoring"),
                
                sidebarLayout(position = "left",
                              sidebarPanel(id = "sidebar", width = 2,
                                           align = "center",
                                           br(),
                                           h3(strong("Graphing Options"), align = 'center'),
                                           #dropdown selection for water quality parameters
                                           selectInput("para", 
                                                       label = "Select Desired Parameter:",
                                                       choices = c( "Ammonia",
                                                                    "Nitrate/Nitrite.(mg/L)",
                                                                    "Orthophosphate.(mg/L)",
                                                                    "Total_Phosphorus.(mg/L)",
                                                                    "Suspended_Sediment_Concentration.(mg/L)",
                                                                    "Water_Temperature.(C)",
                                                                    "Dissolved_Oxygen.(%)",
                                                                    "Dissolved_Oxygen.(mg/L)",
                                                                    "Specific_Conductivity.(uS/cm)",
                                                                    "pH",
                                                                    "Turbidity.(NTU)",
                                                                    "Fecal_Coliform.(cfu)",
                                                                    "Flow.(cfs)",
                                                                    "Stage")),
                                           
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
                                           fluidRow(
                                             column(1,
                                                    br(),
                                                    align = "center",
                                                    uiOutput("pic")
                                             ))
                              ),
                              
                              
                              mainPanel(width =10,
                                        fluidRow(
                                          column(10,
                                                 align = "left",
                                                 h2("Ambient Water Quality Monitoring for the North Fork Palouse River Tributaries"))
                                        ),
                                        
                                        fluidRow(
                                          column(10,
                                                 br(),
                                                 align = "left",
                                                 plotlyOutput("line", width = 1200, height = 600,
                                                 ))),
                                        
                                        fluidRow(
                                          column(3,
                                                 br(),
                                                 align = "center",
                                                 tableOutput("table")))
                                        
                              )))



server = function(input, output){
  
  
  ### SETTING UP DATAFRAMES FOR GRAPH AND SUMMARY TABLE ###
  
  data <-reactive({
    new_data <-filter(wqnew3,Station == input$sta & Parameter == input$para)
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
      add_trace(y = ~Criteriamax,
                line = list(color = 'red'),
                type = 'scatter',
                mode = 'lines+markers',
                marker = list(color = 'red'),
                connectgaps = TRUE,
                name = "Water Quality Criteria Max")%>%
      add_trace(y = ~Criteriamin,
                line = list(color = 'red'),
                type = 'scatter',
                mode = 'lines+markers',
                marker = list(color = 'red'),
                connectgaps = TRUE,
                name = "Water Quality Criteria Min")%>%
      layout(
        plot_bgcolor='#e5ecf6',
        xaxis = list(title ='Date'),
        yaxis = list(title = input$para))
    
  })
  
  
  ### OUTPUTTING TABLE ###
  # This outputs a table with min, max and mean
  
  output$table <- renderTable({
    tdata()},
    striped = TRUE, bordered = TRUE, width = '300', align = 'c')
    

  
  
  ### DISPLAYING IMAGE OF SITE ###
  ### These images are located in the WWW folder, R automatically pulls photos from this folder
  
  output$pic <- renderUI({
    img(width = 250, height = 150, src= ifelse(input$sta == "Duffield Creek (NFPR12)", "NFPR12.png",
                                               ifelse(input$sta == "Clear Creek (NFPR9)", "NFPR9.png",
                                                      ifelse(input$sta == "Cedar Creek (NFPR3)", "NFPR3.png",
                                                             ifelse(input$sta == "Palouse River (NFPR8)", "NFPR8.png",
                                                                    ifelse(input$sta == "Silver Creek (NFPR6)", "NFPR6.png",
                                                                           ifelse(input$sta == "Silver Creek (NFPR5A)", "NFPR5A.png",
                                                                                  ifelse(input$sta == "Palouse River (PAL112.4)", "PAL112.4.png",NA))))))
                                               
                                               
    ))
  })
  
  
  
  
  
}



shinyApp(ui = ui, server = server)



