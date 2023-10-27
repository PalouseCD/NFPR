### WISKI Data Cleanup and Export Function ###
### Tal Atkins ###
### 11/7/22 ###

library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)
library(tools)
library(readr)
library(anytime)
library(here)
library(data.table)
here()

### GETTING COLUMN NAMES FROM FILE NAMES ###
files <- list.files(here("data"),pattern = "*.csv") #makes list of CSVs in appropriate directory
files<- files%>% 
  str_detect(pattern = ".log",negate = TRUE) %>%  #excludes .log files if present
  keep(files, .) 

### FUNCTION ###
data.import<-function(file){ #Cleaning function for PCD data
  data<- read_csv(file, skip = 16,
                  col_names = c("Date", "Time", "Value","State_of_Value"),
                  na = "---") #imports file
  data$datetime <- mdy_hms(paste(data$Date, data$Time)) #changes date time to posixct format
  name<-sapply(strsplit(file,"/"), "[", 6)
  data$Site <- sapply(strsplit(name,"[_]"),`[`, 1)
  filename <- as.character(data[1,6])
  if (filename =="COW0.07" || filename =="DR0.51"||filename =="PAL112.4"|| filename =="SFC0.35"||filename == "STEPTOE0.70"|| filename =="STEPTOE5.23") {
    Parameter <- sapply(strsplit(name, "[[:punct:]]"), "[", 3)
  } else {
    Parameter <- sapply(strsplit(name, "[[:punct:]]"), "[", 2)
  }
  colnames(data)[3] = Parameter
  data <- data[,c(6,5,3)]# assigns columns in appropriate order
  return(data) #assigns the data frame
}

### DATA CLEANUP ###
cleandata <-bind_rows(lapply(here("data",files),data.import)) %>%
  group_by(datetime) %>% 
  arrange(datetime) %>% 
  arrange(Site) %>% 
  distinct(.keep_all = FALSE)

cleandata <- cleandata %>% 
  group_by(Site, datetime) %>% 
  fill(-Site, -datetime, .direction = "updown") %>% 
  distinct() %>%
  ungroup()

### DATA EXPORT ###
filename <- "PRtribs_Clean"

#begindate <- min(cleandata$datetime) 
#begindate <- sub(" .*", "", begindate)

#enddate <- max(cleandata$datetime)
#enddate <- sub(" .*", "", enddate)

write.csv(cleandata, paste(here("outputs",filename)),row.names = FALSE)

### CALCULATE DAILY MEANS OF STAGE AND WATER TEMP ###
filename2 <- "PRTribs_Daily_Averages"

Alldata <- cleandata

#Alldata <- cleandata[, -c(3, 14, 20)]

Alldata2 <- Alldata[rowSums(is.na(Alldata)) != ncol(Alldata), ]

#Alldata2 <- merge(x = Alldata, y = Daily_Averages, by = c('Site', 'datetime'), all = TRUE)
#Alldata2 <- Alldata2[, c(1:2, 18, 3:20)]
#Alldata2 <- Alldata2[, -c(19)]

### CHECKS NUMBER OF COLUMNS ###
cols <- ncol(Alldata2[, -c(1, 2)])

### CHECKS FOR WHICH ELEMENTS HAVE MISSING VALUES ###
is_na <- is.na(Alldata2)

### COMPUTES TOTAL NUMBER OF NAs ENCOUNTERED IN EACH ROW ###
row_na <- rowSums(is_na)

### CHECKS WHERE THE CELLS ARE NOT ALL NA ### 
Alldata3 <- Alldata2[row_na != cols, ] 

### COMBINE DATA POINTS TO SINGLE DAY ###
Alldata3$datetime <- floor_date(Alldata3$datetime, "day")

Alldata4 <- Alldata3 %>%
  group_by(Site, datetime) %>%
  fill(everything(), .direction = "downup") %>%
  slice(1)

### EXPORT DATAFRAME ###
write.csv(Alldata4, paste(here::here("outputs",filename2)),row.names = FALSE)


#EXPORTING INDIVIDUAL SITES#
Alldata4$Site <- as.factor(Alldata4$Site)
levels(Alldata4$Site)

###UNIQUE DATAFRAMES W/ EXPORT###
#DR0.51#
"DR0.51" <- Alldata4 %>% 
  filter(Site == "DR0.51")
#begindate <- min(DR0.51$datetime) 
#begindate <- sub(" .*", "", begindate)
#enddate <- max(DR0.51$datetime)
#enddate <- sub(" .*", "", enddate)
write.csv(DR0.51, paste(here("outputs", "Individual_Sites", "DR0.51.csv")), row.names = FALSE)

#NFPR12#
"NFPR12" <- Alldata4 %>% 
  filter(Site == "NFPR12")
#begindate <- min(NFPR12$datetime) 
#begindate <- sub(" .*", "", begindate)
#enddate <- max(NFPR12$datetime)
#enddate <- sub(" .*", "", enddate)
write.csv(NFPR12, paste(here("outputs", "Individual_Sites", "NFPR12.csv")), row.names = FALSE)

#NFPR3#
"NFPR3" <- Alldata4 %>% 
  filter(Site == "NFPR3")
#begindate <- min(NFPR3$datetime) 
#begindate <- sub(" .*", "", begindate)
#enddate <- max(NFPR3$datetime)
#enddate <- sub(" .*", "", enddate)
write.csv(NFPR3, paste(here("outputs", "Individual_Sites", "NFPR3.csv")), row.names = FALSE)

#NFPR5A#
"NFPR5A" <- Alldata4 %>% 
  filter(Site == "NFPR5A")
#begindate <- min(NFPR5A$datetime) 
#begindate <- sub(" .*", "", begindate)
#enddate <- max(NFPR5A$datetime)
#enddate <- sub(" .*", "", enddate)
write.csv(NFPR5A, paste(here("outputs", "Individual_Sites", "NFPR5A.csv")), row.names = FALSE)

#NFPR6#
"NFPR6" <- Alldata4 %>% 
  filter(Site == "NFPR6")
#begindate <- min(NFPR6$datetime) 
#begindate <- sub(" .*", "", begindate)
#enddate <- max(NFPR6$datetime)
#enddate <- sub(" .*", "", enddate)
write.csv(NFPR6, paste(here("outputs", "Individual_Sites", "NFPR6.csv")), row.names = FALSE)

#NFPR6P#
"NFPR6P" <- Alldata4 %>% 
  filter(Site == "NFPR6P")
#begindate <- min(NFPR3$datetime) 
#begindate <- sub(" .*", "", begindate)
#enddate <- max(NFPR6P$datetime)
#enddate <- sub(" .*", "", enddate)
write.csv(NFPR6P, paste(here("outputs", "Individual_Sites", "NFPR6P.csv")), row.names = FALSE)

#NFPR8#
"NFPR8" <- Alldata4 %>% 
  filter(Site == "NFPR8")
#begindate <- min(NFPR8$datetime) 
#begindate <- sub(" .*", "", begindate)
#enddate <- max(NFPR8$datetime)
#enddate <- sub(" .*", "", enddate)
write.csv(NFPR8, paste(here("outputs", "Individual_Sites", "NFPR8.csv")), row.names = FALSE)

#NFPR8A#
"NFPR8A" <- Alldata4 %>% 
  filter(Site == "NFPR8A")
#begindate <- min(NFPR8A$datetime) 
#begindate <- sub(" .*", "", begindate)
#enddate <- max(NFPR8A$datetime)
#enddate <- sub(" .*", "", enddate)
write.csv(NFPR8A, paste(here("outputs", "Individual_Sites", "NFPR8A.csv")), row.names = FALSE)

#NFPR9#
"NFPR9" <- Alldata4 %>% 
  filter(Site == "NFPR9")
#begindate <- min(NFPR9$datetime) 
#begindate <- sub(" .*", "", begindate)
#enddate <- max(NFPR9$datetime)
#enddate <- sub(" .*", "", enddate)
write.csv(NFPR9, paste(here("outputs", "Individual_Sites", "NFPR9.csv")), row.names = FALSE)

#NFPR9B#
"NFPR9B" <- Alldata4 %>% 
  filter(Site == "NFPR9B")
#begindate <- min(NFPR9B$datetime) 
#begindate <- sub(" .*", "", begindate)
#enddate <- max(NFPR9B$datetime)
#enddate <- sub(" .*", "", enddate)
write.csv(NFPR9B, paste(here("outputs", "Individual_Sites", "NFPR9B.csv")), row.names = FALSE)

#PAL112.4#
"PAL112.4" <- Alldata4 %>% 
  filter(Site == "PAL112.4")
#begindate <- min(PAL112.4$datetime) 
#begindate <- sub(" .*", "", begindate)
#enddate <- max(PAL112.4$datetime)
#enddate <- sub(" .*", "", enddate)
write.csv(PAL112.4, paste(here("outputs", "Individual_Sites", "PAL112.4.csv")), row.names = FALSE)



### CLEANING DATA FOR SHINY APP ###
#Added by Gabby Hannen - 4/24/23
#This portion of the script organizes the data so it is ready to be run in the NFPR Shiny App
NFPRShinyData <- Alldata4

#CHANGING PARAMETER & STATION NAMES
NFPRShinyData1<-rename(NFPRShinyData,
              "Station" = "Site",
                "Datetime" = "datetime",
                "AirTemp" = "AirTemp",
                "Pressure" = "BarPress",
                "Dissolved Oxygen (mg/L)" = "DO",
                "Dissolved Oxygen (%)" = "DOPerc",
                "Fecal Coliform Down" = "DOWN",
                "Fecal Coliform (cfu)" = "FC",
                "Nitrate/Nitrite (mg/L)" = "N",
                "Ammonia (mg/L)" = "NH3",
                "Orthophosphate (mg/L)" = "OP",
                "pH" = "pH",
                "Flow (cfs)" = "Q",
                "Stage (ft)" = "S",
                "Specific Conductivity (uS/cm)" = "SPC",
                "Suspended Sediment Concentration (mg/L)" = "SSC",
                "Total Phosphorus (mg/L)" = "TP",
                "Turbidity (NTU)" = "TURB",
                "Fecal Coliform Up" = "UP",
                "Water Temperature (C)" = "WT")


NFPRShinyData2<-filter(NFPRShinyData1,Station %in% c("NFPR12",
                                    "NFPR9",
                                    "NFPR6",
                                    "NFPR3",
                                    "NFPR8",
                                    "NFPR5A",
                                    "PAL112.4"))

NFPRShinyData3 <- mutate(NFPRShinyData2, Station=recode(Station,
                                       "NFPR12" = "Duffield Creek (NFPR12)",
                                       "NFPR9" = "Clear Creek (NFPR9)",
                                       "NFPR6" ="Silver Creek (NFPR6)",
                                       "NFPR3" = "Cedar Creek (NFPR3)",
                                       "NFPR8" = "Palouse River (NFPR8)",
                                       "NFPR5A" = "Silver Creek (NFPR5A)",
                                       "PAL112.4" = "Palouse River (PAL112.4)"))

NFPRShinyData4 <- NFPRShinyData3%>%
  pivot_longer(cols=!Station & !Datetime, names_to = "Parameter",values_to="Value")

NFPRShinyData5<-na.omit(NFPRShinyData4)

#Writing .csv to be used in shiny app and saving in shiny folder
write.csv(NFPRShinyData5,(here::here("shiny/NF_App/Data", "NFPRshinyappdata.csv")), row.names = FALSE)



