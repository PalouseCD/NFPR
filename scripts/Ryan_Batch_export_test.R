library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)
library(tools)
library(readr)
library(anytime)
library(here)
library(data.table)
here("data")

### GETTING COLUMN NAMES FROM FILE NAMES ###
files <- list.files(here("data/Batch_export_format"),pattern = "*.csv") #makes list of CSVs in appropriate directory
files<- files%>% 
  str_detect(pattern = ".log",negate = TRUE) %>%  #excludes .log files if present
  keep(files, .) 


### FUNCTION ###
data.import<-function(file){ #Cleaning function for PCD data
  data<- read_csv(file, skip = 21,
                  col_names = c("datetime", "Value", "State_of_Value"),
                  na = "---") #imports file
  data$datetime <- dmy_hms(data$datetime) #changes date time to posixct format
  name<-sapply(strsplit(file,"/"), "[", 7)
  data$Site <- sapply(strsplit(name,"[_]"),`[`, 1)
  filename <- as.character(data[1,4])
  if (filename =="COW0.07" || filename =="DR0.51"||filename =="PAL112.4"|| filename =="SFC0.35"||filename == "STEPTOE0.70"|| filename =="STEPTOE5.23") {
    Parameter <- sapply(strsplit(name, "[[:punct:]]"), "[", 3)
  } else {
    Parameter <- sapply(strsplit(name, "[[:punct:]]"), "[", 2)
  }
  colnames(data)[2] = Parameter
  data <- data[,c(4,1,2)]# assigns columns in appropriate order
  return(data) #assigns the data frame
}


### DATA CLEANUP ###
cleandata <-bind_rows(lapply(here("data/Batch_export_format",files),data.import)) %>%
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
filename <- "PRtribs_Clean.csv"

write.csv(cleandata, paste(here("outputs",filename)),row.names = FALSE)

### CALCULATE DAILY MEANS OF STAGE AND WATER TEMP ###
filename2 <- "PRTribs_Daily_Averages.csv"

Alldata <- cleandata

Alldata$date <- date(Alldata$datetime) 

dailya <- Alldata %>% group_by(Site,date) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) 

colnames(dailya)[2] <- "Datetime"

write_csv(dailya,here("outputs",filename2))

### exporting data ####
export <- function(data,Sitename){
  d <- data %>% 
    filter(Site == Sitename)
  write_csv(d,here("outputs/Individual_Sites",paste(Sitename,"csv",sep=".")))
}

export(dailya,"DR0.51")
export(dailya,"NFPR12")
export(dailya,"NFPR3")
export(dailya,"NFPR5A")
export(dailya,"NFPR6")
export(dailya,"NFPR6P")
export(dailya,"NFPR8")
export(dailya,"NFPR8A")
export(dailya,"NFPR9")

###########  Testing the markdown

MASTER = read.csv(here("outputs/Individual_Sites","NFPR3.csv"))

FIRST_SAMPLE <- min(MASTER$Datetime) 
FIRST_SAMPLE <- sub(" .*", "", FIRST_SAMPLE)
FIRST_SAMPLE <- format(as.Date(FIRST_SAMPLE),'%m/%d/%Y')

LAST_SAMPLE <- max(MASTER$Datetime)
LAST_SAMPLE <- sub(" .*", "", LAST_SAMPLE)
LAST_SAMPLE <- format(as.Date(LAST_SAMPLE),'%m/%d/%Y')



