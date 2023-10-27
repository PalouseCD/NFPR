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
library(plyr)


### GETTING COLUMN NAMES FROM FILE NAMES ###
files <- list.files(here::here("data"),pattern = "*.csv")
files<- files%>% 
  str_detect(pattern = ".log",negate = TRUE) %>% 
  keep(files, .) 

# filename <- sapply(strsplit(files[1],"[_]"),`[`, 1)

#file<-"R:/_04_Project_Data/R/NFPR/data/NFPR3_BarPress.csv"
#name<-sapply(strsplit(file,"/"), "[", 6)


### FUNCTION ###
data.import<-function(file){ #Cleaning function for PCD data
  data<- read_csv(file,skip = 16,
                  col_names = c("Date", "Time", "Value","State_of_Value"),
                  na = "---") #imports file
  data$datetime <- mdy_hms(paste(data$Date, data$Time)) #changes date time to posixct format
  name<-sapply(strsplit(file,"/"), "[", 6)
  data$Site <- sapply(strsplit(name,"[_]"),`[`, 1)
  filename <- as.character(data[1,6])
  if (filename == "COW0.07" || filename =="DR0.51"||filename =="PAL112.4"|| filename =="SFC0.35"||filename == "STEPTOE0.70"|| filename =="STEPTOE5.23") {
    Parameter <- sapply(strsplit(name, "[[:punct:]]"), "[", 3)
  } else {
    Parameter <- sapply(strsplit(name, "[[:punct:]]"), "[", 2)
  }
  colnames(data)[3] = Parameter
  data <- data[,c(6,5,3)]# Cleans up data frame
  return(data) #assigns the data frame
}

### Data cleanup ###
cleandata <-bind_rows(lapply(here::here("data",files),data.import)) %>%
  group_by(datetime) %>% 
  arrange(datetime) %>% 
  arrange(Site) %>% 
  distinct(.keep_all = FALSE)

cleandata <- cleandata %>% 
  group_by(Site, datetime) %>% 
  fill(-Site, -datetime, .direction = "updown") %>% 
  distinct() %>%
  ungroup()

#Data export ###
filename <- "PRtribs_Clean"

begindate <- min(cleandata$datetime) 
begindate <- sub(" .*", "", begindate)

enddate <- max(cleandata$datetime)
enddate <- sub(" .*", "", enddate)

#write.csv(cleandata, paste(filename, begindate, "to", enddate, ".csv", sep = "_"),row.names = FALSE)

write.csv(cleandata, paste(here::here("outputs",filename), begindate, "to", enddate, ".csv", sep = "_"),row.names = FALSE)

#Calculate daily means of Stage and Water Temp
names(cleandata)
st <-cleandata[,c(1,2,15,20)] 
  summary(st)
daily_mean <- st %>%
  mutate(date = date(datetime)) %>% 
  drop_na() %>% 
  group_by(Site) %>% 
  summarise(mean_stage = mean(S))

c <- st %>% drop_na()
summary(c)

<- c %>% group_by(date) %>% 
  summarise(mean_Stage = mean(S))
unique(st$Site)

### Run this code after all complete site CSVs have been created to combine them into one watershed data frame ###
AllWatershedList <- list.files(path_out) 

AllWatershed <- bind_rows(lapply(AllWatershedList, read.csv))
AllWatershed$datetime <- ymd_hms(AllWatershed$datetime)

Watershed <- sapply(strsplit(path_out, "[[:punct:]]"), "[", 10)

setwd(path_out)
getwd()

write.csv(AllWatershed, paste(Watershed, begindate, "to", enddate, ".csv", sep = "_"), row.names = FALSE)

