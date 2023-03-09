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
library(data.table)
here::here

### GETTING COLUMN NAMES FROM FILE NAMES ###
files <- list.files(here::here("data"),pattern = "*.csv") #makes list of CSVs in appropriate directory
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

### DATA EXPORT ###
filename <- "PRtribs_Clean"

begindate <- min(cleandata$datetime) 
begindate <- sub(" .*", "", begindate)

enddate <- max(cleandata$datetime)
enddate <- sub(" .*", "", enddate)

write.csv(cleandata, paste(here::here("outputs",filename), begindate, "to", enddate, ".csv", sep = "_"),row.names = FALSE)

### CALCULATE DAILY MEANS OF STAGE AND WATER TEMP ###
filename2 <- "PRTribs_Daily_Averages"

#Daily_Averages <-cleandata[,c(1,2, 3, 14, 20)] 
#Daily_Averages$datetime <- as.Date(Daily_Averages$datetime)
#Daily_Averages <- Daily_Averages %>% 
  #arrange(datetime)
#Daily_Averages <- ddply(Daily_Averages, . (Site, datetime), summarise, AirTemp = mean(AirTemp, na.rm = TRUE), S = mean(S, na.rm = TRUE), WT=mean(WT, na.rm = TRUE))
#Daily_Averages$datetime <- as.POSIXct(Daily_Averages$datetime)

Alldata <- cleandata[, -c(3, 14, 20)]

Alldata <- Alldata[rowSums(is.na(Alldata)) != ncol(Alldata), ]

Alldata2 <- merge(x = Alldata, y = Daily_Averages, by = c('Site', 'datetime'), all = TRUE)
Alldata2 <- Alldata2[, c(1:2, 18, 3:20)]
Alldata2 <- Alldata2[, -c(19)]

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
write.csv(Alldata4, paste(here::here("outputs",filename2), begindate, "to", enddate, ".csv", sep = "_"),row.names = FALSE)

