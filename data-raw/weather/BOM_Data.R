library(tidyverse)
library(XML)
library(data.table)
library(dtplyr)
library(lubridate)
library(fitzRoy)
base_url <- "http://www.bom.gov.au"
years <- 2017
# setwd("data")

# Get stations ------------------------------------------------------------
tmptxt <- tempfile(fileext = ".txt")
download.file("http://www.bom.gov.au/climate/data/lists_by_element/alphaAUS_136.txt", 
              dest = tmptxt)
stations <- read.delim(tmptxt, as.is = TRUE)
unlink(tmptxt)

# Clean by hand
stations <- stations[-c(1,3), ]
cname    <- stations[1]
stations <- data.frame(stations[2:(length(stations)-3)])
names(stations) <- cname

# # read stations file
tmpcsv <- tempfile(fileext = ".csv")
write.csv(stations, tmpcsv, row.names = FALSE, quote = FALSE)
stations <- read.fwf(tmpcsv,widths = c(8,41,10,9,9,10,7,3,4))
unlink(tmpcsv)

xstring  <- apply(stations[1,], 1, as.character)
xstring  <- str_replace_all(xstring, fixed(" "), "")
stations <- stations[-1,]
names(stations) <- xstring
# head(stations)
# View(stations)

stations$StartYear <- as.integer(format(strptime(paste("1",str_replace_all(as.character(stations$Start), fixed(" "), ""),sep=""), "%d%b%Y"), "%Y"))
stations$EndYear <- as.integer(format(strptime(paste("1",str_replace_all(as.character(stations$End), fixed(" "), ""),sep=""), "%d%b%Y"), "%Y"))
stations$Site <- as.numeric(as.character(stations$Site))
# active stations
# stations <- subset(stations, StartYear >= 2014 & EndYear == 2016)

stations<-stations[which(stations$Site %in% c(23090
                                              ,94029,67119
                                              ,40764
                                              ,31011,
                                              86038,
                                              23034,
                                              40913,
                                              87184,
                                              86038,
                                              70351,
                                              14015,
                                              86038,
                                              66062,
                                              66212,
                                              9225,
                                              66212,
                                              14015,
                                              #N/A
                                              91237
)),]


# Download and process ----------------------------------------------------
download_obs_file <- function(station, obs_code){
  tmppath <- paste0(obs_code, "_", station, ".zip")
  if(!file.exists(tmppath)){
    Sys.sleep(0.1)
    # goto station page for rainfall
    page <- htmlParse(paste0("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=", obs_code, "&p_display_type=dailyDataFile&p_startYear=&p_c=&p_stn_num=", station))
    print(page)
    # find the all years data link
    node_title <- c(
      "Data file for daily rainfall data for all years",
      "Data file for daily maximum temperature data for all years",
      "Data file for daily minimum temperature data for all years",
      "Data file for daily global solar exposure data for all years"
    )[match(obs_code, c(136, 122, 123, 193))]
    ziplink <- paste0(base_url, xpathSApply(page, paste0("//a[@title='", node_title, "']"), xmlGetAttr, "href"))
    
    # download and unzip
    if(ziplink != base_url){
      download.file(ziplink, tmppath, quiet = TRUE, mode = "wb")
    }
    
    print(station)
    return(tmppath)
  }
}


process_file <- function(filename, years){
  temp_dir = tempdir()
  unzip(filename, exdir = temp_dir)
  
  datafilename <- dir(temp_dir, pattern = ".csv", full.names = TRUE)
  
  if(length(datafilename) == 1){
    # read csv
    df <- read.csv(datafilename)
    df <- df[df$Year %in% years, 2:6]
  } else {
    df <- 0
  }
  # delete csv
  file.remove(dir(temp_dir, full.names = TRUE))
  
  return(df)
}

# Process -----------------------------------------------------------------
# 136, 122, 123, 193

tmp_paths <- lapply(as.numeric(stations$Site), download_obs_file, obs_code = 136)
df <- lapply(dir(pattern = "136_([0-9]{4,5}).zip"), process_file, years = years)
data_rain <- do.call("rbind", df)
data_rain <- data_rain[data_rain$Year != 0, ]
rm(df)
unlink(tmp_paths)

# tmp_paths <- lapply(stations$Site, download_obs_file, obs_code = 122)
# df <- lapply(dir(pattern = "122_([0-9]{4,5}).zip"), process_file, years = years)
# data_temp_max <- do.call("rbind", df)
# data_temp_max <- data_temp_max[data_temp_max$Year != 0, ]
# rm(df)
# unlink(tmp_paths)
# 
# tmp_paths <- lapply(stations$Site, download_obs_file, obs_code = 123)
# df <- lapply(dir(pattern = "123_([0-9]{4,5}).zip"), process_file, years = years)
# data_temp_min <- do.call("rbind", df)
# data_temp_min <- data_temp_min[data_temp_min$Year != 0, ]
# rm(df)
# unlink(tmp_paths)

tmp_paths <- lapply(stations$Site, download_obs_file, obs_code = 193)
df <- lapply(dir(pattern = "193_([0-9]{4,5}).zip"), process_file, years = years)
data_solar <- do.call("rbind", df)
data_solar <- data_solar[data_solar$Year != 0, ]
rm(df)
unlink(tmp_paths)

bom_data <- merge(data_rain, data_solar, all = TRUE)
# df <- merge(df, data_temp_min, all = TRUE)
# df <- merge(df, data_temp_max, all = TRUE)
# save(df, file = "BOM.RData")
#devtools::use_data(BOM, overwrite = TRUE)
#save(df, file = "./data-raw/weather/BOM.rda")

### Get match results and merge --------------------
results <- fitzRoy::match_results
BOMVenueLookup <- read_csv(file="data-raw/weather/BOM Mapping.csv")
colnames(BOMVenueLookup)<-c("Venue","Description","StationNo")

StationsList<-unique(BOMVenueLookup$StationNo)

colnames(bom_data)[1]<-"StationNo"
colnames(bom_data)[5]<-"Rainfall"

#Convert Y,M,D to date
bom_data$date<-ISOdate(bom_data$Year,bom_data$Month,bom_data$Day,0,0,0)

bom_data$date<-ymd(bom_data$date)

# Merge the AFL data set with the BOM rainfall ----------------------------

AFLDataWithBOMStation <- merge(x = results, y = BOMVenueLookup, by = "Venue", all.x=TRUE)
AFLDataWithBOMStation$Date<-ymd(AFLDataWithBOMStation$Date)


colnames(AFLDataWithBOMStation)[3] <- 'date'
#No missing observations (i.e. lookup table is solid)
# names(AFLDataWithBOMStation)

results_weather <- merge(x=AFLDataWithBOMStation,y=bom_data[,c("StationNo","date","Rainfall")],by=c("StationNo","date"),all.x=TRUE,all.y=FALSE)

results_weather <- results_weather[order(results_weather$date),]
# Write data using devtools
devtools::use_data(results_weather, overwrite = TRUE)
save(results_weather, file = "./data-raw/weather/results_weather.rda")