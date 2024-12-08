
#######################################
###### Load Data and Format Time ######
#######################################

# This code reads in, formats, and combines count and camera-location data.
# Location data is private to Michigan State University. and thus no data is provided to run this code.

# Utilize the Access database (PhotoWarehouse) query for all verified photos. 
# Years after 2019 have 2 databases (APR and NonAPR).
APR.dat <- read.csv("2022APRqryFlatFileVerified.txt")
NonAPR.dat <- read.csv("2022NonAPRqryFlatFileVerified.txt")
data.detect <- rbind(APR.dat, NonAPR.dat)

# Look at species counts for future reference. 
library(dplyr)
data.detect %>% count(SpeciesID)

# Fix any inconsistencies with SpeciesID's among previous years' databases.
# The Access database originally auto assigned IDs.
data.detect$SpeciesID[data.detect$CommonName=="Coyote"] <- 4
data.detect$SpeciesID[data.detect$CommonName=="Fox"] <- 5
data.detect$SpeciesID[data.detect$CommonName=="Camera Problem"] <- 6
data.detect$SpeciesID[data.detect$CommonName=="Doe"] <- 7
data.detect$SpeciesID[data.detect$CommonName=="Fawn"] <- 8
data.detect$SpeciesID[data.detect$CommonName=="Legal Buck"] <- 9
data.detect$SpeciesID[data.detect$CommonName=="Sub-legal Buck"] <- 10
data.detect$SpeciesID[data.detect$CommonName=="Camera Setup/Takedown"] <- 12
data.detect$SpeciesID[data.detect$CommonName=="Remove"] <- 13

# Double check to see if species counts changed.
data.detect %>% count(SpeciesID)

# Save output
write.csv(data.detect, "2022CombinedQryFlatFileVerified.txt")

# Load package to format time data.
library(anytime)

# Format time data for all verified photos.
data.detect$ImageDate2 <- as.POSIXct(data.detect$ImageDate, format="%m/%d/%Y %H:%M:%S")

# Read in camera location data (private to Michigan State University).
cam_locs <- read.csv("APR_Cam_GRTS.csv") 

# Select the columns of interest.
cam_locs <- cam_locs[c("siteID","xcoord","ycoord")] 

# Merge together verified-photo data and camera-location data.
data.detect2 <- merge(data.detect,cam_locs, by.x="LocationName", by.y="siteID", all.x=TRUE)


#############################################
###### Create Function to Rescale Time ######
#############################################

# This code creates a function to rescale time from standard to solar time. 
# Rescaling time like this aids helps with subdividing sampling intervals. 
# This function utilizes Julian date and assumes data are already corrected for timezone. 
# If not corrected for timezone, subtract (number of hours off UTC)/25. 
# For example, EST ends up being -4/24 (so you end up adding). 
# The function was built by David M. Williams using orbit information and trigonometry.

re.time <- function(datetime,lat,long,tz, dataframe=NULL){
  
  jul.day <- (as.numeric(julian(datetime, origin = as.Date("1899-12-30")))) + 2415018.5  
  
  test <- format(datetime, format = "%H:%M:%S")
  
  time <- (as.numeric(as.POSIXct(paste("1974-02-06", test))) - 
             as.numeric(as.POSIXct("1974-02-06 0:0:0")))/(60*60*24)
  
  jul.cent <- (jul.day-2451545)/36525
  
  geom.mean.long <- (280.46646+jul.cent*(36000.76983 + jul.cent*0.0003032))%%360
  
  geom.mean.anom <- 357.52911+jul.cent*(35999.05029 - 0.0001537*jul.cent)
  
  eccent.earth.orbit <- 0.016708634-jul.cent*(0.000042037+0.0000001267*jul.cent)
  
  sun.eq.cent <- sin(geom.mean.anom*(pi/180))*(1.914602-jul.cent*(0.004817+0.000014*jul.cent))+sin(2*geom.mean.anom*(pi/180))*(0.019993-0.000101*jul.cent)+sin(3*geom.mean.anom*(pi/180))*0.000289
  
  sun.true.long <- geom.mean.long + sun.eq.cent
  
  sun.true.anom <- geom.mean.anom + sun.eq.cent
  
  sun.rad.vec <- (1.000001018*(1-eccent.earth.orbit*eccent.earth.orbit))/(1+eccent.earth.orbit*cos(sun.true.anom*(pi/180)))
  
  sun.app.long <- sun.true.long-0.00569-0.00478*sin((125.04-(1934.136*jul.cent))*(pi/180))
  
  mean.obliq.ecc <- 23+(26+((21.448-jul.cent*(46.815+jul.cent*(0.00059-jul.cent*0.001813))))/60)/60
  
  obliq.corr <- mean.obliq.ecc+(0.00256*cos((125.04-(1934.136*jul.cent))*(pi/180)))
  
  sun.rt.ascen <- atan2((cos((obliq.corr*(pi/180)))*sin((sun.app.long*(pi/180)))),cos((sun.app.long*(pi/180))))*(180/pi)
  
  sun.decline <- asin(sin((obliq.corr*(pi/180)))*sin((sun.app.long*(pi/180))))*(180/pi)
  
  var.y <- tan(((obliq.corr/2)*(pi/180)))*tan(((obliq.corr/2)*(pi/180)))
  
  eq.time <- 4*((180/pi)*(var.y*sin(2*(geom.mean.long*(pi/180)))-2*eccent.earth.orbit*sin((geom.mean.anom*(pi/180)))+4*eccent.earth.orbit*var.y*sin((geom.mean.anom*(pi/180)))*cos(2*(geom.mean.long*(pi/180)))-0.5*var.y*var.y*sin(4*(geom.mean.long*(pi/180)))-1.25*eccent.earth.orbit*eccent.earth.orbit*sin(2*(geom.mean.anom*(pi/180)))))
  
  ha.sunrise <- (180/pi)*(acos(cos((90.833*(pi/180)))/(cos((lat*(pi/180)))*cos((sun.decline*(pi/180))))-tan((lat*(pi/180)))*tan((sun.decline*(pi/180)))))
  
  solar.noon <- (720-4*long-eq.time+tz*60)/1440
  
  sunrise.time <- solar.noon-ha.sunrise*4/1440
  
  sunset.time <- solar.noon+ha.sunrise*4/1440
  
  # Rescale time of day for each observation.
  rescale.time <- ifelse(time <= sunrise.time,((0.25-0)*(time-0)/(sunrise.time-0))+0, ifelse(time<=solar.noon & time>sunrise.time, ((0.5-0.25)*(time-sunrise.time)/(solar.noon-sunrise.time))+0.25, ifelse(time<=sunset.time & time>solar.noon,((0.75-0.5)*(time-solar.noon)/(sunset.time-solar.noon))+0.5, (1-0.75)*(time-sunset.time)/(1-sunset.time)+0.75)))
  
  date <- format(datetime, format = "%Y-%m-%d")
  
  date2 <- as.POSIXct(date)
  
  RescaleDate <- date2 + (rescale.time * 24 *3600)
  
  if(is.null(dataframe)) return(list(rescale.time, time, sunrise.time, solar.noon, sunset.time, RescaleDate))
  
  dataframe$rescale.time <- rescale.time
  
  dataframe$time <- time
  
  dataframe$sunrise <- sunrise.time
  
  dataframe$noon <- solar.noon
  
  dataframe$sunset <- sunset.time
  
  dataframe$RescaleDate <- RescaleDate
  
  return(dataframe)
}


############################################
###### Apply Function to Rescale Time ######
############################################

# This code applies the re.time function above to rescale standard time to solar time for each observation at each camera site. 
# Utilize the re.time function and feed information to original dataframe (creates a dataframe with new columns added).
data.detect5 <- re.time(data.detect2$ImageDate2, data.detect2$ycoord, data.detect2$xcoord, -4, data.detect2)


###################################
###### Remove Sensitive Info ######
###################################

# Location data is private to Michigan State University and it is removed in this step.
# After this step, the camera-trap data is prepped (missing data addressed later).

# Remove specific coordinate info to protect privacy.
rm(cam_locs)
rm(data.detect)
rm(data.detect2)

# This is the prepared camera-trap data for all camera locations across all townships.
data.detect5 <- subset(data.detect5, select=-c(xcoord,ycoord))


##########################################
###### Create Additional SpeciesIDs ######
##########################################

# This chunk of code uses the Unknown Deer DetailText column to create new SpeciesIDs (for supplemental explorations).
# Specifically, creating Unknown Deer Antlered and Unknown Deer Antlerless (and leaving Unknown deer as Unknown Deer Cannot See Head). 
# This optional step will allow for the inclusion of more antlerless and antlered deer in the count data.

# Assign new SpeciesIds based on DetailText (Antlered or Antlerless).
data.detect5$SpeciesID[ data.detect5$DetailText == "Antlered" ] <- "20" # Unknown Antlered SpeciesID now = "20"
data.detect5$SpeciesID[ data.detect5$DetailText == "Antlerless" ] <- "30" # Unknown Antlerless SpeciesID now = "30"


#####################################
###### Subset Data by Township ######
#####################################

# This code uses the unique IDs of camera sites to subset data by township.

# Subset by townships in the NonAPR Zone. 
# Note that due to the pandemic, the 2020 season had a reduced field effort (i.e., we did not sample Sherman or Nelson Township).
sherman <- subset(data.detect5,LocationName < 12000)
tyrone <- subset(data.detect5,LocationName > 11999.5 & LocationName <13000)
bowne <- subset(data.detect5,LocationName > 12999.5 & LocationName <14000)
nelson <- subset(data.detect5,LocationName > 13999.5 & LocationName <15000)

# Subset by townships in the APR Zone. 
# Note that due to the pandemic, the 2020 season had a reduced field effort (i.e., we did not sample Sheridan or Douglass Township).
sheridan <- subset(data.detect5,LocationName > 20000 & LocationName <22000)
easton <- subset(data.detect5,LocationName > 21999.5 & LocationName <23000)
bushnell <- subset(data.detect5,LocationName > 22999.5 & LocationName <24000)
douglass <- subset(data.detect5,LocationName > 23999.5)


##################################
###### Address Missing Data ######
##################################

# This code addresses occurrences of missing data (NAs).
# Missing data occurred when individual cameras were out of operation (e.g., camera failure, theft, full SD card).
# This code uses the unique IDs of camera sites to subset data by township.

# Load package to reshape missing data.
library(reshape2)

# Read in the missing-data records (maintained during the field season).
missing <- read.csv("2022_MissingData_05Apr2023_smg.csv") 

# Reshape missing data and format date.
melted <- melt(missing, id.vars=c("LocationName"))
melted <- melted[with(melted, order(LocationName, variable, value)),]
melted$variable <- substring(melted$variable,2)
melted$Date <- as.POSIXct(melted$variable, format="%m.%d.%Y")
melted$day <- floor(as.numeric(julian(melted$Date))) 
melted$value2 <- ifelse(melted$value>0,1,ifelse(melted$value<0,-1,0)) 

# Subset missing data by townships in the NonAPR Zone.
sherman.miss <- subset(melted,LocationName < 12000)
tyrone.miss <- subset(melted,LocationName > 11999.5 & LocationName <13000)
bowne.miss <- subset(melted,LocationName > 12999.5 & LocationName <14000)
nelson.miss <- subset(melted,LocationName > 13999.5 & LocationName <15000)

# Subset missing data by townships in the APR Zone.
sheridan.miss <- subset(melted,LocationName > 20000 & LocationName <22000)
easton.miss <- subset(melted,LocationName > 21999.5 & LocationName <23000)
bushnell.miss <- subset(melted,LocationName > 22999.5 & LocationName <24000)
douglass.miss <- subset(melted,LocationName > 23999.5)
