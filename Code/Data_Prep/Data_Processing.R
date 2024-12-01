
############################################
################ Preface ###################
############################################

# This script was used for each sex-and-age class individually.
# Below is an example for legal-antlered deer. 
# Note when running other sex-and-age classes, hardcoding is needed when loading data and saving output.

# Load packages.

#############################################
###### Create Data-Processing Function ######
#############################################

# Function arguments include township, missing data, daily-sampling frequency, sex-and-age class, and sampling period start and end dates (Julian date).
section.prep <- function(sherman,miss,period=1,demographic="ALL", min.date, max.date){ # Sherman is a generic township in function.
  
  # Remove images that include camera setup and camera takedown.
  sherman <- sherman[!(sherman$SpeciesID==12),]
  summary(sherman)
  
  # Remove rows where ImgID is NA.
  sherman <- sherman[complete.cases(sherman$ImgID),]
  
  # Define the demographic argument of the function using SpeciesID codes.
  # Deer images have SpeciesID codes for each sex and age class.
  if(demographic==9){ # Legal antlered
    sherman.deer <- sherman[sherman$SpeciesID %in% c(9),] 
  }else{
    if(demographic==10){ # Sublegal antlered
      sherman.deer <- sherman[sherman$SpeciesID %in% c(10),]
    }else{
      if(demographic==7){ # Females
        sherman.deer <- sherman[sherman$SpeciesID %in% c(7),]
      }else{
        if(demographic==8){ # Fawns
          sherman.deer <- sherman[sherman$SpeciesID %in% c(8),]
        }else{
          if(demographic==11){ # Unknown deer
            sherman.deer <- sherman[sherman$SpeciesID %in% c(11),]
          }else{
            if(demographic=="antlerless"){ # Females + Fawns 
              sherman.deer <- (sherman[sherman$SpeciesID %in% c(7,8,30),])
            }else{
              if(demographic=="antlered"){ # Legal antlered + Sublegal antlered
                sherman.deer <- sherman[sherman$SpeciesID %in% c(9,10,20),] 
              }else{
                sherman.deer <- sherman[sherman$SpeciesID %in% c(9,10,7,8,11,20,30),] # All-deer default also includes 3 classes of Unknowns
              }}}}}}}
  
  # Aggregate deer counts per image (here and below for daily sampling periods).
  sherman.deer.agg <- aggregate(Individuals~ImgID + SpeciesID, data=sherman.deer, sum)
  
  # Clean up the dataframe.
  sherman.deer1 <- sherman.deer[c("LocationName","ImageDate2","rescale.time","ImgID","SpeciesID")]
  sherman.deer1 <- unique(sherman.deer1)
  
  # Join total individuals to each image in sherman.deer.
  sherman.total <- merge(x = sherman.deer1, y = sherman.deer.agg, by=c("ImgID","SpeciesID"), all.y = TRUE)
  sherman.total$day <- floor(as.numeric(julian(sherman.total$ImageDate2))) 
  
  # Define the period argument of the function by utilizing rescaled time.
  # Subdivide sampling interval by dividing the day into intervals of interest.
  # 4 sampling intervals per day.
  if(period==4){ 
    sherman.total$period <- ifelse(sherman.total$rescale.time>=0 & sherman.total$rescale.time<1/(as.numeric(period)), 1, ifelse(sherman.total$rescale.time>=1/(as.numeric(period)) & sherman.total$rescale.time<2/(as.numeric(period)), 2, ifelse(sherman.total$rescale.time>=2/(as.numeric(period)) & sherman.total$rescale.time<3/(as.numeric(period)),3,4)))
  }
  
  # 3 sampling intervals per day.
  if(period==3){ 
    sherman.total$period <- ifelse(sherman.total$rescale.time>=0 & sherman.total$rescale.time<1/(as.numeric(period)), 1, ifelse(sherman.total$rescale.time>=1/(as.numeric(period)) & sherman.total$rescale.time<2/(as.numeric(period)), 2, 3))
  }
  
    # 2 sampling intervals per day.
  if(period==2){ 
    sherman.total$period <- ifelse(sherman.total$rescale.time>=0 & sherman.total$rescale.time<1/(as.numeric(period)), 1, 2)
  }
  
    # 1 sampling interval per day.
  if(period==1){ 
    sherman.total$period <- 1
  }
  
  # Crepuscular sampling window, defined as 1 hour before and after both sunrise and sunset. 
  # This period includes images from 2 sampling windows, one around sunrise and one around sunset.
  # Crepuscular sampling excludes images outside of the defined crepuscular windows.
  if(period=="C"){
    sherman.total$period <- ifelse(sherman.total$rescale.time>=(0.25-(1/24)) & sherman.total$rescale.time<=(0.25+(1/24)), 1, ifelse(sherman.total$rescale.time>=(0.75-(1/24)) & sherman.total$rescale.time<=(0.75+(1/24)), 2, 0))
    
    # Remove records from outside of the crepuscular period.
    sherman.total <- sherman.total[!sherman.total$period==0,]
    period=2
  }
  
  # Determine max number of each type of deer in each day/period/location.
  sherman.count1 <- aggregate(Individuals ~ day + period + LocationName + SpeciesID, data=sherman.total,max)
  
  # Sum the count of deer per day/period/location.
  sherman.count <- aggregate(Individuals ~ day + period + LocationName, data=sherman.count1,sum)
  
  # Reorder things.
  sherman.count <- sherman.count[with(sherman.count, order(LocationName, day, period)),]
  
  # Define the length of the entire season with a min and max date
  min.date <- min.date   
  max.date <- max.date
  
  # Make a complete table of all possible days and time for each site.
  zeroes <- expand.grid(LocationName = unique(sherman$LocationName),day = seq(min.date,max.date), period=seq(1:period))
  
  # Resort the dataframe.
  zeroes <- zeroes[with(zeroes, order(LocationName, day, period)),]
  test <- merge(sherman.count,zeroes, by=c("LocationName", "day", "period"), all=TRUE)
  test <- test[!(test$day>max.date | test$day<min.date),]  # Revise for each subsampling scenario
  
  # Account for the min and max days within the missing data.
  miss <- miss[miss$day<=max.date & miss$day>=min.date,]
  test2 <- merge(test,miss, by=c("LocationName", "day"), all=TRUE)
  test2$count <- ifelse(is.na(test2$Individuals) & test2$value2>-0.5,0,
                        ifelse(test2$Individuals>0,test2$Individuals,9999))
  
  # Create new column for day and period.
  test2$daytime <- (test2$day*10) + test2$period
  
  # Simplify dataframe.
  test3 <- test2[,c("LocationName", "daytime", "count")]
  
  # Section-prep function returns out.final.
  out.final <<- reshape(test3, idvar = "LocationName", timevar = "daytime", direction = "wide")
}


############################################
###### Apply Data-Processing Function ######
############################################

# This chunk of code utilizes the data-processing function (section.prep) to produce max-daily counts by sex-an-age class.
# The function is applied to each township one at a time, then the results are pooled together into a single dataframe. 
# The function is applied to one sex-and-age class at a time.

# First define the length of the entire season with a min and max date.
#min.date <- 18092   # 15 JUL 2019
#max.date <- 18154   # 15 SEP 2019
#min.date <- 18458   # July 15 2020
#max.date <- 18520   # Sept 15 2020
#min.date <- 18823   # July 15 2021
#max.date <- 18885   # Sept 15 2021
min.date <- 19188   # July 15 2022
max.date <- 19250   # Sept 15 2022


###############################
###### All Antlered Deer ######
###############################

# Apply data-processing function to each township individually for the 1/day sampling frequency.
section.prep(bushnell, bushnell.miss, period="1", demographic="antlered", min.date, max.date)
bushnell.final.2022antlered <- out.final

section.prep(bowne, bowne.miss, period="1", demographic="antlered", min.date, max.date)
bowne.final.2022antlered <- out.final

section.prep(easton, easton.miss, period="1", demographic="antlered", min.date, max.date)
easton.final.2022antlered <- out.final

section.prep(tyrone, tyrone.miss, period="1", demographic="antlered", min.date, max.date)
tyron.final.2022antlered <- out.final

section.prep(sheridan, sheridan.miss, period="1", demographic="antlered", min.date, max.date)
sheridan.final.antlered <- out.final

section.prep(sherman, sherman.miss, period="1", demographic="antlered", min.date, max.date)
sherman.final.antlered <- out.final

section.prep(douglass, douglass.miss, period="1", demographic="antlered", min.date, max.date)
douglas.final.antlered <- out.final

section.prep(nelson, nelson.miss, period="1", demographic="antlered", min.date, max.date)
nelson.final.antlered <- out.final

# Pool township data together.
final.2022antlered.prep <- rbind(bushnell.final.2022antlered, bowne.final.2022antlered, easton.final.2022antlered, tyron.final.2022antlered, sheridan.final.antlered, sherman.final.antlered, douglas.final.antlered, nelson.final.antlered)
final.2022antlered.freq.1 <- final.2022antlered.prep[order(final.2022antlered.prep$LocationName),]

# Save output.
write.csv(final.2022antlered.freq.1, "final.2022antlered.freq.1.csv", row.names = FALSE) 

#################################
###### All Antlerless Deer ######
#################################

# Apply data-processing function to each township individually for the 1/day sampling frequency.
section.prep(bushnell, bushnell.miss, period="1", demographic="antlerless", min.date, max.date)
bushnell.final.2022antlerless <- out.final

section.prep(bowne, bowne.miss, period="1", demographic="antlerless", min.date, max.date)
bowne.final.2022antlerless <- out.final

section.prep(easton, easton.miss, period="1", demographic="antlerless", min.date, max.date)
easton.final.2022antlerless <- out.final

section.prep(tyrone, tyrone.miss, period="1", demographic="antlerless", min.date, max.date)
tyron.final.2022antlerless <- out.final

section.prep(sheridan, sheridan.miss, period="1", demographic="antlerless", min.date, max.date)
sheridan.final.antlerless <- out.final

section.prep(sherman, sherman.miss, period="1", demographic="antlerless", min.date, max.date)
sherman.final.antlerless <- out.final

section.prep(douglass, douglass.miss, period="1", demographic="antlerless", min.date, max.date)
douglas.final.antlerless <- out.final

section.prep(nelson, nelson.miss, period="1", demographic="antlerless", min.date, max.date)
nelson.final.antlerless <- out.final

# Pool township data together.
final.2022antlerless.prep <- rbind(bushnell.final.2022antlerless, bowne.final.2022antlerless, easton.final.2022antlerless, tyron.final.2022antlerless, sheridan.final.antlerless, sherman.final.antlerless, douglas.final.antlerless, nelson.final.antlerless)
final.2022antlerless.freq.1 <- final.2022antlerless.prep[order(final.2022antlerless.prep$LocationName),]

# Save output.
write.csv(final.2022antlerless.freq.1, "final.2022antlerless.freq.1.csv", row.names = FALSE) 

######################
###### All Deer ######
######################

# Apply data-processing function to each township individually for the 1/day sampling frequency.
section.prep(bushnell, bushnell.miss, period="1", demographic="ALL", min.date, max.date)
bushnell.final.2022alldeer <- out.final

section.prep(bowne, bowne.miss, period="1", demographic="ALL", min.date, max.date)
bowne.final.2022alldeer <- out.final

section.prep(easton, easton.miss, period="1", demographic="ALL", min.date, max.date)
easton.final.2022alldeer <- out.final

section.prep(tyrone, tyrone.miss, period="1", demographic="ALL", min.date, max.date)
tyron.final.2022alldeer <- out.final

section.prep(sheridan, sheridan.miss, period="1", demographic="ALL", min.date, max.date)
sheridan.final.alldeer <- out.final

section.prep(sherman, sherman.miss, period="1", demographic="ALL", min.date, max.date)
sherman.final.alldeer <- out.final

section.prep(douglass, douglass.miss, period="1", demographic="ALL", min.date, max.date)
douglas.final.alldeer <- out.final

section.prep(nelson, nelson.miss, period="1", demographic="ALL", min.date, max.date)
nelson.final.alldeer <- out.final

# Pool township data together.
final.2022alldeer.prep <- rbind(bushnell.final.2022alldeer, bowne.final.2022alldeer, easton.final.2022alldeer, tyron.final.2022alldeer, sheridan.final.alldeer, sherman.final.alldeer, douglas.final.alldeer, nelson.final.alldeer)
final.2022alldeer.freq.1 <- final.2022alldeer.prep[order(final.2022alldeer.prep$LocationName),]

# Save output.
write.csv(final.2022alldeer.freq.1, "final.2022alldeer.freq.1.csv", row.names = FALSE) 

#########################
###### Female Deer ######
#########################

# Apply data-processing function to each township individually for the 1/day sampling frequency.
# Note that the 2022 demographic code for females may defer from other years.
section.prep(bushnell, bushnell.miss, period="1", demographic="7", min.date, max.date)
bushnell.final.2022does <- out.final

section.prep(bowne, bowne.miss, period="1", demographic="7", min.date, max.date)
bowne.final.2022does <- out.final

section.prep(easton, easton.miss, period="1", demographic="7", min.date, max.date)
easton.final.2022does <- out.final

section.prep(tyrone, tyrone.miss, period="1", demographic="7", min.date, max.date)
tyron.final.2022does <- out.final

section.prep(sheridan, sheridan.miss, period="1", demographic="7", min.date, max.date)
sheridan.final.2022does <- out.final

section.prep(sherman, sherman.miss, period="1", demographic="7", min.date, max.date)
sherman.final.2022does <- out.final

section.prep(douglass, douglass.miss, period="1", demographic="7", min.date, max.date)
douglas.final.2022does <- out.final

section.prep(nelson, nelson.miss, period="1", demographic="7", min.date, max.date)
nelson.final.2022does <- out.final

# Pool township data together.
final.2022does.prep <- rbind(bushnell.final.2022does, bowne.final.2022does, easton.final.2022does, tyron.final.2022does, sheridan.final.2022does, sherman.final.2022does, douglas.final.2022does, nelson.final.2022does)
final.2022does.freq.1 <- final.2022does.prep[order(final.2022does.prep$LocationName),]

# Save output.
write.csv(final.2022does.freq.1, "final.2022does.freq.1.csv", row.names = FALSE)

###################
###### Fawns ######
###################

# Apply data-processing function to each township individually for the 1/day sampling frequency for fawns.
# Note that the 2022 demographic code may defer from other years,
section.prep(bushnell, bushnell.miss, period="1", demographic="8", min.date, max.date)
bushnell.final.2022allfawns <- out.final

section.prep(bowne, bowne.miss, period="1", demographic="8", min.date, max.date)
bowne.final.2022allfawns <- out.final

section.prep(easton, easton.miss, period="1", demographic="8", min.date, max.date)
easton.final.2022allfawns <- out.final

section.prep(tyrone, tyrone.miss, period="1", demographic="8", min.date, max.date)
tyron.final.2022allfawns <- out.final

section.prep(sheridan, sheridan.miss, period="1", demographic="8", min.date, max.date)
sheridan.final.2022fawns <- out.final

section.prep(sherman, sherman.miss, period="1", demographic="8", min.date, max.date)
sherman.final.2022fawns <- out.final

section.prep(douglass, douglass.miss, period="1", demographic="8", min.date, max.date)
douglas.final.2022fawns <- out.final

section.prep(nelson, nelson.miss, period="1", demographic="8", min.date, max.date)
nelson.final.2022fawns <- out.final

# Pool township data together.
final.2022allfawns.prep <- rbind(bushnell.final.2022allfawns, bowne.final.2022allfawns, easton.final.2022allfawns, tyron.final.2022allfawns, sheridan.final.2022fawns, sherman.final.2022fawns, douglas.final.2022fawns, nelson.final.2022fawns)
final.2022allfawns.freq.1 <- final.2022allfawns.prep[order(final.2022allfawns.prep$LocationName),]

# save output.
write.csv(final.2022allfawns.freq.1, "final.2022allfawns.freq.1.csv", row.names = FALSE)

####################################
###### Sublegal-Antlered Deer ######
####################################

# Apply data-processing function to each township individually for the 1/day sampling frequency for sublegal.
# Note that the 2022 demographic code for sublegal may defer from other years.
section.prep(bushnell, bushnell.miss, period="1", demographic="10", min.date, max.date)
bushnell.final.2022sublegal <- out.final

section.prep(bowne, bowne.miss, period="1", demographic="10", min.date, max.date)
bowne.final.2022sublegal <- out.final

section.prep(easton, easton.miss, period="1", demographic="10", min.date, max.date)
easton.final.2022sublegal <- out.final

section.prep(tyrone, tyrone.miss, period="1", demographic="10", min.date, max.date)
tyron.final.2022sublegal <- out.final

section.prep(sheridan, sheridan.miss, period="1", demographic="10", min.date, max.date)
sheridan.final.2022sublegal <- out.final

section.prep(sherman, sherman.miss, period="1", demographic="10", min.date, max.date)
sherman.final.2022sublegal <- out.final

section.prep(douglass, douglass.miss, period="1", demographic="10", min.date, max.date)
douglas.final.2022sublegal <- out.final

section.prep(nelson, nelson.miss, period="1", demographic="10", min.date, max.date)
nelson.final.2022sublegal <- out.final

# Pool township data together.
final.2022sublegal.prep <- rbind(bushnell.final.2022sublegal, bowne.final.2022sublegal, easton.final.2022sublegal, tyron.final.2022sublegal, sheridan.final.2022sublegal, sherman.final.2022sublegal, douglas.final.2022sublegal, nelson.final.2022sublegal)
final.2022sublegal.freq.1 <- final.2022sublegal.prep[order(final.2022sublegal.prep$LocationName),]

# Save output.
write.csv(final.2022sublegal.freq.1, "final.2022sublegal.freq.1.csv", row.names = FALSE)

#################################
###### Legal-Antlered Deer ######
#################################

# Apply data-processing function to each township individually for the 1/day sampling frequency for legal bucks. 
# Note that the 2022 demographic code for legal may defer from other years..
section.prep(bushnell, bushnell.miss, period="1", demographic="9", min.date, max.date)
bushnell.final.2022legal <- out.final

section.prep(bowne, bowne.miss, period="1", demographic="9", min.date, max.date)
bowne.final.2022legal <- out.final

section.prep(easton, easton.miss, period="1", demographic="9", min.date, max.date)
easton.final.2022legal <- out.final

section.prep(tyrone, tyrone.miss, period="1", demographic="9", min.date, max.date)
tyron.final.2022legal <- out.final

section.prep(sheridan, sheridan.miss, period="1", demographic="9", min.date, max.date)
sheridan.final.2022legal <- out.final

section.prep(sherman, sherman.miss, period="1", demographic="9", min.date, max.date)
sherman.final.2022legal <- out.final

section.prep(douglass, douglass.miss, period="1", demographic="9", min.date, max.date)
douglas.final.2022legal <- out.final

section.prep(nelson, nelson.miss, period="1", demographic="9", min.date, max.date)
nelson.final.2022legal <- out.final

# Pool township data together.
final.2022legal.prep <- rbind(bushnell.final.2022legal, bowne.final.2022legal, easton.final.2022legal, tyron.final.2022legal, sheridan.final.2022legal, sherman.final.2022legal, douglas.final.2022legal, nelson.final.2022legal)
final.2022legal.freq.1 <- final.2022legal.prep[order(final.2022legal.prep$LocationName),]

# Save output.
write.csv(final.2022legal.freq.1, "final.2022legal.freq.1.csv", row.names = FALSE)


############################################################
###### Produce Weekly Max Counts By Sex-And-Age Class ######
############################################################

# This chunk of code uses the daily counts created above to get max counts by week.
# Using the above count data (frequency = 1 count/day), subset counts by week (week 1 - week 9).
# Take the max count per week (i.e., per subset) and remove non-max counts.
# Lastly, merge all subset data together to get a weekly detection history (i.e., weekly max counts). 
# Repeat this process one sex-and-age class at a time. 
# The final output here were used as the count data in the N-mixture models.

# Load package
library(dplyr)

#############################
###### Antlerless Deer ######
#############################

# Start with the wrangled, prepped data from the 1/day count scenario.
df <- read.csv("final.2022antlerless.freq.1.csv")

# Keep LocationName (column 1) and subset data by week (week1 - week9) based on column index (63 days total = 9 weeks).
week1 <- df %>% select(1,2:8)
week2 <- df %>% select(1,9:15)
week3 <- df %>% select(1,16:22)
week4 <- df %>% select(1,23:29)
week5 <- df %>% select(1,30:36)
week6 <- df %>% select(1,37:43)
week7 <- df %>% select(1,44:50)
week8 <- df %>% select(1,51:57)
week9 <- df %>% select(1,58:64)

# Add new column to each week subset and populate with the max count from each week.
week1$week1.count <- do.call(pmax, c(week1[2:8], list(na.rm=TRUE)))
week2$week2.count <- do.call(pmax, c(week2[2:8], list(na.rm=TRUE)))
week3$week3.count <- do.call(pmax, c(week3[2:8], list(na.rm=TRUE)))
week4$week4.count <- do.call(pmax, c(week4[2:8], list(na.rm=TRUE)))
week5$week5.count <- do.call(pmax, c(week5[2:8], list(na.rm=TRUE)))
week6$week6.count <- do.call(pmax, c(week6[2:8], list(na.rm=TRUE)))
week7$week7.count <- do.call(pmax, c(week7[2:8], list(na.rm=TRUE)))
week8$week8.count <- do.call(pmax, c(week8[2:8], list(na.rm=TRUE)))
week9$week9.count <- do.call(pmax, c(week9[2:8], list(na.rm=TRUE)))

# Subset data to keep only LocationName and week.count.
week1 <- subset(week1, select=c(LocationName, week1.count))
week2 <- subset(week2, select=c(LocationName, week2.count))
week3 <- subset(week3, select=c(LocationName, week3.count))
week4 <- subset(week4, select=c(LocationName, week4.count))
week5 <- subset(week5, select=c(LocationName, week5.count))
week6 <- subset(week6, select=c(LocationName, week6.count))
week7 <- subset(week7, select=c(LocationName, week7.count))
week8 <- subset(week8, select=c(LocationName, week8.count))
week9 <- subset(week9, select=c(LocationName, week9.count))

# Merge subset data (i.e., max counts by week) one at a time to create weekly detection history. 
final.2022antlerless.freq.1.week <- merge(week1, week2, by="LocationName", all.x = TRUE)
final.2022antlerless.freq.1.week <- merge(final.2022antlerless.freq.1.week, week3, by="LocationName", all.x = TRUE)
final.2022antlerless.freq.1.week <- merge(final.2022antlerless.freq.1.week, week4, by="LocationName", all.x = TRUE)
final.2022antlerless.freq.1.week <- merge(final.2022antlerless.freq.1.week, week5, by="LocationName", all.x = TRUE)
final.2022antlerless.freq.1.week <- merge(final.2022antlerless.freq.1.week, week6, by="LocationName", all.x = TRUE)
final.2022antlerless.freq.1.week <- merge(final.2022antlerless.freq.1.week, week7, by="LocationName", all.x = TRUE)
final.2022antlerless.freq.1.week <- merge(final.2022antlerless.freq.1.week, week8, by="LocationName", all.x = TRUE)
final.2022antlerless.freq.1.week <- merge(final.2022antlerless.freq.1.week, week9, by="LocationName", all.x = TRUE)

# Save output.
write.csv(final.2022antlerless.freq.1.week, "final.2022antlerless.freq.1.week.csv", row.names=FALSE)


###########################
###### Antlered Deer ######
###########################

# Start with the wrangled, prepped data from the 1/day count scenario.
df <- read.csv("final.2022antlered.freq.1.csv")

# Keep LocationName (column 1) and subset data by week (week1 - week9) based on column index (63 days total = 9 weeks)
week1 <- df %>% select(1,2:8)
week2 <- df %>% select(1,9:15)
week3 <- df %>% select(1,16:22)
week4 <- df %>% select(1,23:29)
week5 <- df %>% select(1,30:36)
week6 <- df %>% select(1,37:43)
week7 <- df %>% select(1,44:50)
week8 <- df %>% select(1,51:57)
week9 <- df %>% select(1,58:64)

# Add new column to each week subset and populate with the max count from each week.
week1$week1.count <- do.call(pmax, c(week1[2:8], list(na.rm=TRUE)))
week2$week2.count <- do.call(pmax, c(week2[2:8], list(na.rm=TRUE)))
week3$week3.count <- do.call(pmax, c(week3[2:8], list(na.rm=TRUE)))
week4$week4.count <- do.call(pmax, c(week4[2:8], list(na.rm=TRUE)))
week5$week5.count <- do.call(pmax, c(week5[2:8], list(na.rm=TRUE)))
week6$week6.count <- do.call(pmax, c(week6[2:8], list(na.rm=TRUE)))
week7$week7.count <- do.call(pmax, c(week7[2:8], list(na.rm=TRUE)))
week8$week8.count <- do.call(pmax, c(week8[2:8], list(na.rm=TRUE)))
week9$week9.count <- do.call(pmax, c(week9[2:8], list(na.rm=TRUE)))

# Subset data to keep only LocationName and week1.count
week1 <- subset(week1, select=c(LocationName, week1.count))
week2 <- subset(week2, select=c(LocationName, week2.count))
week3 <- subset(week3, select=c(LocationName, week3.count))
week4 <- subset(week4, select=c(LocationName, week4.count))
week5 <- subset(week5, select=c(LocationName, week5.count))
week6 <- subset(week6, select=c(LocationName, week6.count))
week7 <- subset(week7, select=c(LocationName, week7.count))
week8 <- subset(week8, select=c(LocationName, week8.count))
week9 <- subset(week9, select=c(LocationName, week9.count))

# Merge subset data (i.e., max counts by week) one at a time to create weekly detection history. 
final.2022antlered.freq.1.week <- merge(week1, week2, by="LocationName", all.x = TRUE)
final.2022antlered.freq.1.week <- merge(final.2022antlered.freq.1.week, week3, by="LocationName", all.x = TRUE)
final.2022antlered.freq.1.week <- merge(final.2022antlered.freq.1.week, week4, by="LocationName", all.x = TRUE)
final.2022antlered.freq.1.week <- merge(final.2022antlered.freq.1.week, week5, by="LocationName", all.x = TRUE)
final.2022antlered.freq.1.week <- merge(final.2022antlered.freq.1.week, week6, by="LocationName", all.x = TRUE)
final.2022antlered.freq.1.week <- merge(final.2022antlered.freq.1.week, week7, by="LocationName", all.x = TRUE)
final.2022antlered.freq.1.week <- merge(final.2022antlered.freq.1.week, week8, by="LocationName", all.x = TRUE)
final.2022antlered.freq.1.week <- merge(final.2022antlered.freq.1.week, week9, by="LocationName", all.x = TRUE)

# Save final output for use in N-mixture model.
write.csv(final.2022antlered.freq.1.week, "final.2022antlered.freq.1.week.csv", row.names=FALSE)


###########################
###### All Deer ######
###########################

# Start with the wrangled, prepped data from the 1/day count scenario.
df <- read.csv("final.2022alldeer.freq.1.csv")

# Keep LocationName (column 1) and subset data by week (week1 - week9) based on column index (63 days total = 9 weeks)
week1 <- df %>% select(1,2:8)
week2 <- df %>% select(1,9:15)
week3 <- df %>% select(1,16:22)
week4 <- df %>% select(1,23:29)
week5 <- df %>% select(1,30:36)
week6 <- df %>% select(1,37:43)
week7 <- df %>% select(1,44:50)
week8 <- df %>% select(1,51:57)
week9 <- df %>% select(1,58:64)

# Add new column to each week subset and populate with the max count from each week.
week1$week1.count <- do.call(pmax, c(week1[2:8], list(na.rm=TRUE)))
week2$week2.count <- do.call(pmax, c(week2[2:8], list(na.rm=TRUE)))
week3$week3.count <- do.call(pmax, c(week3[2:8], list(na.rm=TRUE)))
week4$week4.count <- do.call(pmax, c(week4[2:8], list(na.rm=TRUE)))
week5$week5.count <- do.call(pmax, c(week5[2:8], list(na.rm=TRUE)))
week6$week6.count <- do.call(pmax, c(week6[2:8], list(na.rm=TRUE)))
week7$week7.count <- do.call(pmax, c(week7[2:8], list(na.rm=TRUE)))
week8$week8.count <- do.call(pmax, c(week8[2:8], list(na.rm=TRUE)))
week9$week9.count <- do.call(pmax, c(week9[2:8], list(na.rm=TRUE)))

# Subset data to keep only LocationName and week1.count
week1 <- subset(week1, select=c(LocationName, week1.count))
week2 <- subset(week2, select=c(LocationName, week2.count))
week3 <- subset(week3, select=c(LocationName, week3.count))
week4 <- subset(week4, select=c(LocationName, week4.count))
week5 <- subset(week5, select=c(LocationName, week5.count))
week6 <- subset(week6, select=c(LocationName, week6.count))
week7 <- subset(week7, select=c(LocationName, week7.count))
week8 <- subset(week8, select=c(LocationName, week8.count))
week9 <- subset(week9, select=c(LocationName, week9.count))

# Merge subset data (i.e., max counts by week) one at a time to create weekly detection history. 
final.2022alldeer.freq.1.week <- merge(week1, week2, by="LocationName", all.x = TRUE)
final.2022alldeer.freq.1.week <- merge(final.2022alldeer.freq.1.week, week3, by="LocationName", all.x = TRUE)
final.2022alldeer.freq.1.week <- merge(final.2022alldeer.freq.1.week, week4, by="LocationName", all.x = TRUE)
final.2022alldeer.freq.1.week <- merge(final.2022alldeer.freq.1.week, week5, by="LocationName", all.x = TRUE)
final.2022alldeer.freq.1.week <- merge(final.2022alldeer.freq.1.week, week6, by="LocationName", all.x = TRUE)
final.2022alldeer.freq.1.week <- merge(final.2022alldeer.freq.1.week, week7, by="LocationName", all.x = TRUE)
final.2022alldeer.freq.1.week <- merge(final.2022alldeer.freq.1.week, week8, by="LocationName", all.x = TRUE)
final.2022alldeer.freq.1.week <- merge(final.2022alldeer.freq.1.week, week9, by="LocationName", all.x = TRUE)

# Save final output for use in N-mixture model.
write.csv(final.2022alldeer.freq.1.week, "final.2022alldeer.freq.1.week.csv", row.names=FALSE)


###########################
###### Female Deer ########
###########################

# Start with the wrangled, prepped data from the 1/day count scenario.
df <- read.csv("final.2022does.freq.1.csv")

# Keep LocationName (column 1) and subset data by week (week1 - week9) based on column index (63 days total = 9 weeks)
week1 <- df %>% select(1,2:8)
week2 <- df %>% select(1,9:15)
week3 <- df %>% select(1,16:22)
week4 <- df %>% select(1,23:29)
week5 <- df %>% select(1,30:36)
week6 <- df %>% select(1,37:43)
week7 <- df %>% select(1,44:50)
week8 <- df %>% select(1,51:57)
week9 <- df %>% select(1,58:64)

# Add new column to each week subset and populate with the max count from each week.
week1$week1.count <- do.call(pmax, c(week1[2:8], list(na.rm=TRUE)))
week2$week2.count <- do.call(pmax, c(week2[2:8], list(na.rm=TRUE)))
week3$week3.count <- do.call(pmax, c(week3[2:8], list(na.rm=TRUE)))
week4$week4.count <- do.call(pmax, c(week4[2:8], list(na.rm=TRUE)))
week5$week5.count <- do.call(pmax, c(week5[2:8], list(na.rm=TRUE)))
week6$week6.count <- do.call(pmax, c(week6[2:8], list(na.rm=TRUE)))
week7$week7.count <- do.call(pmax, c(week7[2:8], list(na.rm=TRUE)))
week8$week8.count <- do.call(pmax, c(week8[2:8], list(na.rm=TRUE)))
week9$week9.count <- do.call(pmax, c(week9[2:8], list(na.rm=TRUE)))

# Subset data to keep only LocationName and week1.count
week1 <- subset(week1, select=c(LocationName, week1.count))
week2 <- subset(week2, select=c(LocationName, week2.count))
week3 <- subset(week3, select=c(LocationName, week3.count))
week4 <- subset(week4, select=c(LocationName, week4.count))
week5 <- subset(week5, select=c(LocationName, week5.count))
week6 <- subset(week6, select=c(LocationName, week6.count))
week7 <- subset(week7, select=c(LocationName, week7.count))
week8 <- subset(week8, select=c(LocationName, week8.count))
week9 <- subset(week9, select=c(LocationName, week9.count))

# Merge subset data (i.e., max counts by week) one at a time to create weekly detection history. 
final.2022does.freq.1.week <- merge(week1, week2, by="LocationName", all.x = TRUE)
final.2022does.freq.1.week <- merge(final.2022does.freq.1.week, week3, by="LocationName", all.x = TRUE)
final.2022does.freq.1.week <- merge(final.2022does.freq.1.week, week4, by="LocationName", all.x = TRUE)
final.2022does.freq.1.week <- merge(final.2022does.freq.1.week, week5, by="LocationName", all.x = TRUE)
final.2022does.freq.1.week <- merge(final.2022does.freq.1.week, week6, by="LocationName", all.x = TRUE)
final.2022does.freq.1.week <- merge(final.2022does.freq.1.week, week7, by="LocationName", all.x = TRUE)
final.2022does.freq.1.week <- merge(final.2022does.freq.1.week, week8, by="LocationName", all.x = TRUE)
final.2022does.freq.1.week <- merge(final.2022does.freq.1.week, week9, by="LocationName", all.x = TRUE)

# Save final output for use in N-mixture model.
write.csv(final.2022does.freq.1.week, "final.2022does.freq.1.week.csv", row.names=FALSE)


###################
###### Fawns ######
###################

# Start with the wrangled, prepped data from the 1/day count scenario.
df <- read.csv("final.2022allfawns.freq.1.csv")

# Keep LocationName (column 1) and subset data by week (week1 - week9) based on column index (63 days total = 9 weeks)
week1 <- df %>% select(1,2:8)
week2 <- df %>% select(1,9:15)
week3 <- df %>% select(1,16:22)
week4 <- df %>% select(1,23:29)
week5 <- df %>% select(1,30:36)
week6 <- df %>% select(1,37:43)
week7 <- df %>% select(1,44:50)
week8 <- df %>% select(1,51:57)
week9 <- df %>% select(1,58:64)

# Add new column to each week subset and populate with the max count from each week.
week1$week1.count <- do.call(pmax, c(week1[2:8], list(na.rm=TRUE)))
week2$week2.count <- do.call(pmax, c(week2[2:8], list(na.rm=TRUE)))
week3$week3.count <- do.call(pmax, c(week3[2:8], list(na.rm=TRUE)))
week4$week4.count <- do.call(pmax, c(week4[2:8], list(na.rm=TRUE)))
week5$week5.count <- do.call(pmax, c(week5[2:8], list(na.rm=TRUE)))
week6$week6.count <- do.call(pmax, c(week6[2:8], list(na.rm=TRUE)))
week7$week7.count <- do.call(pmax, c(week7[2:8], list(na.rm=TRUE)))
week8$week8.count <- do.call(pmax, c(week8[2:8], list(na.rm=TRUE)))
week9$week9.count <- do.call(pmax, c(week9[2:8], list(na.rm=TRUE)))

# Subset data to keep only LocationName and week1.count
week1 <- subset(week1, select=c(LocationName, week1.count))
week2 <- subset(week2, select=c(LocationName, week2.count))
week3 <- subset(week3, select=c(LocationName, week3.count))
week4 <- subset(week4, select=c(LocationName, week4.count))
week5 <- subset(week5, select=c(LocationName, week5.count))
week6 <- subset(week6, select=c(LocationName, week6.count))
week7 <- subset(week7, select=c(LocationName, week7.count))
week8 <- subset(week8, select=c(LocationName, week8.count))
week9 <- subset(week9, select=c(LocationName, week9.count))

# Merge subset data (i.e., max counts by week) one at a time to create weekly detection history. 
final.2022allfawns.freq.1.week <- merge(week1, week2, by="LocationName", all.x = TRUE)
final.2022allfawns.freq.1.week <- merge(final.2022allfawns.freq.1.week, week3, by="LocationName", all.x = TRUE)
final.2022allfawns.freq.1.week <- merge(final.2022allfawns.freq.1.week, week4, by="LocationName", all.x = TRUE)
final.2022allfawns.freq.1.week <- merge(final.2022allfawns.freq.1.week, week5, by="LocationName", all.x = TRUE)
final.2022allfawns.freq.1.week <- merge(final.2022allfawns.freq.1.week, week6, by="LocationName", all.x = TRUE)
final.2022allfawns.freq.1.week <- merge(final.2022allfawns.freq.1.week, week7, by="LocationName", all.x = TRUE)
final.2022allfawns.freq.1.week <- merge(final.2022allfawns.freq.1.week, week8, by="LocationName", all.x = TRUE)
final.2022allfawns.freq.1.week <- merge(final.2022allfawns.freq.1.week, week9, by="LocationName", all.x = TRUE)

# Save final output for use in N-mixture model.
write.csv(final.2022allfawns.freq.1.week, "final.2022allfawns.freq.1.week.csv", row.names=FALSE)


###########################
###### Sublegal Antlered ######
###########################

# Start with the wrangled, prepped data from the 1/day count scenario.
df <- read.csv("final.2022sublegal.freq.1.csv")

# Keep LocationName (column 1) and subset data by week (week1 - week9) based on column index (63 days total = 9 weeks)
week1 <- df %>% select(1,2:8)
week2 <- df %>% select(1,9:15)
week3 <- df %>% select(1,16:22)
week4 <- df %>% select(1,23:29)
week5 <- df %>% select(1,30:36)
week6 <- df %>% select(1,37:43)
week7 <- df %>% select(1,44:50)
week8 <- df %>% select(1,51:57)
week9 <- df %>% select(1,58:64)

# Add new column to each week subset and populate with the max count from each week.
week1$week1.count <- do.call(pmax, c(week1[2:8], list(na.rm=TRUE)))
week2$week2.count <- do.call(pmax, c(week2[2:8], list(na.rm=TRUE)))
week3$week3.count <- do.call(pmax, c(week3[2:8], list(na.rm=TRUE)))
week4$week4.count <- do.call(pmax, c(week4[2:8], list(na.rm=TRUE)))
week5$week5.count <- do.call(pmax, c(week5[2:8], list(na.rm=TRUE)))
week6$week6.count <- do.call(pmax, c(week6[2:8], list(na.rm=TRUE)))
week7$week7.count <- do.call(pmax, c(week7[2:8], list(na.rm=TRUE)))
week8$week8.count <- do.call(pmax, c(week8[2:8], list(na.rm=TRUE)))
week9$week9.count <- do.call(pmax, c(week9[2:8], list(na.rm=TRUE)))

# Subset data to keep only LocationName and week1.count
week1 <- subset(week1, select=c(LocationName, week1.count))
week2 <- subset(week2, select=c(LocationName, week2.count))
week3 <- subset(week3, select=c(LocationName, week3.count))
week4 <- subset(week4, select=c(LocationName, week4.count))
week5 <- subset(week5, select=c(LocationName, week5.count))
week6 <- subset(week6, select=c(LocationName, week6.count))
week7 <- subset(week7, select=c(LocationName, week7.count))
week8 <- subset(week8, select=c(LocationName, week8.count))
week9 <- subset(week9, select=c(LocationName, week9.count))

# Merge subset data (i.e., max counts by week) one at a time to create weekly detection history. 
final.2022sublegal.freq.1.week <- merge(week1, week2, by="LocationName", all.x = TRUE)
final.2022sublegal.freq.1.week <- merge(final.2022sublegal.freq.1.week, week3, by="LocationName", all.x = TRUE)
final.2022sublegal.freq.1.week <- merge(final.2022sublegal.freq.1.week, week4, by="LocationName", all.x = TRUE)
final.2022sublegal.freq.1.week <- merge(final.2022sublegal.freq.1.week, week5, by="LocationName", all.x = TRUE)
final.2022sublegal.freq.1.week <- merge(final.2022sublegal.freq.1.week, week6, by="LocationName", all.x = TRUE)
final.2022sublegal.freq.1.week <- merge(final.2022sublegal.freq.1.week, week7, by="LocationName", all.x = TRUE)
final.2022sublegal.freq.1.week <- merge(final.2022sublegal.freq.1.week, week8, by="LocationName", all.x = TRUE)
final.2022sublegal.freq.1.week <- merge(final.2022sublegal.freq.1.week, week9, by="LocationName", all.x = TRUE)

# Save final output for use in N-mixture model.
write.csv(final.2022sublegal.freq.1.week, "final.2022sublegal.freq.1.week.csv", row.names=FALSE)


############################
###### Legal Antlered ######
############################

# Start with the wrangled, prepped data from the 1/day count scenario.
df <- read.csv("final.2022legal.freq.1.csv")

# Keep LocationName (column 1) and subset data by week (week1 - week9) based on column index (63 days total = 9 weeks)
week1 <- df %>% select(1,2:8)
week2 <- df %>% select(1,9:15)
week3 <- df %>% select(1,16:22)
week4 <- df %>% select(1,23:29)
week5 <- df %>% select(1,30:36)
week6 <- df %>% select(1,37:43)
week7 <- df %>% select(1,44:50)
week8 <- df %>% select(1,51:57)
week9 <- df %>% select(1,58:64)

# Add new column to each week subset and populate with the max count from each week.
week1$week1.count <- do.call(pmax, c(week1[2:8], list(na.rm=TRUE)))
week2$week2.count <- do.call(pmax, c(week2[2:8], list(na.rm=TRUE)))
week3$week3.count <- do.call(pmax, c(week3[2:8], list(na.rm=TRUE)))
week4$week4.count <- do.call(pmax, c(week4[2:8], list(na.rm=TRUE)))
week5$week5.count <- do.call(pmax, c(week5[2:8], list(na.rm=TRUE)))
week6$week6.count <- do.call(pmax, c(week6[2:8], list(na.rm=TRUE)))
week7$week7.count <- do.call(pmax, c(week7[2:8], list(na.rm=TRUE)))
week8$week8.count <- do.call(pmax, c(week8[2:8], list(na.rm=TRUE)))
week9$week9.count <- do.call(pmax, c(week9[2:8], list(na.rm=TRUE)))

# Subset data to keep only LocationName and week1.count
week1 <- subset(week1, select=c(LocationName, week1.count))
week2 <- subset(week2, select=c(LocationName, week2.count))
week3 <- subset(week3, select=c(LocationName, week3.count))
week4 <- subset(week4, select=c(LocationName, week4.count))
week5 <- subset(week5, select=c(LocationName, week5.count))
week6 <- subset(week6, select=c(LocationName, week6.count))
week7 <- subset(week7, select=c(LocationName, week7.count))
week8 <- subset(week8, select=c(LocationName, week8.count))
week9 <- subset(week9, select=c(LocationName, week9.count))

# Merge subset data (i.e., max counts by week) one at a time to create weekly detection history. 
final.2022legal.freq.1.week <- merge(week1, week2, by="LocationName", all.x = TRUE)
final.2022legal.freq.1.week <- merge(final.2022legal.freq.1.week, week3, by="LocationName", all.x = TRUE)
final.2022legal.freq.1.week <- merge(final.2022legal.freq.1.week, week4, by="LocationName", all.x = TRUE)
final.2022legal.freq.1.week <- merge(final.2022legal.freq.1.week, week5, by="LocationName", all.x = TRUE)
final.2022legal.freq.1.week <- merge(final.2022legal.freq.1.week, week6, by="LocationName", all.x = TRUE)
final.2022legal.freq.1.week <- merge(final.2022legal.freq.1.week, week7, by="LocationName", all.x = TRUE)
final.2022legal.freq.1.week <- merge(final.2022legal.freq.1.week, week8, by="LocationName", all.x = TRUE)
final.2022legal.freq.1.week <- merge(final.2022legal.freq.1.week, week9, by="LocationName", all.x = TRUE)

# Save final output for use in N-mixture model.
write.csv(final.2022legal.freq.1.week, "final.2022legal.freq.1.week.csv", row.names=FALSE)