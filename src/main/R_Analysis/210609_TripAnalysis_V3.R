## load the necessary libraries ##
library(sf)
library(ggplot2)
library(ggmap)
#stringr is also required to run this script, but I did not use it often so I 
#do not load at the beginning of the script and instead explicitly address it (stringr::function())



####### Read in and format data #######

## INPUT: Run-Name ##
#give the analysed run a name here, consisting out of the date (YYMMDD), an 
#underscore and the name of the run
runName <- "210607_32ndrun"

#create output folders
dir.create("analysis", showWarnings = F)
dir.create(paste("analysis/", runName, sep = ""), showWarnings = F)


## Read in Trips -> INPUT: Trip-File-Name ##
#adjust the name of the trips file in the command below
#(the trips.csv file needs to be unzipped for this to work!)
t.trips <- read.csv2("output_210607_32ndrun/velbert-App2_210607_32ndRun.output_trips.csv", #read in the MATSim output trips.csv file
                     colClasses = c("character", "numeric", rep("character", 4),#further specification of column classes, decimal separator etc. 
                                    rep("numeric", 2), rep("character", 7),
                                    rep("numeric", 2), rep("character", 2), 
                                    rep("numeric", 2), rep("character", 2)),
                     stringsAsFactors = F, dec = ".")

#create separate data frames for trip starts and ends 
tripStarts <- t.trips
tripEnds <- t.trips
#this is necessary as (as far as I know) each row of a data frame can only have 
#ONE simple feature, while the trips data frame has two locations (start & end) per row

#create sf elements in the data frames
tripStarts <- st_as_sf(tripStarts, coords = c("start_x", "start_y"), crs = 25832, remove = F)
tripEnds <- st_as_sf(tripEnds, coords = c("end_x", "end_y"), crs = 25832, remove = F)
#x = UTM easting, y = UTM northing



####### Create a Shapefile of Velbert ##################
PLZ <- st_read("Shape_Auswertung/OSM_PLZ_072019.shp")                           #read in the shapefile with all PLZ zones in germany
wanted_PLZ <- c("42549", "42551", "42553", "42555")                             #create a vector with the PLZ codes of Velbert
PLZ_Velbert <- PLZ[PLZ$plz %in% wanted_PLZ,]                                    #extract the rows with the PLZ codes of Velbert from all PLZ
Velbert <- st_union(PLZ_Velbert)                                                #unionize the shapes of the Velbert PLZ codes into one shape
plot(Velbert)                                                                   #plot the shape
rm(PLZ, wanted_PLZ, PLZ_Velbert)                                                #remove data that is not needed anymore; transform the CRS of the
Velbert <- st_transform(Velbert, crs = 25832)                                   #Velbert shape to match the MATSim output CRS
##end##



####### Filter out inhabitants of Velbert #######

#I will filter out the inhabitants of Velbert using the coordinates of the starts
#and ends of the home activities of the agents
tripStarts$start_act_short <- stringr::str_sub(tripStarts$start_activity_type, end = "4") #take the first four letters of the start and end activities 
tripEnds$end_act_short <- stringr::str_sub(tripEnds$end_activity_type, end = "4")         #to identify home activities

homeStarts <- tripStarts[-which(tripStarts$start_act_short!="home"),]           #only keep trip starts/ends that start/end at home activities
homeEnds <- tripEnds[-which(tripEnds$end_act_short!="home"),]


## Filter out home activity starts/ends that lie in Velbert ##

#homeStarts
homeStartsVelbert <-  homeStarts[st_intersects(homeStarts, Velbert, sparse = F),]  #Only keep starts/ends that are located in Velbert
                                                                                #(which is determined by the point intersecting with the Velbert shape)
#homeEnds
homeEndsVelbert <- homeEnds[st_intersects(homeEnds, Velbert, sparse = F),]

#test if it was successful by plotting the velbert shape and all points that have been kept
ggplot() +
  geom_sf(data = Velbert) +
  geom_sf(data = homeStartsVelbert, col = "red") +
  geom_sf(data = homeEndsVelbert, col = "green")


## reduce the trips with home activity starts/ends to the IDs of the agents who live in Velbert ##
homeStartsVelbert <- homeStartsVelbert$person
homeEndsVelbert <- homeEndsVelbert$person


## create a single list of the IDs of the agents who live in velbert ##
Inhabitants <- c(homeStartsVelbert, homeEndsVelbert)
Inhabitants <- unique(Inhabitants)                                              #Only keep unique IDs in the list


## Filter the Trips by Velbert inhabitants ##
trips <- t.trips
trips <- trips[trips$person %in% Inhabitants,]                                  #Only keep rows of the trips data frame whose person/ID column 
                                                                                #is contained in the Inhabitants vector


#######  Trip Analysis #######

## look at modes ##
table(trips$longest_distance_mode)
#there are records where trips have no longest distance mode, those are trips with no recorded
#traveled length (although some have euclidean lengths) -> I will remove them from the modal split calculation data 
trips <- trips[-which(trips$longest_distance_mode==""),]

#also, the mode ride is not contained in the target/goal modal splits from the SrV -> change it car
trips$modeWRide <- trips$longest_distance_mode                                  #Make a save of the mode column with the ride mode
trips$longest_distance_mode[which(trips$longest_distance_mode=="ride")] <- "car"#Change the ride mode in the "old" mode column to car


## look at the activities ##
table(sub("_[^_]+$", "", trips$start_activity_type))                            #this command removes the time suffix from the activity types so the 
table(sub("_[^_]+$", "", trips$end_activity_type))                              #tables are clustered better
                                                                                #the activities look alright, no stage activities etc. are listed

## calculate the overall modal splits ##
MS <- aggregate(person~longest_distance_mode, data = trips, function(x){length(x)})   #take the number of rows (trips) per mode
MS$TripSum <- sum(MS$person)                                                    #Calculate the sum of all trips
MS$MSPercent <- (MS$person/MS$TripSum)*100                                      #Calculate the overall modal split


## calculate the overall modal splits with the car and ride modes separated ##
MSWRide <- aggregate(person~modeWRide, data = trips, function(x){length(x)})    #see overall modal splits
MSWRide$TripSum <- sum(MSWRide$person)
MSWRide$MSPercent <- (MSWRide$person/MSWRide$TripSum)*100


## prepare distance-based modal splits / create distance bins and sort the trips into them ##
trips$distbins <- NA
trips$distbins[which(trips$traveled_distance <= 1000)] <- "<=1km"
trips$distbins[which(trips$traveled_distance > 1000 & trips$traveled_distance <= 3000)] <- ">1 bis <=3km"
trips$distbins[which(trips$traveled_distance > 3000 & trips$traveled_distance <= 5000)] <- ">3 bis <=5km"
trips$distbins[which(trips$traveled_distance > 5000 & trips$traveled_distance <= 10000)] <- ">5 bis <=10km"
trips$distbins[which(trips$traveled_distance > 10000)] <- "mehr als 10km"
#the rows/trips are sorted into the distance bins based on the traveled distance


## calculate distance-based modal splits ##
MSdist <- aggregate(person~longest_distance_mode+distbins, data = trips, function(x){length(x)})  #count trips per dist bin and mode
MSdistSums <- aggregate(person~distbins, data = trips, function(x){length(x)})                    #Count trips per dist bin
MSdist$TripSum <- NA
MSdist$TripSum <- MSdistSums$person[match(MSdist$distbins, MSdistSums$distbins)]#add the trips per dist bin to the df with trips per dist bin and mode
MSdist$MSPercent <- (MSdist$person/MSdist$TripSum)*100                          #calculate the modal splits per dist bin


## create and print/save plots of the modal splits ##

#overall MS (bar plot)
ggplot(MS, aes(x = longest_distance_mode, y = MSPercent, label = round(MSPercent,1), fill = longest_distance_mode)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("dodgerblue4", "chocolate", "chartreuse4", "deepskyblue")) + 
  theme_light() +
  theme(legend.position = "none") +
  ylab("Modal Split Percentage") +
  xlab("Main trip mode (determined by the longest mode distance)") +
  ggtitle(paste("Overall Modal Split of the run ", runName, sep = "")) +
  ggsave(paste("analysis/", runName, "/MSOverall.png", sep = ""), type = "cairo")

#overall MS with ride mode (bar plot)
ggplot(MSWRide, aes(x = modeWRide, y = MSPercent, label = round(MSPercent,1), fill = modeWRide)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("dodgerblue4", "chocolate", "chartreuse4", "coral", "deepskyblue")) + 
  theme_light() +
  theme(legend.position = "none") +
  ylab("Modal Split Percentage") +
  xlab("Main trip mode (determined by the longest mode distance)") +
  ggtitle(paste("Overall Modal Split of the run ", runName, " \nwith modes ride and car separated", sep = "")) +
  ggsave(paste("analysis/", runName, "/MSOverallWRide.png", sep = ""), type = "cairo")

#dist-based MS (stacked bar plot)
ggplot(MSdist, aes(x = distbins, y = MSPercent, label = round(MSPercent,1), fill = longest_distance_mode))+
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("dodgerblue4", "chocolate", "chartreuse4", "deepskyblue")) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(fill = "Main trip mode \n(determined by the \nlongest mode distance)") +
  ylab("Modal Split Percentage") +
  xlab("Distance bin") +
  ggtitle(paste("Distance-Based Modal Split of the run ", runName, sep = "")) +
  ggsave(paste("analysis/", runName, "/MSDist.png", sep = ""), type = "cairo")



####### Comparison of Model/Is and goal modal splits #######

## prepare the comparison of the overall MS ##

#read in the goal data
MSGoal <- read.csv2("tables_MSgoal/MSGoal.csv", stringsAsFactors = F, check.names = F)

#add the Is-MS-Data to the data frame with the goal data
MSGoal$MSIs <- NA
MSGoal$MSIs <- MS$MSPercent[match(MSGoal$mode, MS$longest_distance_mode)]
MSGoal$MSIs[which(is.na(MSGoal$MSIs)==T)] <- 0


## prepare the comparison of the dist-based MS ##

#read in goal data; create IDs based on mode and distance bin
MSDistGoal <- read.csv2("tables_MSgoal/MSDistGoal.csv", stringsAsFactors = F, check.names = F)
MSDistGoal$id <- paste(MSDistGoal$mode, MSDistGoal$distbin)

#add the Is-MS-Data / create IDs for the Is-MS-Data
MSDistGoal$MSIs <- NA
MSdist$id <- paste(MSdist$longest_distance_mode, MSdist$distbins)
MSDistGoal$MSIs <- MSdist$MSPercent[match(MSDistGoal$id, MSdist$id)]
MSDistGoal$MSIs[which(is.na(MSDistGoal$MSIs)==T)] <- 0


## compare Is and Goal modal splits ##
MSGoal$Diff <- MSGoal$MSGoal-MSGoal$MSIs
MSDistGoal$Diff <- MSDistGoal$MSGoal-MSDistGoal$MSIs
#the MS are compared by subtracting the model/Is modal Splits from the goal modal splits
#to reach the exercise requirements the difference of each mode should lie in the interval 2 > diff > -2



####### write out and check comparison results #######

## write out the comparison results ##
write.csv2(MSGoal, file = paste("analysis/", runName, "/MSGoal.csv", sep = ""))
write.csv2(MSDistGoal, file = paste("analysis/", runName, "/MSDistGoal.csv", sep = ""))


## check the results ##
if(max(MSGoal$Diff)<2 & min(MSGoal$Diff)>-2){
  print("The Model is correctly calibrated to the overall modal split.")
}else{
    warning("The Model is not correctly calibrated to the overall modal split!")
}

if(max(MSDistGoal$Diff)<2 & min(MSDistGoal$Diff)>-2){
  print("The Model is correctly calibrated to the distance-based modal split.")
}else{
  warning("The Model is not correctly calibrated to the distance-based modal split!")
}
#the above conditions check if the overall/distance-based modal splits lie in the 
#targeted interval and print a warning if they don't


