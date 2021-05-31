## librarys einlesen ##
library(sf)
library(ggplot2)
library(ggmap)


## INPUT: Run-Name ##
#give the analysed run a name here, consisting out of the date (YYMMDD), an 
#underscore and the name of the run
runName <- "210531_firstRun"

#create output folders
dir.create("analysis", showWarnings = F)
dir.create(paste("analysis/", runName, sep = ""), showWarnings = F)


## Read in Trips -> INPUT: Trip-File-Name ##
#adjust the name of the trips file in the command below
t.trips <- read.csv2("output_testrun_210526/velbert-v1.0-1pct.output_trips.csv", #read in the MATSim output trips.csv file
                     colClasses = c("character", "numeric", rep("character", 4), 
                                    rep("numeric", 2), rep("character", 7),
                                    rep("numeric", 2), rep("character", 2), 
                                    rep("numeric", 2), rep("character", 2)),
                     stringsAsFactors = F, dec = ".")


#create separate data frames for trip starts and ends (necessary for sf creation)
tripStarts <- t.trips
tripEnds <- t.trips

#create sf elements in the data frames
tripStarts <- st_as_sf(tripStarts, coords = c("start_x", "start_y"), crs = 25832, remove = F)
tripEnds <- st_as_sf(tripEnds, coords = c("end_x", "end_y"), crs = 25832, remove = F)
#x = UTM easting, y = UTM northing
#Annahme: easting = longitude, northing = latitude -> zum Einfügen in Google Maps müssen die Koordinaten umgedreht werden
#In Config ist als CRS 25832 genannt und die Werte sehen auch nach einem System in m aus
#Ich setze das mal in 4326 und setze die Werte (verkehrte Reihenfolge) in Gmaps ein, um zu gucken, ob das grob stimmt
#tripStarts <- st_transform(tripStarts, crs = 4326)
#tripEnds <- st_transform(tripEnds, crs = 4326)
#Die ersten Start-Koords. liegen in Velbert -> scheint zumindest grob zu passen
#Der erste TripEnd-Wert liegt in Essen, ist aber in der Nähe von Velbert, also auch sinnvoll möglich



####### Get Shapefile of Velbert ##################
PLZ <- st_read("Shape_Auswertung/OSM_PLZ_072019.shp")                           #read in shapefile with all PLZ
#st_geometry(PLZ)
wanted_PLZ <- c("42549", "42551", "42553", "42555")
PLZ_Velbert <- PLZ[PLZ$plz %in% wanted_PLZ,]                                    #extract the rows with the PLZ of Velbert
Velbert <- st_union(PLZ_Velbert)                                                #unionize the shapes of the Velbert PLZ into one shape
plot(Velbert)
#print(Velbert)
#st_crs(Velbert)
rm(PLZ, wanted_PLZ, PLZ_Velbert)                                                #remove data that is not needed anymore; transform the CRS of the
Velbert <- st_transform(Velbert, crs = 25832)                                   #Velbert shape to match the MATSim output CRS
##Ende##



############### Filter out inhabitants of Velbert ########################

#I will try to filter out the inhabitants of Velbert by the coordinates of the starts
#and ends of the home activities of the agents
tripStarts$start_act_short <- stringr::str_sub(tripStarts$start_activity_type, end = "4") #take the first four letters of the start and end activities 
tripEnds$end_act_short <- stringr::str_sub(tripEnds$end_activity_type, end = "4")         #to identify home activities
#table(tripStarts$start_act_short)
#table(tripEnds$end_act_short)

homeStarts <- tripStarts[-which(tripStarts$start_act_short!="home"),]           #only keep starts and ends at home activities
homeEnds <- tripEnds[-which(tripEnds$end_act_short!="home"),]


## Filter homeStarts and homeEnds in Velbert ##

#homeStarts
homeStartsVelbert <-  homeStarts[st_intersects(homeStarts, Velbert, sparse = F),]  #Only keep starts/ends that are located in Velbert
#homeStartsOutside <- homeStarts[!st_intersects(homeStarts, Velbert, sparse = F),] #Beim Plotten hiermit sieht man fast kein Velbert mehr weil ganz DE

#homeEnds
homeEndsVelbert <- homeEnds[st_intersects(homeEnds, Velbert, sparse = F),]

#test if it was successful
ggplot() +
  geom_sf(data = Velbert) +
  geom_sf(data = homeStartsVelbert, col = "red") +
  #geom_sf(data = homeStartsOutside, col = "blue") +
  geom_sf(data = homeEndsVelbert, col = "green")


## reduce the starts and ends to the IDs of the agents who live in Velbert ##
homeStartsVelbert <- homeStartsVelbert$person
homeEndsVelbert <- homeEndsVelbert$person


## create a single list of the agents who live in velbert ##
Inhabitants <- c(homeStartsVelbert, homeEndsVelbert)
Inhabitants <- unique(Inhabitants)                                              #Only keep unique IDs in the list
#length(unique(t.trips$person))
#In the first run less than half the agents seem to actually live in Velbert


## Filter Trips for Velbert inhabitants ##
trips <- t.trips
trips <- trips[trips$person %in% Inhabitants,]                                  #Only keep rows of the trips data frame whose person col. is in Inhabitants
#table(unique(trips$person) %in% Inhabitants)   #Test if that worked



#################  Trip Analysis ####################


## look at modes ##
table(trips$longest_distance_mode)
#there are records where trips have no longest distance mode, those are results of trips with no
#traveled length (though some have euclidean lengths) -> purge for now 
trips <- trips[-which(trips$longest_distance_mode==""),]
#also, the mode ride is not contained in the goal modal splits from the SrV -> change to car
trips$modeWRide <- trips$longest_distance_mode                                  #Make a save of the mode column with the ride mode
trips$longest_distance_mode[which(trips$longest_distance_mode=="ride")] <- "car"


## look at activities ##
table(sub("_[^_]+$", "", trips$start_activity_type))
table(sub("_[^_]+$", "", trips$end_activity_type))
#activities look alright, no stage activities etc.

## calculate the overall modal splits ##
MS <- aggregate(person~longest_distance_mode, data = trips, function(x){length(x)})   #take the number of rows (trips) per mode
MS$TripSum <- sum(MS$person)                                                    #Calculate the sum of trips
MS$MSPercent <- (MS$person/MS$TripSum)*100                                      #Calculate the modal split


## calculate the overall modal splits with ride mode ##
MSWRide <- aggregate(person~modeWRide, data = trips, function(x){length(x)})    #see above
MSWRide$TripSum <- sum(MSWRide$person)
MSWRide$MSPercent <- (MSWRide$person/MSWRide$TripSum)*100


## prepare distance-based modal splits / create distance bins and sort trips ##
trips$distbins <- NA
trips$distbins[which(trips$traveled_distance <= 1000)] <- "<=1km"
trips$distbins[which(trips$traveled_distance > 1000 & trips$traveled_distance <= 3000)] <- ">1 bis <=3km"
trips$distbins[which(trips$traveled_distance > 3000 & trips$traveled_distance <= 5000)] <- ">3 bis <=5km"
trips$distbins[which(trips$traveled_distance > 5000 & trips$traveled_distance <= 10000)] <- ">5 bis <=10km"
trips$distbins[which(trips$traveled_distance > 10000)] <- "mehr als 10km"
#table(trips$distbins)
#rows/trips are sorted into dist bins based on the traveled distance -> change it perhaps


## calculate distance-based modal splits ##
MSdist <- aggregate(person~longest_distance_mode+distbins, data = trips, function(x){length(x)})  #count trips per dist bin and mode
MSdistSums <- aggregate(person~distbins, data = trips, function(x){length(x)})                    #Count trips per dist bin
MSdist$TripSum <- NA
MSdist$TripSum <- MSdistSums$person[match(MSdist$distbins, MSdistSums$distbins)]                  #add trips per dist bin to the trips per dist bin and mode
MSdist$MSPercent <- (MSdist$person/MSdist$TripSum)*100                          #calculate modal splits


## create and output plots of the modal splits ##

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
  ggtitle(paste("Overall Modal Split of the run ", runName, " with mode ride separately", sep = "")) +
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



#################### Modal Split comparison #########################


## prepare overall MS comparison ##

#read in goal data
MSGoal <- read.csv2("tables_MSgoal/MSGoal.csv", stringsAsFactors = F, check.names = F)

#put in IS-Data
MSGoal$MSIs <- NA
MSGoal$MSIs <- MS$MSPercent[match(MSGoal$mode, MS$longest_distance_mode)]
MSGoal$MSIs[which(is.na(MSGoal$MSIs)==T)] <- 0


## prepare dist-based MS comparison ##

#read in goal data; create IDs
MSDistGoal <- read.csv2("tables_MSgoal/MSDistGoal.csv", stringsAsFactors = F, check.names = F)
MSDistGoal$id <- paste(MSDistGoal$mode, MSDistGoal$distbin)

#put in IS data / create IDs for IS data
MSDistGoal$MSIs <- NA
MSdist$id <- paste(MSdist$longest_distance_mode, MSdist$distbins)
MSDistGoal$MSIs <- MSdist$MSPercent[match(MSDistGoal$id, MSdist$id)]
MSDistGoal$MSIs[which(is.na(MSDistGoal$MSIs)==T)] <- 0


## compare Is and Goal modal splits ##
MSGoal$Diff <- MSGoal$MSGoal-MSGoal$MSIs
MSDistGoal$Diff <- MSDistGoal$MSGoal-MSDistGoal$MSIs



################ write out and check comparison results #####################

## write out results ##
write.csv2(MSGoal, file = paste("analysis/", runName, "/MSGoal.csv", sep = ""))
write.csv2(MSDistGoal, file = paste("analysis/", runName, "/MSDistGoal.csv", sep = ""))

## check results ##
if(max(MSGoal$Diff)<2 & min(MSGoal$Diff)>-2){
  print("Model is correctly calibrated to the overall modal split.")
}else{
    warning("Model is not correctly calibrated to the overall modal split!")
}

if(max(MSDistGoal$Diff)<2 & min(MSDistGoal$Diff)>-2){
  print("Model is correctly calibrated to the distance-based modal split.")
}else{
  warning("Model is not correctly calibrated to the distance-based modal split!")
}



