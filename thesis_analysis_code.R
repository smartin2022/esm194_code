setwd("C:/Users/smart/Desktop/Honors Thesis/analysis")
#install.packages("RColorBrewer") 
library(RColorBrewer)
library(mgcv)
locations<- read.csv("CT lab data - combined location data.csv", stringsAsFactors = FALSE)
camera_trap_nights<- read.csv("CT lab data - malfunction info.csv", stringsAsFactors = FALSE)
main_data<- read.csv("Maromizaha Camera Trap Data - Sheet1.csv", stringsAsFactors = FALSE)


#assigning location labels
time_fixed<-main_data[main_data$Time.Not.able.to.be.fixed!="y"&
                        is.na(main_data$adjusted.date)==FALSE,]
library(chron)
time_fixed$new_start_date<-chron(time_fixed$adjusted.date, time_fixed$updated.time.start, 
                                format=c("y-m-d", "h:m:s"))
time_fixed$new_stop_date<-chron(time_fixed$adjusted.date, time_fixed$updated.Time.End, 
                                format=c("y-m-d", "h:m:s"))
camera_trap_nights$new_start_date<-chron(camera_trap_nights$start.date,
                                         camera_trap_nights$start.time,
                                         format=c("y-m-d", "h:m:s"))
camera_trap_nights$new_stop_date<-chron(camera_trap_nights$stop.date,
                                         camera_trap_nights$stop.time,
                                         format=c("y-m-d", "h:m:s"))
camera_trap_nights$duration<-camera_trap_nights$new_stop_date-camera_trap_nights$new_start_date
time_fixed$location_label<- NA
for(i in 1:nrow(camera_trap_nights)){
  start_date<- camera_trap_nights[i, "new_start_date"]
  end_date<- camera_trap_nights[i, "new_stop_date"]
  location_label<- camera_trap_nights[i, "new.cam.label"]
  camera_number<- camera_trap_nights[i, "camera.trap.....convert.to.new.grid.labels"]
  print(i)
  print(start_date)
  print(end_date)
  print(location_label)
  print(camera_number)
  nlines<-nrow(time_fixed[time_fixed$new_start_date >= start_date & 
                            time_fixed$new_stop_date<= end_date &
                            time_fixed$Forest.Location..==camera_number,])
  if(nlines>0){
  time_fixed[time_fixed$new_start_date >= start_date & 
              time_fixed$new_stop_date<= end_date &
              time_fixed$Forest.Location..==camera_number, "location_label"]<- location_label
  }
}
# there are 7 images that we cannot add to a working interval
no_location_match<- time_fixed[is.na(time_fixed$location_label),]

final_data<-merge(time_fixed, locations, by.x = "location_label", by.y = "cameraNum", all.x = TRUE)

#species observation bar graph
par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0))
animal_type_count <- table(final_data$Type.of.animal)
barplot(animal_type_count, main="Incidents of different animal types",
        ylab= "Incidents", xlab="Animal Type", ylim=c(0, 1000), col=("dark blue"))
#barplot(animal_type_count[c(3, 5, 8, 9)], main="Incidents of different animal types", ylab= "Incidents", xlab="Animal Type", ylim=c(0, 1000))
 
#incidents per camera
blanks_removed<- final_data[is.na(final_data$location_label)==FALSE,]
final_data_1<-blanks_removed
final_data_1$camera_start<-NA
final_data_1$camera_stop<-NA
final_data_1$camera_duration<-NA
for(i in 1:nrow(final_data_1)) {
#print(i)
  location_label<- final_data_1[i, "location_label"]
  photo_start<- final_data_1[i, "new_start_date"]
  photo_end<- final_data_1[i, "new_stop_date"]
  matching_camera<-camera_trap_nights[camera_trap_nights$new.cam.label==location_label & 
                                        camera_trap_nights$new_start_date<=photo_start & 
                                        camera_trap_nights$new_stop_date>=photo_end,]
  #print(dim(matching_camera))
  final_data_1[i, "camera_start"]<-matching_camera$new_start_date
  final_data_1[i, "camera_stop"]<-matching_camera$new_stop_date
  final_data_1[i, "camera_duration"]<-matching_camera$duration
}

camera_incident_rate<- aggregate(final_data_1$Species, by=list(location_label=final_data_1$location_label, species=final_data_1$Species, 
                                                               Type_of_animal=final_data_1$Type.of.animal, 
                                                               camera_duration=final_data_1$camera_duration), FUN=length)


camera_incident_rate$rate<- camera_incident_rate$x/camera_incident_rate$camera_duration
#rate units = number of instances per day
barplot(camera_incident_rate, main="Incidents per Camera",
        xlab="Camera", ylab="Incidents", xlim=c(1, 50), ylim=c(0,100))


#pie chart showing % occurrence of each animal type for QGIS figure
incidents_animal_type<- aggregate(final_data_1$Species, by=list(location_label=final_data_1$location_label,
                                                                animal_type=final_data_1$Type.of.animal,
                                                                camera_duration=final_data_1$camera_duration),
                                                                
                                  FUN=length)
#adding zeros to the dataset
camera_incident_rate$species_typeofanimal<- factor(as.factor(camera_incident_rate$species):as.factor(camera_incident_rate$Type_of_animal))
camera_incident_rate_simple <- camera_incident_rate[, c(1, 7, 4:6)]
species<-unique(camera_incident_rate_simple$species_typeofanimal)
camera_locations<-unique(camera_incident_rate_simple$location_label)
for(i in camera_locations){
  print(i)  
  usedspecies <- camera_incident_rate_simple[camera_incident_rate_simple$location_label == i, 'species_typeofanimal']
    missingspecies	<- as.character(species[!species %in% usedspecies])
    print(missingspecies)
    zeros			<- rep(0, length(missingspecies))
    location_label		<- rep(i, length(missingspecies))
    duration <- rep(camera_incident_rate_simple[camera_incident_rate_simple$location_label == i, 'camera_duration'][1], length(missingspecies))
    missingLines	<- cbind(location_label, missingspecies, duration, zeros, zeros)
    colnames(missingLines)	<- c('location_label', 'species_typeofanimal', 'camera_duration', 'x', 'rate')
    camera_incident_rate_simple			<- rbind(camera_incident_rate_simple, missingLines)
  
}
library(stringr)
species_typeofanimal_str<- data.frame(str_split_fixed(as.character(camera_incident_rate_simple$species_typeofanimal), ':', n = 2))
colnames(species_typeofanimal_str)		<- c('species', 'type_of_animal')
camera_incident_rate_simple$species<-species_typeofanimal_str$species
camera_incident_rate_simple$typeofanimal<-species_typeofanimal_str$type_of_animal

individual_type_incidents <- reshape(incidents_animal_type, 
             timevar = "animal_type",
             idvar = c("location_label", "camera_duration"),
             direction = "wide")

write.csv(incidents_animal_type, file = "incidents_animal_type.csv")
write.csv(individual_type_incidents, file="individual_type_incidents.csv")

incidents_animal_type$observation_rate<- incidents_animal_type$x/incidents_animal_type$camera_duration
incidents_animal_type_GPS<-merge(incidents_animal_type, locations, by.x = "location_label", by.y = "cameraNum", all.x = TRUE)
no_missing_locations<-incidents_animal_type_GPS[is.na(incidents_animal_type_GPS$lat)==FALSE,]

#selecting good views
good_views<- no_missing_locations[no_missing_locations$Trees.visible.Y.N=="Y" ,]
good_animals<- good_views[good_views$animal_type %in% c("Rodent", "Tenrec", "Carnivore", "Lemur"),]

#conversion to UTM
library("spatstat")
library("sp")
library("maptools")
library("raster")
library("rgdal")
lat_long_conversion	<- SpatialPointsDataFrame(good_animals[,c('long', 'lat')], good_animals, proj4string=CRS("+proj=longlat +datum=WGS84"))
UTM	<- spTransform(lat_long_conversion, CRS("+proj=utm +zone=38 +south +datum=WGS84"))
good_animals$latUTM<-coordinates(UTM)[ ,2]
good_animals$longUTM<-coordinates(UTM)[,1]

#reading in data set with combined intervals and zeros
good_animals0<- read.csv("good_animals0.csv", stringsAsFactors = FALSE)

good_animals_lemurs<-good_animals0[good_animals0$animal_type=="Lemur",]
lat_long_lemurs<- SpatialPointsDataFrame(good_animals_lemurs[,c('long', 'lat')], good_animals_lemurs, proj4string=CRS("+proj=longlat +datum=WGS84"))
UTM_lemurs	<- spTransform(lat_long_lemurs, CRS("+proj=utm +zone=38 +south +datum=WGS84"))

library(gstat)
gs <- gstat(formula=x~1, locations=UTM_lemurs)
v <- variogram(gs, width=20)
#model for lemur activity
model_lemurs<-gam(x ~ zone..E.C. + offset(log(camera_duration)) + distance.to.edge, 
             correlation = corSpher(form = ~latUTM + longUTM),
             family = poisson(link = log), data = good_animals_lemurs)

#model for carnivore activity
good_animals_carnivores<-good_animals0[good_animals0$animal_type=="Carnivore",]

model_carnivores<-gam(x ~ zone..E.C. + offset(log(camera_duration)) + distance.to.edge, 
            correlation = corSpher(form = ~latUTM + longUTM),
            family = poisson(link = log), data = good_animals_carnivores)

#model for rodent activity
good_animals_rodents<-good_animals0[good_animals0$animal_type=="Rodent",]

model_rodents<-gam(x ~ zone..E.C. + offset(log(camera_duration)) + distance.to.edge, 
                      correlation = corSpher(form = ~latUTM + longUTM),
                      family = poisson(link = log), data = good_animals_rodents)

#model for tenrec activity
good_animals_tenrecs<-good_animals0[good_animals0$animal_type=="Tenrec",]

model_tenrecs<-gam(x ~ zone..E.C. + offset(log(camera_duration)) + distance.to.edge, 
                      correlation = corSpher(form = ~latUTM + longUTM),
                      family = poisson(link = log), data = good_animals_tenrecs)

#biodiversity indicies
location_names<- unique(good_animals$location_label)
diversity_indicies <- data.frame()

shannonWeinerD	<- function(data){
  data$pi<- data$x/sum(data$x)
  data$pilogpi <- data$pi * log(data$pi)
  
  return(-1 * sum(data$pilogpi))
}

#converting camera_incident_rate_simple variables to correct class
camera_incident_rate_simple$x <- as.numeric(camera_incident_rate_simple$x)
camera_incident_rate_simple$rate <- as.numeric(camera_incident_rate_simple$rate)
camera_incident_rate_simple$camera_duration <- as.numeric(camera_incident_rate_simple$camera_duration)

#converting lat and long for camera_incident_rate_simple to UTm
camera_incident_rate_simple1<-merge(camera_incident_rate_simple, locations, by.x = "location_label", by.y = "cameraNum", all.x = TRUE)
camera_incident_rate_simple2<-camera_incident_rate_simple1[is.na(camera_incident_rate_simple1$lat)==FALSE,]
camera_incident_rate_simple3<- camera_incident_rate_simple2[camera_incident_rate_simple2$Trees.visible.Y.N=="Y" ,]
camera_incident_rate_simple_good_animals<- camera_incident_rate_simple3[camera_incident_rate_simple3$typeofanimal %in% c("Rodent", "Tenrec", "Carnivore", "Lemur"),]
lat_long_conversion_species	<- SpatialPointsDataFrame(camera_incident_rate_simple_good_animals[,c('long', 'lat')], camera_incident_rate_simple_good_animals, proj4string=CRS("+proj=longlat +datum=WGS84"))
UTM_species	<- spTransform(lat_long_conversion_species, CRS("+proj=utm +zone=38 +south +datum=WGS84"))
camera_incident_rate_simple_good_animals$latUTM<-coordinates(UTM_species)[ ,2]
camera_incident_rate_simple_good_animals$longUTM<-coordinates(UTM_species)[,1]


diversity_indicies<-data.frame()
for (i in location_names) {
  subset<- camera_incident_rate_simple_good_animals[camera_incident_rate_simple_good_animals$location_label == i, ]
  print(subset)
  alpha_diversity <- nrow(subset[subset$rate >= 0, ])
  SD <- shannonWeinerD(subset)
  new_line <- subset[1, c("location_label", "camera_duration", "lat", "long", "trail.type..A.H.N.", "zone..E.C.", "distance.to.edge", "latUTM", "longUTM")]
  new_line<- cbind.data.frame(new_line, alpha_diversity, SD, log(alpha_diversity))
  diversity_indicies <- rbind.data.frame(diversity_indicies, new_line)
}


model_SWD<-gam(SD ~ zone..E.C. + offset(log(camera_duration)) + distance.to.edge, 
                   correlation = corSpher(form = ~latUTM + longUTM),
                   family = gaussian(), data = diversity_indicies)
model_AD<-gam(alpha_diversity ~ zone..E.C. + offset(log(camera_duration)) + distance.to.edge, 
               correlation = corSpher(form = ~latUTM + longUTM),
               family = gaussian(), data = diversity_indicies)


heat_map<- aggregate(camera_incident_rate_simple_good_animals$typeofanimal, by=list(location_label=camera_incident_rate_simple_good_animals$location_label,
                                                                typeofanimal=camera_incident_rate_simple_good_animals$typeofanimal,
                                                                camera_duration=camera_incident_rate_simple_good_animals$camera_duration,
                                                                rate=camera_incident_rate_simple_good_animals$rate), 
                     FUN=length)

#heat map for QGIS
heat_map<- aggregate(camera_incident_rate_simple$typeofanimal, by=list(location_label=camera_incident_rate_simple$location_label,
                                                                                    typeofanimal=camera_incident_rate_simple$typeofanimal,
                                                                                    camera_duration=camera_incident_rate_simple$camera_duration,
                                                                                    rate=camera_incident_rate_simple$rate), 
                     FUN=length)

#test to see how many blanks are in the typeofanimal class
heat_map_aggregate<- aggregate(rate ~ typeofanimal, data=camera_incident_rate_simple, FUN=length)
