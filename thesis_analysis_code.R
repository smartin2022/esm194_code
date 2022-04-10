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
  location_label<- final_data_1[i, "location_label"]
  photo_start<- final_data_1[i, "new_start_date"]
  photo_end<- final_data_1[i, "new_stop_date"]
  matching_camera<-camera_trap_nights[camera_trap_nights$new.cam.label==location_label & 
                                        camera_trap_nights$new_start_date<=photo_start & 
                                        camera_trap_nights$new_stop_date>=photo_end,]
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

#joining rodents and tenrecs into same category
#what function is this?

#pie chart showing % occurrence of each animal type for QGIS figure
incidents_animal_type<- aggregate(final_data_1$Species, by=list(location_label=final_data_1$location_label,
                                                                animal_type=final_data_1$Type.of.animal,
                                                                camera_duration=final_data_1$camera_duration),
                                                                
                                  FUN=length)
individual_type_incidents <- reshape(incidents_animal_type, 
             timevar = "animal_type",
             idvar = c("location_label", "camera_duration"),
             direction = "wide")

individual_type_incidents[1:i,]

write.csv(incidents_animal_type, file = "incidents_animal_type.csv")
write.csv(individual_type_incidents, file="individual_type_incidents.csv")

incidents_animal_type$observation_rate<- incidents_animal_type$x/incidents_animal_type$camera_duration
incidents_animal_type_GPS<-merge(incidents_animal_type, locations, by.x = "location_label", by.y = "cameraNum", all.x = TRUE)
no_missing_locations<-incidents_animal_type_GPS[is.na(incidents_animal_type_GPS$lat)==FALSE,]

#selecting good views
good_views<- no_missing_locations[no_missing_locations$Trees.visible.Y.N=="Y", ] #& no_missing_locations$location_label!="NNN'" & no_missing_locations$location_label!="DD",]
good_animals<- good_views[good_views$animal_type %in% c("Rodent", "Tenrec", "Carnivore", "Lemur"),]

#conversion to UTM
hrData2018SP	<- SpatialPointsDataFrame(hrData2018[,c('longitude', 'latitude')], hrData2018, proj4string=CRS("+proj=longlat +datum=WGS84"))
hrData2018UTM	<- spTransform(hrData2018SP, CRS("+proj=utm +zone=38 +south +datum=WGS84"))

#need to include random effects
model1<-gamm(x ~ animal_type + zone..E.C. + offset(log(camera_duration)), random = list(location_label = ~1), 
             correlation = corSpher(form = ~lat + long),
             family = poisson(link = log), data = good_animals)
