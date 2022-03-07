setwd("C:/Users/smart/Desktop/Honors Thesis/analysis")
#install.packages("RColorBrewer") 
library(RColorBrewer)
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
blanks_removed <- na.omit(final_data$Type.of.animal)
camera_incident_rate<- final_data$blanks_removed/final_data$duration
barplot(camera_incident_rate, main="Incidents per Camera",
        xlab="Camera Name", xlim=c(1, 50), ylim=c(0,100))