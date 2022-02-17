setwd("C:/Users/smart/Desktop/Honors Thesis/analysis")
locations<- read.csv("CT lab data - combined location data.csv", stringsAsFactors = FALSE)
camera_trap_nights<- read.csv("CT lab data - malfunction info.csv", stringsAsFactors = FALSE)
main_data<- read.csv("Maromizaha Camera Trap Data - Sheet1.csv", stringsAsFactors = FALSE)


#assigning location labels
library(chron)
main_data$new_start_date<-chron(main_data$Photo.Date, main_data$Time.Start, 
                                format=c("y-m-d", "h:m:s"))
main_data$new_stop_date<-chron(main_data$Photo.Date, main_data$Time.End, 
                                format=c("y-m-d", "h:m:s"))
camera_trap_nights$new_start_date<-chron(camera_trap_nights$start.date,
                                         camera_trap_nights$start.time,
                                         format=c("y-m-d", "h:m:s"))
camera_trap_nights$new_stop_date<-chron(camera_trap_nights$stop.date,
                                         camera_trap_nights$stop.time,
                                         format=c("y-m-d", "h:m:s"))
main_data$location_label<- NA
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
  main_data[main_data$new_start_date >= start_date & 
              main_data$new_stop_date<= end_date &
              main_data$Forest.Location..==camera_number, "location_label"]<- location_label
}
no_location_match<- main_data[is.na(main_data$location_label),]
