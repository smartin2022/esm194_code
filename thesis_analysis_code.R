setwd("C:/Users/smart/Desktop/Honors Thesis/analysis")
locations<- read.csv("CT lab data - combined location data.csv", stringsAsFactors = FALSE)
camera_trap_nights<- read.csv("CT lab data - malfunction info.csv", stringsAsFactors = FALSE)
main_data<- read.csv("Maromizaha Camera Trap Data - Sheet1.csv", stringsAsFactors = FALSE)


#assigning location labels
main_data$location_label<- NA
for(i in 1:2){
  start_date<- camera_trap_nights[i, "start.date"]
  start_time<- camera_trap_nights[i, "start.time"]
  end_date<- camera_trap_nights[i, "stop.date"]
  end_time<- camera_trap_nights[i, "stop.time"]
  location_label<- camera_trap_nights[i, "new.cam.label"]
  camera_number<- camera_trap_nights[i, "camera.trap.....convert.to.new.grid.labels"]
  print(i)
  print(end_date)
  print(end_time)
  print(start_date)
  print(start_time)
  print(location_label)
  print(camera_number)
  print(dim(
  main_data[main_data$Photo.Date >= start_date & 
              main_data$Time.Start >= start_time &
              main_data$Photo.Date<= end_date &
              main_data$Time.End <= end_time &
              main_data$Forest.Location..==camera_number,]))#<- location_label
}