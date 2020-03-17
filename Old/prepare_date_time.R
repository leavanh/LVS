## Paket laden 

if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")

if (!require("lubridate")) install.packages("lubridate")
library("lubridate")

## Daten laden

data <- readRDS(file = "Daten/data.RDS")


## Schleife füer time
for (i in 1:nrow(data)) {
  # Zeit der Beobachtung 
  time_i <- data[[i, "time"]]
  # Weglassen von 1899-12-31 
  g <- strsplit(as.character(time_i)," +") [[1]][2]
  # Datum der Beobachtung
  date_i <- data[[i, "date"]]
  # Datum und Uhrzeit zusammenfuegen
  pb.txt <- paste(date_i, g, sep=" ")
  # Wird erfasst als as.posixct
  pb.date <- as.POSIXct(pb.txt, tz="UTC")
  # UTC - 4 Stunden
  new_date_time <- format(pb.date, tz="Etc/GMT+4",usetz=TRUE)
  # In der Console wird ausgegeben: Time, Date, New_Date_Time
  print(data[[i, "time"]]) 
  print(date_i)
  print(new_date_time)
  # In der Console wird nur bis Zeile 1451 ausgegeben
  if ( i == 1451) {
   break
  }
} 

  

  
  
  