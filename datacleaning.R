library(dplyr)
#library(htmltools)
library(rstudioapi)

setwd(dirname(getActiveDocumentContext()$path))    # Set working directory to source file location

obs <- read.csv("BisonObservations.csv")

# get list of ages for each site
for (i in 1:length(obs$Site.Name)){
  obs$oldest[i] <- obs %>% filter(Site.Name %in% obs$Site.Name[i]) %>% select(Upper.Age..IntCal20.) %>% max()
  obs$youngest[i] <- obs %>% filter(Site.Name %in% obs$Site.Name[i]) %>% select(Lower.Age..IntCal20.) %>% min()
}

# change source name from "Primary" to "primary source"
for (i in 1:length(obs$Site.Name)){
  if (obs$Data.Source[i] == "Primary"){
    obs$Data.Source[i] <- "primary source"
  }
}

# column with label
obs$label <- paste(sep = "<br/>",
      paste0("<center><b>", obs$Site.Name, "</b>"),
      paste0("Age range: ", obs$youngest, "-", obs$oldest, " cal yr BP"),
      #paste0("Coordinates: ", round(obs$Longitude, digits = 2), ", ", round(obs$Latitude, digits = 2)),
      paste0("Database: ", obs$Data.Source),
      paste0("</center>")
      )

write.csv(obs, "obsBisonShiny.csv", row.names = FALSE)
