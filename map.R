library(readxl)
metadata <- read_excel("data/metadata_agrosavia.xls")
View(metadata)
library(statgenSTA)

td<- createTD(metadata, trial = 'studyName',
              loc = 'locationName', year = 'studyYear', 
              trLat = "latitude", trLong = "longitude",
              trDesign = 'res.rowcol'
) 

pdf(paste(folder, "01_", "_location_map_",
          Sys.Date(),".pdf", sep=""), width = 6, height = 6)

plot(td, plotType = "map",
     minLatRange = 1, minLongRange = 1)
dev.off()

install.packages("ggmap")
library(ggmap)

library(ggplot2)

library(raster)
library(tidyverse)
library(maptools)
library(plotly)


folder = "D:\\OneDrive - CGIAR\\Data Analysis\\check_design\\"




mapa <- borders("world", regions = c("Colombia"), 
                fill = "gray", colour = "black")

pdf(paste(folder, "02_", "_location_map_",
          Sys.Date(),".pdf", sep=""), width = 6, height = 6)

p <- ggplot(data = metadata %>% mutate(longitude = longitude*-1), aes(x = longitude, y = latitude, fill = factor(studyName))) + mapa + theme_bw() + xlab("Longitude") + ylab("Latitude") +
  theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey80"), panel.grid.minor = element_blank())+
  geom_point(shape = 21, show.legend = F, size = 4) +
  labs(title = NULL)
ggsave(paste0("images/map",".png"), plot = p, units =  "in", dpi = 300, width = 12, height = 10)

library(ggrepel)
library(ggrepel)

p <- p + geom_label(aes(label = locationName)) 


dev.off()





cols <- c(">500" = "orange", "0-200" = "lightgreen", "200-500" = "darkgreen")



p + scale_colour_manual(values = cols)