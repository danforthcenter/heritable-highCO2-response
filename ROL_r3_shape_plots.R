#set working directory and load packages
setwd("~/segmentation_output/single_value/")
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(plotly)
library(ggplot2)
library(stringr)

closeAllConnections()
rm(list=ls())

#make a list of the file paths for the csv files you want to upload
files <- list.files(path = "./", pattern="*single-value-traits.csv", full.names = TRUE)
#make a temporary DF that has all the data and read in all csv files in your list
tempDF <- ldply(files, read.csv)
#filter DF columns for the ones you want to analyze
ROL <- select(tempDF, camera, plant_index, timestamp, image, plant_index, area, convex_hull_area, solidity, perimeter,
              width, height, longest_path, convex_hull_vertices, ellipse_major_axis, ellipse_minor_axis,
              ellipse_angle, ellipse_eccentricity, hue_circular_mean, hue_circular_std, hue_median, image, plantbarcode)

rm(tempDF)

# add genotype column based on the image name
c <- nchar('./segmentations/')
l <- nchar(paste(ROL$camera, substring(ROL$plantbarcode,5), sep='_'))
ll <- nchar(lapply(str_split(ROL$image, pattern='/'),'[[',3))
ROL$genotype <- substring(ROL$image, c+l+2, c+ll-2)
# add bio rep number column
ROL$BR <- paste("BR",substring(ROL$image, c+ll, c+ll), sep = "")

#make a column for the imaging round
ROL$round <- "R3"

#reformat the timestamp column into a date column and a time column
TS <- c("year", "month", "day", "hour", "minute") #make a vector for the column names of each part of the timestamp
tempDF <- separate(ROL, col = timestamp, into = TS, sep = "-") #separate each component of timestamp
tempDF <- unite(tempDF, year, month, day, col = date, sep = "-", remove = TRUE) #combine the dates
tempDF <- unite(tempDF, hour, minute, col = time, sep = "-", remove = TRUE) #combine the times
ROL <- tempDF

#calculate days after first image
startdate <- as.POSIXct(min(ROL$date))
ROL$dap=NA
ROL$dap = as.integer(difftime(ROL$date, startdate, units = "days"))

write.csv(ROL, "./round3_raw_data.csv", row.names = FALSE)

# Filter out images taken on day 0
ROL = ROL[ROL$dap>0,]

# filter data to eliminate too high and too low values (some of the plants touch each other 
# and combine into one object, which inflates the area values)
# plants are filtered on each day based on mean pixel area and plants outside of 2 SD of the mean 
# are removed
ROL<-ROL[ROL$area>0,]
ROL<-ROL[ROL$area<80000,]
ROL<-ROL[ROL$area>200,]
#filterROL <- transform(filterROL, dap = as.numeric(dap))

DAP1 <- filter(ROL, ROL$dap == 1)
DAP1filter <- filter(DAP1, DAP1$area <= mean(area)+ 2*sd(area) & DAP1$area >= mean(area)-2*sd(area))
DAP2 <- filter(ROL, ROL$dap == 2)
DAP2filter <- filter(DAP2, DAP2$area <= mean(area)+ 2*sd(area) & DAP2$area >= mean(area)-2*sd(area))
DAP3 <- filter(ROL, ROL$dap == 3)
DAP3filter <- filter(DAP3, DAP3$area <= mean(area)+ 2*sd(area) & DAP3$area >= mean(area)-2*sd(area))
DAP4 <- filter(ROL, ROL$dap == 4)
DAP4filter <- filter(DAP4, DAP4$area <= mean(area)+ 2*sd(area) & DAP4$area >= mean(area)-2*sd(area))
DAP5 <- filter(ROL, ROL$dap == 5)
DAP5filter <- filter(DAP5, DAP5$area <= mean(area)+ 2*sd(area) & DAP5$area >= mean(area)-2*sd(area))
DAP6 <- filter(ROL, ROL$dap == 6)
DAP6filter <- filter(DAP6, DAP6$area <= mean(area)+ 2*sd(area) & DAP6$area >= mean(area)-2*sd(area))
DAP7 <- filter(ROL, ROL$dap == 7)
DAP7filter <- filter(DAP7, DAP7$area <= mean(area)+ 2*sd(area) & DAP7$area >= mean(area)-2*sd(area))
DAP8 <- filter(ROL, ROL$dap == 8)
DAP8filter <- filter(DAP8, DAP8$area <= mean(area)+ 2*sd(area) & DAP8$area >= mean(area)-2*sd(area))
DAP9 <- filter(ROL, ROL$dap == 9)
DAP9filter <- filter(DAP9, DAP9$area <= mean(area)+ 2*sd(area) & DAP9$area >= mean(area)-2*sd(area))
DAP10 <- filter(ROL, ROL$dap == 10)
DAP10filter <- filter(DAP10, DAP10$area <= mean(area)+ 2*sd(area) & DAP10$area >= mean(area)-2*sd(area))
DAP11 <- filter(ROL, ROL$dap == 11)
DAP11filter <- filter(DAP11, DAP11$area <= mean(area)+ 2*sd(area) & DAP11$area >= mean(area)-2*sd(area))
DAP12 <- filter(ROL, ROL$dap == 12)
DAP12filter <- filter(DAP12, DAP12$area <= mean(area)+ 2*sd(area) & DAP12$area >= mean(area)-2*sd(area))
DAP13 <- filter(ROL, ROL$dap == 13)
DAP13filter <- filter(DAP13, DAP13$area <= mean(area)+ 2*sd(area) & DAP13$area >= mean(area)-2*sd(area))
DAP14 <- filter(ROL, ROL$dap == 14)
DAP14filter <- filter(DAP14, DAP14$area <= mean(area)+ 2*sd(area) & DAP14$area >= mean(area)-2*sd(area))
DAP15 <- filter(ROL, ROL$dap == 15)
DAP15filter <- filter(DAP15, DAP15$area <= mean(area)+ 2*sd(area) & DAP15$area >= mean(area)-2*sd(area))
DAP16 <- filter(ROL, ROL$dap == 16)
DAP16filter <- filter(DAP16, DAP16$area <= mean(area)+ 2*sd(area) & DAP16$area >= mean(area)-2*sd(area))
DAP17 <- filter(ROL, ROL$dap == 17)
DAP17filter <- filter(DAP17, DAP17$area <= mean(area)+ 2*sd(area) & DAP17$area >= mean(area)-2*sd(area))

# combine the filtered data from each day back into one data frame
filterROL <- rbind(DAP1filter, DAP2filter, DAP3filter, 
                   DAP4filter, DAP5filter, DAP6filter, 
                   DAP7filter, DAP8filter, DAP9filter, 
                   DAP10filter, DAP11filter, DAP12filter, 
                   DAP13filter, DAP14filter, DAP15filter, DAP16filter, DAP17filter)

rm(DAP1filter, DAP2filter, DAP3filter, 
   DAP4filter, DAP5filter, DAP6filter, 
   DAP7filter, DAP8filter, DAP9filter, 
   DAP10filter, DAP11filter, DAP12filter, 
   DAP13filter, DAP14filter, DAP15filter, DAP16filter, DAP17filter)

rm(DAP1, DAP2, DAP3, 
   DAP4, DAP5, DAP6, 
   DAP7, DAP8, DAP9, 
   DAP10, DAP11, DAP12, 
   DAP13, DAP14, DAP15, DAP16, DAP17)

#write out a CSV file of all the data if you want to save it
write.csv(filterROL, "./filtered_shape_data_round3.csv", row.names = FALSE)


R3 <- read.csv("./filtered_shape_data_round3.csv")
R2 <- read.csv("./filtered_shape_data_round2.csv")
R1 <- read.csv("./filtered-shape-data-round1.csv")

R1$round <- "R1"
R2$round <- "R2"
R3$Co2 <- "lowCo2"

R1select <- select(R1, plant_index, date, time, image, area, Co2, genotype, BR, DAP, round)
R2$DAP <- R2$dap
R2 <- select(R2, -dap)
#rename(R2, DAP = dap)
R2select <- select(R2, plant_index, date, time, image, area, Co2, genotype, BR, DAP, round)
R2select$genotype[R2select$genotype == "ubp"] <- "ubp1"
#R3 <- rename(R3, DAP = dap)
R3$DAP <- R3$dap
R3 <- select(R3, -dap)
R3select <- select(R3, plant_index, date, time, image, area, Co2, genotype, BR, DAP, round)

DF <- rbind(R1select, R2select, R3select)
DF$genotype[DF$genotype == "WtCol"] <- "wtCol"

write.csv(DF, "./all_rounds_combinded_area_curated_plantarea.csv", row.names = FALSE)


