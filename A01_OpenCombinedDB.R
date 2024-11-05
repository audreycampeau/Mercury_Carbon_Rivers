

#Import libraries
library(readxl)
library(tidyverse)


# Open combined dataset from Dominic Ponton, obtained in 2023
QC <- read_excel("Data/Data 2023/Combined Database - 2021111.xlsx", sheet = 3)
QC[QC == "NA"] <- NA
QC <- QC[-which(is.na(QC$Region)), ]

# Remove columns with sample repetition
qc <- QC %>% dplyr::select(-contains(c("Rep1", "Rep2", "Rep3", "SE"), ignore.case = TRUE))

# Format Factors and Numerical columns in database
qc[, c(1:10, 16)] <- lapply(qc[, c(1:10, 16)], as.factor) # Format to numeric
qc[, c( 11:15, # GPS
       17:196)] <- lapply(qc[, c(11:15, 17:196)], as.numeric) # Format to numeric


qc_subset <- qc[qc$Region %in% c("LR", "NS", "HM"), ] # Keep only LR, NS and Hm region 

qc_subset$Region <- recode_factor(qc_subset$Region, "NS" = "EI", "LR" = "RR", "HM" = "HM") # Rename the Regions

qc_subset$Region <- factor(qc_subset$Region, levels = c("EI", "RR", "HM")) # Reorder the Regions

# Define Color Ramp for each region
colorramp <- c("#FC4E07", "#00AFBB", "#E7B800") # Define Color ramp
theme_set(theme_bw(base_size = 16))


