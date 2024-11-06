

# Export data
# Save in RDS format


saveRDS(qc_subset, file = "Outputs/Data/qc_subset.rds")


#Open dataset and format a few things
qc_subset <- readRDS(file = "Outputs/Data/qc_subset.rds")



colorramp <- c("#FC4E07",  "#E69F00" ,"#00AFBB") # Define Color ramp "#E7B800"

#colorramp2 <- c("#0072B2", "#D55E00", "#999999") # Define Color ramp
#colorramp3 <- c("#882255", "#56B4E9", "#F0E442") # Define Color ramp
#colorramp4 <- c("#009E73", "#E69F00", "#666666") # Define Color ramp
colorramp5 <- c("#AA4477", "#E69F00","#009E73") # Define Color ramp

theme_set(theme_minimal(base_size = 14))


#Calculate Hg by-product on substrate ratio
qc_subset$DMeHg_DHg_DMeHG_ratio=qc_subset$DMeHg_Mean /(qc_subset$DHg_Mean - qc_subset$DMeHg_Mean) 

#Calculate CH4 concentration plus MOX 
qc_subset$CH4_plus_MOX=qc_subset$CH4_mgCL + qc_subset$MOX




# reorder the factor region
qc_subset <- qc_subset %>%
  mutate(Region = factor(Region, levels = c("EI", "HM", "RR")))



qc_subset_overall=qc_subset
qc_subset_overall$Region=rep("Overall", nrow(qc_subset))


qc_subset_overall=rbind(qc_subset, qc_subset_overall)


