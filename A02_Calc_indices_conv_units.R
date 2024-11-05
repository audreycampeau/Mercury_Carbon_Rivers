#Calculate Indexies

# Calculate Specific Indices 
# dExcess (Index of evaporation)
qc_subset$dexcess <- qc_subset$d2H_H2O - (8 * qc_subset$dO18_H2O) 

# C1-C4 components of Parafac
calculate_C1_C4_components <- function (qc_subset) {
  
  qc_subset$Ftotal <- rowSums(qc_subset[, c("Fmax1", "Fmax2", "Fmax3", "Fmax4")]) # Calculate Ftotal
  
  
  for (i in 1:4) {
    qc_subset[[paste0("C", i)]] <- qc_subset[[paste0("Fmax", i)]] / qc_subset$Ftotal # Calculate C1, C2, C3, and C4
  }
  
  return(qc_subset)
} 

qc_subset=calculate_C1_C4_components(qc_subset) # Apply function 



# Calculate SUVA
qc_subset$SUVA_a254 <- (qc_subset$A254 * 100) / qc_subset$DOC_Mean
qc_subset$SUVA_a440 <- (qc_subset$A440 * 100) / qc_subset$DOC_Mean


# Calculate Mercury Ratios
qc_subset$DMeHg_DOC_ratio <- qc_subset$DMeHg_Mean / qc_subset$DOC_Mean
qc_subset$DHg_DOC_ratio <- qc_subset$DMeHg_Mean / qc_subset$DOC_Mean
qc_subset$DMeHg_DHg_ratio <- qc_subset$DMeHg_Mean / qc_subset$DHg_Mean
qc_subset$TMeHg_THg_ratio <- qc_subset$TMeHg_Mean / qc_subset$THg_Mean

# Calculate d13C indexes
qc_subset$Alpha_d13C_CO2_CH4 <- (qc_subset$d13C_CO2_Mean + 1000) / (qc_subset$d13C_CH4_Mean + 1000)
qc_subset$Alpha_d13C_DOC_CH4 <- (qc_subset$d13C_DOC + 1000) / (qc_subset$d13C_CH4_Mean + 1000)

sort(qc_subset$Alpha_d13C_CO2_CH4)

# Convert alpha to epsilon
qc_subset$epsilon_d13C_CO2_CH4 <- (qc_subset$Alpha_d13C_CO2_CH4 - 1) * 1000



# Derive Henry's Constant: Convert gas ppm to mgCL (CO2 and CH4)

kH_CO2 <- function(temp) {
  kH <- exp(-58.0931 + (90.5069 * (100 / (temp + 273.15))) + (22.294 * log((temp + 273.15) / 100)))
  return(kH)
}

qc_subset$KH_CO2=kH_CO2(ifelse(is.na(qc_subset$Water_Temp.),  # If Temperature is not available, use the mean of all measurements
                               mean(qc_subset$Water_Temp., na.rm = T), 
                               qc_subset$Water_Temp.)) 


kH_CH4 <- function(temp) {
  kH <- exp(-68.8862 + (101.4956 * (100 / (temp+273.15))) + (28.7314 * log((temp + 273.15) / 100)))/22.4
  return(kH)
}


qc_subset$KH_CH4=kH_CH4(ifelse(is.na(qc_subset$Water_Temp.),  # If Temperature is not available, use the mean of all measurements
                               mean(qc_subset$Water_Temp., na.rm = T), 
                               qc_subset$Water_Temp.)) 


#Calculate gas concentration in mg C/L based on Henry's constant for each gases

qc_subset$CO2_mgCL <- qc_subset$KH_CO2 * qc_subset$pCO2_Mean_Headspace * 12.01 / 1000

qc_subset$CH4_mgCL_error <- qc_subset$KH_CO2 * qc_subset$pCH4_Mean_Headspace * 12.01 / 1000

qc_subset$CH4_mgCL <- qc_subset$KH_CH4 * qc_subset$pCH4_Mean_Headspace * 12.01 / 1000

qc_subset$Cgas_mgCL <- qc_subset$CO2_mgCL + qc_subset$CH4_mgCL

ggplot(qc_subset, aes(x=CH4_mgCL_error, y=CH4_mgCL))+
         geom_point()


# Miller-Tans plots
qc_subset$d13C_CH4_CH4mg <- qc_subset$d13C_CH4_Mean * qc_subset$CH4_mgCL



# Calculate Carbon ratios
qc_subset$CO2_CH4_ratio_mgCL <- qc_subset$CH4_mgCL / qc_subset$CO2_mgCL
qc_subset$CO2_DOC_ratio_mgCL <- qc_subset$CO2_mgCL / qc_subset$DOC_Mean
qc_subset$Cgas_DOC_ratio <- (qc_subset$CO2_mgCL + qc_subset$CH4_mgCL) / (qc_subset$DOC_Mean)
qc_subset$Cgas_TotalC_ratio <- (qc_subset$CO2_mgCL + qc_subset$CH4_mgCL) / (qc_subset$CO2_mgCL + qc_subset$CH4_mgCL + qc_subset$DOC_Mean)

