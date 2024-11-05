library(tidyverse)
library(ggpmisc)


# Make a Biplot of d13C-CO2 and d13C-CH4 with isotope fractionation lines.
ggplot(
  qc_subset,
  aes(y = d13C_CO2_Mean, x = d13C_CH4_Mean, color = Region, size = CH4_mgCL)
) +
  geom_point(aes(fill = Region), color = "black", shape = 21, alpha = 0.9) +
  scale_fill_manual(values = colorramp) +
  geom_point(y = c(-8.5), x = c(-47), shape = "*", show.legend = F, size = 15, color = "black") + # The atmospheric δ13C-CO2 -8.5‰ and δ13C- CH4 -47.5‰

  # geom_point(data=qc_subset[, c(55, 64, 66 ,67)], shape=22)+

  scale_x_continuous(limits = c(-120, -30)) +
  scale_y_continuous(limits = c(-40, 00)) +
  geom_abline(intercept = 40, slope = 1) + # ε = 40‰ for methane oxidation, based on (Whiticar, 1999).
  geom_abline(intercept = 55, slope = 1, linetype = "dashed") + # ε = 55‰ for acetoclastic pathway, based on (Whiticar, 1999).
  geom_abline(intercept = 85, slope = 1, linetype = "dotted") + # ε = 85‰ for hydrogenotrophic pathway , based on (Whiticar, 1999).


  annotate("text", x = -70, y = -38, label = "ε = 40‰ (Methane Oxidation)", angle = 64, hjust = 0) +
  annotate("text", x = -90, y = -38, label = "ε = 55‰ (Acetoclastic Pathway)", angle = 64, hjust = 0) +
  annotate("text", x = -100, y = -21, label = "ε = 85‰ (Hydrogenotrophic Pathway)", angle = 64, hjust = 0) +
  labs(
    y = bquote(delta^13 ~ "C-CO"[2] ~ "(‰)"),
    x = bquote(delta^13 ~ "C-CH"[4] ~ "(‰)"), color = "Region",
    size = bquote("CH"[4] ~ " (mg L"^-1 ~ ")")
  )



# Miller-Tans  ----------------------------------------------------------------------------------------
# Identify the d13C-CH4 source value

ggplot(
  qc_subset,
  aes(y = d13C_CH4_CH4mg, x = CH4_mgCL)
) +
  geom_smooth(
    method = "lm",
    data = function(x) {
      slice(x, -c(55, 64, 66, 67))
    }, # Exclude specific rows from Linear Model
    se = T, fullrange = TRUE, size = 0.5, show.legend = F
  ) +
  geom_point(aes(fill = Region), color = "black", shape = 21, size = 3, alpha = 1) +
  scale_fill_manual(values = colorramp) +
  scale_color_manual(values = colorramp) +
  stat_poly_eq(aes(label = ..eq.label..), # ,
    formula = formula, label.x.npc = "right", label.y.npc = "top", # #label.y = 9,
    parse = TRUE
  ) +
  # facet_wrap(~Region, nrow = 3, scales = "free") +
  labs(
    x = bquote("CH"[4] ~ " (mg L"^-1 ~ ")"),
    y = bquote(delta^13 ~ "C-CH"[4] ~ " (‰) x  CH"[4] ~ " (mg L"^-1 ~ ")"), color = "Region"
  )

# Identify the slope of the miller tans plot for different subset of data

## Make a function
return_miller_tans_slope <- function(data) {
  return(lm(d13C_CH4_CH4mg ~ CH4_mgCL, data = data)$coefficient[2])
}

## Apply the function
return_miller_tans_slope(data = filter(qc_subset, Region == "HM"))
return_miller_tans_slope(data = filter(qc_subset, Region == "RR"))
return_miller_tans_slope(data = filter(qc_subset, Region == "EI"))
return_miller_tans_slope(data = filter(qc_subset))
return_miller_tans_slope(data = filter(qc_subset[-which(qc_subset$Alpha_d13C_CO2_CH4 > 1.055), ]))



# Calculate Fraction of CH4 oxidized based on equation 1 in Thottathil1 et al., 2018 (DOI: 10.1029/2018JG004464))
# fopen = (δs - δb)/((alpha - 1) × 1000) BAstviken es&T 2002
# D13C-Source is derived from the slope of the Miller-Tans Plot
qc_subset$FOX <- (qc_subset$d13C_CH4_Mean - (-51.7)) / ((qc_subset$Alpha_d13C_CO2_CH4 - 1) * 1000)
  #qc_subset$FOX_68 <- (qc_subset$d13C_CH4_Mean - (-68.36)) / ((qc_subset$Alpha_d13C_CO2_CH4 - 1) * 1000)
#How many FOX are negative?
length(which(qc_subset$FOX < 0)) / length(which(!is.na(qc_subset$FOX)))


# Assume that negative values imply no methane oxidation at all
qc_subset$FOX <- ifelse(qc_subset$FOX < 0, is.na(qc_subset$FOX), qc_subset$FOX)
  #qc_subset$FOX_68 <- ifelse(qc_subset$FOX_68 < 0, is.na(qc_subset$FOX_68), qc_subset$FOX_68)

# Convert FOX to MOX by multiplying with CH4
qc_subset$MOX <- qc_subset$FOX * qc_subset$CH4_mgCL
  #qc_subset$MOX_68 <- qc_subset$FOX_68 * qc_subset$CH4_mgCL
