
# Supplementary Figures 

# Make a Biplot of d13C-CO2 and d13C-CH4 with isotope fractionation lines.

d13C_Biplot= ggplot(
                qc_subset,
                aes(y = d13C_CO2_Mean, x = d13C_CH4_Mean, color = Region #, size = CH4_mgCL
                    )) +
                geom_point(aes(fill = Region), color = "black", size=3, shape = 21, alpha = 0.9) +
                scale_fill_manual(values = colorramp) +
                geom_point(y = c(-8.5), x = c(-47), shape = "*", show.legend = F, size = 15, color = "black") + # The atmospheric δ13C-CO2 -8.5‰ and δ13C- CH4 -47.5‰
                
              geom_point(data=qc_subset[67,], color = "black", shape = "x", size = 4, alpha = 1) +
  
                scale_x_continuous(limits = c(-120, -30)) +
                scale_y_continuous(limits = c(-40, 00))+
                
                
                geom_abline(intercept = 40, slope = 1) + # ε = 40‰ for methane oxidation, based on (Whiticar, 1999).
                geom_abline(intercept = 55, slope = 1, linetype = "dashed") + # ε = 55‰ for acetoclastic pathway, based on (Whiticar, 1999).
                geom_abline(intercept = 85, slope = 1, linetype = "dotted") + # ε = 85‰ for hydrogenotrophic pathway , based on (Whiticar, 1999).
                
                
                annotate("text", x = -70, y = -38, label = "ε = 40‰ ", angle = 64, hjust = 0, size=3) + #(Methane Oxidation)
                annotate("text", x = -90, y = -38, label = "ε = 55‰ ", angle = 64, hjust = 0, size=3) + #(Acetoclastic Pathway)
                annotate("text", x = -120, y = -38, label = "ε = 85‰ ", angle = 64, hjust = 0, size=3) + #(Hydrogenotrophic Pathway)
                
                labs(title="B)",
                  y = bquote(delta^13 ~ "C-CO"[2] ~ "(‰)"),
                  x = bquote(delta^13 ~ "C-CH"[4] ~ "(‰)"), color = "Region",
                  size = bquote("CH"[4] ~ " (mg L"^-1 ~ ")")
                ) 

d13C_Biplot

# Miller-Tans  ----------------------------------------------------------------------------------------
# Identify the d13C-CH4 source value
Miller_tans=  ggplot(
                    qc_subset[-67,],
                    aes(y = d13C_CH4_CH4mg, x = CH4_mgCL)
                  ) +
                    
                    geom_point(data=qc_subset, aes(fill = Region), color = "black", shape = 21, size = 3, alpha = 1) +
                    geom_point(data=subset(qc_subset, Alpha_d13C_CO2_CH4 > 1.055 ), color = "black", shape = "x", size = 4, alpha = 1) +
                    
                    geom_smooth(
                      method = "lm", color="black",
                     # data = function(x) {
                     #   slice(x, c(67) ) #55, 64, 66 are also outlier (e >55) but they dont have pCH4 measurements
                     # }, # Exclude specific rows from Linear Model
                      se = T, fullrange = TRUE, size = 0.5, show.legend = F
                    ) +
                    
                    geom_smooth( data=qc_subset,
                      method = "lm", color="black", linetype="dashed",
                      se = T, fullrange = TRUE, size = 0.5, show.legend = F
                    ) +
                    
                    scale_fill_manual(values = colorramp) +
                    scale_color_manual(values = colorramp) +
                    stat_poly_eq(aes(label = ..eq.label..), # ,
                                 formula = formula, label.x.npc = "right", label.y.npc = "top", # #label.y = 9,
                                 parse = TRUE
                    ) +
                    #facet_wrap(~Region, nrow = 3, scales = "free") +
                    labs(title = "A)",
                      x = bquote("CH"[4] ~ " (mg L"^-1 ~ ")"),
                      y = bquote(delta^13 ~ "C-CH"[4] ~ " (‰) x  CH"[4] ~ " (mg L"^-1 ~ ")"), color = "Region"
                    )


Miller_tans

#Combine Bi-plot and Miller-Tans  ----------------------------------------------------------------------------------------
quartz(width = 9, height = 4.5, pointsize = 12)

ggarrange(Miller_tans+ theme(legend.position = "none"), d13C_Biplot+theme(legend.position = "top"),
          nrow = 1, ncol = 2,
          common.legend = T, align = "hv"
)

quartz.save("Outputs/Figures/d13C_SuppFigure.png", type = "png", dpi = 600)



# Supplementary Figure _ Scatter CO2+CH4 ~ DHg:DMeHg ----------------------------------------------------------------------
quartz(width = 6, height = 4.5, pointsize = 12)
ggplot(
  qc_subset,
  aes(
    x = (CO2_mgCL + CH4_mgCL),
    y = DMeHg_DHg_ratio)
) +
  geom_point(aes(fill = Region), shape = 21, size = 3) +
  scale_fill_manual(values = colorramp) +
  scale_color_manual(values = colorramp) +
  stat_ellipse(aes(color = Region), level = 0.95, size = 2, show.legend = F) +
  stat_ellipse(aes(group = Region), color = "black", level = 0.95, show.legend = F) +
  stat_cor(
    method = "spearman",
    label.y.npc = "top", label.x = 3.5, cor.coef.name = "rho",
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"), color = Region),
    r.accuracy = 0.01, p.accuracy = 0.01, show.legend = F
  ) +
  labs(
    y = bquote("DMeHg:DHg"), x = bquote(paste("(CO"[2] ~ "+ CH"[4] ~ ")")),
    fill = "Region"
  )

quartz.save("outputs/Scatter_DMeHg_HgRatio_CO2CH4.png", type = "png", dpi = 600)
