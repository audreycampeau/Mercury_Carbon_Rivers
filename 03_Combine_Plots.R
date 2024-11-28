

# Combine Ridge plots and Map (Figure 1)
quartz(width = 10, height = 8, pointsize = 12)
ridgeplots <- ggarrange(
  
  DOCHist, DHgHist, CO2Hist, CH4Hist,
  C1Hist,  DMeHgHist, d13C_CO2_Hist, d13C_CH4_Hist,
  C4Hist,  DHgRatioHist, MOXHist, AlphaHist,
  align = "hv", ncol = 4, nrow = 3
)

ggarrange (site_map+theme(legend.position = "right"), ridgeplots, nrow=2, heights=c(1,2.5))

quartz.save("Outputs/Figures/Map_RidgePlots.pdf", type = "pdf", dpi = 600)




# Combine scatters and lolipop (Figure 3)

scatters= ggarrange(scatter_Hg_DOC +theme_bw(base_size = 14)+ggtitle("Substrate Convergence"), 
                    scatter_Hg_C1+theme_bw(base_size = 14), 
                    scatter_byprod_subst+theme_bw(base_size = 14)+ggtitle("By-Product Convergence"), 
                    scatter_MeHg_CH4Mox+theme_bw(base_size = 14),
                    nrow = 2, ncol = 2, legend = "top",
                    align = "hv",
                    common.legend = T,
                    widths = c(1, 1, 1),
                    heights = c(1, 1),
                    labels=c("B)", "C)", "D)", "E)")) 
scatters


quartz(width = 18, height =8, pointsize = 12)
ggarrange(lolipop, scatters, ncol=2, labels=c("A)",NA))
quartz.save("Outputs/Figures/Lolipop_scatters.pdf", type = "pdf", dpi = 600)


