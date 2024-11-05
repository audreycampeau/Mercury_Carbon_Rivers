
#library(ggpubr)
library(patchwork)




#Combine scatters and lollipop
quartz(width = 12, height = 8, pointsize = 12)

composite_plot <- ( lollipop+ggtitle("") |
  (scatter_a+ggtitle("") + scatter_b+ggtitle("")) / (scatter_a3+theme(legend.position = "none")+ggtitle("")
                             + scatter_b3+theme(legend.position = "none")+ggtitle(""))
    ) +
  plot_layout(widths = c(2, 2))  # This makes the lollipop plot take up half the width

# Add labels to each plot (optional)
composite_plot <- composite_plot & 
  theme(plot.tag = element_text(size = 12, face = "bold")) &
  plot_annotation(tag_levels = 'A')

# Display the composite plot
composite_plot



# Make the combined plot with multiple pannels
quartz(width = 12, height = 8, pointsize = 12)

scatters= ggarrange(scatter_a +theme_bw(base_size = 14), 
                    scatter_b+theme_bw(base_size = 14), 
                    scatter_a3+theme_bw(base_size = 14), 
                    scatter_b3+theme_bw(base_size = 14),
          nrow = 4, ncol = 1, legend = "top",
          align = "hv",
          common.legend = T,
          widths = c(1, 1, 1),
          heights = c(1, 1),
          labels=c("B)", "C)", "D)", "E)")) # , labels=c("A)", "B)", "C)", "D)", "E)", "F)"))

scatters

#quartz(width = 12, height = 8, pointsize = 12)
#lolli_scatters=ggarrange(lollipop, scatters)

quartz(width = 11, height = 12, pointsize = 12)
ggarrange(ggarrange(lollipop, site_map, nrow=2, labels = c("A)", "F)"), heights = c(1.5,1)), scatters, ncol=2, widths = c(2,1))

quartz(width = 11, height = 8, pointsize = 12)
ggarrange(ggarrange(site_map, radar_med_plot+theme(legend.position = "right"), nrow=1, labels = c("A)", "F)"), widths = c(1,1)), violins+theme(legend.position = "none"), nrow=2, heights = c(1,2))

#ggplotly(combined_plot)
