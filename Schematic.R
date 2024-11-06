

#Conceptual Scheme

# Apply Scatter 3
schematic <- ggplot(data=qc_subset, aes(x=Cgas_DOC_ratio*100, y=DMeHg_DHg_DMeHG_ratio*100
)) + 
  geom_point(size = NA) +
  geom_smooth(method=lm)+
  labs(y = bquote("By-Product : Substrate(%)"), x = bquote(paste("By-Product : Substrate (%)")))  +
  geom_text(aes(x = 50, y = 0, label = "more C by-product →"), color="black", check_overlap = TRUE, size = 6) +
  geom_text(aes(x = 0, y = 50, label = "more Hg by-product →"), color="black", check_overlap = TRUE, angle=90, size = 6)+

scale_x_continuous(limits=c(0,60))+
  scale_y_continuous(limits=c(0,60))
  
schematic

quartz.save("Outputs/Figures/schematic.pdf", type = "pdf", dpi = 600)


