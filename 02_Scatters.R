

library(tidyverse)
library(ggpmisc)
formula <- y ~ x
library(ggpubr)


# Figure 2 Scatters and lolipop:

# Make a function to create the plots
create_scatters_ellipse <- function(data_points, data_ellipse, x, y, Region, cor.position, label_left) {
  ggplot(data=data_ellipse, aes({{ x }}, {{ y }},
                   fill = {{ Region }},
                   color = {{ Region }}, group = {{ Region }}, shape = {{Region}}
  )) + 
    geom_point(data=data_points, color = "black", size = 2, alpha = 0.7) +
    scale_shape_manual(values=c(21,22,24,21))+
    
    stat_ellipse(aes(color = Region), size = 1.5, show.legend = F) +
    stat_ellipse(aes(color = Region), color = "black", size = 0.5, show.legend = F) +
    
    stat_cor(
      method = "spearman",
      label.y.npc = "top", label.x.npc = label_left, cor.coef.name = "rho",
      aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"), color = Region),
      r.accuracy = 0.01, p.accuracy = 0.01, show.legend = F
    ) +
    scale_fill_manual(values = c(colorramp,"black")) +
    scale_color_manual(values = c(colorramp,"black")) +
    guides(fill = guide_legend(order = 1), color = guide_legend(order = 1)) # Adjusted the order argument
}


# Apply Scatter 1
scatter_Hg_DOC <- create_scatters_ellipse(data_points=qc_subset, data_ellipse = qc_subset_overall, 
                                          x = DOC_Mean, y = DHg_Mean, Region = Region,
                                          cor.position = c(10, 9, 8),label_left=0) +
  geom_abline(intercept = 0, slope = 0.25, linewidth = 0.3) +
  scale_x_continuous(limits = c(0, 30)) +
  labs(y = bquote("Hg (ng L"^-1 ~ ")"), x = bquote("DOC (mg L"^-1 ~ ")"), color = "Region") +
  theme(legend.position = "none") +
  geom_text(aes(x = 20, y = 6, label = "0.25"), color = "black", check_overlap = TRUE, size = 4)

scatter_Hg_DOC


# Apply Scatter 2
scatter_Hg_C1 <- create_scatters_ellipse(data_points=qc_subset, data_ellipse = qc_subset_overall,  
                                     x = C1 * 100, y = DHg_Mean, Region = Region, 
                                     cor.position = c(10, 9, 8),label_left=0) +
  theme(legend.position = "none") +
  labs(y = bquote("Hg (ng L"^-1 ~ ")"), x = "C1 (%)", color = "Region")

scatter_Hg_C1



# Apply Scatter 3
scatter_byprod_subst <- create_scatters_ellipse(data_points=qc_subset, data_ellipse = qc_subset_overall,  
                                         x = Cgas_DOC_ratio, y = DMeHg_DHg_DMeHG_ratio, Region = Region, 
                                         cor.position = c(10, 9, 8), label_left=0) +
  theme(legend.position = "none") +
  labs(y = bquote("MeHg : Hg"), x = bquote(paste("(CO"[2] ~ "+ CH"[4] ~ ") : DOC ")),
    fill = "Region")  
  #geom_text(aes(x = 0.3, y = -0.05, label = "more C by-product →"), color="black", check_overlap = TRUE, size = 4) +
  #geom_text(aes(x = -0.09, y = 0.33, label = "more Hg by-product →"), color="black", check_overlap = TRUE, angle=90, size = 4)
  

scatter_byprod_subst

# Apply Scatter 4

scatter_MeHg_CH4Mox <- create_scatters_ellipse(data_points=qc_subset, data_ellipse = qc_subset_overall,  
                                                x = CH4_plus_MOX, y = DMeHg_Mean, Region = Region, 
                                               cor.position = c(10, 9, 8), label_left=0) +
  theme(legend.position = "none") +
  labs(
    y = bquote("MeHg (ng L"^-1 ~ ")"), 
    x = bquote(paste("[CH"[4] ~ "+MOX] (mg L"^-1 ~ ")")),
    color = "Region") +
  scale_x_log10( breaks=c(0.001,0.01, 0.1), labels=c("0.001","0.01", "0.1") ) + 
  scale_y_log10()
  
scatter_MeHg_CH4Mox





