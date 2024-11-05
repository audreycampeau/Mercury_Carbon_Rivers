library(tidyverse)
library(ggridges) # to vizualise the median in the distribution
library(ggpubr)

theme_set(theme_bw(base_size = 14))


# Ridge Plots Function :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
DensityPlots <- function(x, y, dunn_1, dunn_2, dunn_3) { # Function to make the ridge plots
  ggplot(qc_subset_overall, aes({{ x }}, {{ y }},
    fill = {{ y }},
    color = {{ y }}
  )) +
    stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantile_lines = TRUE, quantiles = 2) +
    scale_fill_manual(values = alpha(c(colorramp5, "black"), 0.4)) +
    scale_color_manual(values = c(colorramp5, "black")) +
    annotate("text", x = Inf, y = Inf, label = {{ dunn_1 }}, color = colorramp5[1], hjust = 7.5, vjust = 1.5, size = 4) +
    annotate("text", x = Inf, y = Inf, label = {{ dunn_2 }}, color = colorramp5[2], hjust = 5.5, vjust = 1.5, size = 4) +
    annotate("text", x = Inf, y = Inf, label = {{ dunn_3 }}, color = colorramp5[3], hjust = 3.5, vjust = 1.5, size = 4) +
    theme_minimal()+
    coord_flip()+
    theme(legend.position = "none", axis.title.x = element_blank())

}


# Make plots for each Variable :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

DOCHist <- DensityPlots(x = DOC_Mean, y = Region, 
                        dunn_1 = "a", dunn_2 = "b", dunn_3 = "c"
                        ) +
                        labs(x = bquote("DOC (mg L"^-1 ~ ")"))
print(DOCHist)


C1Hist <- DensityPlots(x = C1 * 100, y = Region,
                       dunn_1 = "a", dunn_2 = "a", dunn_3 = "b"
                        ) +
                        labs(x = bquote("C1 (%)"))
print(C1Hist)

C4Hist <- DensityPlots(x = C4 * 100, y = Region,
                       dunn_1 = "a", dunn_2 = "b", dunn_3 = "b"
                        ) +
                          labs(x = bquote("C4 (%)"))
print(C4Hist)


DHgHist <- DensityPlots(x = DHg_Mean, y = Region,
                        dunn_1 = "a", dunn_2 = "b", dunn_3 = "c"
                        ) +
                         labs(x = bquote("DHg (ng L"^-1 ~ ")"))
print(DHgHist)

DMeHgHist <- DensityPlots(x = DMeHg_Mean, y = Region,
                          dunn_1 = "a", dunn_2 = "b", dunn_3 = "b"
                            ) +
                          labs(x = bquote("DMeHg (ng L"^-1 ~ ")"))
print(DMeHgHist)

DHgRatioHist <- DensityPlots(x = DMeHg_DHg_ratio * 100, y = Region,
                             dunn_1 = "a", dunn_2 = "b", dunn_3 = "c"
                              ) +
                              labs(x = bquote("DMeHg:DHg (%)"))
print(DHgRatioHist)


CO2Hist <- DensityPlots(x = CO2_mgCL, y = Region,
                        dunn_1 = "a", dunn_2 = "b", dunn_3 = "c"
                        ) +
                        scale_x_log10() +
                        labs(x = bquote("CO"[2] ~ "(mg L"^-1 ~ ")"))
print(CO2Hist)


CH4Hist <- DensityPlots(x = CH4_mgCL, y = Region,
                        dunn_1 = "a", dunn_2 = "a", dunn_3 = "b"
                        ) +
                        scale_x_log10(breaks=c(0.001,0.01, 0.1, 1,10), labels=c("0.001", "0.01", "0.1", "1","10")) +
                        labs(x = bquote("CH"[4] ~ "(mg L"^-1 ~ ")"))
print(CH4Hist)


MOXHist <- DensityPlots(x = MOX_noZero, y = Region,
                        dunn_1 = "a", dunn_2 = "a", dunn_3 = "b"
                        ) +
  scale_x_log10(breaks=c(0.0001, 0.001,0.01, 0.1, 1,10), labels=c("0.0001","0.001", "0.01", "0.1", "1","10")) +
  labs(x = bquote("MOX" ~ "(mg L"^-1 ~ ")"))
print(MOXHist)


AlphaHist <- DensityPlots(x = Alpha_d13C_CO2_CH4, y = Region,
                          dunn_1 = "a", dunn_2 = "a", dunn_3 = "a"
                          ) +
                          labs(x = bquote(alpha["CO2-CH4"]))
print(AlphaHist)



d13C_CO2_Hist <- DensityPlots(x = d13C_CO2_Mean, y = Region,
                              dunn_1 = "a ", dunn_2 = "b ", dunn_3 = "ab"
                              ) +
                              labs(x = bquote(delta^13 ~ "C-CO"[2] ~ "(%)"))
print(d13C_CO2_Hist)

d13C_CH4_Hist <- DensityPlots(x = d13C_CH4_Mean, y = Region,
                              dunn_1 = " a ", dunn_2 = "ab", dunn_3 = "b "
                              ) +
                              labs(x = bquote(delta^13 ~ "C-CH"[4] ~ "(%)"))
print(d13C_CH4_Hist)



# Combine Plots :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
quartz(width = 10, height = 8, pointsize = 12)
ridgeplots <- ggarrange(

  DOCHist, DHgHist, CO2Hist, CH4Hist,
  C1Hist,  DMeHgHist, d13C_CO2_Hist, d13C_CH4_Hist,
  C4Hist,  DHgRatioHist, MOXHist, AlphaHist,
  align = "hv", ncol = 4, nrow = 3
)


#Combine ridgeplots with map
ggarrange (site_map+theme(legend.position = "right"), ridgeplots, nrow=2, heights=c(1,2.5))

quartz.save("Outputs/Figures/Map_RidgePlots.pdf", type = "pdf", dpi = 600)


# Statistical test (Dunn's test) :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
library(dunn.test)
library(multcompView)
library(PMCMRplus)

dunn.test(qc_subset$d13C_CH4_Mean, g = qc_subset$Region, method = "bonferroni", kw = T, list = F, alpha = 0.05)
multcompLetters(get.pvalues(PMCMRplus::kwAllPairsDunnTest(qc_subset$d13C_CH4_Mean ~ qc_subset$Region, p.adjust = "bonf")), threshold = 0.05)


# Levene's test to determine if there is a difference in variance :::::::::::::::::::::::::::::::::
library("car")
leveneTest(C1 ~ Region, qc_subset,  method = "bonferroni")
leveneTest(DOC_Mean ~ Region, qc_subset, p.adjust = "bonf")
leveneTest(C4 ~ Region, qc_subset, p.adjust = "bonf")

leveneTest(DHg_Mean ~ Region, qc_subset)
leveneTest(DMeHg_Mean ~ Region, qc_subset)
leveneTest(DMeHg_DHg_ratio ~ Region, qc_subset)

leveneTest(CO2_mgCL ~ Region, qc_subset)
leveneTest(CH4_mgCL ~ Region, qc_subset)
leveneTest(MOX ~ Region, qc_subset)

leveneTest(Alpha_d13C_CO2_CH4 ~ Region, qc_subset)
leveneTest(d13C_CO2_Mean ~ Region, qc_subset)
leveneTest(d13C_CH4_Mean ~ Region, qc_subset)


leveneTest(C1 ~ Region, filter(qc_subset, Region != "EI"))
leveneTest(DOC_Mean ~ Region, filter(qc_subset, Region != "EI"))
leveneTest(C4 ~ Region, filter(qc_subset, Region != "EI"))

leveneTest(DHg_Mean ~ Region, filter(qc_subset, Region != "EI"))
leveneTest(DMeHg_Mean ~ Region, filter(qc_subset, Region != "EI"))
leveneTest(DMeHg_DHg_ratio ~ Region, filter(qc_subset, Region != "EI"))

leveneTest(CO2_mgCL ~ Region, filter(qc_subset, Region != "EI"))
leveneTest(CH4_mgCL ~ Region, filter(qc_subset, Region != "EI"))
leveneTest(MOX ~ Region, filter(qc_subset, Region != "EI"))

leveneTest(Alpha_d13C_CO2_CH4 ~ Region, filter(qc_subset, Region != "EI"))
leveneTest(d13C_CO2_Mean ~ Region, filter(qc_subset, Region != "EI"))
leveneTest(d13C_CH4_Mean ~ Region, filter(qc_subset, Region != "EI"))

round(tapply(qc_subset$CH4_mgCL, qc_subset$Region, var, na.rm = TRUE),6)

