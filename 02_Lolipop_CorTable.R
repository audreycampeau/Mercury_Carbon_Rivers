library(plotly)


# Create a label combining x and y variables
correlation_table_all_2 <- correlation_table_all %>%
  mutate(label = paste(x_var, y_var, sep = " vs "))

# Create size categories based on p-value
correlation_table_all_2 <- correlation_table_all_2 %>%
  mutate(p_value_size_cat = case_when(
    p.value < 0.001 ~ "<0.001",
    p.value < 0.01 ~ "<0.01",
    p.value < 0.05 ~ "<0.05",
    TRUE ~ "N.S."
  ))

# Create a separate dataframe for overall estimates
overall_estimates <- correlation_table_all_2 %>%
  filter(region == "Overall") %>% #Keep only overall estimates
  arrange(estimate) %>% #arrange it in ascendent order
  mutate(order = row_number()) #apply this order to the row number

# Join the order back to the main dataframe
correlation_table_all_2 <- correlation_table_all_2 %>% 
  left_join(overall_estimates %>% select(label, order), by = "label") #combine the ordered estimates with the complete correlation table based on the labels


# Reorder the region factor
correlation_table_all_2 <- correlation_table_all_2 %>%
  mutate(region = factor(region, levels = c("EI", "HM", "RR", "Overall"))) # make sure the region order is EI, HM, RR, Overall

# Create a new column with formatted labels
correlation_table_all_2 <- correlation_table_all_2 %>%
  mutate(label_formatted = case_when(
    label == "DHg_Mean vs C1" ~ "[Hg] vs C1(%)",
    label == "DHg_Mean vs C4" ~ "[Hg] vs C4(%)",
    label == "DHg_Mean vs DOC_Mean" ~ "[Hg] vs [DOC]",
    label == "DMeHg_DHg_DMeHG_ratio vs Cgas_DOC_ratio" ~ "MeHg:Hg vs Cgas:DOC",
    label == "DMeHg_DHg_ratio vs C1" ~ "MeHg:Hg vs C1(%)",
    label == "DMeHg_DHg_ratio vs C4" ~ "MeHg:Hg vs C4(%)",
    label == "DMeHg_DHg_ratio vs DOC_Mean" ~ "MeHg:Hg vs [DOC]",
    label == "DMeHg_Mean vs CH4_mgCL" ~ "[MeHg] vs [CH4]",
    label == "DMeHg_Mean vs CH4_plus_MOX" ~ "[MeHg] vs [CH4 + MOX]",
    TRUE ~ label  # Keep original label if not specified above
  ))

# Create the lollipop plot
lolipop= ggplot(correlation_table_all_2, 
                  aes(x = estimate, y = reorder(label_formatted, order))) +
  
                  geom_segment(aes(x = 0, xend = estimate, yend = label_formatted), color = "black") +
                  geom_point(aes(fill = region, size = p_value_size_cat, shape=region)) +
                  geom_point(aes(size = p_value_size_cat,shape=region), color = "black") +
                  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  
                  scale_fill_manual(values = c(colorramp, "black")) +
                  scale_shape_manual(values=c(21,22,24,23))+
                  scale_size_discrete(range = c(6, 2)) +
                  scale_x_continuous(limits=c(-0.8,0.8),breaks = seq(-1, 1, 0.2)) +
  
                  theme_minimal(base_size = 16) +
                  theme(
                    axis.text.x = element_text(color = "black"),  # Also changed x-axis text to black for consistency
                    axis.text.y = element_text(angle = 0, hjust = 1, color = "black"),
                        legend.position = "top",
                        plot.title = element_text(size = 20)) +
                  
                  labs(x = "Spearman rho", y = "", fill = "Region", size = "p-value")+
                  guides(
                    fill = "none",  # Increase size for fill legend
                    size = guide_legend(override.aes = list(fill = "black"))  # Make size legend points black
                  )

lolipop





