###Fig plot
### Relative Abundance plot
#Fig1
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(eoffice)
###correlation_values
# 
correlation_values <- data.frame(Tuna = character(), correlation = numeric(), label = character())
# Tuna 
tuna_types <- unique(enso_ll_pre_ts_long$Tuna)
# 
for (Tuna in tuna_types) {
  subset_data <- enso_ll_pre_ts_long[enso_ll_pre_ts_long$Tuna == Tuna, ]
  correlation <- cor(subset_data$MEI, subset_data$Abundance, method = "pearson")
  label <- paste0("r = ", round(correlation, 3))
  
  correlation_values <- rbind(
    correlation_values,
    data.frame(Tuna = Tuna, correlation = correlation, label = label)
  )
}
print(correlation_values)
regression_stats <- data.frame()  

for (Tuna in tuna_types) {
  subset_data <- enso_ll_pre_ts_long[enso_ll_pre_ts_long$Tuna == Tuna, ]
  model <- lm(Abundance ~ MEI + I(MEI^2), data = subset_data)
  intercept <- coef(model)[1]
  slope1 <- coef(model)[2]
  slope2 <- coef(model)[3]
  correlation <- cor(subset_data$MEI, subset_data$Abundance, method = "pearson")
  p_mei <- summary(model)$coefficients["MEI", 4]
  formula_label <- paste0("y = ",
                          round(intercept, 3),
                          ifelse(slope1 >= 0, " + ", " - "), abs(round(slope1, 3)), " * x",
                          ifelse(slope2 >= 0, " + ", " - "), abs(round(slope2, 3)), " * x²")
  p_label <- ifelse(p_mei < 0.01, "p < 0.01", paste0("p = ", formatC(p_mei, format = "f", digits = 3)))
  correlation_label <- paste0("r = ", round(correlation, 2))
  label <- paste(formula_label, p_label, correlation_label, sep = "\n")
  regression_stats <- rbind(regression_stats, data.frame(
    Tuna = Tuna,
    formula = formula_label,
    correlation = correlation,
    p_mei = p_mei,
    label = label
  ))
}
# 
print(regression_stats)

p_abundance_liner<-
  ggplot(enso_ll_pre_ts_long, aes(x = MEI, y = Abundance, color = Tuna, fill = Tuna)) +
  geom_point(alpha = 0.5,shape=16, size = 2) +  
  geom_smooth(
    aes(group = Tuna),
    method = "lm",
    formula = y ~ x + I(x^2),
    se = FALSE,
    linewidth = 2.5,
    color = NA  
  ) +
  lapply(names(species_colors_faded), function(tuna) {
    geom_smooth(
      data = subset(enso_ll_pre_ts_long, Tuna == tuna),
      aes(x = MEI, y = Abundance),
      method = "lm",
      formula = y ~ x + I(x^2),
      se = FALSE,
      color = species_colors_faded[[tuna]],
      linewidth = 2.5,
      inherit.aes = FALSE
    )
  }) +
  scale_color_manual(values = c(
    "ALB_N" = "mediumpurple4",
    "ALB_S" = "darkblue",
    "BET" = "darkgreen",
    "YFT" = "darkorange"
  )) +
  scale_fill_manual(values = c(
    "ALB_N" = "mediumpurple4",
    "ALB_S" = "darkblue",
    "BET" = "darkgreen",
    "YFT" = "darkorange"
  )) +
  facet_wrap(~Tuna, scales = "free_y", ncol = 1) +  
  geom_text(
    data = regression_stats,
    aes(
      x = Inf,  
      y = Inf,  
      label = label,  
      color = Tuna
    ),
    inherit.aes = FALSE,
    hjust = 1.1, vjust = 1.1,  
    size = 4
  ) +
  labs(
    x = "MEI",
    y = "Relative Abundance",
    color = "Tuna Species",
    fill = "Tuna Species"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white", colour = "black"),
    text = element_text(family = "Arial"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),  
    legend.position = "none", 
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )+
  theme(panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank())   
p_abundance_liner
fig1a<-p_abundance_liner

###
###abundance_significance
# 
perform_t_test <- function(data) {
  neutral_data <- data %>% filter(Event == "Neutral") %>% pull(Abundance)
  
  results <- data %>%
    filter(Event != "Neutral") %>%
    group_by(Event) %>%
    summarize(
      p_value = t.test(Abundance, neutral_data)$p.value
    )
  
  return(results)
}

significance_results <- enso_ll_pre_ts_long %>%
  group_by(Tuna) %>%
  group_modify(~ perform_t_test(.x)) %>%
  ungroup()

significance_results <- significance_results %>%
  mutate(significance = case_when(
    p_value < 0.001 ~ "< 0.001",
    p_value < 0.01 ~ "< 0.01",
    #p_value < 0.05 ~ "*",
    TRUE ~ "ns"
  ))

print(significance_results)

significance_annotations <- significance_results %>%
  filter(significance != "ns") %>%  
  mutate(
    y_position = 1.1 * max(enso_ll_pre_ts_long$Abundance),  
    xmin = "Neutral",                        
    xmax = Event                             
  )

significance_annotations <- significance_results %>%
  mutate(
    y_position = seq(
      from = max(enso_ll_pre_ts_long$Abundance) * 1.1,
      by = 0.2,
      length.out = n()
    ),
    xmin = "Neutral",
    xmax = Event,
    label = case_when(
      p_value < 0.001 ~ "p < 0.001",
      p_value < 0.01 ~ "p < 0.01",
      TRUE ~ paste0("p = ", formatC(p_value, format = "f", digits = 4), " ns")
    )
  )

significance_annotations <- significance_annotations %>%
  mutate(y_position = ifelse(Tuna %in% c("ALB_N"), y_position * 0.4, y_position),
         y_position = ifelse(Tuna %in% c("ALB_S"), y_position * 0.4, y_position),
         y_position = ifelse(Tuna %in% c("BET"), y_position * 0.05, y_position),
         y_position = ifelse(Tuna %in% c( "YFT"), y_position * 0.05, y_position))
sample_counts <- aggregate(
  Abundance ~ Tuna + Event,
  data = enso_ll_pre_ts_long,
  FUN = length
)
colnames(sample_counts) <- c("Tuna", "Event", "n")
mean_values <- aggregate(
  Abundance ~ Tuna + Event,
  data = enso_ll_pre_ts_long,
  FUN = mean
)
colnames(mean_values) <- c("Tuna", "Event", "mean_abundance")
print(mean_values)

event_colors <- c(
  "Strong La Niña" = "#631879FF",
  "Weak La Niña" = "darkblue",
  "Neutral" = "#54C568FF",
  "Weak El Niño" = "#F7854E",
  "Strong El Niño" = "darkred"
)
event_order <- c("Strong La Niña", "Weak La Niña", "Neutral", "Weak El Niño", "Strong El Niño")
enso_ll_pre_ts_long$Event <- factor(enso_ll_pre_ts_long$Event, levels = event_order)

p_abundance_violin<-
  ggplot(enso_ll_pre_ts_long, aes(x = Event, y = Abundance)) +
  geom_violin(aes(color=Event),alpha = 0.4, trim = FALSE) +  
  geom_boxplot(aes(color=Event),
               width = 0.2, alpha = 0.5, outlier.color = "black", outlier.size = 1
  ) +  
  geom_jitter(
    aes(color = Event),
    width = 0.15, size = 1.5, alpha = 0.3
  ) +  
  scale_color_manual(values = event_colors) +  
  scale_fill_manual(values = event_colors) +  
  facet_wrap(~Tuna, scales = "free_y",ncol=1) +      
  
  geom_signif(
    data = significance_annotations,
    aes(
      xmin = xmin, xmax = xmax,
      annotations = label,
      y_position = y_position
    ),
    manual = TRUE,
    textsize = 4  
  ) +
  geom_point(
    data = mean_values,
    aes(x = Event, y = mean_abundance),
    color = "#A20056FF", size = 3, shape = 21, fill = "#A20056FF",
    inherit.aes = FALSE
  ) +
  labs(
    x = "Event Type",
    y = "Relative Abundance",
    fill = "Event Type"
  ) +
  theme_bw() +
  scale_x_discrete(
    expand = expansion(mult = c(0.2, 0.25))  
  ) +
  theme(
    strip.background = element_rect(fill = "white", colour = "black"),
    strip.text = element_text(face = 'bold', size = 14),
    text = element_text(family = "Arial"),
    axis.title.x = element_text(size = 14, face = 'bold'),
    axis.title.y = element_text(size = 14, face = 'bold'),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  )+
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())   

p_abundance_violin
fig1b<-p_abundance_violin

###
p_abundance_errorbar<-
  ggplot(relative_change, aes(y = Event, x = Relative_Change, fill = Event,color = Event)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")+
    geom_segment(
    aes(
      x = Min_Relative_Change,
      xend = Max_Relative_Change,
      y = Event,
      yend = Event
    ),
    color = "grey60",  
    linetype = "dashed",  
    size = 1)+ 
  geom_point(
    aes(x = Relative_Change),
    size = 5,
    shape = 21, 
  ) +
  geom_point(
    aes(x = Min_Relative_Change, y = Event),
    size = 2,  
    shape = 21,
    fill = "grey60",
    color = "grey60"
  ) +
  geom_point(
    aes(x = Max_Relative_Change, y = Event),
    size = 2,  
    shape = 21,
    fill = "grey60",
    color = "grey60"
  ) +
  geom_text(
    aes(
      x = Relative_Change,
      label = paste0(round(Relative_Change, 2), "%")
    ),vjust=-0.5,
    hjust = -0.5,  
    size = 4,
    color="black"
  ) +
  scale_color_manual(values = c(
    "Strong La Niña" = "#631879FF",
    "Weak La Niña" = "darkblue",
    "Weak El Niño" = "#F7854E",
    "Strong El Niño" = "darkred"
  )) +
  scale_fill_manual(values = c(
    "Strong La Niña" = "#631879FF",
    "Weak La Niña" = "darkblue",
    "Weak El Niño" = "#F7854E",
    "Strong El Niño" = "darkred"
  ))+
  facet_wrap(~ Tuna,nrow = 1) +  
  labs(
    y = "ENSO Event",
    x = "Relative Change in Abundance (%)",
    color = "ENSO Event"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white", colour = "black"),
    text = element_text(family = "Arial"),
    plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
    axis.title.x = element_text(size = 14, face = 'bold'),
    axis.title.y = element_text(size = 14, face = 'bold'),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    title = element_text(size = 14),
    strip.text = element_text(face = 'bold', size = 9),
    legend.text = element_text(size = 12),
    legend.position = 'none'
  ) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())   
p_abundance_errorbar
fig1c<-p_abundance_errorbar

fi1ab<-
  fig1a+fig1b+
  plot_layout(widths = c(1, 1.5))

fi1ab
fi1ab/fig1c+
  plot_annotation(tag_levels = 'a')+
  plot_layout(heights  = c(1.5, 0.5))

topptx(filename = "Fig/Fig 1.pptx",,width=13,height = 14)


###Fig 2
labels_df_lon <- data.frame(Tuna = character(), label = character(), stringsAsFactors = FALSE)

# Loop through species and calculate statistics
unique_tunas <- unique(enso_ll_cog_long$Tuna)
labels_df_lon <- data.frame()  
for (species in unique_tunas) {
  data <- subset(enso_ll_cog_long, Tuna == species)
  lm_model <- lm(MEI ~ G_Lon + I(G_Lon^2), data = data)
  intercept <- coef(lm_model)[1]
  slope1 <- coef(lm_model)[2]
  slope2 <- coef(lm_model)[3]
  # Pearson correlation
  cor_test <- cor.test(data$MEI, data$G_Lon, method = "pearson")
  r_value <- cor_test$estimate
  p_value <- cor_test$p.value
  formula_text <- paste0("y = ",
                         round(intercept, 3),
                         ifelse(slope1 >= 0, " + ", " - "), abs(round(slope1, 3)), " * x",
                         ifelse(slope2 >= 0, " + ", " - "), abs(round(slope2, 3)), " * x²")
  p_text <- case_when(
    p_value < 0.001 ~ "p < 0.001",
    p_value < 0.01 ~ "p < 0.01",
    TRUE ~ paste0("p = ", formatC(p_value, format = "f", digits = 3), " ns")
  )
  r_text <- paste0("r = ", round(r_value, 2))
  label_text <- paste(formula_text, p_text, r_text, sep = "\n")
  labels_df_lon <- rbind(labels_df_lon, data.frame(Tuna = species, label = label_text, stringsAsFactors = FALSE))
}

# Merge labels with original data
enso_ll_cog_lon_long <- merge(enso_ll_cog_long, labels_df_lon, by = "Tuna")
# Define colors for species
species_colors <- c("ALB_N" = "mediumpurple4", "ALB_S" = "darkblue", "BET" = "darkgreen", "YFT" = "darkorange")
species_colors_faded <- c(
  "ALB_N" = alpha("mediumpurple4", 0.7),
  "ALB_S" = alpha("darkblue", 0.7),
  "BET"   = alpha("darkgreen", 0.7),
  "YFT"   = alpha("darkorange", 0.7)
)
# Create the plot
p_cog_lon_linear<-
  ggplot(enso_ll_cog_lon_long, aes(x = MEI, y = G_Lon, color = Tuna)) +
  geom_point(alpha = 0.5, shape=16) +  # Scatter points
  geom_smooth(
    aes(group = Tuna),
    method = "lm",
    formula = y ~ x + I(x^2),
    se = FALSE,
    linewidth = 2.5,
    color = NA  
  ) +
  lapply(names(species_colors_faded), function(tuna) {
    geom_smooth(
      data = subset(enso_ll_cog_lon_long, Tuna == tuna),
      aes(x = MEI, y = G_Lon),
      method = "lm",
      formula = y ~ x + I(x^2),
      se = FALSE,
      color = species_colors_faded[[tuna]],
      linewidth = 2.5,
      inherit.aes = FALSE
    )
  }) +
  facet_wrap(~Tuna,scale = "free_y", ncol = 1) +  # Separate plots for each species
  scale_color_manual(values = species_colors) +  # Custom colors
  geom_text(
    data = labels_df_lon, 
    aes(x = 0, 
        y = 200, label = label,color = Tuna), 
    inherit.aes = FALSE, 
    hjust = 1, size = 4,
    fontface = "bold"  
  ) +  
  labs(
    x = "MEI", 
    y = "Longitude (°)", 
  ) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank())   
p_cog_lon_linear

fig2a<-p_cog_lon_linear
fig2b<-p_tuna_lon_violin
fig2c<-p_lon_relative
fig2a|fig2b|fig2c
topptx(filename = "Fig/Fig2.pptx",,width=13,height = 14)


enso_ll_cog_lat_long


#Fig S9
# Loop through species and calculate statistics
labels_df_lat <- data.frame(Tuna = character(), label = character(), stringsAsFactors = FALSE)
unique_tunas <- unique(enso_ll_cog_long$Tuna)
for (species in unique_tunas) {
  data <- subset(enso_ll_cog_long, Tuna == species)
  lm_model <- lm(G_Lat ~ MEI + I(MEI^2), data = data)
  intercept <- coef(lm_model)[1]
  slope1 <- coef(lm_model)[2]
  slope2 <- coef(lm_model)[3]
  cor_test <- cor.test(data$MEI, data$G_Lat, method = "pearson")
  r_value <- cor_test$estimate
  p_value <- cor_test$p.value
  formula_text <- paste0("y = ",
                         round(intercept, 3),
                         ifelse(slope1 >= 0, " + ", " - "), abs(round(slope1, 3)), " * x",
                         ifelse(slope2 >= 0, " + ", " - "), abs(round(slope2, 3)), " * x²")
  p_text <- ifelse(p_value < 0.001, "p < 0.001", paste0("p = ", format.pval(p_value, digits = 4)))
  r_text <- paste0("r = ", round(r_value, 2))
  label_text <- paste(formula_text, p_text, r_text, sep = "\n")
  labels_df_lat <- rbind(labels_df_lat, data.frame(Tuna = species, label = label_text, stringsAsFactors = FALSE))
}

enso_ll_cog_lat_long <- merge(enso_ll_cog_long, labels_df_lat, by = "Tuna")
enso_ll_cog_lat_long <- enso_ll_cog_lat_long %>%
  group_by(Tuna) %>%
  mutate(
    label_x = max(MEI, na.rm = TRUE) * 0.9,  
    label_y = max(G_Lat, na.rm = TRUE) * 0.6  
  )

# Create the plot
p_cog_lat_linear <-
  ggplot(enso_ll_cog_lat_long, aes(x = MEI, y = G_Lat, color = Tuna)) +
  geom_point(alpha = 0.25) +  
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = TRUE, size = 1.2) +
  facet_wrap(~Tuna, nrow = 2,scale="free") +  
  scale_color_manual(values = species_colors) +
  geom_text(
    data = labels_df_lat,
    aes(x = Inf, y = -Inf, label = label, color = Tuna),
    inherit.aes = FALSE,
    hjust = 1.1, vjust = -0.5,
    size = 4,
    fontface = "bold"
  ) +
  labs(
    x = "MEI",
    y = "Latitude (°)"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white", colour = "black"),
    text = element_text(family = "Arial"),
    plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
    axis.title.x = element_text(size = 14, face = 'bold'),
    axis.title.y = element_text(size = 14, face = 'bold'),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    title = element_text(size = 14),
    strip.text = element_text(face = 'bold', size = 14),
    legend.text = element_text(size = 12),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p_cog_lat_linear

topptx(filename = "/Fig/Fig S9.pptx",width=7,height = 7)
ggsave(filename = "/Fig/Fig S9.png",dpi=600,width=7,height = 7)

#Fig3

p_ellipse_albn<-
  ggplot() +
  geom_point(cleaned_alb_n_data,mapping= aes(x = G_Lon, y = G_Lat, color = Event), size = 3, alpha = 0.2) +  
  scale_color_manual(values = c(
    "Strong La Niña" = "#631879FF",
    "Weak La Niña" = "darkblue",
    "Neutral" = "#54C568FF",
    "Weak El Niño" = "#F7854E",
    "Strong El Niño" = "darkred"
  ), name = "Event Type") +
  stat_ellipse(cleaned_alb_n_data,
               mapping=aes(x = G_Lon, y = G_Lat,group = Event, fill = Event ,color = Event),  
               geom = "polygon", alpha = 0.1, level = 0.95, size = 0.8  
  )+
  scale_fill_manual(values = c(
    "Strong La Niña" = "#631879FF",
    "Weak La Niña" = "darkblue",
    "Neutral" = "#54C568FF",
    "Weak El Niño" = "#F7854E",
    "Strong El Niño" = "darkred"
  ), name = "Event Type") +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Event Type",
    color = "Event Type"
  ) +
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey60',color='grey60',size=0.5)+
  coord_sf(xlim = c(178, 202),  ylim = c(12, 38))+
  theme_bw() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )+
  theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12,angle=25,hjust=0.65))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  theme(legend.position = 'none')+
  scale_y_latitude(name='',breaks = seq(-40,40,5))+
  scale_x_longitude(name='',breaks = seq(100,300,5))+
  theme(panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank()) 
p_ellipse_albn
#topptx(filename = "/Fig/p_ellipse_albn.pptx",width=3,height = 4)


cleaned_tuna_data<-rbind(cleaned_alb_n_data,cleaned_alb_s_data,cleaned_bet_data,cleaned_yft_data)
p_ellipse_tuna<-ggplot() +
  geom_point(cleaned_tuna_data,mapping= aes(x = G_Lon, y = G_Lat, color = Event), size = 3, alpha = 0.2) +  
  scale_color_manual(values = c(
    "Strong La Niña" = "#631879FF",
    "Weak La Niña" = "darkblue",
    "Neutral" = "#54C568FF",
    "Weak El Niño" = "#F7854E",
    "Strong El Niño" = "darkred"
  ), name = "Event Type") +
  stat_ellipse(cleaned_tuna_data,
               mapping=aes(x = G_Lon, y = G_Lat,group = Event, fill = Event ,color = Event),  
               geom = "polygon", alpha = 0.1, level = 0.95, size = 0.8
  )+
  scale_fill_manual(values = c(
    "Strong La Niña" = "#631879FF",
    "Weak La Niña" = "darkblue",
    "Neutral" = "#54C568FF",
    "Weak El Niño" = "#F7854E",
    "Strong El Niño" = "darkred"
  ), name = "Event Type") +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Event Type",
    color = "Event Type"
  ) +
  facet_wrap(~Tuna,scales = "free",nrow=2)+
  theme_bw() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )+
  theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12,angle=25,hjust=0.65))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  theme(legend.position = 'bottom')+
  scale_y_latitude(name='',breaks = seq(-40,40,5))+
  scale_x_longitude(name='',breaks = seq(100,300,10))+
  theme(panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank())+ 
  coord_quickmap()
p_ellipse_tuna
ggsave(filename = "/Fig/Fig S10.png",dpi=600,,width=10,height = 8)
topptx(filename = "/Fig/Fig S10.pptx",width=10,height = 8)

#p_fig2_left<-(p_cog_lon_linear/p_cog_lat_linear)/p_ellipse+
#  plot_layout(heights  = c(0.5,0.5,1.5))
#p_fig2_left
#p_fig2_right<-
#  (p_fig2_left |p_ellipse_tuna)+
#  plot_layout(widths  = c(1,0.5))+
#  plot_annotation(tag_levels = 'a')

#p_fig2_right
#topptx(filename = "/Figure/tuna_cog_significance.pptx",width=12,height = 12)

###Fig 3
p_fig3_left<-  (p_ellipse/p_ellipse_tuna)+
  plot_layout(heights   = c(1,0.25))

(p_tuna_enso_dis|p_fig3_left)+
  plot_annotation(tag_levels = 'a')&
  theme(
    plot.tag = element_text(face = "bold") 
  )

topptx(filename = "/Figure/Fig 3.pptx",width=16,height = 9)
ggsave(filename = "/Figure/Fig 3.png",dpi=600,width=18,height = 10)

#p_tuna_enso_dis|p_ellipse_tuna+
#  plot_layout(widths   = c(1,0.25))+
#  plot_annotation(tag_levels = 'a')

#Fig 4
###imortance_plot
#sv_importance_alb_n
sv_importance_alb_n<-sv_importance(shap_xgboost_alb_northern, show_numbers = TRUE,max_display=25)+
  theme_bw()
sv_importance_alb_n<-sv_importance_alb_n$data

var_importance_alb_n=tibble(var=sv_importance_alb_n$feature, importance=sv_importance_alb_n$value)
var_importance_alb_n=var_importance_alb_n[c(1,2,5,6,7),]

var_importance_alb_n=var_importance_alb_n %>% mutate(importance = importance/sum(importance))

var_importance_alb_n_plot<-
  ggplot(var_importance_alb_n, aes(x=reorder(var,importance), y=importance)) + 
  geom_bar(stat = "identity", fill="mediumpurple4") + 
  coord_flip() + 
  theme_test()+
  scale_y_continuous(expand=c(0,0))+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))

var_importance_alb_n_plot
shap_xgboost_alb_northern_O2_300<-cbind(Y=shap_xgboost_alb_northern$S[,"O2_300"],X=shap_xgboost_alb_northern$X$O2_300/1000)
shap_xgboost_alb_northern_O2_300<-as.data.frame(shap_xgboost_alb_northern_O2_300)

shap_xgboost_alb_northern_O2_300 <- shap_xgboost_alb_northern_O2_300[order(-shap_xgboost_alb_northern_O2_300$Y), ]
index <- which(shap_xgboost_alb_northern_O2_300$Y < 0)[1]

x_when_y_negative <- shap_xgboost_alb_northern_O2_300$X[index]
x_when_y_negative
shap_xgboost_alb_northern_O2_300_plot<-
  ggplot()+
  geom_point(data=shap_xgboost_alb_northern_O2_300,mapping=aes(x=X,y=Y),size=1.5,alpha=0.4,color="grey")+
  geom_smooth(data=shap_xgboost_alb_northern_O2_300,mapping=aes(x=X,y=Y),method = "gam", se = FALSE, color="mediumpurple4", size=2.5,alpha=0.8) +  
  geom_hline(yintercept = 0,linetype="dashed")+
  annotate("text", x = Inf, y = Inf, label = "O2_300", hjust = 1.1, vjust = 1.1, size = 4.5, color = "black") + 
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  labs(x = expression("Oxygen (mmol/m"^3*")")) + 
  scale_y_continuous(limits = c(-0.15,0.15))

shap_xgboost_alb_northern_O2_300_plot
shap_xgboost_alb_northern_Sali_300<-cbind(Y=shap_xgboost_alb_northern$S[,"Sali_300"],X=shap_xgboost_alb_northern$X$Sali_300)

shap_xgboost_alb_northern_Sali_300_plot<-ggplot()+
  geom_point(data=shap_xgboost_alb_northern_Sali_300,mapping=aes(x=X,y=Y),size=1.5,alpha=0.4,color="grey")+
  geom_smooth(data=shap_xgboost_alb_northern_Sali_300,mapping=aes(x=X,y=Y),method = "gam",formula = y ~ s(x, bs = "cs", k = 50), se = FALSE, color="mediumpurple4", size=2.5,alpha=0.8) +  
  geom_hline(yintercept = 0,linetype="dashed")+
  annotate("text", x = Inf, y = Inf, label = "Sali_300", hjust = 1.1, vjust = 1.1, size = 4.5, color = "black") + 
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  labs(x = expression("Salinity (psu)")) +
  scale_y_continuous(limits = c(-0.15,0.15))
shap_xgboost_alb_northern_Sali_300_plot

shap_xgboost_alb_northern_Temp_0<-cbind(Y=shap_xgboost_alb_northern$S[,"Temp_0"],X=shap_xgboost_alb_northern$X$Temp_0)
shap_xgboost_alb_northern_Temp_0<-as.data.frame(shap_xgboost_alb_northern_Temp_0)
shap_xgboost_alb_northern_Temp_0 <- shap_xgboost_alb_northern_Temp_0[order(shap_xgboost_alb_northern_Temp_0$X), ]
index <- which(shap_xgboost_alb_northern_Temp_0$Y > 0)[1]

x_when_y_negative <- shap_xgboost_alb_northern_Temp_0$X[index]
x_when_y_negative

shap_xgboost_alb_northern_Temp_0_plot<-ggplot()+
  geom_point(data=shap_xgboost_alb_northern_Temp_0,mapping=aes(x=X,y=Y),size=1.5,alpha=0.4,color="grey")+
  geom_smooth(data=shap_xgboost_alb_northern_Temp_0,mapping=aes(x=X,y=Y),method = "gam", se = FALSE, color="mediumpurple4", size=2.5,alpha=0.8) +  
  annotate("text", x = Inf, y = Inf, label = "Temp_0", hjust = 1.1, vjust = 1.1, size = 4.5 ,color = "black") + 
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  labs(x = bquote("Temperature (" * degree * "C)")) +
  scale_y_continuous(limits = c(-0.15,0.15))+
  scale_x_continuous(breaks = seq(0,30,5))
shap_xgboost_alb_northern_Temp_0_plot

smooth_data <- ggplot_build(shap_xgboost_alb_southern_SSH_plot)$data[[2]]
head(smooth_data)
first_positive_x <- smooth_data$x[which(smooth_data$y > 0)[3]]
cat("x > 0:", first_positive_x, "\n")

#
#shap_xgboost_alb_northern_O2_47
shap_xgboost_alb_northern_O2_47<-cbind(Y=shap_xgboost_alb_northern$S[,"O2_47"],X=shap_xgboost_alb_northern$X$O2_47/1000)

shap_xgboost_alb_northern_O2_47_plot<-ggplot()+
  geom_point(data=shap_xgboost_alb_northern_O2_47,mapping=aes(x=X,y=Y),size=1.5,alpha=0.4,color="grey")+
  geom_smooth(data=shap_xgboost_alb_northern_O2_47,mapping=aes(x=X,y=Y),method = "gam", formula = y ~ s(x, bs = "cs", k = 20),se = FALSE, color="mediumpurple4", size=2.5,alpha=0.8) +  
  annotate("text", x = Inf, y = Inf, label = "O2_47", hjust = 1.1, vjust = 1.1, size = 4.5, color = "black") + 
  
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  labs(x = expression("Oxygen (mmol/m"^3*")"))  +
  scale_y_continuous(limits = c(-0.15,0.15))  
shap_xgboost_alb_northern_O2_47_plot

#shap_xgboost_alb_northern_MLD
shap_xgboost_alb_northern_MLD<-cbind(Y=shap_xgboost_alb_northern$S[,"MLD"],X=shap_xgboost_alb_northern$X$MLD)

shap_xgboost_alb_northern_MLD_plot<-ggplot()+
  geom_point(data=shap_xgboost_alb_northern_MLD,mapping=aes(x=X,y=Y),size=1.5,alpha=0.4,color="grey")+
  geom_smooth(data=shap_xgboost_alb_northern_MLD,mapping=aes(x=X,y=Y),method = "gam", formula = y ~ s(x, bs = "cs", k = 10),se = FALSE, color="mediumpurple4", size=2.5,alpha=0.8) +  
  annotate("text", x = Inf, y = Inf, label = "MLD", hjust = 1.1, vjust = 1.1, size = 4.5, color = "black") + 
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  labs(x = expression("Mixed Layer Depth (m)"))  +
  scale_y_continuous(limits = c(-0.15,0.15)) 

shap_xgboost_alb_northern_MLD_plot
###
shap_alb_n_plot<-var_importance_alb_n_plot/shap_xgboost_alb_northern_O2_300_plot/shap_xgboost_alb_northern_Temp_0_plot/shap_xgboost_alb_northern_Sali_300_plot/shap_xgboost_alb_northern_MLD_plot/shap_xgboost_alb_northern_O2_47_plot
shap_alb_n_plot
##

#################################
#sv_importance_alb_s
sv_importance_alb_s<-sv_importance(shap_xgboost_alb_southern, show_numbers = TRUE,max_display=25)+
  theme_bw()
sv_importance_alb_s<-sv_importance_alb_s$data


var_importance_alb_s=tibble(var=sv_importance_alb_s$feature, importance=sv_importance_alb_s$value)
var_importance_alb_s=var_importance_alb_s[c(1,3,4,5,7),]

var_importance_alb_s=var_importance_alb_s %>% mutate(importance = importance/sum(importance))

var_importance_alb_s_plot<-
  ggplot(var_importance_alb_s, aes(x=reorder(var,importance), y=importance)) + 
  geom_bar(stat = "identity", fill="darkblue") + 
  coord_flip() + 
  theme_test()+
  scale_y_continuous(expand=c(0,0))+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))
var_importance_alb_s_plot

#shap_xgboost_alb_southern_Temp_245
shap_xgboost_alb_southern_Temp_245<-cbind(Y=shap_xgboost_alb_southern$S[,"Temp_245"],X=shap_xgboost_alb_southern$X$Temp_245)
shap_xgboost_alb_southern_Temp_245_plot<-ggplot()+
  geom_point(data=shap_xgboost_alb_southern_Temp_245,mapping=aes(x=X,y=Y),size=1.5,alpha=0.4,color="grey")+
  geom_smooth(data=shap_xgboost_alb_southern_Temp_245,mapping=aes(x=X,y=Y),method = "gam", se = FALSE, color="darkblue", size=2.5,alpha=0.8) + 
  annotate("text", x = Inf, y = Inf, label = "Temp_245", hjust = 1.1, vjust = 1.1, size = 4.5, color = "black") +
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  labs(x = bquote("Temperature (" * degree * "C)")) +
  scale_y_continuous(limits = c(-0.15,0.15)) +
  scale_x_continuous(breaks=seq(0,30,5)) 
shap_xgboost_alb_southern_Temp_245_plot

#shap_xgboost_alb_southern_SSH
shap_xgboost_alb_southern_SSH<-cbind(Y=shap_xgboost_alb_southern$S[,"SSH"],X=shap_xgboost_alb_southern$X$SSH)
shap_xgboost_alb_southern_SSH_plot<-ggplot()+
  geom_point(data=shap_xgboost_alb_southern_SSH,mapping=aes(x=X,y=Y),size=1.5,alpha=0.4,color="grey")+
  geom_smooth(data=shap_xgboost_alb_southern_SSH,mapping=aes(x=X,y=Y),method = "gam", formula = y ~ s(x, bs = "cs", k = 20), se = FALSE, color="darkblue", size=2.5,alpha=0.8) +  
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  labs(x = "Sea surface height (m)") +
  scale_y_continuous(limits = c(-0.15,0.15)) 
shap_xgboost_alb_southern_SSH_plot

#shap_xgboost_alb_southern_O2_0
shap_xgboost_alb_southern_O2_0<-cbind(Y=shap_xgboost_alb_southern$S[,"O2_0"],X=shap_xgboost_alb_southern$X$O2_0/1000)
shap_xgboost_alb_southern_O2_0_plot<-ggplot()+
  geom_point(data=shap_xgboost_alb_southern_O2_0,mapping=aes(x=X,y=Y),size=1.5,alpha=0.4,color="grey")+
  geom_smooth(data=shap_xgboost_alb_southern_O2_0,mapping=aes(x=X,y=Y),method = "gam", se = FALSE, color="darkblue", size=2.5,alpha=0.8) + 
  annotate("text", x = Inf, y = Inf, label = "O2_0", hjust = 1.1, vjust = 1.1, size = 4.5, color = "black") + 
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  labs(x = expression("Oxygen (mmol/m"^3*")"))  +
  scale_y_continuous(limits = c(-0.15,0.15))
shap_xgboost_alb_southern_O2_0_plot

#shap_xgboost_alb_southern_Chl_0
shap_xgboost_alb_southern_Chl_0<-cbind(Y=shap_xgboost_alb_southern$S[,"Chl_0"],X=shap_xgboost_alb_southern$X$Chl_0)
shap_xgboost_alb_southern_Chl_0_plot<-ggplot()+
  geom_point(data=shap_xgboost_alb_southern_Chl_0,mapping=aes(x=X,y=Y),size=1.5,alpha=0.4,color="grey")+
  geom_smooth(data=shap_xgboost_alb_southern_Chl_0,mapping=aes(x=X,y=Y),method = "gam", se = FALSE, color="darkblue", size=2.5,alpha=0.8) +  
  annotate("text", x = Inf, y = Inf, label = "Chl_0", hjust = 1.1, vjust = 1.1, size = 4.5, color = "black") + 
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  labs(x = expression("Chlorophyll " ~ (mg/m^3))) +
  scale_y_continuous(limits = c(-0.15,0.15)) 
shap_xgboost_alb_southern_Chl_0_plot

#shap_xgboost_alb_southern_Temp_0
shap_xgboost_alb_southern_Temp_0<-cbind(Y=shap_xgboost_alb_southern$S[,"Temp_0"],X=shap_xgboost_alb_southern$X$Temp_0)
shap_xgboost_alb_southern_Temp_0_plot<-ggplot()+
  geom_point(data=shap_xgboost_alb_southern_Temp_0,mapping=aes(x=X,y=Y),size=1.5,alpha=0.4,color="grey")+
  geom_smooth(data=shap_xgboost_alb_southern_Temp_0,mapping=aes(x=X,y=Y),method = "gam", se = FALSE, color="darkblue", size=2.5,alpha=0.8) +  
  annotate("text", x = Inf, y = Inf, label = "Temp_0", hjust = 1.1, vjust = 1.1, size = 4.5, color = "black") + 
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  labs(x = bquote("Temperature (" * degree * "C)")) +
  scale_y_continuous(limits = c(-0.15,0.15)) +
  scale_x_continuous(breaks=seq(0,30,5)) 
shap_xgboost_alb_southern_Temp_0_plot

#
shap_alb_s_plot<-var_importance_alb_s_plot/shap_xgboost_alb_southern_Temp_245_plot/shap_xgboost_alb_southern_Temp_0_plot/shap_xgboost_alb_southern_O2_0_plot/shap_xgboost_alb_southern_SSH_plot/shap_xgboost_alb_southern_Chl_0_plot
shap_alb_s_plot

#################################
#bet
sv_importance_bet<-sv_importance(shap_xgboost_bet, show_numbers = TRUE,max_display=25)+
  theme_bw()
sv_importance_bet<-sv_importance_bet$data
var_importance_bet=tibble(var=sv_importance_bet$feature, importance=sv_importance_bet$value)
var_importance_bet=var_importance_bet[c(1,3,4,6,7),]
var_importance_bet=var_importance_bet %>% mutate(importance = importance/sum(importance))
var_importance_bet_plot<-
  ggplot(var_importance_bet, aes(x=reorder(var,importance), y=importance)) + 
  geom_bar(stat = "identity", fill="darkgreen") + 
  coord_flip() + 
  theme_test()+
  scale_y_continuous(expand=c(0,0))+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))
var_importance_bet_plot


#
#shap_xgboost_bet_Sali_97
shap_xgboost_bet_Sali_97<-cbind(Y=shap_xgboost_bet$S[,"Sali_97"],X=shap_xgboost_bet$X$Sali_97)
shap_xgboost_bet_Sali_97_plot<-ggplot()+
  geom_point(data=shap_xgboost_bet_Sali_97,mapping=aes(x=X,y=Y),size=1.5,alpha=0.4,color="grey")+
  geom_smooth(data=shap_xgboost_bet_Sali_97,mapping=aes(x=X,y=Y),method = "gam",formula = y ~ s(x, bs = "cs", k = 10), se = FALSE, color="darkgreen", size=2.5,alpha=0.8) +  
  annotate("text", x = Inf, y = Inf, label = "Sali_97", hjust = 1.1, vjust = 1.1, size = 4.5, color = "black") + 
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  labs(x = expression("Salinity (psu)")) +
  scale_y_continuous(limits = c(-0.15,0.15))
shap_xgboost_bet_Sali_97_plot

#shap_xgboost_bet_O2_47
shap_xgboost_bet_O2_47<-cbind(Y=shap_xgboost_bet$S[,"O2_47"],X=shap_xgboost_bet$X$O2_47/1000)

shap_xgboost_bet_O2_47_plot<-ggplot()+
  geom_point(data=shap_xgboost_bet_O2_47,mapping=aes(x=X,y=Y),size=1.5,alpha=0.4,color="grey")+
  geom_smooth(data=shap_xgboost_bet_O2_47,mapping=aes(x=X,y=Y),method = "gam",formula = y ~ s(x, bs = "cs", k = 10), se = FALSE, color="darkgreen", size=2.5,alpha=0.8) +  
  annotate("text", x = Inf, y = Inf, label = "O2_47", hjust = 1.1, vjust = 1.1, size = 4.5, color = "black") + 
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  labs(x = expression("Oxygen (mmol/m"^3*")"))  +
  scale_y_continuous(limits = c(-0.15,0.15)) 
shap_xgboost_bet_O2_47_plot

#shap_xgboost_bet_Temp_47
shap_xgboost_bet_Temp_47<-cbind(Y=shap_xgboost_bet$S[,"Temp_47"],X=shap_xgboost_bet$X$Temp_47)
shap_xgboost_bet_Temp_47_plot<-ggplot()+
  geom_point(data=shap_xgboost_bet_Temp_47,mapping=aes(x=X,y=Y),size=1.5,alpha=0.4,color="grey")+
  geom_smooth(data=shap_xgboost_bet_Temp_47,mapping=aes(x=X,y=Y),method = "gam",formula = y ~ s(x, bs = "cs", k = 10), se = FALSE, color="darkgreen", size=2.5,alpha=0.8) +  
  annotate("text", x = Inf, y = Inf, label = "Temp_47", hjust = 1.1, vjust = 1.1, size = 4.5, color = "black") + 
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  labs(x = bquote("Temperature (" * degree * "C)")) +
  scale_y_continuous(limits = c(-0.15,0.15)) +
  scale_x_continuous(breaks=seq(0,30,5)) 
shap_xgboost_bet_Temp_47_plot

#shap_xgboost_bet_O2_300
shap_xgboost_bet_O2_300<-cbind(Y=shap_xgboost_bet$S[,"O2_300"],X=shap_xgboost_bet$X$O2_300/1000)
shap_xgboost_bet_O2_300_plot<-ggplot()+
  geom_point(data=shap_xgboost_bet_O2_300,mapping=aes(x=X,y=Y),size=1.5,alpha=0.4,color="grey")+
  geom_smooth(data=shap_xgboost_bet_O2_300,mapping=aes(x=X,y=Y),method = "gam",formula = y ~ s(x, bs = "cs", k = 10), se = FALSE, color="darkgreen", size=2.5,alpha=0.8) +  
  annotate("text", x = Inf, y = Inf, label = "O2_300", hjust = 1.1, vjust = 1.1, size = 4.5, color = "black") + 
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  labs(x = expression("Oxygen (mmol/m"^3*")"))  +
  scale_y_continuous(limits = c(-0.15,0.15)) 
shap_xgboost_bet_O2_300_plot

#shap_xgboost_bet_Chl_300
shap_xgboost_bet_Chl_300<-cbind(Y=shap_xgboost_bet$S[,"Chl_300"],X=shap_xgboost_bet$X$Chl_300)

shap_xgboost_bet_Chl_300_plot<-ggplot()+
  geom_point(data=shap_xgboost_bet_Chl_300,mapping=aes(x=X,y=Y),size=1.5,alpha=0.4,color="grey")+
  geom_smooth(data=shap_xgboost_bet_Chl_300,mapping=aes(x=X,y=Y),method = "gam",formula = y ~ s(x, bs = "cs", k = 10), se = FALSE, color="darkgreen", size=2.5,alpha=0.8) +  
  annotate("text", x = Inf, y = Inf, label = "Chl_300", hjust = 1.1, vjust = 1.1, size = 4.5, color = "black") +
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  labs(x = expression("Chlorophyll " ~ (mg/m^3))) +
  scale_y_continuous(limits = c(-0.15,0.15)) 
shap_xgboost_bet_Chl_300_plot
shap_bet_plot<-  var_importance_bet_plot/shap_xgboost_bet_O2_47_plot/shap_xgboost_bet_O2_300_plot/shap_xgboost_bet_Temp_47_plot/shap_xgboost_bet_Sali_97_plot/shap_xgboost_bet_Chl_300_plot


#################################
####yft
sv_importance_yft<-sv_importance(shap_xgboost_yft, show_numbers = TRUE,max_display=25)+
  theme_bw()
sv_importance_yft<-sv_importance_yft$data
var_importance_yft=tibble(var=sv_importance_yft$feature, importance=sv_importance_yft$value)
var_importance_yft=var_importance_yft[c(1,2,3,6,7),]
var_importance_yft=var_importance_yft %>% mutate(importance = importance/sum(importance))
var_importance_yft_plot<-
  ggplot(var_importance_yft, aes(x=reorder(var,importance), y=importance)) + 
  geom_bar(stat = "identity", fill="darkorange") + 
  coord_flip() + 
  theme_test()+
  scale_y_continuous(expand=c(0,0))+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))
var_importance_yft_plot

#shap_xgboost_yft_Temp_47
shap_xgboost_yft_Temp_47<-cbind(Y=shap_xgboost_yft$S[,"Temp_47"],X=shap_xgboost_yft$X$Temp_47)
shap_xgboost_yft_Temp_47_plot<-ggplot()+
  geom_point(data=shap_xgboost_yft_Temp_47,mapping=aes(x=X,y=Y),size=1.5,alpha=0.4,color="grey")+
  geom_smooth(data=shap_xgboost_yft_Temp_47,mapping=aes(x=X,y=Y),method = "gam",formula = y ~ s(x, bs = "cs", k = 10), se = FALSE, color="darkorange", size=2.5,alpha=0.8) + 
  annotate("text", x = Inf, y = Inf, label = "Temp_47", hjust = 1.1, vjust = 1.1, size = 4.5, color = "black") + 
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  labs(x = bquote("Temperature (" * degree * "C)")) +
  scale_y_continuous(limits = c(-0.15,0.15)) +
  scale_x_continuous(breaks=seq(0,30,5)) 
shap_xgboost_yft_Temp_47_plot

#shap_xgboost_yft_O2_47
shap_xgboost_yft_O2_47<-cbind(Y=shap_xgboost_yft$S[,"O2_47"],X=shap_xgboost_yft$X$O2_47/1000)

shap_xgboost_yft_O2_47_plot<-ggplot()+
  geom_point(data=shap_xgboost_yft_O2_47,mapping=aes(x=X,y=Y),size=1.5,alpha=0.4,color="grey")+
  geom_smooth(data=shap_xgboost_yft_O2_47,mapping=aes(x=X,y=Y),method = "gam",formula = y ~ s(x, bs = "cs", k = 10), se = FALSE, color="darkorange", size=2.5,alpha=0.8) +  
  annotate("text", x = Inf, y = Inf, label = "O2_47", hjust = 1.1, vjust = 1.1, size = 4.5, color = "black") + 
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  labs(x = expression("Oxygen (mmol/m"^3*")"))  +
  scale_y_continuous(limits = c(-0.15,0.15)) 
shap_xgboost_yft_O2_47_plot

#shap_xgboost_yft_O2_300
shap_xgboost_yft_O2_300<-cbind(Y=shap_xgboost_yft$S[,"O2_300"],X=shap_xgboost_yft$X$O2_300/1000)

shap_xgboost_yft_O2_300_plot<-ggplot()+
  geom_point(data=shap_xgboost_yft_O2_300,mapping=aes(x=X,y=Y),size=1.5,alpha=0.4,color="grey")+
  geom_smooth(data=shap_xgboost_yft_O2_300,mapping=aes(x=X,y=Y),method = "gam",formula = y ~ s(x, bs = "cs", k = 10), se = FALSE, color="darkorange", size=2.5,alpha=0.8) +  
  annotate("text", x = Inf, y = Inf, label = "O2_300", hjust = 1.1, vjust = 1.1, size = 4.5, color = "black") + 
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  labs(x = expression("Oxygen (mmol/m"^3*")"))  +
  scale_y_continuous(limits = c(-0.15,0.15)) 
shap_xgboost_yft_O2_300_plot

#shap_xgboost_yft_O2_200
shap_xgboost_yft_O2_200<-cbind(Y=shap_xgboost_yft$S[,"O2_200"],X=shap_xgboost_yft$X$O2_200/1000)

shap_xgboost_yft_O2_200_plot<-ggplot()+
  geom_point(data=shap_xgboost_yft_O2_200,mapping=aes(x=X,y=Y),size=1.5,alpha=0.4,color="grey")+
  geom_smooth(data=shap_xgboost_yft_O2_200,mapping=aes(x=X,y=Y),method = "gam",formula = y ~ s(x, bs = "cs", k = 10), se = FALSE, color="darkorange", size=2.5,alpha=0.8) +  
  annotate("text", x = Inf, y = Inf, label = "O2_200", hjust = 1.1, vjust = 1.1, size = 4.5, color = "black") + 
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  labs(x = expression("Oxygen (mmol/m"^3*")"))  +
  scale_y_continuous(limits = c(-0.15,0.15))
shap_xgboost_yft_O2_200_plot

#shap_xgboost_yft_O2_97
shap_xgboost_yft_O2_97<-cbind(Y=shap_xgboost_yft$S[,"O2_97"],X=shap_xgboost_yft$X$O2_97/1000)

shap_xgboost_yft_O2_97_plot<-ggplot()+
  geom_point(data=shap_xgboost_yft_O2_97,mapping=aes(x=X,y=Y),size=1.5,alpha=0.4,color="grey")+
  geom_smooth(data=shap_xgboost_yft_O2_97,mapping=aes(x=X,y=Y),method = "gam", se = FALSE, formula = y ~ s(x, bs = "cs", k = 20),color="darkorange", size=2.5,alpha=0.8) + 
  annotate("text", x = Inf, y = Inf, label = "O2_97", hjust = 1.1, vjust = 1.1, size = 4.5, color = "black") + 
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1),
        plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  labs(x = expression("Oxygen (mmol/m"^3*")"))  +
  scale_y_continuous(limits = c(-0.15,0.15)) 
shap_xgboost_yft_O2_97_plot
shap_yft_plot<-var_importance_yft_plot/shap_xgboost_yft_Temp_47_plot/shap_xgboost_yft_O2_47_plot/shap_xgboost_yft_O2_300_plot/shap_xgboost_yft_O2_200_plot/shap_xgboost_yft_O2_97_plot

#######################
shap_combined_plot<-(shap_alb_n_plot|shap_alb_s_plot|shap_bet_plot|shap_yft_plot)
shap_combined_plot <- shap_combined_plot + 
  plot_layout(guides = 'collect') + 
  plot_annotation(tag_levels = 'a',
                  theme = theme(
                    plot.tag = element_text(face = "bold", size = 14)  
                  )
  )
shap_combined_plot
ggsave(filename = "Fig 4.png",width=12,height = 12,dpi=600)

ggsave(filename = "Fig 4.svg",width=12,height = 12,dpi=600)

