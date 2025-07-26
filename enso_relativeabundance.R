###relative abundance
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(forecast)

###cor test
#alb - north #northern_alb_model_data
#northern alb
northern_abundance_alb <- northern_alb_model_data %>%
  group_by(Year,Month)%>%
  dplyr::summarise(MEI=mean(MEI),pre_ALB = mean(pre_ALB))
cor.test(northern_abundance_alb$pre_ALB,northern_abundance_alb$MEI)

#alb - south #southern_alb_model_data
#southern alb
southern_abundance_alb <- southern_alb_model_data %>%
  group_by(Year,Month)%>%
  dplyr::summarise(MEI=mean(MEI),pre_ALB = mean(pre_ALB))
cor.test(southern_abundance_alb$pre_ALB,southern_abundance_alb$MEI)

#bet #pacific_bet_model_data
#bet
bet_abundance <- pacific_bet_model_data %>%
  group_by(Year,Month)%>%
  dplyr::summarise(MEI=mean(MEI),pre_BET = mean(pre_BET))
cor.test(bet_abundance$pre_BET,bet_abundance$MEI)

#yft #pacific_yft_model_data
#yft
yft_abundance <- pacific_yft_model_data %>%
  group_by(Year,Month)%>%
  dplyr::summarise(MEI=mean(MEI),pre_YFT = mean(pre_YFT))
cor.test(yft_abundance$pre_YFT,yft_abundance$MEI)

###ccf
#ccf alb_n
ccf_albn<-ccf(northern_abundance_alb$pre_ALB,northern_abundance_alb$MEI)

p_ccf1<-ggCcf(northern_abundance_alb$pre_ALB,northern_abundance_alb$MEI,color="#631879FF",size=1.5)+
  theme_bw() + 
  geom_line(color = "black")+  
  labs(title = "")+
  theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=9))+
  theme(legend.text = element_text(size=12))+
  theme(legend.position = 'none')+
  theme(panel.grid.major = element_blank(),    
        panel.grid.minor = element_blank())   

#ccf alb_s
ccf_albs<-ccf(southern_abundance_alb$pre_ALB,southern_abundance_alb$MEI)

p_ccf2<-ggCcf(southern_abundance_alb$pre_ALB,southern_abundance_alb$MEI,color="darkblue",size=1.5)+
  theme_bw() + 
  geom_line(color = "black")+  
  labs(title = "")+
  theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=9))+
  theme(legend.text = element_text(size=12))+
  theme(legend.position = 'none')+
  theme(panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank())   

#ccf bet
ccf_bet<-ccf(bet_abundance$pre_BET,bet_abundance$MEI)

p_ccf3<-ggCcf(bet_abundance$pre_BET,bet_abundance$MEI,color="#54C568FF",size=1.5)+
  theme_bw() + 
  geom_line(color = "black")+  
  labs(title = "")+
  theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=9))+
  theme(legend.text = element_text(size=12))+
  theme(legend.position = 'none')+
  theme(panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank())   

p_ccf3

#ccf yft
ccf_yft<-ccf(yft_abundance$pre_YFT,yft_abundance$MEI)

p_ccf4<-ggCcf(yft_abundance$pre_YFT,yft_abundance$MEI,color="#F7854E",size=1.5)+
  theme_bw() + 
  geom_line(color = "black")+  
  labs(title = "")+
  theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=9))+
  theme(legend.text = element_text(size=12))+
  theme(legend.position = 'none')+
  theme(panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank())   

p_ccf4

(p_ccf1|p_ccf2)/(p_ccf3|p_ccf4)+
  plot_annotation(tag_levels = 'a')
#topptx(filename = "/ccf.pptx",width=9,height = 6.5)

#############
###time series plot _ CPUE average Year-Month 
#enso_ll_pre_ts_plot
northern_average_cpue_ALB_ts <- northern_alb_model_data %>%
  group_by(Year, Month,Time) %>%
  dplyr::summarise(
                   MEI = mean(MEI, na.rm = TRUE),
                   ALB_N = mean(pre_ALB, na.rm = TRUE)) %>%
  ungroup()

southern_average_cpue_ALB_ts <- southern_alb_model_data %>%
  group_by(Year, Month) %>%
  dplyr::summarise(ALB_S = mean(pre_ALB, na.rm = TRUE)) %>%
  ungroup()

pacfic_average_cpue_BET_ts <- pacific_bet_model_data %>%
  group_by(Year, Month) %>%
  dplyr::summarise(BET = mean(pre_BET, na.rm = TRUE)) %>%
  ungroup()

pacfic_average_cpue_YFT_ts <- pacific_yft_model_data %>%
  group_by(Year, Month) %>%
  dplyr::summarise(YFT = mean(pre_YFT, na.rm = TRUE)) %>%
  ungroup()

#long data for plotting
enso_ll_pre_ts<-cbind(northern_average_cpue_ALB_ts,southern_average_cpue_ALB_ts[,3],pacfic_average_cpue_BET_ts[,3],pacfic_average_cpue_YFT_ts[,3])
enso_ll_pre_ts_long<-gather(enso_ll_pre_ts,key="Tuna",value = "Abundance",ALB_N:YFT)

# 
correlation_values <- data.frame(Tuna = character(), correlation = numeric(), label = character())
tuna_types <- unique(enso_ll_pre_ts_long$Tuna)

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
    p_mei = p_mei,label = label
  ))
}
print(regression_stats)

p_abundance_liner<-
  ggplot(enso_ll_pre_ts_long, aes(x = MEI, y = Abundance, color = Tuna, fill = Tuna)) +
  geom_point(alpha = 0.5,shape=16, size = 2) +  
  geom_smooth(aes(group = Tuna),
    method = "lm",formula = y ~ x + I(x^2),
    se = FALSE,linewidth = 2.5,color = NA) +
  lapply(names(species_colors_faded), function(tuna) {
    geom_smooth(
      data = subset(enso_ll_pre_ts_long, Tuna == tuna),aes(x = MEI, y = Abundance),
      method = "lm",formula = y ~ x + I(x^2),
      se = FALSE,color = species_colors_faded[[tuna]],
      linewidth = 2.5,inherit.aes = FALSE
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
  geom_text(data = regression_stats,
    aes(x = Inf,y = Inf,  
      label = label,
      color = Tuna),
    inherit.aes = FALSE,
    hjust = 1.1, vjust = 1.1,size = 4) +
  labs(x = "MEI", y = "Relative Abundance",
    color = "Tuna Species", fill = "Tuna Species") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white", colour = "black"),
    text = element_text(family = "Arial"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

p_abundance_liner


short_segments_abundance <- enso_ll_pre_ts_long %>%
  group_by(Tuna) %>%
  summarise(
    y_min = quantile(Abundance, 0.35, na.rm = TRUE),
    y_max = quantile(Abundance, 0.55, na.rm = TRUE)
  ) %>%
  mutate(phase = list(phase_lines)) %>%
  tidyr::unnest(phase) %>%
  rename(x = phase) %>%
  mutate(xend = x)

p_abundance_liner <- p_abundance_liner +
  geom_segment(
    data = short_segments_abundance,
    aes(x = x, xend = xend, y = y_min, yend = y_max),
    inherit.aes = FALSE,
    color = "grey20",
    linewidth = 1.2,
    alpha = 0.6
  )

p_abundance_liner
#topptx(filename = "/p_abundance_linear.pptx",width=5,height = 6.5)




