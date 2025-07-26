#spatial center of gravity (COG)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(eoffice)

#alb north 
northern_centroid_alb <- northern_alb_model_data %>%
  group_by(Year,Month)%>%
  dplyr::summarise(
    MEI=mean(MEI),
    G_Lon_ALB = sum(Lon * pre_ALB) / sum(pre_ALB),
    G_Lat_ALB = sum(Lat * pre_ALB) / sum(pre_ALB),
  )
cor.test(northern_centroid_alb$G_Lon_ALB,northern_centroid_alb$MEI)
cor.test(northern_centroid_alb$G_Lat_ALB,northern_centroid_alb$MEI)

#alb south 
southern_centroid_alb <- southern_alb_model_data %>%
  group_by(Year,Month)%>%
  dplyr::summarise(
    MEI=mean(MEI),
    G_Lon_ALB = sum(Lon * pre_ALB) / sum(pre_ALB),
    G_Lat_ALB = sum(Lat * pre_ALB) / sum(pre_ALB),
  )
cor.test(southern_centroid_alb$G_Lon_ALB,southern_centroid_alb$MEI)
cor.test(southern_centroid_alb$G_Lat_ALB,southern_centroid_alb$MEI)

###bet
bet_centroid <- pacific_bet_model_data %>%
  group_by(Year,Month)%>%
  dplyr::summarise(
    MEI=mean(MEI),
    G_Lon_BET = sum(Lon * pre_BET) / sum(pre_BET),
    G_Lat_BET = sum(Lat * pre_BET) / sum(pre_BET)
  )
cor.test(bet_centroid$G_Lon_BET,bet_centroid$MEI)
cor.test(bet_centroid$G_Lat_BET,bet_centroid$MEI)

###yft
yft_centroid <- pacific_yft_model_data %>%
  group_by(Year,Month)%>%
  dplyr::summarise(
    MEI=mean(MEI),
    G_Lon_YFT = sum(Lon * pre_YFT) / sum(pre_YFT),
    G_Lat_YFT = sum(Lat * pre_YFT) / sum(pre_YFT)
  )
cor.test(yft_centroid$G_Lon_YFT,yft_centroid$MEI)
cor.test(yft_centroid$G_Lat_YFT,yft_centroid$MEI)



#enso_ll_cog_plot
#G_Lon
enso_ll_cog_lon<-cbind(northern_centroid_alb[,1:4],southern_centroid_alb[,4],bet_centroid[,4],yft_centroid[,4])
colnames(enso_ll_cog_lon)[4:7]<-c("ALB_N","ALB_S","BET","YFT")


#Event
##after EVENT CLASSIFY
relative_change_lon <- enso_ll_cog_long %>%
  group_by(Tuna, Event) %>%
  dplyr::summarise(
    Mean_Lon = mean(G_Lon, na.rm = TRUE),          
    Min_Lon = min(G_Lon, na.rm = TRUE),            
    Max_Lon = max(G_Lon, na.rm = TRUE),            
    .groups = "drop"
  )

neutral_means_lon <- relative_change_lon %>%
  filter(Event == "Neutral") %>%
  select(Tuna, Neutral_Lon = Mean_Lon)
relative_change_lon <- relative_change_lon %>%
  left_join(neutral_means_lon, by = "Tuna") %>%
  mutate(
    Relative_Change_Lon = ((Mean_Lon - Neutral_Lon) / Neutral_Lon) * 100, 
    Min_Relative_Change_Lon = ((Min_Lon - Neutral_Lon) / Neutral_Lon) * 100, 
    Max_Relative_Change_Lon = ((Max_Lon - Neutral_Lon) / Neutral_Lon) * 100 
  )
relative_change_lon_no_neutral <- relative_change_lon %>%
  filter(Event != "Neutral")
print(relative_change_lon_no_neutral)

event_order <- c("Strong La Niña", "Weak La Niña", "Neutral", "Weak El Niño", "Strong El Niño")
relative_change_lon_no_neutral$Event <- factor(relative_change_lon_no_neutral$Event, levels = event_order)
baseline_labels_lon <- relative_change_lon_no_neutral %>%
  group_by(Tuna) %>%
  dplyr::summarise(
    Baseline = unique(Neutral_Lon),
    x = max(Relative_Change_Lon, na.rm = TRUE), 
    y = "Strong El Niño")
labels_df_lon <- data.frame(Tuna = character(), label = character(), stringsAsFactors = FALSE)

unique_tunas <- unique(enso_ll_cog_long$Tuna)
labels_df_lon <- data.frame()

for (species in unique_tunas) {
  data <- subset(enso_ll_cog_long, Tuna == species)
  lm_model <- lm(MEI ~ G_Lon + I(G_Lon^2), data = data)
  intercept <- coef(lm_model)[1]
  slope1 <- coef(lm_model)[2]
  slope2 <- coef(lm_model)[3]
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

enso_ll_cog_lon_long <- merge(enso_ll_cog_long, labels_df_lon, by = "Tuna")
species_colors <- c("ALB_N" = "mediumpurple4", "ALB_S" = "darkblue", "BET" = "darkgreen", "YFT" = "darkorange")
species_colors_faded <- c(
  "ALB_N" = alpha("mediumpurple4", 0.7),
  "ALB_S" = alpha("darkblue", 0.7),
  "BET"   = alpha("darkgreen", 0.7),
  "YFT"   = alpha("darkorange", 0.7)
)

p_cog_lon_linear<-
  ggplot(enso_ll_cog_lon_long, aes(x = MEI, y = G_Lon, color = Tuna)) +
  geom_point(alpha = 0.5, shape=16) + 
  geom_smooth(
    aes(group = Tuna),method = "lm", formula = y ~ x + I(x^2),
    se = FALSE, linewidth = 2.5,
    color = NA) +
  lapply(names(species_colors_faded), function(tuna) {
    geom_smooth(
      data = subset(enso_ll_cog_lon_long, Tuna == tuna),
      aes(x = MEI, y = G_Lon),method = "lm", formula = y ~ x + I(x^2),
      se = FALSE, color = species_colors_faded[[tuna]],
      linewidth = 2.5,inherit.aes = FALSE)
  }) +
  facet_wrap(~Tuna,scale = "free_y", ncol = 1) +
  scale_color_manual(values = species_colors) +    
  geom_text( data = labels_df_lon, 
    aes(x = 0, y = 200, label = label,color = Tuna), 
    inherit.aes = FALSE, hjust = 1, size = 4, fontface = "bold") + 
  labs( x = "MEI", y = "Longitude (°)") +
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

p_cog_lat_linear <-
  ggplot(enso_ll_cog_lat_long, aes(x = MEI, y = G_Lat, color = Tuna)) +
  geom_point(alpha = 0.25) +  
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = TRUE, size = 1.2) +
  facet_wrap(~Tuna, nrow = 1) + 
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

#
phase_lines <- c(-1.5, -0.5, 0.5, 1.5)

short_segments_lat <- enso_ll_cog_lat_long %>%
  group_by(Tuna) %>%
  summarise(
    y_min = quantile(G_Lat, 0.35, na.rm = TRUE),
    y_max = quantile(G_Lat, 0.55, na.rm = TRUE)
  ) %>%
  mutate(phase = list(phase_lines)) %>%
  tidyr::unnest(phase) %>%
  rename(x = phase) %>%
  mutate(xend = x)

p_cog_lat_linear <- p_cog_lat_linear +
  geom_segment(
    data = short_segments_lat,
    aes(x = x, xend = xend, y = y_min, yend = y_max),
    inherit.aes = FALSE,
    color = "grey20",
    linewidth = 1.5,
    alpha = 0.6
  )
p_cog_lat_linear
p_cog_lon_linear/p_cog_lat_linear+ plot_annotation(tag_levels = 'a')
topptx(filename = "/Users/linbojun/Library/CloudStorage/OneDrive-個人/Oceanic Fisheries Ecosystem Laboratory/ENSO_LL/final_version/p_cog_linear.pptx",width=10,height = 6.5)


###
#spatial cluster
library(dbscan)
library(ggplot2)
library(dplyr)
library(Hotelling)
library(metR)
library(ggrepel)

# alb_n
alb_n_cog_clusters <- enso_ll_cog_long[enso_ll_cog_long$Tuna == "ALB_N", ]
num_clusters <- 3
if (nrow(alb_n_cog_clusters) >= num_clusters) {
  kmeans_result <- kmeans(alb_n_cog_clusters[, c("G_Lon", "G_Lat")], centers = num_clusters)
  alb_n_cog_clusters$cluster <- kmeans_result$cluster
} else {
  alb_n_cog_clusters$cluster <- NA
}

remove_outliers <- function(data, cols, threshold = 3) {
  cov_matrix <- cov(data[cols])
  center <- colMeans(data[cols])
  mahalanobis_dist <- mahalanobis(data[cols], center, cov_matrix)
  data[mahalanobis_dist <= threshold^2, ]
}

cleaned_alb_n_data <- alb_n_cog_clusters %>%
  group_by(Event) %>%
  group_modify(~ remove_outliers(.x, cols = c("G_Lon", "G_Lat"))) %>%
  ungroup()

# alb_s
alb_s_cog_clusters <- enso_ll_cog_long[enso_ll_cog_long$Tuna == "ALB_S", ]
num_clusters <- 3
if (nrow(alb_s_cog_clusters) >= num_clusters) {
  kmeans_result <- kmeans(alb_s_cog_clusters[, c("G_Lon", "G_Lat")], centers = num_clusters)
  alb_s_cog_clusters$cluster <- kmeans_result$cluster
} else {
  alb_s_cog_clusters$cluster <- NA
}
remove_outliers <- function(data, cols, threshold = 3) {
  cov_matrix <- cov(data[cols])
  center <- colMeans(data[cols])
  mahalanobis_dist <- mahalanobis(data[cols], center, cov_matrix)
  data[mahalanobis_dist <= threshold^2, ]
}

cleaned_alb_s_data <- alb_s_cog_clusters %>%
  group_by(Event) %>%
  group_modify(~ remove_outliers(.x, cols = c("G_Lon", "G_Lat"))) %>%
  ungroup()


####
# bet
bet_cog_clusters <- enso_ll_cog_long[enso_ll_cog_long$Tuna == "BET", ]
num_clusters <- 3
if (nrow(bet_cog_clusters) >= num_clusters) {
  kmeans_result <- kmeans(bet_cog_clusters[, c("G_Lon", "G_Lat")], centers = num_clusters)
  bet_cog_clusters$cluster <- kmeans_result$cluster
  } else {
  bet_cog_clusters$cluster <- NA
}


remove_outliers <- function(data, cols, threshold = 3) {
  cov_matrix <- cov(data[cols])
  center <- colMeans(data[cols])
  mahalanobis_dist <- mahalanobis(data[cols], center, cov_matrix)
  data[mahalanobis_dist <= threshold^2, ]
}

cleaned_bet_data <- bet_cog_clusters %>%
  group_by(Event) %>%
  group_modify(~ remove_outliers(.x, cols = c("G_Lon", "G_Lat"))) %>%
  ungroup()




# YFT
yft_cog_clusters <- enso_ll_cog_long[enso_ll_cog_long$Tuna == "YFT", ]
num_clusters <- 3
if (nrow(yft_cog_clusters) >= num_clusters) {
  kmeans_result <- kmeans(yft_cog_clusters[, c("G_Lon", "G_Lat")], centers = num_clusters)
  yft_cog_clusters$cluster <- kmeans_result$cluster
} else {
  yft_cog_clusters$cluster <- NA
}

remove_outliers <- function(data, cols, threshold = 3) {
  cov_matrix <- cov(data[cols])
  center <- colMeans(data[cols])
  mahalanobis_dist <- mahalanobis(data[cols], center, cov_matrix)
  data[mahalanobis_dist <= threshold^2, ]
}

cleaned_yft_data <- yft_cog_clusters %>%
  group_by(Event) %>%
  group_modify(~ remove_outliers(.x, cols = c("G_Lon", "G_Lat"))) %>%
  ungroup()





# 
alb_n_cog_events <- unique(cleaned_alb_n_data$Event)
compare_with_neutral <- function(event_name, data) {
  neutral_data <- data %>% filter(Event == "Neutral") %>% select(G_Lon, G_Lat)
  event_data <- data %>% filter(Event == event_name) %>% select(G_Lon, G_Lat)
    if (nrow(neutral_data) < 2 || nrow(event_data) < 2) {
    return(data.frame(Event = event_name, T2 = NA, p_value = NA))
  }
  
# Hotelling's T² 
  result <- tryCatch(
    hotelling.test(as.matrix(neutral_data), as.matrix(event_data)),
    error = function(e) NULL 
  )
    if (is.null(result)) {
    return(data.frame(Event = event_name, T2 = NA, p_value = NA))
  }
  
  test_stat <- if ("statistic" %in% names(result$stats)) result$stats$statistic else NA
  p_val <- if ("pval" %in% names(result)) result$pval else NA
  
  data.frame(
    Event = event_name,
    T2 = test_stat,
    p_value = p_val
  )
}
alb_n_cog_results <- lapply(alb_n_cog_events[alb_n_cog_events != "Neutral"], compare_with_neutral, data = cleaned_alb_n_data)
alb_n_cog_results <- do.call(rbind, alb_n_cog_results)
print(alb_n_cog_results)
alb_n_cog_results$Event<-as.factor(alb_n_cog_results$Event)

event_order <- c("Strong La Niña", "Weak La Niña", "Neutral", "Weak El Niño", "Strong El Niño")
alb_n_cog_results$Event <- factor(alb_n_cog_results$Event, levels = event_order)
print(alb_n_cog_results)

alb_n_cog_results <- alb_n_cog_results %>%
  mutate(significance = case_when(
    p_value < 0.001 ~ "< 0.001",
    p_value < 0.01 ~ "< 0.01",
    #p_value < 0.05 ~ "*",
    TRUE ~ "ns"
  ))

alb_n_cog_results_label_data <- alb_n_cog_results %>%
  filter(!is.na(p_value)) %>%
  mutate(
    label = paste(Event, "vs Neutral\nP =", format(p_value, digits = 3), "\n", significance),
    x = mean(cleaned_alb_n_data$G_Lon),  
    y = seq(max(cleaned_alb_n_data$G_Lat) - 3, max(cleaned_alb_n_data$G_Lat) - 10, length.out = n())  
  )




###########################ALB_S
alb_s_cog_events <- unique(cleaned_alb_s_data$Event)

compare_with_neutral <- function(event_name, data) {
  neutral_data <- data %>% filter(Event == "Neutral") %>% select(G_Lon, G_Lat)
  event_data <- data %>% filter(Event == event_name) %>% select(G_Lon, G_Lat)
    if (nrow(neutral_data) < 2 || nrow(event_data) < 2) {
    return(data.frame(Event = event_name, T2 = NA, p_value = NA))
  }
    result <- tryCatch(
    hotelling.test(as.matrix(neutral_data), as.matrix(event_data)),
    error = function(e) NULL  
  )
    if (is.null(result)) {
    return(data.frame(Event = event_name, T2 = NA, p_value = NA))
  }
  test_stat <- if ("statistic" %in% names(result$stats)) result$stats$statistic else NA
  p_val <- if ("pval" %in% names(result)) result$pval else NA
    data.frame(
    Event = event_name,
    T2 = test_stat,
    p_value = p_val
  )
}

alb_s_cog_results <- lapply(alb_s_cog_events[alb_s_cog_events != "Neutral"], compare_with_neutral, data = cleaned_alb_s_data)
alb_s_cog_results <- do.call(rbind, alb_s_cog_results)
print(alb_s_cog_results)
alb_s_cog_results$Event<-as.factor(alb_s_cog_results$Event)

event_order <- c("Strong La Niña", "Weak La Niña", "Neutral", "Weak El Niño", "Strong El Niño")
alb_s_cog_results$Event <- factor(alb_s_cog_results$Event, levels = event_order)
print(alb_s_cog_results)

alb_s_cog_results <- alb_s_cog_results %>%
  mutate(significance = case_when(
    p_value < 0.001 ~ "< 0.001",
    p_value < 0.01 ~ "< 0.01",
    #p_value < 0.05 ~ "*",
    TRUE ~ "ns"
  ))

alb_s_cog_results_label_data <- alb_s_cog_results %>%
  filter(!is.na(p_value)) %>%
  mutate(
    label = paste(Event, "vs Neutral\nP =", format(p_value, digits = 3), "\n", significance),
    x = mean(cleaned_alb_s_data$G_Lon),  
    y = seq(max(cleaned_alb_s_data$G_Lat) - 3, max(cleaned_alb_s_data$G_Lat) - 10, length.out = n())  
  )


#####BET
bet_cog_events <- unique(cleaned_bet_data$Event)
compare_with_neutral <- function(event_name, data) {
  neutral_data <- data %>% filter(Event == "Neutral") %>% select(G_Lon, G_Lat)
  event_data <- data %>% filter(Event == event_name) %>% select(G_Lon, G_Lat)
  if (nrow(neutral_data) < 2 || nrow(event_data) < 2) {
    return(data.frame(Event = event_name, T2 = NA, p_value = NA))
  }
  result <- tryCatch(
    hotelling.test(as.matrix(neutral_data), as.matrix(event_data)),
    error = function(e) NULL 
  )
    if (is.null(result)) {
    return(data.frame(Event = event_name, T2 = NA, p_value = NA))
  }
  test_stat <- if ("statistic" %in% names(result$stats)) result$stats$statistic else NA
  p_val <- if ("pval" %in% names(result)) result$pval else NA
  data.frame(
    Event = event_name,
    T2 = test_stat,
    p_value = p_val
  )
}
bet_cog_results <- lapply(bet_cog_events[bet_cog_events != "Neutral"], compare_with_neutral, data = cleaned_bet_data)
bet_cog_results <- do.call(rbind, bet_cog_results)
print(bet_cog_results)
bet_cog_results$Event<-as.factor(bet_cog_results$Event)
event_order <- c("Strong La Niña", "Weak La Niña", "Neutral", "Weak El Niño", "Strong El Niño")
bet_cog_results$Event <- factor(bet_cog_results$Event, levels = event_order)
print(bet_cog_results)

bet_cog_results <- bet_cog_results %>%
  mutate(significance = case_when(
    p_value < 0.001 ~ "< 0.001",
    p_value < 0.01 ~ "< 0.01",
    #p_value < 0.05 ~ "*",
    TRUE ~ "ns"
  ))

bet_cog_results_label_data <- bet_cog_results %>%
  filter(!is.na(p_value)) %>%
  mutate(
    label = paste(Event, "vs Neutral\nP =", format(p_value, digits = 3), "\n", significance),
    x = mean(cleaned_bet_data$G_Lon),  
    y = seq(max(cleaned_bet_data$G_Lat) - 3, max(cleaned_bet_data$G_Lat) - 10, length.out = n())  
  )


###yft 
yft_cog_events <- unique(cleaned_yft_data$Event)

compare_with_neutral <- function(event_name, data) {
  neutral_data <- data %>% filter(Event == "Neutral") %>% select(G_Lon, G_Lat)
  event_data <- data %>% filter(Event == event_name) %>% select(G_Lon, G_Lat)
  if (nrow(neutral_data) < 2 || nrow(event_data) < 2) {
    return(data.frame(Event = event_name, T2 = NA, p_value = NA))
  }
  result <- tryCatch(
    hotelling.test(as.matrix(neutral_data), as.matrix(event_data)),
    error = function(e) NULL 
  )
    if (is.null(result)) {
    return(data.frame(Event = event_name, T2 = NA, p_value = NA))
  }
  test_stat <- if ("statistic" %in% names(result$stats)) result$stats$statistic else NA
  p_val <- if ("pval" %in% names(result)) result$pval else NA
  data.frame(
    Event = event_name,
    T2 = test_stat,
    p_value = p_val
  )
}

yft_cog_results <- lapply(yft_cog_events[yft_cog_events != "Neutral"], compare_with_neutral, data = cleaned_yft_data)
yft_cog_results <- do.call(rbind, yft_cog_results)
print(yft_cog_results)
yft_cog_results$Event<-as.factor(yft_cog_results$Event)
event_order <- c("Strong La Niña", "Weak La Niña", "Neutral", "Weak El Niño", "Strong El Niño")
yft_cog_results$Event <- factor(yft_cog_results$Event, levels = event_order)
print(yft_cog_results)

yft_cog_results <- yft_cog_results %>%
  mutate(significance = case_when(
    p_value < 0.001 ~ "< 0.001",
    p_value < 0.01 ~ "< 0.01",
    #p_value < 0.05 ~ "*",
    TRUE ~ "ns"
  ))

yft_cog_results_label_data <- yft_cog_results %>%
  filter(!is.na(p_value)) %>%
  mutate(
    label = paste(Event, "vs Neutral\nP =", format(p_value, digits = 3), "\n", significance),
    x = max(cleaned_yft_data$G_Lon) - 5,  
    y = seq(min(cleaned_yft_data$G_Lat) + 5, max(cleaned_yft_data$G_Lat) - 5, length.out = n())
  )

###cog_significance
event_order <- c("Strong La Niña", "Weak La Niña", "Neutral", "Weak El Niño", "Strong El Niño")
cleaned_alb_n_data$Event <- factor(cleaned_alb_n_data$Event, levels = event_order)
cleaned_alb_s_data$Event <- factor(cleaned_alb_s_data$Event, levels = event_order)
cleaned_bet_data$Event <- factor(cleaned_bet_data$Event, levels = event_order)
cleaned_yft_data$Event <- factor(cleaned_yft_data$Event, levels = event_order)

ggplot() +
  geom_point(cleaned_alb_n_data,mapping= aes(x = G_Lon, y = G_Lat, color = Event), size = 3, alpha = 0.5) +  
  geom_point(cleaned_alb_s_data,mapping= aes(x = G_Lon, y = G_Lat, color = Event), size = 3, alpha = 0.5) + 
  geom_point(cleaned_bet_data,mapping= aes(x = G_Lon, y = G_Lat, color = Event), size = 3, alpha = 0.5) +  
  geom_point(cleaned_yft_data,mapping= aes(x = G_Lon, y = G_Lat, color = Event), size = 3, alpha = 0.5) +  
  scale_color_manual(values = c(
    "Strong La Niña" = "#631879FF",
    "Weak La Niña" = "darkblue",
    "Neutral" = "#54C568FF",
    "Weak El Niño" = "#F7854E",
    "Strong El Niño" = "darkred"
  ), name = "Event Type") +
  stat_ellipse(cleaned_alb_n_data,
               mapping=aes(x = G_Lon, y = G_Lat,group = Event, fill = Event ,color = Event), 
               geom = "polygon", alpha = 0.2, level = 0.95, size = 0.8 
  )+
  stat_ellipse(cleaned_alb_s_data,
               mapping=aes(x = G_Lon, y = G_Lat,group = Event, fill = Event ,color = Event), 
               geom = "polygon", alpha = 0.2, level = 0.95, size = 0.8  
  )+
  stat_ellipse(cleaned_bet_data,
               mapping=aes(x = G_Lon, y = G_Lat,group = Event, fill = Event ,color = Event), 
               geom = "polygon", alpha = 0.2, level = 0.95, size = 0.8  
  )+
  stat_ellipse(cleaned_yft_data,
               mapping=aes(x = G_Lon, y = G_Lat,group = Event, fill = Event ,color = Event), 
               geom = "polygon", alpha = 0.2, level = 0.95, size = 0.8  
  ) +
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
  coord_sf(xlim = c(105, 290),  ylim = c(-55, 55))+
  theme_bw() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )+
  theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(text=element_text(family="serif"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12,angle=25,hjust=0.65))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  theme(legend.position = 'bottom')+
  scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
  scale_x_longitude(name='Longitude',breaks = seq(100,300,40))+
  theme(panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank())  






###ggplot stat_ellipse
ggplot(cleaned_alb_n_data, aes(x = G_Lon, y = G_Lat)) +
  geom_point(aes(color = Event), size = 3, alpha = 0.5) +  
  scale_color_manual(values = c(
    "Strong La Niña" = "#631879FF",
    "Weak La Niña" = "darkblue",
    "Neutral" = "#54C568FF",
    "Weak El Niño" = "#F7854E",
    "Strong El Niño" = "darkred"
  ), name = "Event Type") +
  stat_ellipse(
    aes(group = Event, fill = Event ,color = Event),  
    geom = "polygon", alpha = 0.2, level = 0.95, size = 0.8  
  ) +
  scale_fill_manual(values = c(
    "Strong La Niña" = "#631879FF",
    "Weak La Niña" = "darkblue",
    "Neutral" = "#54C568FF",
    "Weak El Niño" = "#F7854E",
    "Strong El Niño" = "darkred"
  ), name = "Event Type") +
  geom_text_repel(
    data = alb_n_cog_results_label_data,
    aes(x = x, y = y, label = label), 
    size = 5, 
    box.padding = 0.5,  
    point.padding = 0.5, 
    segment.size = 0
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Event Type",
    color = "Event Type"
  ) +
  theme_base() +
  theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12,angle=25,hjust=0.65))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=9))+
  theme(legend.text = element_text(size=12))+
  theme(legend.position = 'none')+
  scale_y_latitude(name='',breaks = seq(-40,40,4))+
  scale_x_longitude(name='',breaks = seq(100,300,4))

###alb_s
ggplot(cleaned_alb_s_data, aes(x = G_Lon, y = G_Lat)) +
  geom_point(aes(color = Event), size = 3, alpha = 0.5) + 
  scale_color_manual(values = c(
    "Strong La Niña" = "#631879FF",
    "Weak La Niña" = "darkblue",
    "Neutral" = "#54C568FF",
    "Weak El Niño" = "#F7854E",
    "Strong El Niño" = "darkred"
  ), name = "Event Type") +
  stat_ellipse(
    aes(group = Event, fill = Event ,color = Event),  
    geom = "polygon", alpha = 0.2, level = 0.95, size = 0.8  
  ) +
  scale_fill_manual(values = c(
    "Strong La Niña" = "#631879FF",
    "Weak La Niña" = "darkblue",
    "Neutral" = "#54C568FF",
    "Weak El Niño" = "#F7854E",
    "Strong El Niño" = "darkred"
  ), name = "Event Type") +
  geom_text_repel(
    data = alb_s_cog_results_label_data,
    aes(x = x, y = y, label = label), 
    size = 5, 
    box.padding = 0.5,  
    point.padding = 0.5,  
    segment.size = 0      
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Event Type",
    color = "Event Type"
  ) +
  theme_base() +
  theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(text=element_text(family="serif"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12,angle=25,hjust=0.65))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=9))+
  theme(legend.text = element_text(size=12))+
  theme(legend.position = 'none')+
  scale_y_latitude(name='',breaks = seq(-40,40,4))+
  scale_x_longitude(name='',breaks = seq(100,300,4))

#bet
bet_cog_results_label_data <- bet_cog_results_label_data %>%
  mutate(nudge_x = 1)  

ggplot(cleaned_bet_data, aes(x = G_Lon, y = G_Lat)) +
  geom_point(aes(color = Event), size = 3, alpha = 0.5) +  
  scale_color_manual(values = c(
    "Strong La Niña" = "#631879FF",
    "Weak La Niña" = "darkblue",
    "Neutral" = "#54C568FF",
    "Weak El Niño" = "#F7854E",
    "Strong El Niño" = "darkred"
  ), name = "Event Type") +
  stat_ellipse(
    aes(group = Event, fill = Event ,color = Event), 
    geom = "polygon", alpha = 0.2, level = 0.95, size = 0.8  
  ) +
  scale_fill_manual(values = c(
    "Strong La Niña" = "#631879FF",
    "Weak La Niña" = "darkblue",
    "Neutral" = "#54C568FF",
    "Weak El Niño" = "#F7854E",
    "Strong El Niño" = "darkred"
  ), name = "Event Type") +
  geom_text_repel(
    data = bet_cog_results_label_data,
    aes(x = 202, y = y, label = label, nudge_x = nudge_x),
    size = 5,
    box.padding = 0.5,
    point.padding = 0.5,
    segment.size = 0,
    hjust = 0
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Event Type",
    color = "Event Type"
  ) +
  theme_base() +
  theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(text=element_text(family="serif"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12,angle=25,hjust=0.65))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=9))+
  theme(legend.text = element_text(size=12))+
  theme(legend.position = 'none')+
  scale_y_latitude(name='',breaks = seq(-40,40,2))+
  scale_x_longitude(name='',breaks = seq(100,300,4))

#yft
ggplot(cleaned_yft_data, aes(x = G_Lon, y = G_Lat)) +
  geom_point(aes(color = Event), size = 3, alpha = 0.5) + 
  scale_color_manual(values = c(
    "Strong La Niña" = "#631879FF",
    "Weak La Niña" = "darkblue",
    "Neutral" = "#54C568FF",
    "Weak El Niño" = "#F7854E",
    "Strong El Niño" = "darkred"
  ), name = "Event Type") +
  stat_ellipse(
    aes(group = Event, fill = Event ,color = Event),  
    geom = "polygon", alpha = 0.2, level = 0.95, size = 0.8  
  ) +
  scale_fill_manual(values = c(
    "Strong La Niña" = "#631879FF",
    "Weak La Niña" = "darkblue",
    "Neutral" = "#54C568FF",
    "Weak El Niño" = "#F7854E",
    "Strong El Niño" = "darkred"
  ), name = "Event Type") +
  geom_text_repel(
    data = yft_cog_results_label_data,
    aes(x = x, y = y, label = label), 
    size = 5, 
    box.padding = 0.5, 
    point.padding = 0.5,  
    segment.size = 0      
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Event Type",
    color = "Event Type"
  ) +
  theme_base() +
  theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(text=element_text(family="serif"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12,angle=25,hjust=0.65))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=9))+
  theme(legend.text = element_text(size=12))+
  theme(legend.position = 'none')+
  scale_y_latitude(name='',breaks = seq(-40,40,2))+
  scale_x_longitude(name='',breaks = seq(100,300,4))
