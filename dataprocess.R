#data processing
#WCPFC
#packages
library(tidyverse)
library(patchwork)
###WCPFC LL PS 
#LL
wcpfc_data<-read.csv("/WCPFC/WCPFC_L_PUBLIC_BY_FLAG_MON.CSV",T)
n_columns <- grep("_n$", names(wcpfc_data), value = TRUE)
wcpfc_data$hooks<-wcpfc_data$hhooks*100
wcpfc_data<-wcpfc_data[,c('yy','mm','lon_short','lat_short','flag_id',"hhooks",
                          'alb_c','alb_n','yft_c','yft_n','bet_c','bet_n')]
#
getLonLat = function(value)
{
  flag = substr(value, nchar(value), nchar(value))
  
  if(flag == 'S')
    return(-as.numeric(substr(value, 1, nchar(value)-1)))
  if(flag == 'N')
    return(as.numeric(substr(value, 1, nchar(value)-1)))
  if(flag == 'E')
    return(as.numeric(substr(value, 1, nchar(value)-1)))
  if(flag == 'W')
  {
    a = as.numeric(substr(value, 1, nchar(value)-1))
    if(a == 180)
      return(a)
    else
      return(-a+360)
  }
}
Lon=NULL
Lat=NULL
for(i in 1:nrow(wcpfc_data))
{
  Lon[i]=getLonLat(wcpfc_data[i,'lon_short'])
  Lat[i]=getLonLat(wcpfc_data[i,'lat_short'])
}
#Lon Lat
wcpfc_data$Lon=Lon
wcpfc_data$Lat=Lat
wcpfc_data<-wcpfc_data[,c('yy','mm','Lon','Lat','flag_id','hhooks',
                          #'alb_c','yft_c','bet_c',
                          'alb_n','yft_n','bet_n')]
colnames(wcpfc_data)[1:2]<-c('Year','Month')
wcpfc_data$Lon=wcpfc_data$Lon+2.5
wcpfc_data$Lat=wcpfc_data$Lat+2.5
colnames(wcpfc_data)[5:9]<-c('Flag','hhooks',"ALB","YFT","BET")
wcpfc_data<-subset(wcpfc_data,wcpfc_data$Year>=1993 & wcpfc_data$Year<=2020)
wcpfc_data<-subset(wcpfc_data,wcpfc_data$hhooks>0)
wcpfc_data$Flag<-as.factor(wcpfc_data$Flag)
wcpfc_data$Area="WCPO"
#iattc
iattc_data<-read.csv("/data/PublicLLTunaBillfish/PublicLLTunaBillfishNum.csv",T)
n_columns <- grep("n$", names(iattc_data), value = TRUE)

# 
iattc_data <- iattc_data[-which((rowSums(iattc_data[, n_columns]) - iattc_data$Hooks) > 0), ]

iattc_data <- iattc_data %>%
  filter(Year >= 1993, Year <= 2020)%>%
  select(Year, Month,  LonC5, LatC5,Flag, Hooks, 
         ALBn, BETn,  YFTn, 
         #ALBmt, BETmt, YFTmt
  )

colnames(iattc_data)[3:4]<-c("Lon","Lat")
colnames(iattc_data)[7:9]<-c("ALB","BET","YFT")
iattc_data$Hooks=iattc_data$Hooks/100
colnames(iattc_data)[6]="hhooks"
iattc_data$Lon<-ifelse(iattc_data$Lon<0,iattc_data$Lon+360,iattc_data$Lon)
iattc_data$Flag<-as.factor(iattc_data$Flag)
iattc_data<-subset(iattc_data,iattc_data$hhooks>0)
iattc_data$Area="EPO"
# 
code_map <- data.frame(
  WCPFC = c("AU", "BZ", "CK", "CN", "ES", "FJ", "FM", "ID", "JP", "KI", "KR", "MH", "NC", "NU", "NZ", "PF", "PG", "PH", "PT", "PW", "SB", "TO", "TV", "TW", "US", "VN", "VU", "WS"),
  IATTC = c("AUS", "BLZ", "COK", "CHN", "ESP", "FJI", "FSM", "IDN", "JPN", "KIR", "KOR", "MHL", "NCL", "NIU", "NZL", "PYF", "PNG", "PHL", "PRT", "PLW", "SLB", "TON", "TUV", "TWN", "USA", "VNM", "VUT", "WSM")
)

library(dplyr)

#
pacific_tuna_ll_flag_data<-rbind(wcpfc_data,iattc_data)
pacific_tuna_ll_flag_data <- pacific_tuna_ll_flag_data %>%
  left_join(code_map, by = c("Flag" = "WCPFC")) %>%
  mutate(Flag = ifelse(is.na(IATTC), Flag, IATTC)) %>%
  select(-IATTC)
pacific_tuna_ll_flag_data$Flag<-as.factor(pacific_tuna_ll_flag_data$Flag)
pacific_tuna_ll_data<-pacific_tuna_ll_flag_data%>%
  group_by(Year,Month,Lon,Lat,Area)%>%
  summarise(ALB=sum(ALB),
            BET=sum(BET),
            YFT=sum(YFT),
            hhooks=sum(hhooks))
pacific_tuna_ll_data$nCPUE_ALB=pacific_tuna_ll_data$ALB/pacific_tuna_ll_data$hhooks
pacific_tuna_ll_data$nCPUE_BET=pacific_tuna_ll_data$BET/pacific_tuna_ll_data$hhooks
pacific_tuna_ll_data$nCPUE_YFT=pacific_tuna_ll_data$YFT/pacific_tuna_ll_data$hhooks

# 
lldata_wcpo <- subset(pacific_tuna_ll_data, Area == "WCPO")
lldata_epo <- subset(pacific_tuna_ll_data, Area == "EPO")

# Lon和Lat
pacific_overlap <- inner_join(lldata_wcpo, lldata_epo, by = c("Year","Month","Lon", "Lat"))
pacific_overlap<-pacific_overlap[,-c(5:12)]
colnames(pacific_overlap)<-c("Year","Month","Lon","Lat","Area","ALB","BET","YFT","hhooks","nCPUE_ALB","nCPUE_BET","nCPUE_YFT")
# 
pacific_tuna_ll_data <- anti_join(pacific_tuna_ll_data, pacific_overlap, by = c("Year","Month","Lon", "Lat","Area"))
save(pacific_tuna_ll_data,file="/RData/pacific_tuna_ll_data.RData")
##
pacific_env_data<-pacific_env_data[,-c(10:15,22:27,31:34)]
pacific_env_data<-na.omit(pacific_env_data)
pacific_env_data[pacific_env_data == 0] <- NA
pacific_tuna_model_data<-left_join(pacific_env_data,pacific_tuna_ll_data, by = c("Year", "Month", "Lon", "Lat"))
pacific_tuna_model_data$ALB[is.na(pacific_tuna_model_data$ALB)] <- 0
pacific_tuna_model_data$BET[is.na(pacific_tuna_model_data$BET)] <- 0
pacific_tuna_model_data$YFT[is.na(pacific_tuna_model_data$YFT)] <- 0
pacific_tuna_model_data$hhooks[is.na(pacific_tuna_model_data$hhooks)] <- 0
pacific_tuna_model_data$nCPUE_ALB[is.na(pacific_tuna_model_data$nCPUE_ALB)] <- 0
pacific_tuna_model_data$nCPUE_BET[is.na(pacific_tuna_model_data$nCPUE_BET)] <- 0
pacific_tuna_model_data$nCPUE_YFT[is.na(pacific_tuna_model_data$nCPUE_YFT)] <- 0


pacific_tuna_model_data <- as.data.frame(pacific_tuna_model_data)
pacific_tuna_model_data<-pacific_tuna_model_data[,c("nCPUE_ALB","nCPUE_BET","nCPUE_YFT","ALB","BET","YFT","hhooks",
                                                    "Year","Month","Lon","Lat",
                                                    "Temp_0","Temp_47","Temp_97","Temp_147","Temp_200","Temp_245","Temp_300",
                                                    "Sali_0","Sali_47","Sali_97","Sali_147","Sali_200","Sali_245","Sali_300",
                                                    "SSH","MLD",
                                                    "Chl_0","Chl_47","Chl_97","Chl_147","Chl_200","Chl_245","Chl_300",
                                                    "O2_0","O2_47","O2_97","O2_147","O2_200","O2_245","O2_300",
                                                    "Nppv_0","Nppv_47","Nppv_97","Nppv_147",#"Nppv_200","Nppv_245","Nppv_300",
                                                    "MEI"
)]




library(httr)
library(rvest)
#https://ds.data.jma.go.jp/tcc/tcc/products/elnino/index/sstindex/base_period_9120/Nino_4/anomaly
#https://ds.data.jma.go.jp/tcc/tcc/products/elnino/index/sstindex/base_period_9120/Nino_3/anomaly
#https://psl.noaa.gov/data/correlation/nina34.data
#https://www.cpc.ncep.noaa.gov/data/indices/soi
#https://www.psl.noaa.gov/enso/mei/data/meiv2.data
url <- "https://www.psl.noaa.gov/enso/mei/data/meiv2.data"
response <- GET(url)
webpage <- content(response, "text", encoding = "UTF-8")
lines <- unlist(strsplit(webpage, "\n"))
header <- unlist(strsplit(lines[1], "\\s+"))

data_lines <- lines[-c(1)]  # 
data_list <- lapply(data_lines, function(line) unlist(strsplit(line, "\\s+")))
data_frame <- setNames(data.frame(do.call(rbind, data_list), stringsAsFactors = FALSE), header)
colnames(data_frame)[1]="Year"
colnames(data_frame)[2:13]<-c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
data_frame<-data_frame[,1:13]
mei_data <- pivot_longer(data_frame, cols = -Year, names_to = "Month", values_to = "MEI")
nino_data <- pivot_longer(data_frame, cols = -Year, names_to = "Month", values_to = "Nino4")
soi_data <- pivot_longer(data_frame, cols = -Year, names_to = "Month", values_to = "SOI")

# 
months_map <- c(JAN = 1, FEB = 2, MAR = 3, APR = 4, MAY = 5, JUN = 6, 
                JUL = 7, AUG = 8, SEP = 9, OCT = 10, NOV = 11, DEC = 12)
# 
mei_data <- mei_data %>%
  mutate(Month = months_map[Month])
mei_data<-subset(mei_data,mei_data$Year>=1993 & mei_data$Year<=2020)
mei_data$Year<-as.numeric(mei_data$Year)
mei_data$MEI<-as.numeric(mei_data$MEI)

nino_4<-subset(nino_data,nino_data$Year>=2012 & nino_data$Year<=2020)
#write.csv(nino_data,"/data/nino3.csv",row.names = F)






#enso_envdata plot
library(DataExplorer)

pacific_env_data_model<-pacific_tuna_model_data[,-c(1:11)]
#pacific_env_data_model_long$variable <- factor(pacific_env_data_model_long$variable, levels = variable_order)
#plot_intro(pacific_env_data_model)
#plot_missing(pacific_env_data_model)
#plot_histogram(pacific_env_data_model,ncol=9,ggtheme=theme_base())

plot_density(pacific_env_data_model,ncol=7,nrow=5,geom_density_args = list("fill" = "black", "alpha" = 0.6),ggtheme=theme_classic())

ggsave(filename = "/Fig/Fig S2.png",width=16,height = 9,dpi=600)
topptx(filename = "/Fig/Fig S2.pptx",width=16,height = 9)

#plot_correlation(pacific_tuna_model_data)

mei_data$Date<-as.Date(paste(mei_data$Year, mei_data$Month, "01", sep="-"))

ggplot(data=mei_data,mapping=aes(x=Date,y=MEI))+
  geom_path(linewidth=0.8)+
  geom_ribbon(aes(ymin = 1.5, ymax = ifelse(MEI > 1.5, MEI, 1.5)),
              fill = "darkred", alpha = 0.6) + 
  geom_ribbon(aes(ymin = -1.5, ymax = ifelse(MEI < -1.5, MEI, -1.5)),
              fill = "darkblue", alpha = 0.6) + 
  geom_ribbon(aes(ymin = 0.5, ymax = ifelse(MEI > 0.5, MEI, 0.5)),
              fill = "red", alpha = 0.2) + 
  geom_ribbon(aes(ymin = -0.5, ymax = ifelse(MEI < -0.5, MEI, -0.5)),
              fill = "blue", alpha = 0.2) + 
  #  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed",color="red",alpha=0.3) +
  geom_hline(yintercept = -0.5, linetype = "dashed",color="blue",alpha=0.3) +
  geom_hline(yintercept = 1.5,linetype="dashed",color="darkred")+
  geom_hline(yintercept = -1.5,linetype="dashed",color="darkblue")+
  theme_test()+
  labs(x="Time series")+
  #theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  theme(legend.position = 'bottom')+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",expand = c(0,0))

ggsave(filename = "/Fig/Fig S3.png",width=14,height = 6,dpi=600)
topptx(filename = "/Fig/Fig S3.pptx",width=14,height = 6)


theme_mine<-
  #theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(text=element_text(family="serif"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  theme(legend.position = 'bottom')





###Fishery data plot
#packages
library(maps)
library(ggspatial)
library(ggplot2)
library(tidyverse)
library(eoffice) 
library(cowplot)
library(ggsci)
library(metR)
library(ggthemes)
library(grDevices) 
library(RColorBrewer)
library(patchwork)

#map
worldMap <- fortify(map_data("world2"), region = "subregion")

#colormap
display.brewer.all(type='seq')
colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(106)
colormap_alb_n <- colorRampPalette((brewer.pal(9,'Purples')))(106)
colormap_alb_s <- colorRampPalette((brewer.pal(9,'Blues')))(106)
colormap_bet <- colorRampPalette((brewer.pal(9,'Greens')))(106)
colormap_yft <- colorRampPalette((brewer.pal(9,'Oranges')))(106)
#data filt
pacific_tuna_model_data<-na.omit(pacific_tuna_model_data)

#Average distribution
pacific_tuna_ave_dis<-pacific_tuna_model_data%>%
  group_by(Lon,Lat)%>%
  summarise(nCPUE_ALB=mean(nCPUE_ALB),
            nCPUE_BET=mean(nCPUE_BET),
            nCPUE_YFT=mean(nCPUE_YFT))

northern_alb_ave_dis<-northern_alb_model_data%>%
  group_by(Lon,Lat)%>%
  summarise(nCPUE_ALB=mean(nCPUE_ALB))


southern_alb_ave_dis<-southern_alb_model_data%>%
  group_by(Lon,Lat)%>%
  summarise(nCPUE_ALB=mean(nCPUE_ALB))

alb_p1<-ggplot() + 
  geom_tile(data=northern_alb_ave_dis,aes(x=Lon,y=Lat,fill=nCPUE_ALB))+
  scale_fill_gradientn(colors=colormap_alb_n)+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey60',color='grey60',size=0.5)+
  coord_sf(xlim = c(100, 290),  ylim = c(0,55))+
  theme_bw()+
  theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12,angle=30,hjust=0.65))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  #theme(legend.position = 'bottom')+
  scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
  scale_x_longitude(name='Longitude',breaks = seq(100,300,20))+
  labs(fill='ALB_N \n(the normalized CPUE)')


alb_p2<-ggplot() + 
  geom_tile(data=southern_alb_ave_dis,aes(x=Lon,y=Lat,fill=nCPUE_ALB))+
  scale_fill_gradientn(colors=colormap_alb_s)+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey60',color='grey60',size=0.5)+
  coord_sf(xlim = c(100, 290),  ylim = c(-55, 0))+
  theme_bw()+
  theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12,angle=30,hjust=0.65))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  #theme(legend.position = 'bottom')+
  scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
  scale_x_longitude(name='Longitude',breaks = seq(100,300,20))+
  labs(fill='ALB_S \n(the normalized CPUE)')

###plot
#ALB CPUE distribution 
#alb_p1<-
ggplot() + 
  geom_tile(data=pacific_tuna_ave_dis,aes(x=Lon,y=Lat,fill=nCPUE_ALB))+
  scale_fill_gradientn(colors=colormap_alb)+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey60',color='grey60',size=0.5)+
  coord_sf(xlim = c(100, 290),  ylim = c(-55, 55))+
  theme_bw()+
  theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12,angle=30,hjust=0.65))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  #theme(legend.position = 'bottom')+
  scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
  scale_x_longitude(name='Longitude',breaks = seq(100,300,20))+
  labs(fill='ALB \n(the normalized CPUE)')
alb_p1

#BET CPUE distribution 
bet_p1<-ggplot() + 
  geom_tile(data=pacific_tuna_ave_dis,aes(x=Lon,y=Lat,fill=nCPUE_BET))+
  scale_fill_gradientn(colors=colormap_bet)+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey60',color='grey60',size=0.5)+
  coord_sf(xlim = c(100, 290),  ylim = c(-55, 55))+
  theme_bw()+
  theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12,angle=30,hjust=0.65))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  #theme(legend.position = 'bottom')+
  scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
  scale_x_longitude(name='Longitude',breaks = seq(100,300,20))+
  labs(fill='BET \n(the normalized CPUE)')
bet_p1

#YFT CPUE distribution 
yft_p1<-ggplot() + 
  geom_tile(data=pacific_tuna_ave_dis,aes(x=Lon,y=Lat,fill=nCPUE_YFT))+
  scale_fill_gradientn(colors=colormap_yft)+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey60',color='grey60',size=0.5)+
  coord_sf(xlim = c(100, 290),  ylim = c(-55, 55))+
  theme_bw()+
  theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12,angle=30,hjust=0.65))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  #theme(legend.position = 'bottom')+
  scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
  scale_x_longitude(name='Longitude',breaks = seq(100,300,20))+
  labs(fill='YFT \n(the normalized CPUE)')
yft_p1

alb_p1/alb_p2/bet_p1/yft_p1+plot_annotation(tag_levels = 'a', tag_prefix = '(',tag_suffix = ")")

#Average Timeseries
pacific_tuna_ave_ts<-pacific_tuna_model_data%>%
  group_by(Year,Month)%>%
  summarise(nCPUE_ALB=mean(nCPUE_ALB),
            nCPUE_BET=mean(nCPUE_BET),
            nCPUE_YFT=mean(nCPUE_YFT))

pacific_tuna_ave_ts$Date<-as.Date(paste(pacific_tuna_ave_ts$Year, pacific_tuna_ave_ts$Month, "01", sep="-"))

northern_alb_ave_ts<-northern_alb_model_data%>%
  group_by(Year,Month)%>%
  summarise(nCPUE_ALB=mean(nCPUE_ALB))

northern_alb_ave_ts$Date<-as.Date(paste(northern_alb_ave_ts$Year, northern_alb_ave_ts$Month, "01", sep="-"))

southern_alb_ave_ts<-southern_alb_model_data%>%
  group_by(Year,Month)%>%
  summarise(nCPUE_ALB=mean(nCPUE_ALB))

southern_alb_ave_ts$Date<-as.Date(paste(southern_alb_ave_ts$Year, southern_alb_ave_ts$Month, "01", sep="-"))

ggplot() + 
  geom_line(data=southern_alb_ave_ts,aes(x=Date,y=nCPUE_ALB),linewidth=0.7,alpha=0.8,color="darkblue")+
  theme_bw()+
  #theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(text=element_text(family="serif"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  theme(legend.position = 'bottom')+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",expand = c(0,0))

#ALB ts
alb_p3<-ggplot() + 
  geom_line(data=northern_alb_ave_ts,aes(x=Date,y=nCPUE_ALB),linewidth=1.2,alpha=0.8,color="mediumpurple4")+
  theme_test()+
  #theme(strip.background = element_rect(fill = "white", colour = "black")) +
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
  labs(y="nCPUE_ALB_N")+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",expand = c(0,0))


alb_p4<-ggplot() + 
  geom_line(data=southern_alb_ave_ts,aes(x=Date,y=nCPUE_ALB),linewidth=1.2,alpha=0.8,color="darkblue")+
  theme_test()+
  #theme(strip.background = element_rect(fill = "white", colour = "black")) +
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
  theme(legend.position = 'bottom')+
  labs(y="nCPUE_ALB_S")+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",expand = c(0,0)) 

#alb_p2<-
ggplot() + 
  geom_line(data=pacific_tuna_ave_ts,aes(x=Date,y=nCPUE_ALB),linewidth=1.2,alpha=0.8,color="darkblue")+
  theme_test()+
  #theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(text=element_text(family="serif"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_text(size=14))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  theme(legend.position = 'bottom')+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",expand = c(0,0)) 

alb_p2

#BET ts
bet_p2<-ggplot() + 
  geom_line(data=pacific_tuna_ave_ts,aes(x=Date,y=nCPUE_BET),linewidth=1.2,alpha=0.8,color="darkgreen")+
  theme_test()+
  #theme(strip.background = element_rect(fill = "white", colour = "black")) +
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
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",expand = c(0,0)) 
bet_p2

#YFT ts
yft_p2<-ggplot() + 
  geom_line(data=pacific_tuna_ave_ts,aes(x=Date,y=nCPUE_YFT),linewidth=1.2,alpha=0.8,color="darkorange2")+
  theme_test()+
  #theme(strip.background = element_rect(fill = "white", colour = "black")) +
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
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",expand = c(0,0)) 
yft_p2

alb_p3/alb_p4/bet_p2/yft_p2+plot_annotation(tag_levels = 'a', tag_prefix = '(',tag_suffix = ")")

(alb_p1 | alb_p3) / 
  (alb_p2 | alb_p4) / 
  (bet_p1 | bet_p2) / 
  (yft_p1 | yft_p2) +
  plot_layout(heights = c(1, 1, 2,2), widths = c(4, 1))+plot_annotation(tag_levels = 'a', tag_prefix = '(',tag_suffix = ")",    theme = theme(plot.tag = element_text(size = 14,face="bold")))

#ggsave(filename = "/Users/linbojun/Library/CloudStorage/OneDrive-共用文件庫－onedrive/Oceanic Fisheries Ecosystem Laboratory/ENSO_LL/Figure/tunacpue.png",width=16,height = 9,dpi=600)
ggsave(filename = "/Fig/png/Fig S1.png",width=16,height = 12,dpi=600)
topptx(filename = "Fig S1.pptx",width=16,height=12)
