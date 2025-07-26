#model plot 
library(SHAPforxgboost)
library(mlr3)
library(mlr3tuning)
library(mlr3learners)
library(paradox)
library(ggplot2)
library(mlr3viz)
library(Metrics)
library(xgboost)
library(shapviz)
library(patchwork)
library(ggpubr)
library(eoffice)
library(ggsci)
library(dplyr)
library(metR)

rfecv_alb_northern<-read.csv("/py/cv_result/rfecv_cv_results_alb_northern.csv",T)
rfecv1_alb_northern<-read.csv("/py/cv_result/rfecv1_cv_results_alb_northern.csv",T)
rfecv2_alb_northern<-read.csv("/py/cv_result/rfecv2_cv_results_alb_northern.csv",T)

rfecv_alb_southern<-read.csv("/py/cv_result/rfecv_cv_results_alb_southern.csv",T)
rfecv1_alb_southern<-read.csv("/py/cv_result/rfecv1_cv_results_alb_southern.csv",T)
rfecv2_alb_southern<-read.csv("/py/cv_result/rfecv2_cv_results_alb_southern.csv",T)

#
rfecv_plot1_northern<-
  ggplot(rfecv_alb_northern, aes(x = Step, y = Mean.Score)) +
  geom_line(color="#631879FF",linewidth=1.2) +
  geom_point()+
  geom_point(data = rfecv_alb_northern[which.max(rfecv_alb_northern$Mean.Score), ], aes(x = Step, y = Mean.Score),
             colour = "red", size = 3) +
  xlab("") +  
  ylab("") +
  ggtitle("(a) North ALB RFECV Performance \nCircle 1") +
  theme_bw()+
  scale_x_continuous(breaks = seq(1,35,4),expand=c(0,0))+
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
  theme(panel.grid.major = element_blank(),
panel.grid.minor = element_blank()) 

rfecv_plot2_northern<-
  ggplot(rfecv1_alb_northern, aes(x = Step, y = Mean.Score)) +
  geom_line(color="#631879FF",linewidth=1.2) +
  geom_point()+
  geom_point(data = rfecv1_alb_northern[which.max(rfecv1_alb_northern$Mean.Score), ], aes(x = Step, y = Mean.Score),
             colour = "red", size = 3) +
  xlab("") + 
  ylab("(NB: negative mean squared error)") + 
  ggtitle("(b) Circle 2") +
  theme_bw()+
  scale_x_continuous(breaks = seq(1,35,2),expand=c(0,0))+
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
theme(panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()) 
rfecv_plot2_northern

rfecv_plot3_northern<-
  ggplot(rfecv2_alb_northern, aes(x = Step, y = Mean.Score)) +
  geom_line(color="#631879FF",linewidth=1.2) + 
  geom_point()+
  geom_point(data = rfecv2_alb_northern[which.max(rfecv2_alb_northern$Mean.Score), ], aes(x = Step, y = Mean.Score),
             colour = "red", size = 3) +
  xlab("") +  
  ylab("") +  
  ggtitle("(c) Circle 3") +
  theme_bw()+
  scale_x_continuous(breaks = seq(1,35,2),expand=c(0,0))+
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
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
rfecv_plot3_northern

rfecv_plot4_northern<-
  ggplot(rfecv2_alb_northern, aes(x = Step, y = Mean.Score)) +
  geom_line(color="#631879FF",linewidth=1.2) +  
  geom_point()+
  geom_point(data = rfecv2_alb_northern[which.max(rfecv2_alb_northern$Mean.Score), ], aes(x = Step, y = Mean.Score),
             colour = "red", size = 3) +
  xlab("Number of Features Selected") +  
  ylab("") +  
  ggtitle("(d) Circle 4") +
  theme_bw()+
  scale_x_continuous(breaks = seq(1,35,2),expand=c(0,0))+
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
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 
rfecv_plot4_northern

rfecv_plot1_northern/rfecv_plot2_northern/rfecv_plot3_northern/rfecv_plot4_northern

#
#rfecv_alb_southern
rfecv_plot1_southern<-
  ggplot(rfecv_alb_southern, aes(x = Step, y = Mean.Score)) +
  geom_line(color="darkblue",linewidth=1.2) +  
  geom_point()+
  geom_point(data = rfecv_alb_southern[which.max(rfecv_alb_southern$Mean.Score), ], aes(x = Step, y = Mean.Score),
             colour = "red", size = 3) +
  xlab("") +  
  ylab("") +  
  ggtitle("(e) South ALB RFECV Performance \nCircle 1") +
  theme_bw()+
  scale_x_continuous(breaks = seq(1,35,4),expand=c(0,0))+
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
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

rfecv_plot1_southern

#
rfecv_plot2_southern<-
  ggplot(rfecv2_alb_southern, aes(x = Step, y = Mean.Score)) +
  geom_line(color="darkblue",linewidth=1.2) +  
  geom_point()+
  geom_point(data = rfecv2_alb_southern[which.max(rfecv2_alb_southern$Mean.Score), ], aes(x = Step, y = Mean.Score),
             colour = "red", size = 3) +
  xlab("") +  
  ylab("") +  
  ggtitle("(f) Circle 2") +
  theme_bw()+
  scale_x_continuous(breaks = seq(1,35,2),expand=c(0,0))+
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
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

rfecv_plot2_southern


rfecv_plot3_southern<-
  ggplot(rfecv2_alb_southern, aes(x = Step, y = Mean.Score)) +
  geom_line(color="darkblue",linewidth=1.2) +  
  geom_point()+
  geom_point(data = rfecv2_alb_southern[which.max(rfecv2_alb_southern$Mean.Score), ], aes(x = Step, y = Mean.Score),
             colour = "red", size = 3) +
  xlab("Number of Features Selected") +  
  ylab("") + 
  ggtitle("(g) Circle 3") +
  theme_bw()+
  scale_x_continuous(breaks = seq(1,35,2),expand=c(0,0))+
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
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

rfecv_plot3_southern

###bet

rfecv_bet<-read.csv("/py/cv_result/rfecv_cv_results_bet.csv",T)
rfecv1_bet<-read.csv("/py/cv_result/rfecv1_cv_results_bet.csv",T)
rfecv2_bet<-read.csv("/py/cv_result/rfecv2_cv_results_bet.csv",T)

#rfecv_bet
rfecv_plot4<-
  ggplot(rfecv_bet, aes(x = Step, y = Mean.Score)) +
  geom_line(color="darkgreen",linewidth=1.2) + 
  geom_point()+
  geom_point(data = rfecv_bet[which.max(rfecv_bet$Mean.Score), ], aes(x = Step, y = Mean.Score),
             colour = "red", size = 3) +
  xlab("") +  
  ylab("") +  
  ggtitle("(h) BET RFECV Performance \nCircle 1") +
  theme_bw()+
  scale_x_continuous(breaks = seq(1,35,4),expand=c(0,0))+
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
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

rfecv_plot4

#rfecv1_bet
rfecv_plot5<-
  ggplot(rfecv1_bet, aes(x = Step, y = Mean.Score)) +
  geom_line(color="darkgreen",linewidth=1.2) +  
  geom_point()+
  geom_point(data = rfecv1_bet[which.max(rfecv1_bet$Mean.Score), ], aes(x = Step, y = Mean.Score),
             colour = "red", size = 3) +
  xlab("") +  
  ylab("") +  
  ggtitle("(i) Circle 2") +
  theme_bw()+
  scale_x_continuous(breaks = seq(1,35,2),expand=c(0,0))+
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
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

rfecv_plot5

#rfecv2_bet
rfecv_plot6<-
  ggplot(rfecv2_bet, aes(x = Step, y = Mean.Score)) +
  geom_line(color="darkgreen",linewidth=1.2) +  
  geom_point()+
  geom_point(data = rfecv2_bet[which.max(rfecv2_bet$Mean.Score), ], aes(x = Step, y = Mean.Score),
             colour = "red", size = 3) +
  xlab("") +  
  ylab("") + 
  ggtitle("(j) Circle 3") +
  theme_bw()+
  scale_x_continuous(breaks = seq(1,35,2),expand=c(0,0))+
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
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

rfecv_plot6

#rfecv2_bet
rfecv_plot7<-
  ggplot(rfecv2_bet, aes(x = Step, y = Mean.Score)) +
  geom_line(color="darkgreen",linewidth=1.2) +  
  geom_point()+
  geom_point(data = rfecv2_bet[which.max(rfecv2_bet$Mean.Score), ], aes(x = Step, y = Mean.Score),
             colour = "red", size = 3) +
  xlab("Number of Features Selected") + 
  ylab("") +  
  ggtitle("(k) Circle 4") +
  theme_bw()+
  scale_x_continuous(breaks = seq(1,35,2),expand=c(0,0),limits = c(1,10))+
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
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

rfecv_plot7

rfecv_plot4/rfecv_plot5/rfecv_plot6/rfecv_plot7



#yft
rfecv_yft<-read.csv("/py/cv_result/rfecv_cv_results_yft.csv",T)
rfecv2_yft<-read.csv("/py/cv_result/rfecv2_cv_results_yft.csv",T)
rfecv_plot8<-
  ggplot(rfecv_yft, aes(x = Step, y = Mean.Score)) +
  geom_line(color="darkorange",linewidth=1.2) +  
  geom_point()+
  geom_point(data = rfecv_yft[which.max(rfecv_yft$Mean.Score), ], aes(x = Step, y = Mean.Score),
             colour = "red", size = 3) +
  xlab("") + 
  ylab("") + 
  ggtitle("(l) YFT RFECV Performance \nCircle 1") +
  theme_bw()+
  scale_x_continuous(breaks = seq(1,35,4),expand=c(0,0))+
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
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

rfecv_plot8
#rfecv2_yft
rfecv_plot9<-
  ggplot(rfecv2_yft, aes(x = Step, y = Mean.Score)) +
  geom_line(color="darkorange",linewidth=1.2) + 
  geom_point()+
  geom_point(data = rfecv2_yft[which.max(rfecv2_yft$Mean.Score), ], aes(x = Step, y = Mean.Score),
             colour = "red", size = 3) +
  xlab("") +  
  ylab("") +  
  ggtitle("(m) Circle 2") +
  theme_bw()+
  scale_x_continuous(breaks = seq(1,35,2),expand=c(0,0))+
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
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 
rfecv_plot9

#rfecv2_yft
rfecv_plot10<-
  ggplot(rfecv2_yft, aes(x = Step, y = Mean.Score)) +
  geom_line(color="darkorange",linewidth=1.2) +  
  geom_point()+
  geom_point(data = rfecv2_yft[which.max(rfecv2_yft$Mean.Score), ], aes(x = Step, y = Mean.Score),
             colour = "red", size = 3) +
  xlab("Number of Features Selected") +  
  ylab("") +  
  ggtitle("(n) Circle 3") +
  theme_bw()+
  scale_x_continuous(breaks = seq(1,35,2),expand=c(0,0))+
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
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
rfecv_plot8/rfecv_plot9/rfecv_plot10

plots <- list(rfecv_plot1_northern, rfecv_plot2_northern, rfecv_plot3_northern, rfecv_plot4_northern,
              rfecv_plot1_southern, rfecv_plot2_southern, rfecv_plot3_southern, 
              rfecv_plot4, rfecv_plot5, rfecv_plot6, rfecv_plot7, 
              rfecv_plot8, rfecv_plot9, rfecv_plot10)
rfecv_combined_plot1 <- (rfecv_plot1_northern / rfecv_plot2_northern / rfecv_plot3_northern / rfecv_plot4_northern ) 
rfecv_combined_plot1 
rfecv_combined_plot11 <- (rfecv_plot1_southern / rfecv_plot2_southern / rfecv_plot3_southern / rfecv_plot3_southern ) 
rfecv_combined_plot11 
rfecv_combined_plot2 <- (rfecv_plot4 / rfecv_plot5 / rfecv_plot6 / rfecv_plot7) 
rfecv_combined_plot2
rfecv_combined_plot1 | rfecv_combined_plot2
rfecv_combined_plot3 <- (rfecv_plot8 / rfecv_plot9 / rfecv_plot10 / rfecv_plot10) 
rfecv_combined_plot3
(rfecv_combined_plot1 | rfecv_combined_plot11 | rfecv_combined_plot2 | rfecv_combined_plot3)
topptx(filename = "/Fig/Fig S4",width=16,height = 9)

###########################################======================================
load("/instance_alb_northern.RData")
load("/instance_alb_southern.RData")
load("/instance_bet.RData")
load("/instance_yft.RData")
autoplot_p1<-autoplot(instance_alb_northern,theme = theme_bw())
autoplot_p2<-autoplot(instance_alb_southern,theme = theme_bw())
autoplot_p3<-autoplot(instance_bet,theme = theme_bw())
autoplot_p4<-autoplot(instance_yft,theme = theme_bw())


###Fig S5 
autoplot_p1
topptx(filename = "/Fig/instance_alb_n.pptx",width=8,height = 4)
autoplot_p2
topptx(filename = "/Fig/instance_alb_s.pptx",width=8,height = 4)
autoplot_p3
topptx(filename = "/Fig/instance_bet.pptx",width=8,height = 4)
autoplot_p4
topptx(filename = "/Fig/instance_yft.pptx",width=8,height = 4)

ggarrange(autoplot_p1, autoplot_p2, autoplot_p3, autoplot_p4,
          ncol = 1, nrow = 4)






###########
load("/northern_ALB_xgb_model.RData")
load("/southern_ALB_xgb_model.RData")
load("/pacific_BET_xgb_model.RData")
load("/pacific_YFT_xgb_model.RData")

northern_ALB_xgb_model_RMSE <- as.data.frame(northern_ALB_xgb_model$evaluation_log)
northern_ALB_xgb_model_RMSE_plot<-
  ggplot(data = northern_ALB_xgb_model_RMSE, aes(x = iter)) +
  geom_line(aes(y = train_rmse,color="train_rmse"),linewidth=1.2) +
  geom_line(aes(y = eval_rmse,color="eval_rmse"),linewidth=1.2) +
  annotate("text", x = Inf, y = Inf, label = "ALB_N", hjust = 1.5, vjust = 1.5, size = 4.5, color = "black") + # 添加左上角的文本
  labs(y = "RMSE",color="",x="iteration")+
  theme_bw()+
  scale_color_aaas()+
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
  theme(legend.position = 'bottom')
northern_ALB_xgb_model_RMSE_plot

####
southern_ALB_xgb_model_RMSE <- as.data.frame(southern_ALB_xgb_model$evaluation_log)
southern_ALB_xgb_model_RMSE_plot<-
  ggplot(data = southern_ALB_xgb_model_RMSE, aes(x = iter)) +
  geom_line(aes(y = train_rmse,color="train_rmse"),linewidth=1.2) +
  geom_line(aes(y = eval_rmse,color="eval_rmse"),linewidth=1.2) +
  annotate("text", x = Inf, y = Inf, label = "ALB_S", hjust = 1.5, vjust = 1.5, size = 4.5, color = "black") + # 添加左上角的文本
  labs(y = "RMSE",color="",x="iteration")+
  theme_bw()+
  scale_color_aaas()+
  #theme(strip.background = element_rect(fill = "white", colour = "black")) +
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
  theme(legend.position = 'bottom')
southern_ALB_xgb_model_RMSE_plot

#######
pacific_BET_xgb_model_RMSE <- as.data.frame(pacific_BET_xgb_model$evaluation_log)
pacific_BET_xgb_model_RMSE_plot<-
  ggplot(data = pacific_BET_xgb_model_RMSE, aes(x = iter)) +
  geom_line(aes(y = train_rmse,color="train_rmse"),linewidth=1.2) +
  geom_line(aes(y = eval_rmse,color="eval_rmse"),linewidth=1.2) +
  annotate("text", x = Inf, y = Inf, label = "BET", hjust = 1.5, vjust = 1.5, size = 4.5, color = "black") + # 添加左上角的文本
  labs(y = "RMSE",color="",x="iteration")+
  theme_bw()+
  scale_color_aaas()+
  #theme(strip.background = element_rect(fill = "white", colour = "black")) +
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
  theme(legend.position = 'bottom')

pacific_BET_xgb_model_RMSE_plot

#######
pacific_YFT_xgb_model_RMSE <- as.data.frame(pacific_YFT_xgb_model$evaluation_log)
pacific_YFT_xgb_model_RMSE_plot<-
  ggplot(data = pacific_YFT_xgb_model_RMSE, aes(x = iter)) +
  geom_line(aes(y = train_rmse,color="train_rmse"),linewidth=1.2) +
  geom_line(aes(y = eval_rmse,color="eval_rmse"),linewidth=1.2) +
  annotate("text", x = Inf, y = Inf, label = "YFT", hjust = 1.5, vjust = 1.5, size = 4.5, color = "black") + # 添加左上角的文本
  labs(y = "RMSE",color="",x="iteration")+
  theme_bw()+
  scale_color_aaas()+
  #theme(strip.background = element_rect(fill = "white", colour = "black")) +
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
  theme(legend.position = 'bottom')
pacific_YFT_xgb_model_RMSE_plot

northern_ALB_xgb_model_RMSE_plot+southern_ALB_xgb_model_RMSE_plot+pacific_BET_xgb_model_RMSE_plot+pacific_YFT_xgb_model_RMSE_plot+plot_annotation(tag_levels = 'a', tag_prefix = '(',tag_suffix = ")")
ggsave(filename = "/Fig/png/Fig S6.png",width=8,height = 6,dpi=600)
topptx(filename = "/Fig/pptx/Fig S6.pptx",width=8,height = 6)

#residuals
load("/northern_alb_model_data.RData")
load("/southern_alb_model_data.RData")
load("/pacific_bet_model_data.RData")
load("/pacific_yft_model_data.RData")


worldMap <- fortify(map_data("world2"), region = "subregion")

blue_red_palette <- colorspace::diverging_hcl(n = 100, palette = "Blue-Red 3")

ALB_N = bind_rows((northern_alb_model_data %>% select("Year", 'Month', 'Lon', 'Lat',"MEI", 'nCPUE_ALB', 'pre_ALB')),)
ALB_N<-ALB_N%>%
  mutate(Res=pre_ALB-nCPUE_ALB)
ALB_N <- ALB_N %>%
  mutate(Time = as.Date(paste(Year, Month, "1", sep = "-")))

####plot####
pALBNYearly = ggplot(ALB_N %>% group_by(Year, Lon, Lat) %>% summarise(Res = mean(Res)))+
  geom_raster(aes(x=Lon, y=Lat, fill=Res))+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey60',color='grey60',size=0.5)+
  coord_sf(xlim = c(100, 290),  ylim = c(0, 55))+
  scale_fill_gradientn(colors = blue_red_palette,
                       limits = c(-0.4, 0.4),  
                       oob = scales::squish )+
  facet_wrap(vars(Year), ncol = 7)+
  labs(fill='Spatial Residuals')+
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
  theme(legend.position = 'bottom')+
  scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
  scale_x_longitude(name='Longitude',breaks = seq(100,300,40))
pALBNYearly
#ggsave(filename = "/Fig/pALBNYearly_Res.png",width=16,height=6,dpi=600)

pALBN_res = ggplot(ALB_N %>% group_by(Lon, Lat) %>% summarise(Res = mean(Res)))+
  geom_raster(aes(x=Lon, y=Lat, fill=Res))+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey60',color='grey60',size=0.5)+
  coord_sf(xlim = c(100, 290),  ylim = c(0, 55))+
  scale_fill_gradient2(low='darkblue', high='darkred', midpoint = 0, limits=c(-0.4, 0.4))+
  annotate("text", x = Inf, y = Inf, label = "ALB_N", hjust = 1.5, vjust = 1.5, size = 5, color = "black") + # 添加左上角的文本
  labs(fill='Spatial Residuals')+
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
  theme(legend.position = 'none')+
  scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
  scale_x_longitude(name='Longitude',breaks = seq(100,300,40))

pALBN_res

pALBN_res_ts = ggplot(ALB_N %>% group_by(Year, Month,Time) %>% summarise(Res = mean(Res)))+
  geom_line(aes(x=Time,y=Res),linewidth=1.2,alpha=0.8,color="mediumpurple4")+
  #  scale_fill_gradient2(low='darkblue', high='darkred', midpoint = 0, limits=c(-0.4, 0.4))+
  #annotate("text", x = 2020-01-01, y = Inf, label = "YFT", hjust = 1.7, vjust = 1.5, size = 5, color = "black") + # 添加左上角的文本
  geom_hline(yintercept = 0,linetype="dashed",color="grey30",alpha=0.5)+
  labs(fill='Spatial Residuals',y="Residual")+
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12,angle=30,hjust=0.65))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  theme(legend.position = 'bottom')+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",expand = c(0,0))+
  scale_y_continuous(limits = c(-0.25,0.25))+
  labs(x="Timeseries")

pALBN_res_ts

#########
southern_alb_model_data
ALB_S = bind_rows((southern_alb_model_data %>% select("Year", 'Month', 'Lon', 'Lat',"MEI", 'nCPUE_ALB', 'pre_ALB')),)
ALB_S<-ALB_S%>%
  mutate(Res=pre_ALB-nCPUE_ALB)
ALB_S <- ALB_S %>%
  mutate(Time = as.Date(paste(Year, Month, "1", sep = "-")))

####plot####
pALBSYearly = ggplot(ALB_S %>% group_by(Year, Lon, Lat) %>% summarise(Res = mean(Res)))+
  geom_raster(aes(x=Lon, y=Lat, fill=Res))+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey60',color='grey60',size=0.5)+
  coord_sf(xlim = c(100, 290),  ylim = c(-55, 0))+
  scale_fill_gradient2(low='darkblue', high='darkred', midpoint = 0, limits=c(-0.4, 0.4))+
  facet_wrap(vars(Year), ncol = 7)+
  labs(fill='Spatial Residuals')+
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
  theme(legend.position = 'bottom')+
  scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
  scale_x_longitude(name='Longitude',breaks = seq(100,300,40))
pALBSYearly
#ggsave(filename = "/Fig/pALBSYearly_Res.png",width=16,height=6,dpi=600)

pALBS_res = ggplot(ALB_S %>% group_by(Lon, Lat) %>% summarise(Res = mean(Res)))+
  geom_raster(aes(x=Lon, y=Lat, fill=Res))+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey60',color='grey60',size=0.5)+
  coord_sf(xlim = c(100, 290),  ylim = c(-55, 0))+
  scale_fill_gradient2(low='darkblue', high='darkred', midpoint = 0, limits=c(-0.4, 0.4))+
  annotate("text", x = Inf, y = Inf, label = "ALB_S", hjust = 1.6, vjust = 1.5, size = 5, color = "black") + # 添加左上角的文本
  labs(fill='Spatial Residuals')+
  theme_bw()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12,angle=30,hjust=0.65))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  theme(legend.position = 'bottom')+
  scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
  scale_x_longitude(name='Longitude',breaks = seq(100,300,40))
pALBS_res

pALBS_res_ts = ggplot(ALB_S %>% group_by(Year, Month,Time) %>% summarise(Res = mean(Res)))+
  geom_line(aes(x=Time,y=Res),linewidth=1.2,alpha=0.8,color="darkblue")+
  #  scale_fill_gradient2(low='darkblue', high='darkred', midpoint = 0, limits=c(-0.4, 0.4))+
  #annotate("text", x = 2020-01-01, y = Inf, label = "YFT", hjust = 1.7, vjust = 1.5, size = 5, color = "black") + # 添加左上角的文本
  geom_hline(yintercept = 0,linetype="dashed",color="grey30",alpha=0.5)+
  labs(fill='Spatial Residuals',y="Residual")+
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12,angle=30,hjust=0.65))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  theme(legend.position = 'bottom')+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",expand = c(0,0))+
  scale_y_continuous(limits = c(-0.25,0.25))+
  labs(x="Timeseries")
pALBS_res_ts

#########
pacific_bet_model_data
pacific_tuna_plot_data
BET = bind_rows((pacific_bet_model_data %>% select("Year", 'Month', 'Lon', 'Lat',"MEI", 'nCPUE_BET', 'pre_BET')),)
BET<-BET%>%
  mutate(Res=pre_BET-nCPUE_BET)

BET<-BET%>%
  mutate(Time = as.Date(paste(Year, Month, "1", sep = "-")))
####plot####
pBETYearly = ggplot(BET %>% group_by(Year, Lon, Lat) %>% summarise(Res = mean(Res)))+
  geom_raster(aes(x=Lon, y=Lat, fill=Res))+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey60',color='grey60',size=0.5)+
  coord_sf(xlim = c(100, 290),  ylim = c(-55, 55))+
  scale_fill_gradient2(low='darkblue', high='darkred', midpoint = 0, limits=c(-0.4, 0.4))+
  facet_wrap(vars(Year), ncol = 7)+
  labs(fill='Spatial Residuals')+
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
  theme(legend.position = 'bottom')+
  scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
  scale_x_longitude(name='Longitude',breaks = seq(100,300,40))
pBETYearly
#ggsave(filename = "/Fig/pBETYearly_Res.png",width=16,height=9,dpi=600)

pBET_res = ggplot(BET %>% group_by( Lon, Lat) %>% summarise(Res = mean(Res)))+
  geom_raster(aes(x=Lon, y=Lat, fill=Res))+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey60',color='grey60',size=0.5)+
  coord_sf(xlim = c(100, 290),  ylim = c(-55, 55))+
  scale_fill_gradient2(low='darkblue', high='darkred', midpoint = 0, limits=c(-0.4, 0.4))+
  annotate("text", x = Inf, y = Inf, label = "BET", hjust = 1.7, vjust = 1.5, size = 5, color = "black") + # 添加左上角的文本
  labs(fill='Spatial Residuals')+
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
  theme(legend.position = 'none')+
  scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
  scale_x_longitude(name='Longitude',breaks = seq(100,300,40))
pBET_res

pBET_res_ts = ggplot(BET %>% group_by(Year, Month,Time) %>% summarise(Res = mean(Res)))+
  geom_line(aes(x=Time,y=Res),linewidth=1.2,alpha=0.8,color="darkgreen")+
  geom_hline(yintercept = 0,linetype="dashed",color="grey30",alpha=0.5)+
  labs(fill='Spatial Residuals',y="Residual")+
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12,angle=30,hjust=0.65))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  theme(legend.position = 'bottom')+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",expand = c(0,0))+
  scale_y_continuous(limits = c(-0.25,0.25))+
  labs(x="Timeseries")
pBET_res_ts

#########
pacific_yft_model_data
YFT = bind_rows((pacific_yft_model_data %>% select("Year", 'Month', 'Lon', 'Lat',"MEI", 'nCPUE_YFT', 'pre_YFT')),)
YFT<-YFT%>%
  mutate(Res=pre_YFT-nCPUE_YFT)
YFT<-YFT%>%
  mutate(Time = as.Date(paste(Year, Month, "1", sep = "-")))

####plot####
pYFTYearly = ggplot(YFT %>% group_by(Year, Lon, Lat) %>% summarise(Res = mean(Res)))+
  geom_raster(aes(x=Lon, y=Lat, fill=Res))+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey60',color='grey60',size=0.5)+
  coord_sf(xlim = c(100, 290),  ylim = c(-55, 55))+
  scale_fill_gradient2(low='darkblue', high='darkred', midpoint = 0, limits=c(-0.4, 0.4))+
  facet_wrap(vars(Year), ncol = 7)+
  labs(fill='Spatial Residuals')+
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
  theme(legend.position = 'bottom')+
  scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
  scale_x_longitude(name='Longitude',breaks = seq(100,300,40))
pYFTYearly
#ggsave(filename = "/Fig/pYFTYearly_Res.png",width=16,height=9,dpi=600)

pYFT_res = ggplot(YFT %>% group_by(Lon, Lat) %>% summarise(Res = mean(Res)))+
  geom_raster(aes(x=Lon, y=Lat, fill=Res))+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey60',color='grey60',size=0.5)+
  coord_sf(xlim = c(100, 290),  ylim = c(-55, 55))+
  scale_fill_gradient2(low='darkblue', high='darkred', midpoint = 0, limits=c(-0.4, 0.4))+
  annotate("text", x = Inf, y = Inf, label = "YFT", hjust = 1.7, vjust = 1.5, size = 5, color = "black") + # 添加左上角的文本
  labs(fill='Spatial Residuals')+
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
  theme(legend.position = 'bottom')+
  scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
  scale_x_longitude(name='Longitude',breaks = seq(100,300,40))
pYFT_res

pYFT_res_ts = ggplot(YFT %>% group_by(Year, Month,Time) %>% summarise(Res = mean(Res)))+
  geom_line(aes(x=Time,y=Res),linewidth=1.2,alpha=0.8,color="darkorange2")+
  geom_hline(yintercept = 0,linetype="dashed",color="grey30",alpha=0.5)+
  labs(fill='Spatial Residuals',y="Residual")+
  theme_test()+
  theme(text=element_text(family="Arial"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12,angle=30,hjust=0.65))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  theme(legend.position = 'bottom')+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",expand = c(0,0)) +
  scale_y_continuous(limits = c(-0.15,0.15))+
  labs(x="Timeseries")
pYFT_res_ts

pALBN_res/pALBS_res/pBET_res/pYFT_res+plot_annotation(tag_levels = 'a', tag_prefix = '(',tag_suffix = ")")
#ggsave(filename = "/Fig/ptuna_Res.png",width=6,height=12,dpi=600)

pALBN_res_ts/pALBS_res_ts/pBET_res_ts/pYFT_res_ts+plot_annotation(tag_levels = 'a', tag_prefix = '(',tag_suffix = ")")
#topptx(filename = "/Fig/ptuna_Res_ts.pptx",width=12,height=10)ggsave(filename = "/Users/linbojun/Library/CloudStorage/OneDrive-共用文件庫－onedrive/Oceanic Fisheries Ecosystem Laboratory/ENSO_LL/Figure/ptuna_Res_ts.png",width=12,height=10,dpi=600)

(pALBN_res | pALBN_res_ts) / 
  (pALBS_res | pALBS_res_ts) / 
  (pBET_res | pBET_res_ts) / 
  (pYFT_res | pYFT_res_ts) +
  plot_layout(heights = c(2, 2, 4 , 4), widths = c(2, 0.5))+plot_annotation(tag_levels = 'a', tag_prefix = '(',tag_suffix = ")",    theme = theme(plot.tag = element_text(size = 14,face="bold")))

ggsave(filename = "/pptx/Fig S7.png",width=16,height=11,dpi=600)
topptx(filename = "Fig S7.pptx",width=16,height=11)
