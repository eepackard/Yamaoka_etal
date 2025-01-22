install.packages("Hmisc")
library(Hmisc)
library(ggplot2)
library(patchwork)
#use with corresponding "(date)seedlings" rscript for stats and data sets 

#read in data ----
pp.wf<-read_csv("clean_data/pp.wf.csv")
lpi.wf<-read_csv("clean_data/lpi.wf.csv")
lpi.cc<-read_csv("clean_data/lpi.cc.csv")

#biomass----

biomass_lpi_wf<-ggplot(lpi.wf, aes(month, logbiomass)) +
  geom_jitter(color = "grey56", size = 2, width = 0.15, pch=1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, position=position_nudge(x = 0.25)) +
  stat_summary(fun.y = mean, geom = "point", size = 1.5, position=position_nudge(x = 0.25)) +
  labs(x = "", y = "log-transformed \n seedling biomass (g)", title = "") + 
  theme_classic()+
  theme(aspect.ratio = 0.80,axis.text.x = element_blank(),text = element_text(size = 10),plot.margin = margin(0,0,0,0,unit = "pt")) +
  annotate("text",x=1,y=-0.1,label="0",size=3)+
  annotate("text",x=2,y=-0.1,label="29",size=3)+
  annotate("text",x=3,y=-0.1,label="67",size=3)+
  annotate("text",x=4,y=-0.1,label="100",size=3)+
  annotate("text",x=5,y=-0.1,label="100 %",size=3)+
  annotate("point",x= 2.4,y=mean(lpi.wf[which(lpi.wf$month == "Jul"),]$logbiomass,na.rm=TRUE),col="red",pch=8)+
  annotate("point",x= 3.4,y=mean(lpi.wf[which(lpi.wf$month == "Aug"),]$logbiomass,na.rm=TRUE),col="red",pch=8)+
  annotate("point",x= 4.4,y=mean(lpi.wf[which(lpi.wf$month == "Sep"),]$logbiomass,na.rm=TRUE),col="red",pch=8)+
  annotate("point",x= 5.4,y=mean(lpi.wf[which(lpi.wf$month == "Oct"),]$logbiomass,na.rm=TRUE),col="red",pch=8)


biomass_lpi_cc<-ggplot(lpi.cc, aes(month, logbiomass)) +
  geom_jitter(color = "grey56", size = 2, width = 0.15, pch=1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, position=position_nudge(x = 0.25)) +
  stat_summary(fun.y = mean, geom = "point", size = 1.5, position=position_nudge(x = 0.25)) +
  labs(x = "Month", y = "log-transformed \n seedling biomass (g)", title = "") + 
  theme_classic()+
  theme(aspect.ratio = 0.80, text = element_text(size = 10),plot.margin = margin(0,0,0,0,unit = "pt")) + 
  annotate("text",x=1,y=-0.1,label="0",size=3)+
  annotate("text",x=2,y=-0.1,label="42",size=3)+
  annotate("text",x=3,y=-0.1,label="70",size=3)+
  annotate("text",x=4,y=-0.1,label="100",size=3)+
  annotate("text",x=5,y=-0.1,label="100 %",size=3)+
  annotate("point",x= 2.4,y=mean(lpi.cc[which(lpi.cc$month == "Jul"),]$logbiomass,na.rm=TRUE),col="red",pch=8)+
  annotate("point",x= 3.4,y=mean(lpi.cc[which(lpi.cc$month == "Aug"),]$logbiomass,na.rm=TRUE),col="red",pch=8)+
  annotate("point",x= 4.4,y=mean(lpi.cc[which(lpi.cc$month == "Sep"),]$logbiomass,na.rm=TRUE),col="red",pch=8)+
  annotate("point",x= 5.4,y=mean(lpi.cc[which(lpi.cc$month == "Oct"),]$logbiomass,na.rm=TRUE),col="red",pch=8)


biomass_pp_wf<-ggplot(pp.wf, aes(month, logbiomass)) +
  geom_jitter(color = "grey56", size = 2, width = 0.15, pch=1) +  
  geom_point(data = NULL, aes(x = 'Oct', y = 0), pch = NA) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, position=position_nudge(x = 0.25)) + 
  stat_summary(fun.y = mean, geom = "point", size = 1.5, position=position_nudge(x = 0.25)) + 
  labs(x = "", y = "log-transformed \n seedling biomass (g)", title = "") + 
  theme_classic()+
  theme(aspect.ratio = 0.80,axis.text.x = element_blank(),text = element_text(size = 10),plot.margin = margin(0,0,0,0,unit = "pt")) + 
  annotate("text",x=1,y=0.14,label="0",size=3)+
  annotate("text",x=2,y=0.14,label="16",size=3)+
  annotate("text",x=3,y=0.14,label="82",size=3)+
  annotate("text",x=4,y=0.14,label="100 %",size=3)+
  annotate("point",x= 2.4,y=mean(pp.wf[which(pp.wf$month == "Jul"),]$logbiomass,na.rm=TRUE),col="red",pch=8)+
  annotate("point",x= 3.4,y=mean(pp.wf[which(pp.wf$month == "Aug"),]$logbiomass,na.rm=TRUE),col="red",pch=8)+
  annotate("point",x= 4.4,y=mean(pp.wf[which(pp.wf$month == "Sep"),]$logbiomass,na.rm=TRUE),col="red",pch=8)

#figure 1
tiff("figures/fig.1.tiff",width = 84,height = 200,res = 600,units = "mm")
biomass_pp_wf+biomass_lpi_wf+biomass_lpi_cc+plot_layout(nrow = 3,ncol = 1)+plot_annotation(tag_levels = "a")


#logged total n----
totN_lpi_wf<-ggplot(lpi.wf, aes(month, logtotalN)) +
         geom_jitter(color = "grey56", size = 2, width = 0.15, pch=1) +
         stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, position=position_nudge(x = 0.25)) +
        stat_summary(fun.y = mean, geom = "point", size = 1.5, position=position_nudge(x = 0.25)) +
       labs(x = "", y = "log-transformed \n total seedling N (mg)", title = "") + 
  theme_classic()+
  theme(aspect.ratio = 0.80,axis.text.x = element_blank(),text = element_text(size = 10),plot.margin = margin(0,0,0,0,unit = "pt")) + 
  geom_hline(yintercept = log10(0.13),linetype = "dashed")+
  annotate("point",x= 3.4,y=mean(lpi.wf[which(lpi.wf$month == "Aug"),]$logtotalN,na.rm=TRUE),col="red",pch=8)+
  annotate("point",x= 4.4,y=mean(lpi.wf[which(lpi.wf$month == "Sep"),]$logtotalN,na.rm=TRUE),col="red",pch=8)+
  annotate("point",x= 5.4,y=mean(lpi.wf[which(lpi.wf$month == "Oct"),]$logtotalN,na.rm=TRUE),col="red",pch=8)
  

totN_lpi_cc<-ggplot(lpi.cc, aes(month, logtotalN)) +
  geom_jitter(color = "grey56", size = 2, width = 0.15, pch=1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, position=position_nudge(x = 0.25)) +
  stat_summary(fun.y = mean, geom = "point", size = 1.5, position=position_nudge(x = 0.25)) +
  labs(x = "Month", y = "log-transformed \n total seedling N (mg)", title = "") + 
  theme_classic()+
  theme(aspect.ratio = 0.80, text = element_text(size = 10),plot.margin = margin(0,0,0,0,unit = "pt")) + 
  geom_hline(yintercept = log10(0.13),linetype = "dashed")+
  annotate("point",x= 3.4,y=mean(lpi.cc[which(lpi.cc$month == "Aug"),]$logtotalN,na.rm=TRUE),col="red",pch=8)+
  annotate("point",x= 4.4,y=mean(lpi.cc[which(lpi.cc$month == "Sep"),]$logtotalN,na.rm=TRUE),col="red",pch=8)+
  annotate("point",x= 5.4,y=mean(lpi.cc[which(lpi.cc$month == "Oct"),]$logtotalN,na.rm=TRUE),col="red",pch=8)
  

totN_pp_wf<-ggplot(pp.wf, aes(month, logtotalN)) +
  geom_jitter(color = "grey56", size = 2, width = 0.15, pch=1) +  
  geom_point(data = NULL, aes(x = 'Oct', y = 0), pch = NA) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, position=position_nudge(x = 0.25)) + 
  stat_summary(fun.y = mean, geom = "point", size = 1.5, position=position_nudge(x = 0.25)) + 
  labs(x = "", y = "log-transformed \n total seedling N (mg)", title = "") + 
  theme_classic()+
  theme(aspect.ratio = 0.80,axis.text.x = element_blank(),text = element_text(size = 10),plot.margin = margin(0,0,0,0,unit = "pt")) + 
  geom_hline(yintercept = log10(1.26),linetype = "dashed")+
  annotate("point",x= 4.4,y=mean(pp.wf[which(pp.wf$month == "Sep"),]$logtotalN,na.rm=TRUE),col="red",pch=8)
  
##figure 2
tiff("figures/fig.2.tiff",width = 84,height = 200,res = 600,units = "mm")
totN_pp_wf+totN_lpi_wf+totN_lpi_cc+plot_annotation(tag_levels = "a")+plot_layout(nrow = 3,ncol = 1)



#pct n ----
pctN_lpi_wf<-ggplot(lpi.wf, aes(month, pcnt.N.shoot)) +
  geom_jitter(color = "grey56", size = 2, width = 0.15, pch=1) +
  ylim(0, 10)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, position=position_nudge(x = 0.25)) +
  stat_summary(fun.y = mean, geom = "point", size = 1.5, position=position_nudge(x = 0.25)) +
  labs(x = "", y = "percent shoot N (%)", title = "") + 
  theme_classic()+
  theme(aspect.ratio = 0.80,axis.text.x = element_blank(),text = element_text(size = 10),plot.margin = margin(0,0,0,0,unit = "pt")) + 
  geom_hline(yintercept = 5.6,linetype = "dashed")+
  annotate("text",x= 1.4,y=mean(lpi.wf[which(lpi.wf$month == "Jun"),]$pcnt.N.shoot,na.rm=TRUE),label= "a")+
  annotate("text",x= 2.4,y=mean(lpi.wf[which(lpi.wf$month == "Jul"),]$pcnt.N.shoot,na.rm=TRUE),label= "b")+
  annotate("text",x= 3.4,y=mean(lpi.wf[which(lpi.wf$month == "Aug"),]$pcnt.N.shoot,na.rm=TRUE),label= "c")+
  annotate("text",x= 4.4,y=mean(lpi.wf[which(lpi.wf$month == "Sep"),]$pcnt.N.shoot,na.rm=TRUE),label= "c")+
  annotate("text",x= 5.4,y=mean(lpi.wf[which(lpi.wf$month == "Oct"),]$pcnt.N.shoot,na.rm=TRUE),label= "c")


pctN_lpi_cc<-ggplot(lpi.cc, aes(month, pcnt.N.shoot)) +
  geom_jitter(color = "grey56", size = 2, width = 0.15, pch=1) +
  ylim(0, 10)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, position=position_nudge(x = 0.25)) +
  stat_summary(fun.y = mean, geom = "point", size = 1.5, position=position_nudge(x = 0.25)) +
  labs(x = "Month", y = "percent shoot N (%)", title = "") + 
  theme_classic()+
  theme(aspect.ratio = 0.80, text = element_text(size = 10),plot.margin = margin(0,0,0,0,unit = "pt")) + 
  geom_hline(yintercept = 6.2,linetype = "dashed")+
  annotate("text",x= 1.4,y=mean(lpi.cc[which(lpi.cc$month == "Jun"),]$pcnt.N.shoot,na.rm=TRUE),label= "a")+
  annotate("text",x= 2.4,y=mean(lpi.cc[which(lpi.cc$month == "Jul"),]$pcnt.N.shoot,na.rm=TRUE),label= "b")+
  annotate("text",x= 3.4,y=mean(lpi.cc[which(lpi.cc$month == "Aug"),]$pcnt.N.shoot,na.rm=TRUE),label= "b")+
  annotate("text",x= 4.4,y=mean(lpi.cc[which(lpi.cc$month == "Sep"),]$pcnt.N.shoot,na.rm=TRUE),label= "b")+
  annotate("text",x= 5.4,y=mean(lpi.cc[which(lpi.cc$month == "Oct"),]$pcnt.N.shoot,na.rm=TRUE),label= "b")


pctN_pp_wf<-ggplot(pp.wf, aes(month, pcnt.N.shoot)) +
  geom_jitter(color = "grey56", size = 2, width = 0.15, pch=1) + 
  ylim(0, 10)+
  geom_point(data = NULL, aes(x = 'Oct', y = 0), pch = NA) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, position=position_nudge(x = 0.25)) + 
  stat_summary(fun.y = mean, geom = "point", size = 1.5, position=position_nudge(x = 0.25)) + 
  labs(x = "", y = "percent shoot N (%)", title = "") + 
  theme_classic()+
  theme(aspect.ratio = 0.80,axis.text.x = element_blank(),text = element_text(size = 10),plot.margin = margin(0,0,0,0,unit = "pt")) + 
  geom_hline(yintercept = 6.2,linetype = "dashed")+
  annotate("text",x= 1.4,y=mean(pp.wf[which(pp.wf$month == "Jun"),]$pcnt.N.shoot,na.rm=TRUE),label= "a")+
  annotate("text",x= 2.4,y=mean(pp.wf[which(pp.wf$month == "Jul"),]$pcnt.N.shoot,na.rm=TRUE),label= "b")+
  annotate("text",x= 3.4,y=mean(pp.wf[which(pp.wf$month == "Aug"),]$pcnt.N.shoot,na.rm=TRUE),label= "b")+
  annotate("text",x= 4.4,y=mean(pp.wf[which(pp.wf$month == "Sep"),]$pcnt.N.shoot,na.rm=TRUE),label= "b")

##figure 3
tiff("figures/fig.3.tiff",width = 84,height = 200,res = 600,units = "mm")
pctN_pp_wf+pctN_lpi_wf+pctN_lpi_cc+plot_annotation(tag_levels = "a")+plot_layout(nrow = 3,ncol = 1)


#root 15n -----
x15Nroot_lpi_wf<-ggplot(lpi.wf, aes(month, `15N.root`)) +
  geom_jitter(color = "grey56", size = 2, width = 0.15, pch=1) +
  ylim(-15, 10) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, position=position_nudge(x = 0.25)) +
  stat_summary(fun.y = mean, geom = "point", size = 1.5, position=position_nudge(x = 0.25)) +
  labs(x = "", y = expression(paste("Root \u03b4 ", ""^15, "N")), title = "") + 
  theme_classic()+
  theme(aspect.ratio = 0.80,axis.text.x = element_blank(),text = element_text(size = 10),plot.margin = margin(0,0,0,0,unit = "pt")) + 
  geom_hline(yintercept = -2.56,linetype = "dashed")+
  annotate("point",x= 2.4,y=mean(lpi.wf[which(lpi.wf$month == "Jul"),]$`15N.root`,na.rm=TRUE),col="red",pch=8)+
  annotate("point",x= 3.4,y=mean(lpi.wf[which(lpi.wf$month == "Aug"),]$`15N.root`,na.rm=TRUE),col="red",pch=8)+
  annotate("point",x= 4.4,y=mean(lpi.wf[which(lpi.wf$month == "Sep"),]$`15N.root`,na.rm=TRUE),col="red",pch=8)+
  annotate("point",x= 5.4,y=mean(lpi.wf[which(lpi.wf$month == "Oct"),]$`15N.root`,na.rm=TRUE),col="red",pch=8)



x15Nroot_lpi_cc<-ggplot(lpi.cc, aes(month, `15N.root`)) +
  geom_jitter(color = "grey56", size = 2, width = 0.15, pch=1) +
  ylim(-15, 10) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, position=position_nudge(x = 0.25)) +
  stat_summary(fun.y = mean, geom = "point", size = 1.5, position=position_nudge(x = 0.25)) +
  labs(x = "Month", y = expression(paste("Root \u03b4 ", ""^15, "N")), title = "") + 
  theme_classic()+
  theme(aspect.ratio = 0.80, text = element_text(size = 10),plot.margin = margin(0,0,0,0,unit = "pt")) + 
  geom_hline(yintercept = -2.56,linetype = "dashed")


x15Nroot_pp_wf<-ggplot(pp.wf, aes(month, `15N.root`)) +
  geom_jitter(color = "grey56", size = 2, width = 0.15, pch=1) + 
  ylim(-15, 10) + 
  geom_point(data = NULL, aes(x = 'Oct', y = 0), pch = NA) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, position=position_nudge(x = 0.25)) + 
  stat_summary(fun.y = mean, geom = "point", size = 1.5, position=position_nudge(x = 0.25)) + 
  labs(x = "", y = expression(paste("Root \u03b4 ", ""^15, "N")), title = "") + 
  theme_classic()+
  theme(aspect.ratio = 0.80,axis.text.x = element_blank(),text = element_text(size = 10),plot.margin = margin(0,0,0,0,unit = "pt")) + 
  geom_hline(yintercept = -3.08,linetype = "dashed")+
  annotate("point",x= 3.4,y=mean(pp.wf[which(pp.wf$month == "Aug"),]$`15N.root`,na.rm=TRUE),col="red",pch=8)+
  annotate("point",x= 4.4,y=mean(pp.wf[which(pp.wf$month == "Sep"),]$`15N.root`,na.rm=TRUE),col="red",pch=8)

  
#shoot 15n -----
x15Nshoot_lpi_wf<-ggplot(lpi.wf, aes(month, `15N.shoot`)) +
  geom_jitter(color = "grey56", size = 2, width = 0.15, pch=1) +
  ylim(-15, 10) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, position=position_nudge(x = 0.25)) +
  stat_summary(fun.y = mean, geom = "point", size = 1.5, position=position_nudge(x = 0.25)) +
  labs(x = "", y = expression(paste("Shoot \u03b4 ", ""^15, "N")), title = "") + 
  theme_classic()+
  theme(aspect.ratio = 0.80,axis.text.x = element_blank(),text = element_text(size = 10),plot.margin = margin(0,0,0,0,unit = "pt")) + 
  geom_hline(yintercept = -2.56,linetype = "dashed")+
  annotate("point",x= 3.4,y=mean(lpi.wf[which(lpi.wf$month == "Aug"),]$`15N.shoot`,na.rm=TRUE),col="red",pch=8)+
  annotate("point",x= 4.4,y=mean(lpi.wf[which(lpi.wf$month == "Sep"),]$`15N.shoot`,na.rm=TRUE),col="red",pch=8)+
  annotate("point",x= 5.4,y=mean(lpi.wf[which(lpi.wf$month == "Oct"),]$`15N.shoot`,na.rm=TRUE),col="red",pch=8)


x15Nshoot_lpi_cc<-ggplot(lpi.cc, aes(month, `15N.shoot`)) +
  geom_jitter(color = "grey56", size = 2, width = 0.15, pch=1) +
  ylim(-15, 10) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, position=position_nudge(x = 0.25)) +
  stat_summary(fun.y = mean, geom = "point", size = 1.5, position=position_nudge(x = 0.25)) +
  labs(x = "Month", y = expression(paste("Shoot \u03b4 ", ""^15, "N")), title = "") + 
  theme_classic()+
  theme(aspect.ratio = 0.80, text = element_text(size = 10), plot.margin = margin(0,0,0,0,unit = "pt")) + 
  geom_hline(yintercept = -2.56,linetype = "dashed")+
  annotate("point",x= 4.4,y=mean(lpi.cc[which(lpi.cc$month == "Sep"),]$`15N.shoot`,na.rm=TRUE),col="red",pch=8)+
  annotate("point",x= 5.4,y=mean(lpi.cc[which(lpi.cc$month == "Oct"),]$`15N.shoot`,na.rm=TRUE),col="red",pch=1)


x15Nshoot_pp_wf<-ggplot(pp.wf, aes(month, `15N.shoot`)) +
  geom_jitter(color = "grey56", size = 2, width = 0.15, pch=1) + 
  ylim(-15, 10) + 
  geom_point(data = NULL, aes(x = 'Oct', y = 0), pch = NA) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, position=position_nudge(x = 0.25)) + 
  stat_summary(fun.y = mean, geom = "point", size = 1.5, position=position_nudge(x = 0.25)) + 
  labs(x = "", y = expression(paste("Shoot \u03b4 ", ""^15, "N")), title = "") + 
  theme_classic()+
  theme(aspect.ratio = 0.80,axis.text.x = element_blank(),text = element_text(size = 10),plot.margin = margin(0,0,0,0,unit = "pt")) + 
  geom_hline(yintercept = -3.08,linetype = "dashed")+
  annotate("point",x= 3.4,y=mean(pp.wf[which(pp.wf$month == "Aug"),]$`15N.shoot`,na.rm=TRUE),col="red",pch=8)+
  annotate("point",x= 4.4,y=mean(pp.wf[which(pp.wf$month == "Sep"),]$`15N.shoot`,na.rm=TRUE),col="red",pch=8)


##figure 4
tiff("figures/fig.4.tiff",width = 174,height = 200,res = 600,units = "mm")
x15Nshoot_pp_wf+x15Nroot_pp_wf+x15Nshoot_lpi_wf+x15Nroot_lpi_wf+x15Nshoot_lpi_cc+x15Nroot_lpi_cc+plot_annotation(tag_levels = "a")+plot_layout(nrow = 3,ncol = 2)

##M vs NM  ---- 

#biomass----
status_ppwf_bio<-ggplot(data = pp.wf[which(pp.wf$month %in% c("Jul","Aug")),], aes(x = month, y = logbiomass,color=status)) +
  geom_jitter(position = position_jitterdodge(0.25),pch=1) +
  stat_summary(data=pp.wf[which(pp.wf$month %in% c("Jul","Aug")),],aes(x = month, y = logbiomass,color=status), fun.data="mean_cl_normal",
               fun.args = list(mult=1), geom="errorbar", width=0.2,inherit.aes = FALSE, position =position_dodge(0.3)) +
  stat_summary(data=pp.wf[which(pp.wf$month %in% c("Jul","Aug")),],aes(x = month, y = logbiomass,color=status),
               fun = "mean",geom="point",inherit.aes = FALSE, position =position_dodge(0.3)) +  
  #ylim(-15,10)+
  labs(x = "", y = "log-transformed \n total N (mg)") + scale_color_manual(values=c("darkgreen", "darkorange3"))+
  theme_classic()+
  theme(legend.position = "none",legend.title = element_blank(),axis.text = element_text(size = 10),axis.text.x = element_blank())


status_lpiwf_bio<-ggplot(data = lpi.wf[which(lpi.wf$month %in% c("Jul","Aug")),], aes(x = month, y = logbiomass,color=status)) +
  geom_jitter(position = position_jitterdodge(0.25),pch=1) +
  stat_summary(data=lpi.wf[which(lpi.wf$month %in% c("Jul","Aug")),],aes(x = month, y = logbiomass,color=status), fun.data="mean_cl_normal",
               fun.args = list(mult=1), geom="errorbar", width=0.2,inherit.aes = FALSE, position =position_dodge(0.3)) +
  stat_summary(data=lpi.wf[which(lpi.wf$month %in% c("Jul","Aug")),],aes(x = month, y = logbiomass,color=status),
               fun = "mean",geom="point",inherit.aes = FALSE, position =position_dodge(0.3)) +  
  #ylim(-15,10)+
  labs(x = "", y = "log-transformed \n total N (mg)") + scale_color_manual(values=c("darkgreen", "darkorange3"))+
  theme_classic()+
  theme(legend.position = "none",legend.title = element_blank(),axis.text = element_text(size = 10),axis.text.x = element_blank())


status_lpicc_bio<-ggplot(data = lpi.cc[which(lpi.cc$month %in% c("Jul","Aug")),], aes(x = month, y = logbiomass,color=status)) +
  geom_jitter(position = position_jitterdodge(0.25),pch=1) +
  stat_summary(data=lpi.cc[which(lpi.cc$month %in% c("Jul","Aug")),],aes(x = month, y = logbiomass,color=status), fun.data="mean_cl_normal",
               fun.args = list(mult=1), geom="errorbar", width=0.2,inherit.aes = FALSE, position =position_dodge(0.3)) +
  stat_summary(data=lpi.cc[which(lpi.cc$month %in% c("Jul","Aug")),],aes(x = month, y = logbiomass,color=status),
               fun = "mean",geom="point",inherit.aes = FALSE, position =position_dodge(0.3)) +  
  #ylim(-15,10)+
  labs(x = "Month", y = "log-transformed \n total N (mg)") + scale_color_manual(values=c("darkgreen", "darkorange3"))+
  theme_classic()+
  theme(legend.position = "none",legend.title = element_blank(),axis.text = element_text(size = 10))

#figure 5
tiff("figures/fig.5.tiff",width = 84,height = 200,res = 600,units = "mm")
status_ppwf_bio+status_lpiwf_bio+status_lpicc_bio+plot_annotation(tag_levels = "a")+plot_layout(nrow = 3,ncol = 1)


#totalN----
status_ppwf_totN<-ggplot(data = pp.wf[which(pp.wf$month %in% c("Jul","Aug")),], aes(x = month, y = logtotalN,color=status)) +
  geom_jitter(position = position_jitterdodge(0.25),pch=1) +
  stat_summary(data=pp.wf[which(pp.wf$month %in% c("Jul","Aug")),],aes(x = month, y = logtotalN,color=status), fun.data="mean_cl_normal",
               fun.args = list(mult=1), geom="errorbar", width=0.2,inherit.aes = FALSE, position =position_dodge(0.3)) +
  stat_summary(data=pp.wf[which(pp.wf$month %in% c("Jul","Aug")),],aes(x = month, y = logtotalN,color=status),
               fun = "mean",geom="point",inherit.aes = FALSE, position =position_dodge(0.3)) +  
  #ylim(-15,10)+
  labs(x = "", y = "log-transformed \n total N (mg)") + scale_color_manual(values=c("darkgreen", "darkorange3"))+
  theme_classic()+
  theme(legend.position = "none",legend.title = element_blank(),axis.text = element_text(size = 10),axis.text.x = element_blank())


status_lpiwf_totN<-ggplot(data = lpi.wf[which(lpi.wf$month %in% c("Jul","Aug")),], aes(x = month, y = logtotalN,color=status)) +
  geom_jitter(position = position_jitterdodge(0.25),pch=1) +
  stat_summary(data=lpi.wf[which(lpi.wf$month %in% c("Jul","Aug")),],aes(x = month, y = logtotalN,color=status), fun.data="mean_cl_normal",
               fun.args = list(mult=1), geom="errorbar", width=0.2,inherit.aes = FALSE, position =position_dodge(0.3)) +
  stat_summary(data=lpi.wf[which(lpi.wf$month %in% c("Jul","Aug")),],aes(x = month, y = logtotalN,color=status),
               fun = "mean",geom="point",inherit.aes = FALSE, position =position_dodge(0.3)) +  
  #ylim(-15,10)+
  labs(x = "", y = "log-transformed \n total N (mg)") + scale_color_manual(values=c("darkgreen", "darkorange3"))+
   theme_classic()+
   theme(legend.position = "none",legend.title = element_blank(),axis.text = element_text(size = 10),axis.text.x = element_blank())
 

status_lpicc_totN<-ggplot(data = lpi.cc[which(lpi.cc$month %in% c("Jul","Aug")),], aes(x = month, y = logtotalN,color=status)) +
  geom_jitter(position = position_jitterdodge(0.25),pch=1) +
  stat_summary(data=lpi.cc[which(lpi.cc$month %in% c("Jul","Aug")),],aes(x = month, y = logtotalN,color=status), fun.data="mean_cl_normal",
               fun.args = list(mult=1), geom="errorbar", width=0.2,inherit.aes = FALSE, position =position_dodge(0.3)) +
  stat_summary(data=lpi.cc[which(lpi.cc$month %in% c("Jul","Aug")),],aes(x = month, y = logtotalN,color=status),
               fun = "mean",geom="point",inherit.aes = FALSE, position =position_dodge(0.3)) +  
  #ylim(-15,10)+
  labs(x = "Month", y = "log-transformed \n total N (mg)") + scale_color_manual(values=c("darkgreen", "darkorange3"))+
  theme_classic()+
  theme(legend.position = "none",legend.title = element_blank(),axis.text = element_text(size = 10))

#figure 6
tiff("figures/fig.6.tiff",width = 84,height = 200,res = 600,units = "mm")
status_ppwf_totN+status_lpiwf_totN+status_lpicc_totN+plot_annotation(tag_levels = "a")+plot_layout(nrow = 3,ncol = 1)

#shoot vs root 15n ----
status_ppwf_15N.root<-ggplot(data = pp.wf[which(pp.wf$month %in% c("Jul","Aug")),], aes(x = month, y = `15N.root`,color=status)) +
  geom_jitter(position = position_jitterdodge(0.25),pch=1) +
  stat_summary(data=pp.wf[which(pp.wf$month %in% c("Jul","Aug")),],aes(x = month, y = `15N.root`,color=status), fun.data="mean_cl_normal",
               fun.args = list(mult=1), geom="errorbar", width=0.2,inherit.aes = FALSE, position =position_dodge(0.3)) +
  stat_summary(data=pp.wf[which(pp.wf$month %in% c("Jul","Aug")),],aes(x = month, y = `15N.root`,color=status),
               fun = "mean",geom="point",inherit.aes = FALSE, position =position_dodge(0.3)) +  
  geom_hline(yintercept = -3.08,linetype="dashed")+
  labs(x = "", y = expression(paste("Root \u03b4 ", ""^15, "N"))) + scale_color_manual(values=c("darkgreen", "darkorange3"))+
  theme_classic()+
  theme(legend.position = "none",legend.title = element_blank(),axis.text = element_text(size = 10),axis.text.x = element_blank())


status_lpiwf_15N.root<-ggplot(data = lpi.wf[which(lpi.wf$month %in% c("Jul","Aug")),], aes(x = month, y = `15N.root`,color=status)) +
  geom_jitter(position = position_jitterdodge(0.25),pch=1) +
  stat_summary(data=lpi.wf[which(lpi.wf$month %in% c("Jul","Aug")),],aes(x = month, y = `15N.root`,color=status), fun.data="mean_cl_normal",
               fun.args = list(mult=1), geom="errorbar", width=0.2,inherit.aes = FALSE, position =position_dodge(0.3)) +
  stat_summary(data=lpi.wf[which(lpi.wf$month %in% c("Jul","Aug")),],aes(x = month, y = `15N.root`,color=status),
               fun = "mean",geom="point",inherit.aes = FALSE, position =position_dodge(0.3)) +  
  geom_hline(yintercept = -2.56,linetype="dashed")+  
  #ylim(-15,10)+
  labs(x = "", y = expression(paste("Root \u03b4 ", ""^15, "N"))) + scale_color_manual(values=c("darkgreen", "darkorange3"))+
  theme_classic()+
  theme(legend.position = "none",legend.title = element_blank(),axis.text = element_text(size = 10),axis.text.x = element_blank())


status_lpicc_15N.root<-ggplot(data = lpi.cc[which(lpi.cc$month %in% c("Jul","Aug")),], aes(x = month, y = `15N.root`,color=status)) +
  geom_jitter(position = position_jitterdodge(0.25),pch=1) +
  stat_summary(data=lpi.cc[which(lpi.cc$month %in% c("Jul","Aug")),],aes(x = month, y = `15N.root`,color=status), fun.data="mean_cl_normal",
               fun.args = list(mult=1), geom="errorbar", width=0.2,inherit.aes = FALSE, position =position_dodge(0.3)) +
  stat_summary(data=lpi.cc[which(lpi.cc$month %in% c("Jul","Aug")),],aes(x = month, y = `15N.root`,color=status),
               fun = "mean",geom="point",inherit.aes = FALSE, position =position_dodge(0.3)) +  
  geom_hline(yintercept = -2.56,linetype="dashed")+  
  #ylim(-15,10)+
  labs(x = "Month", y = expression(paste("Root \u03b4 ", ""^15, "N"))) + scale_color_manual(values=c("darkgreen", "darkorange3"))+
  theme_classic()+
  theme(legend.position = "none",legend.title = element_blank(),axis.text = element_text(size = 10))

status_ppwf_15N.shoot<-ggplot(data = pp.wf[which(pp.wf$month %in% c("Jul","Aug")),], aes(x = month, y = `15N.shoot`,color=status)) +
  geom_jitter(position = position_jitterdodge(0.25),pch=1) +
  stat_summary(data=pp.wf[which(pp.wf$month %in% c("Jul","Aug")),],aes(x = month, y = `15N.shoot`,color=status), fun.data="mean_cl_normal",
               fun.args = list(mult=1), geom="errorbar", width=0.2,inherit.aes = FALSE, position =position_dodge(0.3)) +
  stat_summary(data=pp.wf[which(pp.wf$month %in% c("Jul","Aug")),],aes(x = month, y = `15N.shoot`,color=status),
               fun = "mean",geom="point",inherit.aes = FALSE, position =position_dodge(0.3)) +  
  geom_hline(yintercept = -3.08,linetype="dashed")+  
  #ylim(-15,10)+
  labs(x = "", y = expression(paste("Shoot \u03b4 ", ""^15, "N"))) + scale_color_manual(values=c("darkgreen", "darkorange3"))+
  theme_classic()+
  theme(legend.position = "none",legend.title = element_blank(),axis.text = element_text(size = 10),axis.text.x = element_blank())


status_lpiwf_15N.shoot<-ggplot(data = lpi.wf[which(lpi.wf$month %in% c("Jul","Aug")),], aes(x = month, y = `15N.shoot`,color=status)) +
  geom_jitter(position = position_jitterdodge(0.25),pch=1) +
  stat_summary(data=lpi.wf[which(lpi.wf$month %in% c("Jul","Aug")),],aes(x = month, y = `15N.shoot`,color=status), fun.data="mean_cl_normal",
               fun.args = list(mult=1), geom="errorbar", width=0.2,inherit.aes = FALSE, position =position_dodge(0.3)) +
  stat_summary(data=lpi.wf[which(lpi.wf$month %in% c("Jul","Aug")),],aes(x = month, y = `15N.shoot`,color=status),
               fun = "mean",geom="point",inherit.aes = FALSE, position =position_dodge(0.3)) +   
  geom_hline(yintercept = -2.56,linetype="dashed")+ 
  #ylim(-15,10)+
  labs(x = "", y = expression(paste("Shoot \u03b4 ", ""^15, "N"))) + scale_color_manual(values=c("darkgreen", "darkorange3"))+
  theme_classic()+
  theme(legend.position = "none",legend.title = element_blank(),axis.text = element_text(size = 10),axis.text.x = element_blank())


status_lpicc_15N.shoot<-ggplot(data = lpi.cc[which(lpi.cc$month %in% c("Jul","Aug")),], aes(x = month, y = `15N.shoot`,color=status)) +
  geom_jitter(position = position_jitterdodge(0.25),pch=1) +
  stat_summary(data=lpi.cc[which(lpi.cc$month %in% c("Jul","Aug")),],aes(x = month, y = `15N.shoot`,color=status), fun.data="mean_cl_normal",
               fun.args = list(mult=1), geom="errorbar", width=0.2,inherit.aes = FALSE, position =position_dodge(0.3)) +
  stat_summary(data=lpi.cc[which(lpi.cc$month %in% c("Jul","Aug")),],aes(x = month, y = `15N.shoot`,color=status),
               fun = "mean",geom="point",inherit.aes = FALSE, position =position_dodge(0.3)) +   
  geom_hline(yintercept = -2.56,linetype="dashed")+ 
  #ylim(-15,10)+
  labs(x = "Month", y = expression(paste("Shoot \u03b4 ", ""^15, "N"))) + scale_color_manual(values=c("darkgreen", "darkorange3"))+
  theme_classic()+
  theme(legend.position = "none",legend.title = element_blank(),axis.text = element_text(size = 10))

#figure 7
tiff("figures/fig.7.tiff",width = 174,height = 200,res = 600,units = "mm")
status_ppwf_15N.shoot+status_ppwf_15N.root+status_lpiwf_15N.shoot+status_lpiwf_15N.root+status_lpicc_15N.shoot+status_lpicc_15N.root+plot_annotation(tag_levels = "a")+plot_layout(nrow = 3,ncol = 2)

#read in species data
species_data<-read_excel("raw_data/spec.data.jun25.xlsx")

#need to match the seedling nutrient data to the species data 
length(which(paste(species_data$root.code,species_data$shoot.code) %in% paste(seedlingsjmar2$root.code,seedlingsjmar2$shoot.code))) 
#thankfully the root and shoot codes are in same format so we can pull out the desired variable and arrange in same order

seedling_w_spec<- seedlingsjmar2[which(paste(seedlingsjmar2$root.code,seedlingsjmar2$shoot.code) %in% paste(species_data$root.code,species_data$shoot.code)),]
seedling_w_spec<- seedling_w_spec[match(paste(species_data$root.code,species_data$shoot.code),paste(seedling_w_spec$root.code,seedling_w_spec$shoot.code)),] 

#check that that are for sure in same order
paste(seedling_w_spec$root.code,seedling_w_spec$shoot.code) == paste(species_data$root.code,species_data$shoot.code)

#now leave only the required variables and add the species data 

seedling_w_spec <- seedling_w_spec[,which(colnames(seedling_w_spec) %in% c("shoot.code","root.code","site","month","pcnt.N.shoot"))]
seedling_w_spec <- cbind(seedling_w_spec,species_data$genus)
colnames(seedling_w_spec)<-c(colnames(seedling_w_spec[,1:5]),"genus")

unique(species_data$genus) #there are 15 genera represented but only Cenoccum, Pustularia, Wilcoxina, Tomentella, Phialocephla, Trichophea
seedling_w_spec <- seedling_w_spec[which(seedling_w_spec$genus %in% c("Cenococcum", "Pustularia", "Wilcoxina", "Tomentella", "Phialocephala", "Trichophaea")),]

#make genus a factor 
seedling_w_spec$genus <- as.factor(seedling_w_spec$genus)
seedling_w_spec$genus <- fct_relevel(seedling_w_spec$genus,"Cenococcum", "Phialocephala","Pustularia",  "Tomentella",  "Trichophaea","Wilcoxina")

##plots
species_lpi_wf<-ggplot(seedling_w_spec[which(seedling_w_spec$site %in% c("GE","GW","Law") & seedling_w_spec$genus != "Phialocephala"),], aes(genus, pcnt.N.shoot)) +
  geom_jitter(color = "grey56", size = 2, width = 0.15, pch=1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, position=position_nudge(x = 0.25)) +
  stat_summary(fun.y = mean, geom = "point", size = 1.5, position=position_nudge(x = 0.25)) +
  labs(x = "", y = "percent shoot N (%)", title = "") + 
  theme_classic()+
  theme(text = element_text(size = 10),plot.margin = margin(0,0,0,0,unit = "pt")) 


species_lpi_cc<-ggplot(seedling_w_spec[which(seedling_w_spec$site %in% c("12N2","12N3","12N4") & seedling_w_spec$genus != "Tomentella"),], aes(genus, pcnt.N.shoot)) +
  geom_jitter(color = "grey56", size = 2, width = 0.15, pch=1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, position=position_nudge(x = 0.25)) +
  stat_summary(fun.y = mean, geom = "point", size = 1.5, position=position_nudge(x = 0.25)) +
  labs(x = "Genus", y = "percent shoot N (%)", title = "") + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme_classic()+
  theme(text = element_text(size = 10),plot.margin = margin(0,0,0,0,unit = "pt")) 


species_pp_wf<-ggplot(seedling_w_spec[which(seedling_w_spec$site %in% c("EE","EW","CL") & seedling_w_spec$genus != "Tomentella" & seedling_w_spec$genus != "Trichophaea"),], aes(genus, pcnt.N.shoot)) +
  geom_jitter(color = "grey56", size = 2, width = 0.15, pch=1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, position=position_nudge(x = 0.25)) + 
  stat_summary(fun.y = mean, geom = "point", size = 1.5, position=position_nudge(x = 0.25)) + 
  labs(x = "", y = "percent shoot N (%)", title = "") + 
  theme_classic()+
  theme(text = element_text(size = 10),plot.margin = margin(0,0,0,0,unit = "pt"))
 
#figure 8
tiff("figures/fig.8.tiff",width = 84,height = 160,res = 1200,units = "mm")
species_pp_wf+species_lpi_wf+species_lpi_cc+plot_annotation(tag_levels = "a")+plot_layout(nrow=3,ncol=1)

