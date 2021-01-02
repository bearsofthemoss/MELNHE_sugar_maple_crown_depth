
## R code for creating graphs in sugar maple crown N*P.
## Alex Young   5/14/2020
## alexyoung.116@gmail.com

library(ggplot2)
library(ggpubr)


samp<-read.csv("Data/MELNHE_SugarMapleCrownDepth.csv", header=T)


samp$Treatment <- factor(samp$trmt,levels=c("Control","N treatment","P treatment","N+P treatment"))
head(samp)
str(samp)


########################
# Figure 1!
f.area<-ggplot(samp, aes(x=scaled, y=area_cm2, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  labs(y = bquote('leaf area'~(cm^2)), x = bquote('Depth in crown '~(scaled)))+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))
f.area

f.mass<-ggplot(samp, aes(x=scaled, y=mass.g, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  labs(y = bquote('leaf mass'~(g)), x = bquote(''))+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5)) 
f.mass

f.sla<-ggplot(samp, aes(x=scaled, y=SLA, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  labs(y = bquote('specific leaf area ('~cm^2~g^-1*')'), x = bquote(''))+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))
f.sla

## FIGURE 1!
f1<-ggarrange(f.area, f.mass, f.sla, nrow=1, ncol=3, common.legend=T, legend="bottom")
f1

# For pdf.
#dpi=300    #pixels per square inch
#tiff("thesis_fig_1.tif", width=12*dpi, height=8*dpi, res=dpi)
#f1
#dev.off()



##  FIGURE 3!
f.C<-ggplot(samp, aes(x=scaled, y=C, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  ylab("C (mg"~g^-1*")")+xlab("Depth in crown (scaled)")+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))
f.C


f.N<-ggplot(samp, aes(x=scaled, y=N, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  ylab("N (mg"~g^-1*")")+xlab("")+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))
f.N

f.P<-ggplot(samp, aes(x=scaled, y=P, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  ylab("P (mg"~g^-1*")")+xlab("")+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))
f.P

f.NP<-ggplot(samp, aes(x=scaled, y=N_P, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  ylab("N:P ratio")+xlab("")+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))
f.NP

# Figure 3!
f3<-ggarrange(f.C, f.N, f.P,f.NP , nrow=1, ncol=4, common.legend = T, legend="bottom")
f3

# For pdf
#dpi=300    #pixels per square inch
#tiff("thesis_fig_3.tif", width=12*dpi, height=8*dpi, res=dpi)
#f3
#dev.off()


## Figure 4!
f.ca<-ggplot(samp, aes(x=scaled, y=Ca, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  ylab("Ca (mg"~g^-1*")")+xlab("Depth in crown (scaled)")+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+#geom_hline(yintercept=5.5, linetype="dashed", size=2)+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))
f.ca

f.mg<-ggplot(samp, aes(x=scaled, y=Mg, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  ylab("Mg (mg"~g^-1*")")+xlab("")+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))
f.mg

ggarrange(f.C, f.mg, common.legend=T, legend="bottom")

f.mn<-ggplot(samp, aes(x=scaled, y=Mn, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  ylab("Mn (mg"~g^-1*")")+xlab("")+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))
f.mn

f.al<-ggplot(samp, aes(x=scaled, y=Al, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  ylab("Al (mg"~g^-1*")")+xlab("")+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))
f.al



f.b<-ggplot(samp, aes(x=scaled, y=B, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  ylab("B (mg"~g^-1*")")+xlab("")+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))
f.b

f.zn<-ggplot(samp, aes(x=scaled, y=Zn, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  ylab("Zn (mg"~g^-1*")")+xlab("")+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))
f.zn

# Figure 4!
f4<-ggarrange(f.ca, f.mg, f.mn, f.al, f.b, f.zn, nrow=1, ncol=6, common.legend = T, legend="bottom")
f4

#dpi=300    #pixels per square inch
#tiff("thesis_fig_4.tif", width=12*dpi, height=8*dpi, res=dpi)
#f4
#dev.off()


# FIGURE 5 BELOW!


f.chl<-ggplot(samp, aes(x=scaled, y=total.chl, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  ylab("chlorophyll (mg"~g^-1*" FW)")+xlab("Depth in crown (scaled)")+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))
f.chl

f.chl.r<-ggplot(samp, aes(x=scaled, y=Chl.R, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  ylab("chlorophyll a:b")+xlab("")+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))
f.chl.r

f.carot<-ggplot(samp, aes(x=scaled, y=carot, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  ylab("carotenoids (mg"~g^-1*" FW)")+xlab("")+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))
f.carot



# Figure 5!
f5<-ggarrange(f.chl,f.carot,f.chl.r, nrow=1, ncol=3, common.legend=T, legend="bottom")
f5

# for pdf.
#dpi=300    #pixels per square inch
#tiff("thesis_fig_5.tif", width=12*dpi, height=8*dpi, res=dpi)
#f5
#dev.off()


## Figure 6! amino acids
f.glu<-ggplot(samp, aes(x=scaled, y=Glu, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  ylab("glutamic acid (nmol"~g^-1*" FW)")+xlab("")+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))
f.glu

f.ala<-ggplot(samp, aes(x=scaled, y=Ala, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  ylab("alanine (nmol"~g^-1*" FW)")+xlab("Depth in crown (scaled)")+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))
f.ala

f.gaba<-ggplot(samp, aes(x=scaled, y=GABA, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  ylab("GABA (nmol"~g^-1*" FW)")+xlab("")+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))
f.gaba

f.val<-ggplot(samp, aes(x=scaled, y=Val, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  ylab("valine (nmol"~g^-1*" FW)")+xlab("")+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))
f.val


f.pro<-ggplot(samp, aes(x=scaled, y=protein, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  labs(y = bquote("soluble protein (mg"~g^-1*" FW)"), x = "")+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))
f.pro


f6<-ggarrange(f.ala, f.glu, f.val, f.gaba, f.pro, nrow=1, ncol=5, common.legend=T, legend="bottom")
f6

## FIGURE 6! pdf.
#dpi=300    #pixels per square inch
#tiff("thesis_fig_6.tif", width=12*dpi, height=8*dpi, res=dpi)
#f6
#dev.off()



# Figure 7! Polyamines

f.put<-ggplot(samp, aes(x=scaled, y=Put, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  ylab("putrescine (nmol"~g^-1*" FW)")+xlab("Depth in crown (scaled)")+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))
f.put

f.spd<-ggplot(samp, aes(x=scaled, y=Spd, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  ylab("spermidine (nmol"~g^-1*" FW)")+xlab("")+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))
f.spd

f.spm<-ggplot(samp, aes(x=scaled, y=Spm, col=Treatment, shape=Stand, group=Tree_ID))+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  scale_shape_manual(values=c(15,16,17))+ 
  geom_point(aes(group=Tree_ID), size=3, stroke=2)+
  geom_smooth(method="lm",size=1, se=F,show.legend = FALSE, aes(size=1.4))+
  ylab("spermine (nmol"~g^-1*" FW)")+xlab("")+coord_flip()+
  facet_wrap(~Treatment, nrow=4,strip.position="top")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))
f.spm

f7<-ggarrange(f.put, f.spd, f.spm, nrow=1, ncol=3, common.legend=T, legend="bottom")
f7

## FIGURE 7!
# for pdf.
#f7
#dpi=300
#tiff("thesis_fig_7.tif", width=12*dpi, height=8*dpi, res=dpi)
#dev.off()
