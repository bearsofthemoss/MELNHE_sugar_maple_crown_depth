




library(ggplot2)
library(nlme)


samp<-read.csv("better_melnhe_sugar_maple_crown_depth.csv", header=T)

samp$Treatment <- factor(samp$trmt,levels=c("Control","N treatment","P treatment","N+P treatment"))
samp$trmt <- factor(samp$trmt,levels=c("Control","N treatment","P treatment","N+P treatment"))
samp$Tree_ID<-as.numeric(samp$Tree_ID)



###########  SLA, mass, area

mSLA <- lme(SLA ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp)
mmass <- lme(mass_g ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp)
marea <- lme(area_cm2 ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp)


samp$fitSLA <- predict(mSLA)   #Add model fits to dataframe
samp$fitmass <- predict(mmass)   #Add model fits to dataframe
samp$fitarea <- predict(marea)   #Add model fits to dataframe


gSLA<-ggplot(samp,aes(scaled, SLA, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitSLA ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("SLA")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))
gSLA


garea<-ggplot(samp,aes(scaled, area_cm2, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitarea ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  labs(y=bquote('leaf area'~(cm^2)))+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))
garea

gmass<-ggplot(samp,aes(scaled, mass_g, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitmass ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("leaf mass (g)")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))
gmass

library(ggpubr)
ggarrange(gmass, garea, gSLA, common.legend=T, nrow=1, legend="bottom")






###########################
mN_P <- lme(N_P ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp)
mN <- lme(N ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp)
mP <- lme(P ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp)


samp$fitN_P <- predict(mN_P)   #Add model fits to dataframe
samp$fitN <- predict(mN)   #Add model fits to dataframe
samp$fitP <- predict(mP)   #Add model fits to dataframe


gN_P<-ggplot(samp,aes(scaled, N_P, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitN_P ), linetype="longdash", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("Ratio of N:P")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))
gN_P


gN<-ggplot(samp,aes(scaled, N, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitN ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("Foiar N (mg"~g^-1*")")+
scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))
gN

gP<-ggplot(samp,aes(scaled, P, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitP ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("Foiar P (mg"~g^-1*")")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))
gP

library(ggpubr)
ggarrange(gN, gP, gN_P, common.legend=T, nrow=1, legend="bottom")

############################################
















## Maybe erase below??????



################################################


m.Ala<-lmer(Ala~ dfromtop*Ntrmt*Ptrmt+(1|Tree_ID), data=samp, na.action = na.exclude)

samp$fit.Ala <- predict(m.Ala)   #Add model fits to dataframe
g1<-ggplot(samp,aes(-dfromtop, Ala, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  #  facet_grid(~Ptrmt) +
  geom_line(aes(y=fit.Ala, lty=Ntrmt), size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+ggtitle("model fit lines")
g1

m.Val<-lmer(Val~ dfromtop*Ntrmt*Ptrmt+(1|Tree_ID), data=samp, na.action = na.exclude)
samp$fit.Val <- predict(m.Val)   #Add model fits to dataframe

gV<-ggplot(samp,aes(-dfromtop, Val, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  #  facet_grid(~Ptrmt) +
  geom_line(aes(y=fit.Val, lty=Ntrmt), size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+ggtitle("model fit lines")
gV


m.GABA<-lmer(GABA~ dfromtop*Ntrmt*Ptrmt+(1|Tree_ID), data=samp, na.action = na.exclude)
samp$fit.GABA <- predict(m.GABA)   #Add model fits to dataframe

gB<-ggplot(samp,aes(-dfromtop, GABA, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  #  facet_grid(~Ptrmt) +
  geom_line(aes(y=fit.GABA, lty=Ntrmt), size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+ggtitle("model fit lines")
gB




#################

m.B<-lmer(B~ dfromtop*Ntrmt*Ptrmt+(1|Tree_ID), data=samp, na.action = na.exclude)
samp$fit.B <- predict(m.B)   #Add model fits to dataframe

gb<-ggplot(samp,aes(-dfromtop, B, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  #  facet_grid(~Ptrmt) +
  geom_line(aes(y=fit.B, lty=Ptrmt), size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+ggtitle("model fit lines")+ylab("B (mg/g)")+xlab("Distance from top of crown (m)")
gb

m.Fe<-lmer(Fe~ dfromtop*Ntrmt*Ptrmt+(1|Tree_ID), data=samp, na.action = na.exclude)
samp$fit.Fe <- predict(m.Fe)   #Add model fits to dataframe

gFe<-ggplot(samp,aes(-dfromtop, Fe, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  #  facet_grid(~Ptrmt) +
  geom_line(aes(y=fit.Fe, lty=Ntrmt), size=0.8) +
  scale_color_manual(values= c("Black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+ggtitle("model fit lines")
gFe


m.Al<-lmer(Al~ dfromtop*Ntrmt*Ptrmt+(1|Tree_ID), data=samp, na.action = na.exclude)
samp$fit.Al <- predict(m.Al)   #Add model fits to dataframe

gAl<-ggplot(samp,aes(-dfromtop, Al, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  #  facet_grid(~Ptrmt) +
  geom_line(aes(y=fit.Al, lty=Ptrmt), size=0.8) +
  scale_color_manual(values= c("Black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+ggtitle("model fit lines")
gAl


library(patchwork)
gb+gFe

