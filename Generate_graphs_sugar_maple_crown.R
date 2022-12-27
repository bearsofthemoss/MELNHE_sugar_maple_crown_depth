




library(ggplot2)
library(nlme)
library(ggpubr)


getwd()
samp<-read.csv("better_melnhe_sugar_maple_crown_depth.csv", header=T)

samp$Treatment <- factor(samp$trmt,levels=c("Control","N treatment","P treatment","N+P treatment"))
samp$trmt <- factor(samp$trmt,levels=c("Control","N treatment","P treatment","N+P treatment"))
samp$Tree_ID<-as.numeric(samp$Tree_ID)



###########  SLA, mass, area
head(samp$protein)

mSLA <- lme(SLA ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp)
mmass <- lme(mass_g ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp)
marea <- lme(area_cm2 ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp)


samp$fitSLA <- predict(mSLA)   #Add model fits to dataframe
samp$fitmass <- predict(mmass)   #Add model fits to dataframe
samp$fitarea <- predict(marea)   #Add model fits to dataframe


gmass<-ggplot(samp,aes(scaled, mass_g, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitmass ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
 # geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("leaf mass (g)")+
  scale_x_continuous(expand = c(0, 0.05),breaks=seq(0,1,.2)) +
  scale_y_continuous(expand = c(0.05, 0))
gmass



garea<-ggplot(samp,aes(scaled, area_cm2, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitarea ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
 # geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  labs(y=bquote('leaf area'~(cm^2)))+
  scale_x_continuous(expand = c(0, 0.05),breaks=seq(0,1,.2) ) +
  scale_y_continuous(expand = c(0.05, 0))
garea


gSLA<-ggplot(samp,aes(scaled, SLA, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitSLA ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  #geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  labs( y= bquote('SLA'~(cm^2 ~g^-1)))+
  scale_x_continuous(expand = c(0, 0.05),breaks=seq(0,1,.2)) +
  scale_y_continuous(expand = c(0.05, 0.0))
gSLA



f1<-ggarrange(gmass, garea, gSLA, common.legend=T, nrow=1, legend="bottom")

f1
dpi=300    #pixels per square inch
tiff("Fig_1_sm_crown_11_27_2022.tif", width=10*dpi, height=5*dpi, res=dpi)
f1
dev.off()



################################################

## elements

mN <- lme(N ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitN <- predict(mN)   #Add model fits to dataframe
gN<-ggplot(samp,aes(scaled, N, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitN ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  #geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("N (mg "~g^-1*")")+
  scale_x_continuous(expand = c(0, 0.05), breaks=seq(0,1,.2)) +
  scale_y_continuous(expand = c(0.05, 0))
gN

mP <- lme(P ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitP <- predict(mP)   #Add model fits to dataframe
gP<-ggplot(samp,aes(scaled, P, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitP ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  # geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("P (mg "~g^-1*")")+
  scale_x_continuous(expand = c(0, 0.05), breaks=seq(0,1,.2)) +
  scale_y_continuous(expand = c(0.05, 0))
gP

mCa <- lme(Ca ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitCa <- predict(mCa)   #Add model fits to dataframe
gCa<-ggplot(samp,aes(scaled, Ca, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitCa ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  # geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("Ca (mg "~g^-1*")")+
  scale_x_continuous(expand = c(0, 0.05), breaks=seq(0,1,.2)) +
  scale_y_continuous(expand = c(0.05, 0))
gCa


mMn <- lme(Mn ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitMn <- predict(mMn)   #Add model fits to dataframe
gMn<-ggplot(samp,aes(scaled, Mn, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitMn ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  #  geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("Mn (mg "~g^-1*")")+
  scale_x_continuous(expand = c(0, 0.05), breaks=seq(0,1,.2))+ 
  scale_y_continuous(expand = c(0.05, 0))
gMn

mAl <- lme(Al ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitAl <- predict(mAl)   #Add model fits to dataframe
gAl<-ggplot(samp,aes(scaled, Al, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitAl ), linetype="longdash", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  # geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("Al (mg "~g^-1*")")+
  scale_x_continuous(expand = c(0, 0.05), breaks=seq(0,1,.2))+ 
  scale_y_continuous(expand = c(0.05, 0))
gAl

mB <- lme(B ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitB <- predict(mB)   #Add model fits to dataframe
gB<-ggplot(samp,aes(scaled, B, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitB ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  #  geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("B (mg "~g^-1*")")+
  scale_x_continuous(expand = c(0, 0.05), breaks=seq(0,1,.2)) +
  scale_y_continuous(expand = c(0.05, 0))
gB

mFe <- lme(Fe ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitFe <- predict(mFe)   #Add model fits to dataframe
gFe<-ggplot(samp,aes(scaled, Fe, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitFe ), linetype="longdash", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  # geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("Fe (mg "~g^-1*")")+
  scale_x_continuous(expand = c(0, 0.05), breaks=seq(0,1,.2))+ 
  scale_y_continuous(expand = c(0.05, 0))
gFe

mZn <- lme(Zn ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitZn <- predict(mZn)   #Add model fits to dataframe
gZn<-ggplot(samp,aes(scaled, Zn, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitZn ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  # geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("Zn (mg "~g^-1*")")+
  scale_x_continuous(expand = c(0, 0.05), breaks=seq(0,1,.2))+ 
  scale_y_continuous(expand = c(0.05, 0))
gZn


f3<-ggarrange(gN,gMn, gAl, gFe,
          gCa,gP,gB,  gZn,
          common.legend=T, ncol=4, nrow=2, legend="bottom")



f3
dpi=300    #pixels per square inch
tiff("Fig_3_sm_crown_11_27_2022.tif", width=10*dpi, height=5*dpi, res=dpi)
f3
dev.off()


############################################


# Amino acids
###########################
mAla <- lme(Ala ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitAla <- predict(mAla)   #Add model fits to dataframe
gAla<-ggplot(samp,aes(scaled, Ala, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitAla ), linetype="longdash", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
 #  geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("Ala (nmol "~g^-1*") FW")+
  scale_x_continuous(expand = c(0, 0.05),breaks=seq(0,1,.2)) +
  scale_y_continuous(expand = c(0.05, 0))
gAla

mGaba <- lme(GABA ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitGaba <- predict(mGaba)   #Add model fits to dataframe
gGaba<-ggplot(samp,aes(scaled, GABA, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitGaba ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
 # geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("GABA (nmol "~g^-1*") FW")+
  scale_x_continuous(expand = c(0, 0.05),breaks=seq(0,1,.2)) +
  scale_y_continuous(expand = c(0.05, 0))
gGaba

mVal <- lme(Val ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitVal <- predict(mVal)   #Add model fits to dataframe
gVal<-ggplot(samp,aes(scaled, Val, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitVal ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
 # geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("Val (nmol "~g^-1*") FW")+
  scale_x_continuous(expand = c(0, 0.05),breaks=seq(0,1,.2)) +
  scale_y_continuous(expand = c(0.05, 0))
gVal

mArg <- lme(Arg ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitArg <- predict(mArg)   #Add model fits to dataframe
gArg<-ggplot(samp,aes(scaled, Arg, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitArg ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  #geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("Arg (nmol "~g^-1*") FW")+
  scale_x_continuous(expand = c(0, 0.05),breaks=seq(0,1,.2)) +
  scale_y_continuous(expand = c(0.05, 0))
gArg

mIle <- lme(Ile ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitIle <- predict(mIle)   #Add model fits to dataframe
gIle<-ggplot(samp,aes(scaled, Ile, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitIle ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
 # geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("Ile (nmol "~g^-1*") FW")+
  scale_x_continuous(expand = c(0, 0.05),breaks=seq(0,1,.2)) +
  scale_y_continuous(expand = c(0.05, 0))
gIle

mLys <- lme(Lys ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitLys <- predict(mLys)   #Add model fits to dataframe
gLys<-ggplot(samp,aes(scaled, Lys, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitLys ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1)+ 
  #geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("Lys (nmol "~g^-1*") FW")+
  scale_x_continuous(expand = c(0, 0.05),breaks=seq(0,1,.2)) +
  scale_y_continuous(expand = c(0.05, 0))
gLys

mPro <- lme(Pro ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitPro <- predict(mPro)   #Add model fits to dataframe
gPro<-ggplot(samp,aes(scaled, Pro, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitPro ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
 # geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("Pro (nmol "~g^-1*") FW")+
  scale_x_continuous(expand = c(0, 0.05),breaks=seq(0,1,.2)) +
  scale_y_continuous(expand = c(0.05, 0))
gPro

mGlu <- lme(Glu ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitGlu <- predict(mGlu)   #Add model fits to dataframe
gGlu<-ggplot(samp,aes(scaled, Glu, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitGlu ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
 # geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("Glu (nmol "~g^-1*") FW")+
  scale_x_continuous(expand = c(0, 0.05),breaks=seq(0,1,.2)) +
  scale_y_continuous(expand = c(0.05, 0))
gGlu


## protein
mprot <- lme(protein ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitprot <- predict(mprot)   #Add model fits to dataframe

gprot<-ggplot(samp,aes(scaled, protein, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitprot ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  #geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("Soluble protein (nmol "~g^-1*") FW")+
  scale_x_continuous(expand = c(0, 0.05),breaks=seq(0,1,.2)) +
  scale_y_continuous(expand = c(0.05, 0))
gprot



library(ggpubr)

f4<-  ggarrange(gGlu, gArg, gGaba , gAla ,gVal, gprot, common.legend=T, nrow=2,ncol=3, legend="bottom")
f4

dpi=300    #pixels per square inch
tiff("Fig_4_sm_crown_11_27_2022.tif", width=10*dpi, height=5*dpi, res=dpi)
f4
dev.off()
  
#######################################################


##

#Poly's'
mPut <- lme(Put ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitPut <- predict(mPut)   #Add model fits to dataframe
gPut<-ggplot(samp,aes(scaled, Put, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitPut ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  #geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("Put (nmol "~g^-1*") FW")+
  scale_x_continuous(expand = c(0, 0.05), breaks=seq(0,1,.2)) +
  scale_y_continuous(expand = c(0.05, 0))
gPut

mSpd <- lme(Spd ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitSpd <- predict(mSpd)   #Add model fits to dataframe
gSpd<-ggplot(samp,aes(scaled, Spd, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitSpd ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
 # geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("Spd (nmol "~g^-1*") FW")+
  scale_x_continuous(expand = c(0, 0.05), breaks=seq(0,1,.2)) +
  scale_y_continuous(expand = c(0.05, 0))
gSpd

mSpm <- lme(Spm ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitSpm <- predict(mSpm)   #Add model fits to dataframe
gSpm<-ggplot(samp,aes(scaled, Spm, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitSpm ), linetype="longdash", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
 # geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("Spm (nmol "~g^-1*") FW")+
  scale_x_continuous(expand = c(0, 0.05), breaks=seq(0,1,.2)) +
  scale_y_continuous(expand = c(0.05, 0))
gSpm


library(ggpubr)
f5<- ggarrange(gPut, gSpd, gSpm,common.legend=T, ncol=3, legend="bottom")
f5

dpi=300    #pixels per square inch
tiff("Fig_5_sm_crown_11_27_2022.tif", width=10*dpi, height=5*dpi, res=dpi)
f5
dev.off()


#######




## Photoproteins

mChl <- lme(total_chl ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitChl <- predict(mChl)   #Add model fits to dataframe
gChl<-ggplot(samp,aes(scaled, total_chl, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitChl ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  #geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("Chlorophyll (nmol "~g^-1*") FW")+
  scale_x_continuous(expand = c(0, 0.05)) +
  scale_y_continuous(expand = c(0.05, 0))
gChl




mcarot <- lme(carot ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitcarot <- predict(mcarot)   #Add model fits to dataframe
gcarot<-ggplot(samp,aes(scaled, carot, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitcarot ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  #geom_hline(yintercept=0, linetype="solid") +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("Carotenoids (nmol "~g^-1*") FW")+
  scale_x_continuous(expand = c(0, 0.05)) +
  scale_y_continuous(expand = c(0.05, 0))
gcarot


f6<-ggarrange(gChl, gcarot,common.legend=T, ncol=2, legend="bottom")

f6

dpi=300    #pixels per square inch
tiff("Fig_6_sm_crown_11_27_2022.tif", width=10*dpi, height=5*dpi, res=dpi)
f6
dev.off()




library(emmeans)

emmeans(mN, pairwise~ Ntrmt)

(21.3-17.0)/mean(c(17)) # 25%

emmeans(mFe, pairwise~ Ntrmt)
(0.592-0.419)/mean(c(0.419)) # 41

emmeans(mMn, pairwise~ Ntrmt)
(2.5-1.52)/mean(c(1.52)) # 64%

emmeans(mAl, pairwise~ Ntrmt)
(0.0274-0.0194)/mean(c(0.0194))# 41%

emmeans(mAla, pairwise~ Ntrmt)
(486-354)/mean(c(354))# 37%

# P addtion
emmeans(mP, pairwise~ Ptrmt)
(1.87-1.15)/mean(c(1.15))# 63%

emmeans(mB, pairwise~ Ptrmt)
(0.0387-0.0268)/mean(c(0.0268))# 44%

emmeans(mN_P, pairwise~ Ntrmt*Ptrmt)

#NP with N
(15.7-12.7)/(12.7) # 23 % higher

#NP with P # 50% lower
(11.3-17)/(mean(17)) # 34% lower

#NP NP compared to control
(13.89-16.59) /(16.59) # 16% lower




gPro
gAla

gP
gFe
gN_P



######################


mC <- lme(C ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitC <- predict(mC)   #Add model fits to dataframe
gC<-ggplot(samp,aes(scaled, C, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitC ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("C (mg "~g^-1*")")
gC


samp$CN<-samp$C/samp$N
mCN <- lme(CN ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action = na.exclude)
samp$fitCN <- predict(mCN)   #Add model fits to dataframe
gCN<-ggplot(samp,aes(scaled, CN, group=interaction(Tree_ID, Stand), col=Treatment, shape=Stand )) + 
  geom_line(aes(y=fitCN ), linetype="solid", size=0.8) +
  scale_color_manual(values= c("black","blue","red","purple"))+
  geom_point(alpha = 1) + 
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("Depth in the crown")+
  ylab("C : N")
gCN




ggarrange(gC, gCN,
          common.legend=T, ncol=2, nrow=1, legend="bottom")

