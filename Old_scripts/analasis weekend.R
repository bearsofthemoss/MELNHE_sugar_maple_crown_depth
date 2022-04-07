
## R code for data analysis for sugar maple crown N*P.
## Alex Young   9/18/2021
## alexyoung.116@gmail.com

library(data.table)
library(tidyr)
library(ggplot2)
library(ggpubr)



#samp<-read.csv("Data/MELNHE_SugarMapleCrownDepth.csv")
samp<-read.csv("better_melnhe_sugar_maple_crown_depth.csv", header=T)
###samp$br<-paste(samp$Tree_ID, samp$dfromtop)
head(samp)
samp$Treatment <- factor(samp$trmt,levels=c("Control","N treatment","P treatment","N+P treatment"))
samp$trmt <- factor(samp$trmt,levels=c("Control","N treatment","P treatment","N+P treatment"))
samp$Tree_ID<-as.numeric(samp$Tree_ID)

samp_long<-gather(samp, "chx","value",12:55 )
CV <- function(x) {sd(x,na.rm=T)/mean(x, na.rm=T)}
samp_long
# dvar is per unit mass.
dvar <- aggregate(samp_long$value, 
                  by=list(Char=samp_long$chx,Treatment=samp_long$Treatment, Stand=samp_long$Stand),
                  FUN= CV)
dvar$tree<-paste(dvar$Stand, dvar$Treatment)

cvmeand<-dvar
 ### add in leaf characteristics
cvmeand$Group[cvmeand$Char=="area_cm2"]<-"Physical characteristics"
cvmeand$Group[cvmeand$Char=="mass_g"]<-"Physical characteristics"
cvmeand$Group[cvmeand$Char=="SLA"]<-"Physical characteristics"
cvmeand$Group[cvmeand$Char=="moisture"]<-"Physical characteristics"

cvmeand$Group[cvmeand$Char=="Chl_A"]<-"Photoproteins"
cvmeand$Group[cvmeand$Char=="Chl_B"]<-"Photoproteins"
cvmeand$Group[cvmeand$Char=="carot"]<-"Photoproteins"
cvmeand$Group[cvmeand$Char=="Chl_R"]<-"Photoproteins"
cvmeand$Group[cvmeand$Char=="protein"]<-"Photoproteins"


cvmeand$Group[cvmeand$Char=="Ala"]<-"Amino acids"
cvmeand$Group[cvmeand$Char=="GABA"]<-"Amino acids"
cvmeand$Group[cvmeand$Char=="Glu"]<-"Amino acids"
cvmeand$Group[cvmeand$Char=="Val"]<-"Amino acids"
cvmeand$Group[cvmeand$Char=="Arg"]<-"Amino acids"
cvmeand$Group[cvmeand$Char=="Pro"]<-"Amino acids"

cvmeand$Group[cvmeand$Char=="Put"]<-"Polyamines"
cvmeand$Group[cvmeand$Char=="Spd"]<-"Polyamines"
cvmeand$Group[cvmeand$Char=="Spm"]<-"Polyamines"

cvmeand$Group[cvmeand$Char=="C"]<-"Elements"
cvmeand$Group[cvmeand$Char=="Mg"]<-"Elements"
cvmeand$Group[cvmeand$Char=="Ca"]<-"Elements"
cvmeand$Group[cvmeand$Char=="Al"]<-"Elements"
cvmeand$Group[cvmeand$Char=="B"]<-"Elements"
cvmeand$Group[cvmeand$Char=="Mn"]<-"Elements"
cvmeand$Group[cvmeand$Char=="N"]<-"Elements"
cvmeand$Group[cvmeand$Char=="N_P"]<-"Elements"
cvmeand$Group[cvmeand$Char=="P"]<-"Elements"
cvmeand$Group[cvmeand$Char=="S"]<-"Elements"
cvmeand$Group[cvmeand$Char=="Zn"]<-"Elements"




library(ggplot2)
cvmeand$Group<-factor(cvmeand$Group, levels=c("Physical characteristics","Photoproteins","Elements","Amino acids","Polyamines"))

ggplot(cvmeand, aes(x=Char, y=x, shape=Stand, col=Treatment))+geom_point()+coord_flip()+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position="right",legend.box="vertical", legend.margin=margin())+theme(plot.title = element_text(hjust = 0.5))+
facet_wrap(~Group, nrow=7, scales="free_y")
  


###################
#cvmeand<-na.omit(cvmeand)
#cvmeand$Char<-factor(cvmeand$Char, levels=("SLA", "area_cm2","mass_g","Zn","Mg","Ca","P","B","N","Mn","N_P","C","Al","Glu","Ala","GABA","Val","protein","Put","Spm","Spd","total_chl","carot","Chl_R"))

cvmeand$Char<-factor(cvmeand$Char, levels=c("Spm","Spd","Put","Pro","Val","Glu","Arg","GABA","Ala","S","Zn","B","Al","Mn","Mg","Fe","Ca","N_P","P","N","C",
                                            "Chl_R","protein","carot","Chl_B","Chl_A","SLA","moisture","area_cm2","mass_g"))
# without facet wrap
ggplot(cvmeand, aes(x=Char, y=x, shape=Group, col=Treatment))+geom_point()+coord_flip()+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position="right",legend.box="vertical", legend.margin=margin())+theme(plot.title = element_text(hjust = 0.5))+
  ylab("Coefficient of variation")+xlab("Leaf characteristics")

avg<-aggregate(cvmeand$x, by=list(Char=cvmeand$Char,Group=cvmeand$Group ), FUN="mean")
avg$Char<-factor(avg$Char, levels=c("Spm","Spd","Put","Pro","Val","Glu","Arg","GABA","Ala","S","Zn","B","Al","Mn","Mg","Fe","Ca","N_P","P","N","C",
                                            "Chl_R","protein","carot","Chl_B","Chl_A","SLA","moisture","area_cm2","mass_g"))
st.err <- function(x) {sd(x)/sqrt(length(x))}
SE <- aggregate(cvmeand$x, by=list(Char=cvmeand$Char,Group=cvmeand$Group ), FUN=st.err)
avg$se<-SE$x

# without facet wrap
ggplot(avg, aes(x=Char, y=x, shape=Group))+
  #geom_errorbar(aes(ymin=x-se, ymax=x+se), width=.4)+ 
  geom_point(size=4)+coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position="right",legend.box="vertical", legend.margin=margin())+theme(plot.title = element_text(hjust = 0.5))+
  ylab("Coefficient of variation")+xlab("Leaf characteristics")+
  scale_shape_manual(values=c(0,1,2,5,7,8))


# #### try it per unit area
# pre_samp_area<-samp[,c(15:55)]/samp$SLA
# samp_area<-cbind(samp[,c(1:14)],pre_samp_area)
# samp_area$Treatment <- factor(samp_area$trmt,levels=c("Control","N treatment","P treatment","N+P treatment"))
# names(samp_area)
# dim(samp)
# dim(samp_area)
# 
# names(samp_area)
# samp_long_area<-gather(samp_area, "chx","value",12:55 )
# names(samp_long_area)
# 
# avar <- aggregate(samp_long_area$value, 
#                   by=list(Char=samp_long_area$chx,Treatment=samp_long_area$Treatment, Stand=samp_long_area$Stand),
#                   FUN= CV)
# avar$tree<-paste(avar$Stand, avar$Treatment)
# 
# cvmeand$area<-avar$x
# 
# ######### the two of them?
# ggplot(cvmeand, aes(x=Char, y=area, shape=Group, col=Treatment))+geom_point()+coord_flip()+
#   scale_color_manual(values=c("black","blue","red", "purple"))+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#   theme(legend.position="right",legend.box="vertical", legend.margin=margin())+theme(plot.title = element_text(hjust = 0.5))+
#   ylab("Coefficient of variation")+xlab("Leaf characteristics")+ggtitle("Variability of leaf characteristics per unit mass")+ylim(0,1.5)






