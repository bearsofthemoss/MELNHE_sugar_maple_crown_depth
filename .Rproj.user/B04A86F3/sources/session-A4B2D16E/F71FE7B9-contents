## For Figure 6

#
#



library(data.table)
library(tidyr)



samp<-read.csv("better_melnhe_sugar_maple_crown_depth.csv", header=T)
head(samp)

# second<-read.csv("Data/second_collection_MELNHE_SugarMapleCrownDepth.csv")
# second$br<-paste(second$Tree.ID, second$dfromtop)
# 
# second$moisture<-(second$wetmass2_g-second$mass2_g)/second$wetmass2_g
# head(second)
# 
# samp$moisture<-second$moisture[match(samp$br, second$br)]
#write.csv(samp[,c(1:53,55)], file="better_melnhe_sugar_maple_crown_depth.csv")


# IF you want to assess the traits per unit Area rather than Mass
#samp<-samp[,c(14:27)]/samp$SLA
samp$Treatment <- factor(samp$trmt,levels=c("Control","N treatment","P treatment","N+P treatment"))
samp$trmt <- factor(samp$trmt,levels=c("Control","N treatment","P treatment","N+P treatment"))
samp$Tree_ID<-as.numeric(samp$Tree_ID)


tree<-read.csv("Data/Tree_info_MELNHE_sugar_maple_crown.csv")
tree<-tree[, c(1:6)]



library(nlme)
fm2<- lme( N ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action=na.exclude)



inv<- intervals(fm2)


fm2$residuals
# 

str(samp)


#########################


##3 create output for each leaf characteristic
## this gets the coefficients for the fixed effects
aov.lme <- function(y, Stand, Ntrmt, Ptrmt, Tree_ID){
  #1 aov for linear mixed effect model
  library(nlme)
  fm2<- lme( y ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action=na.exclude)
  emm2<-fm2$coefficients$fixed
  em <- as.data.frame(emm2)
    return(em)}



# this is for the coefficients
output.lme<-list()
for(i in c(12:55)){ 
  y = samp[,i]
  Stand= samp$Stand
  Ntrmt=samp$Ntrmt
  Ptrmt=samp$Ptrmt
  output.lme[[i-11]] <- aov.lme(y, Stand, Ntrmt, Ptrmt, Tree_ID)}
d.em<- as.data.frame(rbindlist(output.lme))


d.em

# carry on with d.em for the coefficients!
d.em$`Fixed effect`<-rep(c("intercept","Depth","N addition","P addition", "Depth*N","Depth*P","N:P","Depth*N*P"), 44)




d.em$variable<-rep(names(samp)[c(12:55)], each=8)


write.csv(d.em, file="coefficients_direction.csv")


int<-d.em[d.em$`Fixed effect` == "intercept",]
d.em$int <- int$emm2[match(d.em$variable, int$variable)]

d.em$coefpint<-d.em$emm2+d.em$int

d.em$norm<-d.em$emm2/d.em$int

  
  
  



### add in leaf characteristics


d.em$Group[d.em$variable=="area_cm2"]<-"Physical characteristics"
d.em$Group[d.em$variable=="mass_g"]<-"Physical characteristics"
d.em$Group[d.em$variable=="SLA"]<-"Physical characteristics"

#d.em$Group[d.em$variable=="C"]<-"Elements"
d.em$Group[d.em$variable=="N"]<-"Elements"
d.em$Group[d.em$variable=="Mg"]<-"Elements"
d.em$Group[d.em$variable=="Ca"]<-"Elements"
d.em$Group[d.em$variable=="Al"]<-"Elements"
d.em$Group[d.em$variable=="B"]<-"Elements"
d.em$Group[d.em$variable=="Mn"]<-"Elements"
d.em$Group[d.em$variable=="P"]<-"Elements"
d.em$Group[d.em$variable=="Zn"]<-"Elements"
d.em$Group[d.em$variable=="Fe"]<-"Elements"

#d.em$Group[d.em$variable=="N_P"]<-"Elements"
#d.em$Group[d.em$variable=="S"]<-"Elements"


table(d.em$variable)

d.em$Group[d.em$variable=="total_chl"]<-"Photosynthetic pigments"
d.em$Group[d.em$variable=="carot"]<-"Photosynthetic pigments"

d.em$Group[d.em$variable=="Ala"]<-"Amino acids and soluble protein"
d.em$Group[d.em$variable=="GABA"]<-"Amino acids and soluble protein"
d.em$Group[d.em$variable=="Arg"]<-"Amino acids and soluble protein"
d.em$Group[d.em$variable=="Glu"]<-"Amino acids and soluble protein"
d.em$Group[d.em$variable=="Val"]<-"Amino acids and soluble protein"
d.em$Group[d.em$variable=="protein"]<-"Amino acids and soluble protein"

d.em$Group[d.em$variable=="Put"]<-"Polyamines"
d.em$Group[d.em$variable=="Spd"]<-"Polyamines"
d.em$Group[d.em$variable=="Spm"]<-"Polyamines"


### name updates
d.em[d.em$variable=="total_chl","variable"]<-"Chlorophyll"
d.em[d.em$variable=="carot","variable"]<-"Carotenoids"
d.em[d.em$variable=="mass_g","variable"]<-"leaf mass"
d.em[d.em$variable=="area_cm2","variable"]<-"leaf area"
d.em[d.em$variable=="Put","variable"]<-"Putrescine"
d.em[d.em$variable=="Spm","variable"]<- "Spermine"
d.em[d.em$variable=="Spd","variable"]<-"Spermidine"
d.em[d.em$variable=="Glu","variable"]<-"Glutamine"
d.em[d.em$variable=="Arg","variable"]<-"Arginine"
d.em[d.em$variable=="Ala","variable"]<-"Alanine"
d.em[d.em$variable=="Val","variable"]<-"Valine"






d.em$variable[d.em$variable=="carot"]<-"Carotenoids"
d.em$variable[d.em$variable=="mass_g"]<-"leaf mass"
d.em$variable[d.em$variable=="area_cm2"]<-"leaf area"
d.em$variable[d.em$variable=="Put"]<-"Putrescine"
d.em$variable[d.em$variable=="Spm"]<-"Spermine"
d.em$variable[d.em$variable=="Spd"]<-"Spermidine"
d.em$variable[d.em$variable=="Glu"]<-"Glutamine"
d.em$variable[d.em$variable=="Arg"]<-"Arginine"
d.em$variable[d.em$variable=="Ala"]<-"Alanine"
d.em$variable[d.em$variable=="Val"]<-"Valine"

head(d.em)
table(d.em$variable)

library(ggplot2)
d.em$variable<-factor(d.em$variable, levels=c( "SLA","leaf area","leaf mass","Zn","B","Fe","Al","Mn","Mg","Ca","P","N","Spermidine","Spermine","Putrescine",
                                               "protein","Lys","Ile","Valine","GABA","Proline","Alanine","Arginine","Glutamine",
                                               "Carotenoids","Chlorophyll"))

d.em$Group<-factor(d.em$Group, levels=c("Physical characteristics","Elements","Polyamines","Amino acids and soluble protein","Photosynthetic pigments"))


######################################




table(d.em$`Fixed effect`)


dsca<-subset(d.em, `Fixed effect` == "Depth")
dntr<-subset(d.em, `Fixed effect` == "N addition")
dptr<-subset(d.em, `Fixed effect` == "P addition")
dnhtr<-subset(d.em, `Fixed effect` == "Depth*N")
dphtr<-subset(d.em, `Fixed effect` == "Depth*P")




dnp<-rbind(dsca, dntr, dptr, dnhtr, dphtr)

dnp<-dnp[!is.na(dnp$Group),]


head(dnp)

table(dnp$`Fixed effect`)


dnp$`Fixed effect`<-factor(dnp$`Fixed effect`, levels=c("Depth","N addition","P addition","Depth*N","Depth*P"))

library(ggplot2)

dnp


# couple of clean up vaariable names
ggplot(dnp, aes(x=variable, y=norm, col=`Fixed effect`))+geom_point(position=position_dodge(.2))+facet_wrap(~Group, scales="free_y", ncol=3)+coord_flip()+
  geom_hline(yintercept=0, linetype='dotted', col = 'black')  +scale_color_manual(values=c("black","blue","red","light blue","pink"))+
  xlab("Leaf characteristics")+ylab("Direction and magnitude of response")+theme(legend.position = "bottom")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank())+
  theme(legend.position = c(.85, .25))


