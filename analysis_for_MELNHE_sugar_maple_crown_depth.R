## R code for data analysis for sugar maple crown N*P.
## Alex Young   4/7/2020
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

# tree level information
# tree<-read.csv("Data/Tree_info_MELNHE_sugar_maple_crown.csv")
# tree<-tree[, c(1:6)]

# moisture analysis
# second<-read.csv("Data/second_collection_MELNHE_SugarMapleCrownDepth.csv")
# second$br<-paste(second$Tree.ID, second$dfromtop)
# second$moisture<-(second$wetmass2_g-second$mass2_g)/second$wetmass2_g
# samp$moisture<-second$moisture[match(samp$br, second$br)]
# write.csv(samp[,c(1:53,55)], file="better_melnhe_sugar_maple_crown_depth.csv")
# ggplot(samp, aes(x=scaled, col=Treatment,y=moisture))+geom_point()+
#  scale_color_manual(values=c("black","blue","red","purple"))+  facet_wrap(~Stand)+coord_flip()+  geom_smooth(method="lm")+
#  labs(y = bquote('Moisture content (%)'), x = bquote('Depth in crown '~(scaled)))+
#  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))+  theme_classic()+ggtitle("Dry to wet mass comparison for second collection of leaves")



# CV for leaf char- go to long format
names(samp)
samp_long<-gather(samp, "chx","value",12:55 )
CV <- function(x) {sd(x,na.rm=T)/mean(x, na.rm=T)}

# dvar is per unit mass.
dvar <- aggregate(samp_long$value, 
        by=list(Char=samp_long$chx, Ntrmt=samp_long$Ntrmt,Ptrmt=samp_long$Ptrmt, 
                Treatment=samp_long$Treatment, Stand=samp_long$Stand),
        FUN= CV)
dvar

## avar is per unit area.
# IF you want to assess the traits per unit Area rather than Mass
pre_samp_area<-samp[,c(15:55)]/samp$SLA
samp_area<-cbind(samp[,c(1:14)],pre_samp_area)
samp_area$Treatment <- factor(samp_area$trmt,levels=c("Control","N treatment","P treatment","N+P treatment"))

names(samp_area)
dim(samp)
dim(samp_area)

names(samp_area)
samp_long_area<-gather(samp_area, "chx","value",12:55 )
names(samp_long_area)

avar <- aggregate(samp_long_area$value, 
                  by=list(Char=samp_long_area$chx, Ntrmt=samp_long_area$Ntrmt,Ptrmt=samp_long_area$Ptrmt, 
                          Treatment=samp_long_area$Treatment, Stand=samp_long_area$Stand),
                  FUN= CV)
avar


g1<-ggplot(dvar, aes(x=Char, y=x, col=Treatment, group=Stand))+
  geom_point()+coord_flip()+ggtitle("CV of tree leaf characteristics per unit mass")+
  scale_color_manual(values=c("black","blue","red","purple"))

g2<-ggplot(avar, aes(x=Char, y=x, col=Treatment, group=Stand))+
  geom_point()+coord_flip()+ggtitle("CV of tree leaf characteristics per unit area")+
  scale_color_manual(values=c("black","blue","red","purple"))

ggarrange(g1, g2, common.legend=T, nrow=2)

ggplot(avar, aes(x=Treatment, y=x, col=Treatment, group=Stand))+
  geom_bar(stat="identity",position=position_dodge())+facet_wrap(~Char, scales="free")

head(avar)
dvar$area.cv<-avar$x
head(dvar)

ggplot(dvar, aes(x=x, y=area.cv, col=Char, label=Char, group=Stand))+
  geom_point()+
  #scale_color_manual(values=c("black","blue","red","purple"))+
  geom_abline()+geom_text(aes(label=Char),hjust=0, vjust=0)


ggplot(dvar, aes(x=x, y=area.cv, col=Treatment, label=Char, group=Stand))+
  geom_point()+
  scale_color_manual(values=c("black","blue","red","purple"))+
  geom_abline()+facet_wrap(~Char)


#geom_text(aes(label=Char),hjust=0, vjust=0)


#### overall CV  for avar and dvar
cvmeana <- aggregate(avar$x, 
                  by=list(Char=avar$Char),FUN= CV)

cvmeand <- aggregate(dvar$x, 
                     by=list(Char=avar$Char),FUN= CV)

cvmeand$area<-cvmeana$x

ggplot(cvmeand, aes(x=x, y=area, label=Char))+geom_point()+geom_abline()+
  geom_text(aes(label=Char),hjust=0, vjust=0)





# Additional formating
abcd<- as.data.frame(rbindlist(output.mass))
abcd
colnames(abcd)<-c("Tree_ID","Stand","t.height.m","trmt","Ntrmt","Ptrmt","intercept","slope")
abcd$trmt<-factor(abcd$trmt, levels=c("Con","N","P","NP"))

head(abcd)

names(samp)

abcd$var.names<-rep(names(samp)[c(12:55)], each=12)



## gather the slope and intercept column into a longer dataframe
head(abcd)
abcd.g<-gather(abcd,"var","value",7:8)

head(abcd.g)

# spread dataframe so slope or intercept is in one column
sm.mass<-spread(abcd.g, var.names , value)
head(sm.mass)
# make slope dataframe
sm.slo<-sm.mass[sm.mass$var=="slope",]
# make intercept dataframe
sm.int<-sm.mass[sm.mass$var=="intercept",]
# make average dataframe 
sm.a<-aggregate(samp[ ,c(12:55)], list(Tree_ID=samp$Tree_ID), FUN = mean, na.rm=T)




## add in tree information 
sm.a$Stand<-tree$Stand[match(sm.a$Tree_ID,tree$Tree_ID)]
sm.a$dbh.cm<-tree$dbh.cm[match(sm.a$Tree_ID,tree$Tree_ID)]
sm.a$t.height.m<-tree$t.height.m[match(sm.a$Tree_ID,tree$Tree_ID)]
sm.a$trmt<-tree$trmt[match(sm.a$Tree_ID,tree$Tree_ID)]
sm.a$Ntrmt<-tree$Ntrmt[match(sm.a$Tree_ID,tree$Tree_ID)]
sm.a$Ptrmt<-tree$Ptrmt[match(sm.a$Tree_ID,tree$Tree_ID)]
sm.a$var<-c("ave")


# re-order sm.a to be consistent with sm.int and sm.slo
dim(sm.a)
dim(sm.a)
names(sm.a)
names(sm.int)


sm.a.order<-sm.a[ ,c(1, 46:51,2:45)]
names(sm.a.order)
dim(sm.a.order)
## quick spread and gather to alphabatize the var names
names(sm.a.order)
dim(sm.a.order)
spra<-gather(sm.a.order, "var.names","value",8:51) #
sm.ave<-spread(spra, var.names , value)

names(sm.ave)
##########################
sm.int$trmt<-factor(sm.int$trmt, levels=c("Con","N","P","NP"))
sm.ave$trmt<-factor(sm.ave$trmt, levels=c("Con","N","P","NP"))

##################################################################################

ggplot(sm.int, aes(x=trmt, col=trmt,y=moisture))+geom_point()+
  scale_color_manual(values=c("black","blue","red","purple"))+
  facet_wrap(~Stand)+
  labs(y = bquote('Moisture content (%)'), x = bquote('Depth in crown '~(scaled)))+
#  scale_x_reverse( lim=c(1,0), breaks=seq(0,1,.5))+
  theme_classic()+ggtitle("Moisture content for second collection of leaves")





############################################################################################
## check point!
# spot check
head(sm.int[ ,1:10])
head(sm.slo[ ,1:10])
head(sm.ave[ ,1:10])
dim(sm.slo) # Each should have 12 rows, 51 columns
dim(sm.int)
dim(sm.ave)

# ###########
# ###  write out to .csv for easier graphing or inspection
# write.csv(sm.slo, "mass_scaled_slopes_4_15_21.csv")
# write.csv(sm.ave, "mass_scaled_average_4_15_21.csv")
# write.csv(sm.int, "mass_scaled_intercepts_4_15_21.csv")
# ###########################################################
#


# model m3 is made using the package nlme
# m3 is a mixed effect model Fixed is N*P, random effect is Stand
library(nlme)
m3 <- lme(Al ~ Ntrmt*Ptrmt, random=~1|Stand, data=sm.ave)
summary(m3)
anova(m3)



###############################################
### Functions for statistical anova
aov.slope <- function(y, Stand, Ntrmt, Ptrmt){
  #1 aov for slope
  slope1<-anova(lmer(y~ Ntrmt*Ptrmt+(1|Stand)))
  return(slope1)}

aov.intc <- function(y, Stand, Ntrmt, Ptrmt){
  #1 aov for intercept
  intc1<-anova(lmer(y~ Ntrmt*Ptrmt+(1|Stand)))
  return(intc1)}

aov.ave <- function(y, Stand, Ntrmt, Ptrmt){
  #1 aov for average
  ave1<-anova(lmer(y~ Ntrmt*Ptrmt+(1|Stand)))
  return(ave1)}


names(sm.slo)

#####################################################
##3 creat output of functions
output.slo.mass<-list()
for(i in c(8:51)){ 
  y = sm.slo[,i]
  Stand= sm.slo$Stand
  Ntrmt=sm.slo$Ntrmt
  Ptrmt=sm.slo$Ptrmt
  output.slo.mass[[i-7]] <- aov.slope(y, Stand, Ntrmt, Ptrmt)}
##3 c
output.intc.mass<-list()
names(sm.int)
for(i in c(8:51)){ 
  y = sm.int[,i]
  Stand= sm.int$Stand
  Ntrmt=sm.int$Ntrmt
  Ptrmt=sm.int$Ptrmt
  output.intc.mass[[i-7]] <- aov.intc(y, Stand, Ntrmt, Ptrmt)}
##  average aov for loop

names(sm.ave)
output.mass.ave<-list()
for(i in c(8:51)){ 
  y = sm.ave[,i]
  Stand= sm.ave$Stand
  Ntrmt=sm.ave$Ntrmt
  Ptrmt=sm.ave$Ptrmt
  output.mass.ave[[i-7]] <- aov.ave(y, Stand, Ntrmt, Ptrmt)}

# INTERCEPT
d.int<- as.data.frame(rbindlist(output.intc.mass))
d.int$Source<-rep(c("N","P","N*P"))
d.int$variable<-rep(names(sm.int)[c(8:51)], each=3)
d.int$type<-c("Intercept")
d.int<-d.int[ ,c(9,7,8,1,2,3,4,5,6)]

d.int

# SLOPE
d.slo<- as.data.frame(rbindlist(output.slo.mass))
d.slo$Source<-rep(c("N","P","N*P"))
d.slo$variable<-rep(names(sm.slo)[c(8:51)], each=3)
d.slo$type<-c("Slope")
d.slo<-d.slo[ ,c(9,7,8,1,2,3,4,5,6)]


# AVERAGE
d.ave<- as.data.frame(rbindlist(output.mass.ave))
d.ave$Source<-rep(c("N","P","N*P"))
d.ave$variable<-rep(names(sm.ave)[c(8:51)], each=3)
d.ave$type<-c("Average")
d.ave<-d.ave[ ,c(9,7,8,1,2,3,4,5,6)]

dim(d.slo) #215 rows, 8 col
dim(d.ave)
dim(d.int)


pism<-rbind( d.slo , d.int, d.ave)

pism
## add leaf characteristic
pism$leaf.char[pism$variable=="area_cm2"]<-"Physical characteristics"
pism$leaf.char[pism$variable=="mass_g"]<-"Physical characteristics"
pism$leaf.char[pism$variable=="SLA"]<-"Physical characteristics"

pism$leaf.char[pism$variable=="total_chl"]<-"Photosynthetic pigments"
pism$leaf.char[pism$variable=="carot"]<-"Photosynthetic pigments"
pism$leaf.char[pism$variable=="Chl_R"]<-"Photosynthetic pigments"

pism$leaf.char[pism$variable=="Ala"]<-"Amino acids"
pism$leaf.char[pism$variable=="GABA"]<-"Amino acids"
pism$leaf.char[pism$variable=="Glu"]<-"Amino acids"
pism$leaf.char[pism$variable=="Val"]<-"Amino acids"

pism$leaf.char[pism$variable=="Put"]<-"Polyamines"
pism$leaf.char[pism$variable=="Spd"]<-"Polyamines"
pism$leaf.char[pism$variable=="Spm"]<-"Polyamines"

pism$leaf.char[pism$variable=="C"]<-"Elements"
pism$leaf.char[pism$variable=="Mg"]<-"Elements"
pism$leaf.char[pism$variable=="Ca"]<-"Elements"
pism$leaf.char[pism$variable=="Al"]<-"Elements"
pism$leaf.char[pism$variable=="B"]<-"Elements"
pism$leaf.char[pism$variable=="Mn"]<-"Elements"
pism$leaf.char[pism$variable=="N"]<-"Elements"
pism$leaf.char[pism$variable=="N_P"]<-"Elements"
pism$leaf.char[pism$variable=="P"]<-"Elements"
pism$leaf.char[pism$variable=="S"]<-"Elements"
pism$leaf.char[pism$variable=="Zn"]<-"Elements"

head(pism)
#write.csv(pism, file="supplemental_table_2.csv")

#### DO P VALUE CORRECTION

a<-p.adjust(pism$`Pr(>F)`, method ="BH", n = length(pism$`Pr(>F)`))
table(a<0.05)

a

p.adjust.methods
# c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
#   "fdr", "none")




###############################################################################
pval<-spread(pism[ ,c("leaf.char","variable","Source","type","Pr(>F)")], Source,'Pr(>F)')
head(pval)

pval<-pval[ ,c(1,3,2,4,6,5)]

fp<-cbind(pval[pval$type=="Slope",],pval[pval$type=="Intercept",],pval[pval$type=="Average",] )
head(fp)
fp<-fp[ ,c(1,3,2,4,5,6,8,10,11,12,14,16,17,18)]
head(fp)



t.test(sm.slo$Chl_A)
names(sm.slo)
ggplot(sm.slo, aes(x=trmt, y=Chl_B))+geom_point()

####################################
## overall slope test
slope.zero <- function(y){
  a<- t.test(y)
  p.val<-a$p.value
  mean<-a$estimate[1]
  low.cf<-a$conf.int[1]
  upp.cf<-a$conf.int[2]
  variable<-paste(colnames(sm.slo[i]))
  e<-cbind(variable, p.val, mean, low.cf, upp.cf)
  return(e)}

out.t<-list()
for(i in c(8:51)){ 
  y = sm.slo[,i]
  out.t[[i-7]] <- slope.zero(y)}

load <- data.frame()
for(i in 1:length(out.t)){
  x <- out.t[[i]]
  x <- data.frame(y=as.matrix(x))
  load <- rbind(load, x)}

head(load,40)

head(fp)


fp$mean.slope<-load$y.mean[match(fp$variable, load$y.variable)]
fp$slope.pval<-load$y.p.val[match(fp$variable, load$y.variable)]

head(fp)

names(fp)
fp<-fp[ ,c(1,2,15,16,3:14)]
head(fp)
names(fp)


## order fp by the order of variables. 

order<-seq(1:43)
name<-c("mass_g",	"area_cm2"	,"SLA"	,"protein","total_chl",	"carot"	, "Chl_R"	,"C","N","P","N_P",	"Ca","Mg","Mn","Al",	"B",	"Zn",	"S"	,	"Ala",	"GABA",	"Glu",	"Val",	"Put",	"Spd"	,"Spm"	,"Chl_A",	"Chl_B"	,"Fe"	,"K"	,"Sr",	"Arg",	"Asp",	"Ile",	"Leu"	,"Lys",	"Pro"		,"s_Al"	,"s_Ca"	,"s_K",	"s_Mn",	"s_P"	,"s_Mg"	,"s_Zn")
length(name)
vord<-as.data.frame(order, name)
vord$name<-rownames(vord)
vord$order<-as.numeric(vord$order)

str(vord)
vord

fp$vord<-vord$order [match(fp$variable, vord$name)]
fp<-fp[order(fp$vord),]
head(fp)

fp[10:20,]


summary(samp$area_cm2)
summary(samp$mass_g)

(50.3-34.37) / 44.14
(0.54-0.45)/0.4987


write.csv(fp, file="thesis_p_values_4_17_2020.csv")





######################################################
# look here to normalize its slope by the IQR
sm.slo
head(sm.slo)
names(samp)
dim(samp)
###  get IQR for each tree
sm.iqr<-aggregate(samp[ ,c(12:54)], list(Tree_ID=samp$Tree_ID, Stand=samp$Stand), FUN = IQR, na.rm=T)

tail(sm.iqr)

head(samp)


# gather sm.slo and sm.iqr
names(sm.iqr)
giqr<-gather(sm.iqr, "variable","value",3:45)
head(giqr)
dim(giqr)
names(sm.slo)
gslo<-gather(sm.slo, "variable","value",8:51)
dim(gslo)

#formatting


gslo$slope<-as.numeric(gslo$value)
names(gslo)

giqr$unique<-paste(giqr$Tree_ID, giqr$Stand, giqr$variable)
head(giqr$unique)
gslo$unique<-paste(gslo$Tree_ID,gslo$Stand, gslo$variable)

## Make giqr half as long?
length(giqr$unique)
length(gslo$unique)




giqr[giqr$variable=="protein",]
gslo[gslo$variable=="protein",]

head(giqr)
giqr$variable<-factor(giqr$variable, levels=c("mass_g", "area_cm2","SLA","C","N","P","Ca","Mg","Mn","Al","B","Zn","N_P","total_chl","carot","Chl_R","Ala","GABA","Glu","Val","protein","Put","Spm","Spd"))
dim(giqr)

giqr<-na.omit(giqr) # There were a lot of NAs?

## Now bring in iqr that they are the same length
gslo$iqr<-giqr$value[match(gslo$unique, giqr$unique)]

gslo$normalized<-gslo$slope/gslo$iqr



gslo$variable<-as.character(gslo$variable)

######## Figure 2!
## add leaf characteristic
table(f2$variable)
gslo$Group[gslo$variable=="area_cm2"]<-"Physical characteristics"
gslo$Group[gslo$variable=="mass_g"]<-"Physical characteristics"
gslo$Group[gslo$variable=="SLA"]<-"Physical characteristics"

gslo$Group[gslo$variable=="total_chl"]<-"Photosynthetic pigments"
gslo$Group[gslo$variable=="carot"]<-"Photosynthetic pigments"
gslo$Group[gslo$variable=="Chl_R"]<-"Photosynthetic pigments"

gslo$Group[gslo$variable=="Ala"]<-"Amino acids"
gslo$Group[gslo$variable=="GABA"]<-"Amino acids"
gslo$Group[gslo$variable=="Glu"]<-"Amino acids"
gslo$Group[gslo$variable=="Val"]<-"Amino acids"

gslo$Group[gslo$variable=="Put"]<-"Polyamines"
gslo$Group[gslo$variable=="Spd"]<-"Polyamines"
gslo$Group[gslo$variable=="Spm"]<-"Polyamines"

gslo$Group[gslo$variable=="C"]<-"Elements"
gslo$Group[gslo$variable=="Mg"]<-"Elements"
gslo$Group[gslo$variable=="Ca"]<-"Elements"
gslo$Group[gslo$variable=="Al"]<-"Elements"
gslo$Group[gslo$variable=="B"]<-"Elements"
gslo$Group[gslo$variable=="Mn"]<-"Elements"
gslo$Group[gslo$variable=="N"]<-"Elements"
gslo$Group[gslo$variable=="N_P"]<-"Elements"
gslo$Group[gslo$variable=="P"]<-"Elements"
gslo$Group[gslo$variable=="S"]<-"Elements"
gslo$Group[gslo$variable=="Zn"]<-"Elements"


gslo$Group[gslo$variable=="protein"]<-"Soluble protein"


table(gslo$variable, gslo$Group)




gslo$variable<-factor(gslo$variable, levels=c("SLA", "area_cm2","mass_g","Ca","N","B","N_P","Zn","P","Mg","Mn","Al","C","total_chl","carot","Chl_R","Ala","GABA","Glu","Val","protein","Put","Spm","Spd"))

table(gslo$trmt)
gslo$trmt<-factor(gslo$trmt, levels=c("Con","N","P","NP"))
table(gslo$Group)
gslo$Group<-factor(gslo$Group, levels=c("Physical characteristics","Elements","Photosynthetic pigments","Amino acids","Polyamines", "Soluble protein"))

# 
head(gslo)
table(gslo$variable)


head(gslo)

## AY wants to average by stand, calculate SE-  graph



avg<-aggregate(gslo$normalized, by=list(Stand=gslo$Stand, Group=gslo$Group, variable=gslo$variable), FUN="mean")
avg$group<-paste(avg$Stand, avg$variable)
st.err <- function(x) {  sd(x, na.rm=T)}
se<-aggregate(gslo$normalized, by=list(Stand=gslo$Stand,Group=gslo$Group,variable=gslo$variable), FUN=st.err)
se$group<-paste(se$Stand, se$variable)
avg$se<-se$x[match(avg$group, se$group)]
avg$normalized<-avg$x
library(ggplot2)

## Make sure they're ordered correctly
avg<-na.omit(avg)
avg$variable<-factor(avg$variable, levels=c("SLA", "area_cm2","mass_g","Zn","Mg","Ca","P","B","N","Mn","N_P","C","Al","Glu","Ala","GABA","Val","protein","Put","Spm","Spd","total_chl","carot","Chl_R"))
#avg$Group<-factor(avg$Group, levels=c("Physical characteristics","Elements","Amino acids","Polyamines","Photosynthetic pigments"))

write.csv(avg, file="fig1_write_out.csv")
avg2<-read.csv("fig1_write_out.csv")
head(avg2)

gplo<-ggplot(avg, aes(x=variable,y=normalized, shape=Group))+
  scale_shape_manual(values=c(0,1,2,5,7,8))+xlab("")+coord_flip()+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  geom_point(aes(size=.2), stroke=2)+theme_bw()+ylab("change with depth in the crown")+
  geom_errorbar(aes(ymin=normalized-se, ymax=normalized+se), width=.2)+ 
    theme(text=element_text(size=20))+guides(size=FALSE)+geom_hline(yintercept=0, linetype="dashed")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position="right",legend.box="vertical", legend.margin=margin())+theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=24))+
  guides(colour = guide_legend(override.aes = list(size=5)), shape = guide_legend(override.aes = list(size=5)))+
  theme(legend.title = element_blank())+scale_x_discrete(limits = rev(levels(gslo$variable)))
  #geom_vline(xintercept=3.5)+geom_vline(xintercept=8.5)
#  geom_vline(xintercept=11.5)+geom_vline(xintercept=21.5)

gplo



## FIGURE 1!
# for pdf.
dpi=300
tiff("05_03_thesis_fig_1.tif", width=12*dpi, height=8*dpi, res=dpi)
gplo
dev.off()


############# Don't go below`!`

avg.slo<-aggregate(f2$slope,by=list(variable=f2$variable, Group=f2$Group), FUN="mean", na.rm=T)
avg.iqr<-aggregate(f2$iqr,by=list(variable=f2$variable, Group=f2$Group), FUN="mean", na.rm=T)

head(avg.slo)
head(avg.iqr)

avg.slo$norm<-avg.slo$x/avg.iqr$x
g2<-ggplot(avg, aes(x=reorder(variable, x),y=x,col=Group, shape=Group))+
  scale_shape_manual(values=c(0,1,2,5,7))+xlab("")+coord_flip()+
  geom_point(aes(size=.8), stroke=2)+theme_bw()+ylab("Average slope value")+
  theme(text=element_text(size=20))+guides(size=FALSE)+geom_hline(yintercept=0, linetype="dashed")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position="right",legend.box="vertical", legend.margin=margin())+theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=24))+
  guides(colour = guide_legend(override.aes = list(size=5)), shape = guide_legend(override.aes = list(size=5)))+
  theme(legend.title = element_blank())

g3<-ggplot(avg, aes(x=reorder(variable, x),y=x,col=Group, shape=Group))+
  scale_shape_manual(values=c(0,1,2,5,7))+xlab("")+coord_flip()+
  geom_point(aes(size=.8), stroke=2)+theme_bw()+ylab("Average IQR value")+
  theme(text=element_text(size=20))+guides(size=FALSE)+geom_hline(yintercept=0, linetype="dashed")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position="right",legend.box="vertical", legend.margin=margin())+theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=24))+
  guides(colour = guide_legend(override.aes = list(size=5)), shape = guide_legend(override.aes = list(size=5)))+
  theme(legend.title = element_blank())
g3

g4<-ggplot(avg.slo, aes(x=reorder(variable, norm),y=norm,col=Group, shape=Group))+
  scale_shape_manual(values=c(0,1,2,5,7))+xlab("")+coord_flip()+
  geom_point(aes(size=.8), stroke=2)+theme_bw()+ylab("Relative change through the crown")+
  theme(text=element_text(size=20))+guides(size=FALSE)+geom_hline(yintercept=0, linetype="dashed")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position="right",legend.box="vertical", legend.margin=margin())+theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=24))+
  guides(colour = guide_legend(override.aes = list(size=5)), shape = guide_legend(override.aes = list(size=5)))+
  theme(legend.title = element_blank())
g4
