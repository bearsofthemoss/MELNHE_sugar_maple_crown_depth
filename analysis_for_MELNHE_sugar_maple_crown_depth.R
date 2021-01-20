## R code for data analysis for sugar maple crown N*P.
## Alex Young   4/7/2020
## alexyoung.116@gmail.com


samp<-read.csv("Data/MELNHE_SugarMapleCrownDepth.csv", header=T)
head(samp)

summary(samp$Chl_A)

samp$Treatment <- factor(samp$trmt,levels=c("Control","N treatment","P treatment","N+P treatment"))
samp$trmt <- factor(samp$trmt,levels=c("Control","N treatment","P treatment","N+P treatment"))
samp$Tree_ID<-as.numeric(samp$Tree_ID)


tree<-read.csv("Data/Tree_info_MELNHE_sugar_maple_crown.csv")
tree
tree<-tree[, c(1:6)]


###
library(data.table)
library(tidyr)



# this function for calculating the slope and intercept of the linear model for each tree
lin <- function(y30,y79,y128,y168,y249,y250, y320, y480,y571,y609,y928, y1297){
  t30  <-coefficients(lm(y30 ~ samp[samp$Tree_ID=="30"   ,"scaled" ]))
  t79  <-coefficients(lm(y79 ~ samp[samp$Tree_ID=="79"   ,"scaled" ]))
  t128 <-coefficients(lm(y128~ samp[samp$Tree_ID=="128"  ,"scaled" ]))
  t168 <-coefficients(lm(y168  ~ samp[samp$Tree_ID=="168"  ,"scaled" ]))
  t249 <-coefficients(lm(y249  ~ samp[samp$Tree_ID=="249"  ,"scaled" ]))
  t250 <-coefficients(lm(y250  ~ samp[samp$Tree_ID=="250"  ,"scaled" ]))
  t320 <-coefficients(lm(y320 ~ samp[samp$Tree_ID=="320"  ,"scaled" ]))
  t480 <-coefficients(lm(y480 ~ samp[samp$Tree_ID=="480"  ,"scaled" ]))
  t571 <-coefficients(lm(y571  ~ samp[samp$Tree_ID=="571"  ,"scaled" ]))
  t609 <-coefficients(lm(y609  ~ samp[samp$Tree_ID=="609"  ,"scaled" ]))
  t928 <-coefficients(lm(y928 ~ samp[samp$Tree_ID=="928"  ,"scaled" ]))
  t1297<-coefficients(lm(y1297  ~ samp[samp$Tree_ID=="1297"  ,"scaled"]))
  # create one dataframe to rule them all
  lin<-t(as.data.frame(cbind(t30,t79,t128,t168,t249,t250,t320,t480,t571,t609,t928,t1297)))
  b<-cbind(tree,lin)
  return(b)
}


names(samp)
dim(samp)
output.mass<- list() # create an empty list to catch the output
for(i in c(11:53)){ 
  y30 = samp[samp$Tree_ID=="30",i]
  y79 = samp[samp$Tree_ID=="79",i]
  y128 = samp[samp$Tree_ID=="128",i]
  y168 = samp[samp$Tree_ID=="168",i]
  y249 = samp[samp$Tree_ID=="249",i]
  y250 = samp[samp$Tree_ID=="250",i]
  y320 = samp[samp$Tree_ID=="320",i]
  y480 = samp[samp$Tree_ID=="480",i]
  y571 = samp[samp$Tree_ID=="571",i]
  y609 = samp[samp$Tree_ID=="609",i]
  y928 = samp[samp$Tree_ID=="928",i]
  y1297 = samp[samp$Tree_ID=="1297",i]
  output.mass[[i-10]] <- lin(y30,y79,y128,y168,y249,y250, y320, y480,y571,y609,y928, y1297)
}


# Additional formating
abcd<- as.data.frame(rbindlist(output.mass))
abcd
colnames(abcd)<-c("Tree_ID","Stand","t.height.m","trmt","Ntrmt","Ptrmt","intercept","slope")
abcd$trmt<-factor(abcd$trmt, levels=c("Con","N","P","NP"))

abcd$var.names<-rep(names(samp)[c(11:53)], each=12)



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
sm.a<-aggregate(samp[ ,c(11:53)], list(Tree_ID=samp$Tree_ID), FUN = mean, na.rm=T)

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
names(sm.a)

sm.a.order<-sm.a[ ,c(1, 45:50,2:44)]
names(sm.a.order)
## quick spread and gather to alphabatize the var names
names(sm.a.order)
spra<-gather(sm.a.order, "var.names","value",8:50) #
sm.ave<-spread(spra, var.names , value)

names(sm.ave)
##########################
sm.int$trmt<-factor(sm.int$trmt, levels=c("Con","N","P","NP"))
sm.ave$trmt<-factor(sm.ave$trmt, levels=c("Con","N","P","NP"))



############################################################################################
## check point!
# spot check
head(sm.int)
head(sm.int[ ,1:10])
head(sm.slo[ ,1:10])
head(sm.ave[ ,1:10])
dim(sm.slo) # Each should have 12 rows, 50 columns
dim(sm.int)
dim(sm.ave)

###########
###  write out to .csv for easier graphing or inspection
#write.csv(sm.slo, "mass_scaled_slopes_3_3_20.csv")
#write.csv(sm.ave, "mass_scaled_average_5_8_20.csv")
#write.csv(sm.int, "mass_scaled_intercepts_3_3_20.csv")
###########################################################
#

#Example comparison for three ways to do the analysis.
# model m1 is a linear model with Stand in the model
m1<-lm(Al~ Stand+Ntrmt*Ptrmt, data=sm.ave)
summary(m1)
anova(m1)

# model m2 is made using the package lme4
# m2 is a mixed effect model Fixed is N*P, random effect is Stand
library(lme4)
library(lmerTest)
m2<-lmer(Al~ Ntrmt*Ptrmt+(1|Stand), data=sm.int)
summary(m2)
anova(m2)

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



#####################################################
##3 creat output of functions
output.slo.mass<-list()
for(i in c(8:50)){ 
  y = sm.slo[,i]
  Stand= sm.slo$Stand
  Ntrmt=sm.slo$Ntrmt
  Ptrmt=sm.slo$Ptrmt
  output.slo.mass[[i-7]] <- aov.slope(y, Stand, Ntrmt, Ptrmt)}
##3 c
output.intc.mass<-list()
names(sm.int)
for(i in c(8:50)){ 
  y = sm.int[,i]
  Stand= sm.int$Stand
  Ntrmt=sm.int$Ntrmt
  Ptrmt=sm.int$Ptrmt
  output.intc.mass[[i-7]] <- aov.intc(y, Stand, Ntrmt, Ptrmt)}
##  average aov for loop

names(sm.ave)
output.mass.ave<-list()
for(i in c(8:50)){ 
  y = sm.ave[,i]
  Stand= sm.ave$Stand
  Ntrmt=sm.ave$Ntrmt
  Ptrmt=sm.ave$Ptrmt
  output.mass.ave[[i-7]] <- aov.ave(y, Stand, Ntrmt, Ptrmt)}

output.mass.ave


anova(m2)
# INTERCEPT
d.int<- as.data.frame(rbindlist(output.intc.mass))
d.int$Source<-rep(c("N","P","N*P"))
d.int$variable<-rep(names(sm.int)[c(8:50)], each=3)
d.int$type<-c("Intercept")
d.int<-d.int[ ,c(9,7,8,1,2,3,4,5,6)]

# SLOPE
d.slo<- as.data.frame(rbindlist(output.slo.mass))
d.slo$Source<-rep(c("N","P","N*P"))
d.slo$variable<-rep(names(sm.slo)[c(8:50)], each=3)
d.slo$type<-c("Slope")
d.slo<-d.slo[ ,c(9,7,8,1,2,3,4,5,6)]


# AVERAGE
d.ave<- as.data.frame(rbindlist(output.mass.ave))
d.ave$Source<-rep(c("N","P","N*P"))
d.ave$variable<-rep(names(sm.ave)[c(8:50)], each=3)
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
for(i in c(8:50)){ 
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
write.csv(fp, file="thesis_p_values_1_9_2020.csv")





######################################################
# look here to normalize its slope by the IQR
sm.slo
head(sm.slo)
names(samp)
dim(samp)
###  get IQR for each tree

sm.iqr<-aggregate(samp[ ,c(11:53)], list(Tree_ID=samp$Tree_ID), FUN = IQR, na.rm=T)
head(sm.iqr)

IQR(samp[,"Chl_R"])
IQR(samp[samp$Tree_ID=="30","Chl_R"])
IQR(samp[samp$Tree_ID=="79","Chl_R"])


# gather sm.slo and sm.iqr
dim(sm.iqr)
dim(sm.slo)
giqr<-gather(sm.iqr, "variable","value",2:44)
head(giqr)
dim(giqr)
names(sm.slo)
gslo<-gather(sm.slo, "variable","value",8:50)
dim(gslo)
#formatting


gslo$slope<-as.numeric(gslo$value)
names(gslo)
gslo
f2<-gslo[ ,c(1,2,4,8,9,10)]
giqr$unique<-paste(giqr$Tree_ID, giqr$variable)
f2$unique<-paste(f2$Tree_ID, f2$variable)

## Make giqr half as long?
length(giqr$unique)
length(f2$unique)




head(giqr)
giqr$variable<-factor(giqr$variable, levels=c("mass_g", "area_cm2","SLA","C","N","P","Ca","Mg","Mn","Al","B","Zn","N_P","total_chl","carot","Chl_R","Ala","GABA","Glu","Val","protein","Put","Spm","Spd"))
giqr<-na.omit(giqr)

## Now bring in iqr that they are the same length
f2$iqr<-giqr$value[match(f2$unique, giqr$unique)]

f2$normalized<-f2$slope/f2$iqr


aggregate(f2$slope, by=list(f2$variable), FUN="mean", na.rm=T)
aggregate(f2$iqr, by=list(f2$variable), FUN="mean", na.rm=T)
aggregate(f2$normalized, by=list(f2$variable), FUN="mean", na.rm=T)

head(f2)
str(f2)
f2$Group<-""
f2$variable<-as.character(f2$variable)
#write.csv(f2, file="iqr_slope.csv")
######## Figure 2!
## add leaf characteristic
table(f2$variable)
f2$Group[f2$variable=="area_cm2"]<-"Physical characteristics"
f2$Group[f2$variable=="mass_g"]<-"Physical characteristics"
f2$Group[f2$variable=="SLA"]<-"Physical characteristics"

f2$Group[f2$variable=="total_chl"]<-"Photosynthetic pigments"
f2$Group[f2$variable=="carot"]<-"Photosynthetic pigments"
f2$Group[f2$variable=="Chl_R"]<-"Photosynthetic pigments"

f2$Group[f2$variable=="Ala"]<-"Amino acids"
f2$Group[f2$variable=="GABA"]<-"Amino acids"
f2$Group[f2$variable=="Glu"]<-"Amino acids"
f2$Group[f2$variable=="Val"]<-"Amino acids"

f2$Group[f2$variable=="Put"]<-"Polyamines"
f2$Group[f2$variable=="Spd"]<-"Polyamines"
f2$Group[f2$variable=="Spm"]<-"Polyamines"

f2$Group[f2$variable=="C"]<-"Elements"
f2$Group[f2$variable=="Mg"]<-"Elements"
f2$Group[f2$variable=="Ca"]<-"Elements"
f2$Group[f2$variable=="Al"]<-"Elements"
f2$Group[f2$variable=="B"]<-"Elements"
f2$Group[f2$variable=="Mn"]<-"Elements"
f2$Group[f2$variable=="N"]<-"Elements"
f2$Group[f2$variable=="N_P"]<-"Elements"
f2$Group[f2$variable=="P"]<-"Elements"
f2$Group[f2$variable=="S"]<-"Elements"
f2$Group[f2$variable=="Zn"]<-"Elements"

table(f2$variable, f2$Group)

f2$variable<-factor(f2$variable, levels=c("mass_g", "area_cm2","SLA","C","N","P","Ca","Mg","Mn","Al","B","Zn","N_P","total_chl","carot","Chl_R","Ala","GABA","Glu","Val","protein","Put","Spm","Spd"))

table(f2$trmt)
f2$trmt<-factor(f2$trmt, levels=c("Con","N","P","NP"))
table(f2$Group)
f2$Group<-factor(f2$Group, levels=c("Physical characteristics","Elements","Photosynthetic pigments","Amino acids","Polyamines"))

# 
head(f2)
table(f2$variable)

f2<-na.omit(f2)
head(f2)
library(ggplot2)
m<-ggplot(f2, aes(x=variable,y=normalized, color=trmt, shape=Group))+
  scale_shape_manual(values=c(0,1,2,5,7))+xlab("")+coord_flip()+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  geom_point(aes(size=.8), stroke=2)+theme_bw()+ylab("change with depth in the crown")+
  theme(text=element_text(size=20))+guides(size=FALSE)+geom_hline(yintercept=0, linetype="dashed")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position="bottom",legend.box="vertical", legend.margin=margin())+theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=24))+
  guides(colour = guide_legend(override.aes = list(size=5)), shape = guide_legend(override.aes = list(size=5)))+
  theme(legend.title = element_blank())+geom_vline(xintercept=3.5)+geom_vline(xintercept=8.5)+
  geom_vline(xintercept=11.5)+geom_vline(xintercept=21.5)+scale_x_discrete(limits = rev(levels(f2$variable)))
m


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
