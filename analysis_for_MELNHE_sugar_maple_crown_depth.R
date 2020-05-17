## R code for data analysis for sugar maple crown N*P.
## Alex Young   4/7/2020
## alexyoung.116@gmail.com


samp<-read.csv("C:\\Users\\Dropcopter2\\Downloads\\Sugar Maple Vert\\within_crown_sugar_maple_Young_2_9_2020.csv", header=T)
samp$Treatment <- factor(samp$trmt,levels=c("Control","N treatment","P treatment","N+P treatment"))
samp$trmt <- factor(samp$trmt,levels=c("Control","N treatment","P treatment","N+P treatment"))
samp$Tree.ID<-as.numeric(samp$Tree.ID)


tree<-read.csv("C:\\Users\\Dropcopter2\\Downloads\\Sugar Maple Vert\\tree_thesis_csv.csv")
tree<-tree[, c(1,2,3,4,7,8,9)]


###
library(data.table)
library(tidyr)



# this function for calculating the slope and intercept of the linear model for each tree
lin <- function(y30,y79,y128,y168,y249,y250, y320, y480,y571,y609,y928, y1297){
  t30  <-coefficients(lm(y30 ~ samp[samp$Tree.ID=="30"   ,"scaled" ]))
  t79  <-coefficients(lm(y79 ~ samp[samp$Tree.ID=="79"   ,"scaled" ]))
  t128 <-coefficients(lm(y128~ samp[samp$Tree.ID=="128"  ,"scaled" ]))
  t168 <-coefficients(lm(y168  ~ samp[samp$Tree.ID=="168"  ,"scaled" ]))
  t249 <-coefficients(lm(y249  ~ samp[samp$Tree.ID=="249"  ,"scaled" ]))
  t250 <-coefficients(lm(y250  ~ samp[samp$Tree.ID=="250"  ,"scaled" ]))
  t320 <-coefficients(lm(y320 ~ samp[samp$Tree.ID=="320"  ,"scaled" ]))
  t480 <-coefficients(lm(y480 ~ samp[samp$Tree.ID=="480"  ,"scaled" ]))
  t571 <-coefficients(lm(y571  ~ samp[samp$Tree.ID=="571"  ,"scaled" ]))
  t609 <-coefficients(lm(y609  ~ samp[samp$Tree.ID=="609"  ,"scaled" ]))
  t928 <-coefficients(lm(y928 ~ samp[samp$Tree.ID=="928"  ,"scaled" ]))
  t1297<-coefficients(lm(y1297  ~ samp[samp$Tree.ID=="1297"  ,"scaled"]))
  # create one dataframe to rule them all
  lin<-t(as.data.frame(cbind(t30,t79,t128,t168,t249,t250,t320,t480,t571,t609,t928,t1297)))
  b<-cbind(tree,lin)
  return(b)
}


names(samp)
dim(samp)
output.mass<- list() # create an empty list to catch the output
for(i in c(11:53)){ 
  y30 = samp[samp$Tree.ID=="30",i]
  y79 = samp[samp$Tree.ID=="79",i]
  y128 = samp[samp$Tree.ID=="128",i]
  y168 = samp[samp$Tree.ID=="168",i]
  y249 = samp[samp$Tree.ID=="249",i]
  y250 = samp[samp$Tree.ID=="250",i]
  y320 = samp[samp$Tree.ID=="320",i]
  y480 = samp[samp$Tree.ID=="480",i]
  y571 = samp[samp$Tree.ID=="571",i]
  y609 = samp[samp$Tree.ID=="609",i]
  y928 = samp[samp$Tree.ID=="928",i]
  y1297 = samp[samp$Tree.ID=="1297",i]
  output.mass[[i-10]] <- lin(y30,y79,y128,y168,y249,y250, y320, y480,y571,y609,y928, y1297)
}


# Additional formating
abcd<- as.data.frame(rbindlist(output.mass))

colnames(abcd)<-c("Tree.ID","Stand","dbh.cm","t.height.m","trmt","Ntrmt","Ptrmt","intercept","slope")
abcd$trmt<-factor(abcd$trmt, levels=c("Con","N","P","NP"))
abcd$var.names<-rep(names(samp)[c(11:53)], each=12)


## gather the slope and intercept column into a longer dataframe
abcd.g<-gather(abcd,"var","value",8:9)

# spread dataframe so all var.names in one column
sm.mass<-spread(abcd.g, var.names , value)
# make slope dataframe
sm.slo<-sm.mass[sm.mass$var=="slope",]
# make intercept dataframe
sm.int<-sm.mass[sm.mass$var=="intercept",]
# make average dataframe 
sm.a<-aggregate(samp[ ,c(11:53)], list(Tree.ID=samp$Tree.ID), FUN = mean, na.rm=T)

## add in tree information 
sm.a$Stand<-tree$Stand[match(sm.a$Tree.ID,tree$Tree.ID)]
sm.a$dbh.cm<-tree$dbh.cm[match(sm.a$Tree.ID,tree$Tree.ID)]
sm.a$t.height.m<-tree$t.height.m[match(sm.a$Tree.ID,tree$Tree.ID)]
sm.a$trmt<-tree$trmt[match(sm.a$Tree.ID,tree$Tree.ID)]
sm.a$Ntrmt<-tree$Ntrmt[match(sm.a$Tree.ID,tree$Tree.ID)]
sm.a$Ptrmt<-tree$Ptrmt[match(sm.a$Tree.ID,tree$Tree.ID)]
sm.a$var<-c("ave")
# re-order sm.a to be consistent with sm.int and sm.slo
dim(sm.a)
names(sm.a)

sm.a.order<-sm.a[ ,c(1, 45:51,2:44)]
names(sm.a.order)
## quick spread and gather to alphabatize the var names
names(sm.a.order)
spra<-gather(sm.a.order, "var.names","value",9:51) #
sm.ave<-spread(spra, var.names , value)

names(sm.ave)
##########################
sm.int$trmt<-factor(sm.int$trmt, levels=c("Con","N","P","NP"))
sm.ave$trmt<-factor(sm.ave$trmt, levels=c("Con","N","P","NP"))



############################################################################################
## check point!
# spot check
head(sm.int[ ,1:10])
head(sm.slo[ ,1:10])
head(sm.ave[ ,1:10])
dim(sm.slo) # Each should have 12 rows, 51 columns
dim(sm.int)
dim(sm.ave)

###########
###  write out to .csv for easier graphing or inspection
#write.csv(sm.slo, "mass_scaled_slopes_3_3_20.csv")
#write.csv(sm.ave, "mass_scaled_average_5_8_20.csv")
#write.csv(sm.int, "mass_scaled_intercepts_3_3_20.csv")
###########################################################



### Functions for statistical anova
aov.slope <- function(y, Stand, Ntrmt, Ptrmt){
  #1 aov for slope
  slope1<-anova(lm(y ~ Stand +Ntrmt*Ptrmt))
  return(slope1)}

aov.intc <- function(y, Stand, Ntrmt, Ptrmt){
  #1 aov for intercept
  intc1<-anova(lm(y ~ Stand +Ntrmt*Ptrmt))
  return(intc1)}

aov.ave <- function(y, Stand, Ntrmt, Ptrmt){
  #1 aov for average
  ave1<-anova(lm(y ~ Stand +Ntrmt*Ptrmt))
  return(ave1)}



#####################################################
##3 creat output of functions
output.slo.mass<-list()
for(i in c(9:51)){ 
  y = sm.slo[,i]
  Stand= sm.slo$Stand
  Ntrmt=sm.slo$Ntrmt
  Ptrmt=sm.slo$Ptrmt
  output.slo.mass[[i-8]] <- aov.slope(y, Stand, Ntrmt, Ptrmt)}
##3 c
output.intc.mass<-list()
names(sm.int)
for(i in c(9:51)){ 
  y = sm.int[,i]
  Stand= sm.int$Stand
  Ntrmt=sm.int$Ntrmt
  Ptrmt=sm.int$Ptrmt
  output.intc.mass[[i-8]] <- aov.intc(y, Stand, Ntrmt, Ptrmt)}
##  average aov for loop

names(sm.ave)
output.mass.ave<-list()
for(i in c(9:51)){ 
  y = sm.ave[,i]
  Stand= sm.ave$Stand
  Ntrmt=sm.ave$Ntrmt
  Ptrmt=sm.ave$Ptrmt
  output.mass.ave[[i-8]] <- aov.ave(y, Stand, Ntrmt, Ptrmt)}





# INTERCEPT
d.int<- as.data.frame(rbindlist(output.intc.mass))
d.int$Source<-rep(c("Stand","N","P","N*P","Residuals"))
d.int$resp.var<-rep(names(sm.int)[c(9:51)], each=5)
d.int$type<-c("Intercept")
d.int<-d.int[ ,c(7,8,6,1,2,3,4,5)]

# SLOPE
d.slo<- as.data.frame(rbindlist(output.slo.mass))
d.slo$Source<-rep(c("Stand","N","P","N*P","Residuals"))
d.slo$resp.var<-rep(names(sm.slo)[c(9:51)], each=5)
d.slo$type<-c("Slope")
d.slo<-d.slo[ ,c(7,8,6,1,2,3,4,5)]


# AVERAGE
d.ave<- as.data.frame(rbindlist(output.mass.ave))
d.ave$Source<-rep(c("Stand","N","P","N*P","Residuals"))
d.ave$resp.var<-rep(names(sm.ave)[c(9:51)], each=5)
d.ave$type<-c("Average")
d.ave<-d.ave[ ,c(7,8,6,1,2,3,4,5)]

dim(d.slo) #215 rows, 8 col
dim(d.ave)
dim(d.int)

pism<-rbind( d.slo , d.int, d.ave)

## add leaf characteristic
pism$leaf.char[pism$resp.var=="area.cm2"]<-"Physical characteristics"
pism$leaf.char[pism$resp.var=="mass.g"]<-"Physical characteristics"
pism$leaf.char[pism$resp.var=="SLA"]<-"Physical characteristics"

pism$leaf.char[pism$resp.var=="total.chl"]<-"Photosynthetic pigments"
pism$leaf.char[pism$resp.var=="carot"]<-"Photosynthetic pigments"
pism$leaf.char[pism$resp.var=="Chl.R"]<-"Photosynthetic pigments"

pism$leaf.char[pism$resp.var=="Ala"]<-"Amino acids"
pism$leaf.char[pism$resp.var=="GABA"]<-"Amino acids"
pism$leaf.char[pism$resp.var=="Glu"]<-"Amino acids"
pism$leaf.char[pism$resp.var=="Val"]<-"Amino acids"

pism$leaf.char[pism$resp.var=="Put"]<-"Polyamines"
pism$leaf.char[pism$resp.var=="Spd"]<-"Polyamines"
pism$leaf.char[pism$resp.var=="Spm"]<-"Polyamines"

pism$leaf.char[pism$resp.var=="C"]<-"Elements"
pism$leaf.char[pism$resp.var=="Mg"]<-"Elements"
pism$leaf.char[pism$resp.var=="Ca"]<-"Elements"
pism$leaf.char[pism$resp.var=="Al"]<-"Elements"
pism$leaf.char[pism$resp.var=="B"]<-"Elements"
pism$leaf.char[pism$resp.var=="Mn"]<-"Elements"
pism$leaf.char[pism$resp.var=="N"]<-"Elements"
pism$leaf.char[pism$resp.var=="N_P"]<-"Elements"
pism$leaf.char[pism$resp.var=="P"]<-"Elements"
pism$leaf.char[pism$resp.var=="S"]<-"Elements"
pism$leaf.char[pism$resp.var=="Zn"]<-"Elements"



pval<-spread(pism[ ,c("leaf.char","resp.var","Source","type","Pr(>F)")], Source,'Pr(>F)')
head(pval)

pval<-pval[ ,c(1,3,2,4,6,5)]


fp<-cbind(pval[pval$type=="Slope",],pval[pval$type=="Intercept",],pval[pval$type=="Average",] )
head(fp)
fp<-fp[ ,c(1,3,2,4,5,6,8,10,11,12,14,16,17,18)]
head(fp)

####################################
## overall slope test
slope.zero <- function(y){
  a<- t.test(y)
  p.val<-a$p.value
  mean<-a$estimate[1]
  low.cf<-a$conf.int[1]
  upp.cf<-a$conf.int[2]
  resp.var<-paste(colnames(sm.slo[i]))
  e<-cbind(resp.var, p.val, mean, low.cf, upp.cf)
  return(e)}


names(sm.slo)
out.t<-list()
for(i in c(9:51)){ 
  y = sm.slo[,i]
  out.t[[i-8]] <- slope.zero(y)}

head(out.t)
## ## now a funtion to write over it

load <- data.frame()
for(i in 1:length(out.t)){
  x <- out.t[[i]]
  x <- data.frame(y=as.matrix(x))
  load <- rbind(load, x)}

head(load)

head(fp)


fp$mean.slope<-load$y.mean[match(fp$resp.var, load$y.resp.var)]
fp$slope.pval<-load$y.p.val[match(fp$resp.var, load$y.resp.var)]

head(fp)

names(fp)
fp<-fp[ ,c(1,2,15,16,3:14)]
head(fp)
names(fp)


## order fp by the order of variables. 

order<-seq(1:43)
name<-c("mass.g",	"area.cm2"	,"SLA"	,"protein","total.chl",	"carot"	, "Chl.R"	,"C","N","P","N_P",	"Ca","Mg","Mn","Al",	"B",	"Zn",	"S"	,	"Ala",	"GABA",	"Glu",	"Val",	"Put",	"Spd"	,"Spm"	,"Chl.A",	"Chl.B"	,"Fe"	,"K"	,"Sr",	"Arg",	"Asp",	"Ile",	"Leu"	,"Lys",	"Pro"		,"s.Al"	,"s.Ca"	,"s.K",	"s.Mn",	"s.P"	,"s.Mg"	,"s.Zn")
length(name)
vord<-as.data.frame(order, name)
vord$name<-rownames(vord)
vord$order<-as.numeric(vord$order)

str(vord)
vord

fp$vord<-vord$order [match(fp$resp.var, vord$name)]
fp<-fp[order(fp$vord),]
head(fp)
#write.csv(fp, file="thesis_p_values_5_14_2020.csv")





######################################################
# look here to normalize its slope by the IQR
sm.slo
head(sm.slo)
###  get IQR for each tree
sm.iqr<-aggregate(samp[ ,c(11:53)], list(Tree.ID=samp$Tree.ID), FUN = IQR, na.rm=T)
# gather sm.slo and sm.iqr
giqr<-gather(sm.iqr, "variable","value",2:44)
gslo<-gather(sm.slo, "variable","value",9:51)
#formatting

gslo$slope<-gslo$value
names(gslo)
gslo
f2<-gslo[ ,c(1,2,5,9,11)]
giqr$unique<-paste(giqr$Tree.ID, giqr$variable)
f2$unique<-paste(f2$Tree.ID, f2$variable)
f2$iqr<-giqr$value[match(f2$unique, giqr$unique)]
f2$normalized<-f2$slope/f2$iqr

head(f2)


#write.csv(f2, file="iqr_slope.csv")
######## Figure 2!
f2$variable<-factor(f2$variable, levels=c("leaf mass", "leaf area","specific leaf area","C","N","P","Ca","Mg","Mn","Al","B","Zn","N:P","chlorophyll","carotenoids","chlorophyll a:b","alanine","GABA","glutamic acid","valine","soluble protein","putrescine","spermine","spermidine"))

f2$trmt<-factor(f2$trmt, levels=c("Control","N treatment","P treatment","N+P treatment"))
f2$Group<-factor(f2$Group, levels=c("physical","elements","pigments","amino acids","polyamines"))

# 

table(f2$variable)

f2<-na.omit(f2)

m<-ggplot(f2, aes(x=variable,y=normalized, color=trmt, shape=Group))+
  scale_shape_manual(values=c(0,1,2,5,7))+xlab("")+coord_flip()+
  scale_color_manual(values=c("black","blue","red", "purple"))+
  geom_point(aes(size=.8), stroke=2)+theme_bw()+ylab("change with depth in the crown")+
  theme(text=element_text(size=20))+guides(size=FALSE)+geom_hline(yintercept=0, linetype="dashed")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position="bottom",legend.box="vertical", legend.margin=margin())+theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=24))+ylim(-23, 23)+
  guides(colour = guide_legend(override.aes = list(size=5)), shape = guide_legend(override.aes = list(size=5)))+
  theme(legend.title = element_blank())+geom_vline(xintercept=3.5)+geom_vline(xintercept=8.5)+
  geom_vline(xintercept=11.5)+geom_vline(xintercept=21.5)+scale_x_discrete(limits = rev(levels(f2$variable)))
m