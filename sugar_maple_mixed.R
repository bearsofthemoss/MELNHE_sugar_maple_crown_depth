## R code for data analysis for sugar maple crown N*P.
## Alex Young   10/7/2021
## alexyoung.116@gmail.com
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



##3 create output for each leaf characteristic
## this gets the coefficients for the fixed effects
aov.lme <- function(y, Stand, Ntrmt, Ptrmt, Tree_ID){
  #1 aov for linear mixed effect model
  library(nlme)
  fm2<- lme(y ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action=na.exclude)
  emm2<-fm2$coefficients$fixed
  em<- as.data.frame(emm2)
      return(em)}

# this is for the coeficients
output.lme<-list()
for(i in c(12:55)){ 
  y = samp[,i]
  Stand= samp$Stand
  Ntrmt=samp$Ntrmt
  Ptrmt=samp$Ptrmt
  output.lme[[i-11]] <- aov.lme(y, Stand, Ntrmt, Ptrmt, Tree_ID)}
d.em<- as.data.frame(rbindlist(output.lme))



# this gets at the residuals of each model
aov.res <- function(y, Stand, Ntrmt, Ptrmt, Tree_ID){
  #1 aov for linear mixed effect model
  library(nlme)
  fm2<- lme(y ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action=na.exclude)
  emm2<-t.test(residuals(fm2))$p.value
  em<- as.data.frame(emm2)
  return(em)}

fm2<- lme(N ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action=na.exclude)
hist(residuals(fm2))
t.test(residuals(fm2))$p.value
t.test(residuals(fm2))
f$p.value

# this is for the t test for residuals
output.res<-list()
for(i in c(12:55)){ 
  y = samp[,i]
  Stand= samp$Stand
  Ntrmt=samp$Ntrmt
  Ptrmt=samp$Ptrmt
  output.res[[i-11]] <- aov.res(y, Stand, Ntrmt, Ptrmt, Tree_ID)}

d.res<- as.data.frame(rbindlist(output.res))
d.res$variable<-rep(names(samp)[c(12:55)], each=1)
head(d.res)
table(d.res$emm2)

# carry on with d.em for the coefficients!
d.em$type<-rep(c("intercept","scaled","Ntrmt","Ptrmt","height*N","height*P","N:P","height*N*P"), 44)

rep(names(samp)[c(12:55)], each=8)
d.em$variable<-rep(names(samp)[c(12:55)], each=8)


names(d.em)

head(d.em)
head(d.mean)

int<-d.em[d.em$type=="intercept",]
d.em$int<-int$emm2[match(d.em$variable, int$variable)]
d.em$coef<-d.em$emm2+d.em$int

table(d.em$type)
d.em$type<-factor(d.em$type, levels=c("intercept","scaled","Ntrmt","Ptrmt","height*N","height*P","N:P","height*N*P"))

aggregate(d.em$coef[!d.em$type=="intercept"])



### add in leaf characteristics
d.em$Group[d.em$variable=="area_cm2"]<-"Physical characteristics"
d.em$Group[d.em$variable=="mass_g"]<-"Physical characteristics"
d.em$Group[d.em$variable=="SLA"]<-"Physical characteristics"

d.em$Group[d.em$variable=="total_chl"]<-"Photosynthetic pigments"
d.em$Group[d.em$variable=="carot"]<-"Photosynthetic pigments"
d.em$Group[d.em$variable=="Chl_R"]<-"Photosynthetic pigments"

d.em$Group[d.em$variable=="Ala"]<-"Amino acids"
d.em$Group[d.em$variable=="GABA"]<-"Amino acids"
d.em$Group[d.em$variable=="Glu"]<-"Amino acids"
d.em$Group[d.em$variable=="Val"]<-"Amino acids"

d.em$Group[d.em$variable=="Put"]<-"Polyamines"
d.em$Group[d.em$variable=="Spd"]<-"Polyamines"
d.em$Group[d.em$variable=="Spm"]<-"Polyamines"

d.em$Group[d.em$variable=="C"]<-"Elements"
d.em$Group[d.em$variable=="Mg"]<-"Elements"
d.em$Group[d.em$variable=="Ca"]<-"Elements"
d.em$Group[d.em$variable=="Al"]<-"Elements"
d.em$Group[d.em$variable=="B"]<-"Elements"
d.em$Group[d.em$variable=="Mn"]<-"Elements"
d.em$Group[d.em$variable=="N"]<-"Elements"
d.em$Group[d.em$variable=="N_P"]<-"Elements"
d.em$Group[d.em$variable=="P"]<-"Elements"
d.em$Group[d.em$variable=="S"]<-"Elements"
d.em$Group[d.em$variable=="Zn"]<-"Elements"

d.em$Group[d.em$variable=="protein"]<-"Soluble protein"




library(ggplot2)
d.em$variable<-factor(d.em$variable, levels=c("mass_g", "area_cm2","SLA","C","N","P","Ca","Mg","Mn","Al","B","Zn","N_P","total_chl","carot","Chl_R","Ala","GABA","Glu","Val","protein","Put","Spm","Spd"))

d.em$Group<-factor(d.em$Group, levels=c("Physical characteristics","Photosynthetic pigments","Elements","Amino acids","Polyamines","Soluble protein"))


head(d.em[d.em$variable=="mass_g",], 8)

d.em$norm<-d.em$emm2/ d.em$int


ggplot(d.em[d.em$type=="scaled",], aes(x=variable, y=emm2,col=type, shape=Group))+geom_point()+coord_flip()

ggplot(d.em[d.em$type=="scaled",], aes(x=variable, y=norm, shape=Group))+geom_point()+facet_wrap(~Group, scales="free_y", nrow=7)+coord_flip()+
  geom_hline(yintercept=0, linetype='dotted', col = 'black')  
head(d.em)

gChl_caro
 
 

ggplot(d.em, aes(x=variable, y=emmean, shape=Group))+geom_point()+coord_flip()
d.em$norm<-d.em$emmean/abs(d.em$SE)



ggplot(d.em, aes(x=variable, y=norm, shape=Group))+geom_point()+coord_flip()







############################################################

##3 create output for each leaf characteristic
aov.mixed <- function(y, Stand, Ntrmt, Ptrmt, Tree_ID){
  #1 aov for linear mixed effect model
  library(nlme)
  mixed1<-anova( lme(y ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action=na.exclude))
  return(mixed1)}



output.mixed<-list()
for(i in c(12:55)){ 
  y = samp[,i]
  Stand= samp$Stand
  Ntrmt=samp$Ntrmt
  Ptrmt=samp$Ptrmt
  output.mixed[[i-11]] <- aov.mixed(y, Stand, Ntrmt, Ptrmt, Tree_ID)}

output.mixed

# so some formatting
d.int<- as.data.frame(rbindlist(output.mixed))
d.int
d.int$Source<-rep(c("intercept","height","N","P","height*N","height*P","N*P","height*N*P"))
dim(d.int)

d.int


rep(names(samp)[c(12:55)], each=8)
d.int$variable<-rep(names(samp)[c(12:55)], each=8)
names(d.int)


d.int<-d.int[ ,c(6,5, 1, 2, 3, 4)]

# for an easier time of formatting names for tables
pism<-d.int
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

summary(pism)

pism$p.adj<-p.adjust(pism$`p-value`,method="hochberg")
head(pism)

table(pism$p.adj)

#write.csv(pism, file="supplemental_table_2.csv")


##Carry on with fp
## order fp by the order of variables. 

order<-seq(1:43)
name<-c("mass_g",	"area_cm2"	,"SLA"	,"protein","total_chl",	"carot"	, "Chl_R"	,"C","N","P","N_P",	"Ca","Mg","Mn","Al",	"B",	"Zn",	"S"	,	"Ala",	"GABA",	"Glu",	"Val",	"Put",	"Spd"	,"Spm"	,"Chl_A",	"Chl_B"	,"Fe"	,"K"	,"Sr",	"Arg",	"Asp",	"Ile",	"Leu"	,"Lys",	"Pro"		,"s_Al"	,"s_Ca"	,"s_K",	"s_Mn",	"s_P"	,"s_Mg"	,"s_Zn")
length(name)
vord<-as.data.frame(order, name)
vord$name<-rownames(vord)
vord$order<-as.numeric(vord$order)


#order the variables
fp<-spread(pism[ , c(1,2,6)], "Source","p-value")

fp$vord<-vord$order [match(fp$variable, vord$name)]
fp<-fp[order(fp$vord),]

# order the colums, add some as well.
fp$leaf.char<-pism$leaf.char[match(fp$variable, pism$variable)]
fp<-fp[ ,c(11,1,2,7,9,3,5,8,4)]
head(fp)

fp$height.adj<-p.adjust(fp$height,method="hochberg")
fp$N.adj<-p.adjust(fp$N,method="hochberg")
fp$P.adj<-p.adjust(fp$P,method="hochberg")
fp$heightN.adj<-p.adjust(fp$`height*N`,method="hochberg")
fp$heightP.adj<-p.adjust(fp$`height*P`,method="hochberg")
fp$NP.adj<-p.adjust(fp$`N*P`,method="hochberg")
fp$height.adj<-p.adjust(fp$`height*N*P`,method="hochberg")

write.csv(fp, file="thesis_p_values_8_1_2021.csv")





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
