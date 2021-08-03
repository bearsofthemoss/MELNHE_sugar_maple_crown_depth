## R code for data analysis for sugar maple crown N*P.
## Alex Young   8/1/2021
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


###############################################
### Functions for statistical anova
aov.mixed <- function(y, Stand, Ntrmt, Ptrmt, Tree_ID){
  #1 aov for linear mixed effect model
  library(nlme)
  mixed1<-anova( lme(y ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action=na.exclude))
  return(mixed1)}


out<-list()
for(i in c(12:55)){
  y = samp[ ,i]
  o<-AIC(lme(y ~ dfromtop*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp,na.action=na.exclude))
 out[[i-11]]<-o}

df <- data.frame(matrix(unlist(out), nrow=length(out), byrow=TRUE))

d<-as.data.frame(names(samp)[c(12:55)])
d$dfrom<-df$matrix.unlist.out...nrow...length.out...byrow...TRUE.

d$var<-d$`names(samp)[c(12:55)]`
e<-gather(d, "option","AIC", 2:3)
head(d)
d$diff<-d$dfrom-d$scaled
ggplot(d, aes(x=var, y=diff))+geom_point()+ylab("'AIC dfromtop' - 'AIC scaled'")+
  ggtitle("Subtracting the AIC value for 'dfromtop' - 'scaled' results in positive values. The scaled depth x axis is more parsimonious")

out<-as.data.frame(cbind(out))
out
str(out)
out$scaled<-out$out
out$var<-names(samp)[c(12:55)]

head(out)

str(out)
dec<-out
dec$scaled<-out$scaled
head(dec)



write.csv(dec, file="AIC-thesis.csv")

ggplot(dec, aes(x=dfro, y=scaled))+geom_point()


### residual checking

fm1<- lme(N ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action=na.exclude)
qqnorm(fm1)
head(residuals(fm1, level = 0:1))
summary(residuals(fm1) /
          residuals(fm1, type = "p")) # constant scaling factor 1.432

library(emmeans)
emmeans(fm1)

summary(fm1)

emm1 = emmeans(fm1, specs = pairwise ~ Ntrmt:Ptrmt)
m<-as.data.frame(emm1$emmeans[1:4])
(17.2+16.8)/2
(20.3+22.4)/2

(21.34-16.95)/mean(c(21.34,16.95))
#22.9


fm1$residuals
t.test(fm1$residuals)

names(samp)
################  Lopp through variables with linear mixed effect model, scaled x axis


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
