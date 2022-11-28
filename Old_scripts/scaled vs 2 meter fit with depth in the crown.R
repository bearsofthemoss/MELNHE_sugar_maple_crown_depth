## R code for data analysis for sugar maple crown N*P.
## Alex Young   10/7/2021
## alexyoung.116@gmail.com
library(data.table)
library(tidyr)



samp<-read.csv("better_melnhe_sugar_maple_crown_depth.csv", header=T)
samp$Treatment <- factor(samp$trmt,levels=c("Control","N treatment","P treatment","N+P treatment"))
samp$trmt <- factor(samp$trmt,levels=c("Control","N treatment","P treatment","N+P treatment"))
samp$Tree_ID<-as.numeric(samp$Tree_ID)



lamp<-samp
lamp$scaled<-log((lamp$scaled+1.00001))



fmlin<- lme(P ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action=na.exclude)

fmexp<- lme(P ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=lamp, na.action=na.exclude)

AIC(fmlin)
AIC(fmexp)


##3 create output for each leaf characteristic
## this gets the coefficients for the fixed effects
aic.lme <- function(y, Stand, Ntrmt, Ptrmt, Tree_ID){
  #1 aov for linear mixed effect model
  library(nlme)
  m2<- lme(y ~ dfromtop*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action=na.exclude)
  mem<- as.data.frame(AIC(m2))
  return(mem)}

# this is for the coefficients
output.aic<-list()

names(samp)
for(i in c(12:55)){ 
  y = samp[,i]
  Stand= samp$Stand
  Ntrmt=samp$Ntrmt
  Ptrmt=samp$Ptrmt
  output.aic[[i-11]] <- aic.lme(y, Stand, Ntrmt, Ptrmt, Tree_ID)}


a.em<- as.data.frame(rbindlist(output.aic))
a.em

a.em$variable<-rep(names(samp)[c(12:55)], each=1)
a.em


###   un-hash each one of these depending on the y axis you choose
#exp<-a.em
#lin<-a.em
#dfr<-a.em

## do the above 3 times to make exp, lin, and dfr
exp$exp.AIC<-exp$`AIC(m2)`
lin$lin.AIC<-lin$`AIC(m2)`
dfr$dfr.AIC<-dfr$`AIC(m2)`

# check before combining
head(lin)
head(exp)

# combine the two.
ar<-cbind(exp,lin, dfr)
head(ar)


### AY got here. 
as<-ar[, c(2,3,6,9)]
as

as$mean<-(as$dfr.AIC+as$lin.AIC)/2

as$dif<-as$lin.AIC-as$dfr.AIC
as$ndif<-(as$dif/as$mean)*100

library(ggplot2)
ggplot(as, aes(x=lin.AIC, y=exp.AIC))+geom_point()

head(as)

at<-gather(as[,c(1:4)], "fit","ndif", 2:4)

ggplot(at, aes(x=fit, y=ndif))+geom_point()+facet_wrap(~variable, scales="free")


table(as$dfr.AIC < as$lin.AIC)


summary(as$dif)

       