## R code for data analysis for sugar maple crown N*P.
## Alex Young   10/7/2021
## alexyoung.116@gmail.com
library(data.table)
library(tidyr)



samp<-read.csv("better_melnhe_sugar_maple_crown_depth.csv", header=T)
samp$Treatment <- factor(samp$trmt,levels=c("Control","N treatment","P treatment","N+P treatment"))
samp$trmt <- factor(samp$trmt,levels=c("Control","N treatment","P treatment","N+P treatment"))
samp$Tree_ID<-as.numeric(samp$Tree_ID)



samp$scaled<-log((samp$scaled+1.00001))


#fmlin<- lme(N ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action=na.exclude)
#fmexp<- lme(N ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action=na.exclude)

AIC(fmlin)
AIC(fmexp)


##3 create output for each leaf characteristic
## this gets the coefficients for the fixed effects
aic.lme <- function(y, Stand, Ntrmt, Ptrmt, Tree_ID){
  #1 aov for linear mixed effect model
  library(nlme)
  m2<- lme(y ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action=na.exclude)
  mem<- as.data.frame(AIC(m2))
  return(mem)}

# this is for the coefficients
output.aic<-list()
for(i in c(12:55)){ 
  y = samp[,i]
  Stand= samp$Stand
  Ntrmt=samp$Ntrmt
  Ptrmt=samp$Ptrmt
  output.aic[[i-11]] <- aic.lme(y, Stand, Ntrmt, Ptrmt, Tree_ID)}
a.em<- as.data.frame(rbindlist(output.aic))


a.em$variable<-rep(names(samp)[c(12:55)], each=1)


a.em

#linear<-a.em
exp<-a.em
exp$exp.AIC<-exp$`AIC(m2)`


linear$lin.AIC<-linear$`AIC(m2)`

ar<-cbind(exp,linear)
head(ar)
as<-ar[, c(2,3,6)]
as

as$mean<-(as$exp.AIC+as$lin.AIC)/2

as$dif<-as$lin.AIC-as$exp.AIC

as$ndif<-as$dif/as$mean

ggplot(as, aes(x=lin.AIC, y=exp.AIC))+geom_point()

head(as)

at<-gather(as[,c(1:3)], "fit","ndif", 2:3)

ggplot(at, aes(x=fit, y=ndif))+geom_point()+facet_wrap(~variable)


table(as$exp.AIC > as$lin.AIC)
       