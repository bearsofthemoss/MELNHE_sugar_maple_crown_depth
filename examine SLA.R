

b<-read.csv("Data/data.for.alex.csv")


head(b)


b$Treatment<-factor(b$Treatment, levels=c("C","N","P","NP"))

library(ggplot2)

ggplot(b, aes(x=Stand, y=plot.avg, shape=litter.year,col=Treatment, group=Treatment))+
  geom_point(aes(group=Treatment),size=5, position =position_dodge(width=.5), stat="identity")+
  scale_color_manual(values=c("black","blue","red","purple"))+theme_bw()+
  labs(y = bquote('Litter production '~(g/m^2)))+ theme(text=element_text(size=16))


library(patchwork)
library(ggpubr)

ggarrange(n,p,common.legend=T, nrow=2, legend="bottom")

n+p