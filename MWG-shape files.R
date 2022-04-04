

jam<-read.csv("C:/Users/Alex/Downloads/JTFALCRU202201_file_for_Treeswift.csv")
head(jam)
sum(table(jam$Acres))
dim(jam)

table(jam$Thin.Year)
table(jam$Year)
ggplot(jam, aes(x=Year, y=Thin.Year, col=Location))+geom_point()


jam$Acres<-as.numeric(jam$Acres)
ggplot(jam, aes(x=Year, y=Acres, col=Location))+geom_point()+
  facet_wrap(~Thin.Year)+xlab("Planted year")+ylab("Stand size (acres)")+ggtitle("year thinned by region")

jam$Thin.Year<-as.factor(jam$Thin.Year)
jam$recent<-as.factor(2022-as.numeric(jam$Thin.Year))

ggplot(jam, aes(x=Thin.Year, y=Acres, col=Location))+geom_point()+
  facet_wrap(~Location)+xlab("Planted year")+ylab("Stand size (acres)")+ggtitle("year thinned by region")


jam[jam$Thin.Year=="2021",]

  hist(jam$Thin.Year)

jam2<-jam[!is.na(jam$Location),]  
ggplot(jam2, aes(x=Year, y=Acres, col=Location, size=Acres))+geom_point()+
  facet_wrap(~Location)+xlab("Thinned year")+ylab("Stand size (acres)")+ggtitle("year thinned by region")


jam$Location<-factor(jam$Location, levels=c("North","Northwest","Way north","Far southwest","Southwest","East"))

jam$plots<-jam$Acres/2.5
ggplot(jam, aes(x=Thin.Year, y=plots, col=Location))+geom_point()+
  facet_wrap(~Location, nrow=3)+xlab("Thinned year")+ylab("Estimated number of plots")+ggtitle("year thinned by region")


table(jam$Molpus.requested...sample)
jam[jam$Molpus.requested...sample=="10%",]
is.na(jam$Location)

write.csv(jam[jam$Molpus.requested...sample=="10%",] , file="mwg_10_percent.csv")


fl<-read.csv("C:/Users/Alex/Downloads/ts_flightlog_2021.csv")

head(fl)
library(ggplot2)

names(fl)
ggplot(fl, aes(x=duration/60, y=home_distance, col=final_inventory, shape=Client))+geom_point()+
  xlab("Flight duration (mins)")+ylab("Distance from takeoff (m)")+theme_classic()
