# Import simluated cusum data for an aggregated chart(agg) and person-level (pers)
cusum_agg <- read.csv("cusum_agg.csv")
cusum_pres<- read.csv("cusum_pers.csv")

cusum_agg$Date<-cusum_agg[[1]]
cusum_agg[[1]]<-NULL

cusum_pres$Patient<-cusum_pres[[1]]
cusum_pres[1]<-NULL

cusum_agg$Trigger <-as.factor(cusum_agg$Trigger)
cusum_pres$Trigger <-as.factor(cusum_pres$Trigger)

library(ggplot2)


# Plot aggregate CUSUM
ggplot(cusum_agg, aes(x=Date, y=Ct, group=Date))+
  geom_line(group=1, col="#51E85E", size=1)+
  geom_point(aes(shape=Trigger, size=Trigger, col=Trigger), fill="#51E85E")+
  geom_hline(aes(yintercept=5.48), col="goldenrod", linetype=2, size=1)+
  ggtitle("Aggreagate Cusum Example")+
  scale_shape_manual(values=c(16,4))+
  scale_colour_manual(values=c("#51E85E",2))+
  scale_y_continuous(name="CUSUM value",limits=c(0,6), breaks = seq(6))+
  theme(axis.text.x = element_text(angle=45, hjust = 1))


#Plot person-level CUSUM
ggplot(cusum_pres, aes(x=Patient, y=Ct, group=Patient))+
  geom_line(group=1, col="dodgerblue2", size=1)+
  geom_point(aes(shape=Trigger, size=Trigger, col=Trigger), fill="#51E85E")+
  geom_hline(aes(yintercept=6.3), col="goldenrod", linetype=2, size=1)+
  ggtitle("Person-level Cusum Example")+
  scale_shape_manual(values=c(16,4))+
  scale_colour_manual(values=c("dodgerblue2",2))+
  scale_y_continuous(name="CUSUM value",limits=c(0,7), breaks = seq(6))+
  scale_x_continuous(breaks=seq(10,100,10))
  #theme(axis.text.x = element_text(angle=45, hjust = 1))
