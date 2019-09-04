library(FunnelPlotR)
library(COUNT)
library(ggplot2)

# lets use the \'medpar\' dataset from the \'COUNT\' package. Little reformatting needed.
#Easiest way is to save it so csv, re-import (gets rid og 'labelled' status), then add factors etc.
data(medpar)
write.csv(medpar, "medpar.csv")
rm(medpar)
medpar <- read.csv("medpar.csv")
medpar$provnum<-factor(medpar$provnum)
medpar$type<-factor(medpar$type)
medpar$los<-as.numeric(medpar$los)


# Build simple logistic model
mod<- glm(died ~ los + hmo + age80 + factor(type), family="poisson", data=medpar)

medpar$prds<- predict(mod, type="response")

a<-funnel_plot(numerator=medpar$died, denominator=medpar$prds, group = medpar$provnum,
            title = 'Mortality Ratio plot for `medpar` data', Poisson_limits = TRUE,
            OD_adjust = FALSE,label_outliers = TRUE, return_elements = "plot")

a[[1]] + theme(legend.position = "bottom")


# Buils a Poisson model on LOS, as this is more overdispersed, for demonstration
mod2<- glm(los ~ hmo + died + age80 + factor(type), family="poisson", data=medpar)
summary(mod2)

medpar$prds2<- predict(mod2, type="response")

b<-funnel_plot(numerator=medpar$los, denominator=medpar$prds2, group = medpar$provnum,
            title = 'Length of Stay Funnel plot for `medpar` data', Poisson_limits = FALSE,
            OD_adjust = TRUE, label_outliers = TRUE, return_elements = "plot")

b[[1]] + theme(legend.position = "bottom")
