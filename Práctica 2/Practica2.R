# library
library(ggplot2)
#read csv
seguros <- read.csv("insurance.csv")
summary(seguros)

# dataset:
accidentes <- data.frame(Siniestros =seguros$siniestros)
head(accidentes)


# basic histogram
pl <- ggplot(accidentes, aes(x=Siniestros)) + 
  geom_histogram()

#p

