library(ggplot2)
library(viridis)
library(tidyverse)
library(hrbrthemes)

#Read CSV
BMD<-read.csv("C:/Users/Sebastian/Desktop/Projecten/BMD.csv",header = TRUE, sep = ";", dec = ",")

#Boxplot
ggplot(BMD, aes(x=Perforation, y=BMD, fill=Perforation)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(legend.position="right", plot.title = element_text(size=14)) +
  ggtitle("  Bone mineral density comparison") +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  xlab("GI perforation") + ylab("BMD (mg/ml)") + 
  theme(axis.title.x = element_text(hjust=0.5),
    axis.title.y = element_text(hjust=0.5))+
  theme(axis.text = element_text(face="bold"))+
  theme(axis.title = element_text(face="bold"))
