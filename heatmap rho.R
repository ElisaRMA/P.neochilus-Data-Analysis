# Heatmap for rho correlation


# load data
data <- read.csv(choose.files())
data <- read.csv(file.choose(), sep=',')


library(ggplot2)
library(tidyr)
library(dplyr)
library(hrbrthemes)
library(RColorBrewer)

# rho data
datarho <- data[1:5]
colnames(datarho) <- c('Mass Features', "Min. Temperature", "Mean Temperature", "Max. Temperature", "Rainfall")
datarho <- gather(datarho, key='Environmental Factors', value = 'rho', -'Mass Features')

datarho$`Mass Features`<- c("(2E)-Hexenal","\u03B1-Thujene","\u03B1-Pinene","Thuja-2,4(10)-diene",
                            "Sabinene","UF1","1-Octen-3-ol","3-Octanone","Myrcene","3-Octanol",
                            "(3Z)-Hexenyl acetate","\u03B4-3-Carene","o-Cymene","UF2","Limonene",
                            "(E)-\u03B2-Ocimene","\u03B1-Ocimene", "\u03B1-Cubebene","\u03B1-Copaene",
                            "\u03B2-Bourbonene","\u03B2-Cubebene","E-Caryophyllene","Germacrene D","UF3" )


# pvalue data
datap <- data[,c(1,6:9)]
colnames(datap) <- c('Mass Features', "Min Temperature",
                     "Mean Temperature", "Max Temperature",
                     "Rainfall")

datap <- gather(datap, key='Environmental Factors', value = 'p', -'Mass Features')

datap <- mutate(.data=datap, signif=case_when(p<=0.05 ~ '*', p>=0.05~ ''))

dataheatmap <- data.frame(datarho, datap['signif'])

tiff('heatmaprho.tiff', units="in", width= 7, height= 6, res=300, compression = 'lzw')

ggplot(dataheatmap, aes(Environmental.Factors, Mass.Features, fill=rho)) +
  geom_tile() +
  scale_fill_distiller(palette='PiYG', direction=1,limits=c(-1, 1)) +
  geom_text(aes(label=signif), color="black", size=5) +
  theme(legend.title = element_text(face='italic')) +
  xlab("Environmental Factors") +
  ylab("Mass Features") +
  labs(fill='\u03C1')

dev.off()

