## ---------------- Autoscaling and Heatmap ---------------- ##

dataraw <- read.csv(file.choose())

# Autoscale

library(mdatools)

# only numeric columns - excluded the label and the samples columns
datanum <- data[,3:26]
data_autoscale <- prep.autoscale(datanum, center = T, scale = T)

# Before/After Plots

png("boxplot_norm.png",width = 480, height = 480, units = "px")
boxplot(data_autoscale, main = "Mean centered and standardized")
dev.off()

# separates data for the following plots
data_before <- as.matrix(data[3:26])

png("Before_After.png", height = 480, width = 980 )
par(mfrow = c(1,2))
plot(density(data_before), main = "Before Scale", lwd=3 )
plot(density(data_autoscale), main = "After Scale", lwd=3)
dev.off()

# brings the two columns back
data_autoscale <-as.data.frame(data_autoscale)
data_autoscale <- cbind(data["X"], data["label"], data_autoscale)

# creates a df with the labels, so later we can substitute F... for month name
month <- c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")
label <- c("F01", "F02", "F03", "F04", "F05", "F06", "F07", "F08", "F09", "F10", "F11", "F12")
labeldf <- cbind(label, month)

#changes F... for the month names and brings it to the first column
library(dplyr)
data_heatmap <- merge(data_autoscale, labeldf, by.y= "label", all.x = TRUE)
data_heatmap <- select(data_heatmap, last_col(), everything())
data_heatmap<- data_heatmap[ , c(1,3, 4:27)]

# loads packages, remove column with sample name, takes the mean for each group
# (based on the month)

library(dplyr)
library(tidyr)
library(stringr)

datanormlabeled3 <- cbind(data_heatmap["month"], data_heatmap[, 3:26])

#takes the mean of the feature by month (mean of each column by the month column)
dataheatmap <- aggregate( .~ month, FUN = mean, data = datanormlabeled3)

library(gplots)
library(RColorBrewer)
library(hrbrthemes)

rng <- range(c(-2,2))
cols <- rev(brewer.pal(n = 10, name = "RdBu"))

## Obs: with heatmap.2 function dendogram is automatic. Better than ggplot.

# heatmap df minus the first column
dataheatmap2 <- dataheatmap[,-1]

# transforms the months as row names.
rownames(dataheatmap2) <- dataheatmap[,1]

# transpose data
dataheatmap2 <- as.matrix(t(dataheatmap2))

# unicode for some special characters on compond names
rownames(dataheatmap2) <- c("(2E)-Hexenal","\u03B1-Thujene","\u03B1-Pinene","Thuja-2,4(10)-diene",
                            "Sabinene","UF1","1-Octen-3-ol","3-Octanone","Myrcene","3-Octanol","(3Z)-Hexenyl acetate",
                            "\u03B4-3-Carene","o-Cymene","UF2","Limonene","(E)-\u03B2-Ocimene","\u03B1-Ocimene",
                            "\u03B1-Cubebene","\u03B1-Copaene",
                            "\u03B2-Bourbonene","\u03B2-Cubebene","E-Caryophyllene","Germacrene D","UF3" )

# reorders columns based on months
dataheatmap2 <- dataheatmap2[, c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")]

#plot
tiff('heatmap.tiff', units="in", width= 4.9, height= 5, res=300, compression = 'lzw')

heatmap.2(dataheatmap2, # data
          col = cols, # color scheme
          trace = "none", # no lines along the heatmap
          dendrogram = "row", # where there will be a dendogram (on row, column, both or none?)
          Colv = FALSE, # sets if the columns should be reordered
          colsep = c(0:12), # separation between cells -  vertical
          rowsep = c(0:24), # separation between cells -  horizontal
          sepcolor = "gray", # color of the separation lines
          sepwidth = c(0.005, 0.005), # width of the separation lines
          density.info="none", # density on the color key above the plot
          keysize = 0.8, # size of the color key
          key.xlab = NULL, # label color key
          key.ylab = NULL,# label on color key
          key.title = NULL, #keytitle
          breaks = c(-2,-1.5,-1,-0.5, -0.25, 0 , 0.25, 0.5, 1, 1.5, 2 ), # range on color key (+1 the number of colors)
          margins = c(5,9)) # size of the color key

dev.off()
