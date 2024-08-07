#############################
## LEfSe                   ##
## AIMDs AvH               ##
## Kevin Bu 6/6/24         ##
## AMP AIM MSQ138, 141 w   ##
## Batch correction        ##
#############################

# load libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(RColorBrewer)

# set background theme
bkg <-
  theme(axis.text.x = element_text(size = 36, color = "black")) +
  theme(axis.text.x = element_text(angle=0, vjust = 0.5, hjust = 1)) +
  theme(axis.title.x = element_text(margin = unit(c(0,0,4,0), "mm"))) +
  theme(axis.title.x = element_text(size = 36, color = "black", face="bold")) +
  theme(axis.text.y = element_text(size = 36, color = "black")) +
  theme(axis.title.y = element_text(size = 24, color = "black")) +
  theme(axis.title.y = element_text(margin = unit(c(0,4,0,0), "mm"))) +
  theme(legend.position = "right") +
  theme(legend.title = element_text(size = 36, face="bold")) +
  theme(legend.text = element_text(size = 36)) +
  theme(legend.key.size = unit(0.6, 'cm')) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(size=, color= "black", face ="bold")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(size=24, color= "black"))
#

# load data
data <-read.table("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs16/lefse_results.res", header = FALSE, sep = "\t")
names(data) <- c("RawTaxa", "X", "Group", "LDA", "pval")

# remove na
plot_data <- subset(data, !is.na(data$LDA))

# format taxa strings
taxa_strs <- list()
for (raw in plot_data$RawTaxa) {
  split <- as.character(unlist(str_split(raw, "\\.")))
  i <- length(split)
  blanks <- 0
  while (i > 0) {
    if (split[i] == "__") {
      blanks <- blanks + 1
      print(split)
      split <- split[1:i-1]
      print(split)
    }        
    else {
      break
    }
    i <- i - 1
  }
  
  if (length(split) == 1) {
    taxa_str <- split[1]
  }
  else {
    taxa_str <- paste(split[length(split)-1], split[length(split)])
  }
  if (blanks > 0) {
    for (i in 1:blanks) {
      taxa_str <- paste(taxa_str, "__uncl.", sep="")
    }
  }
  taxa_strs <- append(taxa_strs, taxa_str)
}

plot_data$Taxa <- as.character(taxa_strs)
# plot_data$Taxa <- sub(".*_s__", '', plot_data$RawTaxa)
# this flips the sign of the ref/negative group, which can be "Control" but might be axial or perip
# plot_data[plot_data$Group == "axial",]$LDA <- -1 * plot_data[plot_data$Group == "axial",]$LDA

# set colors and factors
# change these names
group.colors <- c(affected = "#E69F00", healthy = "#5f5843") # B3A98C E69F00
plot_data$Group <- factor(plot_data$Group, levels = c('affected', 'healthy'))

pdf("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs16/lefse_AvH.pdf", width=20, height=20)

p <- ggbarplot(plot_data, x="Taxa", y="LDA", fill="Group", width= 1, color = "white", sort.val = "asc", sort.by.groups=TRUE) +  
  labs(x = "", y = "LDA score", fill="Group") + coord_flip() + 
  #scale_fill_manual(name="Legend", values = c("Affected", "Healthy')")) +
  # scale_fill_manual(values=c("#E69F00",'#B3A98C','#605843')) + bkg # flip around as need be
  # scale_fill_manual(values=c("#B3A98C",'#E69F00','#605843')) + bkg # flip around as need be
  scale_fill_manual(values=group.colors) + bkg
plot(p)
dev.off()

# subset only on taxa overlapped in all
plot_data <- subset(plot_data, Taxa=='f__Acidaminococcaceae g__Phascolarctobacterium_A')

# rename for formatting
plot_data$Taxa[plot_data$Taxa == "f__Acidaminococcaceae g__Phascolarctobacterium_A"] <- "Phascolarctobacterium A"
plot_data$Group <- as.character(plot_data$Group)
plot_data$Group[plot_data$Group == "healthy"] <- "Healthy"
names(plot_data)[names(plot_data) == "Group"] <- "Diagnosis"
group.colors <- c(Healthy = colorRampPalette(brewer.pal(9, "Set2"))(ncol)[1])

# pdf("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs16/lefse_AvH_subset.pdf", width=20, height=3)
pdf("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/lefse_AvH_subset.pdf", width=20, height=3)

p <- ggbarplot(plot_data, x="Taxa", y="LDA", fill="Diagnosis", width= 1, color = "white", sort.val = "asc", sort.by.groups=TRUE) +  
  labs(x = "", y = "LDA score", fill="Diagnosis") + coord_flip() + 
  scale_fill_manual(values=group.colors) + bkg
plot(p)
dev.off()