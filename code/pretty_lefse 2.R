library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggpubr)
#######
bkg <-
  theme(axis.text.x = element_text(size = 24, color = "black")) +
  theme(axis.text.x = element_text(angle=90, vjust = 0.5, hjust = 1)) +
  theme(axis.title.x = element_text(margin = unit(c(0,0,4,0), "mm"))) +
  theme(axis.title.x = element_text(size = 24, color = "black")) +
  theme(axis.text.y = element_text(size = 24, color = "black")) +
  theme(axis.title.y = element_text(size = 24, color = "black")) +
  theme(axis.title.y = element_text(margin = unit(c(0,4,0,0), "mm"))) +
  theme(legend.position = "right") +
  theme(legend.title = element_text(size = 24)) +
  theme(legend.text = element_text(size = 24)) +
  theme(legend.key.size = unit(0.6, 'cm')) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(size=, color= "black", face ="bold")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(size=24, color= "black"))
####

### MSQ138 only

# AvH
# data <-read.table("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/graphs/Lefse_3/lefse_results.res", header = FALSE, sep = "\t")
# RAvH
# data <-read.table("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/graphs/Lefse_4/lefse_results.res", header = FALSE, sep = "\t")
# PsAvH
# data <-read.table("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs02/lefse_results.res", header = FALSE, sep = "\t")
# PsOvH
# data <-read.table("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs03/lefse_results.res", header = FALSE, sep = "\t")
# SSvH
# data <-read.table("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/graphs/Lefse_6/lefse_results.res", header = FALSE, sep = "\t")
# SLEvH
# data <-read.table("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/graphs/Lefse_7/lefse_results.res", header = FALSE, sep = "\t")

### MSQ141 only
#SSvNSS
#data <-read.table("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs15/lefse_results.res", header = FALSE, sep = "\t")


### MSQ138+141
# RAvH
#data <-read.table("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs09/lefse_results.res", header = FALSE, sep = "\t")
# PsOvH
#data <-read.table("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs10/lefse_results.res", header = FALSE, sep = "\t")
# PsAvH
#data <-read.table("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs11/lefse_results.res", header = FALSE, sep = "\t")
# SSvH
# data <-read.table("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs12/lefse_results.res", header = FALSE, sep = "\t")
# SLEvH
#data <-read.table("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs13/lefse_results.res", header = FALSE, sep = "\t")
# SICCAvH
#data <-read.table("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs14/lefse_results.res", header = FALSE, sep = "\t")
# AvH
# data <-read.table("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs16/lefse_results.res", header = FALSE, sep = "\t")


# L7
# AvH MSQ138
data <-read.table("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs17/lefse_results.res", header = FALSE, sep = "\t")
# AvH MSQ138+141
data <-read.table("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs18/lefse_results.res", header = FALSE, sep = "\t")


names(data) <- c("RawTaxa", "X", "Group", "LDA", "pval")

plot_data <- subset(data, !is.na(data$LDA))
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
#plot_data$Taxa <- sub(".*_s__", '', plot_data$RawTaxa)
plot_data[plot_data$Group == "Control",]$LDA <- -1 * plot_data[plot_data$Group == "Control",]$LDA

# set colors and factors
# group.colors <- c(healthy = "#B3A98C", affected = "#E69F00")
#plot_data$Group <- factor(plot_data$Group, levels = c('healthy', 'affected'))

#group.colors <- c(healthy = "#B3A98C", RA = "#E69F00")
#plot_data$Group <- factor(plot_data$Group, levels = c('healthy', 'RA'))

#group.colors <- c(healthy = "#B3A98C", psa = "#E69F00")
#plot_data$Group <- factor(plot_data$Group, levels = c('healthy', 'psa'))

#group.colors <- c(healthy = "#B3A98C", pso = "#E69F00")
# plot_data$Group <- factor(plot_data$Group, levels = c('healthy', 'pso'))

#group.colors <- c(healthy = "#B3A98C", ss = "#E69F00")
#plot_data$Group <- factor(plot_data$Group, levels = c('healthy', 'ss'))

#group.colors <- c(healthy = "#B3A98C", sle = "#E69F00")
#plot_data$Group <- factor(plot_data$Group, levels = c('healthy', 'sle'))

#group.colors <- c(healthy = "#B3A98C", 'non-sjogrens sicca' = "#E69F00") 
#plot_data$Group <- factor(plot_data$Group, levels = c('healthy', 'non-sjogrens sicca'))

#group.colors <- c('non-sjogrens sicca' = "#B3A98C", 'ss' = "#E69F00")
#plot_data$Group <- factor(plot_data$Group, levels = c('non-sjogrens sicca', 'ss'))

group.colors <- c(healthy = "#B3A98C", 'non-sjogrens sicca' = "#E69F00", RA = "#E69F00",psa = "#E69F00",pso = "#E69F00",ss = "#E69F00",sle = "#E69F00" ) 
plot_data$Group <- factor(plot_data$Group, levels = c('healthy', 'non-sjogrens sicca', 'ss', 'sle', 'RA', 'psa', 'pso'))


###
# MSQ138 only
###

# AvH
# pdf("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/graphs/Lefse_3/lefse_pretty.pdf", width=20, height=20)
# RAvH
#pdf("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/graphs/Lefse_4/lefse_pretty.pdf", width=20, height=20)
# PsAvH
# pdf("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs02/lefse_pretty.pdf", width=20, height=20)
# PsOvH
# pdf("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs03/lefse_pretty.pdf", width=20, height=20)
# SSvH
# pdf("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/graphs/Lefse_6/lefse_pretty.pdf", width=20, height=20)
# SLEvH
# pdf("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/graphs/Lefse_7/lefse_pretty.pdf", width=20, height=20)

###
# MSQ141
###
# SSvNSS
#pdf("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs15/lefse_pretty.pdf", width=20, height=20)

###
# MSQ138 + 141
###
# RAvH
#pdf("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs09/lefse_pretty.pdf", width=20, height=20)
# PsOvH
#pdf("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs10/lefse_pretty.pdf", width=20, height=20)
# PsAvH
# pdf("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs11/lefse_pretty.pdf", width=20, height=20)
# SSvH
# pdf("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs12/lefse_pretty.pdf", width=20, height=20)
# SLEvH
#pdf("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs13/lefse_pretty.pdf", width=20, height=20)
# SICCAvH
#pdf("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs14/lefse_pretty.pdf", width=20, height=20)
# AvH
#pdf("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs16/lefse_pretty.pdf", width=20, height=20)
# AvH L7 MSQ138
pdf("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs17/lefse_pretty.pdf", width=20, height=20)
# AvH L7 MSQ138+141
pdf("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs18/lefse_pretty.pdf", width=20, height=20)



p <- ggbarplot(plot_data, x="Taxa", y="LDA", fill="Group", width= 1, color = "white", sort.val = "asc", sort.by.groups=TRUE) +  
  labs(x = "", y = "LDA score", fill="Group") + coord_flip() + 
  #scale_fill_manual(name="Legend", values = c("Affected", "Healthy')")) +
  # scale_fill_manual(values=c("#E69F00",'#B3A98C','#605843')) + bkg # flip around as need be
  # scale_fill_manual(values=c("#B3A98C",'#E69F00','#605843')) + bkg # flip around as need be
  scale_fill_manual(values=group.colors) + bkg
plot(p)
dev.off()
