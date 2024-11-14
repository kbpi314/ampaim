library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(stringr)

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



### all data
aimds = c('RA','PsO','PsA','SjD','SLE','NSS','Affected')
jobs = c('jobs09','jobs10','jobs11','jobs12','jobs13','jobs14','jobs16')
levels = list(c('healthy', 'RA'),
              c('healthy', 'PsO'),
              c('healthy', 'PsA'),
              c('healthy', 'SjD'),
              c('healthy', 'SLE'),
              c('healthy', 'NSS'),
              c('healthy', 'Affected')
              )

library(RColorBrewer)

col1 <- colorRampPalette(brewer.pal(9, "Paired"))(7)
# "black"   "#4F9AA6" "#5DB54B" "#FB9A99" "#EB5037" "#FE9425" "#CAB2D6"
col1[1] <- "#5A5A5A"

# order factors for legend
# dx.order = c("Healthy", "RA", "PsA", "PsO", "SLE", "SjD", "NSS")


gc = list(c(healthy = col1[1], RA = col1[2]), # c(healthy = "#B3A98C", RA = "#E69F00"),
          c(healthy = col1[1], PsO = col1[4]),
          c(healthy = col1[1], PsA = col1[3]),
          c(healthy = col1[1], ss = col1[6]),
          c(healthy = col1[1], sle = col1[5]),
          c(healthy = col1[1], nss = col1[7]),
          c(healthy = col1[1], affected = "#E69F00")
)
slices = list(c(),
              c(),
              c(),
              c(),
              c(3,7),
              c(4,9),
              c(NA,NA))

paths = c('8_metabolites', '10_acpa_fecal', '11_acpa_plasma', '12_olink', '13_metabolon', '15_taxa', '16_path')
offsets = c(3, 3, 3, 3, 2, 2, 2)
nvars = c(11, 116, 116, 86, 10, 690, 327)
sizes = c(24, 16, 16, 24, 16, 24, 16)
units = c(" abundance (nmol/mg)", " abundance (MFI)", " abundance (MFI)", " level (NPX)", " abundance (ng/ml)", " abundance", "")
data_fp = c("metabolites.txt", "acpa_fecal.txt", "acpa_plasma.txt", "olink.tsv", "metabolon.tsv", "taxa.tsv", "path.tsv")

### statistics ###
for (k in 1:length(aimds)){
  aimd = aimds[k]
  jobn = jobs[k]
  level = levels[[k]]
  group.colors = gc[[k]]

  
  ### MSQ138+141
  data <-read.table(paste0("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/",jobn,"/lefse_results.res"), header = FALSE, sep = "\t")
  
  # set names
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
  
  
  # exclude non family results
  plot_data <- plot_data[grep("f__", plot_data$Taxa),]
  
  # exclude uncl
  plot_data <- plot_data[grep("uncl", plot_data$Taxa, invert=T),]
  
  # take everything before trailing __
  # plot_data$Taxa <- sub('__*.', '', plot_data$Taxa)
  # plot_data$Taxa <- lapply(str_split(plot_data$Taxa, '__')[-1])
  # plot_data$Taxa <- sub("__.*", "", plot_data$Taxa)
  # plot_data$Taxa <- sub("__[^__]*$", '', plot_data$Taxa)
  
  # take everything after the last __
  plot_data$Taxa <- sub('.*__', '', plot_data$Taxa)
  
  # drop empty taxa strings
  plot_data <- plot_data[!(is.na(plot_data$Taxa) | plot_data$Taxa==""), ]
  
  # sort by LDA magnitude 
  plot_data <- plot_data[order(plot_data$LDA,decreasing=TRUE),]
  
  # remove duplicates in Taxa
  plot_data <- plot_data[!duplicated(plot_data$Taxa),]
  
  # keep first 5 duplicates (top values) for each group
  plot_data <- plot_data %>%
               group_by(Group) %>%
               filter(row_number() <= 5)
  
  
  # save pdf
  pdf(paste0("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/",aimd,"_lefse_pretty.pdf"), width=10, height=6)

  
  p <- ggbarplot(plot_data, x="Taxa", y="LDA", fill="Group", width= 1, color = "white", sort.val = "asc", sort.by.groups=TRUE) +  
    labs(x = "", y = "LDA score", fill="Group") + coord_flip() + 
    #scale_fill_manual(name="Legend", values = c("Affected", "Healthy')")) +
    # scale_fill_manual(values=c("#E69F00",'#B3A98C','#605843')) + bkg # flip around as need be
    # scale_fill_manual(values=c("#B3A98C",'#E69F00','#605843')) + bkg # flip around as need be
    scale_fill_manual(values=group.colors) + bkg + theme(legend.position = "none")
  plot(p)
  dev.off()
}