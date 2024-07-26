################################# 
## R script                    ##
## Project: AMP AIM            ##
## Taxa                        ##
## Data: 16S                   ##
## Author: Kevin Bu            ##
## Date Created: 7/24/24       ##
#################################

### Load libraries ###
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(rstatix)
library(tidyr)
library(RColorBrewer)

############################################################################
############################################################################
############################################################################

# background theme
bkg <- theme_bw() +
  theme(axis.text.x = element_text(size = 18, color = "black")) +
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 24, color = "black", face = "bold")) +
  theme(axis.title.y = element_text(size = 24, color = "black", face = "bold")) +
  theme(axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), "mm"))) +
  theme(legend.text = element_text(size = 18, color = "black")) +
  theme(legend.title = element_text(size = 24, face = "bold", color = "black"))

############################################################################
############################################################################
############################################################################

# set working dir
dir = "/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/"

### Metadata barplots ###
vars1 = c('Phascolarctobacterium_A', 'Phascolarctobacterium_A', 'Prevotella_copri', 'Prevotella_copri')
vars2 = c('CRP', 'ESR', 'SJC', 'TJC')
groups = c('Diagnosis', 'Diagnosis', 'Diagnosis', 'Diagnosis')
subsample = list(c("PsA"),
                 c("PsA"),
                 c("PsA","RA"),
                 c("PsA","RA"))
  
# choose colors, corresponding to c("Healthy", "RA", "PsA", "PsO", "SLE", "SS", "NSS")
col1 <- colorRampPalette(brewer.pal(8, "Set2"))(7)
colors = list(c("PsA" = col1[3]),
              c("PsA" = col1[3]),
              c("PsA" = col1[3], "RA" = col1[2]),
              c("PsA" = col1[3], "RA" = col1[2]))
color_map = c("PsA" = col1[3], "RA" = col1[2])

for (i in seq(1, length(vars1))) {
  # read data
  df = read.table(paste0('/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/inputs/df_otu_meta.csv'), 
                  sep = ',', header = TRUE, row.names = 1, check.names = FALSE,
                  na.strings = "NA")
  df <- df[df$Diagnosis %in% subsample[[i]], ]

  # create filenames
  filename_box.plot = paste0(vars1[i],'_',vars2[i], "_scatter.plot.pdf")
  
  # create plot
  p <- ggplot(df, aes_string(x = vars1[i], y = vars2[i], fill = groups[[i]])) +
    geom_point(pch=21, size=4) +
    bkg + 
    scale_fill_manual(values = color_map) + 
    # theme(legend.position = "none") + 
    labs(x = gsub("_", " ", vars1[i]), y = gsub("_", " ", vars2[i]))

  fpb = paste(dir, filename_box.plot, sep = "")
  print(p)
  ggsave(fpb, plot = p, width = 6, height = 6, units = "in", dpi = 300)
}