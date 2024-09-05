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
vars1 = c('Phascolarctobacterium_A', 'Phascolarctobacterium_A', 'Prevotella_copri', 'Prevotella_copri',
          'Phascolarctobacterium_A', 'Phascolarctobacterium_A', 'Phascolarctobacterium_A', 'Phascolarctobacterium_A', 'Phascolarctobacterium_A',
          'Phascolarctobacterium_A')
vars2 = c('CRP', 'ESR', 'SJC', 'TJC',
          'CRP', 'CRP', 'CRP', 'CRP', 'CRP',
          'CRP')
groups = c('Diagnosis', 'Diagnosis', 'Diagnosis', 'Diagnosis',
           'Diagnosis', 'Diagnosis', 'Diagnosis', 'Diagnosis', 'Diagnosis',
           'Diagnosis')
subsample = list(c("PsA"),
                 c("PsA"),
                 c("PsA","RA"),
                 c("PsA","RA"),
                 c("RA"),c("PsO"),c("SLE"),c("NSS"),c("SS"),
                 c("RA","PsO","PsA"))
tags = c('phasA_PsA','x', 'x', 'x', 'phasA_RA', 'phasA_PsO', 'phasA_SLE', 'phasA_NSS', 'phasA_SS',
         'phasA_RPP')
# choose colors, corresponding to c("Healthy", "RA", "PsA", "PsO", "SLE", "SS", "NSS")
col1 <- colorRampPalette(brewer.pal(8, "Set2"))(7)
col1 <- colorRampPalette(brewer.pal(9, "Paired"))(7)
# "black"   "#4F9AA6" "#5DB54B" "#FB9A99" "#EB5037" "#FE9425" "#CAB2D6"
col1[1] <- "white"

colors = list(c("PsA" = col1[3]),
              c("PsA" = col1[3]),
              c("PsA" = col1[3], "RA" = col1[2]),
              c("PsA" = col1[3], "RA" = col1[2]),
              c("RA" = col1[1]), c("PsO" = col1[4]), c("SLE" = col1[5]), c("NSS" = col1[7]), c("SS" = col1[6]),
              c("PsA" = col1[3], "RA" = col1[2], "PsO" = col1[4]))
color_map = c("PsA" = col1[3], "RA" = col1[2], "PsO" = col1[4], "SLE" = col1[5], "SS" = col1[6], "NSS" = col1[7])

for (i in seq(1, length(vars1))) {
  # read data
  df = read.table(paste0('/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/inputs/df_otu_meta.csv'), 
                  sep = ',', header = TRUE, row.names = 1, check.names = FALSE,
                  na.strings = "NA")
  df <- df[df$Diagnosis %in% subsample[[i]], ]
  df <- df[complete.cases(df[ , c(1,6)]),]

  # create filenames
  filename_box.plot = paste0(tags[i], '_', vars1[i],'_',vars2[i], "_scatter.plot.pdf")
  
  # create plot
  p <- ggplot(df, aes_string(x = vars1[i], y = vars2[i], fill = groups[[i]])) +
    geom_point(pch=21, size=3) +
    bkg + 
    scale_fill_manual(values = color_map) + 
    # theme(legend.position = "none") + 
    geom_smooth(method = "lm", se = FALSE, aes(color=Diagnosis,  fill=Diagnosis)) +
    scale_color_manual(values = color_map) + 
    labs(x = gsub("_", " ", vars1[i]), y = gsub("_", " ", vars2[i]))

  fpb = paste(dir, filename_box.plot, sep = "")
  print(p)
  ggsave(fpb, plot = p, width = 6, height = 6, units = "in", dpi = 300)
}