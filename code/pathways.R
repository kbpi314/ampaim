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
  theme(axis.title.y = element_text(size = 18, color = "black", face = "bold")) +
  theme(axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), "mm"))) +
  theme(legend.text = element_text(size = 18, color = "black")) +
  theme(legend.title = element_text(size = 24, face = "bold", color = "black"))


dir = "/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/"

### Metadata barplots ###
vars = c('Mannose_TypeO_glycan_Biosynthesis')
groups = c('Diagnosis')

# choose colors, corresponding to c("Healthy", "RA", "PsA", "PsO", "SLE", "SS", "NSS")
col1 <- colorRampPalette(brewer.pal(8, "Set2"))(7)
col1 <- c('#B0E0E6', '#8B0000')
dx.order = c("Healthy", "AIMD") #RA", "PsA", "PsO", "SLE", "SS", "NSS")

# read data
df = read.table(paste0('/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/mwu_glycan.tsv'), 
                sep = '\t', header = TRUE, row.names = 1, check.names = FALSE,
                na.strings = "NA")

for (i in seq(1, length(vars))) {
  # get number of groups
  n_groups = length(unique(df[[groups[i]]]))
  
  # create tables for storing wilcoxon and ttest results
  stats.table.all <- matrix(data = NA, nrow = 1, ncol = 3)
  
  # create rows for each variable
  stats.table.all[1,1] <- vars[i]
  
  # check number of groups, if statement
  if (n_groups == 2){
    colnames(stats.table.all) <- c("metadata", "wilcoxon", "ttest")
    stats.table.all[1,2] <- wilcox.test(formula(paste(vars[i],'~',groups[i])), data = df, paired = FALSE, exact=FALSE, correct=TRUE)$p.value
    stats.table.all[1,3] <- t.test(formula(paste(vars[i],'~',groups[i])), data = df, paired = FALSE)$p.value
  }  else {
    colnames(stats.table.all) <- c("metadata", "kruskal wallis", "anova")
    stats.table.all[1,2] <- kruskal.test(df[[vars[i]]] ~ df[[groups[i]]], data = df)$p.value
    stats.table.all[1,3] <- oneway.test(df[[vars[i]]] ~ df[[groups[i]]], data = df)$p.value
  }
  
  # save
  ft.all = paste0(dir, vars[i], "_stats.csv")
  write.csv(file = ft.all, stats.table.all)
  
  ### bar plot ###
  
  # create filenames
  filename_box.plot = paste0(vars[i], "_box.plot.pdf")
  
  # rewrite order of factors
  df[[groups[i]]] <- factor(df[[groups[i]]], levels = dx.order)
  
  # create plot
  p <- ggplot(df, aes_string(x = groups[i], y = vars[i], fill = groups[i])) +
    geom_boxplot() +
    bkg + 
    # theme_minimal() +
    theme(legend.position = "none") + 
    labs(x = gsub("_", " ", groups[i]), y = gsub("_", " ", vars[i])) +
    geom_jitter(width = 0.2, alpha = 0.7, size = 2) + 
    geom_pwc(method = 'wilcox.test', label = 'p.signif',  hide.ns = TRUE, p.adjust.method='none', ref.group='Healthy') +
    scale_fill_manual(values = col1)#  + 
  
  fpb = paste(dir, filename_box.plot, sep = "")
  print(p)
  ggsave(fpb, plot = p, width = 6, height = 6, units = "in", dpi = 300)
}


############################################################################
############################################################################
############################################################################

# background theme
bkg <- theme_bw() +
  theme(axis.text.x = element_text(size = 18, color = "black")) +
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 24, color = "black", face = "bold")) +
  theme(axis.title.y = element_text(size = 18, color = "black", face = "bold")) +
  theme(axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), "mm"))) +
  theme(legend.text = element_text(size = 18, color = "black")) +
  theme(legend.title = element_text(size = 24, face = "bold", color = "black"))

############################################################################
############################################################################
############################################################################

# set working dir
dir = "/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/"

### Metadata barplots ###
vars1 = c('Coprobacter')
vars2 = c('Mannose_TypeO_glycan_Biosynthesis')
#groups = c('Diagnosis')
#subsample = list(c("PsA"),
#                 c("PsA"),
#                 c("PsA","RA"),
#                 c("PsA","RA"))

# choose colors, corresponding to c("Healthy", "RA", "PsA", "PsO", "SLE", "SS", "NSS")
#col1 <- colorRampPalette(brewer.pal(8, "Set2"))(7)
#colors = list(c("PsA" = col1[3]),
##              c("PsA" = col1[3]),
#              c("PsA" = col1[3], "RA" = col1[2]),
#              c("PsA" = col1[3], "RA" = col1[2]))
#color_map = c("PsA" = col1[3], "RA" = col1[2])

for (i in seq(1, length(vars1))) {
  # read data
  df = read.table(paste0('/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/corr_glycan.tsv'), 
                  sep = '\t', header = TRUE, row.names = 1, check.names = FALSE,
                  na.strings = "NA")
  # df <- df[df$Diagnosis %in% subsample[[i]], ]
  
  # create filenames
  filename_box.plot = paste0(vars1[i],'_',vars2[i], "_scatter.plot.pdf")
  
  # create plot
  p <- ggplot(df, aes_string(x = vars1[i], y = vars2[i], fill = groups[[i]])) +
    geom_point(pch=21, size=4) +
    bkg + 
    scale_fill_manual(values = '#8B0000') + 
    theme(legend.position = "none") + 
    labs(x = gsub("_", " ", vars1[i]), y = gsub("_", " ", vars2[i]))
  
  fpb = paste(dir, filename_box.plot, sep = "")
  print(p)
  ggsave(fpb, plot = p, width = 6, height = 6, units = "in", dpi = 300)
}



### 
# per disease
###
dir = "/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/"

df_strat = read.table('/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/mwu_glycan_strat.tsv', 
                      sep = '\t', header = TRUE, row.names = 1, check.names = FALSE,
                      na.strings = "NA")
d.final <- df_strat

# create tables for storing wilcoxon and ttest results
stats.table.all <- matrix(data = NA, nrow = 1, ncol = 3)

# calculate adiv
# create tables for storing wilcoxon and ttest results
stats.table.all <- matrix(data = NA, nrow = 1, ncol = 3)

# create rows for each variable
vars = c('Mannose_TypeO_glycan_Biosynthesis')
groups = c('Diagnosis')
stats.table.all[1,1] <- vars[i]

# save
ft.all = paste(dir, "Mannose_TypeO_glycan_Biosynthesis.csv", sep = "")
write.csv(file = ft.all, stats.table.all)

# background theme
bkg <- theme_bw() +
  theme(axis.text.x = element_text(size = 18, color = "black")) +
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 24, color = "black", face = "bold")) +
  theme(axis.title.y = element_text(size = 18, color = "black", face = "bold")) +
  theme(axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), "mm"))) +
  theme(legend.text = element_text(size = 18, color = "black")) +
  theme(legend.title = element_text(size = 24, face = "bold", color = "black"))

# choose colors
col1 <- colorRampPalette(brewer.pal(9, "Spectral"))(length(unique(d.final$Diagnosis)))
col1 <- colorRampPalette(brewer.pal(9, "Set2"))(length(unique(d.final$Diagnosis)))

# variable of interest
a <- 'Mannose_TypeO_glycan_Biosynthesis'

# create filenames
filename_box.plot = paste(a, "all_box.plot.pdf", sep = "_")  

# rewrite order of factors
d.final <- df_strat[, c("Diagnosis","Mannose_TypeO_glycan_Biosynthesis")]
dx.order = c("Healthy", "RA", "PsA", "PsO", "SLE", "SS", "NSS")
d.final$Diagnosis <- factor(d.final$Diagnosis, levels = dx.order)

# get all pairs
# pairs <- combn(dx.order, 2, simplify = FALSE)

# Convert each pair into a list element
# pair_list <- lapply(seq_along(pairs), function(i) pairs[[i]])

# create plot
p <- ggplot(d.final, aes(x = Diagnosis, y = Mannose_TypeO_glycan_Biosynthesis, fill = Diagnosis)) +
  geom_boxplot() +
  bkg + 
  # theme_minimal() +
  theme(legend.position = "none") + 
  labs(x = "Diagnosis", y = "Mannose TypeO glycan Biosynthesis") +
  geom_jitter(width = 0.2, alpha = 0.7, size = 2) + 
  geom_pwc(method = 'wilcox.test', label = 'p.signif',  hide.ns = TRUE, p.adjust.method='none') +
  scale_fill_manual(values = col1)#  + 

fpb = paste(dir, filename_box.plot, sep = "")
print(p)
ggsave(fpb, plot = p, width = 6, height = 6, units = "in", dpi = 300)

###
# Copro stratified


dir = "/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/"

### Metadata barplots ###
vars1 = c('Coprobacter')
vars2 = c('Mannose_TypeO_glycan_Biosynthesis')
#groups = c('Diagnosis')
#subsample = list(c("PsA"),
#                 c("PsA"),
#                 c("PsA","RA"),
#                 c("PsA","RA"))

# choose colors, corresponding to c("Healthy", "RA", "PsA", "PsO", "SLE", "SS", "NSS")
#col1 <- colorRampPalette(brewer.pal(8, "Set2"))(7)
#colors = list(c("PsA" = col1[3]),
##              c("PsA" = col1[3]),
#              c("PsA" = col1[3], "RA" = col1[2]),
#              c("PsA" = col1[3], "RA" = col1[2]))
#color_map = c("PsA" = col1[3], "RA" = col1[2])

for (i in seq(1, length(vars1))) {
  # read data
  df = read.table(paste0('/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/corr_glycan_strat.tsv'), 
                  sep = '\t', header = TRUE, row.names = 1, check.names = FALSE,
                  na.strings = "NA")
  # df <- df[df$Diagnosis %in% subsample[[i]], ]
  
  # create filenames
  filename_box.plot = paste0(vars1[i],'_',vars2[i], "_strat_scatter.plot.pdf")
  
  # create plot
  p <- ggplot(df, aes_string(x = vars1[i], y = vars2[i], fill = groups[[i]])) +
    geom_point(pch=21, size=4) +
    bkg + 
    scale_fill_manual(values = col1) + 
    theme(legend.position = "none") + 
    labs(x = gsub("_", " ", vars1[i]), y = gsub("_", " ", vars2[i]))
  
  fpb = paste(dir, filename_box.plot, sep = "")
  print(p)
  ggsave(fpb, plot = p, width = 6, height = 6, units = "in", dpi = 300)
}


