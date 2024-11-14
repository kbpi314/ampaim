################################# 
## R script                    ##
## Project: AMP AIM            ##
## Specific Taxa               ##
## Data: 16S                   ##
## Author: Kevin Bu            ##
## Date Created: 7/15/24       ##
#################################

### Load libraries ###
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(rstatix)
library(tidyr)
library(RColorBrewer)

# All plot statistics: mean, std deviation, median, min value, max value, 10%ile, 25%ile, 75%ile, 90%ile
stats.all = function(x) {
  mean <- mean(x)
  stddev <- sd(x)
  median <- median(x)
  val_min <- min(x)
  val_max <- max(x)
  per10 <- as.numeric(quantile(x, prob = c(0.10)))
  per25 <- as.numeric(quantile(x, prob = c(0.25)))
  per75 <- as.numeric(quantile(x, prob = c(0.75)))
  per90 <- as.numeric(quantile(x, prob = c(0.90)))
  return(c(mean = mean, sd = stddev, median = median, 
           val_min = val_min, val_max = val_max, 
           per10 = per10, per25 = per25, per75 = per75,  per90 = per90))
}

# Boxplot statistics: median, 25%ile, 75%ile
stats.boxplot <- function(x) {
  m <- median(x)
  per25 <- as.numeric(quantile(x, prob = c(0.25)))
  per75 <- as.numeric(quantile(x, prob = c(0.75)))
  return(c(y = m, ymin = per25, ymax = per75))
}

# Whiskers statistics: median, 10th percentile, 90th percentile
stats.whiskers = function(x) {
  m <- median(x)
  per10 <- as.numeric(quantile(x, prob = c(0.10)))
  per90 <- as.numeric(quantile(x, prob = c(0.90)))
  return(c(y = m, ymin = per10, ymax = per90))
}

# Outliers
min.outlier <- function(x) {
  subset(x, quantile(x, prob = c(0.10)) > x)
}

max.outlier <- function(x) {
  subset(x, quantile(x, prob = c(0.90)) < x)
}

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

### Alpha Diversity Boxplots ###
vars = c('Phascolarctobacterium_A', 'Coprobacter', 'shannon_entropy')
files = c('Phascolarctobacterium_A', 'Coprobacter', 'df_alpha')
groups = c('Diagnosis', 'Diagnosis', 'Diagnosis')
orders = list(c("Healthy", "RA", "PsA", "PsO", "SLE", "SjD", "NSS"), 
              c("Healthy", "RA", "PsA", "PsO", "SLE", "SjD", "NSS"), 
           c("Healthy", "RA", "PsA", "PsO", "SLE", "SjD", "NSS")
)

for (i in seq(1, length(files))) {
  # read data
  df = read.table(paste0('/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/', files[i], '.tsv'), 
                  sep = '\t', header = TRUE, row.names = 1, check.names = FALSE,
                  na.strings = "NA")
  
  # choose colors
  col1 <- colorRampPalette(brewer.pal(8, "Set2"))(length(unique(df[[groups[i]]])))
  col1 <- colorRampPalette(brewer.pal(9, "Paired"))(ncol)
  col1[1] <- 'white'

    # get number of groups
  n_groups = length(unique(df[[groups[i]]]))

  # create tables for storing wilcoxon and ttest results
  stats.table.all <- matrix(data = NA, nrow = 1, ncol = 3)

  # create rows for each variable
  stats.table.all[1,1] <- vars[i]
  
  # check number of groups, if statement
  # if (n_groups == 2){
  #  colnames(stats.table.all) <- c("alpha div", "wilcoxon", "ttest")
  #  stats.table.all[1,2] <- wilcox.test(df[[vars[i]]] ~ df[[groups[i]]], data = df, paired = TRUE)$p.value
  #  stats.table.all[1,3] <- t.test(df[[vars[i]]] ~ df[[groups[i]]], data = df, paired = TRUE)$p.value
  #}  else {
  colnames(stats.table.all) <- c("alpha div", "kruskal wallis", "anova")
  stats.table.all[1,2] <- kruskal.test(df[[vars[i]]] ~ df[[groups[i]]], data = df)$p.value
  stats.table.all[1,3] <- oneway.test(df[[vars[i]]] ~ df[[groups[i]]], data = df)$p.value
  # }
  
  # save
  ft.all = paste0(dir, files[i], "_stats.csv")
  write.csv(file = ft.all, stats.table.all)

  ### bar plot ###
  
  # create filenames
  filename_box.plot = paste0(files[i], "_box.plot.pdf")
  
  # rewrite order of factors
  df[[groups[i]]] <- factor(df[[groups[i]]], levels = orders[[i]])
  
  # create plot
  p <- ggplot(df, aes_string(x = groups[i], y = vars[i], fill = groups[i])) +
    geom_boxplot() +
    bkg + 
    # theme_minimal() +
    theme(legend.position = "none") + 
    labs(x = groups[i], y = vars[i]) +
    geom_jitter(width = 0.2, alpha = 0.7, size = 2) + 
    geom_pwc(method = 'wilcox.test', label = 'p.signif',  hide.ns = TRUE, p.adjust.method='none', ref.group=orders[[i]][1]) +
    scale_fill_manual(values = col1)#  + 
  
  fpb = paste(dir, filename_box.plot, sep = "")
  print(p)
  ggsave(fpb, plot = p, width = 6, height = 6, units = "in", dpi = 300)
}