################################# 
## R script                    ##
## Project: AMP AIM            ##
## Boxplots; glm / medication  ##
## Data: 16S                   ##
## Author: Kevin Bu            ##
## Date Created: 7/24/24       ##
#################################

### Load libraries ###
library(reshape2)
library(phyloseq)
library(vegan)
library(ade4)
library(PMCMR)
library(PMCMRplus)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(tidyverse)
library(dplyr)
library(tidyr)
library(scales)
# library(ggpubr)

############################################################################
############################################################################
############################################################################

### Statistics functions ###

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

# Function to add asterisks for significance levels
add_significance <- function(p_value) {
  if (p_value < 0.0001) {
    return("****")
  } else if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.05) {
    return("*")
  } else {
    return("")
  }
}


############################################################################
############################################################################
############################################################################


### glm ###
df = read.delim('/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/inputs/df_med.tsv', sep = '\t')

# out <- glm(shannon_entropy ~ HCQ + MTX, data = df, family = gaussian)
out <- glm(shannon_entropy ~ Medication_Status, data = df, family = gaussian)

summary(out)

# race association and medication status
glm(formula = shannon_entropy ~ Medication_Status + Age + Race, 
    family = gaussian, data = df)

#paths = c('8_metabolites', '10_acpa_fecal', '11_acpa_plasma', '12_olink', '13_metabolon', '15_taxa', '16_path')
#offsets = c(3, 3, 3, 3, 2, 2, 2)
#nvars = c(1)
#sizes = c(24)
#units = c(" abundance (nmol/mg)", " abundance (MFI)", " abundance (MFI)", " level (NPX)", " abundance (ng/ml)", " abundance", " abundance")
#data_fp = c("metabolites.txt", "acpa_fecal.txt", "acpa_plasma.txt", "olink.tsv", "metabolon.tsv", "taxa.tsv", "path.tsv")

### statistics ###

#for (k in 1:length(paths)){
#  path = paths[k]
#  offset = offsets[k]
#  nvar = nvars[k]
#  unit = units[k]
#  data = data_fp[k]
#  size = sizes[k]
  
# directory
#  dir = paste0("/Users/KevinBu/Desktop/clemente_lab/Projects/twinsra/inputs/RA_twins/16S/jobs/",path,"/")
  
# read in data table
#d <- read.table(file = paste(dir, data, sep = ""),
#                  header = TRUE, row.names = 1, sep = "\t", check.names = FALSE,
#                  na.strings = "NA")
  


# background theme
bkg <- theme_bw() +
  theme(axis.text.x = element_text(size = 18, color = "black",face = "bold")) +
  theme(axis.text.y = element_text(size = 12, color = "black")) +
  theme(axis.title.x = element_text(size = 18, color = "black", face = "bold")) +
  theme(axis.title.x = element_text(margin = unit(c(0, 8, 0, 0), "mm"))) +
  theme(axis.title.y = element_text(size = 18, color = "black", face = "bold")) +
  theme(axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), "mm"))) +
  theme(legend.text = element_text(size = 18, color = "black")) +
  theme(legend.title = element_text(size = 24, face = "bold", color = "black"))
  
  
# directory for storing files
dir = "/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/"

# colors
col1 <- c("#ce2525","#929aab")
col1 <- c("#B3A98C", "#E69F00")
col1 <- c("blue","#ADD8E6")


# plot boxplot
p <- ggplot(data = df, aes(x = Medication_Status, y = shannon_entropy, fill = Medication_Status)) +
  stat_summary(fun.data = stats.whiskers, geom = "errorbar", 
               color = "black", size = 0.8, width = 0.3) +
  stat_summary(fun.data = stats.boxplot, geom = "crossbar", 
               color = "black", size = 0.5, width = 0.5) +
  geom_jitter(width = 0.1, size = 1.5) +
  scale_x_discrete(labels = c("Treated", "Untreated")) +
  scale_fill_manual(values = col1) +      
  xlab(NULL) +
  ylab('Alpha Diversity (Shannon)') +
  bkg +
  theme(legend.position = "none")

fpb = paste(dir, 'alpha_medication.pdf', sep = "")
pdf(file = fpb, height = 4.5, width = 5)
plot(p)
dev.off()
    