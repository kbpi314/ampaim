

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

############################################################################
############################################################################
############################################################################

# set working dir
dir = "/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/"

### Alpha Diversity Boxplots ###
df_alpha = read.table('/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/df_alpha.tsv', 
                      sep = '\t', header = TRUE, row.names = 1, check.names = FALSE,
                      na.strings = "NA")

df_alpha = df_alpha[df_alpha$Diagnosis %in% c("Healthy", "RA", "PsA", "PsO"),]

# create tables for storing wilcoxon and ttest results
stats.table.all <- matrix(data = NA, nrow = 1, ncol = 3)

# check number of groups, if statement
# colnames(stats.table.all) <- c("alpha div", "wilcoxon", "ttest")
colnames(stats.table.all) <- c("alpha div", "kruskal wallis", "anova")

# calculate adiv
stats.table.all[1,1] <- colnames(df_alpha)[4]
# stats.table.all[1,2] <- wilcox.test(df_alpha[,4] ~ Diagnosis, data = df_alpha, paired = TRUE)$p.value
# stats.table.all[1,3] <- t.test(df_alpha[,4] ~ Diagnosis, data = df_alpha, paired = TRUE)$p.value
stats.table.all[1,2] <- kruskal.test(df_alpha[,4] ~ Diagnosis, data = df_alpha)$p.value
stats.table.all[1,3] <- oneway.test(df_alpha[,4] ~ Diagnosis, data = df_alpha)$p.value

# save
ft.all = paste(dir, "alpha_stats_KL2.csv", sep = "")
write.csv(file = ft.all, stats.table.all)


############################################################################
############################################################################
############################################################################

### alpha plot ###

# background theme
bkg <- theme_bw() +
  theme(axis.text.x = element_text(size = 18, color = "black")) +
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 24, color = "black", face = "bold")) +
  theme(axis.title.y = element_text(size = 24, color = "black", face = "bold")) +
  theme(axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), "mm"))) +
  theme(legend.text = element_text(size = 18, color = "black")) +
  theme(legend.title = element_text(size = 24, face = "bold", color = "black"))

# choose colors
col1 <- colorRampPalette(brewer.pal(9, "Spectral"))(length(unique(df_alpha$Diagnosis)))
col1 <- colorRampPalette(brewer.pal(9, "Set2"))(length(unique(df_alpha$Diagnosis)))
col1 <- colorRampPalette(brewer.pal(9, "Paired"))(length(unique(df_alpha$Diagnosis)))
# "black"   "#4F9AA6" "#5DB54B" "#FB9A99" "#EB5037" "#FE9425" "#CAB2D6"
col1[1] <- "white"

# variable of interest
a <- 'shannon_entropy'

# create filenames
filename_box.plot = paste(a, "all_box.plot.KL2.pdf", sep = "_")  

# rewrite order of factors
d.final <- df_alpha[, c("Diagnosis","shannon_entropy")]
dx.order = c("Healthy", "RA", "PsA", "PsO")#, "SLE", "SjD", "NSS")
d.final$Diagnosis <- factor(d.final$Diagnosis, levels = dx.order)

# get all pairs
# pairs <- combn(dx.order, 2, simplify = FALSE)

# Convert each pair into a list element
# pair_list <- lapply(seq_along(pairs), function(i) pairs[[i]])

# create plot
p <- ggplot(d.final, aes(x = Diagnosis, y = shannon_entropy, fill = Diagnosis)) +
  geom_boxplot(outlier.shape = NA) +
  bkg + 
  # theme_minimal() +
  theme(legend.position = "none") + 
  labs(x = "Diagnosis", y = "Alpha Diversity (Shannon)") +
  geom_jitter(width = 0.2, alpha = 0.7, size = 2) + 
  geom_pwc(method = 'wilcox.test', label = 'p.signif',  hide.ns = TRUE, p.adjust.method='none',
           symnum.args=list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("****", "***", "**", "*", "ns"))) +
  scale_fill_manual(values = col1)#  + 

fpb = paste(dir, filename_box.plot, sep = "")
print(p)
ggsave(fpb, plot = p, width = 6, height = 6, units = "in", dpi = 300)
