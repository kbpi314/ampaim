################################# 
## R script                    ##
## Project: AMP AIM            ##
## NSS vs SS                   ##
## Data: 16S                   ##
## Author: Kevin Bu            ##
## Date Created: 7/25/24       ##
#################################

### Load libraries ###
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(rstatix)
library(tidyr)
library(RColorBrewer)

### alpha plot ###

# set working dir
dir = "/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/"

### Alpha Diversity Boxplots ###
df_alpha = read.table('/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/df_alpha_nss_ss.tsv', 
                      sep = '\t', header = TRUE, row.names = 1, check.names = FALSE,
                      na.strings = "NA")


# create tables for storing wilcoxon and ttest results
stats.table.all <- matrix(data = NA, nrow = 1, ncol = 3)

# check number of groups, if statement
# colnames(stats.table.all) <- c("alpha div", "wilcoxon", "ttest")
colnames(stats.table.all) <- c("alpha div", "kruskal wallis", "anova")

# calculate adiv
stats.table.all[1,1] <- colnames(df_alpha)[2]
# stats.table.all[1,2] <- wilcox.test(df_alpha[,4] ~ Diagnosis, data = df_alpha, paired = TRUE)$p.value
# stats.table.all[1,3] <- t.test(df_alpha[,4] ~ Diagnosis, data = df_alpha, paired = TRUE)$p.value
stats.table.all[1,2] <- kruskal.test(df_alpha[,2] ~ Diagnosis, data = df_alpha)$p.value
stats.table.all[1,3] <- oneway.test(df_alpha[,2] ~ Diagnosis, data = df_alpha)$p.value

# save
ft.all = paste(dir, "alpha_nss_ss_stats.csv", sep = "")
write.csv(file = ft.all, stats.table.all)


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
colbase <- colorRampPalette(brewer.pal(9, "Set2"))(7)
# c("Healthy", "RA", "PsA", "PsO", "SLE", "SS", "NSS")
col1 <- c(colbase[7], colbase[6])

# variable of interest
a <- 'shannon_entropy'

# create filenames
filename_box.plot = paste("alpha_nss_ss_box.plot.pdf", sep = "_")  

# rewrite order of factors
d.final <- df_alpha[, c("Diagnosis","shannon_entropy")]
dx.order = c("NSS", "SS")
d.final$Diagnosis <- factor(d.final$Diagnosis, levels = dx.order)

# get all pairs
# pairs <- combn(dx.order, 2, simplify = FALSE)

# Convert each pair into a list element
# pair_list <- lapply(seq_along(pairs), function(i) pairs[[i]])

# create plot
p <- ggplot(d.final, aes(x = Diagnosis, y = shannon_entropy, fill = Diagnosis)) +
  geom_boxplot() +
  bkg + 
  # theme_minimal() +
  theme(legend.position = "none") + 
  labs(x = "Diagnosis", y = "Shannon Entropy") +
  geom_jitter(width = 0.2, alpha = 0.7, size = 2) + 
  geom_pwc(method = 'wilcox.test', label = 'p.signif',  hide.ns = TRUE, p.adjust.method='none') +
  scale_fill_manual(values = col1)#  + 

fpb = paste(dir, filename_box.plot, sep = "")
print(p)
ggsave(fpb, plot = p, width = 6, height = 6, units = "in", dpi = 300)


### beta ###
dists <- c('unifrac')

# load data
df <- read.delim(file="/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/bray_curtis_pcoa_nss_ss.tsv",
                 row.names=1)

# order factors for legend
df$Diagnosis <- factor(df$Diagnosis, levels=c('NSS', 'SS'))

# function to specify that axis labels have 2 decimal places
f.dec <- function(x){
  format(round(x, 2), nsmall = 2)
}

for (j in seq_along(dists)) {
  
  # create filenames
  filename_plot = paste("bdiv", dists[j], "plot_nss_ss.pdf", sep = "_")
  
  # plot beta diversity
  p <- ggplot() + # data=df, aes(x = PC1, y = PC2, fill = Diagnosis)) +
    geom_point(data = df, aes(x = PC1, y = PC2, color = Diagnosis),size=4) +
    scale_color_manual(values = c("NSS" = col1[1], "SS" = col1[2])) + #col1, labels = c("Unaffected", "RA")) +
    bkg +
    scale_x_continuous(labels = f.dec) + # 2 decimal places on x-axis
    scale_y_continuous(labels = f.dec)   # 2 decimal places on y-axis
  
  # save plot
  fp = paste(dir, filename_plot, sep = "")
  pdf(file = fp, height = 6, width = 8)
  plot(p)
  dev.off()
}



### LEfSe ###
# set background theme
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
#

# load data
data <-read.table("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs08/nss_ss/lefse/lefse_results.res", header = FALSE, sep = "\t")
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
group.colors <- c(NSS = col1[1], SS = col1[2]) # B3A98C E69F00
# levels(plot_data$Group) <- c("NSS", "SS")
plot_data$Group[plot_data$Group == "nss"] <- "NSS"
plot_data$Group[plot_data$Group == "ss"] <- "SS"
plot_data$Group <- factor(plot_data$Group, levels = c('NSS', 'SS'))


pdf("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/lefse_NSSvSS.pdf", width=20, height=20)


p <- ggbarplot(plot_data, x="Taxa", y="LDA", fill="Group", width= 1, color = "white", sort.val = "asc", sort.by.groups=TRUE) +  
  labs(x = "", y = "LDA score", fill="Group") + coord_flip() + 
  #scale_fill_manual(name="Legend", values = c("Affected", "Healthy')")) +
  # scale_fill_manual(values=c("#E69F00",'#B3A98C','#605843')) + bkg # flip around as need be
  # scale_fill_manual(values=c("#B3A98C",'#E69F00','#605843')) + bkg # flip around as need be
  scale_fill_manual(values=group.colors) + bkg
plot(p)
dev.off()

