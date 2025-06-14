### Load libraries ###
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(rstatix)
library(tidyr)
library(RColorBrewer)

# set working dir
dir = "/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs99/"

# load in df
df_alpha = read.table('/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs99/df_alpha_shannon_entropy.tsv', 
                      sep = '\t', header = TRUE, row.names = 1, check.names = FALSE,
                      na.strings = "NA")

# background theme
bkg <- theme_bw() +
  theme(axis.text.x = element_text(size = 14, color = "black")) +
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 24, color = "black", face = "bold")) +
  theme(axis.title.y = element_text(size = 24, color = "black", face = "bold")) +
  theme(axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), "mm"))) +
  theme(legend.text = element_text(size = 18, color = "black")) +
  theme(legend.title = element_text(size = 24, face = "bold", color = "black"))

# choose colors
col1 <- colorRampPalette(brewer.pal(9, "Spectral"))(length(unique(df_alpha$Diagnosis)))
col1 <- colorRampPalette(brewer.pal(9, "Set2"))(length(unique(df_alpha$Diagnosis)))
col1 <- colorRampPalette(brewer.pal(9, "Paired"))(7) # we FORCE the colors per AMPAIM where this is 7 except no NSS so 6
#c("Healthy", "RA", "PsA", "PsO", "SLE", "SjD", "NSS")
# "black"   "#4F9AA6" "#5DB54B" "#FB9A99" "#EB5037" "#FE9425" "#CAB2D6"
col1 <- c('white', # HC
          "#4F9AA6", # RA
          "#FB9A99", # PsO
          "#EB5037", # SLE; comment this out if issue
          "#FE9425",# SjD
          "#CAB2D6", #PsA
          'purple' # AxSpA 
          ) 

# variable of interest
a <- 'shannon_entropy'

# create filenames
filename_box.plot = paste(a, "all_box.plot.pdf", sep = "_")  

# rewrite order of factors
d.final <- df_alpha[, c("cohort","Diagnosis","shannon_entropy")]

# rename HC to Healthy
d.final[d.final=='HC'] <- 'Healthy'

# change dx order
dx.order = c("Healthy", "RA", "PsO", "SLE", 
             "SjD",'PsA','AxSpA')
d.final$Diagnosis <- factor(d.final$Diagnosis, levels = dx.order)

# change cohort order 
# Diagnoses: 
c.order = c("AMPAIM",
            'Valid4RA',
            'Valid7RA',
            
            "Valid5PsO",   
            'Valid8PsO',
            "Valid6PsO",  
            
            'Valid10PsO',
            'Valid9PsD',
            "Valid1SLE",
            "Valid6SLE",
            
            "Valid3SjD",
            'Valid2AxSpA'
            ) 
d.final$cohort <- factor(d.final$cohort, levels = c.order)

# create plot
p <- ggplot(d.final, aes(x = cohort, y = shannon_entropy, fill = Diagnosis)) +
  geom_boxplot(outlier.shape = NA) +
  # geom_point(position='jitter') + 
  # geom_jitter(width = 0.2, alpha = 0.7, size = 2, fill=Diagnosis) + 
  bkg + 
  # theme(legend.position = "none") + 
  labs(x = "Cohort", y = "Alpha Diversity (Shannon)") +
  geom_pwc(method = 'wilcox.test', 
           label = 'p.signif',  
           hide.ns = TRUE, 
           p.adjust.method = 'none',
           vjust = 0.5, # default 0, positive pushes it towards bar, negative further vertically away up; 1 is on the bar
           size = 0.8, # default 0.3
           label.size = 8, # default 3.88
           symnum.args=list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), 
                            symbols = c("****", "***", "**", "*", "ns"))
           ) +
  scale_fill_manual(values = col1)#  + 

fpb = paste(dir, filename_box.plot, sep = "")
print(p)
ggsave(fpb, plot = p, width = 15, height = 6, units = "in", dpi = 300)


# load in df
df_beta = read.table('/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs99/df_beta.tsv', 
                     sep = '\t', header = TRUE, row.names = 1, check.names = FALSE,
                     na.strings = "NA")

#  # remove PsA color since not yet in dataset
col1 <- col1[-1]

# variable of interest
a <- 'beta'

# create filenames
filename_box.plot = paste(a, "all_box.plot.pdf", sep = "_")  

# rename col
df_beta$psig = df_beta$`-log10p`

# rewrite order of factors
d.final <- df_beta[, c("cohort","Diagnosis","psig")]

# rename HC to Healthy
d.final[d.final=='HC'] <- 'Healthy'

# change dx order
# dx.order = c("Healthy", "RA", "PsO", "SLE", "SjD")
dx.order = dx.order[-1]
d.final$Diagnosis <- factor(d.final$Diagnosis, levels = dx.order)

# change cohort order 
# c.order = c("AMPAIM","Su2020","Wang2022","Luca2024",'Yu2022')
d.final$cohort <- factor(d.final$cohort, levels = c.order)

# Count number of groups per category
group_counts <- d.final %>%
  group_by(cohort) %>%
  mutate(n_group = n())

# Set manual dodge width based on group count
dodge_width <- 0.9

# create plot
p <- ggplot(d.final, aes(x = cohort, y = psig, fill = Diagnosis)) +
  # geom_bar(stat='identity',position = position_dodge()) +
  geom_col(
    position = position_dodge2(width = dodge_width, preserve = "single"),
    width = 0.8
    # width = ifelse(group_counts$n_group == 1, 0.9, 0.4) 
    # width = ifelse(group_counts$n_group == 1, 1, ifelse(group_counts$n_group == 2, 0.7, 0.4))  # Smaller width for single bars
  ) + 
  
  bkg + 
  # theme(legend.position = "none") + 
  labs(x = "Cohort", y = "-log10(p-value)") +
  # geom_pwc(method = 'wilcox.test', 
  #          label = 'p.signif',  
  #          hide.ns = TRUE, 
  #          p.adjust.method = 'none',
  #          vjust = 0.5, # default 0, positive pushes it towards bar, negative further vertically away up; 1 is on the bar
  #          size = 0.8, # default 0.3
  #          label.size = 8, # default 3.88
  #          symnum.args=list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), 
  #                           symbols = c("****", "***", "**", "*", "ns"))
  #          ) +
  scale_fill_manual(values = col1) + 
  geom_hline(yintercept=1.3, linetype='dotted', col = 'red')+
  annotate("text", x = "Valid7RA", y = 1.3, label = "p=0.05", vjust = -0.5)

fpb = paste(dir, filename_box.plot, sep = "")
print(p)
ggsave(fpb, plot = p, width = 15, height = 6, units = "in", dpi = 300)


df = subset(df_alpha, df_alpha$cohort == 'Valid4RA')
df = subset(df_alpha, df_alpha$cohort == 'Valid7RA') # 0.06
df = subset(df_alpha, df_alpha$cohort == 'Valid5PsO')
df = subset(df_alpha, df_alpha$cohort == 'Valid8PsO')
df = subset(df_alpha, df_alpha$cohort == 'Valid10PsO')
df = subset(df_alpha, df_alpha$cohort == 'Valid9PsD') # KW
df = subset(df_alpha, df_alpha$cohort == 'Valid3SjD')
df = subset(df_alpha, df_alpha$cohort == 'Valid2AxSpA')
df = subset(df_alpha, df_alpha$cohort == 'Valid6SLE')
df = subset(df_alpha, df_alpha$cohort == 'Valid1SLE')

# kruskal.test(shannon_entropy ~ Diagnosis, data=df)
wilcox.test(shannon_entropy ~ Diagnosis, data=df)


