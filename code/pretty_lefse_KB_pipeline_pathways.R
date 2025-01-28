library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(stringr)

#######
bkg <-
  theme(axis.text.x = element_text(size = 24, color = "black")) +
  theme(axis.text.x = element_text(angle=0, vjust = 0.5, hjust = 1)) +
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
aimds = c('RA','PsO','PsA','SjD','SLE','NSS','A')
# jobs = c('jobs09','jobs10','jobs11','jobs12','jobs13','jobs14','jobs16')
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


gc = list(c(Healthy = col1[1], RA = col1[2]), # c(healthy = "#B3A98C", RA = "#E69F00"),
          c(Healthy = col1[1], PsO = col1[4]),
          c(Healthy = col1[1], PsA = col1[3]),
          c(Healthy = col1[1], SjD = col1[6]),
          c(Healthy = col1[1], SLE = col1[5]),
          c(Healthy = col1[1], NSS = col1[7]),
          c(Healthy = col1[1], A = "#E69F00")
)


### statistics ###
for (k in 1:length(aimds)){
  aimd = aimds[k]
  jobn = jobs[k]
  level = levels[[k]]
  group.colors = gc[[k]]

  
  ### MSQ138+141
  data <-read.table(paste0("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs06/UpSetR/",aimd,"_path_combined.tsv"), 
                    header = TRUE, sep = "\t", row.names = 1)
  
  # set names
  names(data) <- c("Taxa", "X", "Group", "pval",'qval','logp') 
  plot_data <- subset(data, !is.na(data$logp))
  
  # plot_data$Taxa <- as.character(taxa_strs)
  
  # exclude non family results
  # plot_data <- plot_data[grep("f__", plot_data$Taxa),]
  
  # exclude uncl
  # plot_data <- plot_data[grep("uncl", plot_data$Taxa, invert=T),]
  
  # take everything before trailing __
  # plot_data$Taxa <- sub('__*.', '', plot_data$Taxa)
  # plot_data$Taxa <- lapply(str_split(plot_data$Taxa, '__')[-1])
  # plot_data$Taxa <- sub("__.*", "", plot_data$Taxa)
  # plot_data$Taxa <- sub("__[^__]*$", '', plot_data$Taxa)
  
  # take everything after the last __
  plot_data$Taxa <- sub('.*__', '', plot_data$Taxa)
  
  # drop empty taxa strings
  plot_data <- plot_data[!(is.na(plot_data$Taxa) | plot_data$Taxa==""), ]
  
  # string specific substitutions
  plot_data$Taxa <- str_replace_all(plot_data$Taxa, 'Glycosphingolipid biosynthesis - lacto and neolacto series', 'Glycosphingolipid biosynthesis')
  plot_data$Taxa <- str_replace_all(plot_data$Taxa, 'Glycosphingolipid biosynthesis - lacto and neolacto series', 'Glycosphingolipid biosynthesis')
  
  
  # remove duplicates in Taxa
  plot_data <- plot_data[!duplicated(plot_data$Taxa),]
  
  # remove taxa on this specific list
  plot_data <- subset(plot_data, !(Taxa %in% c(
    'Cushing syndrome', 'Renal cell carcinoma', 'Immune system', 'Other types of O-glycan biosynthesis',
    'Fluid shear stress and atherosclerosis', 'Parkinson disease', 'Renin-angiotensin system',
    'Arrhythmogenic right ventricular cardiomyopathy','Human papillomavirus infection', 'Type II diabetes mellitus',
    'Gastric cancer', 'Retrograde endocannabinoid signaling', 'Nitrotoluene degradation', 'Alcoholism',
    'Amphetamine addiction', 'Cocaine addiction', 'Substance dependence', 'Serotonergic synapse', 'Dopaminergic synapse',
    'Signaling pathways regulating pluripotency of stem cells', 'Furfural degradation',
    'C5-Branched dibasic acid metabolism', 'Glucosinolate biosynthesis',
    'Phenylpropanoid biosynthesis','Proteasome'
    )
  ))
  
  # sort by LDA magnitude 
  plot_data <- plot_data[order(plot_data$logp,decreasing=TRUE),]
  
  # keep first 5 duplicates (top values) for each group
  plot_data <- plot_data %>%
               group_by(Group) %>%
               filter(row_number() <= 5)
  
  # flip directionality for nonhealthy
  plot_data[plot_data$Group != "Healthy",]$logp <- -1 * plot_data[plot_data$Group != "Healthy",]$logp
  
  # save results
  write.csv(plot_data,paste0("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/lefsepath_", aimd, '.csv'), row.names = FALSE)
  
  # save pdf
  pdf(paste0("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/lefsepath_barplot_pretty_",aimd,'.pdf'), width=10, height=6)

  p <- ggbarplot(plot_data, x="Taxa", y="logp", fill="Group", width= 1, color = "white", sort.val = "asc", sort.by.groups=TRUE) +  
    labs(x = "", y = "log(p)", fill="Group") + coord_flip() + 
    scale_fill_manual(values=group.colors) + bkg + theme(legend.position = "none")
  plot(p)
  dev.off()
}