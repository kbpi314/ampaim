

### Load libraries ###
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(rstatix)
library(tidyr)
library(RColorBrewer)

# background theme
bkg <- theme_bw() +
  theme(axis.text.x = element_text(size = 12, color = "black")) +
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
vars1 = c('Phascolarctobacterium', 'Phascolarctobacterium', 'Prevotella_copri', 'Prevotella_copri',
          'Phascolarctobacterium', 'Phascolarctobacterium', 'Phascolarctobacterium', 'Phascolarctobacterium', 'Phascolarctobacterium',
          'Phascolarctobacterium', 'Phascolarctobacterium',
          'Mannose_Biosynthesis',
          'Phascolarctobacterium', 'Phascolarctobacterium', 'Coprobacter',
          'Phascolarctobacterium', 'Coprobacter',
          'logPhascolarctobacterium',
          # log phas against log CRP in PsA, log phas against butanoate in RA, and aggregate without lines
          'logPhascolarctobacterium','logPhascolarctobacterium',
          'logPhascolarctobacterium','logPhascolarctobacterium',
          'logCoprobacter', 'logCoprobacter'
          )
vars2 = c('CRP', 'ESR', 'SJC', 'TJC',
          'CRP', 'CRP', 'CRP', 'CRP', 'CRP',
          'CRP', 'CRP',
          'ESR',
          'CRP','Butanoate_Metabolism','CRP',
          'logCRP','logCRP',
          'Butanoate_Metabolism',
          'logCRP','Butanoate_Metabolism',
          'logCRP','Butanoate_Metabolism',
          'logCRP','logMannose_Biosynthesis'
          
          )

groups = c('Diagnosis', 'Diagnosis', 'Diagnosis', 'Diagnosis',
           'Diagnosis', 'Diagnosis', 'Diagnosis', 'Diagnosis', 'Diagnosis',
           'Diagnosis', 'Diagnosis',
           'Diagnosis',
           'Diagnosis', 'Diagnosis','Diagnosis',
           'Diagnosis','Diagnosis',
           'Diagnosis',
           'Diagnosis','Diagnosis',
           'DiagnosisAIMD','DiagnosisAIMD',
           'DiagnosisAIMD', 'DiagnosisAIMD'
           )
subsample = list(c("PsA"), c("PsA"), c("PsA","RA"), c("PsA","RA"),
                 c("RA"),c("PsO"),c("SLE"),c("NSS"),c("SjD"),
                 c("RA","PsO","PsA"), c("PsO"),
                 c("PsA"),
                 c("RA","PsO","PsA"),  c("RA","PsO","PsA","Healthy","SjD","SLE","NSS"),  c("RA","PsO","PsA"), # middle one is butanoate
                 c("RA","PsO","PsA"),  c("RA","PsO","PsA"),
                 c("RA","PsO","PsA","SjD","SLE","NSS"), # log phas against butanoate
                 c('PsA'),c('RA'),
                 c("RA","PsO","PsA","SjD","SLE","NSS"),c("RA","PsO","PsA","SjD","SLE","NSS"),
                 c("RA","PsO","PsA","SjD","SLE","NSS"), c("AIMD","Healthy")
)

tags = c('phasA_PsA','x', 'x', 'x', 
         'phasA_RA', 'phasA_PsO', 'phasA_SLE', 'phasA_NSS', 'phasA_SjD',
         'phasA_RPP','phasA_PsO',
         'Mannose_PsA', 
         'AvH_Phas_CRP','AvH_Phas_Butanoate','AvH_Copro_CRP',
         'AvH_Phas_logCRP', 'AvH_Copro_logCRP',
         'AvH_logPhas_Butanoate',
         'logPhas_logCRP_PsA', 'logPhas_Butanoate_RA',
         'AvH_logPhas_logCRP', 'AvH_logPhas_Butanoate',
         'AvH_logCopro_logCRP', 'AvH_logCopro_logMannose')



# choose colors, corresponding to c("Healthy", "RA", "PsA", "PsO", "SLE", "SS", "NSS")
col1 <- colorRampPalette(brewer.pal(8, "Set2"))(7)
col1 <- colorRampPalette(brewer.pal(9, "Paired"))(7)
# "black"   "#4F9AA6" "#5DB54B" "#FB9A99" "#EB5037" "#FE9425" "#CAB2D6"
col1[1] <- "white"

colors = list(c("PsA" = col1[3]), c("PsA" = col1[3]), c("PsA" = col1[3], "RA" = col1[2]), c("PsA" = col1[3], "RA" = col1[2]),
              c("RA" = col1[1]), c("PsO" = col1[4]), c("SLE" = col1[5]), c("NSS" = col1[7]), c("SjD" = col1[6]),
              c("PsA" = col1[3], "RA" = col1[2], "PsO" = col1[4]), c("PsA" = col1[4]),
              c("PsA" = col1[4]),
              c("PsA" = col1[3], "RA" = col1[2], "PsO" = col1[4], "SLE" = col1[5], "SjD" = col1[6], "NSS" = col1[7]), 
              c("PsA" = col1[3], "RA" = col1[2], "PsO" = col1[4], "SLE" = col1[5], "SjD" = col1[6], "NSS" = col1[7]),
              c("PsA" = col1[3], "RA" = col1[2], "PsO" = col1[4], "SLE" = col1[5], "SjD" = col1[6], "NSS" = col1[7]), 
              c("PsA" = col1[3], "RA" = col1[2], "PsO" = col1[4], "SLE" = col1[5], "SjD" = col1[6], "NSS" = col1[7]),
              c("PsA" = col1[3], "RA" = col1[2], "PsO" = col1[4], "SLE" = col1[5], "SjD" = col1[6], "NSS" = col1[7]))

color_map = c("PsA" = col1[3], "RA" = col1[2], "PsO" = col1[4], "SLE" = col1[5], "SjD" = col1[6], "NSS" = col1[7])

for (i in seq(1, length(vars1))) {
  # read data
  df = read.table(paste0('/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/inputs/df_otu_meta_fxn_logt.csv'), 
  # df = read.table(paste0('/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/inputs/df_otu_meta_logt.csv'), 
                  sep = ',', header = TRUE,  row.names = 1, check.names = FALSE,
                  na.strings = "NA")
  df <- df[df$Diagnosis %in% subsample[[i]], ]
  df <- df[df$Diagnosis %in% c('Healthy','PsA','PsO','RA'), ]
  
  # df <- df[complete.cases(df[ , c(1,6)]),]
  cc = complete.cases(df[c(vars1[i],vars2[i])])
  df <- df[cc,]

  df$Diagnosis <- as.factor(df$Diagnosis)
  df$DiagnosisAIMD <- as.factor(df$DiagnosisAIMD)

  # create filenames
  filename_box.plot = paste0(tags[i], '_', vars1[i],'_',vars2[i], "_scatter.plot_KL2.pdf")
  
  # create model
  if (nrow(df) > 0){
    print(tags[i])
    plm <- lm(df[,vars2[i]] ~ df[,vars1[i]], data=df)
    print(summary(plm))
  }
  
  # create plot
  p <- ggplot(df, aes_string(x = vars1[i], y = vars2[i], fill = groups[[i]])) +
    geom_point(pch=21, size=3) +
    bkg + 
    scale_fill_manual(values = color_map)  +
    scale_color_manual(values = color_map) + 
    labs(x = gsub("_", " ", vars1[i]), y = gsub("_", " ", vars2[i])) +
    coord_cartesian(clip = "off") + 
    geom_smooth(method = "lm", span=0.1, se = FALSE, aes_string(color=groups[[i]],  fill=groups[[i]]),na.rm=TRUE)
    # geom_smooth(method = "lm", mapping=aes(y=predict(lm,df)))
  
  fpb = paste(dir, filename_box.plot, sep = "")
  print(p)
  ggsave(fpb, plot = p, width = 6, height = 6, units = "in", dpi = 300)
}