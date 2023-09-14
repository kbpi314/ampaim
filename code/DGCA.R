# import libraries
library(DGCA)

# import df, variables as columns (all of them)



# import complete df
df_map = read.delim('/Users/KevinBu/Desktop/clemente_lab/Projects/hippgut/outputs/jobs17/df_complete_L5.txt', 
                    sep='\t',
                    header=TRUE,
                    row.names=1)

# for predicting patients, medications and illness are too highly correlated to be useful
df_map <- subset(df_map, select = -c(CurrentMedication, Comorbidities, Disease_type))

# convert from factor to numeric for select columns
df_map$Height_m = as.numeric(as.character(df_map$Height_m))
df_map$Weight_kg = as.numeric(as.character(df_map$Weight_kg))
df_map$BMI = as.numeric(as.character(df_map$BMI))
df_map$Age = as.numeric(as.character(df_map$Age))

# other factors that incur probabilities of 0, lack data or don't seem relevant
df_map <- subset(df_map, select = -c(Specialized_Diet, Recent_antibiotic_usage, 
                                     CNS_Medication_Yes_No, Oppioid_Addiction_Medication_Yes_No, 
                                     Antibiotics_Yes_No, Antiipertension_Yes_No, Stomach_Medication_Yes_No, 
                                     Laxative_Yes_No, Hormones_Yes_No, NSAID_Yes_No, Local_Anestetic_Yes_No, 
                                     Vitamins_Yes_No, Antihistamine_Yes_No, H6_Positivity))


# split on sample type
df_stool <- df_map[df_map$Sample_type == 'Stool', ]
df_stool <- subset(df_stool, select = -c(Sample_type))
df_oral <- df_map[df_map$Sample_type == 'OralSwab', ]
df_oral <- subset(df_oral, select = -c(Sample_type))

# DGCA TIME
library(DGCA)
# load in OTU tables corresponding to different groups
df_complete = read.delim('/Users/KevinBu/Desktop/clemente_lab/Projects/hippgut/outputs/jobs17/df_complete_div.txt', 
                         #df_complete = read.delim('/Users/KevinBu/Desktop/clemente_lab/hippgut/outputs/jobs16/df_dgca.txt', 
                         sep = '\t', header=TRUE,
                         row.names=1)

# oral
# drop samples
df_complete_oral = df_complete[df_complete$Sample_type == 'OralSwab',]
# df_complete_oral = subset(df_complete_oral, select = -c(Sample_type, opc1, opc2))

# obtain sample names
s_names = df_complete_oral$X
var_names = colnames(df_complete_oral)
var_names = var_names[2:length(var_names)]

# create design matrix
patient = ifelse(df_complete_oral$Type == 'Patient',1,0)
control = ifelse(df_complete_oral$Type == 'Control',1,0)

df_complete_oral = subset(df_complete_oral, select = -c(Type))
df_complete_oral = t(df_complete_oral)
df_complete_oral = data.frame(df_complete_oral)
v_names = rownames(df_complete_oral)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
df_complete_oral <- data.frame(lapply(df_complete_oral, as.numeric.factor))

# create design matrix 
desmat = data.frame(patient = as.numeric(patient), control = as.numeric(control))
desmat = data.matrix(desmat)

v_ids = rownames(df_complete_oral)

# create dgca result
# obtain all correlations to a specific bacteria
x = data.frame()
for (v in v_ids){
  dgca = ddcorAll(inputMat = df_complete_oral, design = desmat, corrType = 'spearman', compare = c('patient', 'control'), 
                  adjust = 'perm', nPerm = 10, splitSet = v)
  x = rbind(x, dgca)
}

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

y = completeFun(x, c('Gene1','Gene2'))
# y <- y[1:1000,]
y = data.frame(lapply(y, as.numeric))
for (i in 1:nrow(y))
{
  y[i,1:2] = sort(y[i,1:2])
}
y = y[!duplicated(y[,1:2]),]

y$'fdr' = p.adjust(y$pValDiff, method = 'fdr')
z = y
z = z[order(z$fdr),]
sig_z = subset(z, fdr <= 0.05)

library(ggplot2)
dir.create('/Users/KevinBu/Desktop/clemente_lab/Projects/hippgut/outputs/jobs17/oral_plots_fdr/')
for (i in 1:nrow(sig_z)){
  plotCors(inputMat = df_complete_oral, design = desmat, compare = c('patient', 'control'),
           geneA = sig_z[i,]$Gene1, 
           geneB = sig_z[i,]$Gene2, 
           xlab = var_names[as.numeric(sig_z2[i,]$Gene1)],
           ylab = var_names[as.numeric(sig_z2[i,]$Gene2)])
  #xlab = sig_z[i,]$Gene1,
  # ylab = sig_z[i,]$Gene2)
  #ggsave(paste('/Users/KevinBu/Desktop/clemente_lab/hippgut/outputs/jobs16/oral_plots_fdr/',sig_z[i,]$Gene1,'_',sig_z[i,]$Gene2,'.pdf', sep = ''))
  ggsave(paste('/Users/KevinBu/Desktop/clemente_lab/Projects/hippgut/outputs/jobs17/oral_plots_fdr/',v_names[as.numeric(sig_z2[i,]$Gene1)],'_',v_names[as.numeric(sig_z2[i,]$Gene2)],'.pdf', sep = ''))
}
