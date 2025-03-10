###
# MaAsLin2 for AMPAIM
###

# load libraries
library(Maaslin2)
library(nnet)
library(epiDisplay)

# set path
setwd('/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/')

# comparison of glms
df <- read.table(paste0('inputs/df_glm_ds.tsv'), sep='\t', header=T, row.names=1)

# I. Two cont variable case:
# (1) summary(lm(formula = Prevotella_copri ~ shannon_entropy, data = df))
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)      0.063618   0.041303    1.54    0.126
# shannon_entropy -0.008445   0.008441   -1.00    0.319
#
# (2) summary(lm(formula = shannon_entropy ~ Prevotella_copri, data = df))
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       4.86594    0.06291   77.35   <2e-16 ***
# Prevotella_copri -0.90474    0.90431   -1.00    0.319    

# II. One cont one binary
# (1) summary(lm(shannon_entropy ~ Medication_Status2,data=df))
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                   5.1073     0.2274  22.461   <2e-16 ***
# Medication_Status2Untreated  -0.2810     0.2356  -1.193    0.235 
# (2) summary(glm(Medication_Status3 ~ shannon_entropy, data=df, family = binomial))
# Estimate Std. Error z value Pr(>|z|)  
# (Intercept)      -5.8066     2.7676  -2.098   0.0359 *
# shannon_entropy   0.6423     0.5397   1.190   0.2340  

# known result; alpha against dx
adiv_dx <- lm(shannon_entropy ~ Diagnosis, data=df)
summary(adiv_dx)

#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)    5.6459     0.2716  20.787  < 2e-16 ***
#   DiagnosisNSS  -0.7541     0.3326  -2.267  0.02511 *  
#   DiagnosisPsA  -0.8820     0.2993  -2.947  0.00383 ** 
#   DiagnosisPsO  -1.0263     0.2993  -3.429  0.00082 ***
#   DiagnosisRA   -0.7138     0.2899  -2.462  0.01517 *  
#   DiagnosisSjD  -0.8180     0.3436  -2.381  0.01877 *  
#   DiagnosisSLE  -0.8615     0.4029  -2.139  0.03442 *  

# see if it holds controlling for medication status
adiv_dxMed <- lm(shannon_entropy ~ Diagnosis + Medication_Status2, data=df)
summary(adiv_dxMed)

# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                   5.9893     0.3687  16.242  < 2e-16 ***
# DiagnosisNSS                 -0.8685     0.3418  -2.541 0.012292 *  
# DiagnosisPsA                 -0.8820     0.2982  -2.957 0.003717 ** 
# DiagnosisPsO                 -1.0386     0.2984  -3.481 0.000691 ***
# DiagnosisRA                  -0.7218     0.2890  -2.498 0.013803 *  
# DiagnosisSjD                 -0.8867     0.3460  -2.563 0.011581 *  
# DiagnosisSLE                 -0.9302     0.4046  -2.299 0.023160 *  
# Medication_Status2Untreated  -0.3434     0.2504  -1.371 0.172802   

# PsA v H only
df <- read.table(paste0('inputs/df_glm_ds_RAPsAvH.tsv'), sep='\t', header=T, row.names=1)
adiv_dx <- lm(shannon_entropy ~ Dx, data=df)
summary(adiv_dx)
 
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   5.6459     0.2513  22.467  < 2e-16 ***
# Dx           -0.7802     0.2617  -2.981  0.00387 ** 
  
dx_adiv <- glm(Dx ~ shannon_entropy, data=df, family=binomial)
summary(dx_adiv)
# Estimate Std. Error z value Pr(>|z|)   
# (Intercept)      14.1416     4.7844   2.956  0.00312 **
# shannon_entropy  -2.2168     0.8631  -2.568  0.01022 * 

# now controlling for medication
adiv_dxMed <- lm(shannon_entropy ~ Dx + Medication_Status2, data=df)
summary(adiv_dxMed)
# Estimate Std. Error z value Pr(>|z|) 
# (Intercept)                  5.68990    0.67341   8.449 1.83e-12 ***
# Dx                          -0.78077    0.26360  -2.962  0.00411 ** 
# Medication_Status2Untreated -0.04401    0.62408  -0.071  0.94397    

dx_adivMed <- glm(Dx ~ shannon_entropy + Medication_Status2, data=df, family=binomial)
summary(dx_adivMed)
# Estimate Std. Error z value Pr(>|z|) 
# (Intercept)                   27.3655  2399.5485   0.011   0.9909  
# shannon_entropy               -2.1999     0.8624  -2.551   0.0107 *
# Medication_Status2Untreated  -13.3252  2399.5448  -0.006   0.9956 

# dx_adivMed <- multinom(Diagnosis ~ shannon_entropy + Medication_Status2, data=df)
# summary(dx_adivMed)
# epiDisplay::mlogit.display(dx_adivMed, decimal=3, alpha=0.05)

###
# Tutorial
###



input_data <- system.file(
  'extdata','HMP2_taxonomy.tsv', package="Maaslin2")
input_metadata <-system.file(
  'extdata','HMP2_metadata.tsv', package="Maaslin2")


df_input_data = read.table(file = input_data, header = TRUE, sep = "\t",
                           row.names = 1,
                           stringsAsFactors = FALSE)
df_input_metadata = read.table(file = input_metadata, header = TRUE, sep = "\t",
                               row.names = 1,
                               stringsAsFactors = FALSE)


#df_path <- read.table('inputs/df_path2.tsv', sep='\t', header=T, row.names=1)
#df_taxa <- read.table('inputs/df_taxa2.tsv', sep='\t', header=T, row.names=1)
df_meta <- read.table('inputs/df_meta2.tsv', sep='\t', header=T, row.names=1)

for (d in c('taxa','path')) {
  # load df
  df_ <- read.table(paste0('inputs/df_',d,'2.tsv'), header=T, row.names=1)

  fit_data2 = Maaslin2(
    input_data = df_,
    input_metadata = df_meta,
    output = paste0('outputs/jobs39/',d,'/'),
    fixed_effects = c("Diagnosis", "Medication_Status2"),# ,'num_severity'),
    reference='Diagnosis,Healthy;Medication_Status2,Untreated',
    correction='none',
    min_prevalence = 0.1,
    min_abundance = 0.0001)
}

