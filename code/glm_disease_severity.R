# import libraries
library(MASS)

# import df
df = read.delim('/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/inputs/df_glm_ds.tsv',sep='\t',row.names=1)

# fill na
df[df==""]<-NA

# get n=132 samples
print(nrow(df))

# drop na in severity (n=92 remain)
df = df[!is.na(df$severity),]
print(nrow(df))

# filter on disease state only
df_aimd = df[df$Diagnosis != 'Healthy',]
print(nrow(df_aimd)) # 86 remain

# convert to factor
df$severity = factor(df$severity, levels=c('none','remission','mild','moderate','severe'))

# What is the effect of medication on diversity controlling for dx? and the effect of dx on diversity controlling for medication?
m <- glm(shannon_entropy ~ Medication_Status2 + Diagnosis, data=df_aimd)
summary(m)

# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                  5.28022    0.27677  19.078   <2e-16 ***
# Medication_Status2Untreated -0.58260    0.31188  -1.868   0.0655 .  
# DiagnosisPsA                 0.30580    0.26978   1.134   0.2604    
# DiagnosisPsO                 0.09922    0.24739   0.401   0.6894    
# DiagnosisRA                  0.24031    0.23452   1.025   0.3086    
# DiagnosisSjD                 0.40167    0.48610   0.826   0.4111    
# DiagnosisSLE                -1.35246    0.69068  -1.958   0.0537 .  

# conclusions
# (1) Medication has a negative effect on adiv, controlling for dx
# (2) SLE has the highest reduction in diversity (controlling for medication)

# What is the effect of disease severity on diversity controlling for dx and medication?
m <- glm(shannon_entropy ~ severity + Diagnosis + Medication_Status2, data=df_aimd)
summary(m)

# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                  4.89309    0.33213  14.732   <2e-16 ***
# severitymild                 0.82185    0.32116   2.559   0.0125 *  
# severitymoderate             0.44312    0.29225   1.516   0.1336    
# severitysevere               0.66438    0.31286   2.124   0.0370 *  
# DiagnosisPsA                -0.01776    0.31249  -0.057   0.9548    
# DiagnosisPsO                -0.28844    0.28068  -1.028   0.3074    
# DiagnosisRA                 -0.06424    0.27881  -0.230   0.8184    
# DiagnosisSjD                 0.29654    0.48792   0.608   0.5452    
# DiagnosisSLE                -1.78718    0.69346  -2.577   0.0119 *  
# Medication_Status2Untreated -0.48431    0.30782  -1.573   0.1198    

# conclusion
# (1) adiv increases disease severity compared to remission with mild > severe > moderate
# note that encoding severity as 'ordinal likert-esque'/ continuous shows no relationship b/w severity and div

m <- glm(shannon_entropy ~ num_severity + Diagnosis + Medication_Status2, data=df_aimd)
summary(m)
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                  5.08347    0.35428  14.349   <2e-16 ***
# num_severity                 0.08154    0.09147   0.891   0.3754    
# DiagnosisPsA                 0.16084    0.31530   0.510   0.6114    
# DiagnosisPsO                 0.04822    0.25423   0.190   0.8501    
# DiagnosisRA                  0.10447    0.27993   0.373   0.7100    
# DiagnosisSjD                 0.32855    0.49359   0.666   0.5076    
# DiagnosisSLE                -1.31879    0.69261  -1.904   0.0606 .  
# Medication_Status2Untreated -0.53209    0.31739  -1.676   0.0977 .

m <- glm(shannon_entropy ~ num_severity + Medication_Status2, data=df_aimd)
summary(m)

# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                  4.84947    0.30046  16.140   <2e-16 ***
# num_severity                 0.12021    0.07088   1.696   0.0936 .  
# Medication_Status2Untreated -0.32286    0.25153  -1.284   0.2029    


# effect of adiv on severity controlling for medication
m <- glm(num_severity ~ shannon_entropy + Medication_Status2, data = df_aimd, family=quasipoisson) # for count data
summary(m)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                  0.45927    0.31665   1.450   0.1507  
# shannon_entropy              0.09362    0.05535   1.691   0.0945 .
# Medication_Status2Untreated  0.19625    0.13696   1.433   0.1556  

# conclusion
# (1) higher div inc disease severity controlling for med usage
# (2) lack of treatment increases severity controlling for adiv

m <- glm(num_severity ~ Phascolarctobacterium + Medication_Status2 + Diagnosis, data = df_aimd, family=quasipoisson) # for count data
summary(m)

# Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                   0.8820     0.1284   6.870 1.39e-09 ***
#   Phascolarctobacterium        -4.0047     3.1230  -1.282   0.2035    
#   Medication_Status2Untreated  -0.2547     0.1437  -1.773   0.0801 .  
#   DiagnosisPsA                  0.6848     0.1271   5.386 7.40e-07 ***
#   DiagnosisPsO                  0.2663     0.1242   2.144   0.0351 *  
#   DiagnosisRA                   0.6254     0.1174   5.329 9.31e-07 ***
#   DiagnosisSjD                  0.3460     0.2104   1.645   0.1041    
#   DiagnosisSLE                 -0.1889     0.3480  -0.543   0.5889    

m <- glm(Phascolarctobacterium ~ num_severity + Medication_Status2 + Diagnosis, data = df_aimd, family=gaussian) 
summary(m)

# DIY maaslin2
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                  0.009779   0.005465   1.789   0.0775 .
# num_severity                -0.001878   0.001411  -1.331   0.1870  
# Medication_Status2Untreated  0.001518   0.004896   0.310   0.7573  
# DiagnosisPsA                 0.005741   0.004864   1.180   0.2415  
# DiagnosisPsO                -0.003502   0.003922  -0.893   0.3746  
# DiagnosisRA                 -0.001825   0.004318  -0.423   0.6738  
# DiagnosisSjD                -0.002646   0.007614  -0.347   0.7292  
# DiagnosisSLE                -0.006022   0.010684  -0.564   0.5747  

