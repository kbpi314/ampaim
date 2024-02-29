#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

# PROJECT: hippGut
# NOTES: Input needs to be non-normalized raw reads.

library(ANCOMBC)
library(plyr)
library(phyloseq)
library(stringr)
library(dplyr)

# Import data
df_orig <- read.csv(args[1], sep = '\t', 
                    row.names = 1,
                    stringsAsFactors = FALSE,
                    check.names=FALSE)
df <- df_orig
metadata <- read.csv('/sc/arion/projects/clemej05a/jakleen/hippGut/data/hippGut_metadata_group.tsv',
                     sep='\t',
                     row.names = 1,
                     stringsAsFactors = FALSE)

# Create 'OTU' matrix (pathway abundances)
### Rows = pathways, Cols = patient IDs
otu_mat <- df[2:nrow(df), ]
otu_mat <- mutate_all(otu_mat, as.numeric)

# Create metadata objects
map_group <- data.frame(original = c('Control', 'OtherPsych', 'SZ'),
                        group2 = c('Control', 'Case', 'Case'))

if(args[2] == 'group2') {
    meta <- data.frame(group = mapvalues(metadata$group,
                                         from = map_group$original,
                                         to = map_group$group2),
                       row.names = rownames(metadata))
} else if (args[2] == 'group3') {
    meta <- metadata
} else if (args[2] == 'Ctrl_SZ') {
    meta <- metadata[metadata$group != 'OtherPsych', , drop = FALSE]
} else {
       print('Invalid group comparison (group2, group2, Ctrl_SZ)')
}

# Create phyloseq object
OTU <- otu_table(otu_mat, taxa_are_rows = TRUE)
META <- sample_data(meta)
physeq <- phyloseq(OTU, META)

output <- ancombc(phyloseq = physeq, formula = "group",    
                  p_adj_method = 'holm', zero_cut = 0.90, lib_cut = 0,
                  group = 'group', struc_zero = FALSE, neg_lb = FALSE,
                  tol = 1e-5, max_iter = 100, conserve = FALSE,
                  alpha = 0.05, global = TRUE)
# struc_zero = FALSE
# similar taxa expected bc same sample location

# Extract ANCOM-BC regression results
res <- output$res
diff_indices <- which(apply(res$diff_abn, 1, any))
print(paste0(length(diff_indices), ' differential features'))

comparison <- rep(args[2], length(diff_indices))
features <- names(diff_indices)
beta <- res$beta[diff_indices, , drop = FALSE]
se <- res$se[diff_indices, , drop = FALSE]
pval <- res$p_val[diff_indices, , drop = FALSE]
qval <- res$q_val[diff_indices, , drop = FALSE]

results <- data.frame(comparison, features,
                       beta, se, pval, qval)

colnames(results) <- c('comparison',
                       'feature',
                       paste0(rep(c('beta', 'se', 'pval', 'qval'),
                                  each = ncol(res$diff_abn)),
                              sep = '_',
                              c(colnames(beta),
                                colnames(se),
                                colnames(pval),
                                colnames(qval))))

print(features)

# Calculate (Wald) Confidence Intervals
# Still in progress..
# Not sure this works for 3 group comparisons
calculateCI <- function(b, e, n) {
    # b = beta matrix
    # e = Std Error matrix
    # p = adjusted alpha = alpha/n
    p <- 0.05 / n
    conf_lvl <- 1 - p
    z <- qnorm(c((1 - conf_lvl)/2, 1 - (1 - conf_lvl)/2))
  # conf_int <- b + z*e

    lower <- matrix(nrow(b), ncol(b))
    upper <- matrix(nrow(b), ncol(b))
    for (i in 1:ncol(b)) {
        lower[ ,i] <- b[ ,i] + e[i]*z[1]
        upper[ ,i] <- b[ ,i] + e[i]*z[2]
    }
    print(lower, upper)
    conf_int <- data.frame(CI_lower = lower, 
                           CI_upper = upper)
#     conf_int <- vector('list', length(e))
#     for (i in 1:length(e)) {
#         conf_int[[i]] <- b[i] + z*e[i]
#     }
#     output <- do.call(rbind, conf_int)
    return(conf_int)
}

# Save results to .tsv files
write.table(results, 
            file = paste0('ancombc_res_', 
                          args[2], '.tsv'),
            row.names = FALSE,
            sep = '\t')
save(results,
     file = paste0('ancombc_res_', args[2], '.RData'))
