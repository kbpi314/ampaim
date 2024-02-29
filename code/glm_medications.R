df = read.delim('/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/inputs/df_ss_med_R.tsv', sep = '\t')

out <- glm(shannon_entropy ~ hcq + mtx + hcq:mtx, data = df, family = gaussian)

summary(out)
