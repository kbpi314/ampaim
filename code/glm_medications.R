df = read.delim('/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/inputs/df_med.tsv', sep = '\t')

out <- glm(shannon_entropy ~ HCQ + MTX, data = df, family = gaussian)

summary(out)
