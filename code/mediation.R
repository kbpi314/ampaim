# install.packages("mediation")
library(mediation)

df = read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs01/mediation.tsv', sep='\t',header=TRUE)

# regress individually
results <- mediate(model.M, model.Y, treat='X', mediator='M',
                   boot=TRUE, sims=500)
summary(results)