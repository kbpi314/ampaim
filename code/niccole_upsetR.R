# install library, packages
library(UpSetR)

# example of list input (list of named vectors)
#listInput <- list(#
# RA = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/RA_lefse_sig.tsv', sep='\t', header=TRUE)$Taxa)[[1]]
data_melted_rg = read.delim('/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/inputs/data_melted_rg.tsv', sep='\t') 

data_melted_rg$accession <- as.character(data_melted_rg$accession)
data_melted_rg$value <- as.numeric(data_melted_rg$value)
data_melted_rg$value[is.na(data_melted_rg$value)] <- 0
str(data_melted_rg)

data_wide <- reshape(data_melted_rg, idvar = "accession", timevar = "Antibiotic_class", direction = "wide")
data_wide[is.na(data_wide)] <- 0

upset(data_wide, 
      #~ accession + value, 
      #sets = "Antibiotic_class", 
      order.by = "freq", 
      main.bar.color = "darkblue", 
      sets.bar.color = "lightblue")

# upset(data_melted_rg, sets = c("Methicillin", "Aminoglycoside", "Bactrim", "ESBL"), mb.ratio = c(0.55, 0.45), order.by = "freq")

# movies <- read.csv(system.file("extdata", "movies.csv", package = "UpSetR"), 
                   header = T, sep = ";")