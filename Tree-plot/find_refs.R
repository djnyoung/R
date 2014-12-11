setwd("C:/Users/DYoung/Dropbox/Research projects/PSME climate adaptation/Data-analysis")

# open data file
trees <- read.csv("Field data/2014 field data/adult_trees_merged.csv")
refs <- read.csv("GPS data/2014 refs/refs 2014.csv")
ref.ids <- toupper(refs$name)

#select only focal trees for ref extraction
trees.foc <- toupper(trees[toupper(trees$foc) == "X",])
tree.ids <- toupper(trees.foc$tree.id)


i <- 1 # keep track of current iteration

# for each tree that needs its reference resolved, find its id and the listed ref
unresolved.tree.id <- as.character(trees.foc$tree.id)
unresolved.tree.ref <- as.character(trees.foc$ref)

#check if ref is another tree, is in refs file or neither
ref.is.self <- (unresolved.tree.ref == unresolved.tree.id)
ref.is.tree <- (unresolved.tree.ref %in% tree.ids) & (!ref.is.self)
ref.is.ref <- (unresolved.tree.ref %in% ref.ids)
ref.missing <- !(ref.is.tree | ref.is.ref)

unresolved.df <- data.frame(unresolved.tree.id,unresolved.tree.ref,ref.is.self,ref.is.ref,ref.is.tree,ref.missing)

unresolved.df[unresolved.df$ref.missing==TRUE,]

