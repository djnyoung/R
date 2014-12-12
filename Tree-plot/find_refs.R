setwd("C:/Users/DYoung/Dropbox/Research projects/PSME climate adaptation/Data-analysis")

# open data file
trees <- read.csv("Field data/2014 field data/adult_trees_merged.csv")
refs <- read.csv("GPS data/2014 refs/refs 2014 merged.csv")
ref.ids <- toupper(refs$name)

#select only focal trees for ref extraction
trees.foc <- trees[toupper(trees$foc) == "X",]
tree.ids <- toupper(trees.foc$tree.id)

### RUN A LOOP TO FIND ALL BASE LOCATION REFERENCE FOR ALL TREES, EVEN ONES THAT DEPEND ON A CHAIN OF REFERENCES

i <- 1 # keep track of current iteration
unresolved.df <- NULL # store the ref resolution for the trees in each iteration

# for each tree that needs its reference resolved, find its id and the listed ref
unresolved.tree.id <- as.character(trees.foc$tree.id)
unresolved.tree.ref <- as.character(trees.foc$ref)

for(i in 1:3) {

  #check if ref is self, another tree, is in refs file, or missing, and merge results into dataframe
  ref.is.self <- (unresolved.tree.ref == unresolved.tree.id)
  ref.is.tree <- (unresolved.tree.ref %in% tree.ids) & (!ref.is.self)
  ref.is.ref <- (unresolved.tree.ref %in% ref.ids)
  ref.missing <- !(ref.is.tree | ref.is.ref)
  unresolved.df[[i]] <- data.frame(unresolved.tree.id,unresolved.tree.ref,ref.is.self,ref.is.ref,ref.is.tree,ref.missing)
  
  #make a list of the trees missing refs
  missing.list <- unresolved.df[[i]][unresolved.df[[i]]$ref.missing==TRUE,]
  
  #prepare list of the next unresolved trees (trees for which ref is another tree)--these are the trees REFERENCED by the the current trees
  unresolved.tree.id <- as.character(unresolved.df[[i]][unresolved.df[[i]]$ref.is.tree==TRUE,]$unresolved.tree.ref)
  
  #now need to look up the refs of those trees
  unresolved.tree.ref <- trees.foc[match(unresolved.tree.id,trees.foc$tree.id),]$ref
  
}