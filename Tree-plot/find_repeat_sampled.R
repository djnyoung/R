#look up 2013 trees

setwd("C:/Users/DYoung/Dropbox/Research projects/PSME climate adaptation/Data-analysis")

trees.2014 <- read.csv("Field data/2014 field data/adult_trees_merged.csv")
trees.2014 <- trees.2014[toupper(trees.2014$foc) == "X",]
trees.2013 <- read.csv("Field data/2013 Field data/trees_loc.csv")

repeat.sampled <- trees.2013[which(toupper(trees.2013$tree.id) %in% trees.2014$former.id),]

write.csv(repeat.sampled,"repeat_sampled.csv")