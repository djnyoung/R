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
finished <- FALSE
unresolved.df <- NULL # store the ref resolution for the trees in each iteration

# for each tree that needs its reference resolved, find its id and the listed ref
unresolved.tree.id <- as.character(trees.foc$tree.id)
unresolved.tree.ref <- as.character(trees.foc$ref)

while(finished == FALSE) {

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
  
  if(length(unresolved.tree.id)==0) {finished<- TRUE} else {i <- i + 1}
  
}

#get all refs out in order
sorted.trees <- do.call("rbind",rev(unresolved.df))
sorted.trees.unique <- unique(sorted.trees) # saves the first occurrence of each tree, so now they are sorted such that earlier ones depend on later ones
trees.loclookup <- merge(sorted.trees.unique,trees.foc, by.x="unresolved.tree.id",by.y="tree.id")
names(trees.loclookup)[1:2] <- c("tree.id","tree.ref")

##### COMPUTE LOCATION INFORMATION #####

### first define convenience functions ###

# function for getting a given ref's location
refloc <- function(ref.name) {
  x <- refs[which(refs$name %in% ref.name),]$x
  y <- refs[which(refs$name %in% ref.name),]$y
  as.data.frame(cbind(x,y))
}

# function for getting a given tree's location; requires first assigning trees their x and y
treeloc <- function(tree.name) {
  x <- trees[which(trees$tree.id %in% tree.name),]$x
  y <- trees[which(trees$tree.id %in% tree.name),]$y
  as.data.frame(cbind(x,y))
  
}

# function for returning points based on offset angle and distance
offsetx <- function(x,bearing,distance,back.brg) {   #coordinates of reference, angle in true degrees shooting TO the reference, distance in meters
  if(back.brg == TRUE) { bearing <- (bearing-180) %% 360 } #convert back-bearing to forward bearing
  x + sin(bearing*pi/180)*distance
}

offsety <- function(y,bearing,distance,back.brg) {   #coordinates of reference, angle in true degrees shooting TO the reference, distance in meters
  if(back.brg == TRUE) { bearing <- (bearing-180) %% 360 } #convert back-bearing to forward bearing
  y + cos(bearing*pi/180)*distance
}

### end define convenience functions ###

### begin script for looping through trees one-by-one and looking up their location information (either from refs or as offsed from previous tree)

#compute true bearing based on magnetic north bearing and declination
trees$bearing.true <- (trees$bearing + 13.5) %% 360
refs$bearing.true <- (refs$bearing + 13.5) %% 360

# fill in coordinates for refs that reference other refs
unk_refs <- refs[which(is.na(refs$x)),]
unk_refs$x <- offsetx(refloc(unk_refs$ref)$x,unk_refs$bearing.true,unk_refs$dist)
unk_refs$y <- offsety(refloc(unk_refs$ref)$y,unk_refs$bearing.true,unk_refs$dist)
refs[which(is.na(refs$x)),] <- unk_refs


## output all refs
write.csv(refs,"refs.csv")



# fill in coordinates for trees
# must do this in a for loop because some later trees reference earlier trees

trees$x <- rep(NA,nrow(trees))
trees$y <- rep(NA,nrow(trees))

for(i in 1:nrow(trees)) {
  
  if(trees$ref.type[i] == 'ref') {
    
    if (nrow(refloc(trees$ref[i]))==0) {
      print("Reference requested not found: ")
      print(trees$ref[i])
    } else if(is.na(refloc(trees$ref[i])$x)) {
      print("Reference requested does not have coordinates: ")
      print(trees$ref[i])
    } else if(is.na(trees$bearing.true[i])) { # there are no coordinates, so set the tree equal to the ref coords
      trees[i,]$x <- refloc(trees$ref[i])$x
      trees[i,]$y <- refloc(trees$ref[i])$y
    } else {      
      trees[i,]$x <- offsetx(refloc(trees$ref[i])$x,trees$bearing.true[i],trees$dist[i])
      trees[i,]$y <- offsety(refloc(trees$ref[i])$y,trees$bearing.true[i],trees$dist[i])
    }
  } else { #the reference is another tree
    if(nrow(treeloc(trees$ref[i]))==0) {
      print("Tree requested as reference not found: ")
      print(trees$ref[i])
    } else if (is.na(treeloc(trees$ref[i])$x)) {
      print("Tree requested as reference does not have coordinates: ")
      print(trees$ref[i])
    } else {
      trees[i,]$x <- offsetx(treeloc(trees$ref[i])$x,trees$bearing.true[i],trees$dist[i])
      trees[i,]$y <- offsety(treeloc(trees$ref[i])$y,trees$bearing.true[i],trees$dist[i])
    }
    
  }
  
}

write.csv(trees,"trees_loc.csv")


