setwd("C:/Users/DYoung/Dropbox/Research projects/PSME climate adaptation/Data-analysis")

# open data file
trees <- read.csv("Field data/2014 field data/adult_trees_merged.csv")
refs <- read.csv("GPS data/2014 refs/refs 2014 merged albers.csv")
ref.ids <- toupper(refs$name)
refs$temp.dummy.coords <- sapply(as.character(refs$temp.dummy.coords),nzchar)

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
trees.loclookup <- merge(sorted.trees.unique,trees.foc, by.x="unresolved.tree.id",by.y="tree.id",sort=FALSE)
names(trees.loclookup)[1:2] <- c("tree.id","tree.ref")

##### COMPUTE LOCATION INFORMATION #####

### first define convenience functions ###

# function for getting a given ref's location
refloc <- function(ref.name) {
  x <- refs[which(toupper(refs$name) %in% toupper(ref.name)),]$x
  y <- refs[which(toupper(refs$name) %in% toupper(ref.name)),]$y
  dummy.coords <- refs[which(toupper(refs$name) %in% toupper(ref.name)),]$temp.dummy.coords
  as.data.frame(cbind(x,y,dummy.coords))
}

# function for getting a given tree's location; requires first assigning trees their x and y
treeloc <- function(tree.name) {
  x <- trees.loclookup[which(toupper(trees.loclookup$tree.id) %in% toupper(tree.name)),]$x
  y <- trees.loclookup[which(toupper(trees.loclookup$tree.id) %in% toupper(tree.name)),]$y
  dummy.coords <- trees.loclookup[which(toupper(trees.loclookup$tree.id) %in% toupper(tree.name)),]$dummy.coords
  as.data.frame(cbind(x,y,dummy.coords))
}

# function for returning points based on offset angle and distance
offsetx <- function(x,bearing,distance) {   #coordinates of reference, angle in true degrees shooting TO the reference, distance in meters
  bearing <- (bearing + 180) %% 360
  x + sin(bearing*pi/180)*distance
}

offsety <- function(y,bearing,distance) {   #coordinates of reference, angle in true degrees shooting TO the reference, distance in meters
  bearing <- (bearing + 180) %% 360
  y + cos(bearing*pi/180)*distance
}

### end define convenience functions ###

### begin script for looping through trees one-by-one and looking up their location information (either from refs or as offset from previous tree; for trees that are at the exact location of a reference, distance is set to 0 so there is no offset)

## first do some prep

# set bearing and distance to numeric; set blanks to 0
trees.loclookup$brg.to.ref <- as.numeric(as.character(trees.loclookup$brg.to.ref))
trees.loclookup[is.na(trees.loclookup$brg.to.ref),]$brg.to.ref <- rep(0,sum(is.na(trees.loclookup$brg.to.ref)))
trees.loclookup[is.na(trees.loclookup$dist.to.ref),]$dist.to.ref <- rep(0,sum(is.na(trees.loclookup$dist.to.ref)))

# turn backbearing column into boolean
trees.loclookup$backbrg <- as.character(trees.loclookup$backbrg)
trees.loclookup$backbrg <- sapply(trees.loclookup$backbrg,nzchar)

#compute true bearing based on magnetic north bearing and declination; reverse backbearings when necessary
trees.loclookup$brg.to.ref.true <- (trees.loclookup$brg.to.ref + 13.5) %% 360
trees.loclookup[trees.loclookup$backbrg==TRUE,]$brg.to.ref.true <- (trees.loclookup[trees.loclookup$backbrg==TRUE,]$brg.to.ref.true + 180) %% 360

# fill in coordinates for trees
# must do this in a for loop because some later trees reference earlier trees

trees.loclookup$x <- rep(NA,nrow(trees.loclookup))
trees.loclookup$y <- rep(NA,nrow(trees.loclookup))
trees.loclookup$dummy.coords <- rep(NA,nrow(trees.loclookup))

# export the tree.loclookup table for debugging purposes
#write.csv(trees.loclookup,"trees_loclookup_preprocess.csv")

for(i in 1:nrow(trees.loclookup)) {
  
  if(trees.loclookup$ref.is.ref[i]) {  # the reference is a reference (in refs spreadsheet)
    
    if (nrow(refloc(trees.loclookup$ref[i]))==0) {
      print("Reference requested not found: ")
      print(trees.loclookup$ref[i])
      error()
    } else if(is.na(refloc(trees.loclookup$ref[i])$x)) {
      print("Reference requested does not have coordinates: ")
      print(trees.loclookup$ref[i])
      error()
    } else {      
      trees.loclookup[i,]$x <- offsetx(refloc(trees.loclookup$ref[i])$x,trees.loclookup$brg.to.ref.true[i],trees.loclookup$dist.to.ref[i])
      trees.loclookup[i,]$y <- offsety(refloc(trees.loclookup$ref[i])$y,trees.loclookup$brg.to.ref.true[i],trees.loclookup$dist.to.ref[i])
      trees.loclookup[i,]$dummy.coords <- refloc(trees.loclookup$ref[i])$dummy.coords
      
    }
  } else { #the reference is another tree
    if(nrow(treeloc(trees.loclookup$ref[i]))==0) {
      print("Tree requested as reference not found: ")
      print(trees.loclookup$ref[i])
      error()
    } else if (is.na(treeloc(trees.loclookup$ref[i])$x)) {
      print("Tree requested as reference does not have coordinates: ")
      print(trees.loclookup$ref[i])
      error()
    } else {
      trees.loclookup[i,]$x <- offsetx(treeloc(trees.loclookup$ref[i])$x,trees.loclookup$brg.to.ref.true[i],trees.loclookup$dist.to.ref[i])
      trees.loclookup[i,]$y <- offsety(treeloc(trees.loclookup$ref[i])$y,trees.loclookup$brg.to.ref.true[i],trees.loclookup$dist.to.ref[i])
      trees.loclookup[i,]$dummy.coords <- treeloc(trees.loclookup$ref[i])$dummy.coords
      
    }
    
  }
  
}

write.csv(trees.loclookup[trees.loclookup$tree.or.logger=="tree",],"processed data/trees_loc.csv")
write.csv(trees.loclookup[trees.loclookup$tree.or.logger=="logger",],"processed data/loggers_loc.csv")

#double-check back bearings are working properly