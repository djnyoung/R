trees <- read.csv("trees.csv",header=TRUE, as.is=TRUE)
refs <- read.csv("refs.csv",header=TRUE, as.is=TRUE)

### BEGIN WITH FUNCTION DEFS

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
offsetx <- function(x,bearing,distance) {   #coordinates of reference, angle in true degrees shooting TO the reference, distance in meters
  bearing <- (bearing-180) %% 360 #convert back-bearing to forward bearing
  x + sin(bearing*pi/180)*distance
}

offsety <- function(y,bearing,distance) {   #coordinates of reference, angle in true degrees shooting TO the reference, distance in meters
  bearing <- (bearing-180) %% 360 #convert back-bearing to forward bearing
  y + cos(bearing*pi/180)*distance
}



### START MAIN CODE

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





