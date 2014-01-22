### 29 April 2013 ###
### By Lizzie ###

## It's time to start the code for Heather's synchrony project ##
## Let's go! ##

options(stringsAsFactors=FALSE)
setwd("~/Documents/git/R/trophsynch")

# get data
taxer <- read.csv("input/rough_taxa.csv", header=TRUE)
obsraw <- read.csv("input/rough.csv", header=TRUE)

# data clean up (more needed here!)
taxer$latbi <- paste(taxer$genus, taxer$species)
taxer$role <- tolower(taxer$role)
taxer$role[taxer$role=="pos"] <- "positive"
taxer$role[taxer$role=="neg"] <- "negative"

# the f(x)s I built require you to divide out trophic intxns
# from other interactions so that's what I do next

taxer.trophic <- subset(taxer, interaction != "mutualist" &
    interaction != "pollination" & interaction != "competition" & 
    studyid != "HMK006" & studyid !="kjb001" & studyid !="kjb002" &
    studyid != "JEH001")
# something is wrong with HMK006, kbj001, kbj002 and JEH001

taxer.mutual <- subset(taxer, interaction=="mutualist" |
    interaction == "pollination")

######
## f(x) to build all possible same-same interactions
######
# note:
# (1) make sure you have a latbi column
# (2) make sure theses really are non-trophic interactions!
buildintxns.nontrophic <- function(dater, idcol) {
intxndf.nt <- lapply(unique(dater[[idcol]]), function(uniquesite){
     subby <- subset(dater, dater[[idcol]]==uniquesite)
     combos <- combn(subby$latbi, 2, simplify=TRUE)
     intxnframe <- data.frame(
        studyid = uniquesite,                     
        poslatbi1 = combos[1,],
        poslatbi2 = combos[2,]
        )
   })
do.call("rbind", intxndf.nt)
}

######
## f(x) to build all possible positive-negative interactions
######
# note:
# (1) make sure you have a 'role' column with positive or negative as values
# (2) make sure you have a latbi column

buildintxns <- function(dater, idcol) {
intxndf <- lapply(unique(dater[[idcol]]), function(uniquesite){
     subby <- subset(dater, dater[[idcol]]==uniquesite)
     allgood <- subby[subby$role == "positive",]
     ohdear <- subby[subby$role == "negative",]
     combinate <- expand.grid(ohdear$latbi, allgood$latbi)
     badgood <- data.frame(
        studyid = uniquesite,
        neglatbi = as.character(combinate$Var1),
        poslatbi = as.character(combinate$Var2)
        )
  })
do.call("rbind", intxndf)
}

## run the f(x)s and say 'ahh':
buildintxns.nontrophic(taxer.mutual, "studyid")
buildintxns(taxer.trophic, "studyid")

# in reality though you will more often run them and assign them to an object, like this:
roughintxn <- buildintxns(taxer.trophic, "studyid")
