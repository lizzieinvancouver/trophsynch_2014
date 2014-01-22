 ### 29 April 2013 ###
### By Lizzie ###

## It's time to start the code for Heather's synchrony project ##
## Let's go! ##

options(stringsAsFactors=FALSE)
setwd("/Volumes/MUSIC/UBC/synchrony project/analysis")

# get data
taxer <- read.csv("taxa.csv", header=TRUE)
obsraw <- read.csv("obs.csv", header=TRUE)

# data clean up (more needed here!)
taxer$latbi <- paste(taxer$genus, taxer$species)
taxer$role <- tolower(taxer$role)
taxer$role[taxer$role=="pos"] <- "positive"
taxer$role[taxer$role=="neg"] <- "negative"

# the f(x)s I built require you to divide out trophic intxns
# from other interactions so that's what I do next

taxer.trophic <- subset(taxer, interaction != "mutualist" & interaction != "pollination" & interaction != "competition")

# interaction != "mutualist" & interaction != "pollination" : trophic! BUT keep statements in because otherwise crash- different coding needed for mutualism

taxer.mutual <- subset(taxer, interaction=="mutualist" & interaction == "pollination")
taxer.comp <- subset(taxer, interaction=="competition")

######
## f(x) to build all possible POSITIVE same-same interactions
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
## f(x) to build all possible NEGATIVE same-same interactions
######
# note:
# (1) make sure you have a latbi column
# (2) make sure theses really are non-trophic interactions!
buildintxns.comp <- function(dater, idcol) {
intxndf.nt <- lapply(unique(dater[[idcol]]), function(uniquesite){
	subby <- subset(dater, dater[[idcol]]==uniquesite)
	combos <- combn(subby$latbi, 2, simplify=TRUE)
	intxnframe <- data.frame(
	studyid = uniquesite,                     
	neglatbi1 = combos[1,],
	neglatbi2 = combos[2,]
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
intxn_trophic <- buildintxns(taxer.trophic, "studyid")
names(intxn_trophic)[2]<-"latbi"
intxn_trophic2<-merge(intxn_trophic, taxer.trophic[,c("latbi","interaction")],by="latbi")
names(intxn_trophic2)[1]<-"spp1"; names(intxn_trophic2)[3]<-"spp2"

intxn_mut<- buildintxns.nontrophic(taxer.mutual, "studyid")
intxn_comp<- buildintxns.comp(taxer.comp, "studyid")
names(intxn_comp)[2]<-"latbi"
intxn_comp2<-merge(intxn_comp, taxer.comp[,c("latbi","interaction")],by="latbi")
names(intxn_comp2)[1]<-"spp1"; names(intxn_comp2)[3]<-"spp2"

intxn<-rbind(intxn_trophic2,intxn_comp2)
intxn<-intxn[,c("studyid","spp1","spp2","interaction")]
