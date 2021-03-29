# Script to associate each CABI listed species with the major activity sector(s) in which it causes damage#

# Written by Emma J. Hudgins as a result of the Invacost Workshop 2019
#emma.hudgins@mail.mcgill.ca

setwd('~/Dropbox/InvaCost Workshop France/Projects/Activity Sector/')
source('CABI_Impacts_scraper.R')
cabiURLS<-read.csv('Datasheet_201911240709.csv') # CABI urls by species from Justice Tambo

whichsector<-data.frame(matrix(0,nrow=nrow(cabiURLS), ncol=6))
whichsector[,1]<-cabiURLS$Scientific.name
colnames(whichsector)<-c("Species", "Agriculture", "Forestry", "Fisheries", "Health", "ispathogen")
for (i in 1:nrow(cabiURLS))
{
  whichsector[i,2:6]<-getImpact(as.character(cabiURLS$URL[i]))
}

write.csv(whichsector, file="cabi_by_sector_v2.csv", row.names=F)
