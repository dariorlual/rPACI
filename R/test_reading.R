
setwd(paste(dirname(rstudioapi::getActiveDocumentContext()$path)))
source("rPACI.R")


setwd(paste(dirname(rstudioapi::getActiveDocumentContext()$path),"/../inst/extdata", sep=""))

#readCornealTopography(filepath, ringsTotal = 24, pointsPerRing = 256, ringsToUse = 15, dropLines, NAvalues=c(-1,-1000))


dataK01 = readCornealTopography("K01.txt")
head(dataK01)
tail(dataK01)
plot(dataK01$x,dataK01$y)


dataK01 = readCornealTopography("K01.txt", ringsToUse=20, onlyCompleteRings=FALSE)
head(dataK01)
tail(dataK01)
plot(dataK01$x,dataK01$y)
dim(dataK01)
dim(dataK01)[1]/256

dataK04 = readCornealTopography("K04.txt", ringsToUse=13)
head(dataK04)
tail(dataK04)
plot(dataK04$x,dataK04$y) 
dim(dataK04)
dim(dataK04)[1]/256

dataK04 = readCornealTopography("K04.txt", ringsToUse=19)
head(dataK04)
tail(dataK04)
plot(dataK04$x,dataK04$y)
dim(dataK04)
dim(dataK04)[1]/256

dataK04 = readCornealTopography("K04.txt", ringsToUse=19, onlyCompleteRings=FALSE)
head(dataK04)
tail(dataK04)
plot(dataK04$x,dataK04$y)
dim(dataK04)
dim(dataK04)[1]/256

dataK04 = readCornealTopography("K04.txt", ringsToUse=24, onlyCompleteRings=FALSE)
head(dataK04)
tail(dataK04)
plot(dataK04$x,dataK04$y)
dim(dataK04)
dim(dataK04)[1]/256

