require("rPACI")


#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setwd(paste(dirname(rstudioapi::getActiveDocumentContext()$path),"/../R", sep=""))
#source("rPACI.R")


##### EXAMPLE 1: (Default) Perfect circular pattern (24 rings * 256 points per ring, with diameter=12mm) ######
dataset = simulateData()
plot(dataset$x,dataset$y,pch=20,cex=0.3,asp=1, xlab="", ylab="")  



##### EXAMPLE 2: Other perfect circular pattern (15 rings, 128 points per ring, diameter 8mm) ######
dataset = simulateData(rings = 15, pointsPerRing = 128, diameter = 8)
plot(dataset$x,dataset$y,pch=20,cex=0.3,asp=1, xlab="", ylab="")  



##### EXAMPLE 3: Circular pattern with unequally distributed radii ######
dataset = simulateData(rings = 18, ringRadiiPerturbation = 0.7)
plot(dataset$x,dataset$y,pch=20,cex=0.3,asp=1, xlab="", ylab="")  



##### EXAMPLE 4a: Adding mire displacement (equally distributed displacements) ######
dataset = simulateData(rings = 12, maximumMireDisplacement = 2, mireDisplacementAngle = 45)
plot(dataset$x,dataset$y,pch=20,cex=0.3,asp=1)  



##### EXAMPLE 4b: Adding mire displacement (unequally distributed displacements) ######
dataset = simulateData(rings = 12, maximumMireDisplacement = 2, mireDisplacementAngle = 45, mireDisplacementNoise = 1.2)
plot(dataset$x,dataset$y,pch=20,cex=0.3,asp=1)  



##### EXAMPLE 5: Elliptic mires ######
dataset = simulateData(ellipticAxesRatio = 0.8, ellipticRotation = 30)
plot(dataset$x,dataset$y,pch=20,cex=0.3,asp=1)    



##### EXAMPLE 6: Random noise in both Cartesian coordinates ######
dataset = simulateData(overallNoise = 0.2)
plot(dataset$x,dataset$y,pch=20,cex=0.3,asp=1)    



##### EXAMPLE 7a: Extremely irregular pattern, combining different perturbations ######
dataset = simulateData(maximumMireDisplacement = 1.5, mireDisplacementAngle = 135, ringRadiiPerturbation = 1, mireDisplacementNoise = 0.8, ellipticAxesRatio = 0.8, ellipticRotation = 30)
plot(dataset$x,dataset$y,pch=20,cex=0.3,asp=1)   



##### EXAMPLE 7b: Same as Example 7a, plus a small Cartesian noise ######
dataset = simulateData(maximumMireDisplacement = 1.5, mireDisplacementAngle = 135, ringRadiiPerturbation = 1, mireDisplacementNoise = 0.8, ellipticAxesRatio = 0.8, ellipticRotation = 30, overallNoise = 0.1)
plot(dataset$x,dataset$y,pch=20,cex=0.3,asp=1)  


