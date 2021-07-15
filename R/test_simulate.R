
  rings = 24
  dataPerRing = 256
  
  maximumMireDisplacement = 2
  mireDisplacementAngle = pi/4
  
  
  mireCentersRho = seq(0,maximumMireDisplacement,length.out = rings+1)[2:(rings+1)]
  mireCentersRho
  mireCentersAngle = rep(mireDisplacementAngle, each = rings)
  mireCentersAngle
  
  mireCentersX = mireCentersRho * cos(mireCentersAngle)
  mireCentersY = mireCentersRho * sin(mireCentersAngle)  
  
  mireCentersX = rep(mireCentersX, each = dataPerRing)
  mireCentersY = rep(mireCentersY, each = dataPerRing)
  
  plot(mireCentersX,mireCentersY,asp=1)  
  
  
  
  
  
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  source("rPACI.R")
  dataset = simulateData(maximumMireDisplacement = 3, mireDisplacementAngle = pi/4)
  plot(dataset$x,dataset$y,pch=20,cex=0.3,asp=1)  
  
  