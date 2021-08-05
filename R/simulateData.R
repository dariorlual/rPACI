#' @todo Add a seed for repeatability, make some periphery data missing at random
simulateData <- function(rings = 15, pointsPerRing = 256, diameter = 12, ringRadiiPerturbation = 0, 
                         maximumMireDisplacement = 0, mireDisplacementAngle = 0, mireDisplacementNoise = 0,
                         ellipticAxesRatio = 1, ellipticRotation = 0, overallNoise = 0) {
  
  dataPoints = pointsPerRing * rings
  
  lastRingRadium = diameter/2
  mireDisplacementAngleRad = pi/180*mireDisplacementAngle
  ellipticRotationRad = pi/180*ellipticRotation  
  
  
  angles = seq(0, 2*pi,length.out = (1+pointsPerRing))[1:pointsPerRing]
  angles = rep(angles, times = rings)
  
  radii = seq(0,lastRingRadium,length.out = rings+1)[2:(rings+1)]
  radii = radii + ringRadiiPerturbation * rnorm(rings, sd = radii[1]/6)
  radii = sort(radii)
  radii = rep(radii, each = pointsPerRing)
  
  # Adding a mire displacement in a certain direction (given by the angle )
  mireCentersRho = seq(0,maximumMireDisplacement,length.out = rings+1)[2:(rings+1)]
  mireCentersRho = mireCentersRho + mireDisplacementNoise * rnorm(rings, sd = mireCentersRho[1]/6)
  
  mireCentersAngle = rep(mireDisplacementAngleRad, each = rings)
  
  mireCentersX = mireCentersRho * cos(mireCentersAngle)
  mireCentersY = mireCentersRho * sin(mireCentersAngle)  
  
  mireCentersX = rep(mireCentersX, each = pointsPerRing)
  mireCentersY = rep(mireCentersY, each = pointsPerRing)
  
  
  result=data.frame(matrix(NA, nrow = rings*pointsPerRing, ncol = 0))
  result["x"] = mireCentersX + radii * cos(ellipticRotationRad) * cos(angles) - radii * ellipticAxesRatio * sin(ellipticRotationRad) * sin(angles) + overallNoise * rnorm(dataPoints,sd=0.1)
  result["y"] = mireCentersY + radii * sin(ellipticRotationRad) * cos(angles) + radii * ellipticAxesRatio * cos(ellipticRotationRad) * sin(angles) + overallNoise * rnorm(dataPoints,sd=0.1)
  result["ring index"] = kronecker(1:rings,rep(1,pointsPerRing))
  
  
  colnames(result) = c("x","y","ring index")
  return(result)
  
}
