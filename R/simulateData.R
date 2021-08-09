#' Simulate data as measured by a Placido disk corneal topographer
#'
#' Read corneal topography files as exported by Placido disk corneal topographer.
#' A corneal topographer is an ophthalmic clinical device that obtains measurements
#' in the cornea (the anterior part of the eye). A Placido disk corneal topographer
#' makes use of the Placido disk (see references), which produce a circular pattern of
#' measurement nodes.
#' This function assumes a file structure of 24 rings * 256 angles per ring,
#' which is the typical distribution of commercial Placido disk topographers.
#' @references Rowsey, J. J., Reynolds, A. E., & Brown, R. (1981). Corneal topography: corneascope. Archives of Ophthalmology, 99(6), 1093-1100
#' @references Rand, R. H., Howland, H. C., & Applegate, R. A. (1997). Keratometer and Its Implications for Recovery of Corneal Topography. Optometry and vision science, 74(11).
#' @param rings The total number of rings of mires in the sample
#' @param pointsPerRing The number of points to be sampled in each ring 
#' @param diameter 
#' @return A \code{data.frame} with columns:
#'   \tabular{ll}{
#' [,1] \tab \code{x}   \tab X Cartesian coordinate of sampled points\cr
#' [,2] \tab \code{y}   \tab Y Cartesian coordinate of sampled points\cr
#' [,3] \tab \code{ring index}  \tab Number of ring from which each point is sampled\cr
#' }
#' The result \code{data.frame} also includes in the 'Parameters' attribute the list of parameters used for the simulation. 
simulateData <- function(rings = 15, pointsPerRing = 256, diameter = 12, ringRadiiPerturbation = 0, 
                         maximumMireDisplacement = 0, mireDisplacementAngle = 0, mireDisplacementNoise = 0,
                         ellipticAxesRatio = 1, ellipticRotation = 0, overallNoise = 0, seed = 0) {
  
  set.seed(seed)
  
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
  
  attr(result, 'Parameters') = list(rings = rings, pointsPerRing = pointsPerRing, diameter = diameter, ringRadiiPerturbation = ringRadiiPerturbation, 
                                    maximumMireDisplacement = maximumMireDisplacement, mireDisplacementAngle = mireDisplacementAngle, mireDisplacementNoise = mireDisplacementNoise,
                                    ellipticAxesRatio = ellipticAxesRatio, ellipticRotation = ellipticRotation, overallNoise = overallNoise, seed = seed)
  return(result)
  
}
