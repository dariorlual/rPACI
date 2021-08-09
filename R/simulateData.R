#' Simulate data as measured by a Placido disk corneal topographer
#'
#' The function \code{simulateData} permits to simulate a wide variety of datasets that appear in clinical practice, as a result of measuring an individual eye with a Placido-disk corneal topographer (see vignette("topographersDataFormat.Rmd")).
#'
#' This function produces a dataset in the same format as the one read by \link[rPACI]{readCornealTopography} from a file, i.e., a list with three columns (x and y coordinates of each point and its ring index) and a row per data point, according to the function parameters (by default, 6144 rows or data points).
#'
#' See vignette("simulating.Rmd") for additional details. The examples included there show different ways of using \code{simulateData}, by adding different transformations or perturbations to the basic circular pattern. Some of the obtained patterns can correlate with certain clinical conditions, such as keratoconus, comma, or others.
#' 
#' The simulated dataset can be later used according to the package workflow explained in the
#' \code{vignette("packageUsage", package = "rPACI")} and \href{../doc/packageUsage.html}{\code{vignette("packageUsage", package = "rPACI")}} vignette [Workflow of the rPACI package](../doc/packageUsage.html).
#' 
#' @param rings The total number of rings of mires in the sample (typically in the range 18-30, around 24).
#' @param pointsPerRing The number of points to be sampled in each ring (typically 256 or 360).
#' @param diameter Diameter of the simulated dataset (in mm, typically around 8-12 mm).
#' @param ringRadiiPerturbation Stochastical perturbation of the mires radii distribution (typically between 0 (no perturbation) and 1 (high perturbation)).
#' @param maximumMireDisplacement Mires displacement, drift or off-centering (expressed in mm, and should be a reasonable number according to the diameter used.
#' @param mireDisplacementAngle Direction of mires drift (an angle in degrees, typically in the range 0-360 with 0 meaning positive x direction).
#' @param mireDisplacementPerturbation Stochastical perturbation of the mires drift (typically between 0 (no perturbation) and 1 (high perturbation)).
#' @param ellipticAxesRatio Rate or quotient between the major and minor axes of each ellipse (related to the ellipse eccentricity; 1 means a perfect circle (no eccentricity)).
#' @param ellipticRotation Direction or orientation of the ellipses (an angle in degrees, typically in the range 0-360 with 0 meaning positive x direction).
#' @param overallNoise Random, white noise of a certain magnitude in the Cartesian coordinates of the sampled points (relative to the diameter and the number of rings; 0 means no noise, and 1 large noise).
#' @param seed A seed, included for repeatability when using random perturbations.
#' @return A \code{data.frame} with columns:
#' \tabular{lll}{
#'   \code{x}   \tab\tab The X Cartesian coordinates of sampled points\cr
#'   \code{y}   \tab\tab The Y Cartesian coordinates of sampled points\cr
#'   \code{ring index}  \tab\tab Number or index of the ring from which each point is sampled\cr
#' }
#' The resulting \code{data.frame} also includes in its \code{Parameters} attribute (\code{attr(result,'Parameters')}) the list of parameters used for the simulation. 
#' @export
#' @examples
#' # Simulating with default parameters
#' dataset = simulateData()  
#' plot(dataset$x,dataset$y)
#' 
#' # Simulating with 20 rings and a diameter of 8 mm
#' dataset = simulateData(rings = 20, diameter = 8)
#' plot(dataset$x,dataset$y)
#' 
#' # Simulating with default parameters and 500 points per ring (15x500 points)
#' dataset = simulateData(pointsPerRing = 500)
#' plot(dataset$x,dataset$y)
#' 
#' # Simulating an elliptic dataset, with ellipses axis ratio of 0.8 and an orientation of 45 degrees.
#' dataset = simulateData(ellipticAxesRatio = 0.8, ellipticRotation = 45)
#' plot(dataset$x,dataset$y)
#' 
#' # To see the parameters used in the simulation, access the 'Parameters' attribute:
#' attr(dataset,'Parameters')
simulateData <- function(rings = 15, pointsPerRing = 256, diameter = 12, ringRadiiPerturbation = 0, 
                         maximumMireDisplacement = 0, mireDisplacementAngle = 0, mireDisplacementPerturbation = 0,
                         ellipticAxesRatio = 1, ellipticRotation = 0, overallNoise = 0, seed = 0) {
  
  if (round(rings)!=rings || rings<=0) {
    stop("The number of rings to use must be a positive integer")  
  }  
  if (round(pointsPerRing)!=pointsPerRing || pointsPerRing<=0) {
    stop("The number of points per rings must be a positive integer")  
  }    
  if (diameter<=0) {
    stop("The diameter must be a positive real number")  
  }
  if (ellipticAxesRatio<=0) {
    stop("The ellipses axes ratio must be a positive real number")  
  }  
  if (maximumMireDisplacement>=diameter/2) {
    warning("maximumMireDisplacement seems to be too high, please revise it")  
  }   
  if (maximumMireDisplacement<0) {
    stop("The maximum mire displacement must be non-negative real number")  
  }      
  
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
  mireCentersRho = mireCentersRho + mireDisplacementPerturbation * rnorm(rings, sd = mireCentersRho[1]/6)
  
  mireCentersAngle = rep(mireDisplacementAngleRad, each = rings)
  
  mireCentersX = mireCentersRho * cos(mireCentersAngle)
  mireCentersY = mireCentersRho * sin(mireCentersAngle)  
  
  mireCentersX = rep(mireCentersX, each = pointsPerRing)
  mireCentersY = rep(mireCentersY, each = pointsPerRing)
  
  
  result=data.frame(matrix(NA, nrow = rings*pointsPerRing, ncol = 0))
  result["x"] = mireCentersX + radii * cos(ellipticRotationRad) * cos(angles) - radii * ellipticAxesRatio * sin(ellipticRotationRad) * sin(angles)
  result["y"] = mireCentersY + radii * sin(ellipticRotationRad) * cos(angles) + radii * ellipticAxesRatio * cos(ellipticRotationRad) * sin(angles)
  
  # Normalize final data so that it has the specified diameter (except for the noise below)
  maxDistance = max(sqrt(result["x"]^2 + result["y"]^2))
  result["x"] = diameter/2/maxDistance * result["x"]
  result["y"] = diameter/2/maxDistance * result["y"]
  
  if (overallNoise > 0) {
    errorX =  overallNoise*rnorm(dataPoints,sd=0.1)*diameter/rings
    errorY =  overallNoise*rnorm(dataPoints,sd=0.1)*diameter/rings
    result["x"] = result["x"] + errorX
    result["y"] = result["y"] + errorY
  }
  result["ring index"] = kronecker(1:rings,rep(1,pointsPerRing))
  
  
  colnames(result) = c("x","y","ring index")
  
  attr(result, 'Parameters') = list(rings = rings, pointsPerRing = pointsPerRing, diameter = diameter, ringRadiiPerturbation = ringRadiiPerturbation, 
                                    maximumMireDisplacement = maximumMireDisplacement, mireDisplacementAngle = mireDisplacementAngle, mireDisplacementPerturbation = mireDisplacementPerturbation,
                                    ellipticAxesRatio = ellipticAxesRatio, ellipticRotation = ellipticRotation, overallNoise = overallNoise, seed = seed)
  return(result)
  
}
