#' Compute the Placido irregularity indices of a corneal dataset
#'
#' This function computes a set of indices or metrics from a \code{data.frame} which contains points measured from 
#' the cornea. The dataset may have been obtained reading from a corneal topography file with \link[rPACI]{readFile} 
#' or simulated with \link[rPACI]{simulateData}. These indices allow to discriminate between normal and irregular 
#' corneas. For more information on the indices and their precise mathematical definitions, see 
#' \href{../doc/indicesDefinition.html}{\code{vignette("indicesDefinition", package = "rPACI")}} or the references below.
#'
#' The Placido irregularity indices can be computed from a \code{data.frame} in the format given by the functions
#' \link[rPACI]{readFile} (also with \link[rPACI]{readCSO} or \link[rPACI]{readrPACI}) or \link[rPACI]{simulateData}.
#' 
#' These irregularity indices can be split into two categories: primary and combined indices. 
#' The primary indices are: **PI_1**, **PI_2**, **PI_3**, **SL**, **AR_1**, **AR_2**, **AR_3**, **AR_4**, **AR_5**.
#' They all measure certain geometrical properties of the data distribution. Based on them, other combined indices are 
#' computed: **GLPI** (a generalized linear model) and **NBI** (naive Bayes index).
#' 
#' For more information on these indices and their precise mathematical definitions, see 
#' \href{../doc/indicesDefinition.html}{\code{vignette("indicesDefinition", package = "rPACI")}} or the references below.
#' 
#' They were introduced and validated with real datasets in 3 scientific papers (see the references below). In these 
#' papers, all indices demonstrated a good sensitivity for detection of keratoconus, a corneal disease. For more
#' details about corneal topography and keratoconus, see \href{../doc/topographersDataFormat.html}{\code{vignette("topographersDataFormat", package = "rPACI")}}
#' 
#' The results include the values of the indices (0 meaning normal, and a large positive value meaning irregular, 
#' check the range for each index) plus a diagnose, which is either "Irregular cornea", "Suspect cornea" or 
#' "Normal cornea", depending on the value of the combined index GLPI.
#'
#' @references Castro-Luna, Gracia M., Andrei Martínez-Finkelshtein, and Darío Ramos-López. 2020. "Robust Keratoconus Detection with Bayesian Network Classifier for Placido Based Corneal Indices." Contact Lens and Anterior Eye 43 (4): 366-72. \href{https://doi.org/10.1016/j.clae.2019.12.006}{DOI link}. 
#' @references Ramos-López, Darío, Andrei Martínez-Finkelshtein, Gracia M. Castro-Luna, Neus Burguera-Giménez, Alfredo Vega-Estrada, David Piñero, and Jorge L. Alió. 2013. "Screening Subclinical Keratoconus with Placido-Based Corneal Indices." Optometry and Vision Science 90 (4): 335-43. \href{https://doi.org/10.1097/opx.0b013e3182843f2a}{DOI link}. 
#' @references Ramos-López, Darío, Andrei Martínez-Finkelshtein, Gracia M. Castro-Luna, David Piñero, and Jorge L. Alió. 2011. "Placido-Based Indices of Corneal Irregularity." Optometry and Vision Science 88 (10): 1220-31. \href{https://doi.org/10.1097/opx.0b013e3182279ff8}{DOI link}. 
#' @param datasetRings A dataset containing data points of a corneal topography, as given by \link[rPACI]{readFile} or \link[rPACI]{simulateData}.
#' @param truncateIndicesAt150 A boolean value (by default \code{TRUE}) indicating whether the primary indices should be truncated at 150 (so they are in the range 0-150) or not.
#' @param useMaxRings A positive integer value (by default 15) to choose the maximum number of innermost rings to use (as long as there are enough).
#' @return A \code{data.frame} containing the Placido irregularity indices as well as the diagnose, with columns:
#' \tabular{lll}{
#'   \code{Diagnose}   \tab\tab A text label indicating the diagnose, according to the value of GLPI\cr
#'   \code{NBI}   \tab\tab The value of NBI index (in the range 0-100).\cr
#'   \code{GLPI}  \tab\tab The value of GLPI index (in the range 0-100).\cr
#'   \code{PI_1}  \tab\tab The value of PI_1 index (usually in the range 0-150).\cr   
#'   \code{PI_2}  \tab\tab The value of PI_2 index (usually in the range 0-150).\cr   
#'   \code{PI_3}  \tab\tab The value of PI_3 index (usually in the range 0-150).\cr   
#'   \code{SL}  \tab\tab The value of SL index (usually in the range 0-150).\cr   
#'   \code{AR_1}  \tab\tab The value of AR_1 index (usually in the range 0-150).\cr
#'   \code{AR_2}  \tab\tab The value of AR_2 index (usually in the range 0-150).\cr   
#'   \code{AR_3}  \tab\tab The value of AR_3 index (usually in the range 0-150).\cr   
#'   \code{AR_4}  \tab\tab The value of AR_4 index (usually in the range 0-150).\cr   
#'   \code{AR_5}  \tab\tab The value of AR_5 index (usually in the range 0-150).\cr      
#' }
#' @importFrom bnlearn cpdist
#' @importFrom stats lm pnorm sd
#' @export
#' @examples
#' # Read the file 'N02.txt' which is a real corneal topography (from a normal eye) measured with a CSO device:
#' dataset = readFile(system.file("extdata","N02.txt", package="rPACI"))
#' 
#' # Compute its Placido irregularity indices with this function:
#' results = computePlacidoIndices(dataset)
#' results
computePlacidoIndices <- function(datasetRings, truncateIndicesAt150 = TRUE, useMaxRings = 15) {
  
  x = datasetRings[,"x"]
  y = datasetRings[,"y"]
  ringIndices = datasetRings[,"ring index"]
  
  if(max(ringIndices) < 5){
    stop('The number of rings must be at least 5. The dataset does not meet this requirement.')
  }
  if(useMaxRings < 5){
    stop('Argument useMaxRings must be >= 5.')
  }
  
  if(sum(as.integer(ringIndices)!=ringIndices)>0) {
    stop("Invalid dataset: a column named 'ring index' of type integer is required")
  }
  
  # if (useMax15Rings) {
  lastRing = max(ringIndices)
  if(lastRing>useMaxRings) {
    warning(paste("Too many rings: using only the", useMaxRings, "innermost rings"))
    lastRing=useMaxRings
    datasetRings = datasetRings[datasetRings$`ring index`<=useMaxRings,]
    x = datasetRings[,"x"]
    y = datasetRings[,"y"]
    ringIndices = datasetRings[,"ring index"]
  }
  # }
  
  if (length(ringIndices)/lastRing == floor(length(ringIndices)/lastRing)) {
    dataPerRing = length(ringIndices)/lastRing
  } else {
    stop("Invalid dataset: incomplete rings")
  }
  
  if(sum( (kronecker(1:lastRing,rep(1,dataPerRing))-ringIndices)**2 ) !=0 ) {
    stop("Invalid dataset: unequal number of data points per ring")
  }
  
  
  PlacidoCornealIndices = c()
  BC_centers = matrix(NA,nrow=lastRing,ncol=2)
  BC_radii = matrix(NA,nrow=lastRing,ncol=1)
  
  dataCentered = matrix(NA,nrow=dataPerRing*lastRing,ncol=2)
  
  #******************************************
  # FIT A CIRCLE TO EACH RING
  #******************************************
  for (k in 1:lastRing) {
    # First and last data point on each ring
    ringInit=1+dataPerRing*(k-1)
    ringEnd=dataPerRing*k
    
    # Center and radius of the best-fit circle to each ring
    A=matrix(NA,nrow=dataPerRing,ncol=2)
    
    xRing = x[ringInit:ringEnd]
    yRing = y[ringInit:ringEnd]
    
    A[,1] = xRing
    A[,2] = yRing
    
    b = - (xRing**2 + yRing**2)
    
    if(sum(is.na(A))>0 || sum(is.na(b))>0) {
      print(datasetRings)
      print(k)
    }
    
    linearRegression = stats::lm(b ~ A)
    coefficients = as.numeric(linearRegression$coefficients)
    
    
    center = - coefficients[2:3]/2
    BC_centers[k,] = center
    
    radius = sqrt( sum(center**2)-coefficients[1] )
    
    if(radius<=0) {
      stop("Best-fit circle with negative radius")
    }
    BC_radii[k,] = radius
    
    # RECOMPUTE DATA POINTS WITH RESPECT TO THE NEW CENTER
    xDesp = xRing - center[1]
    yDesp = yRing - center[2]
    
    # Conversion to polar coordinates
    thetaDesp = atan2(yDesp, xDesp)
    rhoDesp = sqrt( xDesp**2 + yDesp**2 )
    thetaDesp[thetaDesp<0]=thetaDesp[thetaDesp<0]+2*pi
    
    dataCentered[ringInit:ringEnd,1] = rhoDesp
    dataCentered[ringInit:ringEnd,2] = thetaDesp
  }
  
  BC_distanceMatrix = distanceMatrix(BC_centers)
  
  PlacidoCornealIndices[1] = 12368.3980719706*max(BC_distanceMatrix)/lastRing - 12.5200561911951
  PlacidoCornealIndices[2] = 9699.23915314471*mean( BC_distanceMatrix[row(BC_distanceMatrix) == (col(BC_distanceMatrix) - 1)] ) - 18.2916611541283
  
  
  #******************************************
  # FIT AN ELLIPSE TO EACH RING
  #******************************************
  
  BE_eccen = matrix(NA,nrow=lastRing,ncol=1)
  
  for (k in 1:lastRing) {
    # First and last data point on each ring
    ringInit=1+dataPerRing*(k-1)
    ringEnd=dataPerRing*k
    
    BE_eccen[k] = ellipseEccentricity( x[ringInit:ringEnd], y[ringInit:ringEnd] )
  }
  
  PlacidoCornealIndices[3] = 5233.70399537826 * sd(BE_eccen) - 13.7375152045819
  
  
  #******************************************
  #  LINEAR REGRESSION OF BC CENTERS COORDINATES
  #******************************************
  
  linearRegression = stats::lm(BC_centers[,2] ~ BC_centers[,1])
  coefficients = as.numeric(linearRegression$coefficients)
  
  PlacidoCornealIndices[4] = 50 * abs(coefficients[2])
  
  
  #******************************************
  # ABSOLUTE POSITION OF RINGS 1 AND 4
  #******************************************
  
  PlacidoCornealIndices_AR = c()
  
  PlacidoCornealIndices_AR[1]= -1961.92346976023 * mean(BC_radii[1]) + 444.770836424576
  PlacidoCornealIndices_AR[2]= -562.8465909984122 * mean(BC_radii[2]) + 279.7430721547980
  PlacidoCornealIndices_AR[3]= -486.9218093835968 * mean(BC_radii[3]) + 331.3137688964757
  PlacidoCornealIndices_AR[4]= -318.192614292727 * mean(BC_radii[4]) + 298.064078007947
  PlacidoCornealIndices_AR[5]= -288.4037960143595 * mean(BC_radii[5]) + 321.4180137915255
  
  
  
  
  PlacidoCornealIndices[PlacidoCornealIndices<0]=0
  sprintf("%.2f",PlacidoCornealIndices)
  
  PlacidoCornealIndices_AR[PlacidoCornealIndices_AR<0]=0
  sprintf("%.2f",PlacidoCornealIndices_AR)
  
  if (truncateIndicesAt150) {
    PlacidoCornealIndices[PlacidoCornealIndices>150] = 150
    PlacidoCornealIndices_AR[PlacidoCornealIndices_AR>150] = 150
  }
  
  #******************************************
  ########  COMPOSITE INDICES
  #******************************************
  
  PI1 = PlacidoCornealIndices[1]
  PI2 = PlacidoCornealIndices[2]
  PI3 = PlacidoCornealIndices[3]
  PI4 = PlacidoCornealIndices[4]
  
  AR1 = PlacidoCornealIndices_AR[1]
  AR4 = PlacidoCornealIndices_AR[4]
  
  erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
  linearPredictor = 0.05*( -224.8951312 + 1.6878796 * PI1 + 1.2777558 * PI3 + 1.8832870 * AR4 + 0.1938861 * PI4 )
  GLPI = 100 * ( 0.5*( 1.0 + erf(linearPredictor/sqrt(2)) ))
  
  PlacidoIndexGLPI = GLPI
  
  PlacidoCombinedDecision = 2*(GLPI>50) + 1*((GLPI<=50) && (PI4>50))
  
  
  
  
  ############### NEW NAIVE BAYES INDEX #####################
  fittedbn = NB_classifier
  samples <- bnlearn::cpdist(fittedbn, nodes = "KTC", evidence = list(PI_1 = PI1, PI_2 = PI2, PI_3 = PI3, SL = PI4, AR_1 = AR1, AR_4 = AR4),
                             method = "lw", n = 5000)
  
  assigned_classes = samples$KTC
  assigned_classes = as.numeric(assigned_classes)
  assigned_classes = assigned_classes-1
  weights <- attr(samples, "weights")
  NBI = 100*sum(assigned_classes*weights)/sum(weights)
  
  # table(assigned_classes*weights)/length(assigned_classes)
  #
  # mean(weights[which(assigned_classes==0)])
  # mean(weights[which(assigned_classes==1)])
  #
  # aggregate(weights, FUN = mean, by = list(assigned_classes))
  
  
  ############### FINAL DIAGNOSE #####################
  
  
  if (GLPI>=70) {
    diagnose = "Irregular cornea"
  } else if (GLPI<70 && GLPI>=30) {
    diagnose = "Suspect cornea"
  } else {
    diagnose = "Normal cornea"
  }
  
  
  ############### COMBINE RESULTS #####################
  
  PlacidoIndices = data.frame(matrix(NA, nrow = 1, ncol = 12))
  colnames(PlacidoIndices) <- c("Diagnose","NBI","GLPI","PI_1","PI_2","PI_3","SL","AR_1","AR_2","AR_3","AR_4","AR_5")
  
  PlacidoIndices[1,] = c(diagnose,NBI,PlacidoIndexGLPI,PlacidoCornealIndices,PlacidoCornealIndices_AR[c(1:5)])
  
  PlacidoIndices[,-1] <- as.numeric(PlacidoIndices[,-1])
  return(PlacidoIndices)
}

#' @importFrom stats lm
ellipseEccentricity <- function(x,y) {
  
  A=matrix(NA,nrow=length(x),ncol=5)
  
  A[,1] = x**2
  A[,2] = 2*x*y
  A[,3] = y**2
  A[,4] = 2*x
  A[,5] = 2*y
  
  b = - matrix(1, length(x))
  
  linearRegression = stats::lm(b ~ A - 1)
  
  coefficients = as.numeric(linearRegression$coefficients)
  coefficients
  
  
  a=coefficients[1]
  b=coefficients[2]
  c=coefficients[3]
  d=coefficients[4]
  f=coefficients[5]
  g=1;
  
  axis1=sqrt( 2*(a*f**2+c*d**2+g*b**2-2*b*d*f-a*c*g)/( (b**2-a*c)*(sqrt((a-c)**2+4*b**2)-(a+c)) ) );
  axis2=sqrt( 2*(a*f**2+c*d**2+g*b**2-2*b*d*f-a*c*g)/( (b**2-a*c)*(-sqrt((a-c)**2+4*b**2)-(a+c)) ) );
  
  if (axis1>axis2) {
    return(axis1/axis2)
  } else {
    return(axis2/axis1)
  }
}

distanceMatrix = function(points) {
  numPoints = dim(points)[1]
  result = matrix(NA, nrow = numPoints, ncol = numPoints)
  for(i in 1:numPoints) {
    for(j in 1:numPoints) {
      
      x1 = points[i,1]
      y1 = points[i,2]
      
      x2 = points[j,1]
      y2 = points[j,2]
      
      result[i,j] = sqrt((x1-x2)**2 + (y1-y2)**2)
    }
  }
  return(result)
}

