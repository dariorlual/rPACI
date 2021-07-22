#' Read a Placido disk corneal topography file
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
#' @param filepath A file path to a corneal topography file exported by a Placido disk corneal topographer.
#' @return A dataset containing the read corneal topography.
#' @export
#' @examples
#' datasetN = readCornealTopography(system.file("extdata","N01.txt", package="rPACI"))
#' datasetK = readCornealTopography(system.file("extdata","K04.txt", package="rPACI"))
readCornealTopography <- function(filepath, ringsTotal = 24, pointsPerRing = 256, ringsToUse = 15, dropLines) {
  
  if(!file.exists(filepath)) {
    stop("Error: The specified file does not exist or is invalid.")
  }
  
  connection=file(filepath,open="r")
  close(connection)
  connection=file(filepath,open="r")
  file_lines=readLines(connection, warn=FALSE)
  close(connection)
  
  if(missing(dropLines)) {
    firstCharacter=substr(file_lines[1],1,1)
    dropLines = switch(firstCharacter,
                       "P" = 22,
                       "[" = 4,
                       "0" = 0)
  }
  
  if(is.null(dropLines)) {
    stop("Uknown file type. Please specify the number of header lines to drop")
  }
  
  numeric_lines = gsub(",",".", file_lines[-c(1:dropLines)])
  numeric_lines[numeric_lines==-1] = NA
  numeric_lines[numeric_lines==-1000] = NA
  
  length(numeric_lines)
  
  
  dataPoints = pointsPerRing * ringsTotal
  
  
  angles = seq(0,2*pi,length.out = (pointsPerRing+1))[1:pointsPerRing]
  angles = rep(angles,ringsTotal)
  length(angles)
  
  radii = as.numeric(numeric_lines[1:(ringsTotal*pointsPerRing)])
  length(radii)
  
  firstNA = which(is.na(radii))[1]
  
  if(is.null(firstNA) || firstNA > ringsToUse*pointsPerRing) {
    ringsActual = ringsToUse
  } else {
    ringsActual = floor(firstNA/pointsPerRing)
  }
  
  lastData = (pointsPerRing*ringsActual)
  result=data.frame(matrix(NA, nrow = ringsActual*pointsPerRing, ncol = 0))
  result["x"] = radii[1:lastData] * cos(angles[1:lastData])
  result["y"] = radii[1:lastData] * sin(angles[1:lastData])
  result["ring index"] = kronecker(1:ringsActual,rep(1,pointsPerRing))
  
  colnames(result) = c("x","y","ring index")
  return(result)
}



#' Compute the Placido irregularity indices of an eye
#'
#' This function calculates the individual Placido indices of corneal irregularity PI_1, PI_2, PI_3, SL,
#' AR_1, AR_2, AR_3, AR_4, AR_5, the global index GLPI, and the Naive Bayes Index (NBI) (see references). It requires a dataset in
#' the format given by the function \link[rPACI]{readCornealTopography}. The results include the values
#' of the indices plus a diagnose, which is either "Irregular cornea", "Suspect cornea"
#' or "Normal cornea", depending on the value of the global index GLPI.
#'
#' @references Castro-Luna, G. M., Martinez-Finkelshtein, A.,  Ramos-Lopez, D. (2019). Robust keratoconus detection with Bayesian network classifier for Placido-based corneal indices. Contact Lens and Anterior Eye, 43(4), 366-372.
#' @references Ramos-Lopez, D., Martinez-Finkelshtein, A., Castro-Luna, G. M., Burguera-Gimenez, N., Vega-Estrada, A., Pinero, D., & Alio, J. L. (2013). Screening subclinical keratoconus with placido-based corneal indices. Optometry and Vision Science, 90(4), 335-343.
#' @references Ramos-Lopez, D., Martinez-Finkelshtein, A., Castro-Luna, G. M., Pinero, D., & Alio', J. L. (2011). Placido-based indices of corneal irregularity. Optometry and Vision Science, 88(10), 1220-1231.
#' @param datasetRings A dataset containing data of a corneal topography, as read by \link[rPACI]{readCornealTopography}.
#' @return A dataset containg the aforementioned irregularity indices as well as the diagnose.
#' @importFrom bnlearn cpdist
#' @importFrom stats lm pnorm sd
#' @export
#' @examples
#' dataset = readCornealTopography(system.file("extdata","N02.txt", package="rPACI"))
#' results = computePlacidoIndices(dataset)
computePlacidoIndices <- function(datasetRings) {
  
  x = datasetRings[,"x"]
  y = datasetRings[,"y"]
  ringIndices = datasetRings[,"ring index"]
  
  if(sum(as.integer(ringIndices)!=ringIndices)>0) {
    stop("Invalid dataset: a column named 'ring index' of type integer is required")
  }
  
  lastRing = max(ringIndices)
  if(lastRing>15) {
    warning("Too many rings: using only the 15 innermost rings")
    lastRing=15
  }
  
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
      stop("Error: Best-fit circle with negative radius")
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
  
  PlacidoCornealIndices[PlacidoCornealIndices>150] = 150
  PlacidoCornealIndices_AR[PlacidoCornealIndices_AR>150] = 150
  
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


#' Summary plot of the Placido irregularity indices
#'
#' Draw a three-part plot summarizing the corneal topography analysis, based on the Placido irregularity indices calculated by the function \link[rPACI]{computePlacidoIndices}.
#' @param dataset A dataset containing the read corneal topography.
#' @param PlacidoIndices A dataset of results as given by the function \link[rPACI]{computePlacidoIndices} or \link[rPACI]{analyzeFile}.
#' @param filename An optional character argument, corresponding to the file containing the analyzed data.
#' If specified, the filename is displayed on the plot.
#' @importFrom graphics barplot boxplot par plot rect text
#' @importFrom grDevices rgb
#' @export
#' @examples
#' dataset = readCornealTopography(system.file("extdata","K04.txt", package="rPACI"))
#' results = computePlacidoIndices(dataset)
#' plotSingleCornea(dataset, results)
plotSingleCornea <- function(dataset, PlacidoIndices, filename=NULL) {
  opar <- par(no.readonly =TRUE)
  on.exit(par(opar))
  x=dataset[,"x"]
  y=dataset[,"y"]
  
  
  GLPI = as.numeric(PlacidoIndices["GLPI"])
  color = if(GLPI>=70) "red" else if (GLPI>=30 && GLPI<70) "orange" else "green"
  
  par(mar=c(3,3,3,3))
  par(fig=c(0,5,0,10)/10)
  plot(x,y, pch=20, asp=1, cex=0.2, xlab = "X coordinate (mm)", ylab= "Y coordinate (mm)", main="Corneal topography data")
  text(0,min(y),paste("Diagnose:\n",PlacidoIndices["Diagnose"],sep=""), col=color, cex=1.4, font=2)
  if(!is.null(filename)) {
    text(0,max(y),paste("Filename:\n",basename(filename),sep=""), col=color, cex=1.0, font=2)
  }
  
  par(fig=c(5,7.5,0,10)/10)
  par(new=TRUE)
  plot(GLPI, xlim=c(1,1),ylim=c(0,100),pch=16, cex=1.5, cex.main=0.8, col=color,xaxt="n",main="GLPI index")
  rect(0, -20, 2, 30, col = rgb(0,1,0,alpha=0.15), lwd=0)
  rect(0, 30, 2, 70, col = rgb(1,0.5,0,alpha=0.18), lwd=0)
  rect(0, 70, 2, 120, col = rgb(1,0,0,alpha=0.15), lwd=0)
  
  par(fig=c(7.5,10,0,10)/10)
  par(new=TRUE)
  boxplot(as.numeric(PlacidoIndices[,3:6]), ylim=c(0,150), cex=1.5, cex.main=0.8, main="PI indices \n distribution")
  rect(0, -20, 2, 30, col = rgb(0,1,0,alpha=0.15), lwd=0)
  rect(0, 30, 2, 70, col = rgb(1,0.5,0,alpha=0.18), lwd=0)
  rect(0, 70, 2, 180, col = rgb(1,0,0,alpha=0.15), lwd=0)
  
}

#' Analysis of a single corneal topography files
#'
#' Analyze a corneal topography file. This function combines the three operations of functions \link[rPACI]{readCornealTopography}, \link[rPACI]{computePlacidoIndices} and \link[rPACI]{plotSingleCornea}.
#' @param path A corneal topography file, as exported by a Placido disk corneal topographer.
#' @param drawplot An optional parameter indicating whether a plot of results should be displayed or not.
#' @export
#' @examples
#' analyzeFile(system.file("extdata","N01.txt", package="rPACI"))
analyzeFile <- function(path, drawplot=TRUE) {
  
  data = readCornealTopography(path)
  
  result = computePlacidoIndices(data)
  
  if(drawplot) {
    plotSingleCornea(data, result, path)
  }
  
  return(result)
}

#' Analysis several corneal topography files in a common folder.
#'
#' Analyze all corneal topography files in a specific folder. It is equivalent to use \link[rPACI]{analyzeFile} for each file in the folder.
#' It assumes all files with the given extension ('.txt' by defualt) are corneal topography files.
#'
#' @param path The path of a folder which contains corneal topography files, as exported by Placido disks corneal topographers.
#' @param fileExtension The file extension of the corneal topography files in the folder ('.txt' by default).
#' @param individualPlots An optional logical parameter indicating whether the plot for each file should be displayed or not.
#' @param summaryPlot An optional logical parameter indicating whether a summary plot should be displayed or not.
#' @importFrom graphics barplot boxplot par plot rect text
#' @importFrom grDevices rgb
#' @export
#' @examples
#' analyzeFolder(system.file("extdata",package="rPACI"))
analyzeFolder <- function(path, fileExtension="txt", individualPlots = FALSE, summaryPlot = FALSE) {
  opar <- par(no.readonly =TRUE)
  on.exit(par(opar))
  
  if(!dir.exists(path)) {
    stop("Error: The specified directory does not exist or is invalid.")
  }
  
  pattern = paste("\\.", fileExtension, "$", sep="")
  fileList = list.files(path, pattern)
  
  #print(fileList)
  
  results=NULL
  
  for(file in fileList) {
    
    result = analyzeFile(paste(path,"/", file, sep=""), individualPlots)
    
    result["Filename"] = file
    if(is.null(results)) {
      results=result
    }
    else {
      results = rbind(results,result)
    }
  }
  
  if(summaryPlot){
    dignose<- factor(as.factor(results$Diagnose),
                     levels= c("Irregular cornea", "Suspect cornea","Normal cornea"))
    
    barplot(table(dignose),
            col = c(rgb(1,0,0,alpha=0.15), rgb(1,0.5,0,alpha=0.18), rgb(0,1,0,alpha=0.15)))
  }
  
  res <-data.frame(Diagnose = results$Diagnose, sapply(results[,-c(1,13)], as.numeric), Filename = results$Filename)
  ordered_results <- res[order(res$GLPI, decreasing = T),]
  
  return(ordered_results)
  
  
}



simulateData <- function(rings = 24, dataPerRing = 256, lastRingRadium = 8, ringRadiiNoise = 0, 
                         maximumMireDisplacement = 0, mireDisplacementAngle = 0, mireDisplacementNoise = 0,
                         ellipticAxesRatio = 1, ellipticRotation = 0, overallNoise = 0) {
  
  dataPoints = dataPerRing * rings
  
  angles = seq(0,2*pi,length.out = 257)[1:256]
  angles = rep(angles, times = rings)
  
  radii = seq(0,lastRingRadium,length.out = rings+1)[2:(rings+1)]
  radii = radii + ringRadiiNoise * rnorm(rings, sd = radii[1]/6)
  radii = rep(radii, each = dataPerRing)
  
  # Adding a mire displacement in a certain direction (given by the angle )
  mireCentersRho = seq(0,maximumMireDisplacement,length.out = rings+1)[2:(rings+1)]
  mireCentersRho = mireCentersRho + mireDisplacementNoise * rnorm(rings, sd = mireCentersRho[1]/6)
  
  mireCentersAngle = rep(mireDisplacementAngle, each = rings)
  
  mireCentersX = mireCentersRho * cos(mireCentersAngle)
  mireCentersY = mireCentersRho * sin(mireCentersAngle)  
  
  mireCentersX = rep(mireCentersX, each = dataPerRing)
  mireCentersY = rep(mireCentersY, each = dataPerRing)
  
  
  result=data.frame(matrix(NA, nrow = rings*dataPerRing, ncol = 0))
  result["x"] = mireCentersX + radii * cos(ellipticRotation) * cos(angles) - radii * ellipticAxesRatio * sin(ellipticRotation) * sin(angles) + overallNoise * rnorm(dataPoints,sd=0.1)
  result["y"] = mireCentersY + radii * sin(ellipticRotation) * cos(angles) + radii * ellipticAxesRatio * cos(ellipticRotation) * sin(angles) + overallNoise * rnorm(dataPoints,sd=0.1)
  result["ring index"] = kronecker(1:rings,rep(1,dataPerRing))
  
  
  colnames(result) = c("x","y","ring index")
  return(result)
  
  
}