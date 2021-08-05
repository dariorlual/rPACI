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
readCornealTopography <- function(filepath, ringsTotal = 24, pointsPerRing = 256, ringsToUse = 15, onlyCompleteRings = TRUE, NAvalues=c(-1,-1000)) {
  
  if (!file.exists(filepath)) {
    stop("The specified file does not exist or the path is invalid.")
  }
  
  if (ringsToUse>ringsTotal) {
    stop("Number of rings to use must be less or equan than the total number of rings.")  
  }
  
  if (round(ringsTotal)!=ringsTotal || ringsTotal<=0) {
    stop("The total number of rings must be a positive integer")  
  }  
  
  if (round(pointsPerRing)!=pointsPerRing || pointsPerRing<=0) {
    stop("The number of points per rings must be a positive integer")  
  }  
  
  if (round(ringsToUse)!=ringsToUse || ringsToUse<=0) {
    stop("The number of rings to use must be a positive integer")  
  }  
  
  # Open file and extract its lines
  connection=file(filepath,open="r")
  close(connection)
  connection=file(filepath,open="r")
  file_lines=readLines(connection, warn=FALSE)
  close(connection)
  
  
  # Substitute "," by "." as decimal separators 
  file_lines = gsub(",",".", file_lines)
  
  
  # Identify and discard the file header (lines at the beginning of the file
  # that are not convertible to numbers)
  linesToDrop = 0
  headerEnd = FALSE
  j = 1
  while(linesToDrop < length(file_lines) && headerEnd == FALSE) {
    lineNumeric = suppressWarnings(as.numeric(file_lines[j]))
    if (is.na(lineNumeric)) {
      linesToDrop = linesToDrop+1
    }
    else {
      headerEnd = TRUE
    }
    j=j+1
  }
  
  
  # Save separately the lines that are convertible to numbers
  numeric_lines = file_lines
  if (linesToDrop>0) {
    numeric_lines = file_lines[-c(1:linesToDrop)]
  }
  
  
  # Substitute values corresponding to NAs, according to parameter NAvalues (by default, -1 and -1000)
  for (j in NAvalues) {
    numeric_lines[numeric_lines==j] = NA
  }
  
  
  # Create equally-spaced angles between 0 and 2*pi radians (0 and 360 degrees)
  dataPoints = pointsPerRing * ringsTotal
  angles = seq(0,2*pi,length.out = (pointsPerRing+1))[1:pointsPerRing]
  angles = rep(angles,ringsTotal)
  length(angles)
  
  
  # The radii are obtained from the file, corresponding to the first 'dataPoints' numeric lines
  radii = as.numeric(numeric_lines[1:dataPoints])
  
  if(any(radii<0, na.rm = T)){
    stop('Some radii are negative. Probably, the "NAvalues" argument is incorrect.')
  }
  
  # If 'onlyCompleteRings', then discard whole rings starting from the first NA
  ringsActual = ringsToUse
  if (onlyCompleteRings) {
    firstNA = which(is.na(radii))[1]
    
    if(!(is.null(firstNA) || is.na(firstNA) || firstNA > ringsToUse*pointsPerRing)) {
      ringsActual = floor(firstNA/pointsPerRing)
    }
  }
  
  
  # Build the 'result' dataframe with the selected data
  lastDataIndex = (pointsPerRing*ringsActual)
  result=data.frame(matrix(NA, nrow = lastDataIndex, ncol = 0))
  result["x"] = radii[1:lastDataIndex] * cos(angles[1:lastDataIndex])
  result["y"] = radii[1:lastDataIndex] * sin(angles[1:lastDataIndex])
  result["ring index"] = kronecker(1:ringsActual,rep(1,pointsPerRing))
  
  colnames(result) = c("x","y","ring index")
  return(result)
}


checkDataset <- function(dataset){
  # check if the number of columns equals 3
  if(ncol(dataset) != 3){
    stop('The dataset must contain 3 columns: x, y (cartesian coordinates of data points) and ring index (1, 2, …).')
  }
  
  # check if third column contains integers
  if(!all(dataset[,3] == round(dataset[,3]))){
    stop('The third column must contain integers (ring index)')
  }
  
  # look for NAs
  if(any(is.na(dataset))){
    stop('The dataset cannot contain NA values.')
  }
  
  # check if all the rings contain the same number of points
  pointsPerRing = aggregate(dataset, FUN = length, by = list(dataset[,3]))[,2]
  # If length == 1, all the rings contain the same number of points
  if(length(unique(pointsPerRing)) != 1){
    stop('All the rings must contain the same number of data points.')
  }
  
  return(T)
}