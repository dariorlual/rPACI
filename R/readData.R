#' Read a corneal topography file in CSO format
#'
#' This function is intended to read external files with a corneal topography in the format that is exported 
#' by some Placido disk topographers, especially those from CSO (a commercial brand).  
#' In general, we recommend to use the more general wrapper function \link[rPACI]{readFile} to read any file format.
#' 
#' A corneal topographer is an ophthalmic clinical device that obtains measurements in the cornea (the anterior
#' part of the eye). A Placido disk corneal topographer makes use of the Placido disk (see references and the 
#' vignette linked below), which  produce a circular pattern of measurement nodes.
#' 
#' The \link[rPACI]{readCSO} function is able to read a raw Placido disk corneal topography from a file that has been
#' exported by certain corneal topographers. It has been especially designed for the file format exported from
#' topographers manufactured by \href{https://www.csoitalia.it}{CSO} (Firenze, Italy).
#' 
#' This reading function has been designed to be more flexible than the CSO format, allowing to specify different
#' parameters: the amounts of rings available, points per ring, and rings to use, whether to use or not only 
#' complete rings, and the value(s) encoding NAs (missing data) in the file. In addition, this function automatically 
#' processes the file and identifies the size and the header,  without assuming a fixed structure or having to 
#' specify its size as a parameter.
#' 
#' This function produces a \code{data.frame} in the usual format used by \code{rPACI}, i.e., a list with three 
#' columns (x and y coordinates of each point and its ring index) and a row per data point, according to the 
#' function parameters (by default, 24*256 = 6144 rows or data points).
#' 
#' See more details about corneal topographers and the file structure in \href{../doc/topographersDataFormat.html}{\code{vignette("topographersDataFormat", package = "rPACI")}}.
#' 
#' @references Rowsey, J. James, A. E. Reynolds, and Randy Brown. 1981. "Corneal Topography: Corneascope." Archives of Ophthalmology 99 (6): 1093-1100. \href{https://doi.org/10.1001/archopht.1981.03930011093022}{DOI link}.
#' @references Pinero, D. P. 2015. "Technologies for Anatomical and Geometric Characterization of the Corneal Structure and Anterior Segment: A Review." Seminars in Ophthalmology 30 (3): 161-70. \href{https://doi.org/10.3109/08820538.2013.835844}{DOI link}.
#' @references Samapunphong, Sopit, and Dimitri Azar. 1998. "Placido and Elevation-Based Corneal Topography. A Review." Ophthalmology Clinics of North America 11 (3): 311-29. \href{https://doi.org/10.1016/S0896-1549(05)70059-6}{DOI link}.
#' @param filepath A file path to a corneal topography file exported by a Placido disk corneal topographer, in the format used by CSO.
#' @param ringsTotal The total (maximum) number of rings that may be available in the measurement (including incomplete rings or missing data; it depends on the particular device; by default 24)
#' @param pointsPerRings The number of points per rings that are digitized in the measurement (it depends on the particular device; by default 256)
#' @param ringsToUse The effective number of innermost rings to use (as long as they are complete if \code{onlyCompleteRings = TRUE}, otherwise it will be the actual number of complete rings; by default 15)
#' @param onlyCompleteRings A boolean value indicating whether to use only rings with complete data or not (by default, TRUE)
#' @param NAvalues A numerical value or vector indicating how NA values are codified in the file (by default c(-1, -1000))
#' @return A \code{data.frame} containing the corneal topography points, with columns:
#' \tabular{lll}{
#'   \code{x}   \tab\tab The X Cartesian coordinates of sampled points\cr
#'   \code{y}   \tab\tab The Y Cartesian coordinates of sampled points\cr
#'   \code{ring index}  \tab\tab Number or index of the ring from which each point is sampled\cr
#' }
#' @export
#' @examples
#' # Read the example file "N01.txt" included with rPACI (a real CSO exported file)
#' # It corresponds to a normal eye
#' datasetN = readCSO(system.file("extdata","N01.txt", package="rPACI"))
#' 
#' # Read the example file "K03.txt" included with rPACI (a real CSO exported file) 
#' # It corresponds to a keratoconic eye
#' datasetK = readCSO(system.file("extdata","K03.txt", package="rPACI"))
readCSO <- function(filepath, ringsTotal = 24, pointsPerRing = 256, ringsToUse = 15, onlyCompleteRings = TRUE, NAvalues=c(-1,-1000)) {
  
  if (!file.exists(filepath)) {
    stop("The specified file does not exist or the path is invalid.")
  }
  
  if (ringsToUse>ringsTotal) {
    stop("Number of rings to use must be less or equal than the total number of rings.")  
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
  result = data.frame(matrix(NA, nrow = lastDataIndex, ncol = 0))
  result["x"] = radii[1:lastDataIndex] * cos(angles[1:lastDataIndex])
  result["y"] = radii[1:lastDataIndex] * sin(angles[1:lastDataIndex])
  result["ring index"] = kronecker(1:ringsActual,rep(1,pointsPerRing))
  
  colnames(result) = c("x","y","ring index")
  return(result)
}



#' Read a corneal topography file in rPACI format
#'
#' This function is intended to read external files with a corneal topography that have been previously exported
#' by rPACI, using \link[rPACI]{writerPACI}. The file should have the format used by rPACI, i.e., a list with three 
#' columns (x and y coordinates of each point, and its ring index) and a row per data point
#' In general, we recommend to use the more general wrapper function \link[rPACI]{readFile} to read any file format.
#' 
#' A corneal topographer is an ophthalmic clinical device that obtains measurements in the cornea (the anterior
#' part of the eye). A Placido disk corneal topographer makes use of the Placido disk (see references and the 
#' vignette linked below), which  produce a circular pattern of measurement nodes.
#' 
#' The \link[rPACI]{readrPACI} function is able to read a Placido disk corneal topography from a file that has been
#' exported previously by \code{rPACI} using the function \link[rPACI]{writerPACI}. The dataset may have been obtained
#' reading data from a file in other format (e.g. using \link[rPACI]{readFile} or \link[rPACI]{readCSO}), or by 
#' simulation using \link[rPACI]{simulateData}.  
#' 
#' This file format consists of an optional header of any length (its size is automatically detected) and afterwards, 
#' three separated columns (x and y coordinates of each point, and its ring index) and a row per data point.
#' 
#' This function produces a \code{data.frame} in the usual format used by \code{rPACI}, i.e., a list with three 
#' columns (x and y coordinates of each point, and its ring index) and a row per data point, according to the 
#' function parameters (by default, 24*256 = 6144 rows or data points).
#' 
#' See more details about corneal topographers and the file structure in \href{../doc/topographersDataFormat.html}{\code{vignette("topographersDataFormat", package = "rPACI")}}.
#'  
#' @references Rowsey, J. James, A. E. Reynolds, and Randy Brown. 1981. "Corneal Topography: Corneascope." Archives of Ophthalmology 99 (6): 1093-1100. \href{https://doi.org/10.1001/archopht.1981.03930011093022}{DOI link}.
#' @references Pinero, D. P. 2015. "Technologies for Anatomical and Geometric Characterization of the Corneal Structure and Anterior Segment: A Review." Seminars in Ophthalmology 30 (3): 161-70. \href{https://doi.org/10.3109/08820538.2013.835844}{DOI link}.
#' @references Samapunphong, Sopit, and Dimitri Azar. 1998. "Placido and Elevation-Based Corneal Topography. A Review." Ophthalmology Clinics of North America 11 (3): 311-29. \href{https://doi.org/10.1016/S0896-1549(05)70059-6}{DOI link}.
#' @param filepath A file path to a corneal topography dataset exported by a Placido disk corneal topographer.
#' @param sep The character used as column separator in the file (by default, ",").
#' @return A \code{data.frame} containing the corneal topography points, with columns:
#' \tabular{lll}{
#'   \code{x}   \tab\tab The X Cartesian coordinates of the points\cr
#'   \code{y}   \tab\tab The Y Cartesian coordinates of the points\cr
#'   \code{ring index}  \tab\tab Number or index of the ring to which each point belongs\cr
#' }
#' The resulting \code{data.frame} may also include in its \code{Parameters} attribute (\code{attr(result,'Parameters')}) the list of parameters used for the simulation (only if it was generated with \link[rPACI]{simulateData} and saved with \link[rPACI]{writerPACI}). 
#' @export
#' @examples
#' # A dataset that was read from a corneal topographer file was later saved in the rPACI format.
#' # It can be read with:
#' dataset1 = readrPACI(system.file("extdata","ds1.txt", package="rPACI"))
#' 
#  # Read another dataset (simulated and saved in the rPACI format; it includes the simulation parameters in the header): 
#' dataset2 = readrPACI(system.file("extdata","ds2.txt", package="rPACI"))
readrPACI <- function(filepath, sep = ",") {
  
  if (!file.exists(filepath)) {
    stop("The specified file does not exist or the path is invalid.")
  }
  
  # Open file and extract its lines
  connection=file(filepath,open="r")
  close(connection)
  connection=file(filepath,open="r")
  file_lines=readLines(connection, warn=FALSE)
  close(connection)
  
  
  # Identify and discard the file header (lines at the beginning of the file
  # that are not convertible to vectors)
  linesToDrop = 0
  headerEnd = FALSE
  j = 1
  while(linesToDrop < length(file_lines) && headerEnd == FALSE) {
    lineNumeric = suppressWarnings(as.numeric(strsplit(file_lines[j], split = sep)[[1]]))
    if (any(is.na(lineNumeric)) || !is.numeric(lineNumeric) || length(lineNumeric)!=3) {
      linesToDrop = linesToDrop+1
    }
    else {
      headerEnd = TRUE
    }
    j=j+1
  }
  
  if (linesToDrop == length(file_lines)) {
    stop("The dataset could not be read properly. Please revise its format.")
  }  
  
  # Save separately the lines that are convertible to numbers
  numeric_lines = file_lines
  if (linesToDrop>0) {
    numeric_lines = file_lines[-c(1:linesToDrop)]
  }
  
  res = sapply(numeric_lines, strsplit, split = sep, USE.NAMES = F)
  res2  = lapply(res, as.numeric)
  result = as.data.frame(do.call("rbind", res2))

  
  if (!checkDataset(result)) {
    stop("The dataset could not be read properly. Please revise its format.")
  }
  colnames(result) = c("x","y","ring index")
  
  
  #******** save parameters as attributes *********
  # Attribute names of simulateData function
  args = c("rings", "pointsPerRing", "diameter", "ringRadiiPerturbation", 
           "maximumMireDisplacement", "mireDisplacementAngle", "mireDisplacementPerturbation",
           "ellipticAxesRatio", "ellipticRotation", "overallNoise", "seed")
  
  # file header
  header = file_lines[c(1:linesToDrop)]
  
  index = sapply(args, grep, x = header, simplify = T)

  index = unlist(index)

  if(length(index)>0){
    param = header[index]
    param_split = strsplit(param, " = ")
    
    # get parameter values
    param_values = lapply(param_split, "[[",2)
    param_values = lapply(param_values, as.numeric)
    
    # get parameter names 
    param_names = lapply(param_split, "[[",1)
    
    names(param_values) = param_names
    
    attr(result, 'Parameters') = param_values
  }

  
  return(result)
}



# Check that a dataset fits the format required by rPACI
#' @importFrom stats aggregate
checkDataset <- function(dataset){
  # check if the number of columns equals 3
  if(ncol(dataset) != 3){
    stop('The dataset must contain 3 columns: x, y (cartesian coordinates of data points) and ring index (1, 2, ...).')
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
  
  return(TRUE)
}

#' Read a corneal topography file in any available format (recommended by default)
#'
#' This function is a general wrapper function, able to read corneal topography files in any format supported 
#' by \code{rPACI}. Internally, it analyzes the file and detects its format, and then it uses the appropriate 
#' specific reading function (by now, two are available: \link[rPACI]{readCSO} and \link[rPACI]{readrPACI}). 
#' This is the reading function recommended by default, as it is able to read any supported file format.
#' 
#' A corneal topographer is an ophthalmic clinical device that obtains measurements in the cornea (the anterior
#' part of the eye). A Placido disk corneal topographer makes use of the Placido disk (see references and the 
#' vignette linked below), which  produce a circular pattern of measurement nodes.
#' 
#' This function internally determines the format of the specified file, and then it applies either \link[rPACI]{readCSO}
#' or \link[rPACI]{readrPACI} if possible, or else it throws an error (if none can be applied, when the file 
#' format does not fit any of these two available formats). All this process is transparent to the user, so that 
#' using \code{readFile} with one file type or another is done in the same way, and it produces the same results. 
#' The \code{readFile} function propagates its input parameters. See those functions' documentation for more information
#' about their arguments. 
#' 
#' This function produces a \code{data.frame} in the usual format used by \code{rPACI}, i.e., a list with three 
#' columns (x and y coordinates of each point, and its ring index) and a row per data point, according to the 
#' function parameters (by default, 24*256 = 6144 rows or data points).
#' 
#' See more details about corneal topographers and the file structure in \href{../doc/topographersDataFormat.html}{\code{vignette("topographersDataFormat", package = "rPACI")}}.
#'  
#' @references Rowsey, J. James, A. E. Reynolds, and Randy Brown. 1981. "Corneal Topography: Corneascope." Archives of Ophthalmology 99 (6): 1093-1100. \href{https://doi.org/10.1001/archopht.1981.03930011093022}{DOI link}.
#' @references Pinero, D. P. 2015. "Technologies for Anatomical and Geometric Characterization of the Corneal Structure and Anterior Segment: A Review." Seminars in Ophthalmology 30 (3): 161-70. \href{https://doi.org/10.3109/08820538.2013.835844}{DOI link}.
#' @references Samapunphong, Sopit, and Dimitri Azar. 1998. "Placido and Elevation-Based Corneal Topography. A Review." Ophthalmology Clinics of North America 11 (3): 311-29. \href{https://doi.org/10.1016/S0896-1549(05)70059-6}{DOI link}.
#' @param filepath A file path to a corneal topography file in any supported format.
#' @param sep The character used as column separator in the file (by default, ",").
#' @param ... Optional arguments of any of the reading functions.
#' @return A \code{data.frame} containing the corneal topography points, with columns:
#' \tabular{lll}{
#'   \code{x}   \tab\tab The X Cartesian coordinates of the points\cr
#'   \code{y}   \tab\tab The Y Cartesian coordinates of the points\cr
#'   \code{ring index}  \tab\tab Number or index of the ring to which each point belongs\cr
#' }
#' The resulting \code{data.frame} may also include in its \code{Parameters} attribute (\code{attr(result,'Parameters')}) the list of parameters used for the simulation (only if it was generated with \code{simulateData} and saved with \code{writerPACI}). 
#' @export
#' @examples
#' # Read the example file "N01.txt" (the file is in the CSO file format)
#' dataset1 = readFile(system.file("extdata","N01.txt", package="rPACI"))
#' 
#  # Read another example file "ds2.txt" (this file is in the rPACI file format)
#' dataset2 = readFile(system.file("extdata","ds2.txt", package="rPACI"))

readFile <- function(filepath, sep = ",", ...){
 
  # Open file and extract its lines
  connection=file(filepath,open="r")
  close(connection)
  connection=file(filepath,open="r")
  file_lines=readLines(connection, warn=FALSE)
  close(connection)
  
  
  linesToDrop = 0
  headerEnd = FALSE
  j = 1

  while(linesToDrop < length(file_lines) && headerEnd == FALSE) {
    lineNumeric1 = suppressWarnings(as.numeric(gsub(",",".", file_lines)[j]))
    lineNumeric2 = suppressWarnings(as.numeric(strsplit(file_lines[j], split=sep)[[1]]))
    
    
    if ((any(is.na(lineNumeric2)) || !is.numeric(lineNumeric2) || length(lineNumeric2)!=3) && is.na(lineNumeric1)) {
      linesToDrop = linesToDrop+1
    }
    else {
      headerEnd = TRUE
    }
    j=j+1
  }
  
  
  if (linesToDrop == length(file_lines)) {
    stop("The dataset could not be read properly. Please revise its format.")
  }  
  
  # Save separately the lines that are convertible to numbers
  numeric_lines = file_lines
  if (linesToDrop>0) {
    numeric_lines = file_lines[-c(1:linesToDrop)]
  }
  
  # check which read function should be used
  row1 = strsplit(numeric_lines[1], split = sep)[[1]]
  if(length(row1)== 3){
    df = readrPACI(filepath = filepath, sep = sep)
  }else if(length(row1)<3){
    df = readCSO(filepath = filepath, ...)
  }else{
    stop("The dataset could not be read properly. Please revise its format.")
  }
  
  return(df)
}
