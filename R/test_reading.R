readCornealTopography <- function(filepath, ringsTotal = 24, pointsPerRing = 256, ringsToUse = 15, onlyCompleteRings = TRUE, NAvalues=c(-1,-1000)) {
  
  if (!file.exists(filepath)) {
    stop("Error: The specified file does not exist or the path is invalid.")
  }
  
  if (ringsToUse>ringsTotal) {
    stop("Error: number of rings to use must be less or equan than the total number of rings.")  
  }

  if (round(ringsTotal)!=ringsTotal || ringsTotal<=0) {
    stop("Error: the total number of rings must be a positive integer")  
  }  
  
  if (round(pointsPerRing)!=pointsPerRing || pointsPerRing<=0) {
    stop("Error: the number of points per rings must be a positive integer")  
  }  
  
  if (round(ringsToUse)!=ringsToUse || ringsToUse<=0) {
    stop("Error: the number of rings to use must be a positive integer")  
  }  
  
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
  
  warning(paste("Ignoring the header (first", linesToDrop, "lines of file)"))
  
  #cat("Lines to drop:", linesToDrop, "\n")
  
  # if(missing(dropLines)) {
  #   firstCharacter=substr(file_lines[1],1,1)
  #   dropLines = switch(firstCharacter,
  #                      "P" = 22,
  #                      "[" = 4,
  #                      "0" = 0)
  # }
  
  #if(is.null(dropLines)) {
  #  stop("Uknown file type. Please specify the number of header lines to drop")
  #}
  
  numeric_lines = file_lines
  if (linesToDrop>0) {
    numeric_lines = file_lines[-c(1:linesToDrop)]
  }
  
  #numeric_lines = gsub(",",".", file_lines[-c(1:dropLines)])
  
  # Substitute values corresponding to NAs, according to parameter NAvalues (by default, -1 and -1000)
  for (j in NAvalues) {
    numeric_lines[numeric_lines==j] = NA
  }
   
  dataPoints = pointsPerRing * ringsTotal
  
  # Create equally-spaced angles between 0 and 2*pi radians (0 and 360 degrees)
  angles = seq(0,2*pi,length.out = (pointsPerRing+1))[1:pointsPerRing]
  angles = rep(angles,ringsTotal)
  length(angles)
  
  # The radii are obtained from the file, corresponding to the first 'dataPoints' numeric lines
  radii = as.numeric(numeric_lines[1:dataPoints])

  # If 'onlyCompleteRings', then discard whole rings starting from the first NA
  ringsActual = ringsToUse
  if (onlyCompleteRings) {
    firstNA = which(is.na(radii))[1]
    
    if(!(is.null(firstNA) || firstNA > ringsToUse*pointsPerRing)) {
      ringsActual = floor(firstNA/pointsPerRing)
    }
  }
  
  # Build the 'result' dataframe with the desired data
  lastDataIndex = (pointsPerRing*ringsActual)
  result=data.frame(matrix(NA, nrow = lastDataIndex, ncol = 0))
  result["x"] = radii[1:lastDataIndex] * cos(angles[1:lastDataIndex])
  result["y"] = radii[1:lastDataIndex] * sin(angles[1:lastDataIndex])
  result["ring index"] = kronecker(1:ringsActual,rep(1,pointsPerRing))
  
  colnames(result) = c("x","y","ring index")
  return(result)
}



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

dataK04 = readCornealTopography("K04_p.txt", ringsToUse=19)
head(dataK04)
tail(dataK04)
plot(dataK04$x,dataK04$y)
dim(dataK04)
dim(dataK04)[1]/256

dataK04 = readCornealTopography("K04_p.txt", ringsToUse=19, onlyCompleteRings=FALSE)
head(dataK04)
tail(dataK04)
plot(dataK04$x,dataK04$y)
dim(dataK04)
dim(dataK04)[1]/256

dataK04 = readCornealTopography("K04_p.txt", ringsToUse=24, onlyCompleteRings=FALSE)
head(dataK04)
tail(dataK04)
plot(dataK04$x,dataK04$y)
dim(dataK04)
dim(dataK04)[1]/256

