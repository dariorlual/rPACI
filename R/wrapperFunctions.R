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
    stop("The specified directory does not exist or is invalid.")
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

#' Analysis of a single corneal topography dataset
#'
#' Analyze a corneal topography dataset This function combines the three operations of functions \link[rPACI]{readCornealTopography}, \link[rPACI]{computePlacidoIndices} and \link[rPACI]{plotSingleCornea}.
#' @param path A corneal topography datatset, loaded from a file using the function \link[rPACI]{readCornealTopography}, simulated using \link[rPACI]{simulateData}, or by other ways (as long as it meets the dataset requirements).
#' @param drawplot An optional parameter indicating whether a plot of results should be displayed or not.
#' @export
#' @details The dataset must contain 3 columns: x, y (with the X and Y Cartesian coordinates of data points) and ring index (1, 2, …). 
#' The ring index column must contain integer numbers. 
#' The dataset must not contain NA values. 
#' Finally, all the rings must contain the same number of data points.
#' @examples
#' dataset = simulateData(rings = 15, ringRadiiPerturbation = 0.7)
#' analyzeDataset(dataset)
analyzeDataset <- function(dataset, drawplot=TRUE) {
  
  checkDataset(dataset)
  
  result = computePlacidoIndices(dataset)
  
  if(drawplot) {
    plotSingleCornea(dataset, result)
  }
  
  return(result)
}


#' Analysis of repeated measures of the same patient over time
#' Analyze the evolution of a patient over time. This function returns the Placido irregularity indices per time step
#' and two temporal plots.
#'
#' @param data Either 1) the path of a folder that contains corneal topography files, 
#' as exported by Placido disks corneal topographers, or 2) a list containing properly formatted data
#' (loaded from a file using the function \link[rPACI]{readCornealTopography}, 
#' simulated using \link[rPACI]{simulateData}, or by other ways, as long as it meets the dataset requirements).
#' @param fileExtension If data is a path, specify the file extension of the corneal topography files 
#' in the folder. It assumes all files with the given extension are corneal topography files of a single patient.
#' @details If the data are loaded from a folder, it will be assumed that the temporal arrangement is the alphabetical order of the files' name. 
#' Therefore, it is advised to use a proper file name, for instance using this date format: 'YYYY-MM-DD.txt'.
#' Moreover, the folder should contain data measures of one patient only since the function 
#' will read all the files (with the given extension) of the specified folder.
#' On the other hand, if the data are stored in a list, it will be assumed that the temporal order corresponds with the index of the dataset in the list.
#' @import ggplot2
#' @importFrom tidyr gather
#' @importFrom ggpubr ggarrange
#' @export
#' @examples
#' # EXAMPLE 1
#' # Simulate the patient's measures over time
#'  dataT1 = simulateData(rings = 12, maximumMireDisplacement = 0.15, mireDisplacementAngle = 10)
#'  dataT2 = simulateData(rings = 12, maximumMireDisplacement = 0.15, mireDisplacementAngle = 45)
#'  dataT3 = simulateData(rings = 12, maximumMireDisplacement = 0.2, mireDisplacementAngle = 50)
#'  
#' # Create a list containing the data
#' data = list(
#'  dataT1 = dataT1,
#'  dataT2 = dataT2,
#'  dataT3 = dataT3
#' )
#' 
#' # Analyze the data over time
#' analizeEvolution(data)
#' 
#' # EXAMPLE 2
#' # Specify a folder path to analyze a patient's evolution over time
#' analizeEvolution(system.file("extdata", package="rPACI"), fileExtension = 'txt')

analizeEvolution <- function(data, fileExtension = NULL) {
  
  # check wheter 'data' is character path or list
  if(is.character(data)){
    # check if directory exists
    if(!dir.exists(data)) {
      stop("The specified directory does not exist or is invalid.")
    }
    if(is.null(fileExtension)){
      stop("The 'fileExtension' argument must be specified.")
    }
    pattern = paste("\\.", fileExtension, "$", sep="")
    fileList = list.files(data, pattern, full.names = T)
    res = lapply(fileList, analyzeFile, drawplot = F)
    
  }else if(is.list(data)){
    
    if(all(sapply(data, checkDataset))){
      fileList = data
      res = lapply(fileList, analyzeDataset, drawplot = F)
    }
    
  }
  
  
  df = do.call("rbind", res)
  
  df$Time = factor(1:nrow(df))
  p = plotEvolution(df)
  print(p)
  
  return(df)
  
}
