#' Analysis of a single corneal topography file
#'
#' Analyze a corneal topography file. This function combines together the three operations performed
#' by the functions \link[rPACI]{readFile}, \link[rPACI]{computePlacidoIndices},
#' and \link[rPACI]{plotSingleCornea}.
#' The result is a \code{data.frame} in the same format given by \link[rPACI]{computePlacidoIndices}.
#'  
#' More details about supported file formats can be found in 
#' \href{../doc/topographersDataFormat.html}{\code{vignette("topographersDataFormat", package = "rPACI")}}, 
#' and about using \code{rPACI} in \href{../doc/packageUsage.html}{\code{vignette("packageUsage", package = "rPACI")}}.
#' 
#' @param path A corneal topography file, as exported by a Placido disk corneal topographer.
#' @param drawplot An optional parameter indicating whether a plot of results should be displayed or not (by default, \code{TRUE}).
#' @return A \code{data.frame} containing the Placido irregularity indices as well as the diagnose, with a single row and columns:
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
#' @export
#' @examples
#' # Read the file, compute the indices and show results in one step with:
#' results = analyzeFile(system.file("extdata","N01.txt", package="rPACI"))
#' 
#' # The previous command is equivalent to:
#' dataset = readFile(system.file("extdata","N01.txt", package="rPACI"))
#' results = computePlacidoIndices(dataset)
#' # If drawplot=TRUE, then it also performs:
#' plotSingleCornea(dataset, results)
analyzeFile <- function(path, drawplot = TRUE) {
  
  data = readFile(path)
  
  result = computePlacidoIndices(data)
  
  if(drawplot) {
    plotSingleCornea(data, result, path)
  }
  
  return(result)
}

#' Analysis of all corneal topography files in a folder
#'
#' This function analyze all corneal topography files that are stored in a common folder. It is 
#' equivalent to use \link[rPACI]{analyzeFile} on each file in the folder, and then binding the results. 
#' 
#' This function assumes all files in the folder that have the extension given by the argument \code{fileExtension} 
#' ("txt", by default) are corneal topography files and are to be processed. 
#' The result is a \code{data.frame} in the same format yield by \link[rPACI]{computePlacidoIndices} or \link[rPACI]{analyzeFile},
#' but with as many rows as matching files were found in the folder.
#' 
#' More details about supported file formats can be found in 
#' \href{../doc/topographersDataFormat.html}{\code{vignette("topographersDataFormat", package = "rPACI")}}, 
#' and about using \code{rPACI} in \href{../doc/packageUsage.html}{\code{vignette("packageUsage", package = "rPACI")}}.
#' 
#' @param path The path of a folder which contains corneal topography files, as exported by Placido disks corneal topographers.
#' @param fileExtension The file extension of the corneal topography files in the folder ("txt" by default).
#' @param individualPlots An optional logical parameter (by default, FALSE) indicating whether the plot for each file should be displayed or not.
#' @param summaryPlot An optional logical parameter (by default, FALSE) indicating whether a summary plot should be displayed or not.
#' @importFrom graphics barplot boxplot par plot rect text
#' @importFrom grDevices rgb
#' @return A \code{data.frame} containing the Placido irregularity indices as well as the diagnose, with as many rows as data files in the folder, and columns:
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
#' @export
#' @examples
#' # This analyzes together all the corneal topography example files included in rPACI:
#' \dontrun{
#' analyzeFolder(system.file("extdata",package="rPACI"))
#' }
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
#' Analyze a corneal topography dataset (a \code{data.frame} in the rPACI format). This function combines the two
#' operations of functions \link[rPACI]{computePlacidoIndices} and \link[rPACI]{plotSingleCornea}. It assumes a valid 
#' rPACI \code{data.set} is already available on memory.
#' 
#' The dataset is checked to verify it matches the rPACI format: it must contain 3 columns: x, y (with the X and Y Cartesian
#' coordinates of data points) and ring index (1, 2, ...). The ring index column must contain positive integer numbers. 
#' The dataset must not contain \code{NA} values. Finally, all the rings must contain the same number of data points.
#' 
#' More details about supported file formats can be found in 
#' \href{../doc/topographersDataFormat.html}{\code{vignette("topographersDataFormat", package = "rPACI")}}, 
#' and about using \code{rPACI} in \href{../doc/packageUsage.html}{\code{vignette("packageUsage", package = "rPACI")}}.
#' 
#' @param dataset A corneal topography dataset, loaded from a file using the function \link[rPACI]{readFile}, simulated using \link[rPACI]{simulateData}, or by other ways (as long as it meets the dataset requirements).
#' @param drawplot An optional parameter indicating whether a plot of results should be displayed or not (by default, \code{TRUE}).
#' @export
#' @examples
#' # Generate a sample dataset
#' dataset = simulateData(rings = 15, ringRadiiPerturbation = 0.7)
#' 
#' # Analyze this dataset:
#' analyzeDataset(dataset)
analyzeDataset <- function(dataset, drawplot = TRUE) {
  
  checkDataset(dataset)
  
  result = computePlacidoIndices(dataset)
  
  if(drawplot) {
    plotSingleCornea(dataset, result)
  }
  
  return(result)
}


#' Analysis of repeated measures of the same patient over time
#' 
#' Analyze the evolution of a patient over time. This function returns the Placido irregularity indices per time step
#' and two temporal plots.
#'
#' If the data are loaded from a folder, it will assume that the temporal arrangement is the alphabetical order
#' of the files' name. Therefore, it is advised to use proper file names, for instance using this date format: 
#' 'YYYY-MM-DD.txt'.
#' 
#' Moreover, the folder should contain data measures of just one patient, since the function will read all the files
#' (with the given extension) of the specified folder. On the other hand, if the data are stored in a list, it will
#' be assumed that the temporal order corresponds with the index of the dataset in the list.
#' 
#' More details about supported file formats can be found in 
#' \href{../doc/topographersDataFormat.html}{\code{vignette("topographersDataFormat", package = "rPACI")}}, 
#' and about using \code{rPACI} in \href{../doc/packageUsage.html}{\code{vignette("packageUsage", package = "rPACI")}}.
#' 
#' @param data Either 1) the path of a folder that contains corneal topography files, 
#' as exported by Placido disks corneal topographers, or 2) a list containing properly formatted data
#' (loaded from a file using the function \link[rPACI]{readFile}, 
#' simulated using \link[rPACI]{simulateData}, or by other ways, as long as it meets the dataset requirements).
#' @param fileExtension If data is a path, specify the file extension of the corneal topography files 
#' in the folder. It assumes all files with the given extension are corneal topography files of a single patient 
#' (by default, "txt").
#' @details 
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
#' analyzeEvolution(data)
#' 
#' # EXAMPLE 2
#' # Specify a folder path to analyze a patient's evolution over time
#' analyzeEvolution(paste(system.file("extdata", package="rPACI"),"/temporal/",sep=""))
analyzeEvolution <- function(data, fileExtension = "txt") {
  
  # check whether 'data' is character path or list
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
