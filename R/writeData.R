#' Save to disk a dataset in the format used by rPACI
#' 
#' Write to disk a corneal topography dataset in the format used by rPACI, in a plain text file.
#' The file format consists of (possibly) a header (only for data that were simulated), followed by
#' three separated columns (x and y coordinates of each point and its ring index) and a row per data point.
#' This format is returned by the functions \link[rPACI]{readFile} or \link[rPACI]{simulateData}.
#' 
#' This function writes the data from a \code{data.frame} to a plain text file. The file will possibly have
#' a header, followed by a block of three separated columns, according to the usual format used by \code{rPACI}, 
#' i.e., a list with three columns (x and y coordinates of each point, and its ring index) and a row per data point.
#' 
#' If the given \code{data.frame} (named \code{dataset}) was produced using \link[rPACI]{simulateData}, the resulting 
#' text file will also include in its header the \code{Parameters} attribute (\code{attr(dataset,'Parameters')}),
#' i.e., the list of parameters used for the simulation. 
#' 
#' A file stored with \code{writerPACI} can later be read using the general reader function \link[rPACI]{readFile}
#' or the specific reader function \link[rPACI]{readrPACI}.
#' 
#' See more details about the file structure in \href{../doc/topographersDataFormat.html}{\code{vignette("topographersDataFormat", package = "rPACI")}}, 
#' about the usage of \code{rPACI} in \href{../doc/packageUsage.html}{\code{vignette("packageUsage", package = "rPACI")}},
#' and about simulation parameters in \href{../doc/simulating.html}{\code{vignette("simulating", package = "rPACI")}}.
#' 
#' @param dataset A \code{data.frame} containing the corneal topography points, with columns:
#' \tabular{lll}{
#'   \code{x}   \tab\tab The X Cartesian coordinates of the points\cr
#'   \code{y}   \tab\tab The Y Cartesian coordinates of the points\cr
#'   \code{ring index}  \tab\tab Number or index of the ring to which each point belongs\cr
#' }
#' @param filename A character string naming a file (including the extension).
#' @param sep The field separator string (by default, ',').
#' @importFrom utils write.table
#' @export
#' @examples
#' # Simulating an elliptic dataset, with ellipses axis ratio of 0.8 and an orientation 
#' # of 45 degrees.
#' dataset = simulateData(rings = 18, pointsPerRing = 300, 
#'                        ellipticAxesRatio = 0.8, ellipticRotation = 45)
#' 
#' # Now the dataset can be saved to file using 'writerPACI' (check the working directory
#' # before saving):
#' writerPACI(dataset, "datasetFile.txt")
#' 
#' # The file will include as a header the parameters used in simulation
writerPACI <- function(dataset, filename, sep = ","){
  # Get parameters (if param is not NULL, df comes from the simulateData function)
  param = attr(dataset, 'Parameters')
  
  if(!is.null(param)){
    cat(paste('Dataset simulated by rPACI on', date()),'\n', file = filename)
    cat('==== PARAMETERS USED FOR SIMULATION ====\n', file = filename, append = T)
    for(i in 1:length(param)){
      cat(paste(names(param), "=", unlist(param))[i], sep = "\n", file = filename, append = T)
    }
    cat('========================================\n', file = filename, append = T)
  }else{
    cat('', file = filename)
  }
  
  suppressWarnings({
    write.table(dataset, file = filename, append = T, sep = sep, row.names = F)
  })
  
}
