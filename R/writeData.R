#' Write a dataset in the format used by rPACI
#' 
#' Write a corneal topography dataset in the format used by rPACI.
#' This format consists of an optional header of any length (its size is automatically detected) and afterwards, 
#' three columns (x and y coordinates of each point and its ring index) and a row per data point.
#' This format is returned by the functions \link[rPACI]{readCSO} and \link[rPACI]{simulateData}.
#' @param df A \code{data.frame} containing the corneal topography points, with columns:
#' \tabular{lll}{
#'   \code{x}   \tab\tab The X Cartesian coordinates of the points\cr
#'   \code{y}   \tab\tab The Y Cartesian coordinates of the points\cr
#'   \code{ring index}  \tab\tab Number or index of the ring to which each point belongs\cr
#' }
#' @param filename Character string naming a file (including extension).
#' @param sep The field separator string. By default, ','.
#' @importFrom utils write.table
writeDataset <- function(df, filename, sep = ","){
  # Get parameters (if param is not NULL, df comes from the simulateData function)
  param = attr(df, 'Parameters')
  
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
    write.table(df, file = filename, append = T, sep = sep, row.names = F)
  })
  
}
