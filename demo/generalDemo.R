require("rPACI")

##### EXAMPLE 1: NORMAL EYE ######
# Read an example file with the corneal topography of a normal eye, from a file exported by a typical commercial Placido disk topographer
dataset_N = readCSO(system.file("extdata","N01.txt", package="rPACI"))

# Now compute the Placido irregularity indices in Ramos-Lopez et al. (2013), <doi:10.1097/OPX.0b013e3182843f2a>
results_N = computePlacidoIndices(dataset_N)

# Show the diagnose for this dataset
results_N$Diagnose

# Or show the values of individual indices
results_N$GLPI
results_N$PI_1
results_N$AR_1

# Plot the results for the normal eye
plotSingleCornea(dataset_N, results_N, filename = "N01.txt")

# Alternatively, these three operations can be performed with the function analyzeFile:
results_N = analyzeFile(system.file("extdata","N01.txt", package="rPACI"), drawplot=TRUE)


##### EXAMPLE 2: KERATOCONIC EYE ######
# Now analyze another example file with the corneal topography of an eye with keratoconus.
results_K = analyzeFile(system.file("extdata","K01.txt", package="rPACI"), drawplot=TRUE)

# Show the diagnose for this dataset
results_K$Diagnose


##### EXAMPLE 3: SUSPECT EYE ######
# Now analyze another example file with the corneal topography of a suspect eye.
results_S = analyzeFile(system.file("extdata","S01.txt", package="rPACI"), drawplot=TRUE)

# Show the diagnose for this dataset
results_S$Diagnose


##### EXAMPLE 4: ANALYZE MULTIPLE FILES IN A FOLDER ######
# Multiple files in a common folder can be analyzed at a time using the function 'analyzeFolder':
resultsAll = analyzeFolder(system.file("extdata", package="rPACI"), individualPlots = FALSE, summaryPlot = T)

# Show the diagnose for each analyzed file
resultsAll[,c(13,1)]

