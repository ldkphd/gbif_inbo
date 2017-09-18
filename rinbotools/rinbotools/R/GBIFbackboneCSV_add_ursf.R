

# function
GBIFbackboneCSV_add_ursf <-
function(fpathIn, fpathOut = NULL, fileOut = TRUE, 
                                     speciesKey= TRUE, 
                                     verbose = TRUE, sep=',') {
  
  # Add a standard set of attributes from the GBIF backbone to a csv file.
  #
  # Args:
  #   fpath_in:   File path (file_path/file_name including extension)
  #   fpath_out:  File path (file_path/file_name including extension). Default is NULL. 
  #   file_out:   Write resulting dataframe to new csv file. Default is TRUE. 
  #   speciesKey: add speciesKey if TRUE, if not, not. Default is TRUE.
  #   verbose:    If TRUE, prints sample covariance; if not, not. Default is TRUE.
  #   sep:        enables the use of other seperators ('\t' for tab). Default is ','
  #
  # Returns:
  #   1. updated dataframe for direct use in R workspace
  #   2. output csv file as user defined name and location or in same directory with a default name change ('_ursf')
  
  if (!requireNamespace("rgbif", quietly = TRUE)) {
    stop("rgbif package needed for this function to work. Please install it.",
      call. = FALSE)
	  }
  if (!requireNamespace("tools", quietly = TRUE)) {
    stop("tools package needed for this function to work. Please install it.",
      call. = FALSE)
	  }
  # load required package
  library(rgbif)
  library(rgbif)

  # load the csv file
  dfIn <- read.table(file=fpathIn, header=TRUE, sep=",")
  # Init output df
  dfOut <- dfIn
  
  # get keys for all 
  splist <- t(dfOut$scientificName)[1,]
  keys <- sapply(splist, function(x) name_backbone(name=x)$speciesKey, USE.NAMES=FALSE)
  
  # add speciedKey if requested
  if (speciesKey) {
	dfOut$speciesKey   <- sapply(splist, function(x) name_backbone(name=x)$speciesKey, USE.NAMES=FALSE)
	}
  
  # add the additional columns (fixed to specific job)
  dfOut$usageKey     <- sapply(splist, function(x) name_backbone(name=x)$usageKey, USE.NAMES=FALSE)
  dfOut$rank         <- sapply(splist, function(x) name_backbone(name=x)$rank, USE.NAMES=FALSE)
  dfOut$status       <- sapply(splist, function(x) name_backbone(name=x)$status, USE.NAMES=FALSE)
  dfOut$family       <- sapply(splist, function(x) name_backbone(name=x)$family, USE.NAMES=FALSE)
  
  # risk of overwriting custom data
  # TODO(ldekonin): maybe prevent overwrite of duplicate columns and add matching warning at function output
  
  # output to csv
  if(is.null(fpathOut) & fileOut) {
    new_basepath = file.path(dirname(fpathIn), paste(file_path_sans_ext(basename(fpathIn)),'_ursf', '.', file_ext(basename(fpathIn)), sep=''), fsep = .Platform$file.sep)
    write.table(dfOut, file = new_basepath, sep = sep)
    }
  
  
  # Feedback
  if (verbose)
    cat("Added ursf to CSV for", nrow(dfOut), "rows.\n", sep = " ")
  
  return(dfOut)
}
