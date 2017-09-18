library(rgbif)


# -----------------------------------------------------------------------------
# exploring rgbif functionality
# example from https://www.rdocumentation.org/packages/rgbif/versions/0.9.8
key <- name_backbone(name='Helianthus annuus', kingdom='plants')$speciesKey;key

# get multiple keys
splist <- c('Accipiter erythronemius', 'Junco hyemalis', 'Aix sponsa')
keys <- sapply(splist, function(x) name_backbone(name=x)$speciesKey, USE.NAMES=FALSE);keys

# get all attributes of 
all_attributes <- name_backbone(name='Helianthus annuus', kingdom='plants')
names(all_attributes)

#[1] "usageKey"       "scientificName" "canonicalName"  "rank"           "status"         "confidence"    
#[7] "matchType"      "kingdom"        "phylum"         "order"          "family"         "genus"         
#[13] "species"        "kingdomKey"     "phylumKey"      "classKey"       "orderKey"       "familyKey"     
#[19] "genusKey"       "speciesKey"     "synonym"        "class"   

# -----------------------------------------------------------------------------
# Set the working directory to file location
setwd('C:/Users/LucAdmin/Documents/GitHub/gbif_inbo/first_exploration_of_rgbif')

# open csv file
myDataIn <- read.csv(file="gbif_example.csv", header=TRUE, sep=",")
# MyDataIn <- read.csv(file="gbif_example_typos.csv", header=TRUE, sep=",")  # just to check what will happen if the scientificName does not exist...
# Impressed: keeps running...
# MyDataIn <- read.csv(file="gbif_example_typos_extreme.csv", header=TRUE, sep=",")  # Got the picture


# just have a look
names(myDataIn)
#fix(myDataIn)

# get multiple keys based on csv file
splist <- t(myDataIn$scientificName)[1,]
keys <- sapply(splist, function(x) name_backbone(name=x)$speciesKey, USE.NAMES=FALSE);keys



# -----------------------------------------------------------------------------
# manual test: add columns

# init output dataframe
myDataOut <- myDataIn

splist <- t(myDataIn$scientificName)[1,]
keys <- sapply(splist, function(x) name_backbone(name=x)$speciesKey, USE.NAMES=FALSE);keys

myDataOut$speciesKey   <- sapply(splist, function(x) name_backbone(name=x)$speciesKey, USE.NAMES=FALSE)
myDataOut$usageKey     <- sapply(splist, function(x) name_backbone(name=x)$usageKey, USE.NAMES=FALSE)
myDataOut$rank         <- sapply(splist, function(x) name_backbone(name=x)$rank, USE.NAMES=FALSE)
myDataOut$status       <- sapply(splist, function(x) name_backbone(name=x)$status, USE.NAMES=FALSE)
myDataOut$family       <- sapply(splist, function(x) name_backbone(name=x)$family, USE.NAMES=FALSE)

# test issue on duplicate columns... 
myDataOut$kingdom       <- sapply(splist, function(x) name_backbone(name=x)$kingdom, USE.NAMES=FALSE)
# risk of overwriting custom data in the input file if user accidentely uses column names in name_backbone attribute list...

# output to csv
write.csv(myDataOut, file = "mydata.csv")

#fix(myDataOut)


# ---------------------------------------------------------------------------------------------------------------
# Path and file name
library(tools)
path ='C:/Users/LucAdmin/Documents/GitHub/gbif_inbo/first_exploration_of_rgbif/gbif_example.csv'
basename(path)
dirname(path)
file_ext(basename(path))
file_path_sans_ext(basename(path))
file.path(dirname(path), basename(path), fsep = .Platform$file.sep)

new_basepath = file.path(dirname(path), paste(file_path_sans_ext(basename(path)),'_ursf', '.', file_ext(basename(path)), sep=''), fsep = .Platform$file.sep)

# ---------------------------------------------------------------------------------------------------------------
# now write this as a clean function

# version case specific
# choosing a functon name is not trivial... communication with users and knwoledge of in-house standards required
# naming convention
# Why not add the speciesKey? Looks crucial to me... add as an otion
# return updated dataframe by function? Yes...
# build standard outfile_name if none provided? Yes

# required libraries

library(tools)
library(rgbif)


GBIFbackboneCSV_add_ursf <- function(fpathIn, fpathOut = NULL, fileOut = TRUE, 
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
  
  dfIn <- read.table(file=fpathIn, header=TRUE, sep=",")
  # Init output df
  dfOut <- dfIn
  
  # get keys for all 
  splist <- t(dfOut$scientificName)[1,]
  keys <- sapply(splist, function(x) name_backbone(name=x)$speciesKey, USE.NAMES=FALSE)
  
  # add speciedKey if requested
  
  dfOut$speciesKey   <- sapply(splist, function(x) name_backbone(name=x)$speciesKey, USE.NAMES=FALSE)
  
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
  
  
  # Error handling
  if (verbose)
    cat("Added ursf to CSV for", nrow(dfOut), "rows.\n", sep = " ")
#    if (length(na.omit(dfOut$speciesKey)) != length(dfOut$speciesKey))
#      cat("Success rate", nrow(na.omit(dfOut))/nrow(dfOut), ", ", nrow(dfOut) - nrow(na.omit(dfOut)), "errors.\n", sep = " ")
  
  return(dfOut)
}


# partial test code for function
path ='C:/Users/LucAdmin/Documents/GitHub/gbif_inbo/first_exploration_of_rgbif/gbif_example.csv'
fpathIn = path
fpathOut = NULL
fileOut = TRUE
speciesKey= TRUE
verbose = FALSE
sep = ','

# test 1
GBIFbackboneCSV_add_ursf(fpathIn)
# test 2: test verbose
result = GBIFbackboneCSV_add_ursf(fpathIn)
# test 3: custom name of file 


path ='C:/Users/LucAdmin/Documents/GitHub/gbif_inbo/first_exploration_of_rgbif/gbif_example_typos_extreme.csv'
fpathIn = path
verbose = TRUE
GBIFbackboneCSV_add_ursf(fpathIn, fpathOut = file.path(dirname(fpathIn), 'test_file_name.csv', fsep = .Platform$file.sep))



# ---------------------------------------------------------------------------------------------------------------
# version extended
# build function that can add a custom list of attributes
# add 'frequently used variants' of specific cases of the general function (of which the case is a nice example)
# how to control the order of columns in the output file ??? Follow logical order in the attributes ???  --> independent column order function?
# Sorting? Maybe use same approach as ordering --> independent function to accomodate sorting... sort(order(GBIFbackboneCSV())) 

# only directly from CSV ? maybe adjust appraoch
# Why not split function for use on available dataframe in R workspace
# Which bring me to the question: how to handle duplicate columns, when some are already available in the dataframe...   Checked above, needs attention later






# ---------------------------------------------------------------------------------------------------------------
# put this into a R package
# https://www.stt.msu.edu/users/hengwang/R%20package%20tutorial.pdf
# 'our' local INBO_Rtools package


# ---------------------------------------------------------------------------------------------------------------