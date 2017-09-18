# load libraries
library(rinbotools)
library(tools)

# test code for function

# download example file
# set path to file
path ='C:/.../gbif_example.csv'
fpathIn = path
fpathOut = NULL
fileOut = TRUE
speciesKey= TRUE
verbose = FALSE
sep = ','

# test 1
GBIFbackboneCSV_add_ursf(fpathIn)

# test 2
GBIFbackboneCSV_add_ursf(fpathIn, speciesKey= FALSE)

# test 3: test verbose
result = GBIFbackboneCSV_add_ursf(fpathIn, verbose = FALSE)

