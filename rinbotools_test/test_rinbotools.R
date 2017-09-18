# load required libraries
library(rinbotools)
library(tools)

# download example file
# set path to file
path ='C:/Luc/Personal/mygithub/gbif_inbo/rinbotools_test/gbif_example.csv'

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

