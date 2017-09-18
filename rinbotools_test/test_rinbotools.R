library(rinbotools)
library(tools)


# C:\Luc\Personal\gbif_inbo\first_exploration_of_rgbif
# partial test code for function
path ='C:/Luc/Personal/gbif_inbo/first_exploration_of_rgbif/gbif_example.csv'
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

GBIFbackboneCSV_add_ursf(fpathIn, speciesKey= FALSE)

# test 3: custom name of file 

