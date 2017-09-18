\name{GBIFbackboneCSV_add_ursf}
\alias{GBIFbackboneCSV_add_ursf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GBIFbackboneCSV_add_ursf
}
\description{
Add standard set of attributes to csv file with gbif name_backbone list
}
\usage{
GBIFbackboneCSV_add_ursf(fpathIn, fpathOut = NULL, fileOut = TRUE, speciesKey = TRUE, verbose = TRUE, sep = ",")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fpathIn}{
filepath of input csv file, include extension 
}
  \item{fpathOut}{
optional filepath of output csv file, include extension
if not supplied, '_ursf' is added to the input filename  
}
  \item{fileOut}{
optional: FALSE == no file output, resulting dataframe available in R workspace
}
  \item{speciesKey}{
optional: TRUE == add speciesKey to the output file
}
  \item{verbose}{
return some info to user at end of function
}
  \item{sep}{
enables the use of other separators (both input and output)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
dataframe matching the loaded csv file with four standard gbif 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Luc Dekoninck
}
\note{
Just a first example of building an R package
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
gbif biodiversisty: https://www.gbif.org/
rgbif documention:  https://github.com/ropensci/rgbif
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (fpathIn, fpathOut = NULL, fileOut = TRUE, speciesKey = TRUE, 
    verbose = TRUE, sep = ",") 
{
    dfIn <- read.table(file = fpathIn, header = TRUE, sep = ",")
    dfOut <- dfIn
    splist <- t(dfOut$scientificName)[1, ]
    keys <- sapply(splist, function(x) name_backbone(name = x)$speciesKey, 
        USE.NAMES = FALSE)
    dfOut$speciesKey <- sapply(splist, function(x) name_backbone(name = x)$speciesKey, 
        USE.NAMES = FALSE)
    dfOut$usageKey <- sapply(splist, function(x) name_backbone(name = x)$usageKey, 
        USE.NAMES = FALSE)
    dfOut$rank <- sapply(splist, function(x) name_backbone(name = x)$rank, 
        USE.NAMES = FALSE)
    dfOut$status <- sapply(splist, function(x) name_backbone(name = x)$status, 
        USE.NAMES = FALSE)
    dfOut$family <- sapply(splist, function(x) name_backbone(name = x)$family, 
        USE.NAMES = FALSE)
    if (is.null(fpathOut) & fileOut) {
        new_basepath = file.path(dirname(fpathIn), paste(file_path_sans_ext(basename(fpathIn)), 
            "_ursf", ".", file_ext(basename(fpathIn)), sep = ""), 
            fsep = .Platform$file.sep)
        write.table(dfOut, file = new_basepath, sep = sep)
    }
    if (verbose) 
        cat("Added ursf to CSV for", nrow(dfOut), "rows.\n", 
            sep = " ")
    return(dfOut)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
