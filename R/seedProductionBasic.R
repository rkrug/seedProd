## [[file:~/Documents/Projects/AlienManagementDrakensberg/sim/packages/seedProduction/seedProduction.org::*seedProductionBasic][seedProductionBasic:1]]
##' Calculating seed production based on provided function
##'
##' Calculate seed production per cell based on the age and numebr of individuals per cell and using auser specified function. 
##' @title 
##' @param ageLayer raster layer containing the age of the species in each cell
##' @param individualsLayer raster layer containing the number of individuals per cell
##' @param seedProdPerIndFunction function used to calculate the number of seeds based on the age in each cell
##' @param seedsProducedOutputLayer name of the raster layer containing the number of newly produced seeds (will be created!)
##' @return 
##' @author Rainer M Krug
seedProductionBasic <- function(ageLayer,
                                individualsLayer,
                                seedProdPerIndFunction,
                                seedsProducedOutputLayer,
                                initial = FALSE
                                ) {
  ## calculating seedsProduced layer
  seeds <- readRAST6(
    c(
      ageLayer,
      individualsLayer
      ),
    NODATA=-1
    )
  if (initial) {
    seeds[[3]] <- seeds[[1]]
    seeds[[3]] <- 0
    while (max(seeds[[1]], na.rm=TRUE) > 0)
      {
        seeds[[3]] <- seeds[[3]] + seedProdPerIndFunction(seeds[[1]]) * seeds[[2]]
        seeds[[1]] <- seeds[[1]] - 1
        seeds[[1]][seeds[[1]] < 0] <- 0
      }
  } else {
    seeds[[3]] <- seedProdPerIndFunction(seeds[[1]]) * seeds[[2]]
  }
  
  mode(seeds[[3]]) <- "double"
  ## seeds@proj4string <- parameter$proj4string
  writeRAST6(
    seeds,
    seedsProducedOutputLayer,
    zcol=3,
    NODATA=-1
    )
}
## seedProductionBasic:1 ends here
