## [[file:~/Documents/Projects/AlienManagementDrakensberg/sim/packages/seedProd/seedProd.org::*seedProdBasicGRASS][seedProdBasicGRASS:1]]
##' Calculating seed production based on provided function
##'
##' Calculate seed production per cell based on the age and number of
##' individuals per cell by using a user specified \R function. Input
##' and output is from GRASS raster layers.
##'
##' If \code{initial == TRUE}, the seed production over the lifetime
##' of the plant will be returned.
##'
##' The function \code{seedProdPerInd} has to have the following form:
##'
##' \code{seedProdPerInd <- function(age)\{...\}}
##'
##' where
##'
##' \code{age} is the age of the plant and the function returns the
##' nomber of seeds produced.
##' 
##' The MASK in GRASS is \bold{not} respected.
##' 
##' @usage seedProdBasicGRASS(age, inds, output,
##' seedProdPerIndFunction, initial=FALSE, overwrite=FALSE)
##' @name seedProdBasicGRASS
##' @title basic seed production based on age
##' 
##' @param age raster layer containing the age of the species in each cell
##' @param inds raster layer containing the number of individuals per cell
##' @param output name of the output raster layer which will
##' containing the number of newly produced seeds
##' @param seedProdPerIndFunction function used to calculate the
##' number of seeds based on the age in each cell
##' @param initial \code{logical} specifying if the seed production is
##' initial, i.e. accumulative over their life time
##' @param overwrite \code{logical} if TRUE, \code{output} will be overwritten if it exists
##' 
##' @return invisibly returns name of \code{output} layer
##' @author Rainer M Krug <Rainer@@krugs.de>
##' @export
seedProdBasicGRASS <- function(
    age,
    inds,
    output,
    seedProdPerIndFunction,
    initial = FALSE,
    overwrite = FALSE
    ) {
    ## calculating seedsProduced layer
    seeds <- readRAST6(
        c(
            age,
            inds
            ),
        NODATA=-1
        )
    if (initial) {
        seeds[[3]] <- seeds[[1]]
        seeds[[3]] <- 0
        while (max(seeds[[1]], na.rm=TRUE) > 0)
            {
                seeds[[3]] <- seeds[[3]] +  seeds[[2]] * seedProdPerIndFunction( seeds[[1]] ) 
                seeds[[1]] <- seeds[[1]] - 1
                seeds[[1]][seeds[[1]] < 0] <- 0
            }
    } else {
        seeds[[3]] <-  seeds[[2]] * seedProdPerIndFunction(seeds[[1]])
    }
    
    mode(seeds[[3]]) <- "double"
    ## seeds@proj4string <- parameter$proj4string
    writeRAST6(
        seeds,
        output,
        zcol=3,
        NODATA=-1,
        overwrite = overwrite
        )
    invisible(output)
}
## seedProdBasicGRASS:1 ends here

## Local Variables:
## org-babel-tangled-file: t
## buffer-read-only: t
## eval:: (auto-revert-mode)
## End:
