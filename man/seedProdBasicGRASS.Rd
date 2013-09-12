\name{seedProdBasicGRASS}
\alias{seedProdBasicGRASS}
\title{basic seed production based on age}
\usage{
  seedProdBasicGRASS(age, inds, output,
    seedProdPerIndFunction, initial=FALSE, overwrite=FALSE)
}
\arguments{
  \item{age}{raster layer containing the age of the species
  in each cell}

  \item{inds}{raster layer containing the number of
  individuals per cell}

  \item{output}{name of the output raster layer which will
  containing the number of newly produced seeds}

  \item{seedProdPerIndFunction}{function used to calculate
  the number of seeds based on the age in each cell}

  \item{initial}{\code{logical} specifying if the seed
  production is initial, i.e. accumulative over their life
  time}

  \item{overwrite}{\code{logical} if TRUE, \code{output}
  will be overwritten if it exists}
}
\value{
  invisibly returns name of \code{output} layer
}
\description{
  Calculating seed production based on provided function
}
\details{
  Calculate seed production per cell based on the age and
  number of individuals per cell by using a user specified
  \R function. Input and output is from GRASS raster
  layers.

  If \code{initial == TRUE}, the seed production over the
  lifetime of the plant will be returned.

  The function \code{seedProdPerInd} has to have the
  following form:

  \code{seedProdPerInd <- function(age)\{...\}}

  where

  \code{age} is the age of the plant and the function
  returns the nomber of seeds produced.

  The MASK in GRASS is \bold{not} respected.
}
\author{
  Rainer M Krug <Rainer@krugs.de>
}

