% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/structuredSparseMatrices.R
\name{strucMatrixTri}
\alias{strucMatrixTri}
\title{Triangular structured sparse matrix}
\usage{
strucMatrixTri(diagVals, offDiagVals, low = TRUE)
}
\arguments{
\item{diagVals}{values for the diagonal}

\item{offDiagVals}{values for the off-diagonal}

\item{low}{lower triangular?}
}
\description{
Triangular structured sparse matrix
}
\examples{
set.seed(1)
(xTri <- strucMatrixTri(rnorm(5), rnorm(choose(5, 2))))
}
\seealso{
Other strucMatrixSpecial: \code{\link{strucMatrixBlank}},
  \code{\link{strucMatrixCol}},
  \code{\link{strucMatrixCompSymm}},
  \code{\link{strucMatrixConstVarChol}},
  \code{\link{strucMatrixCorFactor}},
  \code{\link{strucMatrixCorMatChol}},
  \code{\link{strucMatrixDiag}},
  \code{\link{strucMatrixExpChol}},
  \code{\link{strucMatrixFull}},
  \code{\link{strucMatrixGenFullTri}},
  \code{\link{strucMatrixIdent}},
  \code{\link{strucMatrixInd}},
  \code{\link{strucMatrixOneOffDiag}},
  \code{\link{strucMatrixOnes}},
  \code{\link{strucMatrixSymm}},
  \code{\link{strucMatrixVarWithCovariate}}
}

