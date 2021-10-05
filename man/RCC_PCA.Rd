\name{RCC_PCA}
\alias{RCC_PCA}
\title{Retained Component Criterion for Principal Component Analysis}
\description{The RCC_PCA criterion is a new tool to determine the optimal number of components (i.e. PCs) to retain for Principal Component Analysis (PCA). This criterion balances between the following two desires, reducing the dimension of the data and increasing the accuracy of the final results of PCA; See Alshammri (2021).}
\usage{RCC_PCA(x)}
\arguments{
  \item{x}{a N-by-m data matrix, where the rows are "N" observations, and the columns are "m" variables}}
\value{The values of RCC criterion}
\references{Alshammri, F. (2021). Retained component criterion for optimizing principal component analysis. Manuscript submitted for publication.}
\author{Fayed Alshammri}
\examples{
##The data matrix x is the scalled first four variables of "iris data" with m=4 and N=150.
x=scale(iris[,1:4])

##calculate and plot the RCC_PCA values of x after applying PCA.
myresults=RCC_PCA(x)

##lists the values of RCC criterion
myresults
}
