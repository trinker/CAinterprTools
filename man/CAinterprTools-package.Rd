\name{CAinterpretation-package}
\alias{CAinterpretation-package}
\alias{CAinterpretation}
\docType{package}
\title{
Visual tools for Correspondence Analysis interpretation and hypothesis testing
}
\description{
A number of interesting packages are available to perform
    Correspondence Analysis in R. At the best of my knowledge, they lack some
    tools to help users to eyeball some critical CA aspects (e.g., contribution
    of rows/cols categories to the principal axes, quality of the
    display,correlation of rows/cols categories with dimensions, etc). Besides
    providing those facilities, this package allows the users to calculate the
    significance of the CA dimensions by means of the 'Average Rule', the
    Malinvaud test, and by permutation test. Further, it allows to also
    calculate the permuted significance of the CA total intertia.
}
\details{
\tabular{ll}{
Package: \tab CAinterpretation\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-09-18\cr
License: \tab GPL\cr
}
The package allows to plot a number of Correspondence Analysis information such as the contribution of rows and columns categories to the principal axes, the quality of points display on selected dimensions, the correlation of row and column categories to selected dimensions, etc. It also allows to assess which dimension(s) is important for the data structure interpretation by means of the so called 'Average Rule'. Moreover, it implements the Malinvaud test, which test the significance of the table dimensions. The package also offers the facility to plot the permuted distribution of the table total inertia as well as of the inertia accounted for by pairs of selected dimensions. The two latter facilities allows to test the significance of the total inertia and of the dimensions the user is interest in. 
For more details about the rationale behind the use of the implemented CA interpretation facilities, see the article Alberti 2013 cited below.
}
\author{
Gianmarco ALBERTI

Maintainer: Gianmarco ALBERTI <gianmarco.alberti@um.edu.mt>
}
\references{
Alberti G. 2013, An R script to facilitate Correspondence Analysis. A guide to the use and the interpretation of results from an archaeological perspective, Archeologia e Calcolatori 24, 25-54.

Benzecri J.P. 1992, Correspondence Analysis Handbook, New York, Marcel Dekker.

Blasius J., Greenacre M. 1998, Visualization of Categorical Data, San Diego-London, Academic Press.

Greenacre M. 2007, Correspondence Analysis in Practice, Boca Raton-London-New York, Chapman&Hall/CRC.

Le S., Josse J., Husson F. 2008, FactoMineR: An R package for multivariate analysis, Journal of Statistical Software, 25, 1-18.

Nenadic O., Greenacre M. 2007, Correspondence Analysis in R, with two- and three-dimensional graphics: The ca package, Journal of Statistical Software, 20, 1-13.

Saporta G. 2006, Probabilites, analyse des donnees et statistique (2e ed.), Paris, Editions Technip.
}
\keyword{ package }
\seealso{

ca, FactoMineR, InPosition
}
\examples{

}