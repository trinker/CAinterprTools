#' Correlation between rows and columns categories
#'
#' This function allows you to calculate the strenght of the correlation between rows and columns of the contingency table. A reference line indicates the threshold above which the correlation can be considered important.
#' @param data Name of the dataset.
#' @keywords correlation contingency table
#' @export
#' @examples
#' ca.corr(data)
#' 
ca.corr <- function (data){
  mydataasmatrix<-as.matrix(data)
  dataframe.after.ca<- summary(ca(data))
  perf.corr<-(1.0)
  sqr.trace<-round(sqrt(sum(dataframe.after.ca$scree[,2])), digits=3)
  barplot(c(perf.corr, sqr.trace), main="Correlation coefficient between rows & columns (=square root of the inertia)", sub="reference line: threshold of important correlation ", ylab="correlation coeff.", names.arg=c("correlation coeff. range", "correlation coeff. bt rows & cols"), cex.main=0.80, cex.sub=0.80, cex.lab=0.80)
  abline(h=0.20)
}