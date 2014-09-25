#' Columns correlation to the dimensions
#'
#' This function allows you to calculate the correlation of the column categories to the selected dimensions.
#' @param data Name of the dataset.
#' @param num.dim Number of the dimension for which the column categories correlation is returned.
#' @keywords cols correlation
#' @export
#' @examples
#' ca.cols.corr(data, 1) Returns the correlation of the column categories to the 1 CA dimension.
#' 
ca.cols.corr <- function (data, num.dim){
  cadataframe<-summary(ca(data, nd=num.dim))
  counter <- 0
  for(i in seq(6, ncol(cadataframe$columns), 3)){    
    counter <- counter +1
    correl.cols <- round(sqrt((cadataframe$columns[,i]/1000)), digits=3)
    barplot(correl.cols, ylim=c(0,1), xlab="Column categories", ylab=paste("Correlation with Dim. ", counter), names.arg=cadataframe$columns[,1], cex.lab=0.80)
  }
}