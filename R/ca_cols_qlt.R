#' Columns quality of the display
#'
#' This function allows you to calculate the quality of the display of the column categories on pairs of selected dimensions.
#' @param data Name of the dataset.
#' @param num.dim Number dimension (minimum 2) for which the column categories quality is returned.
#' @keywords cols quality
#' @export
#' @examples
#' ca.cols.qlt(data, 3) Returns the quality of the display of the column categories on the 1&2, 1&3 dimensions.
#' 
ca.cols.qlt <- function (data,num.dim){
  cadataframe<-summary(ca(data, nd=num.dim))
  counter <- 1
  for(i in seq(9, ncol(cadataframe$columns), 3)){    
    counter <- counter +1
    quality.cols <- (cadataframe$columns[,6]+cadataframe$columns[,i])/10
    barplot(quality.cols, ylim=c(0,100), xlab="Column categories", ylab=paste("Quality of the display (% of inertia) on Dim. 1+", counter), names.arg=cadataframe$columns[,1], cex.lab=0.80)
  }
}