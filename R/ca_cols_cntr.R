#' Columns contribution
#'
#' This function allows you to calculate the contribution of the column categories to the selected dimensions. The reference line showing up in the returned histogram indicates the threshold for a significant (i.e., important for the sake of dimension interpretation) contribution.
#' @param data Name of the dataset.
#' @param num.dim Number of the dimension for which the column categories contribution is returned.
#' @keywords cols contribution
#' @export
#' @examples
#' ca.cols.cntr(data,1) Returns the contribution of the column categories to the 1 CA dimension.
#' 
ca.cols.cntr <- function (data, num.dim){
  ncols <- ncol(data)
  cadataframe<-summary(ca(data, nd=num.dim))
  counter <- 0
  for(i in seq(7, ncol(cadataframe$columns), 3)){    
    counter <- counter +1
    barplot(cadataframe$columns[,i], ylim=c(0,1000), xlab="Column categories", ylab=paste("Contribution to Dim. ",counter," (in permills)"), names.arg=cadataframe$columns[,1], cex.lab=0.80)
    abline(h=round(((100/ncols)*10), digits=0))
  }
}