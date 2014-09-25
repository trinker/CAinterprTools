#' Rows contribution
#'
#' This function allows you to calculate the contribution of the row categories to the selected dimensions. The reference line showing up in the returned histogram indicates the threshold for a significant (i.e., important for the sake of dimension interpretation) contribution.
#' @param data Name of the dataset.
#' @param num.dim Number of the dimension for which the row categories contribution is returned.
#' @keywords rows contribution
#' @export
#' @examples
#' ca.rows.cntr(data, 1) Returns the contribution of the row categories to the 1 CA dimension.
#' 
ca.rows.cntr <- function (data, num.dim){
  nrows <- nrow(data)
  cadataframe<-summary(ca(data, nd=num.dim))
  counter <- 0
  for(i in seq(7, ncol(cadataframe$rows), 3)){    
    counter <- counter +1
    barplot(cadataframe$rows[,i], ylim=c(0,1000), xlab="Row categories", ylab=paste("Contribution to Dim. ",counter," (in permills)"), names.arg=cadataframe$rows[,1], cex.lab=0.80)
    abline(h=round(((100/nrows)*10), digits=0))
  }
}