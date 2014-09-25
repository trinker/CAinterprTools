#' Rows correlation to the dimensions
#'
#' This function allows you to calculate the correlation of the row categories to the selected dimensions.
#' @param data Name of the dataset.
#' @param num.dim Number of the dimension for which the row categories correlation is returned.
#' @keywords rows correlation
#' @export
#' @examples
#' ca.rows.corr(data, 1) Returns the correlation of the row categories to the 1 CA dimension.
#' 
ca.rows.corr <- function (data, num.dim){
  cadataframe<-summary(ca(data, nd=num.dim))
  counter <- 0
  for(i in seq(6, ncol(cadataframe$rows), 3)){    
    counter <- counter +1
    correl.rows <- round(sqrt((cadataframe$rows[,i]/1000)), digits=3)
    barplot(correl.rows, ylim=c(0,1), xlab="Row categories", ylab=paste("Correlation with Dim. ", counter), names.arg=cadataframe$rows[,1], cex.lab=0.80)
  }
}