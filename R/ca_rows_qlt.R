#' Rows quality of the display
#'
#' This function allows you to calculate the quality of the display of the row categories on pairs of selected dimensions.
#' @param data Name of the dataset.
#' @param num.dim Number dimension (minimum 2) for which the row categories quality is returned.
#' @keywords rows quality
#' @export
#' @examples
#' ca.rows.qlt(data, 3) Returns the quality of the display of the row categories on the 1&2, 1&3 dimensions
#' 
ca.rows.qlt <- function (data,num.dim){
  cadataframe<-summary(ca(data, nd=num.dim))
  counter <- 1
  for(i in seq(9, ncol(cadataframe$rows), 3)){    
    counter <- counter +1
    quality.rows <- (cadataframe$rows[,6]+cadataframe$rows[,i])/10
    barplot(quality.rows, ylim=c(0,100), xlab="Row categories", ylab=paste("Quality of the display (% of inertia) on Dim. 1+", counter), names.arg=cadataframe$rows[,1], cex.lab=0.80)
  }
}