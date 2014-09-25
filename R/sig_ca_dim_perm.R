#' Permuted significance of CA dimensions
#'
#' This function allows you to calculate the permuted significance of selected CA dimensions. Number of permutation set at 999. Since the function returns a scatterplot, the function requires that two dimensions are entered.
#' @param data Name of the dataset.
#' @param x First dimension whose significance is calculated.
#' @param y Second dimension whose significance is calculated.
#' @keywords permuted significance dimensions
#' @export
#' @examples
#' sig.ca.dim.perm(data, 1,2) Returns a scatterplot of the permuted inertia of the 1 CA dimension against the permuted inertia of the 2 CA dimension. Observed inertia of the selected dimensions and 95th percentile of the permuted inertias are also displayed for testing the significance of the observed inertias.
#' 
sig.ca.dim.perm <- function(data,x,y) {
  res<-epCA.inference.battery(data, test.iters=999, graphs=FALSE)
  
  #count the number of permuted eigenvalues of the first dimensions that are greater than the observed eigenvalue of the same dimension
  first.dim.ratio<-length(which(res$Inference.Data$components$eigs.perm[,x] > res$Inference.Data$components$eigs[x])) 
  
  #compute the p value of the first selected dimension
  pvalue.first.dim<-round(first.dim.ratio/1000, 5) 
  
  #the same applies for the second selected dimension
  second.dim.ratio<-length(which(res$Inference.Data$components$eigs.perm[,y] > res$Inference.Data$components$eigs[y])) 
  pvalue.second.dim<-round(second.dim.ratio/1000,5)
  
  plot(res$Inference.Data$components$eigs.perm[,x], res$Inference.Data$components$eigs.perm[,y], main=" Scatterplot of permuted Dimensions' inertia", sub="solid line: obs. inertia; dashed line: 95 percentile of the permuted distrib. (=alpha 0.05 threshold)", xlab=paste("inertia of permuted", x,"Dim (", pvalue.first.dim, ")"), ylab=paste("inertia of permuted", y,"Dim (", pvalue.second.dim, ")"))
  
  ##add reference lines
  #reference lines indicating the 95 percentile of the permuted distribution of the first and second dimensions' inertia
  abline(v=quantile(res$Inference.Data$components$eigs.perm[,x], c(0.95)), lty=2, col="blue")
  abline(h=quantile(res$Inference.Data$components$eigs.perm[,y], c(0.95)), lty=2, col="blue")
  
  #reference lines indicating the observed inertia of the first two dimensions
  abline(v=res$Inference.Data$components$eigs[x])
  abline(h=res$Inference.Data$components$eigs[y])
  
  #report some data on the R console
  print(paste("p value of dim", x,"=",pvalue.first.dim))
  print(paste("p value of dim", y,"=",pvalue.second.dim))
}