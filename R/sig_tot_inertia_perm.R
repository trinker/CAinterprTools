#' Permuted significance of the CA total inertia
#'
#' This function allows you to calculate the permuted significance of CA total inertia. Number of permutation set at 999.
#' @param data Name of the dataset.
#' @keywords permuted significance total inertia
#' @export
#' @examples
#' sig.tot.inertia.perm(data) Returns the density curve of the permuted total inertia. Observed total inertia and the 95th percentile of the permuted inertia are also displayed for testing the significance of the observed total inertia.
#'
sig.tot.inertia.perm <- function(data) {
  res<-epCA.inference.battery(data, test.iters=999, graphs=FALSE)
  obs.inrt<-round(sum(res$Fixed.Data$ExPosition.Data$eigs),4)
  thresh<-round(quantile(res$Inference.Data$omni$inertia.perm, c(0.95)),5)
  
  #plot the distribution of the permuted total inertia
  d <- density(res$Inference.Data$omni$inertia.perm)
  plot(d, main="Kernel density of CA permuted total inertia", sub=paste("solid line: obs. inertia (",obs.inrt,"); dashed line: 95th percentile of the permut. distrib. (=alpha 0.05 threshold)"))
  polygon(d, col="red", border="blue")
  
  ##add reference lines
  #add reference line indicating the observed total inertia
  abline(v=obs.inrt)
  #add reference line indicating the 95 percentile of the permuted total inertia
  abline(v=thresh, lty=2, col="blue")
  
  #report some data on the R console
  print(paste("observed total inertia:", obs.inrt))
  print(paste("95th percentile of the permuted total inertia:", thresh))
  print("if the observed inertia is greater than the 95th percentile, the observed inertia is significant at alpha 0.05")
}