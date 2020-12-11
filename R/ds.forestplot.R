#' Title
#'
#' @param mod 
#'
#' @return
#' @export
#'
#' @examples
ds.forestplot <- function(mod){
  
  # Declaration of variables
  ntotal <- NULL
  nvalid <- NULL
  nmissing <- NULL
  names <- NULL
  
  # Get number of studies
  num_stud <- mod$num.valid.studies
  # Obtain from each study the numbers of valid, missing and total of individuals. Obtain the study names too
  for(i in 1:num_stud){
    ntotal <- c(ntotal, mod$output.summary[[i]]$Ntotal)
    nvalid <- c(nvalid, mod$output.summary[[i]]$Nvalid)
    nmissing <- c(nmissing, mod$output.summary[[i]]$Nmissing)
    names <- c(names, names(mod$output.summary)[i])
  }
  
  # Get t-test parameters
  n <- sum(nvalid)
  p <- nrow(mod$betamatrix.valid)
  
  # Get coefficients and calculate confidence interval using the standard errors
  mean <- t(mod$betamatrix.valid)
  lower <- t(mod$betamatrix.valid - mod$sematrix.valid * qt(.975, n-p))
  upper <- t(mod$betamatrix.valid + mod$sematrix.valid * qt(.975, n-p))
    
  # Format to be passed to the foresplot function
  names <- c("Study", names, NA, "Summary")
  ntotal <- c("N total", ntotal, NA, sum(ntotal))
  nvalid <- c("N valid", nvalid, NA, sum(nvalid))
  nmissing <- c("N missing", nmissing, NA, sum(nmissing))
  
  # To do: offer choice of ML, REML, FE. only would affect the columns chosen on next 3 lines
  mean <- rbind(NA, mean, NA, mod$SLMA.pooled.ests.matrix[,1])
  lower <- rbind(NA, lower, NA, mod$SLMA.pooled.ests.matrix[,1] - mod$SLMA.pooled.ests.matrix[,2] * qt(.975, n-p))
  upper <- rbind(NA, upper, NA, mod$SLMA.pooled.ests.matrix[,1] + mod$SLMA.pooled.ests.matrix[,2] * qt(.975, n-p))
  
  # Build plot parameters
  legend <- colnames(mean)
  summary <- c(TRUE, rep(FALSE, num_stud),TRUE, TRUE)
  colors <- RColorBrewer::brewer.pal(n = length(legend), name = "Set2")
  tabletext <- cbind(names, ntotal, nvalid, nmissing)
  
  forestplot::forestplot(tabletext, mean = mean, lower = lower, upper = upper, legend = legend,
             is.summary = summary, col=forestplot::fpColors(box=colors,
                                                summary = colors),
             xlab="Coefficient")
  
}
