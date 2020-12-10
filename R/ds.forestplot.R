ds.forestplot <- function(mod){
  
  # Declaration of variables
  ntotal <- NULL
  nvalid <- NULL
  nmissing <- NULL
  names <- NULL
  
  # Get coefficients and calculate confidence interval using the standard errors
  mean <- t(mod$betamatrix.valid)
  lower <- t(mod$betamatrix.valid - mod$sematrix.valid * 1.96)
  upper <- t(mod$betamatrix.valid + mod$sematrix.valid * 1.96)
  
  # Get number of studies
  num_stud <- mod$num.valid.studies
  # Obtain from each study the numbers of valid, missing and total of individuals. Obtain the study names too
  for(i in 1:num_stud){
    ntotal <- c(ntotal, mod$output.summary[[i]]$Ntotal)
    nvalid <- c(nvalid, mod$output.summary[[i]]$Nvalid)
    nmissing <- c(nmissing, mod$output.summary[[i]]$Nmissing)
    names <- c(names, names(mod$output.summary)[i])
  }
  
  # Format to be passed to the foresplot function
  names <- c("Study", names, NA, "Summary")
  ntotal <- c("N total", ntotal, NA, sum(ntotal))
  nvalid <- c("N valid", nvalid, NA, sum(nvalid))
  nmissing <- c("N missing", nmissing, NA, sum(nmissing))
  
  # To do: offer choice of ML, REML, FE. only would affect the columns chosen on next 3 lines
  mean <- rbind(NA, mean, NA, mod$SLMA.pooled.ests.matrix[,1])
  lower <- rbind(NA, lower, NA, mod$SLMA.pooled.ests.matrix[,1] - mod$SLMA.pooled.ests.matrix[,2] * 1.96)
  upper <- rbind(NA, upper, NA, mod$SLMA.pooled.ests.matrix[,1] + mod$SLMA.pooled.ests.matrix[,2] * 1.96)
  
  # Build plot parameters
  legend <- colnames(mean)
  summary <- c(TRUE, rep(FALSE, length(legend)),TRUE, TRUE)
  colors <- brewer.pal(n = length(legend), name = "Set2")
  tabletext <- cbind(names, ntotal, nvalid, nmissing)
  
  forestplot(tabletext, mean = mean, lower = lower, upper = upper, legend = legend,
             is.summary = summary, col=fpColors(box=colors,
                                                summary = colors),
             xlab="Coefficient")
  
}