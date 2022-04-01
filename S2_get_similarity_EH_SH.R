
# Packages .............................................................................................................
if(!require("pacman")) {install.packages("pacman")}
pacman::p_load("tidyverse","fossil", "vegan","doSNOW","BAT","zetadiv")

#Functions used 

#Jackknife function modified from Millar et al 2011.

# Appending lists function .............................................................................................  
appendList <- function (x, val) {
  stopifnot(is.list(x), is.list(val))
  xnames <- names(x)
  for (v in names(val)) {
    x[[v]] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]])) 
      appendList(x[[v]], val[[v]])
    else c(x[[v]], val[[v]])
  }
  x
}
appendList2 <- function (x, val) {
  stopifnot(is.list(x), is.list(val))
  xnames <- names(x)
  for (v in names(val)) {
    x[[v]] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]])) 
      appendList(x[[v]], val[[v]])
    else rbind(x[[v]], val[[v]])
  }
  x
}

# Estimate beta ####################################################################################################
  
  beta_tax_pa   <- BAT::beta(comm=as.matrix(data$Community), func="sorensen", abund=FALSE)
  beta_tax_abun <- BAT::beta(comm=as.matrix(data$Community), func="sorensen", abund=TRUE)

trait <- data.frame(cmdscale(func_dis, k = 3))
ntasks<-nrow(data$Community)

  cores <- future::availableCores()-1
  cl <- makeSOCKcluster(cores)
  registerDoSNOW(cl)

  functional_alpha_pa <- foreach(
    i = 1:ntasks,
    .combine = hypervolume::hypervolume_join,
    .multicombine = TRUE,
    .errorhandling = 'remove'
  ) %dopar% {
    if(!require("hypervolume")) {install.packages("hypervolume")}
    hv <- hypervolume::hypervolume_gaussian(trait[colnames(data$Community[i, which(data$Community[i, ] > 0)]), ],
                                            verbose = FALSE,
                                            name = rownames(data$Community)[i])
  }
  stopCluster(cl)
  
  cores <- future::availableCores()-1
  cl <- makeSOCKcluster(cores)
  registerDoSNOW(cl)
   
  functional_alpha_abun <- foreach(
    i = 1:ntasks,
    .combine = hypervolume::hypervolume_join,
    .multicombine = TRUE,
    .errorhandling = 'remove'
  ) %dopar% {
    .libPaths(c("/projappl/project_2004675/project_rpackages", .libPaths()))
    if(!require("hypervolume")) {install.packages("hypervolume")}
    abun <- data$Community[i, which(data$Community[i, ] > 0)]
    w <- as.numeric(abun/sum(abun))
    hv <- hypervolume::hypervolume_gaussian(trait[colnames(data$Community[i, which(data$Community[i, ] > 0)]), ],
                                            verbose = FALSE,
                                            weight = w,
                                            name = rownames(data$Community)[i])
  }
  stopCluster(cl)
  
  ########################################################################################################################
  
  # Hypervolume Beta diversity ###########################################################################################
  
  ntasks<-length(functional_alpha_pa@HVList) # Count hypervolumes that succeed in the estimation 
  
  cl <- makeSOCKcluster(cores) # create the parallel structure
  registerDoSNOW(cl)  # initialize the parallel 
  
  
  output_beta_pa <-  foreach(i = 1:ntasks,
                          .combine = appendList) %:%
    foreach(j = i:ntasks,
            .combine = appendList) %dopar% {
              
              
              .libPaths(c("/projappl/project_2004675/project_rpackages", .libPaths())) #set again the library path
              if(!require("hypervolume")) {install.packages("hypervolume")} #load package in the parallel environment 
              
              hyperSet <- hypervolume::hypervolume_set(functional_alpha_pa@HVList[[i]], functional_alpha_pa@HVList[[j]],
                                                       check.memory = FALSE, verbose = FALSE, num.points.max = 1000)
              union <- hyperSet[[4]]@Volume
              unique1 <- hyperSet[[5]]@Volume
              unique2 <- hyperSet[[6]]@Volume
              
              union <- 2 * union - unique1 - unique2
              Btotal <- (unique1 + unique2)/union
              Brepl <- 2 * min(unique1, unique2)/union
              Brich <- abs(unique1 - unique2)/union
              output<-list(Btotal=Btotal,Brepl=Brepl,Brich=Brich)
            }
  stopCluster(cl)
  
  # Pass beta values to the matrix and conver to distance object  ........................................................
  
  beta_fun_pa<-list()
  beta_fun_pa$Btotal <- matrix(NA,ntasks,ntasks)
  beta_fun_pa$Brepl <- matrix(NA,ntasks,ntasks)
  beta_fun_pa$Brich <- matrix(NA,ntasks,ntasks)
  
  beta_fun_pa$Btotal[lower.tri(beta_fun_pa$Btotal,diag=TRUE)] <-round(output_beta_pa$Btotal,3)
  beta_fun_pa$Brepl[lower.tri(beta_fun_pa$Btotal,diag=TRUE)] <- round(output_beta_pa$Brich,3)
  beta_fun_pa$Brich[lower.tri(beta_fun_pa$Btotal,diag=TRUE)] <- round(output_beta_pa$Brepl,3)
  beta_fun_pa<-lapply(beta_fun_pa, as.dist)
  
  #if any value higher than 1, fix it to 1 
  
  beta_fun_pa$Btotal[which(beta_fun_pa$Btotal > 1 )]  <- 1
  beta_fun_pa$Brepl [which(beta_fun_pa$Brepl > 1 )]   <- 1
  beta_fun_pa$Brich [which(beta_fun_pa$Brich > 1 )]   <- 1
  
#Functional beta diversity with abundance data  
  cl <- makeSOCKcluster(cores) # create the parallel structure
  registerDoSNOW(cl)  # initialize the parallel 
  
  ntasks<-length(functional_alpha_abun@HVList) # Count hypervolumes that succeed in the estimation 
  
  output_beta_abun <-  foreach(i = 1:ntasks,
                             .combine = appendList) %:%
    foreach(j = i:ntasks,
            .combine = appendList) %dopar% {
              
              
             if(!require("hypervolume")) {install.packages("hypervolume")} #load package in the parallel environment 
              
              hyperSet <- hypervolume::hypervolume_set(functional_alpha_abun@HVList[[i]], functional_alpha_abun@HVList[[j]],
                                                       check.memory = FALSE, verbose = FALSE, num.points.max = 1000)
              union <- hyperSet[[4]]@Volume
              unique1 <- hyperSet[[5]]@Volume
              unique2 <- hyperSet[[6]]@Volume
              
              union <- 2 * union - unique1 - unique2
              Btotal <- (unique1 + unique2)/union
              Brepl <- 2 * min(unique1, unique2)/union
              Brich <- abs(unique1 - unique2)/union
              output<-list(Btotal=Btotal,Brepl=Brepl,Brich=Brich)
            }
  stopCluster(cl)
  
  # Pass beta values to the matrix and conver to distance object  ........................................................
  
  beta_fun_abun<-list()
  beta_fun_abun$Btotal <- matrix(NA,ntasks,ntasks)
  beta_fun_abun$Brepl <- matrix(NA,ntasks,ntasks)
  beta_fun_abun$Brich <- matrix(NA,ntasks,ntasks)
  
  beta_fun_abun$Btotal[lower.tri(beta_fun_abun$Btotal,diag=TRUE)] <-round(output_beta_abun$Btotal,3)
  beta_fun_abun$Brepl[lower.tri(beta_fun_abun$Btotal,diag=TRUE)] <- round(output_beta_abun$Brich,3)
  beta_fun_abun$Brich[lower.tri(beta_fun_abun$Btotal,diag=TRUE)] <- round(output_beta_abun$Brepl,3)
  beta_fun_abun<-lapply(beta_fun_abun, as.dist)
  
  #if any value higher than 1, fix it to 1 
  
  beta_fun_abun$Btotal[which(beta_fun_abun$Btotal > 1 )]  <- 1
  beta_fun_abun$Brepl [which(beta_fun_abun$Brepl > 1 )]   <- 1
  beta_fun_abun$Brich [which(beta_fun_abun$Brich > 1 )]   <- 1
  
  
########################################################################################################################

# Estimate environmental and spatial distances #########################################################################

  SH<- fossil::earth.dist(data$Coordinates)
  EH<- vegan::vegdist(scale(data$Environment), method='euc', na.rm=TRUE)
  EH<- EH/max(EH)

#Output example 
  
# dataset    Slope_spa Mantel_spa significance_spa    Slope_env Mantel_env significance_env
# Btotal  N18MPP 1.389844e-06 0.16220271            0.169 1.389844e-06  0.2756092            0.060
# Brepl   N18MPP 1.165923e-05 0.07418687            0.326 1.165923e-05  0.1102108            0.254
# Brich   N18MPP 1.502443e-04 0.14069788            0.116 1.502443e-04  0.2214713            0.034


######################################################################################################################

  