# Script to run the null models 

# We ran the null models in a HPC. 
# However, the code used here can also be used in a normal computer.
# Note this will take several hours or days.

###### Set up of the script ############################################################################################

.libPaths(c("/projappl/project_2004675/project_rpackages", .libPaths()))
# Packages .............................................................................................................
if(!require("pacman")) {install.packages("pacman")}
pacman::p_load("itertools","readxl","tidyverse","hypervolume","fossil","doSNOW", "future", "vegan","betapart")
#....................................................................................................................... 

#Program cores for the batch ...........................................................................................
options(future.availableCores.methods = "Slurm")
#....................................................................................................................... 

# Start the script for each dataset ====================================================================================

perm=999 # Number of iterations for null models, I used 999 (perm = 999)

#Functions to be used in the environment ###############################################################################

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
#.......................................................................................................................  

# Define parameters ####################################################################################################
 
  comm <- data$Community #community data
  trait <- data.frame(cmdscale(func_dis, k = 3))  #k represents the number of dimensions
  ntasks <- nrow(comm) #number of repetitions is equal the number of sites
  rownames(trait) <- colnames(comm)
  
########################################################################################################################
  
# Start Null model calculation #########################################################################################

  print(paste("Running null models of dataset:",data$name), stdout())
  print(as.character(Sys.time()), stdout())  
  
cores<- future::availableCores() #if using your own computer I would advise "cores<- future::availableCores()-1"
cl <- makeSOCKcluster(cores)
registerDoSNOW(cl)


TR_randomized <- foreach(i = isplitIndices(perm, chunkSize=1), .errorhandling = "stop") %dopar% {
  
  .libPaths(c("/projappl/project_2004675/project_rpackages", .libPaths()))
  if(!require("hypervolume")) {install.packages("hypervolume")}
  if(!require("foreach")) {install.packages("foreach")}
  
  
# Shuffling species ....................................................................................................
  
  trait <- data.frame(cmdscale(func_dis, k = 3))  #k represents the number of dimensions
  
  #Do not shuffle species in the first iteration
  
  if(i > 1) {
    trait <- trait[sample(nrow(trait)),]
    rownames(trait)<-colnames(comm)
  }
#.......................................................................................................................
  
# Hypervolume alpha diversity ==========================================================================================
  
  ntasks<-nrow(data$Community )
  alpha.FD <- foreach(
    i = 1:ntasks,
    .combine = hypervolume::hypervolume_join,
    .multicombine = TRUE,
    .errorhandling = 'remove'
  ) %do% {
    .libPaths(c("/projappl/project_2004675/project_rpackages", .libPaths()))
    if(!require("hypervolume")) {install.packages("hypervolume")}
    abun <- data$Community [i, which(data$Community [i, ] > 0)]
    w <- as.numeric(abun/sum(abun)) #use w<-NULL for presence-absence
    hv <- hypervolume::hypervolume_gaussian(trait[colnames(data$Community [i, which(data$Community [i, ] > 0)]), ],
                                            weight= w,
                                            verbose = FALSE,
                                            name = rownames(data$Community )[i])
  }
name_sites<-sapply(seq_along(alpha.FD@HVList), function(i) alpha.FD@HVList[[i]]@Name)
  
# Hypervolume beta diversity =========================================================================================== 
  
ntasks<-length(alpha.FD@HVList) #number of hypervolumes that succeded in the calculation

pairwise_beta <-  foreach(i=1:ntasks,
                            .combine=appendList) %:%
    foreach(j=i:ntasks, .combine=appendList) %do% {
      .libPaths(c("/projappl/project_2004675/project_rpackages", .libPaths()))
      if(!require("hypervolume")) {install.packages("hypervolume")}
      hyperSet <- hypervolume::hypervolume_set(
        alpha.FD@HVList[[i]], alpha.FD@HVList[[j]],      
        check.memory = FALSE, verbose = FALSE, num.points.max = 10000)
      union <- hyperSet[[4]]@Volume
      unique1 <- hyperSet[[5]]@Volume
      unique2 <- hyperSet[[6]]@Volume
      
      union <- 2 * union - unique1 - unique2
      Btotal <- (unique1 + unique2)/union
      Brepl <- 2 * min(unique1, unique2)/union
      Brich <- abs(unique1 - unique2)/union
      output<-list(Btotal=Btotal,Brepl=Brepl,Brich=Brich)
      return(output)
    }

  output_beta<-list()
  output_beta$Btotal <- matrix(NA,ntasks,ntasks)
  output_beta$Brepl <- matrix(NA,ntasks,ntasks)
  output_beta$Brich <- matrix(NA,ntasks,ntasks)
  
  output_beta$Btotal[lower.tri(output_beta$Btotal,diag=TRUE)] <-round(pairwise_beta$Btotal,3)
  output_beta$Brepl [lower.tri(output_beta$Btotal,diag=TRUE)] <- round(pairwise_beta$Brich,3)
  output_beta$Brich [lower.tri(output_beta$Btotal,diag=TRUE)] <- round(pairwise_beta$Brepl,3)
  TRpa <- lapply(output_beta, as.dist)
  names(TRpa$Btotal) <- names(TRpa$Brepl) <- names(TRpa$Brich) <- name_sites
  
  return(TRpa)
}
stopCluster(cl)
print(as.character(Sys.time()), stdout())

########################################################################################################################

# Calculate the spartial and environmental heterogeneity for each randomized beta diversity ############################

SH.null<- lapply(1:length(TR_randomized), function(i) fossil::earth.dist(data$Coordinates[rownames(data$Coordinates) %in% names(TR_randomized[[i]]$Btotal),1:2]))
EH.null<- lapply(1:length(TR_randomized), function(i) vegan::vegdist(scale(data$Environment[rownames(data$Coordinates) %in% names(TR_randomized[[i]]$Btotal),]),
                                                 method='euc', na.rm=TRUE))

  
########################################################################################################################
  
# Saving results #######################################################################################################
  
  print(paste("Save results of dataset:",dataset), stdout())
  null_results <- list(TR_randomized, SH.null, EH.null)
  saveRDS(null_results, file = paste0(dataset,"_null_results.rds"))

######################################################################################################################
