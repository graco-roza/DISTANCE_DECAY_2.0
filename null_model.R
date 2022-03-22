# Script to run the null models on abundance data 

###### Set up of the script ############################################################################################

.libPaths(c("/projappl/project_2004675/project_rpackages", .libPaths()))
# Packages .............................................................................................................
if(!require("pacman")) {install.packages("pacman")}
pacman::p_load("itertools","readxl","tidyverse","hypervolume","fossil","doSNOW", "future", "vegan","betapart")
#....................................................................................................................... 

#Program cores for the batch ...........................................................................................
options(future.availableCores.methods = "Slurm")
#....................................................................................................................... 

# Read datasets ........................................................................................................
files <- sort(list.files("pre_processed"))
#....................................................................................................................... 

# Start the script for each dataset ====================================================================================

# grab the array id value from the environment variable passed from sbatch
slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')

# coerce the value to an integer
jj <- as.numeric(slurm_arrayid)
  
data<-  readRDS(paste0("pre_processed/",files[jj]))
perm=2

  
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
  func_dis <- gawdis::gawdis(data$Traits, w.type="optimized", opti.maxiter=10)
  trait <- data.frame(cmdscale(func_dis, k = 3))  #k represents the number of dimensions
  ntasks <- nrow(comm) #number of repetitions is equal the number of sites
  rownames(trait) <- colnames(comm)
  
########################################################################################################################
  
# Start Null model calculation #########################################################################################

  print(paste("Running null models of dataset:",data$name), stdout())
  print(as.character(Sys.time()), stdout())  
  
cores<- future::availableCores()
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
  
  ntasks<-nrow(comm)
  alpha.FD <- foreach(
    i = 1:ntasks,
    .combine = hypervolume::hypervolume_join,
    .multicombine = TRUE,
    .errorhandling = 'remove'
  ) %do% {
    .libPaths(c("/projappl/project_2004675/project_rpackages", .libPaths()))
    if(!require("hypervolume")) {install.packages("hypervolume")}
    abun <- comm[i, which(comm[i, ] > 0)]
    w <- as.numeric(abun/sum(abun)) #use w<-NULL for presence-absence
    hv <- hypervolume::hypervolume_gaussian(trait[colnames(comm[i, which(comm[i, ] > 0)]), ],
                                            weight= w,
                                            verbose = FALSE,
                                            name = rownames(comm)[i])
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
  
  save(TR_randomized,SH.null,EH.null, file = paste0("null_models_workspace/",dataset,"_workspace.R"))

######################################################################################################################
