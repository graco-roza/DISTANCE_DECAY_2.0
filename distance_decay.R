#Null model script
setwd("")
###### Set up of the script ############################################################################################

.libPaths(c("/projappl/project_2004675/project_rpackages", .libPaths())) #For use in Cluster ONLY
# Packages .............................................................................................................
if(!require("pacman")) {install.packages("pacman")}
pacman::p_load("tidyverse","fossil", "vegan","doSNOW","BAT","zetadiv")
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

data <- readRDS(paste0("pre_processed/",files[jj])) #load the data into R 

  
#Functions to be used in the environment ###############################################################################
 
# Booststrap slopes function ...........................................................................................

  Jknife=function(y,sh,eh) {

    require(doSNOW)

    cores<- future::availableCores()
    cl <- makeSOCKcluster(cores)
    registerDoSNOW(cl)
    
    nsites=nrow(as.matrix(y))
    
    Results <- foreach(i=1:nsites, .combine=rbind, .errorhandling = "stop") %do% {
      
      d1 <- as.vector(as.dist(as.matrix(sh)[-i,-i])) #SH = spatial distances
      d2 <- as.vector(as.dist(as.matrix(eh)[-i,-i])) #EH =  Environmental distances
      s <-  as.vector(as.dist(as.matrix(y)[-i,-i]))  #Y = community similarity
      
      #log-binomial fit
 
      mod1 <- tryCatch(coef(glm(s ~ d1, family = binomial("log"), start=c(log(mean(s)),0)))[2],error = function(e) NA)
      mod2 <- tryCatch(coef(glm(s ~ d2, family = binomial("log"), start=c(log(mean(s)),0)))[2],error = function(e) NA)
      
      output<-data.frame(SH = mod1, EH = mod2)
      return(output)
    }
    #stop cluster
    stopCluster(cl)

    #Parameters of interest
    result<- colMeans(Results, na.rm=TRUE)
    return(result)
  }
#.......................................................................................................................
# Define parameters ####################################################################################################
 
  comm <- data$Community #community data

########################################################################################################################
  
# Estimate beta ####################################################################################################
  
  beta_pa <- BAT::beta(comm=as.matrix(comm), func="sorensen", abund=FALSE)
  beta_abun <- BAT::beta(comm=as.matrix(comm), func="sorensen", abund=TRUE)

########################################################################################################################

# Estimate environmental and spatial distances #########################################################################

  SH<- fossil::earth.dist(data$Coordinates)
  EH<- vegan::vegdist(scale(data$Environment), method='euc', na.rm=TRUE)
  EH<- EH/max(EH)
#Mantel calculations .......................................................

  Mantel_env_pa <- lapply(beta_pa, function(x) vegan::mantel(x,EH, method="spearman", parallel = future::availableCores()))
  Mantel_spa_pa <- lapply(beta_pa, function(x) vegan::mantel(x,SH, method="spearman", parallel = future::availableCores()))

  Mantel_env_abun <- lapply(beta_abun, function(x) vegan::mantel(x,EH, method="spearman", parallel = future::availableCores()))
  Mantel_spa_abun <- lapply(beta_abun, function(x) vegan::mantel(x,SH, method="spearman", parallel = future::availableCores()))
  
# Bootstrap slopes  ........................................................
  
 

  slopes_pa <- lapply(beta_pa, function(x) Jknife(x,SH,EH))
  slopes_abun <- lapply(beta_abun, function(x) Jknife(x,SH,EH))
  


########################################################################################################################
  
# Saving results #######################################################################################################

  
  output_pa <-  data.frame(dataset=dataset,
                        Slope_spa = do.call(rbind,slopes_pa)[,1], 
                        Mantel_spa = do.call(rbind,lapply(Mantel_spa_pa, function(x) x$statistic)),
                        significance_spa = do.call(rbind,lapply(Mantel_spa_pa, function(x) x$signif)),
                        Slope_env = do.call(rbind,slopes_pa)[,2], 
                        Mantel_env = do.call(rbind,lapply(Mantel_env_pa, function(x) x$statistic)),
                        significance_env = do.call(rbind,lapply(Mantel_env_pa, function(x) x$signif)))

  output_abun <-  data.frame(dataset=dataset,
                         Slope_spa = do.call(rbind,slopes_abun)[,1], 
                         Mantel_spa = do.call(rbind,lapply(Mantel_spa_abun, function(x) x$statistic)),
                         significance_spa = do.call(rbind,lapply(Mantel_spa_abun, function(x) x$signif)),
                         Slope_env = do.call(rbind,slopes_abun)[,2], 
                         Mantel_env = do.call(rbind,lapply(Mantel_env_abun, function(x) x$statistic)),
                         significance_env = do.call(rbind,lapply(Mantel_env_abun, function(x) x$signif)))
  
#  write.csv(file = paste0("taxonomic_results_pa/",dataset,"_tax_res_pa.csv"), x= output_pa)
#  write.csv(file = paste0("taxonomic_results_abun/",dataset,"_tax_res_abun.csv"), x= output_abun)
  
#Constraints 
  
# dataset    Slope_spa Mantel_spa significance_spa    Slope_env Mantel_env significance_env
# Btotal  N18MPP 1.389844e-06 0.16220271            0.169 1.389844e-06  0.2756092            0.060
# Brepl   N18MPP 1.165923e-05 0.07418687            0.326 1.165923e-05  0.1102108            0.254
# Brich   N18MPP 1.502443e-04 0.14069788            0.116 1.502443e-04  0.2214713            0.034
  
#No constraints
  
# dataset     Slope_spa Mantel_spa significance_spa Slope_env Mantel_env significance_env
# Btotal  N18MPP -0.0001195972 0.16220271            0.181 0.3784608  0.2756092            0.068
# Brepl   N18MPP -0.0002587262 0.07418687            0.309 0.2488193  0.1102108            0.250
# Brich   N18MPP  0.0001298781 0.14069788            0.152 0.6923562  0.2214713            0.044  


######################################################################################################################

  