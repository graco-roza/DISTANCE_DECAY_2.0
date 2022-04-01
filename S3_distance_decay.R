#GLM, Mantel R and build the final table.

###### Set up of the script ############################################################################################

# Packages .............................................................................................................
if(!require("pacman")) {install.packages("pacman")}
pacman::p_load("tidyverse","vegan","performance","zetadiv","future")
#....................................................................................................................... 

# Functions ============================================================================================================
Jknife=function(y,sh,eh) {
  require(doSNOW)
  
  cores<- future::availableCores()
  cl <- makeSOCKcluster(cores)
  registerDoSNOW(cl)
  
  nsites=nrow(as.matrix(y))
  
  Results <- foreach(i=1:nsites, .combine=rbind, .errorhandling = "stop") %do% {
    d1 <- as.vector(as.dist(as.matrix(sh)[-i,-i]))
    d2 <- as.vector(as.dist(as.matrix(eh)[-i,-i]))
    s <-  as.vector(as.dist(as.matrix(y)[-i,-i]))
    
    #log-binomial fit
    mod1 <- tryCatch(coef(zetadiv::glm.cons(s ~ d1, family = quasibinomial("log"), start=c(log(mean(s)),0), cons=1, cons.inter=-1)),error = function(e) c(NA,NA))
    mod2 <- tryCatch(coef(zetadiv::glm.cons(s ~ d2, family = quasibinomial("log"), start=c(log(mean(s)),0), cons=1, cons.inter=-1)),error = function(e) c(NA,NA))
    
    output <- c(mod1, mod2)
    return(output)
  }
  #stop cluster
  stopCluster(cl)
  
  #Parameters of interest
  
  r2_sh <- performance::r2_nagelkerke(zetadiv::glm.cons(as.vector(y) ~ as.vector(sh), family = gaussian("log"), start=c(log(mean(s)),0), cons=1, cons.inter=-1))
  r2_eh <- performance::r2_nagelkerke(zetadiv::glm.cons(as.vector(y) ~ as.vector(eh), family = gaussian("log"), start=c(log(mean(s)),0), cons=1, cons.inter=-1))
  
  result<- colMeans(Results, na.rm=TRUE)
  result<- c(result,r2_sh=r2_sh,r2_env=r2_eh)
  return(result)
}

#Mantel test  ==========================================================================================================

tax_pa_mantel <- do.call(rbind, lapply(beta_tax_pa, function(x) {
  mod_SH <- vegan::mantel(
    x,SH,
    method="spearman",
    parallel=future::availableCores()
  )
  mod_EH <- vegan::mantel(
    x,EH,
    method="spearman",
    parallel=future::availableCores()
  )
  mantel_res<- c(
    Mantel_spa = mod_SH$statistic,
    spa_signif = mod_SH$signif,
    Mantel_env = mod_EH$statistic,
    env_signif = mod_EH$signif,
    Level = "TAX",
    Based = "occ"
  )
  return(mantel_res)})) %>%
  data.frame() %>%   mutate(Beta_type = c("Total similarities", "Replacement", "Richness differences"))

tax_abun_mantel <- do.call(rbind, lapply(beta_tax_abun, function(x) {
  mod_SH <- vegan::mantel(
    x,SH,
    method="spearman",
    parallel=future::availableCores()
  )
  mod_EH <- vegan::mantel(
    x,EH,
    method="spearman",
    parallel=future::availableCores()
  )
  mantel_res<- c(
    Mantel_spa = mod_SH$statistic,
    spa_signif = mod_SH$signif,
    Mantel_env = mod_EH$statistic,
    env_signif = mod_EH$signif,
    Level = "TAX",
    Based = "abund"
  )
  return(mantel_res)})) %>% 
  data.frame() %>%  
  mutate(Beta_type = c("Total similarities", "Replacement", "Richness differences"))

fun_pa_mantel <- do.call(rbind, lapply(beta_fun_pa, function(x) {
  mod_SH <- vegan::mantel(
    x,SH,
    method="spearman",
    parallel=future::availableCores()
  )
  mod_EH <- vegan::mantel(
    x,EH,
    method="spearman",
    parallel=future::availableCores()
  )
  mantel_res<- c(
    Mantel_spa = mod_SH$statistic,
    spa_signif = mod_SH$signif,
    Mantel_env = mod_EH$statistic,
    env_signif = mod_EH$signif,
    Level = "FUN",
    Based = "occ"
  )
  return(mantel_res)})) %>% 
  data.frame() %>%  
  mutate(Beta_type = c("Total similarities", "Replacement", "Richness differences"))

fun_abun_mantel <- do.call(rbind, lapply(beta_fun_abun, function(x) {
  mod_SH <- vegan::mantel(
    x,SH,
    method="spearman",
    parallel=future::availableCores()
  )
  mod_EH <- vegan::mantel(
    x,EH,
    method="spearman",
    parallel=future::availableCores()
  )
  mantel_res<- c(
    Mantel_spa = mod_SH$statistic,
    spa_signif = mod_SH$signif,
    Mantel_env = mod_EH$statistic,
    env_signif = mod_EH$signif,
    Level = "FUN",
    Based = "abund"
  )
  return(mantel_res)})) %>% 
  data.frame() %>%  
  mutate(Beta_type = c("Total similarities", "Replacement", "Richness differences"))



#Generalized dissimilarity model =======================================================================================
tax_pa_glm <- do.call(rbind, lapply(beta_tax_pa, function(x) {
  mod <- Jknife(x,SH,EH)
  glm_res<- c(
    Intercept_spa = as.numeric(mod[1]),
    Slope_spa = as.numeric(mod[2]),
    R2_spa = as.numeric(mod[5]) ,
    Intercept_env = as.numeric(mod[3]),
    Slope_env = as.numeric(mod[4]),
    R2_env = as.numeric(mod[6]) ,
    Level = "TAX",
    Based = "occ"
  )
return(glm_res)}
  )) %>% data.frame() %>%   mutate(Beta_type = c("Total similarities", "Replacement", "Richness differences"))

tax_abun_glm <- do.call(rbind, lapply(beta_tax_abun, function(x) {
  mod <- Jknife(x,SH,EH)
  glm_res<- c(
    Intercept_spa = as.numeric(mod[1]),
    Slope_spa = as.numeric(mod[2]),
    R2_spa = as.numeric(mod[5]) ,
    Intercept_env = as.numeric(mod[3]),
    Slope_env = as.numeric(mod[4]),
    R2_env = as.numeric(mod[6]) ,
    Level = "TAX",
    Based = "abund"
  )
  return(glm_res)}
)) %>% data.frame() %>%   mutate(Beta_type = c("Total similarities", "Replacement", "Richness differences"))

fun_pa_glm <- do.call(rbind, lapply(beta_fun_pa, function(x) {
  mod <- Jknife(x,SH,EH)
  glm_res<- c(
    Intercept_spa = as.numeric(mod[1]),
    Slope_spa = as.numeric(mod[2]),
    R2_spa = as.numeric(mod[5]) ,
    Intercept_env = as.numeric(mod[3]),
    Slope_env = as.numeric(mod[4]),
    R2_env = as.numeric(mod[6]) ,
    Level = "FUN",
    Based = "occ"
  )
  return(glm_res)}
)) %>% data.frame() %>%   mutate(Beta_type = c("Total similarities", "Replacement", "Richness differences"))

fun_abun_glm <- do.call(rbind, lapply(beta_fun_abun, function(x) {
  mod <- Jknife(x,SH,EH)
  glm_res<- c(
    Intercept_spa = as.numeric(mod[1]),
    Slope_spa = as.numeric(mod[2]),
    R2_spa = as.numeric(mod[5]) ,
    Intercept_env = as.numeric(mod[3]),
    Slope_env = as.numeric(mod[4]),
    R2_env = as.numeric(mod[6]) ,
    Level = "FUN",
    Based = "abund"
  )
  return(glm_res)}
)) %>% data.frame() %>%   mutate(Beta_type = c("Total similarities", "Replacement", "Richness differences"))

final_table <- full_join(
  bind_rows(tax_pa_glm, fun_pa_glm, tax_abun_glm, fun_abun_glm) %>% mutate(Dataset = data$name) %>%
    mutate_at(vars(contains(c("spa","env"))),as.numeric) %>% 
    mutate_at(vars(!contains(c("spa","env"))),as.character) ,
  bind_rows(tax_pa_mantel, fun_pa_mantel, tax_abun_mantel, fun_abun_mantel) %>% mutate(Dataset = data$name) %>%
    mutate_at(vars(contains(c("Mantel","signif"))),as.numeric) %>% 
    mutate_at(vars(!contains(c("Mantel","signif"))),as.character),
  by =c("Dataset","Beta_type","Level","Based"))
  
rownames(final_table) <- NULL

final_table <- final_table %>% relocate(Dataset,Based,Level,Beta_type)


