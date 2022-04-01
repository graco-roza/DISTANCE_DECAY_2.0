#Data pre-processing (

#Log transformation of environmental variables
#Log transformation of traits
#Trait grouping and gawdis

#Load packages

library(tidyverse)

#'@ header *****************************************************************
#'@ dataset (01)  N05TTP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N05TTP"
data <- readRDS(here::here("input_within", "Encrypted", paste0(dataset,".rds")))

# Pairwise functional similarity  -----

func_dis <- gawdis::gawdis(
  data$Traits,
  groups = c(1, 1, 2, 2, 3, 3),
  groups.weight = TRUE,
  ord = "podani",
  w.type = "optimized"
)

source(here::here("scripts","get_tax_dd.R"))

rm(list = setdiff(ls(), c("initial_checking","n")))
#'@ header *****************************************************************
#'@ dataset (02)  N18MPP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)

# Load dataset 
dataset <- "N18MPP"
data <- readRDS(here::here("input_within", "Encrypted", paste0(dataset,".rds")))

# Estimate the pairwise functional similarity among species.

func_dis <- gawdis::gawdis(data$Traits,
                           ord = "podani",
                           w.type = "equal")

#Estimate the communisyt similarity, spatial distances and environmental distances
source(here::here("scripts","get_taxbeta_EH_SH.R"))

#Estimate slopes, Mantel R for functoinal beta diversity (Note this might take several hours)
source(here::here("scripts","distance_decay.R"))

#'@ header *****************************************************************
#'@ dataset (03)  N19MFI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N19MFI"
data <- readRDS(here::here("input_within", "Encrypted", paste0(dataset,".rds")))

# Estimate the pairwise functional similarity among species.

func_dis <- gawdis::gawdis(data$Traits,
                           ord = "podani",
                           w.type = "equal")

#Estimate the communisyt similarity, spatial distances and environmental distances
source(here::here("scripts","get_taxbeta_EH_SH.R"))

#Estimate slopes, Mantel R for functoinal beta diversity (Note this might take several hours)
source(here::here("scripts","distance_decay.R"))


#'@ header *****************************************************************
#'@ dataset (04)  N21FBD_2
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N21FBD_2"
data <- readRDS(here::here("input_within", "Encrypted", paste0(dataset,".rds")))

# Estimate the pairwise functional similarity among species.

func_dis <- gawdis::gawdis(
  x = data$Traits,
  groups = c(rep(1, 13), rep(2, 4), 3, 4),
  ord = "podani",
  groups.weight = TRUE,
  w.type = "optimized"
)

#Estimate the communisyt similarity, spatial distances and environmental distances
source(here::here("scripts","get_taxbeta_EH_SH.R"))

#Estimate slopes, Mantel R for functoinal beta diversity (Note this might take several hours)
source(here::here("scripts","distance_decay.R"))


#'@ header *****************************************************************
#'@ dataset (05)  N21FBD
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N21FBD"
files <-
  readxl::excel_sheets(paste0("input_within/raw_data/", dataset, ".xlsx"))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----

ENV <- data$environment %>%
  mutate_at(
    vars(
      -temperature,
      -pH,
      -moss,
      -sand,
      -gravel,
      -pebble,
      -cobble,
      -boulders,
      -bedrock
    ),
    log1p
  ) #do not transform percentages

# Trait categorization and transformation ----

TR <- data$traits #all binary

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(
#     x = TR[, -c(9, 13, 18)],
#     #remove traits with zero species
#     groups = c(rep(1, 2), rep(2, 10), rep(3, 3)),
#     ord = "podani",
#     groups.weight = TRUE,
#     w.type = "optimized"
#   )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))


#'@ header *****************************************************************
#'@ dataset (06)  N21FMI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N21FMI"
files <-
  readxl::excel_sheets(paste0("input_within/raw_data/", dataset, ".xlsx"))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----

data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----

ENV <- data$environment %>%
  mutate_at(vars(-Order, -PH), log1p)

# Trait categorization and transformation ----

TR <- data$traits %>% mutate_all(ordered) #all binary

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(x = TR,
#                            ord = "podani",
#                            w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))


#'@ header *****************************************************************
#'@ dataset (07)  N25FBD
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N25FBD"
files <-
  readxl::excel_sheets(paste0("input_within/raw_data/", dataset, ".xlsx"))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----

ENV <- data$environment %>%
  mutate_at(vars(-Order, -PH), log1p)
# Trait categorization and transformation ----

TR <- data$traits #all binary

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(rep(1, 13), rep(2, 4), 3, 4),
#   ord = "podani",
#   groups.weight = TRUE,
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (08)  N25FMI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N25FMI"
files <-
  readxl::excel_sheets(paste0("input_within/raw_data/", dataset, ".xlsx"))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----

ENV <- data$environment %>%
  mutate_at(vars(-Order, -PH, -waterT), log1p)
# Trait categorization and transformation ----

TR <- data$traits %>% mutate_all(ordered) #all binary

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(x = TR,
#                            ord = "podani",
#                            w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (09)  N26MCI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N26MCI"
files <-
  readxl::excel_sheets(paste0("input_within/raw_data/", dataset, ".xlsx"))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----

ENV <- data$environment
# Trait categorization and transformation ----

TR <- data$traits #all binary

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(rep(1, 3),
#              rep(2, 3),
#              rep(3, 3),
#              rep(4, 2),
#              rep(5, 2)),
#   fuzzy = 1:5,
#   ord = "podani",
#   groups.weight = TRUE,
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (10)  N29FBD
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N29FBD"
files <-
  readxl::excel_sheets(paste0("input_within/raw_data/", dataset, ".xlsx"))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----

ENV <- data$environment %>%
  mutate_at(vars(-Order, -PH, -waterT), log1p)  %>%
  select(-Order) #remove because there are NAs
# Trait categorization and transformation ----

TR <- data$traits #all binary

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(rep(1, 13), rep(2, 4), 3, 4),
#   ord = "podani",
#   groups.weight = TRUE,
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))


#'@ header *****************************************************************
#'@ dataset (11)  N29FMI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N29FMI"
files <-
  readxl::excel_sheets(paste0("input_within/raw_data/", dataset, ".xlsx"))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----

ENV <- data$environment %>%
  mutate_at(vars(-Order, -PH, -waterT), log1p) %>%
  select(-Order) #remove because there are NAs

# Trait categorization and transformation ----

TR <- data$traits %>% mutate_all(ordered) #all binary

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   ord = "podani",
#   groups.weight = TRUE,
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)

rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (12)  N33TTP_3
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N33TTP_3"
files <-
  readxl::excel_sheets(paste0("input_within/raw_data/", dataset, ".xlsx"))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----

ENV <- data$environment %>%
  mutate_at("Aridity", log1p)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(LA, SLA, LDMC, LCC, LNC, LCNR, VH, MH), log1p) %>%
  mutate_at(vars(GrowthForm, FlowerOnset), as.factor)

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----
# 
# func_dis <- gawdis::gawdis(
#   x = TR,
#   ord = "podani",
#   groups.weight = TRUE,
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (13)  N33TTP_4
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N33TTP_4"
files <-
  readxl::excel_sheets(paste0("input_within/raw_data/", dataset, ".xlsx"))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----

ENV <- data$environment %>%
  mutate_at("Aridity", log1p)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(LA, SLA, LDMC, LCC, LNC, LCNR, VH, MH), log1p) %>%
  mutate_at(vars(GrowthForm, FlowerOnset), as.factor)

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   ord = "podani",
#   groups.weight = TRUE,
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (14)  N34FBD
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N34FBD"
files <-
  readxl::excel_sheets(paste0("input_within/raw_data/", dataset, ".xlsx"))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----

ENV <- data$environment %>%
  mutate_at(vars(-waterT, -PH, -Order), log1p) %>%
  select(-Order) #remove because of NAs

# Trait categorization and transformation ----

TR <- data$traits

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(rep(1, 13), rep(2, 4), 3, 4),
#   ord = "podani",
#   groups.weight = TRUE,
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (15)  N34FMI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N34FMI"
files <-
  readxl::excel_sheets(paste0("input_within/raw_data/", dataset, ".xlsx"))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
lares::corr_cross(
  na.omit(data$environment %>% select(everything())),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"# display top 10 couples of variables (by correlation coefficient)
)
ENV <- data$environment %>%
  mutate_at(vars(-waterT, -PH, -Order), log1p) %>%
  select(-Order, -TP, -COD) #removed because of NAs and high correlation (r > 0.7)

# Trait categorization and transformation ----

TR <- data$traits %>% mutate_all(ordered)

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   ord = "podani",
#   groups.weight = TRUE,
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (16)  N38FMI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N38FMI"
files <-
  readxl::excel_sheets(paste0("input_within/raw_data/", dataset, ".xlsx"))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----

ENV <- data$environment %>%
  mutate_at(vars(-ENV11, -ENV10), log1p)
# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(
#     rep(1, 7),
#     rep(2, 2),
#     rep(3, 3),
#     rep(4, 4),
#     rep(5, 8),
#     rep(6, 4),
#     rep(7, 5),
#     rep(8, 9),
#     rep(9, 8),
#     rep(10, 4),
#     rep(11, 8)
#   ),
#   fuzzy = 1:11,
#   ord = "podani",
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (17)  N38MFI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N38MFI"
files <-
  readxl::excel_sheets(paste0("input_within/raw_data/", dataset, ".xlsx"))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----

ENV <- data$environment
# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at("Size.Class",
            ordered,
            levels = c("S1", "S2", "S3", "S4", "S5", "S6")) %>%
  mutate_at("Home.Range", ordered, levels = c("Sed", "Mob", "VMob")) %>%
  mutate_at("Activity", ordered, levels = c("Day", "Both", "Night")) %>%
  mutate_at("Schooling",
            ordered,
            levels = c("Sol", "Pair", "SmallG", "MedG", "LargeG")) %>%
  mutate_at("Level.water", ordered, levels = c("Bottom", "Low", "High")) %>%
  mutate_at("Diets",
            ordered,
            levels = c("HD", "HM", "IS", "IM", "PK", "FC", "OM"))
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----
# 
# func_dis <- gawdis::gawdis(x = TR,
#                            ord = "podani",
#                            w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (18)  N38TTP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N38TTP"
files <-
  readxl::excel_sheets(paste0("input_within/raw_data/", dataset, ".xlsx"))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----

ENV <- data$environment %>%
  mutate_at(vars(-Sand, -Clay), log1p)

# Trait categorization and transformation ----

TR <- data$traits

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   ord = "podani",
#   groups.weight = TRUE,
#   w.type = "optimized"
# )
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (19)  N39FMI_2
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N39FMI_2"
files <-
  readxl::excel_sheets(paste0("input_within/raw_data/", dataset, ".xlsx"))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----

ENV <- data$environment %>% mutate_at("COND", log1p)

# Trait categorization and transformation ----

TR <- data$traits %>%
  #  mutate_at("Main_size",ordered, levels=c("2.5-5","5-10","10-20","20-40")) %>%
  #  mutate_at("Second_size",ordered, levels=c("2.5-5","5-10","10-20","20-40")) %>%
  #   mutate_at(vars(-Main_size,-Second_size), as.factor) %>%
  select(!starts_with(c("Main", "Second"))) %>% #Use only fuzzy-coded traits
  mutate_all(as.numeric) #convert to numeric
rownames(TR) <- colnames(TA)

# # Pairwise functional similarity  -----
# 
# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(rep(1, 4),
#              rep(2, 4),
#              rep(3, 9),
#              rep(4, 4)),
#   ord = "podani",
#   fuzzy = 1:4,
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (20)  N39FMI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N39FMI"
files <-
  readxl::excel_sheets(paste0("input_within/raw_data/", dataset, ".xlsx"))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
lares::corr_cross(
  na.omit(data$environment %>% select(everything())),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"# display top 10 couples of variables (by correlation coefficient)
)
ENV <- data$environment %>% mutate_at("COND", log1p)

# Trait categorization and transformation ----

TR <- data$traits %>%
  select(!starts_with(c("Main", "Second"))) %>% #Use only fuzzy-coded traits
  mutate_all(as.numeric) #convert to numeric

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(rep(1, 2),
#              rep(2, 5),
#              rep(3, 6),
#              rep(4, 4)),
#   fuzzy = 2:4,
#   ord = "podani",
#   groups.weight = TRUE,
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (21)  N39TTP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N39TTP"
files <-
  readxl::excel_sheets(paste0("input_within/raw_data/", dataset, ".xlsx"))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
lares::corr_cross(
  na.omit(data$environment %>% select(everything())),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"# display top 10 couples of variables (by correlation coefficient)
)
ENV <- data$environment %>%
  mutate_at(vars(-starts_with("Cover_"), -Clay, -Sand, -pH_H2O, -Inclination), log1p)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_all(as.numeric) #convert to numeric

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(x = TR,
#                            ord = "podani",
#                            w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (22)  N40FMI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N40FMI"
files <-
  readxl::excel_sheets(paste0("input_within/raw_data/", dataset, ".xlsx"))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
lares::corr_cross(
  na.omit(data$environment %>% select(everything())),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"# display top 10 couples of variables (by correlation coefficient)
)
ENV <- data$environment %>%
  select(-Oxp) %>%  #correlation above >.7
  mutate_at(vars(COND, ALCA, NO3, PO4), log1p)

# Trait categorization and transformation ----

TR <- data$traits %>%
  select(!starts_with(c("Main", "Second"))) %>% #Use only fuzzy-coded traits
  mutate_all(as.numeric) #convert to numeric

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----
# 
# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(rep(1, 2),
#              rep(2, 5),
#              rep(3, 6),
#              rep(4, 4)),
#   fuzzy = 2:4,
#   ord = "podani",
#   groups.weight = TRUE,
#   w.type = "optimized"
# )

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(x = TR,
#                            ord = "podani",
#                            w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (23)  N40TAT
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N40TAT"
files <-
  readxl::excel_sheets(paste0("input_within/raw_data/", dataset, ".xlsx"))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----

ENV <- data$environment %>%
  select(
    -Minimum_precipitation,
    -Maximum_precipitation,
    -Minimum_temperature,
    -Maximum_temperature,
    -Precipitation
  )   #correlation above >.7

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(
    vars(HuntingStrategy, Strata, Activity, Feeding, Web),
    as.factor
  )

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----
# 
# func_dis <- gawdis::gawdis(x = TR,
#                            ord = "podani",
#                            w.type = "optimized")

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(x = TR,
#                            ord = "podani",
#                            w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (24)  N41FMI
#'@ header *****************************************************************


#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N41FMI"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----

ENV <- data$environment %>%
  mutate_at(
    vars(
      TSS,
      TDS,
      AFDM,
      HCO3,
      O2,
      ClorA,
      ClorC,
      DOP,
      Fl,
      Cl,
      NO2,
      Br,
      NO3,
      DIP,
      SO4,
      K,
      DIN
    ),
    log1p
  )


# Trait categorization and transformation ----

TR <- data$traits %>%
  select(-b5) %>%  #no species with this trait
  mutate_all(as.numeric) #convert to numeric

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(
#     rep(1, 7),
#     rep(2, 4),
#     rep(3, 2),
#     rep(4, 3),
#     rep(5, 8),
#     rep(6, 4),
#     rep(7, 4),
#     rep(8, 8),
#     rep(9, 7),
#     rep(10, 9),
#     rep(11, 9),
#     rep(12, 3),
#     rep(13, 3),
#     rep(14, 5),
#     rep(15, 6),
#     rep(17, 4)
#   ),
#   fuzzy = 1:17,
#   ord = "podani",
#   w.type = "optimized"
# )

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (25)  N41TTP_2
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N41TTP_2"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
lares::corr_cross(
  na.omit(data$environment %>% select(everything())),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"# display top 10 couples of variables (by correlation coefficient)
)
ENV <- data$environment

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(flowering_start, flowering_end), as.factor) #convert to numeric

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(rep(1, 2),
#              rep(2, 3)),
#   ord = "podani",
#   groups.weight = TRUE,
#   w.type = "optimized"
# )
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (26)  N41TTP_3
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N41TTP_3"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
lares::corr_cross(
  na.omit(data$environment %>% select(everything())),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"# display top 10 couples of variables (by correlation coefficient)
)
ENV <- data$environment %>%
  select(-PercCurrGrass, -PercGrassRed) %>%
  mutate_at(vars(MAP, PPA, CPA), log1p)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(
    vars(
      Disp,
      Corolla,
      LifeForm,
      LeafAnat,
      Phytogeo,
      HerbivPref,
      MeanPlantHeight
    ),
    as.factor
  ) #convert to numeric

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (27)  N41TTP
#'@ header *****************************************************************


#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N41TTP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
lares::corr_cross(
  na.omit(data$environment %>% select(everything())),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"# display top 10 couples of variables (by correlation coefficient)
)
ENV <- data$environment %>%
  mutate_at(vars(-pH_H2O_ , -Elevation), log1p)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))


#'@ header *****************************************************************
#'@ dataset (28)  N42TTP_2
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N42TTP_2"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
lares::corr_cross(
  na.omit(data$environment %>% select(everything())),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"# display top 10 couples of variables (by correlation coefficient)
)
ENV <- data$environment %>%
  select(-Organic_content, -C_gr_100_gr, -Silt) %>%
  mutate_at(
    vars(Altitude, Phosphate_mg_kg, Available_Phosphorus_mg_kg),
    log1p
  )

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----
# 
# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))


#'@ header *****************************************************************
#'@ dataset (29)  N42TTP_3
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N42TTP_3"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
lares::corr_cross(
  na.omit(data$environment %>% select(everything())),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"# display top 10 couples of variables (by correlation coefficient)
)
ENV <- data$environment %>%
  select(-Organic_matter) %>%
  mutate_at(
    vars(
      Elevation,
      Max__microlief__cm_ ,
      C_N,
      Carbonate,
      Available_P,
      Available_K
    ),
    log1p
  )

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (30)  N42TTP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N42TTP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
lares::corr_cross(
  na.omit(data$environment %>% select(everything())),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"# display top 10 couples of variables (by correlation coefficient)
)

ENV <- data$environment %>%
  mutate_at(vars(-ph2, -water2, -dist_sea, -burial), log1p)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (31)  N43MMI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N43MMI"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
lares::corr_cross(
  na.omit(data$environment %>% select(everything())),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"# display top 10 couples of variables (by correlation coefficient)
)

ENV <- data$environment %>%
  select(-sstmax, -BSI) %>%
  mutate_at(vars(-Width, -W, -Tb, -ER), log1p)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(rep(1, 4),
#              rep(2, 3),
#              3, 4, 5, rep(6, 4)),
#   fuzzy = c(2, 6),
#   ord = "podani",
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (32)  N43TTP_1
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N43TTP_1"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-Texture_1_0_5_mm) %>%
  mutate_at(vars(starts_with("Texture_"), "P_(mg.L)"), log1p)

lares::corr_cross(
  na.omit(ENV %>% select(everything())),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"# display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----
# 
# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (33)  N43TTP_2
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N43TTP_2"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-D) %>% #remove because of high correlation
  mutate_at(vars(
    -starts_with("Cover_"), -ends_with("_pct"), -pH
  ), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (34)  N43TTP_3
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N43TTP_3"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(-pH_H2O), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----
# 
# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (35)  N43TTP_4
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N43TTP_4"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-Na_K) %>% #remove because of high correlation
  mutate_at(vars(-pH, -Ca), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (36)  N43TTP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N43TTP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-Silt) %>% #removed because of high correlation with Sand.
  mutate_at(vars(
    Inclination, Max_microrelief , EC, Organic_matter
  ), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (37)  N44TAT
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N44TAT"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-ED400)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <-
  data$traits %>% mutate_all(as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (38)  N44TBI_2
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N44TBI_2"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(DietBreed, ForagGuild, Nest, Migr, Date), as.factor) %>%
  mutate_at(vars(Home), ordered)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (39)  N44TBI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N44TBI"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-ED400, -SHDI400) %>%
  mutate_at(vars("DISDEC", "AREA400"), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(trend, forag, diet, nest, migr, biog, date), as.factor) %>%
  mutate_at(vars(eggs, mass, natio, regio, range), ordered)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (40)  N44TBT
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N44TBT"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(
    -Perim,
    -RelPerim,
    -Npatch_inside,
    -ReserveArea,
    -Npatch_outside,
    -Nbiotop_inside
  ) %>%
  mutate_at(vars(-Nbiotop_outside), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(
    vars(
      mobility,
      density,
      rangesize,
      fertility,
      overwinter,
      hostplant,
      RedList
    ),
    ordered
  )

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (41)  N44TTP_2
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N44TTP_2"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_all(log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(FS, DM, LS, LF), as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (42)  N44TTP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N44TTP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_all(log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (43)  N45FFI_2
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N45FFI_2"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(-Ele), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(Feeding_type, Size), as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----
# 
# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(1,
#              rep(2, 8),
#              rep(3, 3),
#              rep(4, 2),
#              rep(5, 6),
#              6,
#              7),
#   ord = "podani",
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (44)  N69FFI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N45FFI"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-MaxDepth, -TotN, -CompDepth) %>% #removed because of high correlatio with MeanDepth, and TotP
  mutate_all(log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(Origin, Thermal_guild, Season), as.factor)

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----
# 
# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(1,
#              2,
#              8,
#              rep(3, 4),
#              rep(4, 3),
#              rep(5, 4),
#              rep(6, 3),
#              rep(7, 2)),
#   ord = "podani",
#   w.type = "optimized"
# )
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (45)  N46FFI_2
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N46FFI_2"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(-Subm, -cover), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  select(-MouthSup, -FeedSurf) #removed because there were no species with this trait.
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----
# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(
#     rep(1, 5),
#     rep(2, 2),
#     rep(3, 3),
#     rep(4, 2),
#     rep(5, 2),
#     rep(6, 3),
#     rep(7, 3),
#     rep(8, 2)
#   ),
#   ord = "podani",
#   groups.weight = FALSE,
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (46)  N46FFI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N46FFI"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(-Subm, -cover), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----
# 
# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(
#     rep(1, 5),
#     rep(2, 3),
#     rep(3, 3),
#     rep(4, 2),
#     rep(5, 3),
#     rep(6, 3),
#     rep(7, 3),
#     rep(8, 2)
#   ),
#   ord = "podani",
#   groups.weight = FALSE,
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (47)  N46FMI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N46FMI"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  select(!starts_with(c("Main_", "Second_"))) %>%
  mutate_at("Size", ordered)


rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(1,
#              rep(2, 5),
#              rep(3, 6),
#              rep(4, 4)),
#   fuzzy = 2:4,
#   ord = "podani",
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (48)  N46TAT
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----
require(tidyverse)
dataset <- "N46TAT"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
source("scripts/Functions/initial_checking.R")
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  select(-larval_substrate) %>% #The trait is identical to the values in Nesting Guild!
  mutate_at("Nesting_guild", as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----
# 
# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(rep(1, 2), rep(2, 2), 3, 4),
#   ord = "podani",
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (49)  N47FAP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----
require(tidyverse)
dataset <- "N47FAP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
source("scripts/Functions/initial_checking.R")
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(-pH), log1p)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars("Life.form", "Perennation"), as.factor) %>%
  mutate_at("Normal.method.of.propagation", ordered) %>%
  mutate_at("Maximum.height", as.numeric)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----
# 
# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (50)  N47FFI
#'@ header *****************************************************************


#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N47FFI"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
source("scripts/Functions/initial_checking.R")
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(
    vars(
      bank_vegetation_H,
      depth,
      wetted_width,
      curr_velocity,
      O2_mg_L,
      conductivity,
      NO2_N_ug_L,
      NO3_mg_L,
      NH4_mg_L,
      Ca_mg_L,
      P_ug_L,
      PO43_mg_L
    ),
    log1p
  )


lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (51)  N47TAT_2
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----
dataset <- "N47TAT_2"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
source("scripts/Functions/initial_checking.R")
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(-Forest, -Slope), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at("Humidity_preference", as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))


#'@ header *****************************************************************
#'@ dataset (52)  N47TAT_3
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N47TAT_3"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
source("scripts/Functions/initial_checking.R")
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-TEMP4_9) %>% #high correlation with Elevation
  mutate_at(vars(-Slope), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(Larval_feeding_guild, larval_substrate), as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(rep(1, 2), rep(2, 2), 3, 4),
#   ord = "podani",
#   w.type = "optimized"
# )
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (53)  N47TAT_4
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N47TAT_4"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
source("scripts/Functions/initial_checking.R")
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(-Slope, -Forest), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  select(-Family)  #Not a functional trait

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(rep(1, 2), rep(2, 3), 3),
#   ord = "podani",
#   groups.weight = TRUE,
#   w.type = "optimized"
# )
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (54)  N47TAT
#'@ header *****************************************************************


#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N47TAT"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
source("scripts/Functions/initial_checking.R")
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(-Slope, -Forest), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(hind_wing_development, Trophic_level), as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(rep(1, 2), 2, 3, 4, rep(5, 2)),
#   ord = "podani",
#   groups.weight = TRUE,
#   w.type = "optimized"
# )
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (55)  N47TTP_2
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N47TTP_2"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files


# initial checking ----
source("scripts/Functions/initial_checking.R")
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(-Max_microlief_cm), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (56)  N47TTP_3
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N47TTP_3"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
source("scripts/Functions/initial_checking.R")
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-Total_vegetation_cover) %>% #Because of high correlation with Cover_herb
  mutate_at(
    vars(
      Elevation,
      Microrelief_cm,
      Soil_depth_median,
      Soil_depth_range,
      Mean_height_herb_m,
      Maximum_height_herb_m,
      soil_tot_C,
      EC,
      Live_plants_g_m2,
      Dead_matter
    ),
    log1p
  )

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (57)  N47TTP_4
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N47TTP_4"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
source("scripts/Functions/initial_checking.R")
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(-pH), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(Type_Bio, Dispersal), as.factor)

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(1, 2, 3, 4, 5, 6, 7, 7, 8, 8),
#   groups.weight = TRUE,
#   ord = "podani",
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (58)  N47TTP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N47TTP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-soil_organic_C) %>%
  mutate_at(
    vars(
      Elevation,
      Max_microlief,
      Soil_depth_mean,
      Skeleton_content,
      EC,
      Soil_N_content,
      C_N,
      Organic_matter,
      Available_P
    ),
    log1p
  )

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (59)  N48FAB
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")
# Read dataset ----

dataset <- "N48FAB"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(Area), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  select(-Ter) # removed because all species have value 1
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(rep(1, 3), rep(2, 2), rep(3, 3), 4, rep(5, 2), 4, 6, rep(7, 2)),
#   groups.weight = TRUE,
#   ord = "podani",
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (60)  N48FBD
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N48FBD"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-Order, -TDS) %>%
  mutate_at(vars(
    Cond, Ca2., Mg2., PO4., TP, COD, NH4., Elevation
  ), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(rep(1, 13), rep(2, 4), 3, 4),
#   ord = "podani",
#   groups.weight = TRUE,
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (61)  N48FMI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N48FMI"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-Order, -TDS) %>%
  mutate_at(vars(
    Cond, Ca2., Mg2., PO4., TP, TN, COD, NH4., Elevation
  ), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_all(ordered)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (62)  N48TTP
#'@ header *****************************************************************


#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----
dataset <- "N48TTP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-Sand_pct, -Carbonate, -EC) %>%  #removed because of high correlation with Silt, Soil_C_content and CN_ratio
  mutate_at(
    vars(
      Elevation,
      Max_microlief,
      Skeleton_content,
      Soil_N_content,
      Soil_C_content,
      CN_ratio
    ),
    log1p
  )

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (63)  N48TVASCPLANT
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----
dataset <- "N48TVASCPLANT"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(mil_nat_buff200), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(
    "Selfsterility_selfincompatibility",
    recode_factor,
    SI = 0,
    SC = 1
  ) %>%  #two-level factor should be binary
  mutate_at(
    vars(
      -Floral_rewards,
      -Number_flowering_phases,
      -Selfsterility_selfincompatibility
    ),
    as.factor
  ) %>%
  mutate_at(
    vars(
      Floral_rewards,
      Number_flowering_phases,
      Selfsterility_selfincompatibility
    ),
    as.numeric
  )


# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (64)  N49TAT
#'@ header *****************************************************************



#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----
dataset <- "N49TAT"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(-T_mean, -NDVI_wint , -NDVI_summ), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(1, 2, 2, 1, 3, 3, 4, 4, 4, 5, 5, 5),
#   groups.weight = TRUE,
#   ord = "podani",
#   w.type = "optimized"
# )
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (65)  N49TBI
#'@ header *****************************************************************


#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----
dataset <- "N49TBI"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(forag, diet, nest, migr, date), as.factor) %>%
  mutate_at("home", ordered, levels = c("small", "mid", 'large'))
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (66)  N49TBT
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N49TBT"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(forag, diet, nurse, migr, date, home), as.factor) %>%
  mutate_at("home", ordered, levels = c("small", "mid", "large"))

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (67)  N49TTP_2  @This_dataset_only_has_binary_environmental_variables_(TEST)
#'@ header *****************************************************************


#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N49TTP_2"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(1, 2, 3, 4, 5, 6, 6, 7, 8, 9, 10, 01, 11, rep(12, 7)),
#   ord = "podani",
#   w.type = "optimized"
# )
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (68)  N49TTP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N49TTP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  select(
    -Life_form,
    -Veget_propag,
    ARC,
    -Type_of_reproduction,
    -ARC,
    -GrwtForm
  )
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----
# 
# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(
#     1,
#     2,
#     3,
#     4,
#     rep(5, 8),
#     rep(6, 3),
#     rep(7, 7),
#     rep(8, 3),
#     rep(9, 3),
#     rep(10, 9),
#     11,
#     rep(12, 2),
#     rep(13, 3),
#     11
#   ),
#   ord = "podani",
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (69)  N50TAT_1
#'@ header *****************************************************************


#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N50TAT_1"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(PARA_MN500, SHDI500, PLAND_G_500), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(
    vars(Stratum, EcolTyp, ActTyp, GildeCardosoEtAl2011),
    as.factor
  )

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (70)  N50TAT_2
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N50TAT_2"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(PARA_MN500, SHDI500, PLAND_G_500), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(Feeding_short, Stratum), as.factor)  %>%
  mutate_at(vars(Mean_BodySize, Wingclass), as.numeric)

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))


#'@ header *****************************************************************
#'@ dataset (71)  N50TAT_3
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N50TAT_3"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(Deadwood, AgeOldestTree), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (72)  N50TAT_4
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N50TAT_4"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(
    TimesinceProt , Deadwood, VeteranTrees, ConStand
  ), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (73)  N50TAT_5
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N50TAT_5"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at("NSCP", log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(Eggs_Batch, Voltinism, Overwinter, Diet), as.factor) %>%
  mutate_at("Flight_Per", ordered, levels = c("Short", "Medium", "Long"))

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----
# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (74)  N50TFG
#'@ header *****************************************************************


#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N50TFG"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <-
  data$environment %>% mutate_at(vars(AREA, ALTITUDE), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(
    vars(Fruit_body_type, Spore_wall, Host_tree, Rot_type, Ecology),
    as.factor
  ) %>%
  mutate_at("Rhizomorphs", recode, No = 0, Yes = 1) %>%
  mutate_at("Size_class", ordered) %>%
  mutate_at(
    vars(Rhizomorphs, Cystidia.Paraphyses, Ornamented_spores),
    as.numeric
  )
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (75)  N51TAT_1
#'@ header *****************************************************************


#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N51TAT_1"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(SoilDepth), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  select(-Feeding_guild_short) %>%  #all species are coded as "c"
  mutate_at(vars(Stratum_use_short), as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (76)  N51TAT_2
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N51TAT_2"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(SoilDepth), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(Feeding_guild_short, Stratum_use_short), as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (77)  N51TAT_3
#'@ header *****************************************************************


#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N51TAT_3"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(SoilDepth), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(Feeding_guild_short, Stratum_use_short), as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (78)  N51TAT_4
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N51TAT_4"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(SoilDepth), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p",
  rm.na = TRUE
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  select(-Feeding_guild_short) %>%  #All species are coded as "h"
  mutate_at(vars(Stratum_use_short), as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))


#'@ header *****************************************************************
#'@ dataset (79)  N51TAT_5
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N51TAT_5"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(SoilDepth), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p",
  rm.na = TRUE
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  select(-Stratum_use_short) %>%  #All species coded as "h"
  mutate_at(vars(Feeding_guild_short), as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))


#'@ header *****************************************************************
#'@ dataset (80)  N51TAT_6
#'@ header *****************************************************************


#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N51TAT_6"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(Stratum_use_short), as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))


#'@ header *****************************************************************
#'@ dataset (81)  N51TAT_7
#'@ header *****************************************************************


#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N51TAT_7"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(Feeding_guild_short, Stratum_use_short), as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))


#'@ header *****************************************************************
#'@ dataset (82)  N51TAT_8
#'@ header *****************************************************************


#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N51TAT_8"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(Feeding_guild_short, Stratum_use_short), as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))


#'@ header *****************************************************************
#'@ dataset (83)  N51TAT_9
#'@ header *****************************************************************


#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N51TAT_9"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(
    vars(dispersal_ability, ovariole_number),
    ordered,
    levels = c("low", "med", "high")
  ) %>%
  mutate_at(vars(body_size), ordered, levels = c("small", "med", "large")) %>%
  mutate_at(
    vars(
      passive_dispersal,
      oviposition_in_plants,
      oviposition_at_ground
    ),
    as.numeric
  )

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))


#'@ header *****************************************************************
#'@ dataset (84)  N51TAT_10
#'@ header *****************************************************************


#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N51TAT_10"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(
    vars(Texture, Org, AvailP, AvailK, Plants_m2, Stem.density, Elevation),
    log1p
  )

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(CLG, CLB, WIN, PRS, FOA, BRE, EME), as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))


#'@ header *****************************************************************
#'@ dataset (85)  N51TBI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N51TBI"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(pop50, pop200), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(food, foraging.technique), as.factor) %>%
  mutate_at(
    "mig1.strategy",
    ordered,
    levels = c("sedentary", "short", "long")
  )

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (86)  N51TTP_2
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N51TTP_2"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-PROTECTION, -SpLog) %>%  #binary variables
  mutate_at(vars(BRYOPHYTES, AREA, ALTITUDE), sqrt) #square root approximates better to normality

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(DispType), as.factor) %>%
  mutate_at(vars(rosette, semirosette, regular), vegan::decostand, "pa") %>% #convert these to binary.
  mutate_at(
    vars(
      rosette,
      semirosette,
      regular,
      C,
      S,
      CSR,
      SC,
      CR,
      SR,
      annual,
      biannual,
      herbperennial,
      woodyperennial
    ),
    as.numeric
  )

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(1, 2, 3, 4, rep(5, 3), rep(6, 6), rep(7, 2), rep(8, 2)),
#   fuzzy = c(8),
#   ord = "podani",
#   w.type = "optimized"
# )
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (87)  N51TTP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N51TTP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-EC) %>% #Correlated with pH_H2o
  mutate_at(
    vars(
      Elevation,
      Soil_depth_mean,
      Skeleton_content,
      loss_at_ignition,
      Soil_N_content
    ),
    log1p
  )

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (88)  N52FPP
#'@ header *****************************************************************


#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N52FPP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(cond, visib..m., NO3, NH4, TN, PO4), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(1, 2, 2, 2, 3, 4, 5, 6, 7, 8),
#   ord = "podani",
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (89)  N53TTP
#'@ header *****************************************************************


#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----
dataset <- "N53TTP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (90)  N54MPP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N54MPP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(SurfacePAR, Kd490, Phosphate, MLD), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (91)  N55TAT
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N55TAT"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(soil, S3, S1, S2), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(wing, diet2, HabitatAff), as.factor)

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (92)  N55TBP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N55TBP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-tot_sox) %>% #correlated with tot_nhx
  mutate_at(vars(tot_nhx, PTwarm), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (93)  N55TTP_2
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N55TTP_2"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-tot_sox) %>% #correlated with tot_nhx
  mutate_at(vars(tot_nhx, PTwarm), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----
# 
# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (94)  N55TTP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N55TTP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(Max_microlief, Soil_depth_mean), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (95)  N56FAP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N56FAP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(
    vars(Chlorophyll_a, Total_nitrogen, Total_phosphorus, Area, Depth),
    log1p
  )


lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(Life_form, Perennation), as.factor)

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (96)  N57TTP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N57TTP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  select(-Chamaeph, -Hemiphan, -LS_bienn, -PV_insct)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(
#     rep(1, 3),
#     2,
#     3,
#     rep(4, 2),
#     rep(5, 3),
#     rep(6, 2),
#     rep(7, 2),
#     rep(8, 3),
#     rep(9, 2),
#     10,
#     11,
#     12,
#     13,
#     14,
#     15
#   ),
#   fuzzy = c(5, 6, 7, 9),
#   ord = "podani",
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (97)  N59MAP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N59MAP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-GS_0.063mm_1) %>%
  mutate_at(vars(NH4_wc, NH4_pw, starts_with("GS_")), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (98)  N59MMI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N59MMI"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-Median_grain_size_um, -PW_Si, -BW_NO2.N) %>%
  mutate_at(vars(Depth_m, C.N, Chla, starts_with(c(
    "BW_", "PW_"
  ))), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(
#     rep(1, 5),
#     rep(2, 3),
#     rep(3, 4),
#     rep(5, 5),
#     rep(6, 2),
#     rep(7, 3),
#     rep(8, 4)
#   ),
#   fuzzy = 1:8,
#   ord = "podani",
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (99)  N59MPP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N59MPP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-Kd490) %>%  # correlated with temperature
  mutate_at(vars(SurfacePAR, Phosphate, MLD), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (100)  N59TTP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N59TTP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(Nitur, Fosfor, Moister, Kalium), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (101)  N61FBD
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N61FBD"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-TN_microgL, -Conductivity) %>%
  mutate_at(
    vars(
      Colour,
      TP_microgL,
      NO3,
      DO_concentr.,
      Stream_width,
      Current_velocity
    ),
    log1p
  )


lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  select(-Filament_colony)

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----
# length(c(1, 2, 3, rep(4, 13), rep(5, 4), 6, 7))
# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(1, 2, 3, rep(4, 12), rep(5, 4), 6, 7),
#   ord = "podani",
#   w.type = "optimized"
# )
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (102)  N62FBD
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N62FBD"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(
    vars(
      tot_P,
      Colour_Pt,
      conductivity,
      deciduous_trees,
      width,
      velocity,
      depth
    ),
    log1p
  )

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  select(-Filament_colony, -Stellate_colony)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(1, 2, 3, rep(4, 11), rep(5, 4), 6, 7),
#   ord = "podani",
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (103)  N62FMI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N62FMI"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-tot.N, -depth) %>%
  mutate_at(
    vars(
      tot.P,
      colour,
      conductivity,
      deciduous.trees,
      velocity,
      discharge
    ),
    log1p
  )

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(HTG, FFG), as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (104)  N62MBD
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N62MBD"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-Conductivity, -pH) %>%
  mutate_at(vars(
    -c("Water_temperature", "Precipitation", "Air_temperature")
  ), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <-
  data$traits %>% mutate_at(vars("Body_size", "Life-form"), as.factor)

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(1, 2, rep(3, 5), rep(4, 4), 5),
#   ord = "podani",
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (105)  N63FAP_2
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N63FAP_2"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-Conductivity, -TN, -Colour) %>%
  mutate_at(vars(Width_mean, Alkalinity, TP, Turbidity), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(Life.form, Perennation), as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (106)  N63FAP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N63FAP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(AreaKm, TP, TN, Conductivity, colour), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(Life.form, Perennation), as.factor)

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (107)  N63MPP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N63MPP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-Phosphate) %>%  #High correlation with Nitrate and Silicate (r > 0.7)
  mutate_at(vars(MLD, SurfacePAR), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (108)  N63TAT
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N63TAT"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(
    Bio1,
    Bio8,
    Bio10,
    Bio11,
    Bio12,
    Bio13,
    Bio14,
    Bio15,
    Bio16,
    Bio17,
    Bio18,
    Bio19
  ) #Not correlated among themselves

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (109)  N63TBI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N63TBI"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(Mig, Hab), as.factor) %>%
  mutate_at("Mass", as.numeric)

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (110)  N64FBD
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N64FBD"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(
    vars(
      Depth_cm,
      Width_m,
      Water_Speed_m.per.s,
      TOC_ppm,
      NOX.N,
      NO2.N,
      PO4.P,
      NH4.N,
      TN,
      TP
    ),
    log1p
  )

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  select(-Stellate.colony) #no species with this trait

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(rep(1, 11), rep(2, 4), 3, 4),
#   ord = "podani",
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (111)  N65FBD
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N65FBD"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(
    vars(
      particle_size_dm3,
      current_velocity,
      conductivity,
      colour,
      totP
    ),
    log1p
  )

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(rep(1, 12), rep(2, 4), 3, 4),
#   ord = "podani",
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))


#'@ header *****************************************************************
#'@ dataset (112)  N65TBF
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N65TBF"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(
    vars(Width.of.larval.host.plant.use, Growth.form.of.host.plant),
    as.factor
  )

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (113)  N66FBD
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N66FBD"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(
    vars(TN, conductivity, velocity, depth, width, particle_size),
    log1p
  )

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  select(-Stellate_colony) #No species with this trait
rownames(TR) <- colnames(TA)
colSums(TR)
# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(rep(1, 3), rep(2, 11), rep(3, 4), 4, 5),
#   ord = "podani",
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (114)  N66FMI_2
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N66FMI_2"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(
    vars(
      distance_lakeabove,
      are_lakeabove,
      catch_area,
      lakenes_spro,
      altitude
    ),
    log1p
  )

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(FFG, HTG), as.factor) %>%
  mutate_at(
    vars(SIZE),
    ordered,
    levels = c("0.25-0.5cm", "0.5-1cm", "1-2cm", "2-4cm", "4-8cm")
  )
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (115)  N66FMI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N66FMI"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(FFG, SUBASS), as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (116)  N67TTP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N67TTP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(
    Mesotopo, peat_depth, soil_depth, Moisture
  ), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (117)  N68FBD
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N68FBD"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(spc, total.nitrogen, si, ca, mg), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(1, rep(2, 5), rep(3, 3), 4, 5),
#   ord = "podani",
#   w.type = "optimized"
# )
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (118)  N69FMI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N69FMI"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(
    vars(TN, color, manganese, conductivity, velocity, depth, width),
    log1p
  )

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits  %>%
  mutate_at(vars(FFG, HTG), as.factor) %>%
  mutate_at(
    vars(Size),
    ordered,
    levels = c("0.25-0.5 cm", "0.5-1 cm", "1-2 cm", "2-4 cm", "4-8 cm")
  )
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (119)  N70TBI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N70TBI"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (120)  N70TBP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N70TBP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at("general_growth_form", as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))


#'@ header *****************************************************************
#'@ dataset (121)  N70TLC
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N70TLC"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(photobiont, Cyanobacteria), as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(rep(1, 4), 2, 3),
#   ord = "podani",
#   w.type = "optimized"
# )
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))
#'@ header *****************************************************************
#'@ dataset (122)  N70TTP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "N70TTP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))


#'@ header *****************************************************************
#'@ dataset (123) N78TTP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "N78TTP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(
    Mesotopo, peat_depth, soil_depth, Moisture
  ), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p" # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (124)  S01MFI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S01MFI"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(salinity, CHLOmean, CHLOrange), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at("Size.Class", ordered, levels = paste0("S", 1:6)) %>%
  mutate_at("Home.Range", ordered, levels = c("Sed", "Mob", "VMob")) %>%
  mutate_at("Activity", ordered, levels = c("Day", "Both", "Night")) %>%
  mutate_at(
    "Schooling",
    ordered,
    levels = c("Sol", "Pair", "SmallG", "MedG", "LargeG")
  ) %>%
  mutate_at("Level.water", ordered, levels = c("Bottom", "Low", "High")) %>%
  mutate_at(
    "Diets",
    ordered,
    levels = c("HD", "HM", "IS", "IM", "PK", "FC", "OM")
  )
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (125)  S01TBI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S01TBI"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(Guild), as.factor) %>%
  mutate_at(
    vars(Size),
    ordered,
    levels = c("S-VS", "S-S", "S-M", "S-L", "S-VL")
  )
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (126)  S02TBT
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S02TBT"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(NT, BT, L, R, VC, TH), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(dietary_specialization), ordered) %>%
  mutate_at("vertical_stratification", as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (127)  S03FBD
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "S03FBD"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(
    Color, Conductivity, TotN, width, velocity, depth
  ), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  select(
    -Filament_colony,
    -zig.zag_colony,
    Stellate_colony,
    -Arbuscular_colony,
    -Planktonic
  ) # No species with these traits

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----
# names(TR)
# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(rep(1, 3), rep(2, 10), rep(3, 3)),
#   ord = "podani",
#   w.type = "optimized"
# )
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (128)  S05MFI_2
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "S05MFI_2"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment


lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(FGc), as.factor) %>%
  mutate_at("FishingPressure", ordered, levels = c("N", "O", "I", "P"))

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (129)  S05MFI_3
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "S05MFI_3"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(Tra, DO), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(1:11, rep(12, 3), 13),
#   ord = "podani",
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (130)  S05MFI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")

# Read dataset ----

dataset <- "S05MFI"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at("FGc", as.factor) %>%
  mutate_at("FishingPressure", ordered, levels = c("N", "O", "I", "P"))

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (131)  S05MFR
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S05MFR"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(
    vars(
      Sedimentary.areas,
      Coral.formations,
      sparse.coral,
      Dense.reef.flat,
      Scattered.reef.flat,
      human.settlement,
      Visibility
    ),
    log1p
  )

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(Form, Symbiont), as.factor)

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (132)  S05MPP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S05MPP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-Phosphate, -PARz, -MLD) %>% #Highly correlated
  mutate_at(vars(SurfacePAR, Nitrate, Nitrite, Silicate), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (133)  S06MCO
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S06MCO"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(ColoShape, ColoForm), as.factor) %>%
  mutate_at(vars(CoralSize_ORDINAL, AdaptStrat_ORDINAL), ordered)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (134)  S07MFI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S07MFI"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(salinity, Chlorophyll_mean), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at("Size.Class", ordered, levels = paste0("S", 1:6)) %>%
  mutate_at("Home.Range", ordered, levels = c("Sed", "Mob", "VMob")) %>%
  mutate_at("Activity", ordered, levels = c("Day", "Both", "Night")) %>%
  mutate_at(
    "Schooling",
    ordered,
    levels = c("Sol", "Pair", "SmallG", "MedG", "LargeG")
  ) %>%
  mutate_at("Level.water", ordered, levels = c("Bottom", "Low", "High")) %>%
  mutate_at(
    "Diets",
    ordered,
    levels = c("HD", "HM", "IS", "IM", "PK", "FC", "OM")
  )

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (135)  S14FPP_1
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S14FPP_1"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(OD, TURB, DIN, SRP, TP), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(1, 2, rep(3, 3), 4:7),
#   ord = "podani",
#   w.type = "optimized"
# )
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (136)  S14FPP_2
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S14FPP_2"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(OD, TURB, DIN, SRP, TP), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----
# 
# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(1, 2, rep(3, 3), 4:7),
#   ord = "podani",
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (137)  S20FAB @Not_in_supplement
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S20FAB"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_all(log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(
    Feeding_behaviour, Position_eyes, Oral_disc
  ), as.factor)
rownames(TR) <- colnames(TA)


# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(rep(1, 3), 2:11),
#   ord = "podani",
#   w.type = "optimized"
# )

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (138)  S21TAT
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S21TAT"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(CH, LOFC, LL, LW, DBL, Bio), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(1:6, rep(7, 5), 8, rep(9, 3), rep(10, 2), 11, rep(12, 3)),
#   ord = "podani",
#   w.type = "optimized"
# )
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (139)  S22FPP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S22FPP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-SRP) %>%
  mutate_at(vars(Turb, NH4, NO3, TP, TOC, Si), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (140)  S22MPP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S22MPP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(
    SurfacePAR, Nitrate, Phosphate, Silicate, MLD
  ), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (141)  S22TAT
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S22TAT"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(CH, HILC, LL, LW, DBL, Bio, RGF), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <- gawdis::gawdis(
#   x = TR,
#   groups = c(1:6, rep(7, 5), 8, rep(9, 3), rep(10, 2), 11, rep(12, 3)),
#   ord = "podani",
#   w.type = "optimized"
# )
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (142)  S28TAT
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S28TAT"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(CN, P), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (143)  S29TTP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S29TTP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(Altitude, EC, Mass, Nitrogen), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (144)  S30TTP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S30TTP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(SMP_index, P, K, Al, Mg, CEC), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (145)  S33TAT
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S33TAT"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (146)  S34TAT_2
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S34TAT_2"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(
    vars(DistBush, TotArea, GdStruct, Water, Compost, Pets, Kids, NbTrees),
    log1p
  )

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(Hunting.style, Capture.lines), as.factor) %>%
  mutate_at("Size", ordered, levels = c("small", "medium", "large"))

rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (147)  S34TAT
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S34TAT"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(starts_with("sqKM_")), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(Hunting.style, Capture.lines), as.factor) %>%
  mutate_at("Size", ordered, levels = c("small", "medium", "large"))
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (148)  S34TTP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S34TTP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_all(log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(RegenStrat, RootStruct), as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (149)  S43TBI
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S43TBI"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-perc_grasslands, -perc_forests) %>%
  mutate_at(vars(patch_area), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits %>%
  mutate_at(vars(mass, eggs), ordered) %>%
  mutate_at(vars(biog, forag, diet, move, nest), as.factor)
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (150)  S47TTP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S47TTP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  mutate_at(vars(Mesotopo, soil_depth, Moisture), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")
# 
rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking"))

#'@ header *****************************************************************
#'@ dataset (151)  S62MPP
#'@ header *****************************************************************

#packages and functions
require(tidyverse)
source("scripts/Functions/initial_checking.R")


# Read dataset ----

dataset <- "S62MPP"
files <-
  readxl::excel_sheets(paste0(
    "input_within/raw_data/", dataset, ".xlsx"
  ))
data <- lapply(seq_along(files),
               function(x)
                 readxl::read_xlsx(paste0(
                   "input_within/raw_data/", dataset, ".xlsx"
                 ),
                 sheet = files[x]) %>% column_to_rownames(names(.)[1]))
names(data) <- files

# initial checking ----
data <- initial_checking(data)

TA <- data$species
C <- data$coordinates

# Environmental pre processing ----
ENV <- data$environment %>%
  select(-Silicate) %>% #highly correlated (r > 0.9 )
  mutate_at(vars(Phosphate, Nitrogen, Salinity), log1p)

lares::corr_cross(
  ENV %>% select(everything()),
  # name of dataset
  max_pvalue = 0.05,
  # display only significant correlations (at 5% level)
  top = 10,
  method = "p"
  # display top 10 couples of variables (by correlation coefficient)
)

# Trait categorization and transformation ----

TR <- data$traits
rownames(TR) <- colnames(TA)

# Pairwise functional similarity  -----

# func_dis <-
#   gawdis::gawdis(x = TR,
#                  ord = "podani",
#                  w.type = "optimized")

rmarkdown::render(
  "scripts/Process files/pre_processing.Rmd",
  output_format = "html_document",
  output_file = paste0(dataset, ".htm"),
  output_dir = "input_within/encrypt_process/"
)
rm(list = setdiff(ls(), "initial_checking")
)