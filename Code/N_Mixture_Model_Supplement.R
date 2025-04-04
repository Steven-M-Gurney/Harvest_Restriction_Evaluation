# 2 April 2025
# Updated script to fit N-mixture models with year as a factor variable

############################################
################ Preface ###################
############################################

# This script was used to run each sex-and-age class individually in the N-mixture model and interpret results.
# Below is an example for legal-antlered deer. 
# Note when running other sex-and-age classes, hardcoding is needed when loading data and saving output.

# Load packages.
library(tidyverse)
library(nimble)
library(MCMCvis)
library(ggplot2)
library(here)

############################################
############# Covariate Data ###############
############################################

# Read in covariate tables for each year (2019 - 2022) and add an identifier for year.
# Use index position of numbers in LocationName to extract more identifiers, including
# treatment (1 = NonAPR; 2 = APR), township pairing (4 pairs total), and township site
# (refers to GRTS-generated list). 
# Create a new column for siteID, which is based on row number (144 sites, 18 sites/township, and listed in order). 
# Select columns of interest.

classes <- c("Fawns", "Females", "Legal", "Sublegal")
for( i in 1:length(classes)){
  
  # Prepare 2019 covaiate table.
  cov19 <- read_csv(here::here("Data/Covariate_Tables/CovariateTable_2019.csv")) |> 
    add_column( year = 2019) |> 
    rename( LocationName = SiteID) |> 
    mutate( trt = substr( LocationName, 1, 1),
            twn = substr( LocationName, 2, 2), 
            site = substr( LocationName, 3, 5)) |> 
    mutate(siteID = row_number()) |> # 
    dplyr::select( LocationName, year, siteID, trt, twn, ClosedCover, AgCover)
  
  # Prepare 2020 covariate table.
  cov20 <- read_csv(here::here("Data/Covariate_Tables/CovariateTable_2020.csv")) |> 
    add_column( year = 2020) |> 
    rename( LocationName = SiteID) |> 
    mutate( trt = substr( LocationName, 1, 1),
            twn = substr( LocationName, 2, 2), 
            site = substr( LocationName, 3, 5)) |> 
    mutate(siteID = row_number()) |> 
    dplyr::select( LocationName, year, siteID, trt, twn, ClosedCover, AgCover)
  
  # Prepare 2021 covariate table.
  cov21 <- read_csv(here::here("Data/Covariate_Tables/CovariateTable_2021.csv")) |> 
    add_column( year = 2021) |> 
    rename( LocationName = SiteID) |> 
    mutate( trt = substr( LocationName, 1, 1),
            twn = substr( LocationName, 2, 2), 
            site = substr( LocationName, 3, 5)) |> 
    mutate(siteID = row_number()) |> 
    dplyr::select( LocationName, year, siteID, trt, twn, ClosedCover, AgCover)
  
  # Prepare 2022 covariate table.
  cov22 <- read_csv(here::here("Data/Covariate_Tables/CovariateTable_2022.csv")) |> 
    add_column( year = 2022) |> 
    rename( LocationName = SiteID) |> 
    mutate( trt = substr( LocationName, 1, 1),
            twn = substr( LocationName, 2, 2), 
            site = substr( LocationName, 3, 5)) |> 
    mutate(siteID = row_number()) |> 
    dplyr::select( LocationName, year, siteID, trt, twn, ClosedCover, AgCover)
  
  
  ############################################
  ############### Count Data #################
  ############################################
  
  # Read in weekly deer count data for the selected sex-and-age class.
  # Add new columns to reflect formatting of covariate tables.
  
  # 2019 count data, sex-and-age class (CSV) hardcoded.
  count19 <- read_csv(here::here(paste0("Data/Count_Data/Counts_", classes[i], "_2019.csv"))) |> 
    add_column( year = 2019) |> 
    mutate( trt = substr( LocationName, 1, 1),
            twn = substr( LocationName, 2, 2), 
            site = substr( LocationName, 3, 5)) |> 
    mutate(siteID = row_number()) |> 
    dplyr::select( LocationName, year, siteID, trt, twn, week1.count:week9.count)
  
  # 2020 count data, sex-and-age class (CSV) hardcoded.
  count20 <- read_csv(here::here(paste0("Data/Count_Data/Counts_", classes[i], "_2020.csv"))) |> 
    add_column( year = 2020) |> 
    mutate( trt = substr( LocationName, 1, 1),
            twn = substr( LocationName, 2, 2), 
            site = substr( LocationName, 3, 5)) |> 
    mutate(siteID = row_number()) |> 
    dplyr::select( LocationName, year, siteID, trt, twn, week1.count:week9.count)
  
  # 2021 count data, sex-and-age class (CSV) hardcoded.
  count21 <- read_csv(here::here(paste0("Data/Count_Data/Counts_", classes[i], "_2021.csv"))) |> 
    add_column( year = 2021) |> 
    mutate( trt = substr( LocationName, 1, 1),
            twn = substr( LocationName, 2, 2), 
            site = substr( LocationName, 3, 5)) |> 
    mutate(siteID = row_number()) |> 
    dplyr::select( LocationName, year, siteID, trt, twn, week1.count:week9.count)
  
  # 2022 count data, sex-and-age class (CSV) hardcoded.
  count22 <- read_csv(here::here(paste0("Data/Count_Data/Counts_", classes[i], "_2022.csv"))) |> 
    add_column( year = 2022) |> 
    mutate( trt = substr( LocationName, 1, 1),
            twn = substr( LocationName, 2, 2), 
            site = substr( LocationName, 3, 5)) |> 
    mutate(siteID = row_number()) |> 
    dplyr::select( LocationName, year, siteID, trt, twn, week1.count:week9.count)
  
  
  ###########################################
  ############## Format Data ################
  ###########################################
  
  # Combine yearly count data and reshape data into long format (pivot by week#.count, 
  # name new column "rep" for repeated visits, and add associated values to the new
  # "count" column). 
  # Drop non-numeric characters from new "rep" column. Arrange year, siteID, and rep
  # in ascending order.
  # Add new column for "yearID" (4 years total). 
  # Ungroup data after performing calculations to prevent future data-management errors. 
  # Create new column "new_twp" and assign unique numeric identifier for each township 
  # (based on combination of treatment and township pairing).
  long <- list(count19, count20, count21, count22) |>
    reduce(full_join) |>
    pivot_longer(week1.count:week9.count, names_to = "rep", values_to = "count") |> 
    mutate(rep = parse_number(rep)) |> 
    arrange(year, siteID, rep) |> 
    mutate(yearID = year -2018) |> 
    ungroup() |> 
    group_by(trt, twn) |> 
    mutate( new_twn = cur_group_id()) |> 
    ungroup()
  
  # create a 3D matrix for the counts (dim1 = site; dim2 = repeated visit; dim3 = year).
  # Important note: The rows in the different array slices can be different sites among years.
  # Array stores data in multiple dimensions and "nsite" is the number of sites in a
  # given year (year 2 only has 72 sites, all others have 144).
  deer_all <- array(NA, dim = c( 144, 9, 4))
  
  nsite <- long |> 
    group_by(year) |>
    summarise( nsite = length(unique(siteID))) |> 
    pull(nsite)
  
  # In the "all" array (above), loop through each year of data (yearID) "t", each weekly
  # count (rep) "j", each site (siteID) "i" (number of sites depends on year; addressed
  # above and incorporated below), and assign counts.
  for( t in 1:4 ) {
    for( j in 1:9) {
      for( k in 1:nsite[t]){
        deer_all[ k, j, t] <- long[ c( long$siteID == k & long$rep == j & long$yearID == t), "count"]$count
      }
    }
  }
  
  # Combine yearly covariate tables.
  cov_combined <- list(cov19, cov20, cov21, cov22) |>
    reduce(full_join) |>
    mutate(ag_sc = as.numeric(scale(AgCover)), # Add scaled AgCover values
           yearID = year - 2018 ) # Add year identifier (4 years total)
  
  # Take long data, select columns of interest, remove duplicate rows, reshape back 
  # to wide format, remove siteID, make it a matrix, then remove column names. 
  new_twn <- long |> 
    dplyr::select(siteID, year, new_twn) |> 
    distinct() |> 
    pivot_wider(names_from = year, values_from = new_twn) |> 
    dplyr::select(-siteID) |> 
    as.matrix() |> 
    unname()
  
  # Use similar approach as above for covariate data.
  # Agriculture covariate (scaled).
  ag <- cov_combined |> 
    dplyr::select( year, siteID, ag_sc ) |> 
    pivot_wider(names_from = year, values_from = ag_sc ) |> 
    dplyr::select(-siteID) |> 
    as.matrix() |> 
    unname()
  
  # Cover covariate (binary).
  cover <- cov_combined |> 
    dplyr::select( year, siteID, ClosedCover) |> 
    pivot_wider(names_from = year, values_from = ClosedCover) |> 
    dplyr::select(-siteID) |> 
    as.matrix() |> 
    unname()
  
  # Treatment covariate (indexed).
  trt <- cov_combined |> 
    dplyr::select( year, siteID, trt ) |> 
    dplyr::mutate(trt = as.numeric(trt)) |> 
    # Very important note: trt must be 1 or 2, not 0 or 1, since we are indexing and
    # not using a binary approach. 
    pivot_wider(names_from = year, values_from = trt) |> 
    dplyr::select(-siteID) |> 
    as.matrix() |> 
    unname()
  
  # Townships (indexed).
  twn <- cov_combined |>
    mutate(twn_trt = paste0(trt, twn)) |>
    group_by(twn_trt) |> 
    mutate(twn2 = cur_group_id()) |>
    ungroup() |>
    dplyr::select( year, siteID, twn2 ) |> 
    dplyr::mutate(twn2 = as.numeric(twn2)) |>
    pivot_wider(names_from = year, values_from = twn2) |> 
    dplyr::select(-siteID) |> 
    as.matrix() |> 
    unname()
  
  # Year covariate (reformatted).
  yr <- cov_combined |> 
    dplyr::select( yearID, year, siteID) |> 
    mutate(year = year - 2019) |> # Reformatted (0, 1, 2, 3). 
    mutate( year = as.numeric(scale(year))) |> 
    pivot_wider(names_from = yearID, values_from = year) |> 
    dplyr::select(-siteID) |> 
    as.matrix() |> 
    unname()
  
  ############################################
  ############# N-Mixture Model ##############
  ############################################
  
  # Create constants for model from array.
  constants <- list(
    nyear = dim(deer_all)[[3]],
    nsite = nsite, 
    nreps = dim(deer_all)[[2]],
    trt = trt, # The "trt" matrix should be bundled into the constants list, not the data list.
    twn = twn)
  
  # Bundle data (counts and covariates).
  data <- list(
    y = deer_all,
    ag = ag, 
    cover = cover)
  
  # Model in NIMBLE
  code <- nimbleCode({
    
    alpha0 ~ dnorm(0, sd = 2)
    alpha1 ~ dnorm(0, sd = 1)
    # Standard deviation among site x year combinations for the residual error epsilon.
    sd_epsilon ~ dexp(1) # Exponential prior (since it must be positive).
    # Detection error for lambda.
    sd_epsilon_lambda ~ dexp(1) # Exponential prior
    
    for( i in 1:2){
      beta3[i] ~ dnorm(0, sd = 1)
    }
    
    for( i in 1:8) {
      epsilon_lambda[i] ~ dnorm(0, sd = sd_epsilon_lambda)
    }
    
    beta5 ~ dnorm(0, sd = 1)
    beta6 ~ dnorm(0, sd = 1)
    
    for( t in 1:nyear ) {
      beta2[t] ~ dnorm(0, sd = 2)
      
      # Loop through only the sites that are surveyed in a given year.
      for( i in 1:nsite[t] ){
        N[i, t] ~ dpois( lambda[ i, t ] )
        
        # Neil note: messy naming of betas - whoops
        log( lambda[i,t] ) <- beta2[t] + beta3[trt[i,t]] + beta5*ag[i,t] + beta6*ag[i,t]*ag[i,t] + epsilon_lambda[twn[i,t]]
        
        # Adding site x year random effect (epsilon) for detection probability. 
        logit( p[i, t] ) <- alpha0 + alpha1 * cover[i, t] + epsilon[i, t]
        # Epsilon is modeled as a residual, a draw from a zero-mean normal distribution.
        epsilon[i, t] ~ dnorm(0, sd = sd_epsilon) # We let the model estimate the standard deviation.
        
        for( j in 1:nreps ){
          y[i, j, t] ~ dbin( p[i, t], N[i, t] )
          
        }
      }
    }
    
  })         
  
  # Initial values.
  Nst <- apply( data$y, c(1, 3), max, na.rm = TRUE)
  
  Nst[Nst == -Inf] <- 0
  
  Nst <- Nst + 1 # This line is important.
  
  yst <- data$y
  
  yst[is.na(yst)] <- round( mean( data$y, na.rm = TRUE) )
  
  inits <- function(){
    list(
      alpha0 = rnorm(1, 0, 0.5), 
      alpha1 = rnorm(1, 0, 0.5), 
      beta2 = rnorm(constants$nyear, 0, 0.5), 
      beta3 = rnorm(2, 0, 0.5), 
      beta5 = rnorm(1, 0, 0.5), 
      beta6 = rnorm(1, 0, 0.5),
      N = Nst,
      y = yst,
      # Initial values for epsilon and sd_epsilon.
      epsilon = array( rnorm( constants$nsite[1] * constants$nyear, 0, 0.5), 
                       dim = c(constants$nsite[1], constants$nyear)),
      sd_epsilon = rexp(1, 1),
      sd_epsilon_lambda = rexp(1, 1),
      epsilon_lambda = rnorm(8, 0, 0.25)
    )
  }
  
  # Parameters monitored. Good to track sd_epsilon to ensure convergence and see among-site variation in p.
  params <- c("alpha0", "alpha1", 
              "beta2", "beta3",
              "beta5", "beta6", "sd_epsilon", "sd_epsilon_lambda", "N", "epsilon_lambda") 
  
  m <- nimble::nimbleModel(
    code = code, 
    constants = constants, 
    data = data, 
    inits = inits())
  
  m$initializeInfo()
  
  # Model output.
  out <- nimbleMCMC(
    monitors = params,
    code = code, 
    data = data, 
    constants = constants, 
    inits = inits(), 
    niter = 50000, 
    nburnin = 2000, 
    thin = 10, 
    nchains = 3)

  
  betas <-
    MCMCvis::MCMCpstr( out, params = c("beta2"), type = "chains")[[1]] |> 
    tibble::as_tibble(rownames = "param") |> 
    tidyr::pivot_longer(dplyr::starts_with("V")) |> 
    dplyr::mutate(iter = readr::parse_number(name),
                  param = stringr::str_remove_all(param, "beta2"),
                  param = readr::parse_number(param)) |> 
    dplyr::select(iter, year = param, beta2 = value) |> 
    dplyr::full_join(
      
      MCMCvis::MCMCpstr( out, params = c("beta3"), type = "chains")[[1]] |> 
        tibble::as_tibble(rownames = "param") |> 
        tidyr::pivot_longer(dplyr::starts_with("V")) |> 
        dplyr::mutate(iter = readr::parse_number(name),
                      param = stringr::str_remove_all(param, "beta3"),
                      param = readr::parse_number(param)) |> 
        dplyr::select(iter, trt = param, beta3 = value)) |> 
    dplyr::full_join(
      
      MCMCvis::MCMCpstr( out, params = c("beta5"), type = "chains")[[1]] |> 
        tibble::as_tibble(rownames = "param") |> 
        tidyr::pivot_longer(dplyr::starts_with("V")) |> 
        dplyr::mutate(iter = readr::parse_number(name)) |> 
        dplyr::select(iter, beta5 = value)) |> 
    dplyr::full_join(
      MCMCvis::MCMCpstr( out, params = c("beta6"), type = "chains")[[1]] |> 
        tibble::as_tibble(rownames = "param") |> 
        tidyr::pivot_longer(dplyr::starts_with("V")) |> 
        dplyr::mutate(iter = readr::parse_number(name)) |> 
        dplyr::select(iter, beta6 = value))
  
  # Create list of year names to replace standardized years used in model.
  yr_names <- c( 2019, 2020, 2021, 2022)
  
  # Create keys for easier interpretation.
  yr_key <- tibble(yr = 1:4, year = yr_names) # Years.
  
  trt_key <- tibble( trt_name = c("Non-APR", "APR"), # Harvest treatment.
                     trt = c(1, 2))
  
  
  pred_df <- dplyr::cross_join(yr_key, trt_key) |> 
    dplyr::select(year = yr, year.lab = year, trt, trt.lab = trt_name) |> 
    tibble::add_column(ag = mean(data$ag, na.rm = TRUE))
  
  posterior <- dplyr::full_join(
    betas, 
    pred_df) |> 
    mutate(lambda = exp( beta2 + beta3 + beta5*ag + beta6*ag*ag)) |> 
    dplyr::group_by(trt, trt.lab, year, year.lab) |>
    tibble::add_column( class = classes[i])
  
  setwd(here::here("Results/Supplemental_Model_Results"))
  save(
    code, 
    out, 
    posterior, 
    file = paste0(tolower(classes[i]), "_year_factor_no_itx.RData"))
  print(paste("Finished", classes[i]))
}