create_package("~/Documents/GitHub/XGFriction")

#' Initialize XGFriction Processing Class
#'
#' This function initializes the XGFriction processing class from the ReddyProc package.
#' @param site Site name
#' @param date_range Date range
#' @param variables Variables to include
#' @param lat Latitude
#' @param long Longitude
#' @param timezone TimeZone
#' @return XGFriction processing object
#' @export
initialize_xgfriction_processing <- function(site, date_range, variables, lat, long, timezone) {
  xgfriction_proc <- ReddyProc::sEddyProc$new(site, date_range, variables)
  xgfriction_proc$sSetLocationInfo(LatDeg = lat, LongDeg = long, TimeZoneHour = timezone)
  return(xgfriction_proc)
}


##Call this function in my package.
xgfriction_proc <- initialize_xgfriction_processing('US-Bar', Bartlett.2004.2023.posix, c('NEE','Rg','Tair','VPD', 'Ustar'), lat, long, timezone)


#' Gap Fill MDS Variables
#'
#' This function performs gap filling for specified MDS variables.
#' @param xgfriction_proc XGFriction processing object
#' @param variables Vector of variables to gap fill
#' @param fill_all Logical indicating whether to fill all gaps
#' @return Updated XGFriction processing object
#' @export
gap_fill_mds_variables <- function(xgfriction_proc, variables, fill_all = FALSE) {
  for (variable in variables) {
    xgfriction_proc$sMDSGapFill(variable, FillAll = fill_all)
  }
  return(xgfriction_proc)
}



##Optional for user
#' Perform IQR Filtering
#'
#' This function performs IQR filtering on specified variables.
#' @param xgfriction_proc XGFriction processing object
#' @param variables Vector of variables to filter
#' @param threshold_multiplier Multiplier for IQR threshold
#' @return Updated XGFriction processing object
#' @export
perform_iqr_filtering <- function(xgfriction_proc, variables, threshold_multiplier = 6) {
  for (variable in variables) {
    residual <- xgfriction_proc$sTEMP[[paste0(variable, "_orig")]] - xgfriction_proc$sTEMP[[paste0(variable, "_fall")]]
    IQR_val <- IQR(residual, na.rm = TRUE)
    outlier <- abs(residual) > (IQR_val * threshold_multiplier)
    xgfriction_proc$sDATA[[variable]] <- ifelse(outlier == 0, xgfriction_proc$sTEMP[[paste0(variable, "_orig")]], NA)
  }
  return(xgfriction_proc)
}

#Use function like this
xgfriction_proc <- perform_iqr_filtering(xgfriction_proc, c("NEE"), threshold_multiplier = 6)





#' Perform Ustar Threshold Distribution Estimation and Gap Filling
#'
#' This function estimates the u* threshold distribution and performs gap filling.
#' @param xgfriction_proc XGFriction processing object
#' @param variable Variable to perform gap filling on
#' @param nSample Number of samples for estimation
#' @param probs Probabilities for estimation
#' @param fill_all Logical indicating whether to fill all gaps
#' @return Updated XGFriction processing object
#' @export
perform_ustar_gap_fill <- function(xgfriction_proc, variable, nSample = 1000L, probs = c(0.05, 0.5, 0.95), fill_all = TRUE) {
  set.seed(2000)
  xgfriction_proc$sEstUstarThresholdDistribution(nSample = nSample, probs = probs)
  uStarTh <- xgfriction_proc$sGetEstimatedUstarThresholdDistribution()
  uStarThAnnual <- usGetAnnualSeasonUStarMap(uStarTh)[-2]
  uStarSuffixes <- colnames(uStarThAnnual)[-1]
  xgfriction_proc$sMDSGapFillAfterUStarDistr(variable, uStarTh = uStarThAnnual, uStarSuffixes = uStarSuffixes, FillAll = fill_all)
  return(xgfriction_proc)
}

##Function use
xgfriction_proc <- perform_ustar_gap_fill(xgfriction_proc, "NEE")







#' Train XGBoost model with custom grid
#'
#' This function trains an XGBoost model using the provided data and a custom grid for hyperparameter tuning.
#' @param data The dataset
#' @param formula The formula for the model
#' @param grid Custom grid for hyperparameter tuning
#' @param folds Number of folds for cross-validation
#' @param repeats Number of repeats for cross-validation
#' @param strata Variable for stratified sampling
#' @param n_trees Number of trees
#' @param tree_depth Maximum tree depth
#' @param min_n Minimum observations in terminal nodes
#' @param loss_reduction Minimum loss reduction to make further splits
#' @param sample_size Fraction of the training set to sample
#' @param mtry Number of variables to sample at each split
#' @param learn_rate Learning rate
#' @return Trained XGBoost model
#' @export
train_xgboost_model <- function(data, formula, grid, folds = 5, repeats = 2, strata = NULL, n_trees = 2000,
                                tree_depth = tune(), min_n = tune(), loss_reduction = tune(),
                                sample_size = sample_prop(), mtry = tune(), learn_rate = tune()) {
  # Prepare data
  set.seed(60723)
  split <- initial_split(data)
  train_data <- training(split)

  # Set up model specification
  xgb_spec <- boost_tree(trees = n_trees,
                         tree_depth = tree_depth,
                         min_n = min_n,
                         loss_reduction = loss_reduction,
                         sample_size = sample_size,
                         mtry = mtry,
                         learn_rate = learn_rate) %>%
    set_engine("xgboost") %>%
    set_mode("regression")

  # Create workflow
  bart_workflow <- workflow() %>%
    add_formula(formula) %>%
    add_model(xgb_spec)

  # Create folds
  set.seed(60723)
  folds <- vfold_cv(train_data, v = folds, repeats = repeats, strata = strata)

  # Tune grid
  doParallel::registerDoParallel()
  set.seed(60723)
  res <- tune_grid(bart_workflow, resamples = folds, grid = grid, control = control_grid(save_pred = TRUE))

  # Train model with best parameters
  best_params <- select_best(res, "rmse")
  trained_model <- finalize_model(xgb_spec, best_params)

  return(trained_model)
}


#function usage:
trained_model <- train_xgboost_model(data = bart_splitt.naomit,
                                     formula = NEE_U50_orig ~ .,
                                     grid = Xgb_grid,
                                     folds = 5,
                                     repeats = 2,
                                     strata = NULL)




use_git()



devtools::install_github("your_username/your_package")
library(your_package)



