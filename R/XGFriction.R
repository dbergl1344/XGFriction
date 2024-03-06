library(devtools)
library(REddyProc)
library(xgboost)
library(roxygen2)

create_package("~/Documents/GitHub/XGFriction")
devtools::load_all("~/Documents/GitHub/XGFriction")

colnames(Bartlett_30_minute_data_July2023)

roxygen2::roxygenize()

#' This class represents a processor for XGFriction data.
#'
#' @field site The name of the site associated with the processor.
#' @export
XGFrictionProcessor <- setRefClass("XGFrictionProcessor",
                                   fields = list(site = "character"),
                                   methods = list(
                                     initialize = function(site) {
                                       site <<- site
                                     },
                                     initialize_xgfriction_processing = function(dataframe, LatDeg, LongDeg, TimeZoneHour) {
                                       # Check if the data is in 30-minute timestamp format
                                       if (any(diff(dataframe$DateTime) != 30*60)) {
                                         mins <- 15*60
                                         print("Adding 15 minutes to DateTime")
                                         print(dataframe$DateTime + mins)
                                         dataframe$DateTime <- (dataframe$DateTime + mins)
                                       }

                                       # Initialize the processing
                                       xgfriction_proc <- REddyProc::sEddyProc$new(site, dataframe$DateTime,
                                                                                   c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'))
                                       xgfriction_proc$sSetLocationInfo(LatDeg = LatDeg, LongDeg = LongDeg, TimeZoneHour = TimeZoneHour)
                                       return(xgfriction_proc)
                                     }
                                   )
)

#' Initialize XGFriction Processor
#'
#' This function initializes the XGFriction processor object.
#'
#' @param site Site name.
#' @return XGFrictionProcessor object.
#' @export
initialize_xgfriction_processor <- function(site) {
  return(XGFrictionProcessor$new(site))
}
?XGFrictionProcessor
?initialize_xgfriction_processor

##Test dataframe
# Subsetting to get data for the year 2007
Bartlett_2010 <- Bartlett_30_minute_data_July2023[Bartlett_30_minute_data_July2023$YEAR == 2010, ]

# check
print(sum(is.na(Bartlett_2010$NEE)))


##example usage for functions above:
# Create an instance of the XGFrictionProcessor class for the site 'US-Bar'
processor <- initialize_xgfriction_processor('US-Bar')
# Call the initialize_xgfriction_processing method with their dataframe and other parameters
xgfriction_proc <- processor$initialize_xgfriction_processing(Bartlett_2010, LatDeg = 40.7128, LongDeg = -74.0060, TimeZoneHour = -5)






#' Gap Fill MDS Variables
#'
#' This function performs gap filling for specified MDS variables using REddyProc.
#' @param processor XGFriction processing object
#' @param variables Vector of variables to gap fill
#' @param fill_all Logical indicating whether to fill all gaps
#' @return Updated XGFriction processing object
#' @export
gap_fill_mds_variables <- function(processor, variables, fill_all = FALSE) {
  # Initialize REddyProc object for gap filling
  reddy_proc <- REddyProc::sEddyProc$new()

  # Loop through variables
  for (variable in variables) {
    # Perform gap filling using REddyProc's sMDSGapFill method
    gap_filled_data <- reddy_proc$sMDSGapFill(processor$data[[variable]], FillAll = fill_all)

    # Update processor with gap-filled data
    processor$data[[variable]] <- gap_filled_data
  }

  return(processor)
}




##package user example:
gap_fill_mds_variables(xgfriction_proc, variables = c('NEE', 'Rg', 'Tair'), fill_all = TRUE)



#' Perform IQR Filtering
#'
#' This function performs IQR filtering on specified variables.
#' @param processor XGFriction processing object
#' @param dataframe Data frame containing the variables to filter
#' @param variables Vector of variables to filter
#' @param threshold_multiplier Multiplier for IQR threshold
#' @return Updated XGFriction processing object
#' @export
perform_iqr_filtering <- function(xgfriction_proc, dataframe, variables, threshold_multiplier = 6) {
  for (variable in variables) {
    residual <- dataframe[[paste0(variable, "_orig")]] - dataframe[[paste0(variable, "_fall")]]
    IQR_val <- IQR(residual, na.rm = TRUE)
    outlier <- abs(residual) > (IQR_val * threshold_multiplier)
    xgfriction_proc$data[[variable]] <- ifelse(outlier == 0, dataframe[[paste0(variable, "_orig")]], NA)
  }
  return(xgfriction_proc)
}

# Perform IQR filtering using their own processing object
my_processor <- perform_iqr_filtering(my_processor, dataframe = Bartlett_2010, variables = c('NEE', 'Rg', 'Tair'), threshold_multiplier = 6)




#' Perform Ustar Threshold Distribution Estimation and Gap Filling
#'
#' This function estimates the u* threshold distribution and performs gap filling.
#' @param processor XGFriction processing object
#' @param dataframe Data frame containing the variable to perform gap filling on
#' @param variable Variable to perform gap filling on
#' @param nSample Number of samples for estimation
#' @param probs Probabilities for estimation
#' @param fill_all Logical indicating whether to fill all gaps
#' @return Updated XGFriction processing object
#' @export
perform_ustar_gap_fill <- function(processor, dataframe, variable, nSample = 1000L, probs = c(0.05, 0.5, 0.95), fill_all = TRUE) {
  set.seed(2000)
  processor$sEstUstarThresholdDistribution(nSample = nSample, probs = probs)
  uStarTh <- processor$sGetEstimatedUstarThresholdDistribution()
  uStarThAnnual <- usGetAnnualSeasonUStarMap(uStarTh)[-2]
  uStarSuffixes <- colnames(uStarThAnnual)[-1]
  processor$sMDSGapFillAfterUStarDistr(variable, uStarTh = uStarThAnnual, uStarSuffixes = uStarSuffixes, FillAll = fill_all)
  return(processor)
}


##for user
# Assuming the user has their own initialized processing object named my_processor
my_processor <- processor$initialize_xgfriction_processing(Bartlett_2010, LatDeg = 40.7128, LongDeg = -74.0060, TimeZoneHour = -5)

# Perform u* threshold distribution estimation and gap filling using their own processing object, data frame, and variable
my_processor <- perform_ustar_gap_fill(my_processor, dataframe = Bartlett_2010, variable = "NEE")





#' Train XGBoost model with custom grid
#'
#' This function trains an XGBoost model using the provided data and a custom grid for hyperparameter tuning.
#'
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
  require(caret)
  require(tidymodels)
  require(xgboost)

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
  xgb_workflow <- workflow() %>%
    add_formula(formula) %>%
    add_model(xgb_spec)

  # Create folds
  set.seed(60723)
  folds <- vfold_cv(train_data, v = folds, repeats = repeats, strata = strata)

  # Tune grid
  doParallel::registerDoParallel()
  set.seed(60723)
  res <- tune_grid(xgb_workflow, resamples = folds, grid = grid, control = control_grid(save_pred = TRUE))

  # Train model with best parameters
  best_params <- select_best(res, "rmse")
  trained_model <- finalize_model(xgb_spec, best_params)

  return(trained_model)
}

#' Select Best Model
#'
#' This function selects the best model based on specified evaluation metrics.
#'
#' @param results A data frame containing the results of model training (e.g., cross-validation results).
#' @param metric The evaluation metric used for selecting the best model (e.g., "rmse", "rsq").
#' @param higher_is_better Logical indicating whether a higher value of the evaluation metric is better.
#' @return The best model based on the specified evaluation metric.
#' @export
select_best_model <- function(results, metric, higher_is_better = TRUE) {
  require(caret)
  require(tidymodels)

  # Add code here to select the best model based on the specified evaluation metric
}






##selecting the best model

#' Select Best Model
#'
#' This function selects the best model based on specified evaluation metrics.
#'
#' @param results A data frame containing the results of model training (e.g., cross-validation results).
#' @param metric The evaluation metric used for selecting the best model (e.g., "rmse", "rsq").
#' @param higher_is_better Logical indicating whether a higher value of the evaluation metric is better.
#' @return The best model based on the specified evaluation metric.
#' @export
select_best_model <- function(results, metric, higher_is_better = TRUE) {
  # Add code here to select the best model based on the specified evaluation metric
}


##user example
# Train the XGBoost model
my_model <- train_xgboost_model(data = my_data, formula = my_formula, grid = my_grid, folds = 10, repeats = 3)

# Select the best model based on the RMSE metric
best_model <- select_best_model(results = my_model$results, metric = "rmse")

# Select the best model based on the R-squared metric
best_model <- select_best_model(results = my_model$results, metric = "rsq", higher_is_better = TRUE)







#' Plot Variables of Importance
#'
#' This function plots the variables of importance of a trained XGBoost model.
#'
#' @param model The trained XGBoost model.
#' @param n_top Number of top variables to plot.
#' @return A plot showing the variables of importance.
#' @export
plot_variables_of_importance <- function(model, n_top = 10) {
  ggplot2::ggplot(ggplot2::aes(x = Importance, y = reorder(Variable, Importance))) +
    ggplot2::geom_col() +
    ggplot2::labs(title = "Top Variables of Importance",
                  x = "Importance",
                  y = "Variable")
}

#' Plot Model Metrics
#'
#' This function plots the metrics of a trained XGBoost model.
#'
#' @param metrics The metrics data frame.
#' @param metric_name The name of the metric to plot.
#' @return A plot showing the specified metric.
#' @export
plot_model_metrics <- function(metrics, metric_name) {
  ggplot2::ggplot(metrics, ggplot2::aes(x = .pred, y = .resid)) +
    ggplot2::geom_point(color = "purple") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "yellow") +
    ggplot2::labs(x = "Predicted NEE",
                  y = "Residuals",
                  title = "Residuals vs. Predicted Values (Test Set)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}


#' Use trained XGBoost model to predict missing data
#'
#' This function uses a trained XGBoost model to predict missing data in a dataset.
#'
#' @param model The trained XGBoost model.
#' @param data The dataset with missing values.
#' @return The dataset with missing values filled using the XGBoost model.
#' @export
predict_missing_data <- function(model, data) {
  require(xgboost)

  # Predict missing values
  predicted_values <- predict(model, newdata = data)

  # Replace NA values with predicted values
  data[is.na(data)] <- predicted_values[is.na(data)]

  return(data)
}








use_git()



devtools::install_github("your_username/your_package")
library(your_package)




