###### Forecasting - Fit Random Forest on Lags against GLM Benchmark ######

# Make R check happy about non-standard evaluation with '.'
utils::globalVariables(".")

#' Build a forecasting model for counts using random forest, benchmarked against our standard GLM approach.
#' Forecasting for random forest is performed recursively so that we can compare models over the training data.
#'
#' @param dat Dataframe for a single species as prepared by prep_dat_glm.
#' @param train_start First YrQtr for train set.
#' @param test_start First YrQtr for test set.
#' @param forecast_start First YrQtr for forecast set.
#' @param horizon Number of survey periods to forecast.
#' @param lag_order Number of lags to include in model.
#' @param seed Optional seed to set so that random forest results can be replicated.
#'
#' @return Dataframe with random forest train/test predictions and glm benchmark train/test predictions.
#' @export
model_rf <- function(dat,
                     train_start,
                     test_start,
                     forecast_start,
                     horizon,
                     lag_order,
                     seed = NULL) {
  # If seed is provided, set for replicable results
  if(!is.null(seed)) {
    set.seed(seed)
  }

  # Take log and first difference of counts, then create time delay embedding matrix
  counts_mbd <- dat$Count %>%
    log() %>%
    diff(differences = 1) %>%
    stats::embed(lag_order + 1)

  # Find corresponding train and test rows in embedding
  train_start_idx <- which(dat$YrQtr == train_start) - (lag_order + 1)
  test_start_idx <- which(dat$YrQtr == test_start) - (lag_order + 1)
  test_end_idx <- max(which(dat$YrQtr < forecast_start)) - (lag_order + 1)

  # Define index sets train and test
  idx_train <- seq(train_start_idx, test_start_idx - 1)
  idx_test <- seq(test_start_idx, test_end_idx)
  # Retrieve data sets for train and test from embedding
  y_train <- counts_mbd[idx_train, 1]
  x_train <- counts_mbd[idx_train,-1]

  y_test <- counts_mbd[idx_test, 1]
  x_test <- counts_mbd[idx_test[1], -1]

  # Train random forest
  rfmod <- randomForest::randomForest(x_train, y_train)
  # Recursively generate predictions for test set
  preds_test <- c()
  for(i in 1:length(idx_test)) {
    # Predict
    preds_test[i] <- stats::predict(rfmod, x_test)
    # Update test data with new prediction
    x_test <- c(preds_test[i], x_test[-length(x_test)])
  }

  # Continue generating predictions for foreccast
  preds_forecast <- c()
  for(i in 1:horizon) {
    # Predict
    preds_forecast[i] <- stats::predict(rfmod, x_test)
    # Update test data with new prediction
    x_test <- c(preds_forecast[i], x_test[-length(x_test)])
  }

  # Backtransform test predictions
  exp_term <- exp(cumsum(preds_test))
  last_obs <- dat$Count[test_start_idx + lag_order]
  rf_test_preds <- last_obs * exp_term

  # Backtransform forecast predictions
  exp_term <- exp(cumsum(preds_forecast))
  last_obs <- dat$Count[test_end_idx + lag_order + 1]
  rf_forecast_preds <- last_obs * exp_term

  # Get train predictions and backtransform
  preds_train <- stats::predict(rfmod, x_train)
  exp_term <- exp(cumsum(preds_train))
  last_obs <- dat$Count[train_start_idx + lag_order]
  rf_train_preds <- last_obs * exp_term

  # Generate forecast YrQtrs - start at forecast_start and include all quarters present in training data
  forecast_YrQtr <- (rep(floor(forecast_start):(floor(forecast_start) + horizon - 1), each = length(unique(dat$Qtr))) +
                       rep(unique(dat$YrQtr %% 1), horizon)) %>%
                    .[which(.==forecast_start):(which(.==forecast_start)+horizon-1)]

  # Start preparing dataframe to return, join predictions to original data
  ret <- rbind(dat %>%
                 select(.data$Species_Code, .data$YrQtr, .data$Count) %>%
                 mutate(Model = "None", Set = "Observed"),
               data.frame(Species_Code = dat$Species_Code[1],
                          YrQtr = dat$YrQtr[idx_train + lag_order + 1],
                          Count = rf_train_preds,
                          Model = "Random Forest",
                          Set = "Train"),
               data.frame(Species_Code = dat$Species_Code[1],
                          YrQtr = dat$YrQtr[idx_test + lag_order + 1],
                          Count = rf_test_preds,
                          Model = "Random Forest",
                          Set = "Test"),
               data.frame(Species_Code = dat$Species_Code[1],
                          YrQtr = forecast_YrQtr,
                          Count = rf_forecast_preds,
                          Model = "Random Forest",
                          Set = "Forecast")
          )

  # Set up benchmark glm data
  bench_train <- dat[idx_train + lag_order + 1,]
  # For test data, assume all 50 stations surveyed
  bench_test <- dat[idx_test + lag_order + 1,]
  bench_test$N_Stations <- 50
  # Expand forecast YrQtrs into remaining vars, again assume 50 stations
  bench_forecast <- data.frame(YrQtr = forecast_YrQtr,
                               Species_Code = dat$Species_Code[1],
                               Year = floor(forecast_YrQtr),
                               Year2 = floor(forecast_YrQtr)^2,
                               Qtr = as.factor((forecast_YrQtr %% 1)*4 + 1),
                               N_Stations = 50
  )
  # Check data for all quarters, only model seasonality if all present
  if(length(unique(dat$Qtr)) == 4) {
    ct <- c("Year", "Year2", "Qtr")
  } else {
    ct <- c("Year", "Year2")
  }
  # Fit glm
  bench_glm <- bench_train %>%
    model_birds_glm(family = "auto", choose_terms = ct, verbose = FALSE) %>%
    select(.data$models) %>%
    pull() %>%
    .[[1]]
  # Get glm predictions
  bench_train_preds <- bench_glm$fitted.values
  bench_test_preds <- stats::predict(bench_glm, bench_test, type = "response")
  bench_forecast_preds <- stats::predict(bench_glm, bench_forecast, type = "response")

  # Add benchmark to return dataframe
  ret <- rbind(ret,
               data.frame(Species_Code = dat$Species_Code[1],
                          YrQtr = dat$YrQtr[idx_train + lag_order + 1],
                          Count = bench_train_preds,
                          Model = "GLM",
                          Set = "Train"),
               data.frame(Species_Code = dat$Species_Code[1],
                          YrQtr = dat$YrQtr[idx_test + lag_order + 1],
                          Count = bench_test_preds,
                          Model = "GLM",
                          Set = "Test"),
               data.frame(Species_Code = dat$Species_Code[1],
                          YrQtr = forecast_YrQtr,
                          Count = bench_forecast_preds,
                          Model = "GLM",
                          Set = "Forecast")
  )

  return(ret)
}
