
#' Calculate MAPE and RMSE for rf and benchmark glm.
#' @param rf_out Dataframe from model_rf.
#'
#' @return Tibblewith train/test MAPE and RMSE for each model.
#' @export
error_comp_rf <- function(rf_out) {
  # Separate observed and predicted counts
  obs_dat <- rf_out %>%
    filter(.data$Model == "None")
  pred_dat <- rf_out %>%
    filter(.data$Model != "None" & .data$Set != "Forecast")

  # Reaggregate and calculate error metrics
  err_dat <- left_join(pred_dat, obs_dat, by = "YrQtr") %>%
    group_by(.data$Model.x, .data$Set.x) %>%
    summarise(MAPE = mean(abs(.data$Count.x - .data$Count.y) / .data$Count.y),
              RMSE = sqrt(mean((.data$Count.x - .data$Count.y)^2))
    ) %>%
    select(Model = .data$Model.x, Set = .data$Set.x, .data$MAPE, .data$RMSE) %>%
    arrange(desc(.data$Model), desc(.data$Set))

  return(err_dat)
}

#' Calculate MAPE and RMSE for rf and benchmark glm and format into gt table.
#' @param rf_out Dataframe from model_rf.
#'
#' @return gt table with train/test MAPE and RMSE for each model.
#' @export
error_table_rf <- function(rf_out) {
  # Calculate errors
  err_dat <- error_comp_rf(rf_out)

  # Build table
  err_gt <- err_dat %>%
    gt::gt(rowname_col = "Set", groupname_col = "Model") %>%
    gt::fmt_percent(columns = "MAPE",
                    decimals = 0) %>%
    gt::fmt_number(columns = "RMSE",
                   decimals = 2) %>%
    gt::tab_header("Model Accuracy Comparison")

  return(err_gt)
}

#' Build forecast plot.
#'
#' @param rf_out Dataframe from model_rf.
#'
#' @return ggplot with forecast models.
#' @export
plot_rf_model <- function(rf_out) {
  # Get plot features
  bird <- unique(rf_out$Species_Code)
  train_start <- rf_out %>%
    filter(.data$Set == "Train") %>%
    select(.data$YrQtr) %>%
    min()
  test_start <- rf_out %>%
    filter(.data$Set == "Test") %>%
    select(.data$YrQtr) %>%
    min()
  forecast_start <- rf_out %>%
    filter(.data$Set == "Forecast") %>%
    select(.data$YrQtr) %>%
    min()
  min_yr <- floor(min(rf_out$YrQtr))
  max_yr <- ceiling(max(rf_out$YrQtr))
  # Make basic plot
  plt <- rf_out %>%
    ggplot(aes(x = .data$YrQtr, y = .data$Count, color = .data$Model)) +
    geom_point() +
    geom_line() +
    geom_vline(xintercept = c(train_start,
                              test_start,
                              forecast_start),
               linetype = "dashed") +
    labs(title = bird) +
    scale_x_continuous(limits = c(min_yr, max_yr),
                       breaks = seq(min_yr, max_yr, 5),
                       minor_breaks = seq(min_yr,max_yr,1)) +
    scale_color_manual(values = c("red", "black", "blue"))

  return(plt)
}
