#' Produce visualizations and model summary for a single species.
#'
#' @param glm_out Tibble output of model_birds_glm.
#' @param bdx Species code or integer index for lists in glm_out.
#'
#' @return Prints plots associated with species model.
#' @export
#'
visualize_bird_glm <- function(glm_out, bdx) {
  if(!is.numeric(bdx)) {
    bdx <- which(glm_out$species_code == bdx)
  }
  # Plot back-transformed fit and residuals
  graphics::plot(glm_out$back_trans_plots[[bdx]])
  graphics::plot(glm_out$resid_plots[[bdx]])
  graphics::plot(glm_out$unexp_plots[[bdx]])

  stats::qqnorm(glm_out$models[[bdx]]$residuals)
  stats::qqline(glm_out$models[[bdx]]$residuals)

  # Print model summary
  print(summary(glm_out$models[[bdx]]))
}

#' Create a summary table indicating trends identified in the models.
#'
#' @param bmod_tbl Tibble output of model_birds_glm.
#'
#' @return gt table summarizing trends, model type, and model fit.
#' @export
#'
glm_exec_summary <- function(bmod_tbl) {
  # Create summary for each species
  gt_dat <- bmod_tbl$models %>%
    lapply(glm_exec_summary_row) %>%
    bind_rows() %>%
    cbind(Species_Code = bmod_tbl$species_code)

  # Build table
  exec_gt <- gt_dat %>%
    gt::gt(rowname_col = "Species_Code") %>%
    gt::cols_label(Species_Code = "Species Code",
                   Year = "Yearly Trend",
                   Season = "Seasonality",
                   N_Stations = "# Stations",
                   Family = "Model",
                   GOF = "GOF Test") %>%
    gt::tab_header("GLM Findings (Best Model by AIC)")

  exec_gt
}

#' Create row of executive summary table.
#' @param bmod Object of class glm or negbin corresponding to model for one species.
#'
#' @return Dataframe with one row and columns Year, Season, and N_Stations, Family, and GOF for building into executive summary table.
glm_exec_summary_row <- function(bmod) {
  # Identify Year trend
  selected <- names(bmod$coefficients)
  yr <- case_when("Year2" %in% selected ~ "Quadratic",
                  "Year" %in% selected ~ "Linear",
                  TRUE ~ "N")
  # If there is a trend, calculate year gradient at end for inc/dec
  if(yr == "Quadratic") {
    dyr <- bmod$coefficients["Year"] + 2 * bmod$coefficients["Year2"] * utils::tail(bmod$model$Year, n = 1)
    yr <- case_when(dyr >= 0 ~ paste0(yr," (inc)"),
                    dyr < 0 ~ paste0(yr, " (dec)"))
  } else if(yr == "Linear") {
    dyr <- bmod$coefficients["Year"]
    yr <- case_when(dyr >= 0 ~ paste0(yr," (inc)"),
                    dyr < 0 ~ paste0(yr, " (dec)"))
  }

  # Identifying Season trend
  ssn <- case_when(length(grep("Qtr", selected)) > 0 ~ "Y",
                   TRUE ~ "N")
  if(ssn == "Y") {
    qtrs <- data.frame(qtr = grep("Qtr", selected, value = TRUE),
                       beta = bmod$coefficients[grep("Qtr", selected)]) %>%
      bind_rows(data.frame(qtr = "Qtr1", beta = 0)) %>%
      arrange(desc(beta))
    ssn <- paste0(qtrs$qtr, collapse = " > ")

  }

  # Check that N_Stations is positively correlated, or at least not significantly negatively correlated.
  nstats <- case_when(bmod$coefficients["N_Stations"] >= 0 ~ "+",
                      bmod$coefficients["N_Stations"] < 0 ~ "-",
                      TRUE ~ "N")
  if(nstats != "N") {
    nstats_pval <- bmod %>%
      summary() %>%
      stats::coef() %>%
      .[which(rownames(.)=="N_Stations"),4]
    if(nstats_pval < 0.05) {
      nstats <- paste0(nstats, "*")
    }
  }

  # Identify model family
  fam <- ifelse(stats::family(bmod)[1] == "poisson",
                "Poisson",
                "Negative Binomial")

  # Check for goodness of fit
  gof <- gof_test(bmod)
  gof_pass <- ifelse(gof$p > .05,
                     sprintf("Pass (%.3f)", gof$p),
                     sprintf("Fail (%.3f)", gof$p))

  ret <- data.frame(Year = yr,
                    Season = ssn,
                    N_Stations = nstats,
                    Family = fam,
                    GOF = gof_pass)
}

