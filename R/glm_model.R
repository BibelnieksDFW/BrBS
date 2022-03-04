#' Fit GLMs for one or more species based on island-wide counts.
#'
#' @param dat Dataframe with counts and predictors for each species to be modeled
#' @param family Family of GLM to apply to data. One of "poisson", "negative binomial", or "auto".
#' @param species_formulas Optional - tibble with two columns: Species_Code and formulas. Each entry of formulas should be a list of valid formula character vectors.
#' @param response Optional - name of response column. Default is Count.
#' @param choose_terms Optional - names of terms selected by AIC. Defaults are Year, Year2, and Qtr.
#' @param fix_terms Optional - names of terms always included in model. Defaults to N_Stations.
#' @param depends Optional - tibble with two columns: term and on. term is a name in choose_terms and is another name from choose_terms that term depends on. Default has term = Year2, on = Year.
#' @param robust_se Flag for including HAC robust standard errors from package 'sandwich' in plots.
#' @param verbose Flag for printing timing and progress updates.
#'
#'
#' @return A tibble with species_code (char), models (list of glm objects), fit_plots, back_trans_plots, resid_plots, and unexp_plots (all lists of ggplots)
#' @export
#'
model_birds_glm <- function(dat,
                            family = "poisson",
                            species_formulas = NULL,
                            response = "Count",
                            choose_terms = c("Year", "Year2", "Qtr"),
                            fix_terms = "N_Stations",
                            depends = tibble(term = "Year2", on = "Year"),
                            robust_se = FALSE,
                            verbose = TRUE) {
  # Time the modeling
  if(verbose){
    tictoc::tic()
  }

  # REMOVE IN FINAL: Set for walkthrough debug
  # dat = glm_dat_spn
  # family = "negative binomial"
  # b = 6
  # response = "Count"
  # choose_terms = c("Year", "Year2", "Qtr")
  # fix_terms = "N_Stations"
  # depends = tibble(term = "Year2", on = "Year")

  # If set formulas not provided, create combinations to search over
  if(is.null(species_formulas)) {
    # Build modeling formulas
    species_formulas <- tidyr::tibble(Species_Code = unique(dat$Species_Code),
                               formulas = list(build_formulas(response, choose_terms, fix_terms, depends)))
  } else {
    # Check that enough formulas were provided
    if(length(unique(dat$Species_Code)) != nrow(species_formulas)) {
      print("Incomplete formula specification - Include all species or omit formula argument.")
      return(NULL)
    }
  }

  # Set up for iterating through species
  birds <- unique(dat$Species_Code)
  model_list <- list()
  fit_plot_list <- list()
  back_trans_plot_list <- list()
  fit_resid_plot_list<- list()
  unexp_plot_list <- list()
  for(b in seq_along(birds)) {
    # Get data for bird
    bdat <- dat %>%
      filter(.data$Species_Code == birds[b]) %>%
      # Create log count for use later
      mutate(log_Count = log(.data$Count)) %>%
      ungroup()

    # Get formulas for bird
    forms <- species_formulas %>%
      filter(.data$Species_Code == birds[b]) %>%
      select(.data$formulas) %>%
      pull() %>%
      unlist()

    # Fit model, using chosen family
    if(family == "auto") {
      # Stage 1: fit a poisson model, search all available formulas
      poi <- fit_glm(bdat, forms, "poisson")
      # Stage 2: test fit (evaluating overdispersion)
      gof <- gof_test(poi)
      if(gof$p > .05) {
        # Pass: keep poisson model
        model_list[[b]] <- poi
      } else {
        # Fail > Stage 3: fit nbinom model, again search all formulas
        nbinom <- fit_glm(bdat, forms, "negative binomial")
        gof <- gof_test(nbinom)
        if(gof$p > .05) {
          # Pass: keep nbinom model
          model_list[[b]] <- nbinom
        } else {
          # Fail > Stage 4: choose poisson or nbinom by AIC
          if(stats::AIC(nbinom) < stats::AIC(poi)) {
            model_list[[b]] <- nbinom
          } else{
            model_list[[b]] <- poi
          }
        }
      }
    } else {
      model_list[[b]] <- fit_glm(bdat, forms, family)
    }

    # Get std error for CI's
    mod_se <- stats::predict(model_list[[b]], type = "link", se.fit = TRUE)

    # Plot
    bdat <- bdat %>% mutate(fit = log(model_list[[b]]$fitted.values),
                            resids = model_list[[b]]$residuals,
                            fit_exp = model_list[[b]]$fitted.values,
                            lower_exp = exp(mod_se$fit - 1.96 * mod_se$se.fit),
                            upper_exp = exp(mod_se$fit + 1.96 * mod_se$se.fit)
    )
    # HAC robust std errors also
    if(robust_se) {
      mod_design <- stats::model.matrix(model_list[[b]])
      mod_vcovHAC <- sandwich::vcovHAC(model_list[[b]], weights = sandwich::weightsAndrews)
      mod_se_hac <- sqrt(diag(mod_design %*% mod_vcovHAC %*% t(mod_design)))

      bdat <- bdat %>% mutate(lower_exp_HAC = exp(mod_se$fit - 1.96 * mod_se_hac),
                              upper_exp_HAC = exp(mod_se$fit + 1.96 * mod_se_hac)
      )
    }



    min_yr <- floor(min(bdat$YrQtr))
    max_yr <- ceiling(max(bdat$YrQtr))

    fit_plot_list[[b]] <- bdat %>% ggplot(aes(x = .data$YrQtr, y = .data$log_Count)) +
      geom_point() + geom_line() +
      geom_line(aes(y = .data$fit), color = "red") +
      labs(title = birds[b])

    back_trans_plot_list[[b]] <- bdat %>% ggplot(aes(x = .data$YrQtr, y = .data$Count)) +
      geom_point() + geom_line() +
      geom_line(aes(y = .data$fit_exp), color = "red") +
      geom_ribbon(aes(ymin = .data$lower_exp, ymax = .data$upper_exp), alpha = .1, fill = "red") +
      scale_x_continuous(breaks = seq(min_yr, max_yr, 5), minor_breaks = seq(min_yr,max_yr,1)) +
      labs(title = birds[b])
    if(robust_se) {
      back_trans_plot_list[[b]] <- back_trans_plot_list[[b]] +
        geom_ribbon(aes(ymin = .data$lower_exp_HAC, ymax = .data$upper_exp_HAC), alpha = .2, fill = "orange")
    }


    fit_resid_plot_list[[b]] <- bdat %>% ggplot(aes(x = .data$fit, y = .data$resids)) +
      geom_point() +
      labs(title = sprintf("Fitted vs Residuals for %s", birds[b]))

    unexp_plot_list[[b]] <- bdat %>% ggplot(aes(x = .data$YrQtr, y = .data$resids)) +
      geom_col() +
      geom_hline(yintercept = 0) +
      labs(title = sprintf("Unexplained Variation for %s", birds[b]))

    if(verbose){
      print(sprintf("%s complete (%d/%d)", birds[b], b, length(birds)))
    }
  }

  if(verbose){
    tictoc::toc()
  }

  models_plots <- tidyr::tibble(species_code = birds,
                                 models = model_list,
                                 fit_plots = fit_plot_list,
                                 back_trans_plots = back_trans_plot_list,
                                 resid_plots = fit_resid_plot_list,
                                 unexp_plots = unexp_plot_list)
  return(models_plots)
}

#' Fit a Poisson or Negative Binomial GLM to determine
#' @param bdat BBS data filtered to one specific species. Dataframe with columns YrQtr, Year, Year2, Qtr, N_Stations, and Count.
#' @param formulas Character vector with model formulas.
#' @param family Type of model to use. One of "poisson" or "negative binomial".
#'
#' @return Object of class glm, with formula selected via AIC.
fit_glm <- function(bdat, formulas, family) {
  if(family == "poisson") {
    # Poisson distribution, log link
    mods <- lapply(formulas, stats::glm, data = bdat, family = stats::poisson())
    # Check AIC
    aics <- unlist(lapply(mods, stats::AIC))
    # Save the best one (min AIC)
    bmod <- mods[[which(aics == min(aics))]]
  } else if(family == "negative binomial") {
    # Negative Binomial distribution, log link
    mods <- lapply(formulas, MASS::glm.nb, data = bdat)
    # Check AIC
    aics <- unlist(lapply(mods, stats::AIC))
    # Save the best one (min AIC)
    bmod <- mods[[which(aics == min(aics))]]
  } else {
    print("Invalid family argument passed, returning NULL.")
    bmod <- NULL
  }

  return(bmod)
}

#' Build formula strings for passing to lm and glm type calls.
#'
#' @param resp Name of response variable
#' @param prmt Character vector with terms to permute
#' @param const Character vector with terms to always include
#' @param depends Tibble describing dependent terms (i.e. Year2 depends on Year being in the model)
#'
#' @return Character vector of formulas
#' @export
#'
build_formulas <- function(resp, prmt, const, depends) {
  # List all combos of terms in prmt
  combos <- unlist(lapply(0:length(prmt), utils::combn,
                          x = prmt, simplify = FALSE),
                   recursive = FALSE)
  # Check dependencies, remove dupes
  combos <- unique(lapply(combos, dep_check,
                          dep = depends))

  # Add fixed terms
  combos <- lapply(combos, c,
                   const)

  # Build formula string
  forms <- paste(resp,
                 lapply(combos, paste,
                        collapse = " + "),
                 sep = " ~ ")

  return(forms)
}

#' Check dependencies when building formula strings.
#'
#' @param combo Character vector with combination of terms
#' @param dep Dataframe with dependencies to check
#'
#' @return Character vector with appropriate term dependencies
#'
dep_check <- function(combo, dep) {
  for(i in 1:nrow(dep)) {
    if(dep$term[i] %in% combo &
       !dep$on[i] %in% combo) {
      combo <- c(dep$on[i], combo)
    }
  }
  return(combo)
}

#' Extract formula character vector from glm object
#'
#' @param mod glm object
#'
#' @return character vector representing formula
#' @export
#'
get_glm_formula <- function(mod) {
  pieces <- as.character(stats::formula(mod))
  whole <- sprintf("%s ~ %s", pieces[2], pieces[3])
  return(whole)
}

#' Goodness of fit test based on residual deviance.
#'
#' @param glm glm object to conduct test for.
#'
#' @return List of 3 consisting of residual deviance, degrees of freedom, and p-value.
#' @export
#'
gof_test <- function(glm) {
  res_deviance <- glm$deviance
  df <- glm$df.residual
  p <- stats::pchisq(res_deviance, df, lower.tail = FALSE)

  ret <- list(res_deviance = res_deviance,
               df = df,
               p = p)
  return(ret)
}

#' Prep BBS data for GLM modeling.
#' Create dataframe with Count, Year, Year squared, Quarter, and N_Stations for passing to model_birds_glm.
#'
#' @param bbs_dat Dataframe with BBS data.
#'
#' @return Dataframe with columns suitable for BrBS' default glm modeling.
#' @export
#'
prep_dat_glm <- function(bbs_dat) {
  # Sum islandwide counts
  spec_counts <- islandwide_counts(bbs_dat)
  # Get N_Stations control variable
  n_stats <- get_n_stats(bbs_dat)

  # Create glm vars, join N_Stations
  glm_dat <- spec_counts %>%
    mutate(Year = floor(.data$YrQtr),
           Year2 = .data$Year^2,
           Qtr = as.factor((.data$YrQtr - .data$Year)*4+1)) %>%
    left_join(n_stats, by = "YrQtr")

  return(glm_dat)
}

