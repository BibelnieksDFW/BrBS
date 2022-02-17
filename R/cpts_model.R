#### Functions for fitting changepoint models ####


#' Build changepoint models and plots for multiple species.
#' Convenient wrapper for applying to multiple species at once. Modeling driven by model_cpts and plotting done by plot_cpts_model.
#'
#' @param dat Dataframe with time and count.
#' @param min_len Minimum time length (data points) to model changepoints.
#' @param alpha Specify level at which to control FWE or FDR.
#' @param multiplicity Specify form of multiplicity control. One of "none", "bonferroni", "BH" (equivalently, "FDR"), "BY". Defaults to "BH"
#' @param boot_iter Number of bootstrap iterations for changepoint selection statistic.
#' @param robust_se Flag for including HAC robust standard errors from package 'sandwich' in plots.
#' @param verbose Flag for printing timing and progress updates.
#'
#'
#' @return A tibble with species_code (char), models (list of list of negbin objects), paths (list of path lists), fit_plots, resid_plots, and unexp_plots (all lists of ggplots)
#' @export
#'
model_birds_cpts <- function(dat,
                             min_len = 10,
                             alpha = 0.05,
                             multiplicity = "BH",
                             boot_iter = 10^3,
                             robust_se = FALSE,
                             verbose = TRUE) {
  # dat = cpts_dat %>% filter(Species_Code == "MAFD")
  # Species to model
  birds <- unique(dat$Species_Code)

  # Setup to save results
  mod_list <- list()
  path_list <- list()
  fit_plt_list <- list()
  resid_plt_list <- list()
  unexp_plt_list <- list()
  for(b in seq_along(birds)) {
    if(verbose) {
      tictoc::tic()
    }
    bdat <- dat %>%
      filter(.data$Species_Code == birds[b]) %>%
      select(.data$time, .data$count)
    fit_obj <- model_cpts(bdat,
                          min_len = min_len,
                          alpha = alpha,
                          multiplicity = multiplicity,
                          boot_iter = boot_iter
    )
    mod_list[[b]] <- fit_obj$mods
    path_list[[b]] <- fit_obj$path
    plts <- plot_cpts_model(mod_list[[b]], robust_se = robust_se, species = birds[b])
    fit_plt_list[[b]] <- plts$fit
    resid_plt_list[[b]] <- plts$resid_fit
    unexp_plt_list[[b]] <- plts$resid_time
    if(verbose) {
      tictoc::toc()
      print(sprintf("%s Modeled (%d/%d)", birds[b], b, length(birds)))
    }
  }
  models_plots <- tidyr::tibble(species_code = birds,
                                 models = mod_list,
                                 paths = path_list,
                                 fit_plots = fit_plt_list,
                                 resid_plots = resid_plt_list,
                                 unexp_plots = unexp_plt_list)
  models_plots
}


#' Identify possible changepoints for testing.
#'
#' @param bdat Dataframe with time and count.
#' @param min_len Minimum time length for any segment.
#'
#' @return Vector of valid changepoints to test.
get_cpts <- function(bdat, min_len = 10) {
  first <- min_len + 1
  last <- nrow(bdat) - min_len
  if(first <= last) {
    cpts <- bdat$time[first:last]
  } else {
    cpts <- c()
  }
  cpts
}

#' Given data and location of changepoint, fit a model.
#'
#' @param cpt Changepoint to fit. Should be a value in time column.
#' @param bdat Dataframe with time and count.
#'
#' @return glm object for changepoint model.
#'
fit_cpt <- function(cpt, bdat) {
  cmod <- MASS::glm.nb(formula = count ~ time*I(time >= cpt),
                       data = bdat)
  cmod
}

#' Fit a glm model model with an arbitrary number of changepoints.
#' Applies binary segmentation to obtain changepoints that approximately maximize likelihood.
#'
#' @param bdat Dataframe with time and count.
#' @param min_len Minimum time length to model changepoints.
#' @param alpha Specify level at which to control FWE or FDR.
#' @param multiplicity Specify form of multiplicity control. One of "none", "bonferroni", "BH" (equivalently, "FDR"), "BY". Defaults to "BH"
#' @param boot_iter Number of bootstrap iterations for changepoint selection statistic.
#'
#' @return List of glm objects collectively representing multiple-changepoint model.
#' @export
#'
model_cpts <- function(bdat, min_len = 10, alpha = 0.05, multiplicity = "BH", boot_iter = 10^3) {

  # Fit model without cpt
  nocmod <- MASS::glm.nb(formula = count ~ time,
                         data = bdat)

  # Identify potential cpts
  cpts <- get_cpts(bdat, min_len)

  # If no more cpts, return no cpt model
  if(length(cpts) == 0) {
    return(list(mods = list(nocmod),
                path = NULL
    )
    )
  }

  # Fit all cpts
  cmods <- lapply(as.list(cpts), fit_cpt, bdat = bdat)

  # Calculate test stats vs no cpt model
  comps <- lapply(cmods, stats::anova, nocmod)
  lrstats <- unlist(lapply(comps, function(x) x$`LR stat.`[2]))

  # Determine critical value based on type of multiplicity control
  if(multiplicity == "none") {
    # No control, use unadjusted alpha
    lrcrit <- stats::qchisq(alpha, 2, lower.tail = FALSE)

  } else if(multiplicity == "bonferroni") {
    # Apply Bonferroni correction
    lrcrit <- stats::qchisq(alpha/length(cpts), 2, lower.tail = FALSE)

  } else if(multiplicity ==  "BH" | multiplicity == "fdr") {
    # Apply Benjamini-Hochberg procedure to control FDR
    # Get p-vals, order ascending, calculate threshold
    bh_df <- data.frame(cpts = cpts,
                        p_vals = unlist(lapply(comps, function(x) x$`Pr(Chi)`[2]))) %>%
              arrange(.data$p_vals) %>%
              mutate(p_crit = alpha * (row_number()/length(cpts))) %>%
              filter(.data$p_vals <= .data$p_crit)
    # If even the smallest p-value is too big, show strictest threshold
    if(nrow(bh_df) == 0) {
      p_crit <- alpha/length(cpts)
    } else {
      p_crit <- bh_df %>%
        summarize(max_crit = max(.data$p_crit)) %>%
        pull(.data$max_crit)
    }

    # Calculate critical value based on BH-p-val thresholds
    lrcrit <- stats::qchisq(p_crit, 2, lower.tail = FALSE)

  } else if(multiplicity == "BY") {
    # Apply Benjamini-Yekutieli procedure to control FDR
    # Get p-vals, order ascending, calculate threshold
    harm <- sum(1/seq(1, length(cpts)))
    by_df <- data.frame(cpts = cpts,
                        p_vals = unlist(lapply(comps, function(x) x$`Pr(Chi)`[2]))) %>%
              arrange(.data$p_vals) %>%
              mutate(p_crit = alpha * (row_number()/(length(cpts) * harm))) %>%
              filter(.data$p_vals <= .data$p_crit)
    # If even the smallest p-value is too big, show strictest threshold
    if(nrow(by_df) == 0) {
      p_crit <- alpha/(length(cpts)*harm)
    } else {
      p_crit <- by_df %>%
        summarize(max_crit = max(.data$p_crit)) %>%
        pull(.data$max_crit)
    }

    # Calculate critical value based on BY-p-val threshold
    lrcrit <- stats::qchisq(p_crit, 2, lower.tail = FALSE)
  }

  # Bootstrap distribution of LRT statistic supremum
  # Make sure there's more than one potential changepoint
  if(length(cpts) > 1) {
    boot_sups <- c()
    for(i in 1:boot_iter) {
      boot_sample <- sample(lrstats, length(lrstats), replace = TRUE)
      boot_sups[i] <- max(boot_sample)
    }
    boot_cpts <- sapply(boot_sups, function(x) cpts[which(lrstats == x)])
  } else {
    # With just one, resampling is trivial, just generate filler for plot
    boot_cpts <- rep(cpts, boot_iter)
  }

  ######## TODO: Revise significance testing here informed by Zeileis et al 2002 section 5.3
  # Use some aggregate of Chi-square statistics to test for significance.
  # See aggregated F (Chow) tests as put forth by Andrews (1993) and Andrews and Ploberger (1994)
  #   based on supremum, average, and expectation (?)
  # Currently takes supremum of LR stats, then bootstraps distribution for visual check.

  # Find cpt that maximizes the log likelihood
  logLiks <- sapply(cmods, stats::logLik)
  best_idx <- which(logLiks == max(logLiks))
  best_cmod <- cmods[[best_idx]]

  # Compare to see if cpt is significant
  best_comp <- comps[[best_idx]]
  # p <- pchisq(comp$Deviance[2], comp$Df[2], lower.tail = FALSE)
  # p <- comp$`Pr(Chi)`[2]
  sig <- best_comp$`LR stat.`[2] > lrcrit

  # Plot LRT statistic profile and distribution of supremum
  cpts_dist <- data.frame(boot_cpts = boot_cpts) %>%
                ggplot(aes(.data$boot_cpts)) +
                geom_density() +
                geom_vline(xintercept = cpts[best_idx], linetype = "dashed") +
                scale_x_continuous(limits = c(floor(min(cpts)), ceiling(max(cpts)))) +
                labs(x = "Changepoints",
                     y = "Density (Sup.)")

  lr_plt <- data.frame(Changepoints = cpts,
                       Test_Stats = lrstats) %>%
              ggplot(aes(x = .data$Changepoints, y = .data$Test_Stats)) +
              geom_point() +
              geom_vline(xintercept = cpts[best_idx], linetype = "dashed") +
              geom_hline(yintercept = lrstats[best_idx], linetype = "dashed") +
              # unadjusted critical value for reference
              geom_hline(yintercept = stats::qchisq(alpha, 2, lower.tail = FALSE),
                         color = "red",
                         linetype = "dashed") +
              geom_hline(yintercept = lrcrit, color = "red") +
              scale_x_continuous(limits = c(floor(min(cpts)), ceiling(max(cpts)))) +
              labs(title = sprintf("Supremum:  %.2f (%s)",
                                   cpts[best_idx],
                                   ifelse(sig, "S", "NS")),
                   x = element_blank(),
                   y = "LR Stat.")

  # Make sure we have more than one changepoint before connecting dots
  if(length(cpts) > 1) {
    lr_plt <- lr_plt + geom_line()
  }

  # Left-align x-axes and stack
  node_plt <- rbind(ggplotGrob(lr_plt), ggplotGrob(cpts_dist))
  # Also make label for plotting in tree
  node_label <- grid::textGrob(sprintf("%.2f (%s)",
                                       cpts[best_idx],
                                       ifelse(sig, "S", "NS")))

  if(sig) {
    # If significant, now search both sides for more changepoints

    # Break up left and right sides
    left_dat <- bdat %>%
                  filter(.data$time < cpts[best_idx])
    right_dat <- bdat %>%
                  filter(.data$time >= cpts[best_idx])

    # Model both sides
    left_path <- model_cpts(left_dat, min_len, alpha, multiplicity, boot_iter)
    right_path <- model_cpts(right_dat, min_len, alpha, multiplicity, boot_iter)

    # Append sides together and return
    return(list(mods = append(left_path$mods, right_path$mods),
                path = list(node_plt = node_plt,
                            node_label = node_label,
                            best_cpt = cpts[best_idx],
                            left = left_path$path,
                            right = right_path$path
                )
    )
    )
  } else {
    # If not significant, return no cpt model
    return(list(mods = list(nocmod),
                path = list(node_plt = node_plt,
                            node_label = node_label,
                            best_cpt = NULL,
                            left = NULL,
                            right = NULL
                )
    )
    )
  }
}
