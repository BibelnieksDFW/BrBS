#### Functions for summarizing and visualizing changepoint models ####

#' Retrieve fitted changepoints from model list
#'
#' @param modlist Model list from model_cpts
#'
#' @return Vector of changepoints.
#' @export
#'
get_modeled_cpts <- function(modlist) {
  cpts <- lapply(modlist[-1], function(x) min(x$model$time))
  unlist(cpts)
}

#' Plot changepoint models.
#'
#' @param modlist Model list from model_cpts
#' @param robust_se Flag for including HAC robust standard errors from package 'sandwich' in plots.
#' @param species Species code for species being modeled. Used in title of plot.
#'
#' @return ggplot of fitted model.
#' @export
#'
plot_cpts_model <- function(modlist, robust_se = FALSE, species = "") {

  # modlist = bmod_cpt; species = "MAFD"; robust_se = TRUE

  # Grab plotting vars from model list
  count <- unlist(lapply(modlist, function(x) x$y))
  time <- unlist(lapply(modlist, function(x) x$model$time))
  fit_exp <- unlist(lapply(modlist, stats::fitted))
  resids <- unlist(lapply(modlist, stats::resid))
  se <- unlist(lapply(modlist, function(x) {
                                            design_mat <- stats::model.matrix(x)
                                            vcov_mat <- stats::vcov(x)
                                            sqrt(diag(design_mat %*% vcov_mat %*% t(design_mat)))
                                          })
               )
  # Min/max model years
  min_yr <- floor(min(time))
  max_yr <- ceiling(max(time))
  # Estimated Changepoints
  cpts <- get_modeled_cpts(modlist)
  # Construct plot df
  plot_dat <- data.frame(fit_exp = fit_exp,
                         lower_exp = exp(log(fit_exp) - 1.96 * se),
                         upper_exp = exp(log(fit_exp) + 1.96 * se),
                         resids = resids,
                         time = time,
                         count = count
  )
  # Calculate robust standard errors and add to df
  if(robust_se) {
    se_hac <- unlist(lapply(modlist, function(x) {
      design_mat <- stats::model.matrix(x)
      vcov_mat <- sandwich::vcovHAC(x, weights = sandwich::weightsAndrews)
      sqrt(diag(design_mat %*% vcov_mat %*% t(design_mat)))
    }))
    plot_dat <- plot_dat %>%
                  mutate(lower_exp_HAC = exp(log(fit_exp) - 1.96 * se_hac),
                         upper_exp_HAC = exp(log(fit_exp) + 1.96 * se_hac)
                  )
  }
  # Make ggplot of fit model
  fit_plt <- plot_dat %>%
              ggplot(aes(x = .data$time, y = .data$count)) +
              geom_point() + geom_line() +
              geom_line(aes(y = .data$fit_exp), color = "red") +
              geom_ribbon(aes(ymin = .data$lower_exp, ymax = .data$upper_exp), alpha = .1, fill = "red") +
              scale_x_continuous(breaks = seq(min_yr, max_yr, 5), minor_breaks = seq(min_yr,max_yr,1)) +
              geom_vline(xintercept = cpts, linetype = "dashed") +
              labs(title = sprintf("Fitted Model for %s", species),
                   x = "Year",
                   y = "Count")
  # Plot robust standard errors
  if(robust_se) {
    fit_plt <- fit_plt +
                geom_ribbon(aes(ymin = .data$lower_exp_HAC, ymax = .data$upper_exp_HAC), alpha = .2, fill = "orange")
  }

  # Make ggplot of resids vs fitted
  resid_fitted_plt <- plot_dat %>%
                        ggplot(aes(x = .data$fit_exp, y = .data$resids)) +
                        geom_point() +
                        labs(title = sprintf("Fitted vs Residuals for %s", species),
                             x = "Fitted",
                             y = "Residuals")


  # Make ggplot of resids vs time
  resid_time_plt <- plot_dat %>%
                      ggplot(aes(x = .data$time, y = .data$resids)) +
                      geom_col() +
                      geom_hline(yintercept = 0) +
                      geom_vline(xintercept = cpts, linetype = "dashed") +
                      labs(title = sprintf("Unexplained Variation for %s", species),
                           x = "Year",
                           y = "Residuals")
  # Return list of plots
  list(fit = fit_plt,
       resid_fit = resid_fitted_plt,
       resid_time = resid_time_plt)
}

#' Plot changepoint distribution over multiple species.
#'
#' @param cpts_fits Fitted models from model_birds_cpts.
#'
#' @return ggplot of stacked bar chart displaying the distribbution of changepoints.
#' @export
#'
plot_cpts_dist <- function(cpts_fits) {
  # Grab changepoints from fits
  cpt_list <- list()
  for(b in seq_along(unique(cpts_fits$species_code))) {
    cpts <- get_modeled_cpts(cpts_fits$models[[b]])
    cpt_list[[b]] <- data.frame(Species_Code = rep(cpts_fits$species_code[b], length(cpts)), Cpts = cpts)
  }
  cpts_all <- bind_rows(cpt_list) %>%
    mutate(n = 1, Cpts_Yr = floor(.data$Cpts))

  # Get time range
  time_range <- unlist(lapply(cpts_fits$models,
                              function(x) lapply(x,
                                                 function(y) y$model$time
                                                )
                              )
                      )
  # Build stacked bar chart
  min_yr <- floor(min(time_range))
  max_yr  <- floor(max(time_range))
  cpts_plt <- cpts_all %>%
    ggplot(aes(x = .data$Cpts_Yr,
               y = .data$n,
               fill = .data$Species_Code,
               label = .data$Species_Code)) +
    geom_bar(position = "stack", stat = "identity") +
    geom_text(size = 3, position = position_stack(vjust = 0.5), angle = 90) +
    scale_x_continuous(limits = c(min_yr, max_yr),
                       breaks = seq(min_yr, max_yr, 3),
                       minor_breaks = seq(min_yr,max_yr,1)) +
    labs(title = "Modeled Changepoints", x = "Year", y = element_blank())

  return(cpts_plt)
}


#' Plot binary segmentation path for changepoint model.
#'
#' @param cpts_path Path list from model_cpts or model_birds_cpts.
#'
#' @return Graphical object representing changepoint path.
#' @export
#'
plot_cpts_path <- function(cpts_path) {
  # Get layout
  lay <- build_path_layout(cpts_path, 1)
  lay <- trim_layout(lay)
  # Get grobs
  grobs <- build_path_grobs(cpts_path)
  # Arrange into one
  tree <- gridExtra::arrangeGrob(grobs = grobs, layout_matrix = lay)

  tree
}


#' Build grob list for arranging path plots.
#'
#' @param cpts_path Head of path.
#'
#' @return List of grobs.
#'
build_path_grobs <- function(cpts_path) {
  # Check where we're at in the tree
  if(is.null(cpts_path$left) & is.null(cpts_path$right)) {
    # Leaf - no children
    return(list(cpts_path$node_label))

  } else if(is.null(cpts_path$left)){
    # Only right child, null left
    right_plt <- build_path_grobs(cpts_path$right)
    return(append(list(cpts_path$node_label), right_plt))

  } else if(is.null(cpts_path$right)) {
    # Only left child, null right
    left_plt <- build_path_grobs(cpts_path$left)
    return(append(list(cpts_path$node_label), left_plt))

  } else {
    # Both children not null
    left_plt <- build_path_grobs(cpts_path$left)
    right_plt <- build_path_grobs(cpts_path$right)
    return(append(list(cpts_path$node_label), append(left_plt, right_plt)))

  }
}


#' Build layout matrix for arranging path grobs.
#'
#' @param cpts_path Head of path.
#' @param plot_num Current plot number (start with 1).
#'
#' @return Layout matrix for arrangeGrob.
#'
build_path_layout <- function(cpts_path, plot_num) {
  # Null path - non-existent child
  if(is.null(cpts_path)) {
    return(matrix(NA))
  }

  # Valid node - get left child first
  left_num <- plot_num + 1
  left_lay <- build_path_layout(cpts_path$left, left_num)
  # Get max node number from left to pass to right
  left_max <- left_lay[which(!is.na(left_lay))]
  left_max <- ifelse(length(left_max) == 0, NA, max(left_max))
  # Get right child
  right_num <- ifelse(is.na(left_max), plot_num + 1, left_max + 1)
  right_lay <- build_path_layout(cpts_path$right, right_num)

  # Balance layouts
  if(nrow(left_lay) > nrow(right_lay)) {
    right_lay <- pad_layout(right_lay, nrow(left_lay))
  } else if(nrow(right_lay) > nrow(left_lay)) {
    left_lay <- pad_layout(left_lay, nrow(right_lay))
  }

  # Combine left and right
  comb_lay <- cbind(left_lay, NA, right_lay)
  # Add current level
  comb_lay <- rbind(c(rep(NA, ncol(left_lay)), plot_num, rep(NA, ncol(right_lay))),
                    comb_lay)
  comb_lay
}

#' Pad layout matrix with NAs (for balancing layout).
#'
#' @param lay Layout matrix to pad.
#' @param height Height to pad to.
#'
#' @return Padded layout matrix.
#'
pad_layout <- function(lay, height) {
  # Get new width
  width <- sum(2^seq(0, height - 1))
  # Construct side pads
  side_pad <- matrix(NA,
                     nrow = height,
                     ncol = (width - ncol(lay))/2)
  # Construct bottom pad
  bottom_pad <- matrix(NA,
                       nrow = height - nrow(lay),
                       ncol = ncol(lay))
  # Pad
  padded <- cbind(side_pad, rbind(lay, bottom_pad), side_pad)

  padded
}

#' Trim excess NAs from layout matrix.
#'
#' @param lay Layout matrix to trim.
#'
#' @return Trimmed layout matrix.
#'
trim_layout <- function(lay) {
  # Check for more than one plot
  if(sum(!is.na(lay)) > 1) {
    # Remove all NA columns
    lay <- lay[,which(colSums(lay, na.rm = TRUE) > 0)]
    # Remove all NA rows
    lay <- lay[which(rowSums(lay, na.rm = TRUE) > 0),]
  } else {
    lay <- matrix(1)
  }

  lay
}

#' Find a node_plt that corresponds to a node_label
#'
#' @param path  Path list from model_cpts or model_birds_cpts.
#' @param label Label corresponding node plot to retrieve. Should come from get_node_labels.
#'
#' @return node_plt grob or NULL if no such label.
#' @export
#'
get_node_plt <- function(path, label) {
  # If we're at the right node, return the plot
  if(path$node_label$label == label) {
    return(path$node_plt)
  }
  # If not, check if more tree to search
  if(!is.null(path$best_cpt)) {
    # Extract numeric from label character vector
    label_num <- as.numeric(gsub("\\s\\(.*\\)", "", label))
    # If smaller than current cpt, go left, otherwise go right
    if(label_num < path$best_cpt) {
      return(get_node_plt(path$left, label))
    } else {
      return(get_node_plt(path$right, label))
    }
  } else {
    return(NULL)
  }
}

#' Retrieve node (changepoint) labels from a changepoint fit path.
#'
#' @param path Path list from model_cpts or model_birds_cpts.
#'
#' @return List of labels for each node in the fit path.
#' @export
#'
get_node_labels <- function(path) {
  if(is.null(path)) {
    return(NULL)
  } else {
    return(append(path$node_label$label,
                  append(get_node_labels(path$left),
                         get_node_labels(path$right))))
  }
}
