% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/species_counts.R
\name{plot_bird_count}
\alias{plot_bird_count}
\title{Create plot for individual bird.}
\usage{
plot_bird_count(bird, bbs_dat)
}
\arguments{
\item{bird}{Character vector for Species_Code found in bdat.}

\item{bbs_dat}{BBS dataframe with data for 'bird' Species_Code. May have more as well, intended to be passed through lapply in plot_counts.}
}
\value{
ggplot of counts over time for bird.
}
\description{
Create plot for individual bird.
}
