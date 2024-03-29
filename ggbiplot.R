#' Create a biplot with ggplot2, from a `prcomp` object.
#' 
#' It is assumed that `ggplot2` has been loaded to the 
#'   namespace.
#'   
#' @param pca_object An object created by the `prcomp` function 
#'   from the `stats`` package.
#' @param components Numeric of length 2. Which principal components 
#'   to plot, defaults to the first two.
#' @param add_variables Logical. Should the the variables be 
#'   added to the plot? Defaults to TRUE.
#' @param add_cases Logical. Should the cases (rows in the data 
#'   on which the pca was performed) be added to the plot? 
#'   Defaults to TRUE.
#' @return A `ggplot` object showing the biplot.
#' @examples 
#' pc_mtcars <- prcomp(mtcars)
#' ggbiplot(pc_mtcars)
#' ggbiplot(pc_mtcars, add_cases = FALSE)
#' ggbiplot(pc_mtcars, add_variables = FALSE)
#' ggbiplot(pc_mtcars, components = c(3, 4))
ggbiplot <- function(pca_object, 
                     components    = c(1, 2),
                     add_variables = TRUE,
                     add_cases     = TRUE){
  
  stopifnot(inherits(pca_object, "prcomp"),
            is.numeric(components),
            length(components) == 2)
  
  check_valid_components(pca_object, components)
  
  check_if_ggplot_is_loaded()
  
  pc_loadings <- get_loadings(pca_object, components)
  
  cols <- colnames(pc_loadings)
  
  vars_expl <- get_variance_explained(pca_object, components)
  
  return_plot <- ggplot(pc_loadings) +
    xlab(paste0(cols[1], " (", vars_expl[1], "% of variance)")) +
    ylab(paste0(cols[2], " (", vars_expl[2], "% of variance)"))
  
  if (add_variables) {
    return_plot <- 
      return_plot +
      geom_text(aes_string(x = cols[1], y = cols[2], label = "variables")) +
      geom_segment(aes_string(x = 'origin', 
                              y = 'origin', 
                              xend = cols[1], 
                              yend = cols[2]),
                   arrow = arrow(length = unit(0.5, "cm")), 
                   color = 'red') +
      xlab(paste0(cols[1], " (", vars_expl[1], "% of variance)")) +
      ylab(paste0(cols[2], " (", vars_expl[2], "% of variance)"))
  }

  
  if (add_cases) {
    cases <- extract_and_scale_cases(pca_object, components, pc_loadings)
    
    return_plot <- 
      return_plot + 
      geom_text(data = cases, 
                aes_string(x = cols[1], y = cols[2], label = "names"),
                color = "blue")
  }
  return_plot
}

check_valid_components <- function(pca_object, 
                                   components) {
  ncomps <- ncol(pca_object$rotation)
  if (max(components) > ncomps) {
    stop("The largest component asked exceeds the number of components in the pca_object")
  }
}

check_if_ggplot_is_loaded <- function() {
  if (!"ggplot2" %in% (.packages())) {
    "Please run library(ggplot2) before using this function"
  }
}

get_loadings <- function(pca_object, 
                         components) {
  rotations <- as.data.frame(pca_object$rotation[, components])
  rotations$variables <- rownames(rotations)
  # origin is needed for the arrows in the biplot
  rotations$origin <- 0  
  rotations
}

get_variance_explained <- function(pca_object,
                                  components) {
  variances_expl <- pca_object$sdev / sum(pca_object$sdev) * 100
  round(variances_expl[components], 2)
}

extract_and_scale_cases <- function(pca_object, 
                                    components,
                                    pc_loadings) {
  cases <- as.data.frame(pca_object$x[,components])
  
  # the cases need to be scaled to the PC scales 
  loadings_max    <- max(abs(apply(pc_loadings[ ,1:2], 2, range)))
  projections_max <- max(abs(cases))
  
  cases <- cases * (loadings_max / projections_max)
  cases$names <- rownames(pca_object$x[,components])
  cases
}
