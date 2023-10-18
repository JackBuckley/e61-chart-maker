
#' Automatically select appropriate color scale
#'
#' @param mapping Aesthetics used in \code{ggplot}.
#' @param palette Color palette.
#' @param data An optional \code{data.frame} to choose the right type for variables.
#' @param fill_type,color_type Scale to use according to the variable used
#'  in \code{fill}/\code{color} aesthetic : \code{"discrete"} or \code{"continuous"}.
#'  Ignored if \code{data} is provided: it will be guessed from data.
#' @param reverse Reverse colors order or not.
#'
#' @return a \code{list}
#' @export
#'
#' @importFrom ggplot2 scale_fill_hue scale_color_hue scale_fill_gradient scale_color_gradient
#'  scale_fill_brewer scale_color_brewer scale_fill_distiller scale_color_distiller
#'  scale_fill_viridis_c scale_color_viridis_c scale_fill_viridis_d scale_color_viridis_d
#' @importFrom rlang is_named
#'
#' @examples
#' library(ggplot2)
#'
#' # Automatic guess according to data
#' which_pal_scale(
#'   mapping = aes(fill = Sepal.Length),
#'   palette = "ggplot2",
#'   data = iris
#' )
#' which_pal_scale(
#'   mapping = aes(fill = Species),
#'   palette = "ggplot2",
#'   data = iris
#' )
#'
#'
#' # Explicitly specify type
#' which_pal_scale(
#'   mapping = aes(color = variable),
#'   palette = "Blues",
#'   color_type = "discrete"
#' )
#'
#'
#' # Both scales
#' which_pal_scale(
#'   mapping = aes(color = var1, fill = var2),
#'   palette = "Blues",
#'   color_type = "discrete",
#'   fill_type = "continuous"
#' )
which_pal_scale <- function(mapping,
                            palette = "base",
                            data = NULL,
                            fill_type = c("continuous", "discrete"),
                            color_type = c("continuous", "discrete"),
                            reverse = FALSE) {
  # checks
  if (length(palette) < 1) return(list())

  args <- list()
  fill_type <- match.arg(fill_type)
  color_type <- match.arg(color_type)

  # check data type
  if (!is.null(data)) {
    data_mapped <- lapply(mapping, rlang::eval_tidy, data = data)
    if (inherits(x = data_mapped$fill, what = c("character", "factor"))) {
      fill_type <- "discrete"
    } else {
      fill_type <- "continuous"
    }
    if (inherits(x = data_mapped$colour, what = c("character", "factor"))) {
      color_type <- "discrete"
    } else {
      color_type <- "continuous"
    }
  }

  # # Return scale functions
  # if (!is.null(mapping$fill)) {
  #   fill_scale <- switch(fill_type,
  #                        "discrete" = "scale_fill_e61",
  #                        "continuous" = "scale_fill_e61")
  #   args[[fill_scale]] <- switch(
  #     fill_type,
  #     "discrete" = list(discrete = TRUE, n = 10),
  #     "continuous" = list(discrete = FALSE, n = 10)
  #   )
  # } else {
  #   fill_scale <- NULL
  # }
  #
  # if (!is.null(mapping$colour)) {
  #   color_scale <- switch(color_type,
  #                         "discrete" = "scale_colour_e61",
  #                         "continuous" = "scale_colour_e61")
  #   args[[color_scale]] <- switch(
  #     color_type,
  #     "discrete" = list(discrete = TRUE, n = 10),
  #     "continuous" = list(discrete = FALSE, n = 10)
  #   )
  # } else {
  #   color_scale <- NULL
  # }

  fill_scale <- NULL
  color_scale <- NULL

  return(list(
    scales = c(fill_scale, color_scale),
    args = args
  ))
}
