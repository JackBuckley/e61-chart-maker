
#' Generate code to create a \code{ggplot2}
#'
#' @param data Character. Name of the \code{data.frame}.
#' @param mapping List. Named list of aesthetics.
#' @param geom Character. Name of the geom to use (with or without "geom_").
#' @param geom_args List. Arguments to use in the geom.
#' @param scales Character vector. Scale(s) to use (with or without "scale_").
#' @param scales_args List. Arguments to use in scale(s),
#'  if \code{scales} is length > 1, must be a named list with \code{scales} names.
#' @param coord Character. Coordinates to use (with or without "coord_").
#' @param labs List. Named list of labels to use for title, subtitle, x & y axis, legends.
#' @param theme Character. Name of the theme to use (with or without "theme_").
#' @param theme_args Named list. Arguments for \code{\link[ggplot2:theme]{theme}}.
#' @param facet Character vector. Names of variables to use in \code{\link[ggplot2]{facet_wrap}}.
#' @param facet_row Character vector. Names of row variables to use in \code{\link[ggplot2]{facet_grid}}.
#' @param facet_col Character vector. Names of col variables to use in \code{\link[ggplot2]{facet_grid}}.
#' @param facet_args Named list. Arguments for \code{\link[ggplot2:facet_wrap]{facet_wrap}}.
#' @param xlim A vector of length 2 representing limits on x-axis.
#' @param ylim A vector of length 2 representing limits on y-axis.
#'
#' @return a \code{call} that can be evaluated with \code{eval}.
#' @export
#'
#' @importFrom stats setNames
#' @importFrom rlang sym syms expr as_name is_call call2 has_length
#' @importFrom ggplot2 ggplot aes theme facet_wrap vars coord_flip labs
#'
#' @example examples/ex-ggcall.R
ggcall <- function(data = NULL,
                   mapping = NULL,
                   geom = NULL,
                   geom_args = list(),
                   scales = NULL,
                   scales_args = list(),
                   coord = NULL,
                   add_logo = FALSE,
                   labs = list(),
                   theme = NULL,
                   theme_args = list(),
                   alpha = NULL,
                   facet = NULL,
                   facet_row = NULL,
                   facet_col = NULL,
                   facet_args = list(),
                   xlim = NULL,
                   ylim = NULL,
                   position = NULL) {

  show_legend <- F

  # Load data
  if (is.null(data))
    return(expr(ggplot()))
  if (!is_call(data)) {
    data <- as.character(data)
    if (grepl("::", data)) {
      data <- str2lang(data)
    } else {
      data <- sym(data)
    }
  }

  # Load mapping
  if (rlang::is_call(mapping))
    mapping <- eval(mapping)
    mapping <- dropNulls(mapping)

    aes <- expr(aes(!!!syms2(mapping)))
    aes <- deparse(aes)
    aes <- gsub("\\)$", "", aes)

  # add alpha
  if(!is.null(alpha) & alpha != 1){

   aes <- paste0(aes, ", alpha = ", alpha, ")")
  } else {
    aes <- paste0(aes, ")")
  }

  ggcall <- paste0("theme61::ggplot(", data, ", ", aes, ")")

  # if(geom == "density_ridges"){
  #   geom_temp <<- geom
  #   geom_args_temp <<- geom_args
  #   ggcall_temp <<- ggcall
  #
  #   stop()
  # }

  # facet options
  if (!is.null(facet)) {
    facet_args <- dropNullsOrEmpty(facet_args)
    if (length(facet_args) > 0) {
      facet <- paste0("facet_wrap(vars(", facet, "), ", facet_args, ")")
      ggcall <- paste0(ggcall, " + ", facet)
    } else {
      facet <- paste0("facet_wrap(vars(", facet, "))")
      ggcall <- paste0(ggcall, " + ", facet)
    }

  } else if (!is.null(facet_row) | !is.null(facet_col)) {
    facet_args$ncol <- NULL
    facet_args$nrow <- NULL
    facet_args <- dropNullsOrEmpty(facet_args)

    if (length(facet_args) > 0) {
      facet <- paste0("facet_grid(vars(", facet_row, "), vars(", facet_col, "), facet_args)")
      ggcall <- paste0(ggcall, " + ", facet)
    } else {
      facet <- paste0("facet_grid(vars(", facet_row, "), vars(", facet_col, "))")
      ggcall <- paste0(ggcall, " + ", facet)
    }
  }

  # Load geoms
  if (length(geom) == 1)
    geom_args <- setNames(list(geom_args), geom)

  for (g in geom) {
    g_args <- dropNulls(geom_args[[g]])

    if (!grepl("^geom_", g)){
      g <- paste0("geom_", g)
    }

    if (grepl("geom_density_ridges", g)){
      g <- paste0("ggridges::", g)
    }

    if (grepl("geom_col", g) && !is.null(position)){
      g_args <- append(g_args, position)
    }

    if(length(g_args) != 0){
      g_args <- deparse(g_args)
      g_args <- gsub("^list", "", g_args)
      g_args <- gsub("alpha = 1L, ", "", g_args)

      if(g == "geom_point") {
        g_args <- gsub("size = 1.5", "", g_args)
        g_args <- gsub('shape = "circle"', "", g_args)
      }

      g_args <- gsub(", ,", ",", g_args)
      g_args <- gsub("\\(\\s+,\\s+\\)", "()", g_args)
      g_args <- gsub("\\s*,\\s*\\)", ")", g_args)
      g_args <- gsub("\\(\\s*,\\s*", "(", g_args)

      g <- paste0(g, g_args)
    } else {
      g <- paste0(g, "()")
    }

    ggcall <- paste0(ggcall, " + ", g)
  }

  # load scales
  if (!is.null(scales)) {
    if (length(scales) == 1 && !isTRUE(grepl(scales, names(scales_args))))
      scales_args <- setNames(list(scales_args), scales)
    for (s in scales) {
      s_args <- dropNulls(scales_args[[s]])
      if (grepl("::", x = s)) {
        scl <- strsplit(x = s, split = "::")[[1]]
        scl <- call2(scl[2], !!!s_args, .ns = scl[1])
      } else {
        # if (!grepl("^scale_", s))
        #   s <- paste0("scale_", s)
        s_args <- deparse(s_args)
        s_args <- gsub("^list", "", s_args)

        scl <- paste0(s, s_args)
      }
      ggcall <- paste0(ggcall, " + ", scl)
    }
  }

  # limits options
  if (has_length(xlim, 2)) {
    xlim <- as.list(xlim)
    ggcall <- paste0(ggcall, " + scale_x_continuous_e61(limits = c(", xlim[1], ",", xlim[2], "))")
  }
  if (has_length(ylim, 2)) {
    ylim <- as.list(ylim)
    ggcall <- paste0(ggcall, " + scale_y_continuous_e61(limits = c(", ylim[1], ",", ylim[2], "))")
  }

  # labels
  labs <- dropNullsOrEmpty(labs)
  if (length(labs) > 0) {

    if(!is.null(labs$color) || !is.null(labs$fill))
      show_legend <- T

    labs <- deparse(labs)

    if(length(labs) > 1){
      labs <- paste(labs, collapse = "")
      labs <- gsub("\\s+", " ", labs)
    }

    labs <- gsub("^list", "", labs)
    labs <- gsub("color", "colour", labs)
    # changing the two below manually because I made a mistake and I'm lazy
    labs <- gsub("footnote \\=", "footnotes \\=", labs)
    labs <- gsub("source \\=", "sources \\=", labs)

    labs <- paste0("labs_e61", labs)

    ggcall <- paste0(ggcall, " + ", labs)
  }

  # coordinates
  if (!is.null(coord)) {
    if (!grepl("^coord_", coord))
      coord <- paste0("coord_", coord, "()")

    ggcall <- paste0(ggcall, " + ", coord)
  }

  # theme - if it's not theme_e61
  if (!is.null(theme) & theme != "theme_e61") {
    ggcall <- paste0(ggcall, " + ", theme, "()")
  }

  if (!any(c("fill", "colour", "color", "size", "shape") %in% names(mapping))) {
    theme_args$legend.position <- NULL
  } else {
    theme_args$legend.title <- NULL
  }

  theme_args <- dropNullsOrEmpty(theme_args)
  e61_theme_args <- list()

  if (length(theme_args) > 0) {

    # check for e61 specific theme options
    for(i in seq_along(theme_args)){

      if(names(theme_args)[i] == "legend.position"){

        e61_theme_args[[length(e61_theme_args) + 1]] <- paste0("legend = '", theme_args[[i]], "'")

        theme_args[[i]] <- NULL
      }
    }

    # add a legend title if required
    if(!is.null(show_legend) && show_legend)
      e61_theme_args[[length(e61_theme_args) + 1]] <- "legend_title = T"

    # Add theme_e61 args
    if(length(e61_theme_args > 0)){
      e61_theme_args <- unlist(e61_theme_args)
      e61_theme_args <- paste(e61_theme_args, collapse = ", ")

      ggcall <- paste0(ggcall, " + theme_e61(", e61_theme_args, ")")
    }

    # Add the other theme args
    theme_args <- dropNullsOrEmpty(theme_args)

    # if there are still remaining arguments, then call them inside the regular theme
    if(length(theme_args) > 0){

      theme_args <- deparse(theme_args)
      theme_args <- gsub("^list", "", theme_args)
      theme_args <- paste0("theme", theme_args)

      ggcall <- paste0(ggcall, " + ", theme_args)
    }
  }

  # add e61 logo
  if(add_logo)
    ggcall <- paste0(ggcall, " + add_e61_logo()")

  ggcall
}

syms2 <- function(x) {
  lapply(
    X = x,
    FUN = function(y) {
      if (inherits(y, "AsIs")) {
        as.character(y)
      } else {
        sym(as_name(y))
      }
    }
  )
}
