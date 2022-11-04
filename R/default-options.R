

# Palettes ----------------------------------------------------------------

#' @importFrom scales hue_pal viridis_pal brewer_pal
default_pals <- function() {
  pals <- list(
    choices = list(
      e61 = list(
        "base" = theme61::e61_palette(10)
      )
    ), 
    textColor = c(
      rep(c("white", "black"), times = c(23, 18))
    )
  )
  return(pals)
}


# Get list of palettes
get_palettes <- function() {
  pals <- getOption("esquisse.palettes")
  if (is.null(pals))
    pals <- default_pals()
  if (is.function(pals))
    pals <- pals()
  if (!is.list(pals)) {
    stop("Option 'esquisse.palettes' must be a list with at least one slot : 'choices'", call. = FALSE)
  }
  if (is.null(pals$textColor)){
    pals$textColor <- "white"
  }
  
  pals
}



# Themes ------------------------------------------------------------------


#' @importFrom ggplot2 theme_bw theme_classic theme_dark theme_gray theme_grey 
#'  theme_light theme_linedraw theme_minimal theme_void
default_themes <- function() {
  
  themes <- list()
  
  if (requireNamespace("theme61", quietly = TRUE)) {
    e61_themes <- c("e61")
    e61_themes <- setNames(as.list(paste0("theme61::theme_", e61_themes)), e61_themes)
  }
  
  themes$e61_themes <- e61_themes
  
  return(themes)
}

check_theme_exist <- function(x, package = "ggplot2") {
  vapply(X = x, FUN = function(fun) {
    if (grepl(pattern = "::", x = fun)) {
      x <- strsplit(x = fun, split = "::")[[1]]
      fun <- x[2]
      package <- x[1]
      exists(fun, where = asNamespace(package), mode = "function")
    } else {
      if (!startsWith(fun, "theme_"))
        fun <- paste0("theme_", fun)
      exists(fun, where = asNamespace(package), mode = "function")
    }
  }, FUN.VALUE = logical(1), USE.NAMES = FALSE)
}


# Get list of themes
get_themes <- function() {
  themes <- getOption("esquisse.themes")
  if (is.null(themes))
    themes <- default_themes()
  if (is.function(themes))
    themes <- themes()
  if (!is.list(themes)) {
    stop("Option 'esquisse.themes' must be a list", call. = FALSE)
  }
  themes <- rapply(
    object = themes, 
    f = function(x) {
      if (all(check_theme_exist(x))) {
        x
      } else {
        warning(paste("Theme", x, "not found!"), call. = FALSE)
        NULL
      }
    }, how = "list"
  )
  dropNullsOrEmptyRecursive(themes)
}



# Colors ------------------------------------------------------------------

#' @importFrom scales viridis_pal brewer_pal
default_cols <- function() {
  cols <- list(
    "custom" = c("#0C4C8A", "#EF562D", "forestgreen", "steelblue", "firebrick", "darkorange", "hotpink"),
    "viridis" = viridis_pal(option = "viridis")(8),
    "plasma" = viridis_pal(option = "plasma")(8),
    "Blues" = brewer_pal(palette = "Blues")(9)[-1],
    "Greens" = brewer_pal(palette = "Greens")(9)[-1],
    "Reds" = brewer_pal(palette = "Reds")(9)[-1],
    "Greys" = brewer_pal(palette = "Greys")(9)[-1]
  )
  return(cols)
}

# Get list of colors (spectrum)
get_colors <- function() {
  cols <- getOption("esquisse.colors")
  if (is.null(cols))
    cols <- default_cols()
  if (is.function(cols))
    cols <- cols()
  # if (!is.character(cols)) {
  #   stop("Option 'esquisse.colors' must be a character vector", call. = FALSE)
  # }
  cols
}
