knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  cache = FALSE,
  eval = TRUE,
  cache.lazy = FALSE,
  df_print = "paged",
  dpi = 72,
  tidy = "styler",
  dev = "ragg_png",
  autodep = TRUE,
  out.width = '200%',
  fig.align = 'center',
  fig.width = 9,
  fig.asp = 0.618      #,   1 / phi   use 1 for facet_wrap
#  class.output = "scroll-100"   # must include the CSS, doesn't style on github pages
)

knitr::opts_template$set(
  fig.large = list(fig.asp = 0.8), #
  fig.square = list(fig.asp = 1),
  fig.long = list(fig.asp = 1.5)
)

alpha_viridis <- function(...) {
  scale_fill_gradientn(..., colors = viridis::viridis(256, option = 'H'))
}

color_index <- c(12,8,4,11,7,3,10,6,2,9,5,1)

options(
  ggplot2.discrete.fill = viridis::viridis_pal(option = "H")(12)[color_index],
  ggplot2.discrete.colour = viridis::viridis_pal(option = "H")(12)[color_index],
  ggplot2.continuous.fill = alpha_viridis,
  ggplot2.continuous.colour = alpha_viridis,
  hrbrthemes.loadfonts = TRUE
)

# Package Rttf2pt1 version 1.3.9 interferes with
# font loading on windows
# https://github.com/wch/extrafont/issues/32
#
#

# ----
#  nearly identical to hrbrthemes::theme_ipsum_tw()
theme_jim <- function(base_size = 12,
                      base_family = "Titillium Web",
                      plot_title_family=if (.Platform$OS.type == "windows") "Titillium Web" else "Titillium Web Bold",
                      plot_title_size = 18,
                      plot_title_face="bold",
                      plot_title_margin = 10,
                      subtitle_family=if (.Platform$OS.type == "windows") "Titillium Web" else "Titillium Web Light",
                      subtitle_size = 13,
                      subtitle_face = "plain",
                      subtitle_margin = 15,
                      strip_text_family = base_family,
                      strip_text_size = 12,
                      strip_text_face = "plain",
                      caption_family=if (.Platform$OS.type == "windows") "Titillium Web" else "Titillium Web Light",
                      caption_size = 9,
                      caption_face = "plain",
                      caption_margin = 10,
                      axis_text_size = base_size,
                      axis_title_family = base_family,
                      axis_title_size = rel(0.75),
                      axis_title_face = "plain",
                      axis_title_just = "rt",
                      plot_margin = margin(30, 30, 30, 30),
                      grid_col = "#cccccc", grid = TRUE,
                      axis_col = "#cccccc", axis = FALSE, ticks = FALSE
                      ){


  ret <-
    ggplot2::theme_minimal(base_family = base_family,
                           base_size = base_size)

  ret <- ret + theme(legend.background = element_blank())
  ret <- ret + theme(legend.key = element_blank())

  if (inherits(grid, "character") | grid == TRUE) {
    ret <-
      ret + theme(panel.grid = element_line(color = grid_col,
                                            size = 0.2))
    ret <-
      ret + theme(panel.grid.major = element_line(color = grid_col,
                                                  size = 0.2))
    ret <-
      ret + theme(panel.grid.minor = element_line(color = grid_col, size = 0.15))

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0)
        ret <- ret + theme(panel.grid.major.x = element_blank())
      if (regexpr("Y", grid)[1] < 0)
        ret <- ret + theme(panel.grid.major.y = element_blank())
      if (regexpr("x", grid)[1] < 0)
        ret <- ret + theme(panel.grid.minor.x = element_blank())
      if (regexpr("y", grid)[1] < 0)
        ret <- ret + theme(panel.grid.minor.y = element_blank())
    }

  } else {
    ret <- ret + theme(panel.grid = element_blank())
  }

  if (inherits(axis, "character") | axis == TRUE) {
    ret <-
      ret + theme(axis.line = element_line(color = axis_col, size = 0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x = element_blank())
      } else {
        ret <-
          ret + theme(axis.line.x = element_line(color = axis_col, size = 0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y = element_blank())
      } else {
        ret <-
          ret + theme(axis.line.y = element_line(color = axis_col, size = 0.15))
      }
    } else {
      ret <-
        ret + theme(axis.line.x = element_line(color = axis_col, size = 0.15))
      ret <-
        ret + theme(axis.line.y = element_line(color = axis_col, size = 0.15))
    }
  } else {
    ret <- ret + theme(axis.line = element_blank())
  }

  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
    ret <- ret + theme(axis.ticks.x = element_blank())
    ret <- ret + theme(axis.ticks.y = element_blank())
  } else {
    ret <- ret + theme(axis.ticks = element_line(size = 0.15))
    ret <- ret + theme(axis.ticks.x = element_line(size = 0.15))
    ret <- ret + theme(axis.ticks.y = element_line(size = 0.15))
    ret <- ret + theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  xj <-
    switch(
      tolower(substr(axis_title_just, 1, 1)),
      b = 0,
      l = 0,
      m = 0.5,
      c = 0.5,
      r = 1,
      t = 1
    )
  yj <-
    switch(
      tolower(substr(axis_title_just, 2, 2)),
      b = 0,
      l = 0,
      m = 0.5,
      c = 0.5,
      r = 1,
      t = 1
    )

  ret <-
    ret +
    theme(axis.text.x = element_text(size = axis_text_size,
                                     margin = margin(t = 0)))

  ret <-
    ret +
    theme(axis.text.y = element_text(size = axis_text_size,
                                     margin = margin(r = 0)))

  ret <-
    ret +
    theme(axis.title = element_text(size = axis_title_size,
                                    family = axis_title_family))
  ret <-
    ret +
    theme(
      axis.title.x = element_text(
        hjust = xj,
        size = axis_title_size,
        family = axis_title_family,
        face = axis_title_face
      )
    )

  ret <-
    ret + theme(
      axis.title.y = element_text(
        hjust = yj,
        size = axis_title_size,
        family = axis_title_family,
        face = axis_title_face
      )
    )

  ret <-
    ret +
    theme(
      axis.title.y.right = element_text(
        hjust = yj,
        size = axis_title_size,
        angle = 90,
        family = axis_title_family,
        face = axis_title_face
      )
    )

  ret <-
    ret +
    theme(
      strip.text = element_text(
        hjust = 0,
        size = strip_text_size,
        face = strip_text_face,
        family = strip_text_family
      )
    )

  ret <- ret +
    theme(panel.spacing = grid::unit(2, "lines"))

  ret <-
    ret +
    theme(
      plot.title = element_text(
        hjust = 0,
        size = plot_title_size,
        margin = margin(b = plot_title_margin),
        family = plot_title_family,
        face = plot_title_face
      ),
      plot.title.position = "plot"
    )

  ret <-
    ret +
    theme(
      plot.subtitle = element_text(
        hjust = 0,
        size = subtitle_size,
        margin = margin(b = subtitle_margin),
        family = subtitle_family,
        face = subtitle_face
      )
    )

  ret <-
    ret + theme(
      plot.caption = element_text(
        hjust = 1,
        size = caption_size,
        margin = margin(t = caption_margin),
        family = caption_family,
        face = caption_face
      ),
      plot.caption.position = "plot"
    )

  ret <- ret + theme(plot.margin = plot_margin)

  ret

}

autoplot.conf_mat <- function(object, type = "heatmap", ...) {
  cm_heat(object)
}

# Yardstick----

cm_heat <- function(x) {
  `%+%` <- ggplot2::`%+%`

  table <- x$table

  df <- as.data.frame.table(table)

  # Force known column names, assuming that predictions are on the
  # left hand side of the table (#157).
  names(df) <- c("Prediction", "Truth", "Freq")

  # Have prediction levels going from high to low so they plot in an
  # order that matches the LHS of the confusion matrix
  lvls <- levels(df$Prediction)
  df$Prediction <- factor(df$Prediction, levels = rev(lvls))
  axis_labels <- yardstick:::get_axis_labels(x)

  df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = Truth,
        y = Prediction,
        fill = Freq
      )
    ) %+%
    ggplot2::geom_tile(show.legend = FALSE) %+%
    ggplot2::scale_fill_viridis_c(option = "H") %+%
    ggplot2::scale_x_discrete(guide = guide_axis(n.dodge = 2)) %+%
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      legend.position = "none"
    ) %+%
    ggplot2::geom_text(mapping = ggplot2::aes(label = Freq,
                                              color = after_scale(prismatic::clr_desaturate(prismatic::clr_negate(fill), 0.5))),
                       size = rel(5)) %+%
    ggplot2::labs(x = axis_labels$x, y = axis_labels$y)
}

# VariableImportance----
# https://www.tmwr.org/explain.html from Tidy Modeling With R

ggplot_imp <- function(...) {
  obj <- list(...)
  metric_name <- attr(obj[[1]], "loss_name")
  metric_lab <- paste(metric_name,
                      "after permutations\n(higher indicates more important)")

  full_vip <- dplyr::bind_rows(obj) %>%
    dplyr::filter(variable != "_baseline_")

  perm_vals <- full_vip %>%
    dplyr::filter(variable == "_full_model_") %>%
    dplyr::group_by(label) %>%
    summarise(dropout_loss = mean(dropout_loss))

  p <- full_vip %>%
    dplyr::filter(variable != "_full_model_") %>%
    dplyr::mutate(variable = fct_reorder(variable, dropout_loss)) %>%
    ggplot2::ggplot(ggplot2::aes(dropout_loss, variable))
  if(length(obj) > 1) {
    p <- p +
      ggplot2::facet_wrap(vars(label)) +
      ggplot2::geom_vline(data = perm_vals, ggplot2::aes(xintercept = dropout_loss, color = label),
                 size = 1.4, lty = 2, alpha = 0.7) +
      ggplot2::geom_boxplot(ggplot2::aes(color = label, fill = label), alpha = 0.2)
  } else {
    p <- p +
      ggplot2::geom_vline(data = perm_vals, ggplot2::aes(xintercept = dropout_loss),
                 size = 1.4, lty = 2, alpha = 0.7) +
      ggplot2::geom_boxplot(fill = "#91CBD765", alpha = 0.4)

  }
  p +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = metric_lab,
         y = NULL,  fill = NULL,  color = NULL)
}

# ggplot categorical count helper function ----

withfreq <- function(x, width = 20){
  tibble(x) %>%
    add_count(x) %>%
    mutate(combined = glue::glue("{ str_wrap(x, width = width) } ({ n })")) %>%
    pull(combined)
}

# ----

tidymodels::tidymodels_prefer(quiet = TRUE)
conflicted::conflict_prefer("vi", "vip", quiet = TRUE)
conflicted::conflict_prefer("explain", "lime", quiet = TRUE)
conflicted::conflict_prefer("select", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("filter", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("spec", "yardstick", quiet = TRUE)
conflicted::conflict_prefer("lag", "dplyr", quiet = TRUE)

