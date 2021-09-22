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

hrbrthemes::update_geom_font_defaults(hrbrthemes::font_rc_light)

theme_jim <- function(base_size = 12){
   hrbrthemes::theme_ipsum_tw(base_size = base_size) %+replace%
      theme(plot.title.position = "plot",
            plot.caption.position = "plot")
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
                       size = rel(15),
                       face = "bold") %+%
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

withfreq <- function(x){
  tibble(x) %>%
    add_count(x) %>%
    mutate(combined = glue::glue("{ str_wrap(x, width = 20) } ({ n })")) %>%
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
