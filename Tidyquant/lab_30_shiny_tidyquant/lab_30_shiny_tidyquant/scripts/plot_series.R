# PLOTTING UTILITY ----

require(tidyverse)
require(tidyquant)
plot_series <- function(data, x, y, color, ncol = 1, scales = "fixed") {
    data %>%
        ggplot(aes(!! enquo(x), !! enquo(y), color = !! enquo(color))) +
        geom_line() +
        scale_color_tq() +
        theme_tq() +
        facet_wrap(facets = vars(!! enquo(color)), ncol = ncol, scales = scales) +
        theme(legend.position = "none")
}