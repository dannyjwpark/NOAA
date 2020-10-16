#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#' @name geom_timeline
#'
#' @title Module 2: Visualization tools: geom_timeline()
#'
#' @description Building geom_timeline()
#'a geom for ggplot2 for plotting a timeline of earthquakes (xmin to xmax of dates) for each earthquake
#' points (in the example) represent earthquake events, point size indicating
#' earthquake magnitude and colour represent number of deaths.
#' xaes = the date (required), yaes = country (optional)
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  earthquake_data <- earthquake_data %>% filter(COUNTRY %in% c("CHINA", "USA"), YEAR > 2000)
#'  ggplot(earthquake_data, aes(x = date, y = COUNTRY,
#'                 color = as.numeric(TOTAL_DEATHS),
#'                 size = as.numeric(EQ_PRIMARY),
#'                 label = CLEAN_LOCATION_NAME)) +
#'    geom_timeline() +
#'    labs(size = "Richter scale value", color = "# deaths") +
#'    ggplot2::theme(panel.background = ggplot2::element_blank(),
#'          legend.position = "bottom",
#'          axis.title.y = ggplot2::element_blank()) +
#'    ggplot2::xlab("DATE")
#' }
#'
library(ggplot2)
library(grid)
library(dplyr)
#' Title
#'
#' @param mapping
#' @param data
#' @param stat
#' @param position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
geom_timeline <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity", na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE, ...) {

  ggplot2::layer(
    geom = GeomTimeline,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#' @name geom_timeline_label
#'
#' @title Visualization tool: geom_timeline_label
#'
#' @description Scripting geom_timeline_label function
#' geom function aims to take nmax value of earthquakes as vertical line label
#'
#' @param n_max An integer (default = 5) max # of earthquakes
#'
#' @import ggplot2, dplyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot(earthquake_data, aes(x = Date, y = COUNTRY,
#'                color = as.numeric(TOTAL_DEATHS),
#'                size = as.numeric(EQ_PRIMARY),
#'                label = CLEAN_LOCATION_NAME)) +
#'   geom_timeline() +
#'   labs(size = "Richter scale value", color = "# deaths") +
#'   ggplot2::theme(panel.background = ggplot2::element_blank(),
#'         legend.position = "bottom",
#'         axis.title.y = ggplot2::element_blank()) + ggplot2::xlab("DATE") +
#'   geom_timeline_label(data=earthquake_data)
#' }

geom_timeline_label <- function(mapping = NULL, data = NULL,
                                stat = "identity", position = "identity", na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE, n_max = 5, ...) {

  # here we alter the number of earthquakes we apply a label to as n_max
  data <- data %>% dplyr::mutate(COUNTRY = as.character(COUNTRY), EQ_PRIMARY = as.numeric(EQ_PRIMARY)) %>%
    dplyr::arrange(COUNTRY, desc(EQ_PRIMARY))

  countries <- unique(data$COUNTRY)
  earthquake_data_all <- data.frame()
  for(country in countries){
    earthquake_data <- data %>% dplyr::filter(COUNTRY == country) %>% head(n_max)
    earthquake_data_all <- rbind(earthquake_data_all, earthquake_data)
  }
  data <- earthquake_data_all
  #print(data)

  ggplot2::layer(
    geom = GeomTimelineLabel,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------

#' @name GeomTimeline
#'
#' @title Defines Our GeomTimeline class
#'
#' @description As per the course materials:
#' The ggproto() function is used to construct a new class corresponding to your new geom.
#' The geom_* function is constructed as a regular function.
#' Build a geom for ggplot2 called geom_timeline() for plotting a time line of
#' earthquakes ranging from xmin to xmaxdates with a point for each earthquake.
#' Optional aesthetics include color, size, and alpha (for transparency).
#' The xaesthetic is a date and an optional y aesthetic is a factor indicating
#' some stratification in which case multiple time lines will be plotted for
#' each level of the factor (e.g. country).
#' This geom looks to chart a timeline of earthquakes for a given country / countries
#' with points (in the example) representing earthquake events, point size indicating
#' earthquake magnitude and colour representing number of deaths. x (the date) is a
#' required aesthetic whereas y (country) is optional.
#'
#' @import ggplot2, grid, scales
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  earthquake_data <- earthquake_data_earthquakes %>% filter(COUNTRY %in% c("CHINA", "USA"), YEAR > 2000)
#'  ggplot(earthquake_data, aes(x = date, y = COUNTRY,
#'                 color = as.numeric(TOTAL_DEATHS),
#'                 size = as.numeric(EQ_PRIMARY),
#'                 label = CLEAN_LOCATION_NAME)) +
#'    geom_timeline() +
#'    labs(size = "Richter scale value", color = "# deaths") +
#'    ggplot2::theme(panel.background = ggplot2::element_blank(),
#'          legend.position = "bottom",
#'          axis.title.y = ggplot2::element_blank()) +
#'    ggplot2::xlab("DATE")
#' }
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                  required_aes = c("x"), # optional y aesthetic
                  default_aes = ggplot2::aes(y=1, alpha=0.7, fill="grey", colour="grey", size=1, shape=21, stroke=1),
                  draw_key = ggplot2::draw_key_point,

                  # we'll need points across a line for each level / country
                   draw_group = function(data, panel_scales, coord) {
                  #print(head(data))
                   coords <- coord$transform(data, panel_scales)

                   #?grid::pointsGrob
                   # pch -> numeric or character vector indicating what sort of plotting symbol to use
                   # points for more details: pch = 21 is a filled circle. See page 259 of course materials
                   points <- grid::pointsGrob(coords$x, coords$y,
                      pch = coords$shape,
                      size = grid::unit(coords$size / 6, "lines"),        # see ?grid::unit
                      gp = gpar(col = alpha(coords$colour, coords$alpha), # see ?scales::alpha
                      fill = alpha(coords$colour, coords$alpha)
                          )
                       )
                    #?grid::segmentsGrob
                      line <- grid::segmentsGrob(
                        x0 = 0, y0 = coords$y,
                        x1 = 1, y1 = coords$y,
                        gp = gpar(col = "grey", alpha=0.7, size=1)
                       )
                      grid::gList(points, line)
                     }
)
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#' @name GeomTimelineLabel
#'
#' @title Defining GeomTimelineLabel class
#'
#' @description
#' The geom_* function is constructed as a regular function.
#' Build a geom called geom_timeline_label() for adding annotations to the earthquake data.
#' This geom adds a vertical line to each data point with a text annotation
#' (e.g. the location of the earthquake) attached to each line.
#' There should be an option to subset to n_max number of earthquakes, where we take the
#' n_max largest (by magnitude) earthquakes. Aesthetics are x, which is the date of the
#' earthquake and label which takes the column name from which annotations will be obtained.
#' Note that you should run geom_timeline first such that we have our points to annotate.
#'
#' @import ggplot2, grid
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot(earthquake_data, aes(x = date, y = COUNTRY,
#'                color = as.numeric(TOTAL_DEATHS),
#'                size = as.numeric(EQ_PRIMARY),
#'                label = CLEAN_LOCATION_NAME)) +
#'   geom_timeline() +
#'   labs(size = "Richter scale value", color = "# deaths") +
#'   ggplot2::theme(panel.background = ggplot2::element_blank(),
#'         legend.position = "bottom",
#'         axis.title.y = ggplot2::element_blank()) + ggplot2::xlab("DATE") +
#'   geom_timeline_label(data=earthquake_data)
#' }
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
  required_aes = c("x", "label"),
  default_aes = ggplot2::aes(y=1, alpha=0.7, fill="grey", colour="grey"),
  draw_key = ggplot2::draw_key_label,

  # we can already get the points and horizontal line using geom_timeline
  # here we look to add a vertical line to a label
  draw_group = function(data, panel_scales, coord) {
    print(head(data))
    coords <- coord$transform(data, panel_scales)

    y_extension <- 0.05
    line <- grid::segmentsGrob(# get the vertical line
      x0 = coords$x, y0 = coords$y,
      x1 = coords$x, y1 = coords$y + y_extension,
      gp = grid::gpar(col = "grey", alpha=0.7, size=1)
    )

text <- grid::textGrob(# ?grid::textGrob
    label=coords$label,
    x = coords$x,
    y = coords$y + y_extension,
    rot = 45,
    just = c("left", "bottom")
  )

grid::gList(line, text)
  }
)

#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#' @name plot_earthquakes_timeline
#'
#' @title Plot the earthquake timeline and optionally save
#'
#' @description Given a subset of earthquake data (earthquake_data) this will plot the
#' data as a timeline (without label annotations) and apply formatting.
#' The resulting image will then be saved as earthquakes_timeline.png if
#' the parameter save_png is TRUE.
#'
#' @param earthquake_data A dataframe of the earthquake data your wish to plot
#' @param save_png Boolean, default is FALSE, of whether to save as a png file
#'
#' @return NULL
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' \dontrun{
#' earthquake_data <- earthquake_data %>%
#'   filter(COUNTRY %in% c("CHINA", "USA"), YEAR > 2000)
#' plot_earthquakes_timeline(earthquake_data, save_png=TRUE)
#' }
plot_earthquakes_timeline <- function(earthquake_data, save_png=FALSE){
  ggplot(earthquake_data, aes(x = date, y = COUNTRY,
    color = as.numeric(TOTAL_DEATHS),
    size = as.numeric(EQ_PRIMARY))) +
    geom_timeline() +
    labs(size = "Richter scale value", color = "# deaths") +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
        legend.position = "bottom",
        axis.title.y = ggplot2::element_blank()) +
    ggplot2::xlab("DATE")

  if(save_png){ggsave("earthquakes_timeline.png")}
}

#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#' @name plot_earthquakes_timeline_label
#'
#' @title Plot the earthquake timeline with label annotations and optionally save
#'
#' @description Given a subset of earthquake data (earthquake_data) this will plot the
#' data as a timeline (with label annotations) and apply formatting.
#' The resulting image will then be saved as earthquakes_timeline_label.png if
#' the parameter save_png is TRUE.
#'
#' @param earthquake_data A dataframe of the earthquake data your wish to plot
#' @param save_png Boolean, default is FALSE, of whether to save as a png file
#'
#' @return NULL
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' \dontrun{
#' earthquake_data <- earthquake_data %>%
#'   filter(COUNTRY %in% c("CHINA", "USA"), YEAR > 2000)
#' plot_earthquakes_timeline_label(earthquake_data, save_png=TRUE)
#' }
plot_earthquakes_timeline_label <- function(earthquake_data, save_png=FALSE){
  ggplot(earthquake_data, aes(x = date, y = COUNTRY,
     color = as.numeric(TOTAL_DEATHS),
     size = as.numeric(EQ_PRIMARY),
     label = CLEAN_LOCATION_NAME)) +
     geom_timeline() +
     labs(size = "Richter scale value", color = "# deaths") +
     ggplot2::theme(panel.background = ggplot2::element_blank(),
                   legend.position = "bottom",
                   axis.title.y = ggplot2::element_blank()) + ggplot2::xlab("DATE") +
     geom_timeline_label(data=earthquake_data)

  if(save_png){ggsave("earthquakes_timeline_label.png")}
}

#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------

#print(sort(unique(earthquake_data$COUNTRY)))
#earthquake_data <- earthquake_data %>%
#  filter(COUNTRY %in% c("CHINA", "USA"), YEAR > 2000)
#plot_earthquakes_timeline(df, save_png=TRUE)
#plot_earthquakes_timeline_label(df, save_png=TRUE)
