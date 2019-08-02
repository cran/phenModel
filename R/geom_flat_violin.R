geom_flat_violin <-
function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  
  group <- x <- y <- width <- violinwidth <- xmin <- xminv <- xmaxv <- NULL
  
  GeomFlatViolin <-
    ggproto("GeomFlatViolin", Geom,
            setup_data = function(data, params) {
              
              "%||%" <- function(a, b) {
                if (!is.null(a)) a else b
              }
              
              data$width <- data$width %||% 
                params$width %||% (resolution(data$x, FALSE) * 0.9)
              
              # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
              data %>%
                dplyr::group_by(group) %>%
                dplyr::mutate(ymin = min(y),
                              ymax = max(y),
                              xmin = x - width / 2,
                              xmax = x)
            },
            
            draw_group = function(data, panel_scales, coord) {
              # Find the points for the line to go all the way around
              data <- transform(data, 
                                xmaxv = x,
                                xminv = x - violinwidth * (xmin - x))
              
              # Make sure it's sorted properly to draw the outline
              newdata <- rbind(dplyr::arrange(transform(data, x = xminv), y),
                               dplyr::arrange(transform(data, x = xmaxv), -y))
              
              # Close the polygon: set first and last point the same
              # Needed for coord_polar and such
              newdata <- rbind(newdata, newdata[1,])
              
              ggname <- function(prefix, grob) {
                grob$name <- grid::grobName(grob, prefix)
                grob
              }
              ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
            },
            
            draw_key = draw_key_polygon,
            
            default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                              alpha = NA, linetype = "solid"),
            
            required_aes = c("x", "y")
    )
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}
