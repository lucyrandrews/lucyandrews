#' A function that extracts the midpoints of sf line objects
#'
#' This function takes a collection of `sf LINESTRING` objects and returns their midpoints as `sf POINTS` objects.
#'
#' @param sf_lines An `sf` object with geometry type `LINESTRING` or `MULTILINESTRING`
#'
#' @return An `sf` object with geometry type `POINT` containing the midpoint(s) of the `LINESTRING` object(s)
#'
#' @examples
#' pts <- matrix(1:4, nrow = 2, ncol = 2, byrow = FALSE)
#' ls <- st_linestring(pts)
#' mid <- st_line_midpoints(ls)
#' ggplot() +
#'   geom_sf(data = ls, color = "black) +
#'   geom_sf(data = mid, color = "green", size = 5)
#'
#' @references
#' This function was heavily inspired by a
#' \href{https://gis.stackexchange.com/questions/277219/sf-equivalent-of-r-maptools-packages-spatiallinesmidpoints}{GIS Stack Exchange answer posted by user obrl_soil}.

st_line_midpoints <- function(sf_lines = NULL) {

  g <- st_geometry(sf_lines)

  g_mids <- lapply(g, function(x) {

    coords <- as.matrix(x)

    get_mids <- function (coords) {
      dist <- sqrt((diff(coords[ , 1])^2 + (diff(coords[ , 2]))^2))
      dist_mid <- sum(dist)/2
      dist_cum <- c(0, cumsum(dist))
      end_index <- which(dist_cum > dist_mid)[1]
      start_index <- end_index - 1
      start <- coords[start_index, ]
      end <- coords[end_index, ]
      dist_remaining <- dist_mid - dist_cum[start_index]
      mid <- start + (end - start) * (dist_remaining/dist[start_index])
      return(mid)
    }

    mids <- st_point(get_mids(coords))

  })

  geometry <- st_sfc(g_mids, crs = st_crs(sf_lines))
  geometry <- st_sf(geometry)

}
