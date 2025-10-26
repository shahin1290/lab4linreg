#' Visualize Mean Airport Delays
#'
#' Creates a scatter plot showing the mean departure delay for different airports
#' across the United States, using longitude and latitude from the `airports` dataset.
#'
#' @details
#' This function uses data from the `nycflights13` package, combines the `flights`
#' and `airports` datasets, and visualizes average delays with `ggplot2`.
#'
#' @return A ggplot object showing mean delays by airport location.
#'
#' @examples
#' \dontrun{
#' visualize_airport_delays()
#' }
#'
#' @importFrom dplyr select mutate summarise %>%
#' @importFrom ggplot2 ggplot aes geom_point
#' @export
visualize_airport_delays <- function() {
  delay_summary <- nycflights13::flights %>%
    dplyr::group_by(.data$dest) %>%
    dplyr::summarise(mean_delay = mean(.data$dep_delay, na.rm = TRUE)) %>%
    dplyr::inner_join(
      nycflights13::airports %>%
        dplyr::select(.data$faa, .data$name, .data$lat, .data$lon),
      by = c("dest" = "faa")
    )

  ggplot2::ggplot(delay_summary, ggplot2::aes(x = .data$lon, y = .data$lat, color = .data$mean_delay)) +
    ggplot2::geom_point(size = 3, alpha = 0.8) +
    ggplot2::labs(
      title = "Average Flight Departure Delays by Airport (2013)",
      x = "Longitude", y = "Latitude", color = "Mean Delay (min)"
    ) +
    ggplot2::theme_minimal()
}
