#' Plot the n-th roots of unity on the unit circle
#'
#' This function visualizes the n-th complex roots of unity by placing them
#' on the unit circle in the complex plane. The roots are connected in order
#' to form a regular n-gon.
#'
#' @param n An integer (n ≥ 2). The number of complex roots of unity to display.
#'
#' @return A static `ggplot` object showing the roots and the corresponding polygon.
#'
#' @examples
#' n <- 16
#' plot_unit_roots(n)
#'
#' @import ggplot2
#' @export

# Create a function to plot the n-th roots of unity as points on the unit circle
plot_unit_roots <- function(n) {
  if (n < 2) stop("n must be at least 2")

  # Compute the n roots of unity using polar coordinates
  angles <- 2 * pi * (0:(n - 1)) / n
  roots <- complex(modulus = 1, argument = angles)

  # Create a data frame with the real and imaginary parts
  df <- data.frame(
    x     = Re(roots),
    y     = Im(roots),
    label = paste0("ω^", 0:(n - 1)),
    id    = 1:n
  )

  # Duplicate the first point at the end to close the polygon
  df_poly <- rbind(df, df[1, ])

  # Create the plot
  ggplot(df, aes(x = x, y = y)) +
    geom_path(data = df_poly, aes(group = 1), color = "lightgreen", size = 1.5) +
    geom_point(color = "darkgreen", size = 4) +
    geom_text(aes(label = label), vjust = -1, size = 4) +
    coord_fixed() +
    ylim(min(df$y) - 0.1, max(df$y) + 0.3) +
    xlim(NA, max(df$x) + 0.1) +
    theme_classic() +
    labs(
      title = paste("n-th Roots of Unity (n =", n, ")"),
      x = "Real axis",
      y = "Imaginary axis"
    ) +
    theme(
      axis.title.x = element_text(size = 13, vjust = -0.5),
      axis.title.y = element_text(size = 13, vjust = 2),
      plot.title   = element_text(hjust = 0.5, face = "bold")
    )
}
