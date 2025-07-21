#' Plot the sum of two complex numbers
#'
#' This function visualizes the addition of two complex numbers `z` and `w`
#' as vectors in the complex plane. It plots the individual vectors and their
#' resulting sum `z + w`, illustrating complex addition geometrically.
#'
#' @param z A complex number.
#' @param w A complex number.
#'
#' @return An animated `ggplot` object showing the addition of the two complex numbers as vectors in the complex plane
#'
#' @examples
#' z <- 1 + 1i
#' w <- 2 - 1i
#' plot_complex_addition(z, w)
#'
#' @import ggplot2 gganimate
#' @importFrom grid unit
#'
#' @export

# Create function to visualize the addition of two complex numbers
plot_complex_addition <- function(z, w) {
  # Create a data frame for the addition of complex numbers
  # z, w (starting at z) and z + w
  complex_n <- data.frame(
    x     = c(0,        Re(z),    0),
    y     = c(0,        Im(z),    0),
    xend  = c(Re(z),    Re(z + w), Re(z + w)),
    yend  = c(Im(z),    Im(z + w), Im(z + w)),
    label = c("z",      "w",       "z + w"),
    step  = c(1,        2,         3)
  )

  # Create an animated plot for the addition
  addition_c <- ggplot(complex_n) +
    # Draw arrow segments with labels and colors
    geom_segment(
      aes(x = x, y = y, xend = xend, yend = yend, color = label),
      arrow = arrow(length = grid::unit(0.3, "cm")),
      size = 1.2
    ) +
    # Keep x and y scales equal so vectors look right (no stretching or squishing)
    coord_fixed() +
    theme_classic() +
    labs(
      title = "Animated Complex Addition",
      x = "Real axis", y = "Imaginary axis",
      color = ""
    ) +
    theme(
      axis.title.x = element_text(size = 13, vjust = -0.5),
      axis.title.y = element_text(size = 13, vjust = 2),
      plot.title   = element_text(hjust = 0.5, face = "bold")
    ) +
    transition_states(step, transition_length = 2, state_length = 1) +
    enter_fade() +
    exit_fade() +
    shadow_mark(past = TRUE, future = FALSE, keep = FALSE)

  # Render and return the animation
  animate(addition_c, nframes = 35, fps = 10)
}
