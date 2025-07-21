#' Plot the product of two complex numbers
#'
#' This function visualizes the multiplication of two complex numbers `z` and `w`
#' as vectors in the complex plane. It plots the individual vectors and their
#' resulting product `z * w`, illustrating complex multiplication geometrically.
#' In particular, the angle (argument) is animated to show how complex multiplication
#' affects both magnitude and direction.
#'
#' @param z A complex number.
#' @param w A complex number.
#'
#' @return An animated `ggplot` object showing the multiplication of the two complex numbers as vectors in the complex plane
#'
#' @examples
#' z <- 2 + 1i
#' w <- 2 - 2i
#' plot_complex_multiplication(z, w)
#'
#' @import ggplot2 gganimate
#' @importFrom grid unit
#'
#' @export

# Create function to visualize the multiplication of two complex numbers
plot_complex_multiplication <- function(z, w, steps = 50) {

  # Compute the lengths of z and w
  r_z <- Mod(z)
  r_w <- Mod(w)

  # Compute arguments (angles) of z and w in [0, 2π)
  arg_z <- Arg(z) %% (2 * pi)
  arg_w <- Arg(w) %% (2 * pi)
  arg_zw <- (arg_z + arg_w) %% (2 * pi) # Final angle after multiplication

  # Create interpolated angles from 0 to final angle, for animation of rotation
  if (arg_zw >= 0) {
    interp_angles <- seq(0, arg_zw, length.out = steps) # Step from z to the argument of z * w in 'steps' frames
  } else {
    interp_angles <- seq(0, arg_zw + 2 * pi, length.out = steps)
    interp_angles <- interp_angles %% (2 * pi)
  }

  interp_lengths <- seq(0, r_z * r_w, length.out = steps)

  # Create animated segments for the product vector z * w
  interp_points <- data.frame(
    x = 0,
    y = 0,
    xend = interp_lengths * cos(interp_angles),
    yend = interp_lengths * sin(interp_angles),
    step = 1:steps
  )

  # Create a rotating arc to illustrate argument addition
  arc_radius <- 0.1 * r_z * r_w # Adjusted radius for visual clarity
  arc_list <- lapply(1:steps, function(s) {
    angle_seq <- seq(0, interp_angles[s], length.out = 100) %% (2 * pi)
    data.frame(
      x = arc_radius * cos(angle_seq),
      y = arc_radius * sin(angle_seq),
      step = s
    )
  })
  arc <- do.call(rbind, arc_list)

  # Create static segments for z and w
  static_vectors <- data.frame(
    x = 0,
    y = 0,
    xend = c(Re(z), Re(w)),
    yend = c(Im(z), Im(w)),
    label = c("z", "w")
  )

  # Determine plot limits
  max_y <- max(Im(z), Im(w), Im(z * w)) + 0.5
  min_x <- min(Re(z), Re(w), Re(z * w)) - 0.5

  # Create the animated ggplot
  p <- ggplot() +
    geom_segment(data = static_vectors,
                 aes(x = x, y = y, xend = xend, yend = yend, color = label),
                 arrow = arrow(length = unit(0.3, "cm")),
                 size = 1.1) +
    geom_text(data = static_vectors,
              aes(x = xend, y = yend, label = label),
              vjust = -1.2, size = 5) +
    geom_segment(data = interp_points,
                 aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.3, "cm")),
                 size = 1.4, color = "purple") +
    geom_text(data = interp_points[steps, ],
              aes(x = xend, y = yend, label = "z * w"),
              vjust = -1.2, size = 5, color = "purple") +
    # Animated arc showing the angle change
    geom_path(data = arc,
              aes(x = x, y = y, group = step),
              color = "purple", linewidth = 1.2) +
    expand_limits(x = min_x, y = max_y) +
    # Coordinate and theme settings
    coord_fixed() + # Keep x and y scales equal so vectors look right (no stretching or squishing)
    theme_classic() +
    labs(title = "Complex Multiplication",
         x = "Real axis",
         y = "Imaginary axis") +
    theme(
      plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
      axis.title.x = element_text(size = 13),
      axis.title.y = element_text(size = 13)
    ) +
    guides(color = "none") +
    transition_manual(step)

  # Render and return the animation
  animate(p, fps = 20, nframes = steps, end_pause = 20)
}
