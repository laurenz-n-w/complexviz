#' Animate the polar form of a complex number
#'
#' This function visualizes a complex number `z` using its polar representation.
#' It first shows the modulus (radius) as a vector from the origin, then rotates
#' it by the argument (angle) to reach the final position of `z` in the complex plane.
#' This animation illustrates how the modulus and argument define the position
#' of a complex number in the complex plane using polar coordinates.
#'
#' @param z A complex number
#' @param steps Number of steps for each animation phase (default: 50)
#' @return An animated ggplot object showing the polar form.
#'
#' @examples
#' z <- 1 +2i
#' plot_polar(z)
#'
#' @import ggplot2 gganimate
#' @importFrom gifski gifski_renderer
#' @importFrom grid unit
#'
#' @export

plot_polar <- function(z, steps = 50) {

  # Modulus (radius) and argument (angle) of the complex number
  r <- Mod(z)
  theta <- Arg(z)

  # Phase 1: animate growing the radius as a horizontal line
  phase1 <- data.frame(
    x = seq(0, r, length.out = steps),
    y = 0,
    step = 1:steps
  )

  # Phase 2: rotate the vector from 0 to the angle theta
  angles <- seq(0, theta, length.out = steps)

  # Ensure counter-clockwise direction
  if (theta < 0) theta <- 2 * pi + theta
  angles <- seq(0, theta, length.out = steps)

  phase2 <- data.frame(
    x = r * cos(angles),
    y = r * sin(angles),
    step = (steps + 1):(2 * steps)
  )

  # Phase 3: hold the final position of the arrow for a few frames
  hold_steps <- 10
  last_point <- tail(phase2, 1)
  hold <- data.frame(
    x = rep(last_point$x, hold_steps),
    y = rep(last_point$y, hold_steps),
    step = (2 * steps + 1):(2 * steps + hold_steps)
  )

  # Combine all animation steps
  df <- rbind(phase1, phase2, hold)

  # Draw arc representing the angle theta
  arc_step <- max(df$step)
  arc <- data.frame(
    angle = seq(0, theta, length.out = 100),
    r = 0.25 * r,
    step = rep(arc_step, 100)
  )
  arc$x <- arc$r * cos(arc$angle)
  arc$y <- arc$r * sin(arc$angle)

  # Legend showing cartesian representation, radius, angle and polar representation
  legend_label <- paste0(
    "z = ", round(Re(z), 2), " + ", round(Im(z), 2), "i\n",
    "r = ", round(r, 2), "\n",
    "θ = ", round(theta, 2), " rad\n",
    "Polar: z = r · exp(iθ)"
  )

  # Extend x-axis so the legend is visible
  x_max <- max(df$x, na.rm = TRUE)
  x_extension <- 0.3 * r

  # Create the animated plot
  plot <- ggplot(df, aes(x = 0, y = 0)) +
    geom_segment(aes(xend = x, yend = y),
                 arrow = arrow(length = unit(0.25, "cm")),
                 color = "salmon", size = 1.5) +
    geom_path(data = arc, aes(x = x, y = y, group = step),
              color = "salmon", size = 1,
              inherit.aes = FALSE) +
    annotate("label",
             x = x_max + x_extension / 2,
             y = max(df$y, na.rm = TRUE) / 2,
             label = legend_label,
             size = 4.2,
             hjust = 0.5, vjust = 0,
             fill = "white", color = "salmon") +
    coord_fixed() +
    # Extension of the x-axis to fit the legend
    xlim(min(df$x, na.rm = TRUE) - 0.3 * r, max(df$x, na.rm = TRUE) + 0.5 * r) +
    ylim(min(df$y, na.rm = TRUE), max(df$y, na.rm = TRUE) + 0.25 * r) +
    labs(
      title = "Polar Coordinates of a Complex Number",
      x = "Real",
      y = "Imaginary"
    ) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    transition_manual(step)

  # Render animation
  animate(plot,
          nframes = max(df$step) + 45,
          fps = 15,
          end_pause = 45,
          renderer = gifski_renderer())
}
