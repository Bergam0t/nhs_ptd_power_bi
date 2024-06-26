ptd_validate_plot_options <- function(point_size = NULL,
                                      percentage_y_axis = NULL,
                                      main_title = NULL,
                                      x_axis_label = NULL,
                                      y_axis_label = NULL,
                                      fixed_x_axis_multiple = NULL,
                                      fixed_y_axis_multiple = NULL,
                                      x_axis_date_format = NULL,
                                      x_axis_breaks = NULL,
                                      y_axis_breaks = NULL,
                                      icons_size = NULL,
                                      icons_position = NULL,
                                      colours = NULL,
                                      theme_override = NULL,
                                      break_lines = NULL) {
  if (!is.null(point_size)) {
    assertthat::assert_that(
      is.numeric(point_size),
      assertthat::is.scalar(point_size),
      point_size > 0,
      point_size <= 10,
      msg = "point_size must be a single number greater than 0 and less than or equal to 10."
    )
  }

  if (!is.null(percentage_y_axis)) {
    assertthat::assert_that(
      is.logical(percentage_y_axis),
      assertthat::is.scalar(percentage_y_axis),
      msg = "percentage_y_axis argument must a single logical."
    )
  }

  if (!is.null(main_title)) {
    assertthat::assert_that(
      is.character(main_title),
      assertthat::is.scalar(main_title),
      msg = "main_title argument must be a character of length 1."
    )
  }

  if (!is.null(x_axis_label)) {
    assertthat::assert_that(
      is.character(x_axis_label),
      assertthat::is.scalar(x_axis_label),
      msg = "x_axis_label argument must be a character of length 1."
    )
  }

  if (!is.null(y_axis_label)) {
    assertthat::assert_that(
      is.character(y_axis_label),
      assertthat::is.scalar(y_axis_label),
      msg = "y_axis_label argument must be a character of length 1."
    )
  }

  if (!is.null(fixed_x_axis_multiple)) {
    assertthat::assert_that(
      is.logical(fixed_x_axis_multiple),
      assertthat::is.scalar(fixed_x_axis_multiple),
      msg = "fixed_x_axis_multiple argument must be a logical of length 1."
    )
  }

  if (!is.null(fixed_y_axis_multiple)) {
    assertthat::assert_that(
      is.logical(fixed_y_axis_multiple),
      assertthat::is.scalar(fixed_y_axis_multiple),
      msg = "fixed_y_axis_multiple argument must be a logical of length 1."
    )
  }

  if (!is.null(x_axis_date_format)) {
    assertthat::assert_that(
      is.character(x_axis_date_format),
      assertthat::is.scalar(x_axis_date_format),
      msg = "x_axis_date_format argument must be a character of length 1."
    )
  }

  if (!is.null(x_axis_breaks)) {
    assertthat::assert_that(
      is.character(x_axis_breaks),
      assertthat::is.scalar(x_axis_breaks),
      grepl("^\\d+ (day|week|month|quarter|year)s?$", x_axis_breaks),
      msg = paste0(
        "x_axis_breaks argument must be a character of length 1, and be a valid string for seq.Date 'by'. ",
        "See seq.Date for more information."
      )
    )
  }

  if (!is.null(y_axis_breaks)) {
    assertthat::assert_that(
      is.numeric(y_axis_breaks),
      assertthat::is.scalar(y_axis_breaks),
      msg = "y_axis_breaks argument must be a numeric of length 1."
    )
  }

  if (!is.null(icons_size)) {
    assertthat::assert_that(
      is.numeric(icons_size),
      assertthat::is.scalar(icons_size),
      msg = "icons_size must be an integer of length 1."
    )
  }

  if (!is.null(icons_position)) {
    assertthat::assert_that(
      all(icons_position %in% c("top right", "bottom right", "bottom left", "top left", "none")),
      assertthat::is.scalar(icons_position),
      msg = "icons_position argument must be one of 'top right', 'bottom right', 'bottom_left', 'top left', or 'none'"
    )
  }

  if (!is.null(colours)) {
    assertthat::assert_that(
      inherits(colours, "ptd_spc_colours_class"),
      msg = "colours must be an object created by ptd_spc_colours()."
    )
  }

  if (!is.null(theme_override)) {
    assertthat::assert_that(
      inherits(theme_override, c("theme", "gg")),
      msg = "theme_override must be an object created by theme()."
    )
  }

  if (!is.null(break_lines)) {
    assertthat::assert_that(
      assertthat::is.scalar(break_lines),
      break_lines %in% c("both", "limits", "process", "none"),
      msg = "break_lines must be one of 'both', 'limits', 'process', or 'none'."
    )
  }

  invisible(TRUE)
}
