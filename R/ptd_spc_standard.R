#' SPC Standard Calculations (internal function)
#'
#' Returns a data frame containing SPC fields which are common to all methodologies, including 'plot the dots'
#'
#' This function is designed to produce consistent SPC charts
#' across Information Department reporting, according to the 'plot the dots'
#' logic produced by NHSI. The function can return either a plot or data frame.
#'
#'
#' @param .data A data frame containing a value field, a date field,
#' and a category field (if for faceting). There should be no gaps in the time series
#' for each category.
#' @param options created by spcOptions function
#'
#' @noRd

#' @import dplyr

ptd_spc_standard <- function(.data, options = NULL) {
  # get values from options
  value_field <- options$value_field
  date_field <- options$date_field
  facet_field <- options$facet_field
  fix_after_n_points <- options$fix_after_n_points
  trajectory_field <- options$trajectory

  # set trajectory field
  if (is.null(trajectory_field)) {
    .data$trajectory <- rep(as.numeric(NA), nrow(.data))
  } else {
    assertthat::assert_that(
      !is.null(.data[[trajectory_field]]),
      msg = paste0(
        "Trajectory column (",
        trajectory_field,
        ") does not exist in .data"
      )
    )
    .data$trajectory <- .data[[trajectory_field]]
  }

  # Set facet/grouping or create pseudo
  # If no facet field specified, bind a pseudo-facet field for grouping/joining purposes
  if (is.null(facet_field)) {
    .data$facet <- "no facet"
  } else {
    .data$facet <- .data[[facet_field]]
  }

  
  .data$y <- .data[[value_field]]
  .data$x <- .data[[date_field]]
  
  # Constants
  limit <- 2.66
  limitclose <- 2 * (limit / 3)

  # Restructure starting data frame
  temp <- .data %>%
    select(
      # y = any_of(value_field),
      # x = any_of(date_field),
      # f = any_of("facet"),
      # rebase = any_of("rebase"),
      # trajectory = any_of("trajectory"),
      y,
      x,
      f = facet,
      rebase = rebase,
      trajectory = trajectory,
    ) %>%
    # Group data frame by facet
    group_by(.data$f) %>%
    # Order data frame by facet, and x axis variable
    arrange(.data$f, .data$x) %>%
    # convert rebase 0/1's to group indices
    mutate(rebase_group = cumsum(.data$rebase)) %>%
    group_by(.data$rebase_group, .add = TRUE) %>%
    mutate(
      fix_y = ifelse(row_number() <= (fix_after_n_points %||% Inf), .data$y, NA),
      mean = mean(.data$fix_y, na.rm = TRUE),
      mr = c(NA, abs(diff(.data$fix_y))),
      amr = mean(.data$mr, na.rm = TRUE),

      # screen for outliers
      mr = case_when(
        !options$screen_outliers ~ .data$mr,
        .data$mr < 3.267 * .data$amr ~ .data$mr,
        TRUE ~ as.numeric(NA)
      ),
      amr = mean(.data$mr, na.rm = TRUE),

      # identify lower/upper process limits
      lpl = .data$mean - (limit * .data$amr),
      upl = .data$mean + (limit * .data$amr),
      # identify near lower/upper process limits
      nlpl = .data$mean - (limitclose * .data$amr),
      nupl = .data$mean + (limitclose * .data$amr),

      # Identify any points which are outside the upper or lower process limits
      outside_limits = (.data$y > .data$upl | .data$y < .data$lpl),
      # Identify whether a point is above or below the mean
      relative_to_mean = sign(.data$y - .data$mean),

      # Identify if a point is between the near process limits and process limits
      close_to_limits = !.data$outside_limits & (.data$y < .data$nlpl | .data$y > .data$nupl)
    ) %>%
    # clean up by removing columns that no longer serve a purpose and ungrouping data
    #select(-any_of(c("mr", "nlpl", "nupl", "amr", "rebase"))) %>%
    select(-c("mr", "nlpl", "nupl", "amr", "rebase")) %>%
    ungroup()
}
