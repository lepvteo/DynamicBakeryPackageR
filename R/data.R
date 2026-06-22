### ATTENDANCE DATA ###

# Raw attendance vectors — sourced from Google Maps Popular Times
# https://maps.app.goo.gl/JhYvKgQEy5hVBCzB9
monday_attendance <- c(30, 42, 50, 59, 69, 75, 94, 100, 90, 82, 65, 59, 40, 26)
tuesday_attendance <- c(30, 42, 51, 50, 50, 42, 40, 42, 38, 38, 46, 51, 34, 25)
wednesday_attendance <- c(38, 46, 57, 57, 61, 55, 57, 50, 44, 42, 51, 59, 57, 34)
thursday_attendance <- c(36, 42, 55, 63, 65, 65, 57, 59, 69, 65, 57, 55, 40, 36)
friday_attendance <- c(28, 40, 53, 65, 71, 75, 73, 67, 57, 55, 48, 42, 32, 17)
saturday_attendance <- c(36, 48, 71, 80, 73, 76, 67, 65, 48, 42, 25, 0, 0, 0)

#' Hourly attendance data for Lubaszka Solec bakery
#'
#' Relative customer attendance per hour from 6h to 20h
#' (1-hour intervals: 6h-7h, 7h-8h, ..., 19h-20h)
#' for each weekday, sourced from Google Maps Popular Times.
#'
#' @format A data frame with 14 rows and 7 columns: \code{hour}, \code{mon},
#'   \code{tue}, \code{wed}, \code{thu}, \code{fri}, \code{sat}.
#' @source \url{https://maps.app.goo.gl/JhYvKgQEy5hVBCzB9}
attendance_df <- data.frame(
  hour  = 6:19,                # 6h-7h, 7h-8h, ..., 19-20h
  mon   = monday_attendance,
  tue   = tuesday_attendance,
  wed   = wednesday_attendance,
  thu   = thursday_attendance,
  fri   = friday_attendance,
  sat   = saturday_attendance
)


# -----
# Bakery
# -----
#' Lubaszka Solec bakery object
#'
#' Main S4 Bakery object representing the Lubaszka bakery (Solec location),
#' initialized with real hourly attendance data.
#'
#' @format An S4 object of class \code{Bakery}.
"lubaszka_solec"

# -----
# Sandwiches
# -----
#' Kanapka szarpana product
#' @format An S4 object of class \code{Sandwich}.
"kanapka_szarpana"

#' Kanapka Reuben product
#' @format An S4 object of class \code{Sandwich}.
"kanapka_reuben"

#' Kanapka weganska product
#' @format An S4 object of class \code{Sandwich}.
"kanapka_weganska"

# -----
# Breads
# -----
#' Chleb wiejski product
#' @format An S4 object of class \code{Bread}.
"chleb_wiejski"

#' Chleb gryczany product
#' @format An S4 object of class \code{Bread}.
"chleb_grycany"

#' Chleb fitness product
#' @format An S4 object of class \code{Bread}.
"chleb_fitness"

# -----
# Pizzas
# -----
#' Pizza Margherita product
#' @format An S4 object of class \code{Pizza}.
"pizza_margherita"

#' Pizza Salami product
#' @format An S4 object of class \code{Pizza}.
"pizza_salami"

#' Pizza Hawaii product
#' @format An S4 object of class \code{Pizza}.
"pizza_hawaii"

# -----
# Product catalog
# -----
#' Product catalog
#'
#' Named list of all S4 product objects, used for Shiny input selection.
#'
#' @format A named list of S4 objects (Sandwich, Bread, Pizza).
"product_catalog"