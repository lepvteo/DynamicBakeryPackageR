### CLASSES DEFINITION ###

# -----
# Class: Bakery (to implement attendance for the simulation of the demand)
# -----

#' Bakery: bakery shop configuration
#'
#' Holds shop metadata and the hourly attendance data used to weight
#' demand moment generation during simulations.
#'
#' @slot name Character. Display name of the bakery.
#' @slot opening_time Numeric. Opening hour (e.g. 6.5 = 6h30).
#' @slot closing_time Numeric. Closing hour (e.g. 20.0 = 20h00).
#' @slot attendance_df Data frame. Hourly attendance weights; rows are hours, columns are weekdays.
#'
#' @rdname Bakery-class
#' @exportClass Bakery
#' @export
Bakery <- setClass("Bakery",
  slots = list(
    name             = "character",
    opening_time     = "numeric",   # e.g. 6.5 = 6h30
    closing_time     = "numeric",   # e.g. 20.0
    attendance_df    = "data.frame" # rows = hours, cols = mon/tue/wed/thu/fri/sat
  ),
  prototype = list(
    opening_time = 6.5,
    closing_time = 20
    # attendance_df = attendance_df
  )
)



# -----
# Parent class: Product
# -----

# Default demand profile: uniform (no modification of attendance weights)
default_demand_profile <- rep(1.0, 14)  # 14 hours, 6h to 19h

#' Product: base class for all bakery products
#'
#' Parent S4 class holding pricing, stock and freshness attributes shared
#' by every product category (Bread, Sandwich, Pizza).
#'
#' @slot type Character. Product category label.
#' @slot name Character. Product display name.
#' @slot price_ref Numeric. Reference price in PLN.
#' @slot price Numeric. Current dynamic price in PLN.
#' @slot stock_initial Numeric. Stock available at opening.
#' @slot stock Numeric. Current stock on the shelf.
#' @slot production_time Numeric. Hour of production (0-24).
#' @slot demand_profile Numeric vector of length 14. Hourly demand weights.
#' @slot age Numeric. Product age in hours since production.
#'
#' @rdname Product-class
#' @exportClass Product
#' @export
Product <- setClass(
  "Product",
  slots = list(
    type = "character",
    name = "character",
    # weight = "numeric",
    price_ref = "numeric",        # reference price (from real photos)
    price = "numeric",            # current dynamic price
    stock_initial = "numeric",    # keeping intial stock in memory for simulation overview purpose?
    stock = "numeric",            # stock on the shelf
    production_time = "numeric",
    demand_profile = "numeric",
    age = "numeric"
  ),
  prototype = list(
    price = 0,
    demand_profile = default_demand_profile
  ),
  validity = function(object) {
  if (object@price_ref <= 0)
    return("price_ref must be strictly positive")
  if (object@stock < 0)
    return("stock cannot be negative")
  if (object@stock_initial <= 0)
    return("stock_initial must be strictly positive")
  if (object@stock > object@stock_initial)
    return("stock cannot exceed stock_initial")
  if (object@production_time < 0 || object@production_time > 24)
    return("production_time must be between 0 and 24 hours") # can be before opening time though
  return(TRUE)
  }
)



# -----
# Child classes: products (inherit from Product parent class' arguments and validity)
# -----

#' Bread: product subclass for bakery breads
#'
#' Inherits all slots and validity from \linkS4class{Product}.
#' Sets a morning-weighted demand profile and dawn production time.
#'
#' @rdname Bread-class
#' @exportClass Bread
#' @export
Bread <- setClass("Bread",
          contains = "Product",
          prototype = list(
            type = "Bread",
            production_time = 6.5,    # Produced at dawn to be ready on the opening
            demand_profile = c(2.0, 2.0, 1.5, 1.2, 0.8, 0.6, 0.5, 0.4, 0.3, 0.3, 0.2, 0.2, 0.1, 0.1)
            # Bread sells mostly in the morning: heavy weights on early hours
          )
)

#' Sandwich: product subclass for bakery sandwiches
#'
#' Inherits all slots and validity from \linkS4class{Product}.
#' Sets a lunch-peaked demand profile and mid-morning production time.
#'
#' @rdname Sandwich-class
#' @exportClass Sandwich
#' @export
Sandwich <- setClass("Sandwich",
          contains = "Product",
          prototype = list(
            type = "Sandwich",       
            production_time = 10,     # Prepared in the middle of the morning
            demand_profile = c(0.2, 0.3, 0.5, 0.8, 1.2, 1.5, 1.5, 1.3, 1.0, 0.7, 0.4, 0.2, 0.1, 0.1)
            # Sandwiches peak at lunch: 11h-13h
          )
)

#' Pizza: product subclass for bakery pizzas
#'
#' Inherits all slots and validity from \linkS4class{Product}.
#' Sets a lunch and early-afternoon demand profile with noon production time.
#'
#' @rdname Pizza-class
#' @exportClass Pizza
#' @export
Pizza <- setClass("Pizza",
          contains = "Product",
          prototype = list(
            type = "Pizza",
            production_time = 12,     # First batch in the late morning / noon
            demand_profile = c(0.1, 0.1, 0.2, 0.3, 0.5, 0.8, 1.5, 1.5, 1.3, 0.8, 0.5, 0.4, 0.3, 0.3)
            # Pizza peaks at lunch and early afternoon, still remains significant in the evening
          )
)