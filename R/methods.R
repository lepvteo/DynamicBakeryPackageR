### ALL METHODS FOR SIMULATION ###

# -----
# Bakery method: Generate demand moments
# -----

#' Generate attendance-weighted demand moments
#'
#' Distributes purchase events across the business day using bakery attendance
#' data and an optional product-specific demand profile.
#'
#' @param bakery A \linkS4class{Bakery} object.
#' @param nb_purchase Numeric. Number of purchase events (strictly positive integer).
#' @param bins Numeric vector. Time bin edges in minutes from production time.
#' @param day Character. Day of the week matching a column in \code{bakery@@attendance_df}.
#' @param demand_profile Numeric vector or NULL. Product hourly demand weights (default NULL).
#' @return Numeric vector of sorted demand moments in minutes from production time.
#'
#' @export
setGeneric("generate_demand_moments",
           function(bakery, nb_purchase, bins, day, demand_profile = NULL) {
             standardGeneric("generate_demand_moments")
           })

#' @describeIn generate_demand_moments Bakery: samples demand moments weighted by attendance and demand profile.
setMethod("generate_demand_moments", "Bakery",
  function(bakery, nb_purchase, bins, day, demand_profile = NULL) {

    valid_days <- setdiff(names(bakery@attendance_df), "hour")
    if (!day %in% valid_days)
      stop(paste("'day' must be one of:", paste(valid_days, collapse = ", ")))
    if (!is.numeric(nb_purchase) || nb_purchase <= 0 || nb_purchase != round(nb_purchase))
      stop("'nb_purchase' must be a strictly positive integer.")
    if (length(bins) < 2)
      stop("'bins' must have at least 2 elements.")

    tic_hours <- bakery@opening_time + (bins / 60)

    weights <- sapply(tic_hours, function(h) {
      slot <- max(which(bakery@attendance_df$hour <= h))
      bakery@attendance_df[slot, day]
    })

    if (!is.null(demand_profile)) {
      profile_weights <- sapply(tic_hours, function(h) {
        hour_index <- max(which(bakery@attendance_df$hour <= h))
        demand_profile[hour_index]
      })
      weights <- weights * profile_weights
    }

    if (sum(weights) == 0)
      stop("All attendance weights are zero. Cannot generate demand moments.")

    weights <- weights / sum(weights)

    demand_moment <- sort(
      sample(
        x       = bins[-length(bins)],
        size    = nb_purchase,
        replace = TRUE,
        prob    = weights[-length(weights)]
      )
    )
    return(demand_moment)
  }
)



# -----
# Product (parent) method: Demand adjustment calculation
# -----

#' Calculate the demand price adjustment coefficient
#'
#' Computes a multiplicative coefficient based on recent purchase history
#' to modulate price in response to observed demand pressure.
#'
#' @param object A \linkS4class{Product} object.
#' @param n_purchases Numeric. Number of purchases in the current time interval.
#' @param purchase_history Numeric vector. History of purchases in previous intervals.
#' @return A list with \code{cor_demand} (numeric coefficient) and \code{history} (updated history vector).
#'
#' @export
setGeneric("calculate_demand_adjustment", function(object, n_purchases, purchase_history) {
  standardGeneric("calculate_demand_adjustment")
})

#' @describeIn calculate_demand_adjustment Product: rolling 3-interval average, coefficient clamped between 0.95 and 1.15.
setMethod("calculate_demand_adjustment", "Product", function(object, n_purchases, purchase_history) {
  purchase_history <- c(purchase_history, n_purchases)
  window <- tail(purchase_history, 3)
  avg_demand <- mean(window)
  cor_demand <- 0.95 + (avg_demand / 3) * 0.20
  cor_demand <- min(max(cor_demand, 0.95), 1.15)
  return(list(cor_demand = cor_demand, history = purchase_history))
})



# -----
# Product (parent) method: Stock adjustment calculation
# -----

#' Calculate the stock price adjustment coefficient
#'
#' Returns a multiplicative price coefficient based on the current
#' stock ratio relative to the initial stock level.
#'
#' @param object A \linkS4class{Product} object.
#' @param current_stock Numeric. Current stock on the shelf.
#' @param initial_stock Numeric. Stock at opening.
#' @return Numeric coefficient: 0.95 (stock above 70%), 1.0 (mid-range), or 1.10 (stock below 30%).
#'
#' @export
setGeneric(name = "calculate_stock_adjustment",
           function(object, current_stock, initial_stock){
            standardGeneric("calculate_stock_adjustment")
           })

#' @describeIn calculate_stock_adjustment Product: step-wise adjustment 0.95 above 70%, 1.10 below 30%.
setMethod("calculate_stock_adjustment", "Product",
          function(object, current_stock, initial_stock){
            stock_ratio <- current_stock / initial_stock

            if (stock_ratio > 0.7) {
              cor_stock <- 0.95  # High stock: reduce price
            } else if (stock_ratio < 0.3) {
              cor_stock <- 1.10  # Low stock: increase price (scarcity)
            } else {
              cor_stock <- 1.0
            }

            return(cor_stock)
          })



# -----
# Product (parent) method: Freshness adjustment calculation
# -----

#' Calculate the freshness price adjustment coefficient
#'
#' Returns a multiplicative coefficient reflecting how product freshness
#' affects price over the business day.
#'
#' @param object A \linkS4class{Product} object.
#' @param current_time Numeric. Current hour of day.
#' @param max_age Numeric. Maximum product age in hours (default 13.5).
#' @return Numeric coefficient applied to the reference price.
#'
#' @export
setGeneric("calculate_freshness_adjustment",
           function(object, current_time, max_age = 13.5) {
             standardGeneric("calculate_freshness_adjustment")
           })

#' @describeIn calculate_freshness_adjustment Default for Product: no freshness effect.
setMethod("calculate_freshness_adjustment", "Product",
          function(object, current_time, max_age = 13.5) {
            return(1.0)
          })

# -----
# Internal helpers (not generics, not user-facing)
# -----

.get_age <- function(object, current_time) {
  current_time - object@production_time
}

.linear_freshness <- function(object, current_time, max_age,
                              freshness_start, freshness_end) {
  age           <- .get_age(object, current_time)
  slope         <- (freshness_start - freshness_end) / max_age
  cor_freshness <- freshness_start - (slope * age)
  return(max(freshness_end, cor_freshness))
}

# -----
# Child class methods: Freshness adjustment calculation
# -----

# Note: one shared production time per product. All units have the same age.
# Future improvement: track per-batch age to handle intra-day refills.

#' @describeIn calculate_freshness_adjustment Sandwich: moderate linear decay (+15% to -15%).
setMethod("calculate_freshness_adjustment", "Sandwich",
          function(object, current_time, max_age = 13.5) {
            .linear_freshness(object, current_time, max_age,
                              freshness_start = 1.15,
                              freshness_end   = 0.85)
          })

#' @describeIn calculate_freshness_adjustment Bread: narrow freshness range, stays good most of the day.
setMethod("calculate_freshness_adjustment", "Bread",
          function(object, current_time, max_age = 13.5) {
            .linear_freshness(object, current_time, max_age,
                              freshness_start = 1.05,
                              freshness_end   = 0.95)
          })

#' @describeIn calculate_freshness_adjustment Pizza: temperature-sensitive, hot commands premium.
setMethod("calculate_freshness_adjustment", "Pizza",
          function(object, current_time, max_age = 13.5) {
            .linear_freshness(object, current_time, max_age,
                              freshness_start = 1.20,
                              freshness_end   = 0.80)
          })




# -----
# Product (parent) method: End-of-day adjustment (last opening hour significant discount)
# -----

#' Calculate the end-of-day price adjustment coefficient
#'
#' Applies a progressive discount during the final hour before closing
#' to clear remaining stock.
#'
#' @param object A \linkS4class{Product} object.
#' @param current_time Numeric. Current hour of day.
#' @param closing_time Numeric. Closing hour of the bakery.
#' @return Numeric coefficient between 0.70 and 1.0.
#'
#' @export
setGeneric("calculate_endofday_adjustment", function(object, current_time, closing_time) {
  standardGeneric("calculate_endofday_adjustment")
})

#' @describeIn calculate_endofday_adjustment Product: up to 30% discount in the last hour before closing.
setMethod("calculate_endofday_adjustment", "Product",
  function(object, current_time, closing_time) {
    if (current_time >= closing_time - 1) {
      time_left <- closing_time - current_time 
      discount <- 0.30 * (1 - time_left)         # 0% most of the time, 30% at the closing
      return(1 - discount)
    }
    return(1.0)
  }
)



# -----
# Product (parent) method: Dynamic price calculation
# -----

#' Compute the dynamic price for a product
#'
#' Combines demand, stock, freshness and end-of-day multiplicative adjustment
#' coefficients into a final price relative to the reference price.
#'
#' @param object A \linkS4class{Product} object.
#' @param current_time Numeric. Current hour of day.
#' @param closing_time Numeric. Bakery closing hour (default 20).
#' @param cor_demand Numeric. Demand adjustment coefficient (default 1.0).
#' @param cor_stock Numeric. Stock adjustment coefficient (default 1.0).
#' @param cor_freshness Numeric. Freshness adjustment coefficient (default 1.0).
#' @return Numeric. Dynamic price in PLN.
#'
#' @export
setGeneric("get_dynamic_price", function(object, current_time, closing_time = 20,
                                         cor_demand = 1.0, cor_stock = 1.0,
                                         cor_freshness = 1.0) {
  standardGeneric("get_dynamic_price")
})

#' @describeIn get_dynamic_price Product: additive delta combination of all four adjustment coefficients.
setMethod("get_dynamic_price", "Product",
          function(object, current_time, closing_time = 20,
                   cor_demand = 1.0, cor_stock = 1.0, cor_freshness = 1.0) {

  cor_endofday <- calculate_endofday_adjustment(object, current_time, closing_time)

  # Convert coefficients to deltas (deviations from 1.0)
  delta_demand    <- cor_demand    - 1
  delta_stock     <- cor_stock     - 1
  delta_freshness <- cor_freshness - 1
  delta_endofday  <- cor_endofday  - 1

  total_delta <- delta_demand + delta_stock + delta_freshness + delta_endofday

  final_price <- object@price_ref * (1 + total_delta)
  return(final_price)
})



# -----
# Product (parent) method: Price cap (hard boundaries around reference price)
# -----

#' Cap dynamic price within hard boundaries
#'
#' Enforces a floor of 70% and a ceiling of 130% of the reference price,
#' preventing extreme pricing in either direction.
#'
#' @param object A \linkS4class{Product} object with an updated \code{price} slot.
#' @return Numeric. Capped price in PLN.
#'
#' @export
setGeneric("cap_price", function(object) {
  standardGeneric("cap_price")
})

#' @describeIn cap_price Product: clamps price between 70 and 130 percent of price_ref.
setMethod("cap_price", "Product", function(object) {
  p_min <- object@price_ref * 0.7
  p_max <- object@price_ref * 1.3
  return(min(max(object@price, p_min), p_max))
})



# -----
# Product (parent) method: Sell (with defensive stock validation)
# -----

#' Sell a quantity of a product
#'
#' Deducts the sold quantity from stock in a defensive manner,
#' capping actual sales at the available stock.
#'
#' @param object A \linkS4class{Product} object.
#' @param quantity Numeric. Requested number of units to sell.
#' @return A list with \code{product} (updated object), \code{attempted} (requested quantity),
#'   \code{actual_sales} (units sold), and \code{stockout} (logical, TRUE if demand exceeded supply).
#'
#' @export
setGeneric("sell", function(object, quantity) {
  standardGeneric("sell")
})

#' @describeIn sell Product: sells up to available stock, sets stockout flag if demand exceeds supply.
setMethod("sell", "Product", function(object, quantity) {

  if (!is.numeric(quantity) || length(quantity) != 1)
    stop("'quantity' must be a single numeric value.")
  if (quantity < 0)
    stop("Cannot sell negative quantity.")

  actual_sales  <- min(quantity, object@stock)
  object@stock  <- object@stock - actual_sales

  return(list(
    product      = object,
    attempted    = quantity,
    actual_sales = actual_sales,
    stockout     = (quantity > actual_sales)
  ))
})