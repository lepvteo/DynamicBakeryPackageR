#' @importFrom methods is new
#' @importFrom stats runif
#' @importFrom utils head tail
NULL

#' Run a dynamic pricing simulation over a business day
#'
#' Simulates demand, stock depletion and dynamic price evolution for a single
#' product across one business day, optionally weighting demand by real
#' bakery attendance data.
#'
#' @param product A \linkS4class{Product} object to simulate.
#' @param nb_purchase Numeric. Number of purchase events to simulate (positive integer).
#' @param tic_size Numeric. Length of each time interval in minutes (default 1).
#' @param verbose Logical. If TRUE, print detailed progress (default TRUE).
#' @param bakery A \linkS4class{Bakery} object for attendance-weighted demand, or NULL.
#' @param day Character. Day of the week for attendance weighting (default "mon").
#' @return A list with simulation history: time, price, revenue, demand,
#'   unmet_demand and the final product state.
#'
#' @export
simulator <- function(product, nb_purchase = 25, tic_size = 10,
                      verbose = TRUE, bakery = NULL, day = "mon") {

  # A few necessary checks before running the simulation
  if (!is(product, "Product"))
    stop("'product' must be an S4 object inheriting from Product.")

  # Shiny slider input will return numerics, not integers, so we have to make the checks based on that
  if (!is.numeric(nb_purchase) || length(nb_purchase) != 1 || nb_purchase <= 0 || nb_purchase != round(nb_purchase))
    stop("'nb_purchase' must be a strictly positive integer.")

  if (!is.numeric(tic_size) || tic_size <= 0)
    stop("'tic_size' must be a strictly positive number.")

  if (!is.null(bakery) && !is(bakery, "Bakery"))
    stop("'bakery' must be an S4 object of class Bakery.")
  
  if (!is.null(bakery)) {
    valid_days <- setdiff(names(bakery@attendance_df), "hour")
    if (!(day %in% valid_days))
      stop(paste("'day' must be one of:", paste(valid_days, collapse = ", ")))
  }

  # Time span starts at production, ends at closing, in minutes
  closing_time_h <- if (!is.null(bakery)) bakery@closing_time else 20
  time_span <- (closing_time_h - product@production_time) * 60

  # Create time bins
  bins   <- seq(0, time_span, by = tic_size)
  n_tics <- length(bins) - 1

  if (verbose) {
    cat("========================================\n")
    cat("SIMULATION START\n")
    cat("========================================\n")
    cat("Product:", product@name, "\n")
    cat("Initial stock:", product@stock, "\n")
    cat("Reference price:", product@price_ref, "PLN\n")
    cat("Simulation duration:", time_span, "minutes\n")
    cat("Number of demands:", nb_purchase, "\n")
    cat("========================================\n\n")
  }

  # Generate demand moments: weighted if bakery provided, uniform otherwise
  if (!is.null(bakery)) {
    demand_moment <- generate_demand_moments(bakery, nb_purchase, bins, day, demand_profile = product@demand_profile)
  } else {
    demand_moment <- sort(runif(nb_purchase, min = 0, max = time_span))
  }

  if (verbose) {
    cat("Purchase moments (first 5):", head(demand_moment, 5), "...\n\n")
  }

  if (verbose) {
    cat("Total time intervals (tics):", n_tics, "\n")
    cat("Starting simulation...\n\n")
  }

  # Initialize history
  price_history <- numeric(n_tics)
  revenue_history <- numeric(n_tics)
  unmet_moment <- numeric(0)  # Store moments of blocked demand
  purchase_history <- numeric(0)
  
  product@price <- product@price_ref   # Reset price to reference before simulation

  # Simulate each time interval
  for (i in seq_len(n_tics)) {
    start <- bins[i]
    end <- bins[i + 1]

    # Count purchases in this time interval
    tic_demands <- sum(demand_moment >= start & demand_moment < end)

    # Use defensive method for attempts of buying a sold-out product (and update stock in sell() method)
    sale_result <- sell(product, tic_demands)
    product <- sale_result$product
    tic_purchases <- sale_result$actual_sales

    # Track unmet demand moments
    if (sale_result$stockout) {
        tic_demand_moments <- demand_moment[demand_moment >= start & demand_moment < end]
        n_unmet <- tic_demands - tic_purchases
        # Tag the last n_unmet demand moments in this tic as unmet
        if (n_unmet > 0 && length(tic_demand_moments) >= n_unmet) {
          unmet_moment <- c(
            unmet_moment,
            tail(tic_demand_moments, n_unmet)
        )
      }
    }

    # Absolute clock time in hours (e.g. 12.5 = 12h30)
    # .get_age() will subtract production_time internally
    current_time_hours <- product@production_time + (end / 60)

    # Simple stock adjustment: more stock = lower price
    cor_stock <- calculate_stock_adjustment(
      product,
      current_stock = product@stock,
      initial_stock = product@stock_initial
    )

    # Calculate cor_demand (if stock=0, no price adjustment)
    # Later, if we inplemente a refill of stocks when stocks get to 0 very quickly, we will have to base the demand
    # based on the true demand signal (even at stock=0) so the demand pressure can still be captured for the moment the stock is refilled

    if (product@stock == 0) {
      cor_demand <- 1.0   # No stock, no price signal
    } else {
      result_demand <- calculate_demand_adjustment(product, tic_demands, purchase_history)
      cor_demand <- result_demand$cor_demand
      purchase_history <- result_demand$history
    }

    # Calculate freshness coefficient (sandwich-specific)
    cor_freshness <- calculate_freshness_adjustment(product, current_time_hours)

    # Use your S4 method to get dynamic price
    product@price <- get_dynamic_price(
      product,
      current_time = current_time_hours,
      closing_time = closing_time_h,
      cor_demand = cor_demand,
      cor_stock = cor_stock,
      cor_freshness = cor_freshness
    )

    # Update product's current price (and cap it if ever it exceeds the boundariesd)
    product@price <- cap_price(product)

    # Store prices in history
    price_history[i] <- product@price

    # Store revenue in history
    revenue_history[i] <- tic_purchases * product@price

    # Verbose output every 10 tics or when purchases happen
    if (verbose && (i %% 10 == 0 || tic_demands > 0)) {
      tic_unmet <- tic_demands - tic_purchases
      cat(sprintf("Tic %3d | Time: %5.1f min | Demand: %d | Sold: %d | Unmet: %d | Stock: %2d | cor_stock: %.2f | cor_demand: %.2f | Price: %.2fPLN\n",
                  i, end, tic_demands, tic_purchases, tic_unmet, product@stock, cor_stock, cor_demand, product@price))
    }
  }

  if (verbose) {
    cat("\n========================================\n")
    cat("SIMULATION END\n")
    cat("========================================\n")
    cat("Final stock:", product@stock, "\n")
    cat("Items sold:", product@stock_initial - product@stock, "\n")
    cat("Final price:", round(product@price, 2), "PLN\n")
    cat("Min price:", round(min(price_history), 2), "PLN\n")
    cat("Max price:", round(max(price_history), 2), "PLN\n")
    cat("Avg price:", round(mean(price_history), 2), "PLN\n")
    cat("========================================\n")
  }

  # Return simulation results
  list(
    time = bins[-length(bins)],  # Remove last bin edge
    time_offset_min = product@production_time * 60,   # minutes since midnight at t=0
    price = price_history,
    revenue = revenue_history,
    demand = demand_moment,
    unmet_demand = unmet_moment,
    final_product = product
  )
}



#' Run a Monte Carlo simulation over multiple business days
#'
#' Repeats the single-day dynamic pricing simulation \code{n_runs} times for a
#' given product, aggregating revenue, price and sales outcomes across runs.
#'
#' @param product A \linkS4class{Product} object to simulate.
#' @param bakery A \linkS4class{Bakery} object for attendance-weighted demand, or NULL.
#' @param day Character. Day of the week for attendance weighting (default "mon").
#' @param n_runs Numeric. Number of simulation runs to perform (default 100).
#' @param nb_purchase Numeric. Number of purchase events per run (positive integer, default 25).
#' @param tic_size Numeric. Length of each time interval in minutes (default 10).
#' @return A list with one numeric vector per metric across all runs:
#'   \code{total_revenue}, \code{avg_price}, \code{items_sold}, and the scalar
#'   \code{n_runs}.
#'
#' @export
monte_carlo_simulation <- function(product, bakery = NULL, day = "mon",
                                   n_runs = 100, nb_purchase = 25,
                                   tic_size = 10) {
  results <- vector("list", n_runs)

  for (i in seq_len(n_runs)) {
    results[[i]] <- simulator(
      product      = product,
      nb_purchase  = nb_purchase,
      tic_size     = tic_size,
      verbose      = FALSE,
      bakery       = bakery,
      day          = day
    )
  }

  total_revenue <- vapply(results, function(r) sum(r$revenue), numeric(1))
  avg_price     <- vapply(results, function(r) mean(r$price),  numeric(1))
  items_sold    <- vapply(results, function(r) {
    r$final_product@stock_initial - r$final_product@stock
  }, numeric(1))

  list(
    total_revenue = total_revenue,
    avg_price     = avg_price,
    items_sold    = items_sold,
    n_runs        = n_runs
  )
}
