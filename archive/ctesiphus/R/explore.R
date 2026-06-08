# Exploration procedure for the Ctesiphus Expedition.
# Depends on dice.R and exploration_tables.R.

# Look up a D36 value in one of the tables (rows span roll_lo..roll_hi).
lookup_d36 <- function(tbl, roll) {
  hit <- tbl[tbl$roll_lo <= roll & roll <= tbl$roll_hi, , drop = FALSE]
  if (nrow(hit) != 1L) {
    stop(sprintf("D36 value %s did not match exactly one row.", roll))
  }
  hit
}

location_table <- function(hex_type) {
  switch(hex_type,
    surface = surface_location_table,
    tomb    = tomb_location_table,
    stop("hex_type must be 'surface' or 'tomb'.")
  )
}

condition_table <- function(hex_type) {
  switch(hex_type,
    surface = surface_condition_table,
    tomb    = tomb_condition_table,
    stop("hex_type must be 'surface' or 'tomb'.")
  )
}

# Roll a result on a table, re-rolling until it is either repeatable
# (Ruin / Clear Conditions) or a code not already present in `existing`.
roll_unique_result <- function(tbl, existing = character(), max_tries = 1000L) {
  for (i in seq_len(max_tries)) {
    row <- lookup_d36(tbl, roll_d36())
    if (isTRUE(row$repeatable) || !(row$code %in% existing)) {
      return(row)
    }
  }
  stop("Could not roll a non-duplicate result; the table's non-repeatable rows may be exhausted.")
}

# Explore a hex: roll a location and a condition for the given hex type,
# honouring the re-roll-on-duplicate rule, and roll any initial resource.
#
#   hex_type             "surface" or "tomb"
#   existing_locations   location codes already on the map (same hex type)
#   existing_conditions  condition codes already on the map (same hex type)
#
# Returns a one-row tibble describing the explored hex.
explore_hex <- function(hex_type = c("surface", "tomb"),
                        existing_locations  = character(),
                        existing_conditions = character()) {
  hex_type <- match.arg(hex_type)

  loc  <- roll_unique_result(location_table(hex_type),  existing_locations)
  cond <- roll_unique_result(condition_table(hex_type), existing_conditions)

  init_amount <- if (!is.na(loc$init_dice)) roll_notation(loc$init_dice) else NA_integer_

  tibble::tibble(
    hex_type        = hex_type,
    location_code   = loc$code,
    location_name   = loc$name,
    becomes_blocked = loc$becomes_blocked,
    init_resource   = loc$init_resource,   # "supply"/"intel"/"campaign"/"search_cost"/NA
    init_amount     = init_amount,         # value rolled for that resource, or NA
    condition_code  = cond$code,
    condition_name  = cond$name,
    location_rules  = loc$rules,
    condition_rules = cond$rules
  )
}
