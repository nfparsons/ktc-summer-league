# tests/testthat/test-exploration.R
# Run with: testthat::test_local()  (or devtools::test())
# Assumes dice.R, exploration_tables.R, explore.R are loaded (e.g. via the
# package, or source()'d in tests/testthat/setup.R).

valid_d36 <- c(11:16, 21:26, 31:36)
tables <- list(
  SL = surface_location_table,  TL = tomb_location_table,
  SC = surface_condition_table, TC = tomb_condition_table
)

test_that("every table has 13 rows with unique codes and full D36 coverage", {
  for (nm in names(tables)) {
    t <- tables[[nm]]
    expect_equal(nrow(t), 13L, info = nm)
    expect_false(as.logical(anyDuplicated(t$code)), info = nm)
    expect_true(all(nzchar(t$code)) && all(nzchar(t$name)) && all(nzchar(t$rules)), info = nm)
    covered <- vapply(valid_d36, function(v) nrow(t[t$roll_lo <= v & v <= t$roll_hi, ]), integer(1))
    expect_true(all(covered == 1L), info = paste(nm, "D36 coverage"))
  }
})

test_that("dice rollers stay in range", {
  set.seed(1)
  expect_true(all(replicate(2000, roll_d36()) %in% valid_d36))
  expect_true(all(replicate(200, roll_d3())  %in% 1:3))
  expect_true(all(replicate(200, roll_d6())  %in% 1:6))
  expect_true(all(replicate(200, roll_notation("2d6")) %in% 2:12))
  expect_true(is.na(roll_notation(NA_character_)))
})

test_that("the 11-16 band resolves to Ruin / Clear Conditions", {
  expect_equal(lookup_d36(surface_location_table, 13)$code, "SL11")
  expect_equal(lookup_d36(tomb_location_table, 11)$code,    "TL11")
  expect_equal(lookup_d36(surface_condition_table, 16)$code, "SC11")
  expect_equal(lookup_d36(tomb_condition_table, 14)$code,   "TC11")
})

test_that("only Tectonic Fissure auto-blocks its hex on exploration", {
  expect_true(surface_location_table$becomes_blocked[surface_location_table$code == "SL21"])
  expect_equal(sum(surface_location_table$becomes_blocked), 1L)
  expect_equal(sum(tomb_location_table$becomes_blocked), 0L)
})

test_that("initial-resource hooks are set where expected", {
  expect_equal(surface_location_table$init_dice[surface_location_table$code == "SL32"], "2d6")
  expect_equal(tomb_location_table$init_resource[tomb_location_table$code == "TL23"], "campaign")
  expect_equal(tomb_location_table$init_resource[tomb_location_table$code == "TL35"], "search_cost")
})

test_that("explore_hex returns the right families for each hex type", {
  set.seed(3)
  for (i in 1:100) {
    s <- explore_hex("surface")
    expect_match(s$location_code, "^SL"); expect_match(s$condition_code, "^SC")
    t <- explore_hex("tomb")
    expect_match(t$location_code, "^TL"); expect_match(t$condition_code, "^TC")
  }
})

test_that("non-Ruin duplicates are re-rolled but Ruin may repeat", {
  set.seed(4)
  all_sl <- surface_location_table$code
  res <- vapply(1:100, function(i) explore_hex("surface", existing_locations = all_sl)$location_code, "")
  expect_true(all(res == "SL11"))  # only the repeatable Ruin remains available

  res2 <- vapply(1:300, function(i)
    explore_hex("surface", existing_locations = c("SL22","SL23","SL24"))$location_code, "")
  expect_false(any(res2 %in% c("SL22","SL23","SL24")))
})

test_that("init_amount is rolled for resource hexes and NA otherwise", {
  set.seed(6)
  for (i in 1:300) {
    e <- explore_hex("surface")
    if (e$location_code %in% c("SL22","SL31")) expect_true(e$init_amount %in% 1:6)
    if (e$location_code == "SL32")             expect_true(e$init_amount %in% 2:12)
    if (!(e$location_code %in% c("SL21","SL22","SL31","SL32"))) expect_true(is.na(e$init_amount))
  }
})
