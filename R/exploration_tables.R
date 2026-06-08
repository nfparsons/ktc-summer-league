# Exploration tables for the Ctesiphus Expedition (Dead Silence campaign pack,
# pp. 58-67). Rule text below is a paraphrase of the game mechanics in our own
# words, not the book's flavour text.
#
# Each row maps to a D36 roll. The rows 11-16 ("Ruin" for locations, "Clear
# Conditions" for conditions) are a single repeatable result spanning that range
# (roll_lo = 11, roll_hi = 16, repeatable = TRUE); every other row is a single
# D36 value (roll_lo == roll_hi) and may appear at most once on a map.
#
# Location tables also carry the hooks that fire automatically on first
# exploration:
#   becomes_blocked : the hex is immediately blocked (Tectonic Fissure).
#   init_resource   : "supply" / "intel" / "campaign" stockpile seeded on the
#                     hex, or "search_cost" (Doomsday Vault's per-search SP cost).
#   init_dice       : dice to roll for that initial value (see roll_notation()).
# Everything else (Search / Camp / Threat-phase effects) is left as paraphrased
# `rules` text for now and will be turned into executable hooks in later steps.

# ---- Surface Location (SL) --------------------------------------------------
surface_location_table <- tibble::tibble(
  roll_lo         = c(11L, 21L, 22L, 23L, 24L, 25L, 26L, 31L, 32L, 33L, 34L, 35L, 36L),
  roll_hi         = c(16L, 21L, 22L, 23L, 24L, 25L, 26L, 31L, 32L, 33L, 34L, 35L, 36L),
  repeatable      = c(TRUE, rep(FALSE, 12L)),
  code            = c("SL11","SL21","SL22","SL23","SL24","SL25","SL26",
                      "SL31","SL32","SL33","SL34","SL35","SL36"),
  name            = c("Ruin","Tectonic Fissure","Abandoned Camp","Cryovolcanic Edifices",
                      "Asteroid Impact","Landing Site","Observation Tower","Crashed Ship",
                      "Resource Stockpile","Starsteles","Blackstone Obelisk",
                      "Forsaken Fortress","Beast Lair"),
  becomes_blocked = c(FALSE, TRUE, rep(FALSE, 11L)),
  init_resource   = c(NA, NA, "supply", NA, NA, NA, NA, "intel", "supply", NA, NA, NA, NA),
  init_dice       = c(NA, NA, "d6",     NA, NA, NA, NA, "d6",    "2d6",    NA, NA, NA, NA),
  rules = c(
    "No additional rules.",
    "Hex becomes blocked.",
    "On exploration, roll D6 for its supply stockpile. Search: gain D3 of that stockpile (reducing it); nothing remains once it reaches 0.",
    "Search: spend 1 or 2 SP. Spend 1 to roll one D3, spend 2 to roll two D3. Per die: 1 = nothing, 2 = gain D3 SP, 3 = gain 1 CP. Each player may gain the '2' and '3' outcomes at most once each per campaign.",
    "While a kill team is here it cannot Resupply. Manoeuvring out of this hex costs +1 SP to enter a surface hex and -1 SP to enter a tomb hex (so an adjacent tomb hex is free).",
    "Encamp here costs only 1 SP. Resupplying while here gains an extra D3 SP.",
    "Search: immediately Scout for 0 SP, choosing either (a) any unexplored surface hex regardless of range, or (b) an unexplored surface hex within 3, generating two valid location results and keeping one. Camp: when you explore any surface hex you may generate two valid location results and keep one, and Scout never costs more than 1 SP when targeting a surface hex.",
    "On exploration, roll D6 for its intel. Search: gain D3 intel (reducing it; none left at 0). Each intel lets you immediately Scout for 0 SP, targeting any unexplored surface (not tomb) hex regardless of range.",
    "On exploration, roll 2D6 for its supply stockpile. Search: gain D6 of that stockpile (reducing it; none left at 0).",
    "Search: roll D3 - 1 = nothing; 2 = move your kill team to a different surface hex (not one already holding two kill teams); 3 = move to a tomb hex (same restriction). Camp: gain 1 CP while you hold a camp here.",
    "Search: gain 1 CP (each player once per campaign). Camp: gain 1 CP while you hold a camp here.",
    "The first time any player Encamps here they may relocate their base here (the old base location becomes an Abandoned Camp); doing so grants 1 CP but not the camp rule below. Camp: gain 1 CP while camped here. Search: gain D3 SP; you cannot Search if an opponent has a base or camp here.",
    "Threat phase: a beast may attack each player within 2 surface hexes - roll off, each adding 1 per hex of distance (max +2); the loser loses D6 SP. If only one player is in range they roll D6 instead and avoid the attack on 5+. Encamp is barred here until a Demolish has occurred, and Demolish cannot happen the same round the hex is explored. The first Demolish here kills the beast (this rule is ignored thereafter)."
  )
)

# ---- Tomb Location (TL) -----------------------------------------------------
tomb_location_table <- tibble::tibble(
  roll_lo         = c(11L, 21L, 22L, 23L, 24L, 25L, 26L, 31L, 32L, 33L, 34L, 35L, 36L),
  roll_hi         = c(16L, 21L, 22L, 23L, 24L, 25L, 26L, 31L, 32L, 33L, 34L, 35L, 36L),
  repeatable      = c(TRUE, rep(FALSE, 12L)),
  code            = c("TL11","TL21","TL22","TL23","TL24","TL25","TL26",
                      "TL31","TL32","TL33","TL34","TL35","TL36"),
  name            = c("Ruin","Transdimensional Portal","Transeptum Maze","Crucible of Whispers",
                      "Power Cell Sanctum","Transtechnic Fulcrum","Energy Hot Spot",
                      "Vivitrophic Terminal","Hyperfractal Gaol","Revivification Crypt",
                      "Astral Augury","Doomsday Vault","Dimension Matrix"),
  becomes_blocked = rep(FALSE, 13L),
  init_resource   = c(NA, NA, NA, "campaign", NA, NA, NA, NA, NA, NA, NA, "search_cost", NA),
  init_dice       = c(NA, NA, NA, "d3",       NA, NA, NA, NA, NA, NA, NA, "d3",          NA),
  rules = c(
    "No additional rules.",
    "Search (at most once per campaign round): select one tomb hex and one surface hex. A kill team beginning a Manoeuvre here may treat the distance to either selected hex as 1, move to it, then continue up to 2 more hexes.",
    "Manoeuvring out of this hex costs 1 extra SP (so an adjacent move costs 2). Camp: Demolish cannot be performed here by anyone (except via the Doomsday Vault), and you may ignore the extra-SP Manoeuvre rule.",
    "On exploration, roll D3 for its campaign-point store. Search: spend 1/3/5 SP to roll 1/2/3 D6; any 5+ gains 1 of its CP (reducing it; none at 0). Camp: at the start of each Threat phase roll D6, on 5+ gain 1 of its CP (reducing it).",
    "The first Demolish here makes every player with a kill team in a tomb hex (except the one who acted) lose D6 SP and bars Encamp here for the rest of the campaign; no opponent camp is needed, and a camp of yours here is removed. Camp: gain 1 CP while camped here.",
    "Search: choose one other tomb hex to become blocked (the previously chosen one, if any, is unblocked). A camp in a blocked hex keeps its benefits but is ignored for Regroup until it is unblocked.",
    "Camp: gain 1 CP while camped here, and gain 1 SP at the start of each Threat phase.",
    "Search: roll 2D6; you may immediately spend SP equal to the result to gain 1 CP (at most 1 CP per campaign from searching this hex). Camp: at campaign end, if you have a camp here and 5+ SP, gain 1 CP.",
    "The first Encamp here adds a roaming released prisoner to this hex (track its location). The first Demolish here removes the prisoner from the campaign (even if not yet added). Camp: at the start of each Threat phase move the prisoner up to D3 hexes; where it stops, unless that is the Transeptum Maze, each other player with a kill team there loses D6 SP (rolled separately) and an opponent camp there is removed. If that costs anyone SP or a camp, roll D6 at the end of each Threat phase - on 4+ the prisoner leaves the campaign.",
    "Start of each Action phase: each player with a kill team here may take a free Resupply (this does not stop them taking another action that phase, except a second Resupply). Camp: at the start of each Threat phase roll D3 - on 2+ gain SP equal to the result.",
    "Search: the first player to search gains D3 CP; each later searcher gains 1 CP. Each player may search once per campaign.",
    "On exploration, roll D3 for the SP cost to search it. Search: choose one tomb hex and immediately Demolish it at no extra SP cost (removing your own camp there if present); that hex becomes blocked and you gain 1 CP.",
    "Search: gain the dimensional key (unless a player already holds it). While you hold it you may take a Dimensional Manoeuvre instead of a normal move - move to any hex (not one with two kill teams) for 1 SP, then release the key (it becomes available to the next searcher of this hex)."
  )
)

# ---- Surface Condition (SC) -------------------------------------------------
surface_condition_table <- tibble::tibble(
  roll_lo    = c(11L, 21L, 22L, 23L, 24L, 25L, 26L, 31L, 32L, 33L, 34L, 35L, 36L),
  roll_hi    = c(16L, 21L, 22L, 23L, 24L, 25L, 26L, 31L, 32L, 33L, 34L, 35L, 36L),
  repeatable = c(TRUE, rep(FALSE, 12L)),
  code       = c("SC11","SC21","SC22","SC23","SC24","SC25","SC26",
                 "SC31","SC32","SC33","SC34","SC35","SC36"),
  name       = c("Clear Conditions","Dust Storms","Radiation Field","Blighted Land",
                 "Missile Strike","Minefield","Skull Mounds","Subterranean Tremors",
                 "Exotic Particle Field","Metallic Infused Vegetation","Gyromantic Shards",
                 "Cryoflux Blizzard","Gravitic Anomaly"),
  rules = c(
    "No additional rules.",
    "Operatives' ranged Hit stat is worsened by 1 (not cumulative with being injured) unless Heavy terrain is in their control range, they are wholly within a stronghold, or part of their base is under Vantage terrain.",
    "When an operative is activated with no Heavy terrain in its control range, roll D6; if the result is below its Save stat it takes damage equal to the result.",
    "Ready step of each Strategy phase: 1 damage to each operative on the killzone floor (except those wholly within a stronghold).",
    "Ready step of the first Strategy phase, one player rolls D3+1 to pick a turning point. In that turning point's Ready step, each operative within 6\" of the killzone centre takes D6+3 damage (rolled separately).",
    "Place a Mines marker on the centre of each objective marker. When choosing equipment each player must take the mines universal equipment and may pick up to five equipment options instead of four.",
    "The battle's winner gains 1 extra CP (each player at most once per campaign from this condition).",
    "Charge actions get -1\" Move; Dash actions move 1\" less.",
    "When shooting with the centreline intervening, re-roll one of your critical successes, or one normal success if you have no crits.",
    "Each player must take the heavy barricade equipment and may pick up to five equipment options; set it up wholly in your territory on the floor, over 2\" from other equipment terrain. Crossing it within 1\" inflicts D3 damage.",
    "When shooting, if any of your attack dice show a 1 (before re-rolls) you must re-roll all of your attack dice (not just some).",
    "When a friendly operative Charges, before it moves roll D6; if the result exceeds its APL your opponent picks an enemy operative it can reach and moves your operative for that action toward that enemy (ending within control range).",
    "Once per turning point, each player: when a friendly operative takes an action that moves it, it may FLY - instead of moving, remove and set it back up within its Move (3\" if Dashing), measuring horizontal distance only and obeying placement rules; no extra distance on a Charge, and it cannot be set up within control range of an enemy unless Charging."
  )
)

# ---- Tomb Condition (TC) ----------------------------------------------------
tomb_condition_table <- tibble::tibble(
  roll_lo    = c(11L, 21L, 22L, 23L, 24L, 25L, 26L, 31L, 32L, 33L, 34L, 35L, 36L),
  roll_hi    = c(16L, 21L, 22L, 23L, 24L, 25L, 26L, 31L, 32L, 33L, 34L, 35L, 36L),
  repeatable = c(TRUE, rep(FALSE, 12L)),
  code       = c("TC11","TC21","TC22","TC23","TC24","TC25","TC26",
                 "TC31","TC32","TC33","TC34","TC35","TC36"),
  name       = c("Clear Conditions","Darkness","Scarab Swarm","Tesla Rupture",
                 "Automated System Error","Weakened Structure","Crypt Fatigue",
                 "Temporal Diffusement","Hyperspatial Breach","Xenoviral Demise",
                 "Collapsing Tomb","Nanoweave Web Traps","Neurotechnic Haunting"),
  rules = c(
    "No additional rules.",
    "An operative shooting a target more than 8\" away treats that target as obscured.",
    "Place a Scarab Swarm marker near the killzone centre. Each Strategy phase Ready step, move it 2D3\" toward the nearest operative (roll off if tied). Once per operative's activation, when the marker is within its control range, it takes D3 damage.",
    "Ready step of the 2nd and 4th Strategy phases: 1 damage to each operative that has Wall terrain in control range, or is within control range of one that does (not cumulative).",
    "Ready step of each Strategy phase: one player randomly picks a hatchway and toggles it open or closed.",
    "Breach and Operate Hatch actions cost 1 less AP. This stacks with Breach's second-effect reduction (e.g. Grenadier) but not with other AP reductions.",
    "Ready step of each Strategy phase: each player selects operatives equal to the turning point number (or as many as possible), none with a below-normal APL; each loses 1 APL until the end of its next activation.",
    "When an operative is activated, roll D6: 1-2 = -1\" Move; 3-4 = no change; 5+ = +1\" Move, until the end of that activation.",
    "End of Set Up Operatives: each player randomly removes one of their operatives. In the 2nd Strategy phase Ready step (initiative player first) set it back up within 6\" of your edge or within 3\" of a friendly operative, not within control range of an enemy.",
    "When an operative is incapacitated, before it is removed roll D6 for each other operative within 2\"; on 4+ that operative takes 2 damage.",
    "Ready step of the 2nd Strategy phase: D3 damage to each operative with a breach point in control range (rolled separately), then open every breach point.",
    "Each time an operative ends a Dash, roll D6; on a 1 it gains a Web token until the end of its next activation. An operative with a Web token cannot take actions that move it.",
    "When an operative is activated while contesting an objective marker, roll D6; on 1-2, until the start of its next activation treat its APL as 1 lower for marker control (this is not an APL stat change, so it stacks)."
  )
)
