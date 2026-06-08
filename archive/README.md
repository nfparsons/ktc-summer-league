# Ctesiphus Expedition — campaign manager

A digital manager for a Kill Team Summer League run on the Ctesiphus
Expedition rules (Dead Silence pack). A pure-functional R rules engine, a
Postgres persistence bridge, and a mobile-first Shiny UI.

## Architecture

The **rules core** operates on an in-memory `state`/`board` contract with no
database coupling, so it is fully testable in isolation. The **persistence
bridge** (`persist.R`) is the only place that knows about Postgres: it hydrates
a `board` from the schema and writes results + `event_log` back. The **Shiny
app** (`app.R`) is the only place real-time/mobile concerns live.

## Source order

    R/dice.R                 # roll_d3 / roll_d6 / roll_d36 / roll_notation
    R/hexmap.R               # axial geometry, BFS graph distance, hexes_within
    R/exploration_tables.R   # the four D36 tables + paraphrased mechanics
    R/explore.R              # explore_hex: re-roll-on-duplicate draw
    R/actions.R              # Action phase: five actions + location specials
    R/movement.R             # Movement phase: priority, 2-team cap, leave-costs
    R/battle.R               # Battle phase: scoring + no-show defaults
    R/threat.R               # Threat phase: camp/beast/prisoner triggers + raise
    R/orchestrator.R         # action order + board<->per-team-state bridge
    R/map_selection.R        # player-count-weighted board draw at setup
    R/persist.R              # board <-> Postgres; event_log
    inst/sql/schema.sql      # Postgres persistence target
    inst/maps/map[1-5].csv   # digitized boards (hex_number, type, q, r)
    tests/testthat/          # exploration tests
    app.R                    # Shiny entry point

## State of play

All four campaign phases, the orchestrator, the persistence bridge and a Shiny
UI now exist and are parse-clean. What that means honestly:

WORKS (in-memory, logic verified by inspection):
  - Full rules core; Movement / Battle / Action / Threat resolvers
  - Won->drew->lost action order and the Demolish fold-back
  - A pre-existing apply_scout bug (res$resource -> res$init_resource) is fixed
  - Resource tracking extended to all four stores (supply/intel/campaign/search_cost)

NOT YET EXERCISED / still thin:
  - Nothing has run against a live Postgres or a live Shiny server. First
    runtime targets: integer[] binding (camps/path) and jsonb round-trips
    (flags/tokens) in persist.R.
  - The Shiny app wires up the pre-game read, the battle submit, and the
    facilitator resolve/persist loop. The Action submit, scout/demolish target
    pickers, the needs_input prompts (redeclare movement, choose a camp to
    remove, interactive searches), the weekly advance_phase button, and
    shinymanager auth are stubbed or minimal.
  - A handful of threat/location specials remain (the beast and prisoner
    auto-resolve with logged rolls; the prisoner's roam is a random walk).
  - No campaign bootstrap yet (seed campaign row + hexes from a chosen map CSV
    + place bases). That setup script is the first thing to add for testing.

## Running (once a DB exists)

    # env: CTES_DB, CTES_HOST, CTES_PORT, CTES_USER, CTES_PASS, CTES_CAMPAIGN
    psql ... -f inst/sql/schema.sql      # create schema
    # (bootstrap a campaign + board - script TBD)
    R -e "shiny::runApp('.')"            # launch
