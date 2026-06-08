# =====================================================================
# Ctesiphus Expedition - campaign manager (Shiny, mobile-first)
# ---------------------------------------------------------------------
# Entry point. Sources the functional engine + persistence bridge, then
# serves three flows on a phone-friendly bslib layout:
#   - Pre-game  (read):  your hex, its location + condition rules, killzone
#   - Post-game (write): submit your battle result, then a campaign action
#   - Facilitator:       resolve the current phase, advance the round
#
# DB credentials come from env vars (CTES_DB/HOST/PORT/USER/PASS). If the
# database is unreachable the app still launches and says so, rather than
# failing - the persistence seam is the untested part of the stack.
# =====================================================================

library(shiny)
library(bslib)
library(dplyr)

for (f in c("dice.R","hexmap.R","exploration_tables.R","explore.R","actions.R",
            "movement.R","battle.R","threat.R","orchestrator.R","map_selection.R","persist.R"))
  source(file.path("R", f), local = FALSE)

CAMPAIGN_ID <- as.integer(Sys.getenv("CTES_CAMPAIGN", "1"))

# rule text for a code, from the paraphrased mechanics in exploration_tables.R
rule_for <- function(code) {
  if (is.null(code) || is.na(code)) return("Unexplored.")
  for (tb in list(surface_location_table, tomb_location_table)) {
    hit <- tb[tb$code == code, ]; if (nrow(hit)) return(paste0(hit$name[1], " - ", hit$rules[1]))
  }
  for (tb in list(surface_condition_table, tomb_condition_table)) {
    hit <- tb[tb$code == code, ]; if (nrow(hit)) return(paste0(hit$name[1], " - ", hit$rules[1]))
  }
  code
}
killzone_for <- function(type) if (identical(type, "tomb"))
  "Tomb World / close-quarters killzone." else "Volkus (open) killzone."

# ---- theme ------------------------------------------------------------------
theme <- bs_theme(
  version = 5, bg = "#0b0e0d", fg = "#cfe8d8",
  primary = "#36e0a0", secondary = "#1c2622", warning = "#e0a44d",
  base_font   = font_google("IBM Plex Sans"),
  heading_font = font_google("Chakra Petch"),
  "border-radius" = "0.15rem"
) |>
  bs_add_rules("
    body { background:
      radial-gradient(1200px 600px at 50% -10%, #14201b 0%, #0b0e0d 60%); }
    .card { border: 1px solid #1f2d27; box-shadow: 0 0 0 1px #0e1512 inset; }
    .threat { letter-spacing:.08em; text-transform:uppercase; color:#e0a44d; }
    .hex-tag { font-family:'Chakra Petch'; font-size:1.6rem; color:#36e0a0; }
  ")

# ---- UI ---------------------------------------------------------------------
ui <- page_navbar(
  title = "CTESIPHUS EXPEDITION", theme = theme, fillable = TRUE,
  sidebar = sidebar(
    open = "closed", width = 260,
    selectInput("team", "Your kill team", choices = NULL),
    uiOutput("cursor_badge")
  ),

  nav_panel("Pre-game", icon = icon("eye"),
    card(card_header("Where you stand"),
      div(class = "hex-tag", textOutput("hex_label", inline = TRUE)),
      uiOutput("killzone")),
    card(card_header("Location rule"), textOutput("loc_rule")),
    card(card_header("Condition rule"), textOutput("cond_rule"))
  ),

  nav_panel("Post-game", icon = icon("dice"),
    card(card_header("Report your battle"),
      radioButtons("result", NULL,
        c("Win" = "win","Draw" = "draw","Loss" = "loss","Bye" = "bye"), inline = TRUE),
      numericInput("ops", "Enemy operatives incapacitated (weighted)", 0, min = 0),
      actionButton("submit_battle", "Submit result", class = "btn-primary w-100")),
    card(card_header("Campaign action"),
      selectInput("action", NULL,
        choices = c("Scout" = "scout", "Resupply" = "resupply", "Search" = "search",
                    "Encamp" = "encamp", "Demolish" = "demolish")),
      uiOutput("action_args"),
      actionButton("submit_action", "Submit action", class = "btn-primary w-100")),
    card(card_header("Activity"), verbatimTextOutput("player_log"))
  ),

  nav_panel("Facilitator", icon = icon("gauge"),
    card(card_header("Round control"),
      uiOutput("phase_state"),
      layout_columns(
        actionButton("resolve", "Resolve current phase", class = "btn-warning"),
        actionButton("advance", "Advance phase", class = "btn-primary"))),
    card(card_header("Resolution log"), verbatimTextOutput("facilitator_log"))
  )
)

# ---- server -----------------------------------------------------------------
server <- function(input, output, session) {
  rv <- reactiveValues(board = NULL, log = character(0), err = NULL)

  refresh <- function() {
    tryCatch({
      con <- db_connect(); on.exit(DBI::dbDisconnect(con))
      rv$board <- load_board(con, CAMPAIGN_ID); rv$err <- NULL
    }, error = function(e) rv$err <- conditionMessage(e))
  }
  observeEvent(TRUE, refresh(), once = TRUE)

  observe({
    req(rv$board)
    updateSelectInput(session, "team", choices = names(rv$board$teams))
  })

  output$cursor_badge <- renderUI({
    if (!is.null(rv$err)) return(div(class = "threat", "DB offline"))
    req(rv$board)
    div(class = "threat",
        sprintf("Round %s - %s (%s)", rv$board$cursor$round,
                rv$board$cursor$phase, rv$board$cursor$status),
        br(), sprintf("Threat %d / %d", rv$board$threat, rv$board$max_threat))
  })

  team <- reactive({ req(rv$board, input$team); rv$board$teams[[input$team]] })
  hexrow <- reactive({ b <- rv$board; b$map[b$map$hex_number == team()$hex, ] })

  output$hex_label <- renderText(sprintf("HEX %s", team()$hex))
  output$killzone  <- renderUI(div(class = "text-secondary", killzone_for(hexrow()$type)))
  output$loc_rule  <- renderText(rule_for(hexrow()$location_code))
  output$cond_rule <- renderText(rule_for(hexrow()$condition_code))

  output$action_args <- renderUI({
    switch(input$action,
      scout = tagList(
        sliderInput("scout_sp", "SP to spend (= range)", 1, 3, 1),
        selectInput("scout_target", "Target hex", choices = NULL)),
      demolish = selectInput("demolish_target", "Opponent in this hex", choices = NULL),
      NULL)
  })

  observeEvent(input$submit_battle, {
    tryCatch({
      con <- db_connect(); on.exit(DBI::dbDisconnect(con))
      rid <- DBI::dbGetQuery(con,
        "SELECT round_id FROM rounds WHERE campaign_id=$1 ORDER BY week_number DESC LIMIT 1",
        params = list(CAMPAIGN_ID))$round_id
      DBI::dbExecute(con,
        "INSERT INTO battles (round_id, team_id, result, operatives_incap)
         VALUES ($1,$2,$3,$4) ON CONFLICT (round_id, team_id) DO UPDATE
           SET result=EXCLUDED.result, operatives_incap=EXCLUDED.operatives_incap",
        params = list(rid, as.integer(input$team), input$result, input$ops))
      rv$log <- c(rv$log, sprintf("Battle result '%s' recorded.", input$result))
    }, error = function(e) rv$log <- c(rv$log, paste("Error:", conditionMessage(e))))
  })

  output$player_log <- renderText(paste(rev(tail(rv$log, 12)), collapse = "\n"))

  output$phase_state <- renderUI({
    if (!is.null(rv$err)) return(div(class="threat", paste("DB offline:", rv$err)))
    req(rv$board)
    tagList(
      div(class = "hex-tag", toupper(rv$board$cursor$phase)),
      div(class = "text-secondary",
          sprintf("Round %s, status %s", rv$board$cursor$round, rv$board$cursor$status)))
  })

  observeEvent(input$resolve, {
    req(rv$board)
    b <- rv$board; phase <- b$cursor$phase
    out <- switch(phase,
      movement = resolve_movement_phase(b, list()),
      battle   = resolve_battle_phase(b, list()),
      threat   = resolve_threat_phase(b),
      list(board = b, log = sprintf("Phase '%s' resolved via its own submissions.", phase)))
    rv$board <- out$board
    rv$log <- c(rv$log, out$log)
    tryCatch({
      con <- db_connect(); on.exit(DBI::dbDisconnect(con))
      save_board(con, CAMPAIGN_ID, out$board, log = out$log, phase = phase)
    }, error = function(e) rv$log <- c(rv$log, paste("Persist error:", conditionMessage(e))))
  })

  output$facilitator_log <- renderText(paste(rev(tail(rv$log, 20)), collapse = "\n"))
}

shinyApp(ui, server)
