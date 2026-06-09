# =====================================================================
# Ctesiphus Expedition - campaign manager (Shiny, mobile-first)
# ---------------------------------------------------------------------
# GUI-driven; no R console required. The current board is shown on every
# post-creation tab. Players reach a campaign by join code or QR (a code
# embedded in the app URL as ?join=CODE), not by picking from a list.
# DB credentials come from the host's env vars (CTES_*).
# =====================================================================

library(shiny); library(bslib); library(dplyr); library(readr); library(jsonlite)

for (f in c("dice.R","hexmap.R","exploration_tables.R","explore.R","actions.R",
            "movement.R","battle.R","threat.R","orchestrator.R","map_selection.R",
            "persist.R","lifecycle.R","board_plot.R"))
  source(file.path("R", f), local = FALSE)
source("setup_campaign.R", local = FALSE)

rule_for <- function(code) {
  if (is.null(code) || is.na(code)) return("Unexplored.")
  for (tb in list(surface_location_table, tomb_location_table,
                  surface_condition_table, tomb_condition_table)) {
    hit <- tb[tb$code == code, ]; if (nrow(hit)) return(paste0(hit$name[1], " - ", hit$rules[1]))
  }
  code
}
killzone_for <- function(type) if (identical(type, "tomb"))
  "Tomb World / close-quarters killzone." else "Volkus (open) killzone."
surface_hexes_of <- function(map_id) {
  m <- readr::read_csv(file.path("inst", "maps", sprintf("map%d.csv", map_id)),
                       show_col_types = FALSE)
  sort(m$hex_number[m$type == "surface"])
}
board_text <- function(board) {
  m <- board$map; surf <- sort(m$hex_number[m$type == "surface"])
  taken  <- vapply(board$teams, function(t) as.integer(t$base), integer(1))
  owners <- vapply(board$teams, function(t) t$name %||% "", character(1))
  open   <- setdiff(surf, taken)
  paste0("Open surface hexes: ", if (length(open)) paste(open, collapse = ", ") else "none",
         "\nTaken bases: ",
         if (length(taken)) paste(sprintf("hex %d = %s", taken, owners), collapse = "; ") else "none")
}
board_card <- function(id) card(card_header("Current board - bases outlined in orange, open surface hexes"),
  plotOutput(paste0(id, "_board"), height = "480px"),
  verbatimTextOutput(paste0(id, "_summary")))

theme <- bs_theme(
  version = 5, bg = "#16161a", fg = "#e8e6e2",
  primary = "#e0571c", secondary = "#2a2a2d", warning = "#e0571c",
  base_font = font_google("IBM Plex Sans"), heading_font = font_google("Chakra Petch"),
  "border-radius" = "0.15rem") |>
  bs_add_rules("
    body { background: radial-gradient(1200px 600px at 50% -10%, #232327 0%, #141417 60%); }
    .card { border: 1px solid #2c2c31; box-shadow: 0 0 0 1px #0e0e12 inset; }
    .threat { letter-spacing:.08em; text-transform:uppercase; color:#e0571c; }
    .hex-tag { font-family:'Chakra Petch'; font-size:1.6rem; color:#e0571c; }
    .code-tag { font-family:'Chakra Petch'; font-size:2rem; letter-spacing:.2em; color:#e0571c; }")

ui <- page_navbar(
  title = "KTC Campaign Tool", theme = theme, fillable = TRUE, id = "nav",
  sidebar = sidebar(open = "closed", width = 280,
    selectInput("campaign", "Campaign (facilitator)", choices = NULL),
    selectInput("team", "Your kill team", choices = NULL),
    uiOutput("cursor_badge")),

  nav_panel("Setup", icon = icon("flag-checkered"),
    card(card_header("New campaign (facilitator)"),
      textInput("season", "Season name", placeholder = "e.g. Summer 2026"),
      radioButtons("map_mode", "Board",
        c("Draw weighted by player count" = "draw", "Choose a specific map" = "choose")),
      conditionalPanel("input.map_mode == 'draw'",
        numericInput("draw_players", "Expected players", 4, 2, 6),
        numericInput("draw_seed", "Random seed (auditable draw)", 42, 1, 99999)),
      conditionalPanel("input.map_mode == 'choose'",
        selectInput("choose_map", "Map", c(
          "Map 1 - 26 hexes" = 1, "Map 2 - 33 hexes" = 2, "Map 3 - 36 hexes" = 3,
          "Map 4 - 37 hexes" = 4, "Map 5 - 52 hexes" = 5))),
      actionButton("set_map", "Set board", class = "btn-secondary w-100"),
      uiOutput("map_info"),
      textAreaInput("roster", "Available kill teams (one per line)", rows = 6),
      actionButton("create_campaign", "Create campaign", class = "btn-primary w-100 mt-2"),
      uiOutput("begin_btn"),
      verbatimTextOutput("setup_status")),
    card(card_header("Share with players"),
      div(class = "text-secondary", "Players join with this code or by scanning the QR."),
      div(class = "code-tag", textOutput("setup_code_txt", inline = TRUE)),
      plotOutput("setup_qr", height = "220px"))),

  nav_panel("Manage", icon = icon("sliders"),
    card(card_header("Campaign settings"),
      selectInput("mng_campaign", "Campaign", choices = NULL),
      textInput("mng_season", "Season name"),
      numericInput("mng_maxthreat", "Max threat (campaign ends when reached)", 7, 2, 12),
      checkboxInput("mng_signup", "Sign-ups open", TRUE),
      actionButton("mng_save", "Save changes", class = "btn-primary w-100"),
      div(class = "text-secondary mt-2", textOutput("mng_status"))),
    card(card_header("Sign-up link & QR"),
      div(class = "text-secondary", "Share this link or QR with players:"),
      verbatimTextOutput("mng_link"),
      div(class = "code-tag", textOutput("mng_code_txt", inline = TRUE)),
      plotOutput("mng_qr", height = "220px"),
      actionButton("mng_regen", "Regenerate code", class = "btn-secondary w-100")),
    card(card_header("Danger zone"),
      div(class = "text-secondary", "Permanently delete this campaign and all its data."),
      actionButton("mng_delete", "Delete campaign", class = "btn-danger w-100"))),

  nav_panel("Join", icon = icon("user-plus"),
    card(card_header("Find your campaign"),
      textInput("join_code", "Join code (from your facilitator / QR)"),
      actionButton("find_campaign", "Find campaign", class = "btn-secondary w-100"),
      div(class = "text-secondary mt-2", textOutput("join_campaign_label"))),
    board_card("join"),
    card(card_header("Sign up"),
      textInput("join_player", "Your name"),
      selectInput("join_team", "Your kill team", choices = NULL),
      textInput("join_handle", "Handle (optional)"),
      selectInput("join_base", "Starting base (surface hex)", choices = NULL),
      actionButton("join_btn", "Join campaign", class = "btn-primary w-100"),
      verbatimTextOutput("join_status"))),

  nav_panel("Pre-game", icon = icon("eye"),
    board_card("pre"),
    card(card_header("Where you stand"),
      div(class = "hex-tag", textOutput("hex_label", inline = TRUE)), uiOutput("killzone")),
    card(card_header("Location rule"), textOutput("loc_rule")),
    card(card_header("Condition rule"), textOutput("cond_rule"))),

  nav_panel("Post-game", icon = icon("dice"),
    board_card("post"),
    card(card_header("Report your battle"),
      radioButtons("result", NULL,
        c("Win"="win","Draw"="draw","Loss"="loss","Bye"="bye"), inline = TRUE),
      numericInput("ops", "Enemy operatives incapacitated (weighted)", 0, min = 0),
      actionButton("submit_battle", "Submit result", class = "btn-primary w-100")),
    card(card_header("Activity"), verbatimTextOutput("player_log"))),

  nav_panel("Facilitator", icon = icon("gauge"),
    board_card("fac"),
    card(card_header("Round control"), uiOutput("phase_state"),
      layout_columns(
        actionButton("resolve", "Resolve current phase", class = "btn-warning"),
        actionButton("advance", "Advance / begin", class = "btn-primary"))),
    card(card_header("Resolution log"), verbatimTextOutput("facilitator_log")))
)

server <- function(input, output, session) {
  rv <- reactiveValues(board = NULL, log = character(0), err = NULL, db_ok = NA,
                       schema_ready = FALSE, campaign_id = NULL,
                       setup_map = NULL, setup_msg = "", join_msg = "",
                       mng_msg = "", join_tick = 0)

  withCon <- function(fn) {
    res <- db_connect()
    if (!res$ok) {                 # genuine connection failure -> the DB really is unreachable
      rv$db_ok <- FALSE
      rv$err   <- res$db_response
      return(NULL)
    }
    rv$db_ok <- TRUE               # we reached the database; anything below is an OPERATION error
    con <- res$con
    on.exit(DBI::dbDisconnect(con))
    if (!isTRUE(rv$schema_ready)) {                  # once per session, on the first live connection:
      ok <- tryCatch({ ensure_schema(con); TRUE },   # bring the schema up to date before any query runs
                     error = function(e) { rv$err <- conditionMessage(e); FALSE })
      if (!ok) return(NULL)                          # migration failed -> surface it, don't proceed
      rv$schema_ready <- TRUE
    }
    tryCatch(fn(con), error = function(e) { rv$err <- conditionMessage(e); NULL })
  }
  refresh <- function() { if (!is.null(rv$campaign_id))
    withCon(function(con) { rv$board <- load_board(con, rv$campaign_id); rv$err <- NULL }) }
  load_campaigns <- function() withCon(function(con)
    DBI::dbGetQuery(con, "SELECT campaign_id, season_name FROM campaign ORDER BY campaign_id"))
  set_campaign_choices <- function(selected = NULL) {
    cs <- load_campaigns()
    if (!is.null(cs) && nrow(cs)) {
      ch <- setNames(cs$campaign_id, sprintf("%d - %s", cs$campaign_id, cs$season_name))
      updateSelectInput(session, "campaign",     choices = ch, selected = selected)
      updateSelectInput(session, "mng_campaign", choices = ch, selected = selected)
    }
    cs
  }
  # single path for switching campaigns; keeps both selectors in sync, no loop
  select_campaign <- function(cid) {
    cid <- suppressWarnings(as.integer(cid))
    if (!length(cid) || is.na(cid)) return()
    if (!is.null(rv$campaign_id) && identical(rv$campaign_id, cid)) return()
    rv$campaign_id <- cid; rv$join_tick <- rv$join_tick + 1; refresh()
    updateSelectInput(session, "campaign",     selected = cid)
    updateSelectInput(session, "mng_campaign", selected = cid)
  }
  base_url <- reactive({ cd <- session$clientData
    port <- if (!is.null(cd$url_port) && nzchar(cd$url_port)) paste0(":", cd$url_port) else ""
    paste0(cd$url_protocol, "//", cd$url_hostname, port, cd$url_pathname) })

  # startup: campaigns, catalogue auto-seed, roster pre-fill
  observeEvent(TRUE, {
    cs <- set_campaign_choices()
    if (!is.null(cs) && nrow(cs)) { rv$campaign_id <- cs$campaign_id[1]; refresh() }
    else nav_select("nav", "Setup")
    cat_names <- withCon(function(con) {
      ensure_catalogue(con)
      if (!length(catalogue_teams(con))) try(load_catalogue(con, "inst/kill_teams.csv"), silent = TRUE)
      catalogue_teams(con) })
    if (!is.null(cat_names) && length(cat_names))
      updateTextAreaInput(session, "roster", value = paste(cat_names, collapse = "\n"))
  }, once = TRUE)

  # QR deep-link: ?join=CODE routes a scanner straight to that campaign's Join
  observeEvent(session$clientData$url_search, {
    q <- parseQueryString(session$clientData$url_search)
    code <- q[["join"]]
    if (!is.null(code) && nzchar(code)) {
      cid <- withCon(function(con) campaign_by_code(con, code))
      if (!is.null(cid) && !is.na(cid)) {
        rv$campaign_id <- as.integer(cid); set_campaign_choices(selected = rv$campaign_id)
        rv$join_tick <- rv$join_tick + 1; refresh(); nav_select("nav", "Join")
      }
    }
  }, once = TRUE)

  observeEvent(input$reconnect, {
    rv$mng_msg <- "Waking the database..."
    cs <- set_campaign_choices(selected = rv$campaign_id)
    if (!is.null(cs) && nrow(cs)) {
      if (is.null(rv$campaign_id)) rv$campaign_id <- cs$campaign_id[1]
      refresh()
    }
    rv$mng_msg <- if (is.null(rv$err)) "Database is awake." else paste("Still offline:", rv$err)
  })
  observeEvent(input$campaign,     select_campaign(input$campaign),     ignoreInit = TRUE)
  observeEvent(input$mng_campaign, select_campaign(input$mng_campaign), ignoreInit = TRUE)
  observe({ req(rv$board)
    ids  <- names(rv$board$teams)
    labs <- vapply(rv$board$teams, function(t) t$name %||% ids, character(1))
    updateSelectInput(session, "team", choices = setNames(ids, labs)) })

  output$cursor_badge <- renderUI({
    if (isFALSE(rv$db_ok)) return(tagList(            # only when db_connect() itself failed
      div(class = "threat", "DB offline"),
      div(class = "text-secondary", style = "font-size:.8rem",
          rv$err %||% "Could not reach the database."),
      actionButton("reconnect", "Wake database", class = "btn-warning btn-sm w-100 mt-2")))
    if (!is.null(rv$err)) return(                     # DB reachable, but an operation errored
      div(class = "threat", paste("Error:", rv$err)))
    req(rv$board)
    div(class = "threat",
        sprintf("Round %s - %s (%s)", rv$board$cursor$round, rv$board$cursor$phase,
                rv$board$cursor$status),
        br(), sprintf("Threat %d / %d", rv$board$threat, rv$board$max_threat))
  })

  # board view on every post-creation tab
  for (k in c("join","pre","post","fac")) local({ key <- k
    output[[paste0(key, "_board")]]   <- renderPlot({ req(rv$board); plot_board(rv$board$map, rv$board$teams) })
    output[[paste0(key, "_summary")]] <- renderText({ req(rv$board); board_text(rv$board) })
  })

  # QR + code display (Setup and Facilitator share the active campaign's code)
  qr_render <- function(id) output[[id]] <- renderPlot({
    req(rv$board); req(rv$board$join_code)
    if (!requireNamespace("qrcode", quietly = TRUE)) { plot.new(); text(.5,.5,"Install 'qrcode' for QR codes"); return() }
    plot(qrcode::qr_code(paste0(base_url(), "?join=", rv$board$join_code)))
  }, width = 220, height = 220)   # fixed square device; avoids startPNG 'invalid width' on a hidden/unmeasured tab
  qr_render("setup_qr"); qr_render("mng_qr")
  output$setup_code_txt <- renderText(if (!is.null(rv$board)) rv$board$join_code %||% "" else "")
  output$mng_code_txt   <- renderText(if (!is.null(rv$board)) rv$board$join_code %||% "" else "")
  output$mng_link <- renderText(if (!is.null(rv$board) && !is.null(rv$board$join_code))
    paste0(base_url(), "?join=", rv$board$join_code) else "Create or select a campaign first.")
  output$mng_status <- renderText(rv$mng_msg)

  # ---- Setup ----------------------------------------------------------------
  observeEvent(input$set_map, {
    mid <- if (input$map_mode == "choose") as.integer(input$choose_map) else {
      set.seed(input$draw_seed); as.integer(choose_map(input$draw_players)) }
    rv$setup_map <- mid; rv$setup_msg <- sprintf("Board set to map %d.", mid)
  })
  output$map_info <- renderUI({ req(rv$setup_map)
    div(class = "text-secondary",
        sprintf("Map %d - %d surface hexes available as bases.",
                rv$setup_map, length(surface_hexes_of(rv$setup_map)))) })
  output$begin_btn <- renderUI(if (!is.null(rv$campaign_id))
    actionButton("advance2", "Begin campaign (open week 1)", class = "btn-success w-100 mt-2"))

  observeEvent(input$create_campaign, {
    if (!nzchar(input$season)) { rv$setup_msg <- "Enter a season name."; return() }
    if (is.null(rv$setup_map)) { rv$setup_msg <- "Set a board first."; return() }
    roster <- trimws(strsplit(input$roster, "\n")[[1]]); roster <- roster[nzchar(roster)]
    if (!length(roster)) { rv$setup_msg <- "List at least one available kill team."; return() }
    cid <- withCon(function(con)
      create_campaign(con, season_name = input$season, available_teams = roster, map_id = rv$setup_map))
    if (is.null(cid)) { rv$setup_msg <- paste("Create failed:", rv$err); return() }
    rv$campaign_id <- as.integer(cid); set_campaign_choices(selected = rv$campaign_id)
    rv$join_tick <- rv$join_tick + 1; refresh()
    rv$setup_msg <- sprintf("Created campaign %d (map %d, %d teams). Share the join code; click Begin when ready.",
                            cid, rv$setup_map, length(roster))
  })
  output$setup_status <- renderText(rv$setup_msg)

  # ---- Manage ---------------------------------------------------------------
  observe({ req(rv$board)
    updateTextInput(session, "mng_season", value = rv$board$season %||% "")
    updateNumericInput(session, "mng_maxthreat", value = rv$board$max_threat)
    updateCheckboxInput(session, "mng_signup", value = isTRUE(rv$board$signup_open)) })
  observeEvent(input$mng_save, { req(rv$campaign_id)
    ok <- withCon(function(con) update_campaign(con, rv$campaign_id,
      season_name = input$mng_season, max_threat = input$mng_maxthreat,
      signup_open = input$mng_signup))
    set_campaign_choices(selected = rv$campaign_id); refresh()
    rv$mng_msg <- if (is.null(ok)) paste("Save failed:", rv$err) else "Saved." })
  observeEvent(input$mng_regen, { req(rv$campaign_id)
    code <- withCon(function(con) regen_join_code(con, rv$campaign_id)); refresh()
    rv$mng_msg <- if (is.null(code)) paste("Failed:", rv$err) else paste("New join code:", code) })
  observeEvent(input$mng_delete, { req(rv$board)
    showModal(modalDialog(title = "Delete campaign?",
      sprintf("This permanently deletes '%s' and all of its data. This cannot be undone.",
              rv$board$season %||% ""),
      footer = tagList(modalButton("Cancel"),
        actionButton("mng_delete_confirm", "Delete permanently", class = "btn-danger")))) })
  observeEvent(input$mng_delete_confirm, { removeModal(); req(rv$campaign_id)
    ok <- withCon(function(con) delete_campaign(con, rv$campaign_id))
    if (is.null(ok)) { rv$mng_msg <- paste("Delete failed:", rv$err); return() }
    rv$board <- NULL; rv$campaign_id <- NULL
    cs <- set_campaign_choices()
    if (!is.null(cs) && nrow(cs)) { rv$campaign_id <- cs$campaign_id[1]; refresh() }
    else nav_select("nav", "Setup") })

  # ---- Join (code-gated) ----------------------------------------------------
  observeEvent(input$find_campaign, {
    cid <- withCon(function(con) campaign_by_code(con, input$join_code))
    if (is.null(cid) || is.na(cid)) { rv$join_msg <- "No campaign with that code."; return() }
    rv$campaign_id <- as.integer(cid); set_campaign_choices(selected = rv$campaign_id)
    rv$join_tick <- rv$join_tick + 1; refresh(); rv$join_msg <- "Campaign found - sign up below."
  })
  output$join_campaign_label <- renderText(
    if (!is.null(rv$board)) sprintf("Campaign: %s (code %s)", rv$board$season %||% "?",
                                    rv$board$join_code %||% "?") else "Enter a join code to begin.")

  join_opts <- reactive({ rv$join_tick; req(rv$campaign_id)
    withCon(function(con) {
      mid <- DBI::dbGetQuery(con, "SELECT map_id FROM campaign WHERE campaign_id=$1",
                             params = list(rv$campaign_id))$map_id
      r <- DBI::dbGetQuery(con, "SELECT value FROM campaign_state WHERE campaign_id=$1 AND key='available_teams'",
                           params = list(rv$campaign_id))
      roster  <- if (nrow(r)) as.character(jsonlite::fromJSON(r$value)) else character(0)
      claimed <- DBI::dbGetQuery(con, "SELECT team_name FROM kill_teams WHERE campaign_id=$1",
                                 params = list(rv$campaign_id))$team_name
      used    <- DBI::dbGetQuery(con,
        "SELECT h.hex_number FROM kill_teams k JOIN hexes h ON h.hex_uid=k.base_hex_uid WHERE k.campaign_id=$1",
        params = list(rv$campaign_id))$hex_number
      list(teams = setdiff(roster, claimed), bases = setdiff(surface_hexes_of(mid), used))
    })
  })
  observe({ o <- join_opts(); req(o)
    updateSelectInput(session, "join_team", choices = o$teams)
    updateSelectInput(session, "join_base", choices = o$bases) })

  observeEvent(input$join_btn, { req(rv$campaign_id)
    r <- withCon(function(con) join_campaign(con, rv$campaign_id, input$join_player,
                   input$join_team, as.integer(input$join_base), input$join_handle))
    if (is.null(r)) { rv$join_msg <- paste("DB error:", rv$err); return() }
    if (isTRUE(r$ok)) { rv$join_msg <- sprintf("Joined as %s (%s). Welcome aboard.",
                                               input$join_team, input$join_player)
      rv$join_tick <- rv$join_tick + 1; refresh()
      updateTextInput(session, "join_player", value = ""); updateTextInput(session, "join_handle", value = "")
    } else rv$join_msg <- r$msg
  })
  output$join_status <- renderText(rv$join_msg)

  # ---- Pre-game -------------------------------------------------------------
  team   <- reactive({ req(rv$board, input$team); rv$board$teams[[input$team]] })
  hexrow <- reactive({ b <- rv$board; b$map[b$map$hex_number == team()$hex, ] })
  output$hex_label <- renderText(sprintf("HEX %s", team()$hex))
  output$killzone  <- renderUI(div(class = "text-secondary", killzone_for(hexrow()$type)))
  output$loc_rule  <- renderText(rule_for(hexrow()$location_code))
  output$cond_rule <- renderText(rule_for(hexrow()$condition_code))

  # ---- Post-game ------------------------------------------------------------
  observeEvent(input$submit_battle, { req(rv$campaign_id, input$team)
    withCon(function(con) {
      rid <- DBI::dbGetQuery(con,
        "SELECT round_id FROM rounds WHERE campaign_id=$1 ORDER BY week_number DESC LIMIT 1",
        params = list(rv$campaign_id))$round_id
      DBI::dbExecute(con,
        "INSERT INTO battles (round_id, team_id, result, operatives_incap)
         VALUES ($1,$2,$3,$4) ON CONFLICT (round_id, team_id) DO UPDATE
           SET result=EXCLUDED.result, operatives_incap=EXCLUDED.operatives_incap",
        params = list(rid, as.integer(input$team), input$result, input$ops))
      rv$log <- c(rv$log, sprintf("Battle result '%s' recorded.", input$result)) })
  })
  output$player_log <- renderText(paste(rev(tail(rv$log, 12)), collapse = "\n"))

  # ---- Facilitator ----------------------------------------------------------
  output$phase_state <- renderUI({
    if (!is.null(rv$err)) return(div(class="threat", paste("DB:", rv$err)))
    req(rv$board)
    tagList(div(class = "hex-tag", toupper(rv$board$cursor$phase)),
            div(class = "text-secondary",
                sprintf("Round %s, status %s", rv$board$cursor$round, rv$board$cursor$status)))
  })
  observeEvent(input$resolve, { req(rv$board); b <- rv$board; phase <- b$cursor$phase
    out <- switch(phase,
      movement = resolve_movement_phase(b, list()), battle = resolve_battle_phase(b, list()),
      threat = resolve_threat_phase(b),
      list(board = b, log = sprintf("Phase '%s' resolved from its submissions.", phase)))
    rv$board <- out$board; rv$log <- c(rv$log, out$log)
    withCon(function(con) { save_board(con, rv$campaign_id, out$board, log = out$log, phase = phase)
      mark_phase_resolved(con, rv$campaign_id); rv$board$cursor$status <- "resolved" })
  })
  observeEvent(c(input$advance, input$advance2), { req(rv$campaign_id)
    withCon(function(con) { db_advance_phase(con, rv$campaign_id); rv$log <- c(rv$log, "Advanced phase.") })
    refresh()
  }, ignoreInit = TRUE)
  output$facilitator_log <- renderText(paste(rev(tail(rv$log, 20)), collapse = "\n"))
}

shinyApp(ui, server)
