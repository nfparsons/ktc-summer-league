-- =====================================================================
-- Ctesiphus Expedition — campaign manager schema (PostgreSQL)
-- ---------------------------------------------------------------------
-- Shared persistent state only. All logic (rules engine, sf map render,
-- Shiny UI) lives in R and reads/writes these tables.
--
-- Design:
--   * Surrogate integer keys with natural UNIQUE constraints.
--   * Mutable current-state tables + append-only event_log (audit, not
--     full event-sourcing).
--   * Objective standings (Warlord / Pioneer / Explorer / Trooper /
--     Warrior / Headhunter) are NOT stored; derived at campaign end from
--     battles / movements / actions / event_log (one source of truth).
--   * One-off campaign tokens live in campaign_state as key/value JSONB.
--   * Round/phase cursor lives on `campaign` (current_round +
--     current_phase + phase_status); `rounds` is the per-week ledger
--     (active while closed_at IS NULL). The four phases cycle
--     movement -> battle -> action -> threat each week.
-- =====================================================================

-- ---------- enumerated types -----------------------------------------
CREATE TYPE hex_type      AS ENUM ('surface', 'tomb', 'blocked');
CREATE TYPE camp_kind     AS ENUM ('base', 'camp');
CREATE TYPE move_action   AS ENUM ('manoeuvre', 'regroup', 'hold');
CREATE TYPE battle_result AS ENUM ('win', 'draw', 'loss', 'bye');
CREATE TYPE action_type   AS ENUM ('scout', 'resupply', 'search', 'encamp', 'demolish');
CREATE TYPE phase_name    AS ENUM ('setup', 'movement', 'battle', 'action', 'threat');
CREATE TYPE phase_status  AS ENUM ('open', 'resolved');

-- ---------- one row per campaign / season ----------------------------
-- The live round/phase cursor lives here. current_round 0 + current_phase
-- 'setup' is the pre-game state (board draw, base placement).
CREATE TABLE campaign (
    campaign_id    integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    season_name    text         NOT NULL,
    map_id         integer      NOT NULL,            -- which printed map (pp. 68-72)
    max_threat     integer      NOT NULL DEFAULT 7,  -- campaign ends when reached
    current_threat integer      NOT NULL DEFAULT 1,
    current_round  integer      NOT NULL DEFAULT 0,
    current_phase  phase_name   NOT NULL DEFAULT 'setup',
    phase_status   phase_status NOT NULL DEFAULT 'open',
    signup_open    boolean      NOT NULL DEFAULT true,
    created_at     timestamptz  NOT NULL DEFAULT now()
);

-- ---------- the board ------------------------------------------------
-- `type` is topology (surface/tomb/blocked at seed). `blocked` is the
-- *current* status, toggled in play (Tectonic Fissure SL21 permanent,
-- Transtechnic Fulcrum TL25 temporary, Doomsday Vault TL35). Pre-printed
-- blocked hexes seed with type='blocked' AND blocked=true.
CREATE TABLE hexes (
    hex_uid        integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    campaign_id    integer  NOT NULL REFERENCES campaign,
    hex_number     integer  NOT NULL,               -- printed number on the map
    type           hex_type NOT NULL,
    coord_q        integer  NOT NULL,               -- axial coords: adjacency + sf render
    coord_r        integer  NOT NULL,
    explored       boolean  NOT NULL DEFAULT false,
    blocked        boolean  NOT NULL DEFAULT false,
    location_code  text,                             -- e.g. 'SL22','TL31'; null until explored
    condition_code text,                             -- e.g. 'SC21','TC11'
    UNIQUE (campaign_id, hex_number)
);

-- ---------- depleting per-location resources -------------------------
-- Abandoned Camp (SL22) supply, Crashed Ship (SL31) intel, Resource
-- Stockpile (SL32) supply, Crucible of Whispers (TL23) campaign, Doomsday
-- Vault (TL35) search_cost. resource_type is free text; documented set:
-- 'supply' | 'intel' | 'campaign' | 'search_cost'.
CREATE TABLE hex_resources (
    resource_id   integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    hex_uid       integer NOT NULL REFERENCES hexes,
    resource_type text    NOT NULL,
    remaining     integer NOT NULL CHECK (remaining >= 0),
    UNIQUE (hex_uid, resource_type)
);

-- ---------- players / kill teams -------------------------------------
CREATE TABLE kill_teams (
    team_id         integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    campaign_id     integer NOT NULL REFERENCES campaign,
    player_name     text    NOT NULL,
    team_name       text    NOT NULL,
    login_handle    text    UNIQUE,                 -- optional; ties to auth if used
    base_hex_uid    integer NOT NULL REFERENCES hexes,
    current_hex_uid integer NOT NULL REFERENCES hexes,
    campaign_points integer NOT NULL DEFAULT 0,
    supply_points   integer NOT NULL DEFAULT 10
                    CHECK (supply_points BETWEEN 0 AND 10),
    UNIQUE (campaign_id, team_name),
    UNIQUE (campaign_id, base_hex_uid)               -- one team per base hex
);

-- ---------- bases and camps ------------------------------------------
-- Base is permanent (kind='base'); max 2 active camps per team enforced
-- in app logic. Demolish sets active=false (preserves the audit trail).
CREATE TABLE camps (
    camp_id     integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    team_id     integer   NOT NULL REFERENCES kill_teams,
    hex_uid     integer   NOT NULL REFERENCES hexes,
    kind        camp_kind NOT NULL,
    active      boolean   NOT NULL DEFAULT true,
    created_at  timestamptz NOT NULL DEFAULT now()
);

-- ---------- weekly rounds (ledger) -----------------------------------
-- One row per week. Active while closed_at IS NULL; the live phase cursor
-- is on `campaign`, not here.
CREATE TABLE rounds (
    round_id        integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    campaign_id     integer     NOT NULL REFERENCES campaign,
    week_number     integer     NOT NULL,
    threat_at_start integer     NOT NULL,
    opened_at       timestamptz NOT NULL DEFAULT now(),
    closed_at       timestamptz,
    UNIQUE (campaign_id, week_number)
);

-- ---------- movement declarations ------------------------------------
-- `path` is the ordered list of hex_numbers ENTERED (excludes the start
-- hex), so SP cost for a manoeuvre = array_length(path).
CREATE TABLE movements (
    movement_id    integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    round_id       integer     NOT NULL REFERENCES rounds,
    team_id        integer     NOT NULL REFERENCES kill_teams,
    action         move_action NOT NULL,
    path           integer[]   NOT NULL DEFAULT '{}',
    sp_spent       integer     NOT NULL DEFAULT 0,
    resolved_order integer,
    resolved_at    timestamptz,
    UNIQUE (round_id, team_id)
);

-- ---------- battle results (entered async during the week) -----------
-- hex_uid (condition used) is nullable: a no-show loss or a bye has no
-- battlefield. UNIQUE(round_id, team_id): one league game per team/week.
CREATE TABLE battles (
    battle_id        integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    round_id         integer       NOT NULL REFERENCES rounds,
    team_id          integer       NOT NULL REFERENCES kill_teams,
    opponent_team_id integer       REFERENCES kill_teams,
    opponent_label   text,
    hex_uid          integer       REFERENCES hexes,
    result           battle_result NOT NULL,
    operatives_incap integer       NOT NULL DEFAULT 0,   -- Headhunter weighted count (p.56)
    cp_gained        integer       NOT NULL DEFAULT 0,
    sp_gained        integer       NOT NULL DEFAULT 0,
    entered_at       timestamptz   NOT NULL DEFAULT now(),
    UNIQUE (round_id, team_id)
);

-- ---------- campaign (Action-phase) actions --------------------------
-- Resolved at week close in won -> drew -> lost order, priority inside tier.
CREATE TABLE actions (
    action_id      integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    round_id       integer     NOT NULL REFERENCES rounds,
    team_id        integer     NOT NULL REFERENCES kill_teams,
    action_type    action_type NOT NULL,
    target_hex_uid integer     REFERENCES hexes,
    sp_cost        integer     NOT NULL DEFAULT 0,
    params         jsonb       NOT NULL DEFAULT '{}',
    resolved_order integer,
    resolved_at    timestamptz,
    result         jsonb       NOT NULL DEFAULT '{}',
    UNIQUE (round_id, team_id)
);

-- ---------- one-off campaign-level tokens / flags --------------------
CREATE TABLE campaign_state (
    campaign_id integer NOT NULL REFERENCES campaign,
    key         text    NOT NULL,
    value       jsonb   NOT NULL,
    PRIMARY KEY (campaign_id, key)
);

-- ---------- append-only audit log ------------------------------------
CREATE TABLE event_log (
    event_id    integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    campaign_id integer     NOT NULL REFERENCES campaign,
    round_id    integer     REFERENCES rounds,
    team_id     integer     REFERENCES kill_teams,
    phase       phase_name,
    event_type  text        NOT NULL,
    payload     jsonb       NOT NULL DEFAULT '{}',
    created_at  timestamptz NOT NULL DEFAULT now()
);

-- ---------- editable rule prose (facilitator-populated) --------------
CREATE TABLE rule_text (
    rule_code    text PRIMARY KEY,
    rule_kind    text NOT NULL CHECK (rule_kind IN
                   ('surface_location','tomb_location',
                    'surface_condition','tomb_condition')),
    title        text,
    flavour_text text,
    rules_text   text,
    updated_at   timestamptz NOT NULL DEFAULT now(),
    updated_by   text
);

-- ---------- kill team catalogue (campaign-independent reference) ------
-- Master roster of selectable kill teams; seeded from inst/kill_teams.csv.
-- `active` toggles a team out of the picker without deleting it.
CREATE TABLE kill_team_catalogue (
    name    text PRIMARY KEY,
    faction text,                                    -- optional grouping
    active  boolean NOT NULL DEFAULT true
);

-- ---------- helpful indexes ------------------------------------------
CREATE INDEX idx_hexes_campaign    ON hexes (campaign_id);
CREATE INDEX idx_teams_campaign    ON kill_teams (campaign_id);
CREATE INDEX idx_camps_team        ON camps (team_id) WHERE active;
CREATE INDEX idx_battles_round     ON battles (round_id);
CREATE INDEX idx_movements_round   ON movements (round_id);
CREATE INDEX idx_actions_round     ON actions (round_id);
CREATE INDEX idx_eventlog_campaign ON event_log (campaign_id, created_at);
