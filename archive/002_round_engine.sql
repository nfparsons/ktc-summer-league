-- =====================================================================
-- Ctesiphus Expedition — round engine migration (002)
-- ---------------------------------------------------------------------
-- Adds the campaign round/phase state machine on top of the base schema.
--
-- Model: submission is asynchronous, resolution is gated.
--   * A round advances through four phases: movement -> battle ->
--     action -> threat.
--   * Within a phase, players SUBMIT declarations/results in any order
--     (open status). Submissions are recorded but NOT applied.
--   * The facilitator resolves the phase: the engine computes priority
--     order and applies every submission in that order atomically, then
--     marks the phase resolved and opens the next.
-- =====================================================================

-- --- phase state, held as a single row in campaign_state -------------
-- campaign_state already exists (one row, holds the JSONB token bag).
-- We add the round/phase cursor to it.
ALTER TABLE campaign_state
  ADD COLUMN IF NOT EXISTS current_round  INT  NOT NULL DEFAULT 1,
  ADD COLUMN IF NOT EXISTS current_phase  TEXT NOT NULL DEFAULT 'movement'
      CHECK (current_phase IN ('movement','battle','action','threat')),
  ADD COLUMN IF NOT EXISTS phase_status   TEXT NOT NULL DEFAULT 'open'
      CHECK (phase_status IN ('open','resolved')),
  ADD COLUMN IF NOT EXISTS threat_level   INT  NOT NULL DEFAULT 1;

-- --- movement submissions --------------------------------------------
-- One row per team per round. `path` is the ordered list of hex_uids the
-- team declares moving THROUGH (manoeuvre only); the engine derives the
-- SP cost server-side rather than trusting the client.
CREATE TABLE IF NOT EXISTS movements (
  id             SERIAL PRIMARY KEY,
  round          INT  NOT NULL,
  team_id        INT  NOT NULL REFERENCES teams(team_id),
  action         TEXT NOT NULL CHECK (action IN ('manoeuvre','regroup','hold')),
  path           INT[],                       -- ordered hex_uids (manoeuvre)
  dest_hex       INT  REFERENCES hexes(hex_uid),
  sp_spent       INT,                          -- computed at resolution
  resolved_order INT,                          -- priority position, filled at resolution
  explore_needed BOOLEAN NOT NULL DEFAULT FALSE,
  submitted_at   TIMESTAMPTZ NOT NULL DEFAULT now(),
  resolved_at    TIMESTAMPTZ,
  UNIQUE (round, team_id)
);

-- --- battle results --------------------------------------------------
-- Independent per team; no priority ordering needed to resolve.
-- `result` drives point awards (win -> +1 CP; draw/loss -> +1 SP; bye -> +2 SP).
CREATE TABLE IF NOT EXISTS battles (
  id             SERIAL PRIMARY KEY,
  round          INT  NOT NULL,
  team_id        INT  NOT NULL REFERENCES teams(team_id),
  opponent_id    INT  REFERENCES teams(team_id),  -- NULL for non-campaign opponent or bye
  result         TEXT NOT NULL CHECK (result IN ('win','draw','loss','bye')),
  ops_incap      INT  NOT NULL DEFAULT 0,          -- weighted enemy operatives incapacitated
  submitted_at   TIMESTAMPTZ NOT NULL DEFAULT now(),
  resolved_at    TIMESTAMPTZ,
  UNIQUE (round, team_id)
);

-- --- action submissions ----------------------------------------------
-- One campaign action per team per round. `params` carries action-specific
-- input (e.g. Scout target hex, Encamp/Demolish target) as JSONB so the
-- effects registry can dispatch without per-action columns.
CREATE TABLE IF NOT EXISTS actions (
  id             SERIAL PRIMARY KEY,
  round          INT  NOT NULL,
  team_id        INT  NOT NULL REFERENCES teams(team_id),
  action         TEXT NOT NULL CHECK (action IN
                   ('scout','resupply','encamp','search','demolish')),
  params         JSONB NOT NULL DEFAULT '{}'::jsonb,
  resolved_order INT,
  outcome        JSONB,                            -- what the effect did, for the log/UI
  submitted_at   TIMESTAMPTZ NOT NULL DEFAULT now(),
  resolved_at    TIMESTAMPTZ,
  UNIQUE (round, team_id)
);

-- event_log already exists (append-only audit trail). The engine writes
-- one row per state change during resolution.
