-- =====================================================================
-- Ctesiphus Expedition — schema contract (003)
-- ---------------------------------------------------------------------
-- The exact table/column shape the R engine reads and writes. This is
-- the contract round_engine.R / effects.R / exploration.R rely on.
--
-- It is written defensively (CREATE/ADD ... IF NOT EXISTS) so it can be
-- applied on top of the original schema.sql without clobbering it. Diff
-- it against that file: where they disagree on a column name or type,
-- the engine code follows THIS file.
-- =====================================================================

-- --- hexes ------------------------------------------------------------
-- One row per hex on the chosen board. hex_type is the board topology
-- (surface/tomb/blocked, p.52). "Base" is NOT a type: it's recorded as
-- teams.base_hex. `blocked` is the *current* status — TRUE for pre-printed
-- blocked hexes at seed, and toggled TRUE/FALSE in play (Tectonic Fissure
-- SL21, Transtechnic Fulcrum TL25, Doomsday Vault TL35).
CREATE TABLE IF NOT EXISTS hexes (
  hex_uid        SERIAL  PRIMARY KEY,
  map_id         INT     NOT NULL,
  hex_no         INT     NOT NULL,                 -- printed label 1..52
  hex_type       TEXT    NOT NULL CHECK (hex_type IN ('surface','tomb','blocked')),
  axial_q        INT     NOT NULL,                 -- axial coords -> sf adjacency
  axial_r        INT     NOT NULL,
  explored       BOOLEAN NOT NULL DEFAULT FALSE,
  blocked        BOOLEAN NOT NULL DEFAULT FALSE,
  location_code  TEXT,                             -- set on exploration (SL../TL..)
  condition_code TEXT,                             -- set on exploration (SC../TC..)
  UNIQUE (map_id, hex_no)
);

-- columns the engine needs if `hexes` already existed without them
ALTER TABLE hexes
  ADD COLUMN IF NOT EXISTS explored       BOOLEAN NOT NULL DEFAULT FALSE,
  ADD COLUMN IF NOT EXISTS blocked        BOOLEAN NOT NULL DEFAULT FALSE,
  ADD COLUMN IF NOT EXISTS location_code  TEXT,
  ADD COLUMN IF NOT EXISTS condition_code TEXT;

-- --- teams ------------------------------------------------------------
-- Engine-required columns. Starting Supply points are 10 (p.53); Campaign
-- points start at 0 and only ever rise. base_hex is the team's base (it
-- can move, e.g. Forsaken Fortress SL35).
ALTER TABLE teams
  ADD COLUMN IF NOT EXISTS current_hex     INT REFERENCES hexes(hex_uid),
  ADD COLUMN IF NOT EXISTS base_hex        INT REFERENCES hexes(hex_uid),
  ADD COLUMN IF NOT EXISTS campaign_points INT NOT NULL DEFAULT 0,
  ADD COLUMN IF NOT EXISTS supply_points   INT NOT NULL DEFAULT 10
      CHECK (supply_points BETWEEN 0 AND 10);

-- --- camps ------------------------------------------------------------
-- A team may hold at most two camps (p.55) — enforced in app logic, not
-- here, since it's a rule with exceptions.
CREATE TABLE IF NOT EXISTS camps (
  id         SERIAL PRIMARY KEY,
  team_id    INT NOT NULL REFERENCES teams(team_id),
  hex_uid    INT NOT NULL REFERENCES hexes(hex_uid),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (team_id, hex_uid)
);

-- --- hex_resources ----------------------------------------------------
-- Typed per-hex counters (the table we chose over JSONB). Populated on
-- exploration for resource-bearing locations; drawn down by Search.
CREATE TABLE IF NOT EXISTS hex_resources (
  hex_uid       INT  NOT NULL REFERENCES hexes(hex_uid),
  resource_type TEXT NOT NULL CHECK (resource_type IN
                  ('supply','intel','campaign','search_cost')),
  amount        INT  NOT NULL DEFAULT 0,
  PRIMARY KEY (hex_uid, resource_type)
);

-- --- event_log --------------------------------------------------------
-- Append-only audit trail. team_id is nullable (campaign-wide events such
-- as threat_raised have no team).
CREATE TABLE IF NOT EXISTS event_log (
  id         BIGSERIAL PRIMARY KEY,
  round      INT,
  team_id    INT REFERENCES teams(team_id),
  kind       TEXT NOT NULL,
  detail     JSONB NOT NULL DEFAULT '{}'::jsonb,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- --- campaign_state ---------------------------------------------------
-- Single-row table. 002 added the round/phase cursor + threat_level; this
-- ensures the JSONB token bag the effects use (once-per-campaign flags,
-- Dimension Matrix key, Hyperfractal Gaol prisoner) exists.
ALTER TABLE campaign_state
  ADD COLUMN IF NOT EXISTS state JSONB NOT NULL DEFAULT '{}'::jsonb;
