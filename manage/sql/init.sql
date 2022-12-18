CREATE TABLE comment (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  text TEXT NOT NULL
);

INSERT INTO comment (text)
VALUES ('First comment')
     , ('Second comment')
     , ('Third comment')
     , ('This is freaking nuts!')
     ;

-- Keeping for ref, using session cookies for now
-- CREATE TABLE dream_session (
--   id TEXT PRIMARY KEY,
--   label TEXT NOT NULL,
--   expires_at REAL NOT NULL,
--   payload TEXT NOT NULL
-- );