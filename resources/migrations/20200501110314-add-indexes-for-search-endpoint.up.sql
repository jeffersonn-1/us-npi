BEGIN;

DROP INDEX IF EXISTS idxprc;
DROP INDEX IF EXISTS idxorg;

CREATE INDEX idxprc ON practitioner USING gin (resource);
CREATE INDEX idxorg ON organizations USING gin (resource);

COMMIT;