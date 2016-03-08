create table if not exists event_streams (
    id          serial4,
    type        text,
    version     int4
);

create table if not exists events (
    id          serial4,
    stream_id   int4,
    index       int4,
    "timestamp" timestamptz default now(),
    payload     jsonb
);
