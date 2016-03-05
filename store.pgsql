create table if not exists event_streams (
    id          serial4,
    type        text,
    tag         int4
);

create table if not exists events (
    id          serial4,
    stream_id   int4,
    "timestamp" timestamptz default now(),
    payload     jsonb
);
