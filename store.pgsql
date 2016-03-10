create table if not exists event_streams (
    id          serial4 primary key,
    type        text not null,
    version     int4 not null,
    snapshot    int4 null
);

create table if not exists events (
    id          serial4 primary key,
    stream_id   int4 not null references event_streams,
    index       int4 not null,
    "timestamp" timestamptz default now(),
    payload     jsonb not null,
    unique (stream_id, index)
);

create table if not exists snapshots (
    id          serial4 primary key,
    stream_id   int4 not null references event_streams,
    version     int4 not null,
    "timestamp" timestamptz default now(),
    payload     jsonb not null,
    unique (stream_id, version)
);
