create table if not exists people (
    id      serial,
    name    text,
    unique  (name)
);
