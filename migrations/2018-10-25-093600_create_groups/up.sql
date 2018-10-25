create table groups(
  id serial primary key,
  name text not null,
  parent_id integer,
  description text,
  created_at timestamp with time zone not null default now(),
  updated_at timestamp with time zone not null default now()
);
