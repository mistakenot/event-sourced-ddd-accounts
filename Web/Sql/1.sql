create table event(
    id int not null primary key,
    type text not null,
    payload jsonb not null
);

create function put_event(previous_id int, type text, payload jsonb) returns void language plpgsql as $$
begin
    insert into event(id, type, payload) values (previous_id + 1, type, payload);
end
$$;

create function get_events(starting_at int) returns table(eventId int, eventType text, eventPayload jsonb) language plpgsql as $$
begin
    return query(select id, type, payload from event where id >= starting_at order by id);
end
$$;

create function get_most_recent_event_id() returns int language plpgsql as $$
begin
    return (select max(id) from event);
end
$$