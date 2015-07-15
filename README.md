# relang
RethinkDB driver in Erlang

# Why

Learn Erlang. 

# Credit

My implementation is very simple and maybe buggy but I inspied by
[https://github.com/taybin/lethink](https://github.com/taybin/lethink)
especially on the query syntax.

# Compile

```
rebar get-deps
rebar compile
erl -pa ebin -pa deps/protobuffs/ebin
```

# Using

```Erlang

%% Creation conection
Connection = relang:connect("127.0.0.1")

relang:r(Connection, [{db_create, "db1}])
relang:r(Connection, [{db_list}])
relang:r(Connection, [{db, ["test"]}, {table_list}])
relang:r(Connection, [{db, ["test"]}, {table_create, ["t1"]}])
relang:r(Connection, [{db, ["test"]}, {table, ["t4"]}, {insert, ["{\"name\":\"vinh\",\"age\":27}"]}]).
```

Ideally instead of chaining function like in Ruby, we put the query into
a list of tuples. I want and I like chaining style but I don't know how
to do that in Erlang.

Some special function has different syntax such as changefeed and filter
because they are a bit different.


## Changefeeds

```
relang:r(Connection, [[{db, ["test"]}, {table, ["t4"]}, {change, fun(Item) -> io:format(Item) end}).
```

## Filter and row

On the surface, filter looks like they are code that run on driver side,
but actually they are serialized and pass to the server for evaluate.

Depend on driver, the syntax of using `row` with `filter` is different.
Here is how we do it in Erlang:

```
relang:r(Connection, [[{db, ["test"]}, {table, ["t4"]}, {filter, [{<<"age">>, 30}).
```

With functional style

```
relang:r(C9, [{db, ["test"]}, {table, ["tv_shows"]}, {filter, fun(X) -> io:format("filter") end}]).

relang:r(Connection, [[{db, ["test"]}, {table, ["t4"]}, {filter, func(X) ->
  relang:row(X, age, {eq, <<30>>})
end}).

relang:r(Connection, [[{db, ["test"]}, {table, ["t4"]}, {filter, func(X) ->
  relang:row(X, age, {and, [{eq, <<30>>}, {eq, <<30>>}]})
end}).

relang:r(Connection, [[{db, ["test"]}, {table, ["t4"]}, {filter, func(X) ->
  relang:row(X, [{gt, <<"age">>, 30}, {or, match, <<"name">>, <<"V">>}]
end}).

relang:r(D1, [{db, ["test"]}, {table, ["tv_shows"]}, {filter, fun(X) ->
  relang:row(X, [{eq, <<"age">>, 30}])
end}]).
```
