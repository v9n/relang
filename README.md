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

  * Limit: Only a `change` command in the list

## Filter and row

On the surface, filter looks like they are code that run on driver side,
but actually they are serialized and pass to the server for evaluate.

Depend on driver, the syntax of using `row` with `filter` is different.
Here is how we do it in Erlang:

With exactly match.
```
l(relang). l(relang_ast). l(log).
relang:r(relang:connect("127.0.0.1"),
  [ {db, [<<"test">>]},
    {table, [<<"tv_shows">>]},
    {filter, [
      [{<<"age">>, 30},
      {<<"name">>, <<"kurei">>},
      {<<"show">>, 1}]
    ]}
  ]
).

```

It's not powerful enough so we come up With functional style

```Erlang

relang:r(C, [{db, [<<"test">>]}, {table,
[<<"tv_shows">>]}, {filter, fun(X) ->
  X([
    {'and', [
      {gt, [<<"age">>, 22]},
      {lt, [<<"age">>, 25]},
      {match, [<<"name">>,  <<"^k">>]}
    ]}
  ])
end}]).

relang:r(C, [{db, [<<"test">>]}, {table,
[<<"tv_shows">>]}, {filter, fun(X) ->
  X([
    {'and', [
      {gt, [<<"age">>, 22]},
      {lt, [<<"age">>, 25]}
    ]}
  ])
end}]).

# find user 22 -> 25 of age, name starts with `k`, and opt-in to `show`
relang:r(C, [{db, [<<"test">>]}, {table,
[<<"tv_shows">>]}, {filter, fun(X) ->
  X([
    {'and', [
      {gt, [<<"age">>, 22]},
      {lt, [<<"age">>, 25]},
      {match, [<<"name">>,  <<"^k">>]},
      {has_field, <<"show">>
    ]}
  ])
end}]).

l(relang). l(relang_ast). l(log).
relang:r(relang:connect("127.0.0.1"), [{db, [<<"test">>]}, {table,
[<<"tv_shows">>]}, {filter, fun(X) ->
  X([
    {'and', [
      {gt, [<<"age">>, 22]},
      {lt, [<<"age">>, 25]},
      {match, [<<"name">>,  <<"^k">>]},
      {has_field, <<"show">>}
    ]}
  ])
end}]).

l(relang). l(relang_ast). l(log).
relang:r(relang:connect("127.0.0.1"), [{db, [<<"test">>]}, {table,
[<<"tv_shows">>]}, {filter, fun(X) ->
  X([
      {has_field, <<"show">>}
  ])
end}]).

```

## Expr

```
relang:r(Connection, [{expr(2)}]).

relang:r(Connection, {expr, 2
```

# API

```
relang:r(relang:connect("127.0.0.1"), [{now}]).
```


# Development

Make sure to use `tcpdump` during development for ReQL inspect

```
tcpdump -nl -w - -i lo0 -c 500 port 28015|strings
```

# Test

```
rebar eu
```

## Integration test with Docker (pending)

