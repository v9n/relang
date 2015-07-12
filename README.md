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

## Changefeeds

```
relang:r(Connection, [[{db, ["test"]}, {table, ["t4"]}, {change, fun(Item) -> io:format(Item) end}).
```
