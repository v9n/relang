# relang
RethinkDB driver in Erlang

# Build status

[![build status](https://app.wercker.com/status/37f8fa6c7f3a92b04f665f5ddb97e3ca/m/master "wercker status")](https://app.wercker.com/project/bykey/37f8fa6c7f3a92b04f665f5ddb97e3ca)

I used `wercker` to run test on my docker image
[relang](https://registry.hub.docker.com/u/kureikain/relang/)

# Why

I want to learn more about RethinkDB to work on my RethinkDB book
[Simply RethinkDB](https://github.com/kureikain/simplyrethink)

# Credit

My implementation is very simple and maybe buggy but I inspied by
[https://github.com/taybin/lethink](https://github.com/taybin/lethink)
especially on the query syntax.

# Compile

```
rebar get-deps
rebar compile
erl -pa ebin -pa deps/protobuffs/ebin deps/jsx/ebin
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

Each of tuples of query includes one to 3 element:

```
{operation, argument, options}
```

operation is the name that try to match official Ruby driver. Such as 

```
{table_create, argument, options}
{insert, argument, options}
```

Argument list element can be anything, from tuple to `function` when the
anonymous function is passed.

Argument is a list, if you pass a tuple, the driver attempt to convert
it to a list. How many items of the list is depends on how many item the
operation accept.


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
relang:r(relang:connect(),
[{db, [<<"test">>]}, {table, [<<"tv_shows">>]}, 
                  {filter, fun(X) ->
                               [
                                  {'and', [
                                           {gt, [{field, [X, <<"age">>]}, 22]},
                                           {lt, [{field, [X, <<"age">>]}, 25]},
                                           {match, [{field, [X, <<"name">>]},  <<"^k">>]}
                                          ]}
                                 ]
                           end}]
).

relang:r(C, [{db, [<<"test">>]}, {table,
[<<"tv_shows">>]}, {filter, fun(X) ->
  [
    {'and', [
      {gt, [{field, [X, <<"age">>]}, 22]},
      {lt, [{field, [X, <<"age">>]}, 25]}
    ]}
  ]
end}]).

# find user 22 -> 25 of age, name starts with `k`, and opt-in to `show`
relang:r(C, [{db, [<<"test">>]}, {table,
[<<"tv_shows">>]}, {filter, fun(X) ->
  [
    {'and', [
      {gt, [{field, [X, <<"age">>]}, 22]},
      {lt, [{field, [X, <<"age">>]}, 25]},
      {match, [{field, [X, <<"name">>]},  <<"^k">>]},
      {field, <<"show">>
    ]}
  ]
end}]).

l(relang). l(relang_ast). l(log).
relang:r(relang:connect("127.0.0.1"), [{db, [<<"test">>]}, {table,
[<<"tv_shows">>]}, {filter, fun(X) ->
  [
    {field, [X, <<"show">>]}
  ]
end}]).

```

## Expr(WIP)

```
relang:r(Connection, [{expr(2)}]).
```

# API

```
relang:r(relang:connect("127.0.0.1"), [{now}]).
```

## Connect

```Erlang
C1 = relang:connect("127.0.0.1")
```

## Manipulating tables

#### table_create

```Erlang
relang:r(relang:connect(), [{table_create, geo}]).
% or with db
relang:r(relang:connect(), [{db, test}, {table_create, geo}]).
```

#### table_drop

```Erlang
relang:r(relang:connect(), [{table_drop, geo}]).
% or with db
relang:r(relang:connect(), [{db, test}, {table_drop, geo}]).
```

## Selecting data

### db
### table

```Erlang
relang:r(C1, [{db, [<<"test">>]},  {table, <<"tv_shows">>}]).
```
### get

```Erlang
relang:r(C1, [{db, [<<"test">>]},  {table, <<"tv_shows">>}, {get, <<"primarykey">>]).
```

### filter
Reference filter below because they have a different syntax.

## Joins

### Inner joins

```
relang:r(relang:connect(),
  [{db, [<<"foodb">>]},
    {table, <<"tv_shows">>},
    {eq_join,
      [<<"compound_id">>,
        [{db, [<<"foodb">>]}, {table, <<"compounds">>}]
      ]
    }
  ]
).
```

### Equal join

Simple form, join use a column on left table whose value is equal to
index on right table. Using primary index by default
```Erlang
relang:r(relang:connect(),
  [{db, [<<"foodb">>]},
    {table, <<"tv_shows">>},
    {eq_join,
      [<<"compound_id">>,
        [{db, [<<"foodb">>]}, {table, <<"compounds">>}]
      ]
    }
  ]
).
```

Or using a secondary index

```
relang:r(relang:connect(),
  [{db, [<<"foodb">>]},
    {table, <<"tv_shows">>},
    {eq_join,
      [<<"compound_id">>,
        [{db, [<<"foodb">>]}, {table, <<"compounds">>}]
      ],
      [{<<"index">>, <<"different_index">>}]
    }
  ]
).
```

@TODO
Or using function instead of string

```Erlang
l(relang). l(relang_ast). l(log).
relang:r(relang:connect(),
  [{db, [<<"foodb">>]},
    {table, <<"tv_shows">>},
    {eq_join,
      [
        fun (X) ->
            [
              {field, {field, [X, <<"Parent">>]}, <<"Sub">>}
            ]
        end,
        [{table, <<"compounds">>}]
      ]
      ,
      [{<<"index">>, <<"different_index">>}]
    }
  ]
).
```

More complex expression

```
l(relang). l(relang_ast). l(log).
relang:r(relang:connect(),
  [{db, [<<"foodb">>]},
    {table, <<"tv_shows">>},
    {eq_join,
      [
        fun (X) ->
            [
              {field, {field, [X, <<"Parent">>]}, <<"Sub">>},
               {nth, 20}
            ]
        end,
        [{table, <<"compounds">>}]
      ]
      ,
      [{<<"index">>, <<"different_index">>}]
    }
  ]
).
```

### zip

Using with `eq_join`

```Erlang
l(relang). l(relang_ast). l(log).
relang:r(relang:connect(),
  [{db, [<<"foodb">>]},
    {table, <<"compounds_foods">>},
    {eq_join,
      [<<"compound_id">>,
        [{db, [<<"foodb">>]}, {table, <<"compounds">>}]
      ],
      [{<<"index">>, <<"second_index">>}]
    },
    {zip}
  ]
).
```

Or without index:

```Erlang
l(relang). l(relang_ast). l(log).
relang:r(relang:connect(),
  [{db, [<<"foodb">>]},
    {table, <<"compounds_foods">>},
    {eq_join,
      [<<"compound_id">>,
        [{db, [<<"foodb">>]}, {table, <<"compounds">>}]
      ]
    },
    {zip}
  ]
).
```

## Transformation

### nth

```
relang:r(relang:connect(),
  [
    {table, <<"tv_shows">>},
     {nth, 120}
  ]
).
```

## Writing data

### Insert

```Erlang
C1 = relang:connect("127.0.0.1")
relang:r(C1, [{db, [<<"test">>]},  {table, <<"tv_shows">>}, {insert, [[{<<"name">>, <<"kurei">>}, {<<"age">>, <<28>>}]]}])
```

### Update

```Erlang
relang:r(C1, [{db, [<<"test">>]},  {table, <<"tv_shows">>}, {get, <<"6b443331-d7c9-4304-867d-251db183446f">>}, {update, [[{<<"name">>, <<"kurei kain">>}, {<<"age">>, <<29>>}]]}])

% Or update with option
relang:r(C1,
  [{db, [<<"test">>]},
  {table, <<"tv_shows">>},
  {get, <<"6b443331-d7c9-4304-867d-251db183446f">>},
  {update,
    [[{<<"name">>, <<"kurei kain">>},
    {<<"age">>, <<29>>}]],
    [{<<"durability">>, soft}, {return_changes, false}]
  }
  ])

% @TODO Or update with function 
relang:r(C1,
  [{db, [<<"test">>]},
  {table, <<"tv_shows">>},
  {get, <<"6b443331-d7c9-4304-867d-251db183446f">>},
  {update,
    fun (X) ->
    end
  }
  ])
```

## Aggregation

### Count

```
relang:query(C, [ {db, [<<"test">>]}, {table, [<<"tv_shows">>]}, {count}]).
```

## Geospatial commands

### circle

```Erlang
l(relang). l(relang_ast). l(log).
relang:r(relang:connect(), [{circle, [{-122.423246, 37.779388}, 1000]}]).
```

### distance

```Erlang
relang:r(relang:connect(), 
  [
    {distance, 
      [
        relang:r([{point, [-122.423246,37.779388]}]),
        relang:r([{point, [-117.220406,32.719464]}])
      ],
      [{unit, km}]
    }
  ]).
```

### fill

### geojson

### to_geojson

### get_intersecting

### get_nearest

### includes

### intersects

### line

```Erlang
relang:r([
   {line,
    [
     [-122.423246,37.779388],
     [-121.886420,37.329898]
    ]
   }
  ]).
```

or using in other expression:

```Erlang
relang:r(relang:connect(),   [
    {table, geo},
    {insert, [[
      {id, 101},
      {route,
        relang:r([
          {line, [
            [-122.423246,37.779388], [-121.886420,37.329898]
          ]}
        ])
      }
    ]]}
  ]
).
```

### point

```Erlang
l(relang). l(relang_ast). l(log).
relang:r([{point, [-122.423246, 37.779388]}]).
```

### polygon

```Erlang
l(relang). l(relang_ast). l(log).
relang:r(relang:connect(),  [{polygon,
        [
          [-122.423246,37.779388],
          [-122.423246,37.329898],
          [-121.886420,37.329898],
          [-121.886420,37.779388]
        ]
       }
      ]
).
```

Or using in other expression

```Erlang
(relang). l(relang_ast). l(log).
relang:r(relang:connect(),     [
      {table, geo},
      {insert, [[
        {id, 101},
        {rectangle, relang:r([{polygon,
            [
              [-122.423246,37.779388],
              [-122.423246,37.329898],
              [-121.886420,37.329898],
              [-121.886420,37.779388]
            ]
           }
          ])
        }
      ]]}
    ]
).
```

### polygon_sub


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

