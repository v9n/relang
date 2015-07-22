-define(QUERYTYPE_START, 1).
-define(QUERYTYPE_CONTINUE, 2).
-define(QUERYTYPE_STOP, 3).
-define(QUERYTYPE_NOREPLY_WAIT, 4).

-define(SUCCESS_ATOM, 1).
-define(SUCCESS_SEQUENCE, 2).
-define(SUCCESS_PARTIAL, 3).
-define(WAIT_COMPLETE, 4).
-define(CLIENT_ERROR, 16).
-define(COMPILE_ERROR, 17).
-define(RUNTIME_ERROR, 18).

%% Term definition
-define(TERMTYPE_MAKE_ARRAY, 2).

-define(VAR, 10).
-define(NOW, 103).
-define(DURING, 105).

-define(MATCH, 97).
-define(CHANGE, 152).
-define(TERMTYPE_COUNT, 43).
-define(TERMTYPE_UPDATE, 53).
-define(TERMTYPE_INSERT, 56).
-define(TERMTYPE_NTH, 45). %% Sequence, NUMBER -> DATUM
-define(BRACKET, 170).

-define(TERMTYPE_INNER_JOIN         , 48). % Sequence, Sequence, Function(2) -> Sequence
-define(TERMTYPE_OUTER_JOIN         , 49). % Sequence, Sequence, Function(2) -> Sequence
-define(TERMTYPE_EQ_JOIN            , 50). % Sequence, !STRING, Sequence, {index:!STRING} -> Sequence
-define(TERMTYPE_ZIP                , 72). % Sequence -> Sequence
-define(TERMTYPE_RANGE              , 173).%  -> Sequence                        [0, +inf)

-define(DB, 14).
-define(TABLE, 15).
-define(TERMTYPE_GET, 16).
-define(GET_ALL, 78).
-define(EQ, 17).
-define(NE, 18).
-define(LT, 19).
-define(LE, 20).
-define(GT, 21).
-define(GE, 22).
-define(NOT, 23).

-define(FILTER, 39).
-define(DB_CREATE, 57).
-define(db_list, 59).
-define(table_list, 62).
-define(TABLE_CREATE, 60).

-define(FUNC, 69).

-define(ADD, 24). % NUMBER... -> NUMBER | STRING... -> STRING
-define(SUB, 25). %% NUMBER... -> NUMBER
-define(MUL, 26). %% NUMBER... -> NUMBER
-define(DIV, 27). %% NUMBER... 
-define(MOD, 28). %% NUMBER, NUMBER -> NUMBER

-define(TERMTYPE_AND, 67).
-define(TERMTYPE_OR, 66).

-define(TERMTYPE_GEOJSON , 157).           % OBJECT -> PSEUDOTYPE(GEOMETRY)
-define(TERMTYPE_TO_GEOJSON , 158).        % PSEUDOTYPE(GEOMETRY) -> OBJECT
-define(TERMTYPE_POINT , 159).             % NUMBER, NUMBER -> PSEUDOTYPE(GEOMETRY)
-define(TERMTYPE_LINE , 160).              % (ARRAY | PSEUDOTYPE(GEOMETRY))... -> PSEUDOTYPE(GEOMETRY)
-define(TERMTYPE_POLYGON , 161).           % (ARRAY | PSEUDOTYPE(GEOMETRY))... -> PSEUDOTYPE(GEOMETRY)
-define(TERMTYPE_DISTANCE , 162).          % PSEUDOTYPE(GEOMETRY), PSEUDOTYPE(GEOMETRY) {geo_system:STRING, unit:STRING} -> NUMBER
-define(TERMTYPE_INTERSECTS , 163).        % PSEUDOTYPE(GEOMETRY), PSEUDOTYPE(GEOMETRY) -> BOOL
-define(TERMTYPE_INCLUDES , 164).          % PSEUDOTYPE(GEOMETRY), PSEUDOTYPE(GEOMETRY) -> BOOL
-define(TERMTYPE_CIRCLE , 165).            % PSEUDOTYPE(GEOMETRY), NUMBER {num_vertices:NUMBER, geo_system:STRING, unit:STRING, fill:BOOL} -> PSEUDOTYPE(GEOMETRY)
-define(TERMTYPE_GET_INTERSECTING , 166).  % TABLE, PSEUDOTYPE(GEOMETRY) {index:!STRING} -> StreamSelection
-define(TERMTYPE_FILL , 167).              % PSEUDOTYPE(GEOMETRY) -> PSEUDOTYPE(GEOMETRY)
-define(TERMTYPE_GET_NEAREST , 168).       % TABLE, PSEUDOTYPE(GEOMETRY) {index:!STRING, max_results:NUM, max_dist:NUM, geo_system:STRING, unit:STRING} -> ARRAY
-define(TERMTYPE_POLYGON_SUB , 171).       % PSEUDOTYPE(GEOMETRY), PSEUDOTYPE(GEOMETRY) -> PSEUDOTYPE(GEOMETRY)



