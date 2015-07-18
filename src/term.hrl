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
-define(VAR, 10).
-define(NOW, 103).
-define(MATCH, 97).
-define(CHANGE, 152).
-define(INSERT, 56).
-define(BRACKET, 170).
-define(GT, 21).
-define(EQ, 17).
-define(FILTER, 39).
-define(DB, 14).
-define(DB_CREATE, 57).
-define(db_list, 59).
-define(TABLE, 15).
-define(table_list, 62).
-define(TABLE_CREATE, 60).

-define(ADD, 24). % NUMBER... -> NUMBER | STRING... -> STRING
-define(SUB, 25). %// NUMBER... -> NUMBER
-define(MUL, 26). %// NUMBER... -> NUMBER
-define(DIV, 27). %// NUMBER... 
-define(MOD, 28). %// NUMBER, NUMBER -> NUMBER


