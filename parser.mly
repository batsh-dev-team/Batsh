%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token TRUE
%token FALSE
%token IF
%token ELSE
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACK
%token RIGHT_BRACK
%token LEFT_BRACE
%token RIGHT_BRACE
%token SEMICOLON
%token COMMA
%token EOF

%nonassoc IF
%nonassoc ELSE

%start <Statement.statements> prog

%%

prog:
    stmts = statement_list; EOF; { stmts };

statement_list:
    stmts = list(statement); { stmts } ;

statement:
  | SEMICOLON; { `Empty }
  | expr = expression; SEMICOLON { `Expression expr }
  | LEFT_BRACE; stmts = statement_list; RIGHT_BRACE; { `Block stmts }
  | stmt = if_statement; { stmt } ;

if_statement:
  | IF; LEFT_PAREN; expr = expression; RIGHT_PAREN; thenStmt = statement; %prec IF;
      { `If (expr, thenStmt) }
  | IF; LEFT_PAREN; expr = expression; RIGHT_PAREN; thenStmt = statement;
      ELSE; elseStmt = statement;
      { `IfElse (expr, thenStmt, elseStmt) }
  ;

expression:
  | s = STRING                                { `String s   }
  | i = INT                                   { `Int i      }
  | x = FLOAT                                 { `Float x    }
  | LEFT_BRACK; vl = list_fields; RIGHT_BRACK { `List vl    }
  | TRUE                                      { `Bool true  }
  | FALSE                                     { `Bool false } ;

list_fields:
    vl = separated_list(COMMA, expression)         { vl } ;
