%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token TRUE
%token FALSE
%token IF
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACK
%token RIGHT_BRACK
%token LEFT_BRACE
%token RIGHT_BRACE
%token SEMICOLON
%token COMMA
%token EOF


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
  | IF; LEFT_PAREN; expr = expression; RIGHT_PAREN; stmt = statement { `If (expr, stmt) } ;

expression:
  | s = STRING                                { `String s   }
  | i = INT                                   { `Int i      }
  | x = FLOAT                                 { `Float x    }
  | LEFT_BRACK; vl = list_fields; RIGHT_BRACK { `List vl    }
  | TRUE                                      { `Bool true  }
  | FALSE                                     { `Bool false } ;

list_fields:
    vl = separated_list(COMMA, expression)         { vl } ;
