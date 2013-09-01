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
%token PLUS
%token MINUS
%token MULTIPLY
%token DIVIDE
%token MODULO
%token EOF

%left PLUS MINUS
%left MULTIPLY DIVIDE MODULO

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
  | TRUE                                      { `Bool true  }
  | FALSE                                     { `Bool false }
  | LEFT_BRACK; vl = list_fields; RIGHT_BRACK { `List vl    }
  | expr = binary_expression;                 { expr }
  ;

binary_expression:
  | left = expression; PLUS; right = expression;
      { `Plus (left, right) }
  | left = expression; MINUS; right = expression;
      { `Minus (left, right) }
  | left = expression; MULTIPLY; right = expression;
      { `Multiply (left, right) }
  | left = expression; DIVIDE; right = expression;
      { `Divide (left, right) }
  | left = expression; MODULO; right = expression;
      { `Modulo (left, right) }
  ;

list_fields:
    vl = separated_list(COMMA, expression)         { vl } ;
