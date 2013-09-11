%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> IDENTIFIER
%token <string> COMMENT
%token TRUE
%token FALSE
%token IF
%token ELSE
%token WHILE
%token EQUAL
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
%token CONCAT
%token EQ
%token NE
%token GT
%token LT
%token GE
%token LE
%token EOF

%nonassoc EQ NE
%nonassoc GT LT GE LE
%left CONCAT
%left PLUS MINUS
%left MULTIPLY DIVIDE MODULO

%nonassoc IF
%nonassoc ELSE

%start <Batshast.statements> prog

%%

prog:
    stmts = statement_list; EOF;
      { stmts }
  ;

statement_list:
    stmts = list(statement);
      { stmts }
  ;

statement:
  | SEMICOLON;
      { Batshast.Empty }
  | comment = COMMENT;
      { Batshast.Comment comment }
  | expr = expression; SEMICOLON;
      { Batshast.Expression expr }
  | LEFT_BRACE; stmts = statement_list; RIGHT_BRACE;
      { Batshast.Block stmts }
  | ident = IDENTIFIER; EQUAL; expr = expression; SEMICOLON;
      { Batshast.Assignment (ident, expr) }
  | stmt = if_statement;
      { stmt }
  | stmt = loop_statement;
      { stmt }
  ;

if_statement:
  | IF; LEFT_PAREN; expr = expression; RIGHT_PAREN; thenStmt = statement;
      %prec IF;
      { Batshast.If (expr, thenStmt) }
  | IF; LEFT_PAREN; expr = expression; RIGHT_PAREN; thenStmt = statement;
      ELSE; elseStmt = statement;
      { Batshast.IfElse (expr, thenStmt, elseStmt) }
  ;

loop_statement:
  | WHILE; LEFT_PAREN; expr = expression; RIGHT_PAREN; stmt = statement;
      { Batshast.While (expr, stmt) }
  ;

expression:
  | ident = IDENTIFIER
      { Batshast.Identifier ident }
  | s = STRING
      { Batshast.String s }
  | i = INT
      { Batshast.Int i }
  | x = FLOAT
      { Batshast.Float x }
  | TRUE
      { Batshast.Bool true }
  | FALSE
      { Batshast.Bool false }
  | LEFT_BRACK; vl = list_fields; RIGHT_BRACK
      { Batshast.List vl }
  | expr = binary_expression;
      { expr }
  | LEFT_PAREN; expr = expression; RIGHT_PAREN;
      { Batshast.Parentheses expr }
  | ident = IDENTIFIER; LEFT_PAREN; exprs = expression_list; RIGHT_PAREN;
      { Batshast.Call (ident, exprs) }
  ;

expression_list:
    exprs = separated_list(COMMA, expression);
      { exprs }
  ;

binary_expression:
  | left = expression; PLUS; right = expression;
      { Batshast.Plus (left, right) }
  | left = expression; MINUS; right = expression;
      { Batshast.Minus (left, right) }
  | left = expression; MULTIPLY; right = expression;
      { Batshast.Multiply (left, right) }
  | left = expression; DIVIDE; right = expression;
      { Batshast.Divide (left, right) }
  | left = expression; MODULO; right = expression;
      { Batshast.Modulo (left, right) }
  | left = expression; CONCAT; right = expression;
      { Batshast.Concat (left, right) }
  | left = expression; EQ; right = expression;
      { Batshast.Equal (left, right) }
  | left = expression; NE; right = expression;
      { Batshast.NotEqual (left, right) }
  | left = expression; GT; right = expression;
      { Batshast.Greater (left, right) }
  | left = expression; LT; right = expression;
      { Batshast.Less (left, right) }
  | left = expression; GE; right = expression;
      { Batshast.GreaterEqual (left, right) }
  | left = expression; LE; right = expression;
      { Batshast.LessEqual (left, right) }
  ;

list_fields:
    vl = separated_list(COMMA, expression)
      { vl }
  ;
