%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> IDENTIFIER
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

%start <Statement.statements> prog

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
      { Statement.Empty }
  | expr = expression; SEMICOLON;
      { Statement.Expression expr }
  | LEFT_BRACE; stmts = statement_list; RIGHT_BRACE;
      { Statement.Block stmts }
  | ident = IDENTIFIER; EQUAL; expr = expression; SEMICOLON;
      { Statement.Assignment (ident, expr) }
  | stmt = if_statement;
      { stmt }
  | stmt = loop_statement;
      { stmt }
  ;

if_statement:
  | IF; LEFT_PAREN; expr = expression; RIGHT_PAREN; thenStmt = statement;
      %prec IF;
      { Statement.If (expr, thenStmt) }
  | IF; LEFT_PAREN; expr = expression; RIGHT_PAREN; thenStmt = statement;
      ELSE; elseStmt = statement;
      { Statement.IfElse (expr, thenStmt, elseStmt) }
  ;

loop_statement:
  | WHILE; LEFT_PAREN; expr = expression; RIGHT_PAREN; stmt = statement;
      { Statement.While (expr, stmt) }
  ;

expression:
  | ident = IDENTIFIER
      { Statement.Identifier ident }
  | s = STRING
      { Statement.String s }
  | i = INT
      { Statement.Int i }
  | x = FLOAT
      { Statement.Float x }
  | TRUE
      { Statement.Bool true }
  | FALSE
      { Statement.Bool false }
  | LEFT_BRACK; vl = list_fields; RIGHT_BRACK
      { Statement.List vl }
  | expr = binary_expression;
      { expr }
  | LEFT_PAREN; expr = expression; RIGHT_PAREN;
      { Statement.Parentheses expr }
  | ident = IDENTIFIER; LEFT_PAREN; exprs = expression_list; RIGHT_PAREN;
      { Statement.Call (ident, exprs) }
  ;

expression_list:
    exprs = separated_list(COMMA, expression);
      { exprs }
  ;

binary_expression:
  | left = expression; PLUS; right = expression;
      { Statement.Plus (left, right) }
  | left = expression; MINUS; right = expression;
      { Statement.Minus (left, right) }
  | left = expression; MULTIPLY; right = expression;
      { Statement.Multiply (left, right) }
  | left = expression; DIVIDE; right = expression;
      { Statement.Divide (left, right) }
  | left = expression; MODULO; right = expression;
      { Statement.Modulo (left, right) }
  | left = expression; CONCAT; right = expression;
      { Statement.Concat (left, right) }
  | left = expression; EQ; right = expression;
      { Statement.Equal (left, right) }
  | left = expression; NE; right = expression;
      { Statement.NotEqual (left, right) }
  | left = expression; GT; right = expression;
      { Statement.Greater (left, right) }
  | left = expression; LT; right = expression;
      { Statement.Less (left, right) }
  | left = expression; GE; right = expression;
      { Statement.GreaterEqual (left, right) }
  | left = expression; LE; right = expression;
      { Statement.LessEqual (left, right) }
  ;

list_fields:
    vl = separated_list(COMMA, expression)
      { vl }
  ;
