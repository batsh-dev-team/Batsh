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
%token FUNCTION
%token GLOBAL
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
%token SEQ
%token SNE
%token AEQ
%token ANE
%token GT
%token LT
%token GE
%token LE
%token EOF

%nonassoc SEQ SNE AEQ ANE
%nonassoc GT LT GE LE
%left CONCAT
%left PLUS MINUS
%left MULTIPLY DIVIDE MODULO

%nonassoc IF
%nonassoc ELSE

%start <Batshast.asttype> program

%%

program:
    toplevels = toplevel_list; EOF;
      { toplevels }
  ;

toplevel:
  | stmt = statement;
      { Batshast.Statement stmt }
  | FUNCTION; name = IDENTIFIER; LEFT_PAREN;
      params = identifier_list; RIGHT_PAREN;
      LEFT_BRACE; stmts = statement_list; RIGHT_BRACE;
      { Batshast.Function (name, params, stmts) }

toplevel_list:
    toplevels = list(toplevel);
      { toplevels }
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
  | lvalue = leftvalue; EQUAL; expr = expression; SEMICOLON;
      { Batshast.Assignment (lvalue, expr) }
  | stmt = if_statement;
      { stmt }
  | stmt = loop_statement;
      { stmt }
  | GLOBAL ident = IDENTIFIER; SEMICOLON;
      { Batshast.Global ident }
  ;

statement_list:
    stmts = list(statement);
      { stmts }
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
  | lvalue = leftvalue
      { Batshast.Leftvalue lvalue }
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

identifier_list:
    idents = separated_list(COMMA, IDENTIFIER);
      { idents }
  ;

leftvalue:
  | ident = IDENTIFIER;
      { Batshast.Identifier ident }
  | lvalue = leftvalue; LEFT_BRACK; index = expression; RIGHT_BRACK;
      { Batshast.ListAccess (lvalue, index) }
  ;

binary_expression:
  | left = expression; PLUS; right = expression;
      { Batshast.ArithBinary ("+", left, right) }
  | left = expression; MINUS; right = expression;
      { Batshast.ArithBinary ("-", left, right) }
  | left = expression; MULTIPLY; right = expression;
      { Batshast.ArithBinary ("*", left, right) }
  | left = expression; DIVIDE; right = expression;
      { Batshast.ArithBinary ("/", left, right) }
  | left = expression; MODULO; right = expression;
      { Batshast.ArithBinary ("%", left, right) }
  | left = expression; AEQ; right = expression;
      { Batshast.ArithBinary ("===", left, right) }
  | left = expression; ANE; right = expression;
      { Batshast.ArithBinary ("!==", left, right) }
  | left = expression; GT; right = expression;
      { Batshast.ArithBinary (">", left, right) }
  | left = expression; LT; right = expression;
      { Batshast.ArithBinary ("<", left, right) }
  | left = expression; GE; right = expression;
      { Batshast.ArithBinary (">=", left, right) }
  | left = expression; LE; right = expression;
      { Batshast.ArithBinary ("<=", left, right) }
  | left = expression; CONCAT; right = expression;
      { Batshast.StrBinary ("++", left, right) }
  | left = expression; SEQ; right = expression;
      { Batshast.StrBinary ("==", left, right) }
  | left = expression; SNE; right = expression;
      { Batshast.StrBinary ("!=", left, right) }
  ;

list_fields:
    vl = separated_list(COMMA, expression)
      { vl }
  ;
