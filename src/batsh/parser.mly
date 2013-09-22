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
%token NOT
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
%nonassoc NOT
%left PLUS MINUS
%left MULTIPLY DIVIDE MODULO

%nonassoc IF
%nonassoc ELSE

%start <Batsh_ast.asttype> program

%%

program:
    toplevels = toplevel_list; EOF;
      { toplevels }
  ;

toplevel:
  | stmt = statement;
      { Batsh_ast.Statement stmt }
  | FUNCTION; name = IDENTIFIER; LEFT_PAREN;
      params = identifier_list; RIGHT_PAREN;
      LEFT_BRACE; stmts = statement_list; RIGHT_BRACE;
      { Batsh_ast.Function (name, params, stmts) }

toplevel_list:
    toplevels = list(toplevel);
      { toplevels }
  ;

statement:
  | SEMICOLON;
      { Batsh_ast.Empty }
  | comment = COMMENT;
      { Batsh_ast.Comment comment }
  | expr = expression; SEMICOLON;
      { Batsh_ast.Expression expr }
  | LEFT_BRACE; stmts = statement_list; RIGHT_BRACE;
      { Batsh_ast.Block stmts }
  | lvalue = leftvalue; EQUAL; expr = expression; SEMICOLON;
      { Batsh_ast.Assignment (lvalue, expr) }
  | stmt = if_statement;
      { stmt }
  | stmt = loop_statement;
      { stmt }
  | GLOBAL ident = IDENTIFIER; SEMICOLON;
      { Batsh_ast.Global ident }
  ;

statement_list:
    stmts = list(statement);
      { stmts }
  ;

if_statement:
  | IF; LEFT_PAREN; expr = expression; RIGHT_PAREN; thenStmt = statement;
      %prec IF;
      { Batsh_ast.If (expr, thenStmt) }
  | IF; LEFT_PAREN; expr = expression; RIGHT_PAREN; thenStmt = statement;
      ELSE; elseStmt = statement;
      { Batsh_ast.IfElse (expr, thenStmt, elseStmt) }
  ;

loop_statement:
  | WHILE; LEFT_PAREN; expr = expression; RIGHT_PAREN; stmt = statement;
      { Batsh_ast.While (expr, stmt) }
  ;

expression:
  | lvalue = leftvalue
      { Batsh_ast.Leftvalue lvalue }
  | s = STRING
      { Batsh_ast.String s }
  | i = INT
      { Batsh_ast.Int i }
  | x = FLOAT
      { Batsh_ast.Float x }
  | TRUE
      { Batsh_ast.Bool true }
  | FALSE
      { Batsh_ast.Bool false }
  | LEFT_BRACK; vl = list_fields; RIGHT_BRACK
      { Batsh_ast.List vl }
  | expr = unary_expression;
      { expr }
  | expr = binary_expression;
      { expr }
  | LEFT_PAREN; expr = expression; RIGHT_PAREN;
      { Batsh_ast.Parentheses expr }
  | ident = IDENTIFIER; LEFT_PAREN; exprs = expression_list; RIGHT_PAREN;
      { Batsh_ast.Call (ident, exprs) }
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
      { Batsh_ast.Identifier ident }
  | lvalue = leftvalue; LEFT_BRACK; index = expression; RIGHT_BRACK;
      { Batsh_ast.ListAccess (lvalue, index) }
  ;

unary_expression:
  | NOT; expr = expression;
      { Batsh_ast.ArithUnary ("!", expr) }
  ;

binary_expression:
  | left = expression; PLUS; right = expression;
      { Batsh_ast.ArithBinary ("+", left, right) }
  | left = expression; MINUS; right = expression;
      { Batsh_ast.ArithBinary ("-", left, right) }
  | left = expression; MULTIPLY; right = expression;
      { Batsh_ast.ArithBinary ("*", left, right) }
  | left = expression; DIVIDE; right = expression;
      { Batsh_ast.ArithBinary ("/", left, right) }
  | left = expression; MODULO; right = expression;
      { Batsh_ast.ArithBinary ("%", left, right) }
  | left = expression; AEQ; right = expression;
      { Batsh_ast.ArithBinary ("===", left, right) }
  | left = expression; ANE; right = expression;
      { Batsh_ast.ArithBinary ("!==", left, right) }
  | left = expression; GT; right = expression;
      { Batsh_ast.ArithBinary (">", left, right) }
  | left = expression; LT; right = expression;
      { Batsh_ast.ArithBinary ("<", left, right) }
  | left = expression; GE; right = expression;
      { Batsh_ast.ArithBinary (">=", left, right) }
  | left = expression; LE; right = expression;
      { Batsh_ast.ArithBinary ("<=", left, right) }
  | left = expression; CONCAT; right = expression;
      { Batsh_ast.StrBinary ("++", left, right) }
  | left = expression; SEQ; right = expression;
      { Batsh_ast.StrBinary ("==", left, right) }
  | left = expression; SNE; right = expression;
      { Batsh_ast.StrBinary ("!=", left, right) }
  ;

list_fields:
    vl = separated_list(COMMA, expression)
      { vl }
  ;
