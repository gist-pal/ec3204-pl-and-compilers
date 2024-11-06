%{
  open Ast
%}

%token NEWLINE LPAREN RPAREN PLUS MINUS MULTIPLY DIVIDE
%token<int> NUM

%left PLUS
%left MINUS
%left MULTIPLY
%left DIVIDE

%start program
%type <Ast.expr> program

%%

program : exp NEWLINE { $1 }

exp : NUM { Ast.Num ($1) }
  | exp PLUS exp { Ast.Add ($1, $3) }
  | exp MINUS exp { Ast.Minus ($1, $3) }
  | exp MULTIPLY exp { Ast.Mul ($1, $3) }
  | exp DIVIDE exp { Ast.Div ($1, $3) }
  | LPAREN exp RPAREN { $2 }
