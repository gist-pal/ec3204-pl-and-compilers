%{
  open Lang
  module L = Lang
%}

%token <int> NUM
%token <string> ID
%token TRUE FALSE

%token LEN
%token INT
%token PLUS MINUS MUL EQ EQEQ NEQ
%token LE LT GE GT NOT OR AND
%token IF WHILE COMMA ELSE ASSERT SEMICOLON
%token LBRACE RBRACE LBRACKET RBRACKET LPAREN RPAREN EOF

%left SEMICOLON
%right OR
%right AND
%nonassoc LT LE GT GE EQEQ NEQ
%left PLUS MINUS
%left MUL
%right NOT

%start program
%type <Lang.pgm> program

%%

program:
  ID LPAREN params RPAREN
  LBRACE cmd RBRACE EOF
  {($1,$3,$6)}

params:
  | param COMMA params {$1::$3}
  | param {[$1]}

param:
  | typ ID { ($1,$2) }

typ:
  | INT { T_Int }
  | INT LBRACKET NUM RBRACKET { T_Array $3 }

cmd:
  | atom_cmd SEMICOLON { $1 }
  | atom_cmd SEMICOLON cmd {L.Seq ($1, $3)}
  | compound_cmd { $1 }
  | compound_cmd cmd {L.Seq ($1,$2)}

atom_cmd:
  | typ ID { L.Decl ($1, $2) }
  | lv EQ aexp { L.Assign ($1, $3) }
  | ASSERT LPAREN bexp RPAREN {L.Assert ($3)}

compound_cmd:
  | IF LPAREN bexp RPAREN LBRACE cmd RBRACE ELSE LBRACE cmd RBRACE {L.If ($3,$6,$10)}
  | IF LPAREN bexp RPAREN LBRACE cmd RBRACE {L.If ($3,$6,L.Skip)}
  | WHILE
      LPAREN bexp RPAREN
      LBRACE cmd RBRACE {L.While ($3,$6)}

aexp: (* TODO: complete the grammar spec *)
  | MINUS NUM {L.Int ($2* (-1))}
  | NUM {L.Int $1}

bexp: (* TODO: complete the grammar spec *)
  | TRUE {L.True}

lv:
  | ID {L.Var $1}
  | ID LBRACKET aexp RBRACKET {L.Arr ($1,$3)}

%%
