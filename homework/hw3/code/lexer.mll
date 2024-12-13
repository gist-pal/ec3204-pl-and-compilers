{
 open Parser
 exception Eof
 exception LexicalError
 let comment_depth = ref 0
}

let blank = [' ' '\n' '\t' '\r']+
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let number = ['0'-'9']+

rule start =
 parse blank   { start lexbuf }
     | "/*"    { comment_depth :=1; comment lexbuf; start lexbuf }
     | number  { NUM (int_of_string (Lexing.lexeme lexbuf)) }
     | "int"   { INT }
     | "if"    { IF }
     | "else"  { ELSE }
     | "while" { WHILE }
     | "do"    { DO }
     | "read"  { READ }
     | "print" { PRINT }
     | id      { ID (Lexing.lexeme lexbuf) }
     | "+"     { PLUS }
     | "-"     { MINUS }
     | "*"     { STAR }
     | "/"     { SLASH }
     | "!"     { NOT }
     | "=="    { EQUALEQUAL }
     | "="     { EQUAL }
     | "<="    { LE }
     | "<"     { LT }
     | ">="    { GE }
     | ">"     { GT }
     | "{"     { LBRACE }
     | "}"     { RBRACE }
     | "]"     { RBLOCK }
     | "["     { LBLOCK }
     | "("     { LPAREN }
     | ")"     { RPAREN }
     | "&&"    { AND }
     | "||"    { OR }
     | ";"     { SEMICOLON }
     | eof     { EOF }
     | _       { raise LexicalError }

and comment = parse
     "/*" {comment_depth := !comment_depth+1; comment lexbuf}
   | "*/" {comment_depth := !comment_depth-1;
           if !comment_depth > 0 then comment lexbuf }
   | eof {raise Eof}
   | _   {comment lexbuf}
