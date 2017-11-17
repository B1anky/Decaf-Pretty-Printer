grammar Decaf;

start
  : class2+
  ;
class2
  : 'class' ID super2? '{' member* '}'
  ;
super2
  : 'extends' ID
  ;
member
  : field
  | method
  | ctor
  ;
field
  : modifier* type varDeclaratorList ';'
  ;
method
  : modifier* type ID formalArgs block
  ;
ctor
  : modifier* ID formalArgs block
  ;
modifier
  : 'static'
  | 'public'
  | 'private'
  | 'protected'
  ;
formalArgs
  : '(' formalArgList? ')'
  ; 
formalArgList
  : formalArg 
  | formalArg ',' formalArgList
  ;
formalArg
  : type varDeclaratorId
  ;
type
  : primitiveType
  | ID
  | type '[' ']'
  ;
primitiveType
  : 'boolean'
  | 'char'
  | 'int'
  | 'void'
  ;
varDeclaratorList
  : varDeclarator ',' varDeclaratorList
  | varDeclarator
  ;
varDeclarator
  : varDeclaratorId
  | varDeclaratorId '=' expression
  ;
varDeclaratorId
  : ID
  | varDeclaratorId '[' ']'
  ;
block
  : '{' statement* '}'
  ;
statement
  : ';'
  | type varDeclaratorList ';'
  | 'if' '(' expression ')' statement
  | 'if' '(' expression ')' statement 'else' statement
  | expression ';'
  | 'while' '(' expression ')' statement
  | 'return' expression? ';'
  | 'continue' ';'
  | 'break' ';'
  | 'super' actualArgs ';'
  | block
  ;
expression
  : expression binaryOp expression
  | unaryOp expression
  | primary
  ;
binaryOp
  : '=' | '||' | '&&' | '==' | '!=' | '<' | '>' | '<=' | '>='
  | '+' | '-' | '*' | '/' | '%'
  ;
unaryOp
  : '+' | '-' | '!'
  ;
primary
  : newArrayExpr
  | nonNewArrayExpr
  | ID
  ;
newArrayExpr
  : 'new' ID dimension+
  | 'new' primitiveType dimension+
  ;
dimension
  : '[' expression ']'
  ;
nonNewArrayExpr
  : literal
  | 'this'
  | '(' expression ')'
  | 'new' ID actualArgs
  | ID actualArgs
  | nonNewArrayExpr2
  | 'super' '.' ID actualArgs
  | arrayExpr
  | fieldExpr
  ;
nonNewArrayExpr2
  : newArrayExpr nonNewArrayExpr2
  | ID nonNewArrayExpr2
  | /*epsilon*/
  ;
nonNewArrayExpr3
  : dimension nonNewArrayExpr3
  | /*epsilon*/
  ;
fieldExpr
  : primary '.' ID
  | 'super' '.' ID
  ;
fieldExpr2
  : '.' ID fieldExpr2
  | 
  ;
arrayExpr
  : ID dimension
  | nonNewArrayExpr3
  ;
literal
  : 'null' | 'true' | 'false' | INT_LITERAL | CHAR_LITERAL | STRING_LITERAL
  ;
actualArgs
  : '(' exprList? ')'
  ;
exprList
  : expression
  | expression ',' exprList
  ;
INT_LITERAL
  : ('0'..'9') | (('1'..'9')('0'..'9')+)
  ;
CHAR_LITERAL
  : ('"' ( '\\"' | ~('\n'|'\r') )? '"')
  ;
STRING_LITERAL
  : ('"' ( '\\"' | ~('\n'|'\r') )*? '"')
  ;
ID
  : ('_'|'a'..'z'|'A'..'Z')(('a'..'z'|'A'..'Z'|'0'..'9'|'_')*)
  ;
WS
  : [ \t\r\n]+ -> skip
  ;
COMMENT
  : '/*'.*?'*/' -> skip
  ;
LINE_COMMENT
  : '//' ~[\r\n]* -> skip
  ;