grammar Decaf;

start
  : class+
  ;
class
  : 'class' ID super? '{' member* '}'
  ;
super
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
  | primary '.' ID actualArgs
  | 'super' '.' ID actualArgs
  | arrayExpr
  | fieldExpr
  ;
fieldExpr
  : primary '.' ID
  | 'super' '.' ID
  ;
arrayExpr
  : ID dimension
  | nonNewArrayExpr dimension
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

