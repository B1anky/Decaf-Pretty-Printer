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
  : unaryOp expression 
  | ID dimension 
  | nonNewArrayExpr dimension 
  | primary '.' ID
  | 'super' '.' ID 
  | 'new' ID dimension+ 
  | 'new' primitiveType dimension+ 
  | literal | 'this' | '(' expression ')' 
  | 'new' ID actualArgs 
  | ID actualArgs 
  | primary '.' ID actualArgs 
  | 'super' '.' ID actualArgs 
  | ID | unaryOp expression mExpression
  | ID dimension mExpression 
  | nonNewArrayExpr dimension mExpression 
  | primary '.' ID mExpression 
  | 'super' '.' ID mExpression
  | 'new' ID dimension+ mExpression 
  | 'new' primitiveType dimension+ mExpression 
  | literal mExpression 
  | 'this' mExpression 
  | '(' expression ')' mExpression 
  | 'new' ID actualArgs mExpression 
  | ID actualArgs mExpression 
  | primary '.' ID actualArgs mExpression 
  | 'super' '.' ID actualArgs mExpression 
  | ID mExpression
  ;
mExpression
  : binaryOp expression mExpression 
  | binaryOp expression
  ;
binaryOp
  : '=' | '||' | '&&' | '==' | '!=' | '<' | '>' | '<=' | '>='
  | '+' | '-' | '*' | '/' | '%'
  ;
unaryOp
  : '+' | '-' | '!'
  ;
primary
  : 'new' primitiveType dimension+ mPrimary 
  | literal mPrimary 
  | 'this' mPrimary 
  | '(' expression ')' mPrimary 
  | 'new' ID actualArgs mPrimary 
  | ID actualArgs mPrimary 
  | 'super' '.' ID actualArgs mPrimary 
  | literal dimension mPrimary 
  | 'this' dimension mPrimary 
  | '(' expression ')' dimension mPrimary 
  | 'new' ID actualArgs dimension mPrimary 
  | ID actualArgs dimension mPrimary 
  | 'super' '.' ID actualArgs dimension mPrimary 
  | ID dimension dimension mPrimary 
  | 'super' '.' ID dimension mPrimary 
  | literal mNonNewArrExpr dimension mPrimary 
  | 'this' mNonNewArrExpr dimension mPrimary 
  | '(' expression ')' mNonNewArrExpr dimension mPrimary 
  | 'new' ID actualArgs mNonNewArrExpr dimension mPrimary 
  | ID actualArgs mNonNewArrExpr dimension mPrimary 
  | 'super' '.' ID actualArgs mNonNewArrExpr dimension mPrimary 
  | ID dimension mNonNewArrExpr dimension mPrimary 
  | 'super' '.' ID mNonNewArrExpr dimension mPrimary
  ;
mPrimary
  :' .' ID mPrimary 
  | '.' ID 
  | '.' ID actualArgs mPrimary 
  | '.' ID actualArgs 
  | '.' ID actualArgs dimension mPrimary 
  | '.' ID actualArgs dimension 
  | '.' ID dimension mPrimary 
  | '.' ID dimension 
  | '.' ID actualArgs mNonNewArrExpr dimension mPrimary
  | '.' ID actualArgs mNonNewArrExpr dimension 
  | '.' ID mNonNewArrExpr dimension mPrimary 
  | '.' ID mNonNewArrExpr dimension
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
  | ID dimension 
  | primary '.' ID 
  | 'super' '.' ID 
  | literal mNonNewArrExpr 
  | 'this' mNonNewArrExpr 
  | '(' expression ')' mNonNewArrExpr 
  | 'new' ID actualArgs mNonNewArrExpr 
  | ID actualArgs mNonNewArrExpr 
  | primary '.' ID actualArgs mNonNewArrExpr 
  | 'super' '.' ID actualArgs mNonNewArrExpr 
  | ID dimension mNonNewArrExpr 
  | primary '.' ID mNonNewArrExpr 
  | 'super' '.' ID mNonNewArrExpr
  ;
mNonNewArrExpr
  : dimension mNonNewArrExpr 
  | dimension
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
  : unaryOp expression 
  | ID dimension 
  | nonNewArrayExpr dimension 
  | primary '.' ID 
  | 'super' '.' ID 
  | 'new' ID dimension+ 
  | 'new' primitiveType dimension+ 
  | literal 
  | 'this' 
  | '(' expression ')' 
  | 'new' ID actualArgs 
  | ID actualArgs 
  | primary '.' ID actualArgs 
  | 'super' '.' ID actualArgs 
  | ID 
  | unaryOp expression ',' exprList 
  | ID dimension ',' exprList 
  | nonNewArrayExpr dimension ',' exprList 
  | primary '.' ID ',' exprList 
  | 'super' '.' ID ',' exprList 
  | 'new' ID dimension+ ',' exprList 
  | 'new' primitiveType dimension+ ',' exprList 
  | literal ',' exprList 
  | 'this' ',' exprList 
  | '(' expression ')' ',' exprList 
  | 'new' ID actualArgs ',' exprList 
  | ID actualArgs ',' exprList 
  | primary '.' ID actualArgs ',' exprList 
  | 'super' '.' ID actualArgs ',' exprList 
  | ID ',' exprList 
  | unaryOp expression mExpression ',' exprList 
  | ID dimension mExpression ',' exprList 
  | nonNewArrayExpr dimension mExpression ',' exprList 
  | primary '.' ID mExpression ',' exprList 
  | 'super' '.' ID mExpression ',' exprList 
  | 'new' ID dimension+ mExpression ',' exprList 
  | 'new' primitiveType dimension+ mExpression ',' exprList 
  | literal mExpression ',' exprList 
  | 'this' mExpression ',' exprList 
  | '(' expression ')' mExpression ',' exprList 
  | 'new' ID actualArgs mExpression ',' exprList 
  | ID actualArgs mExpression ',' exprList 
  | primary '.' ID actualArgs mExpression ',' exprList 
  | 'super' '.' ID actualArgs mExpression ',' exprList 
  | ID mExpression ',' exprList 
  | unaryOp expression binaryOp expression 
  | ID dimension binaryOp expression 
  | nonNewArrayExpr dimension binaryOp expression 
  | primary '.' ID binaryOp expression 
  | 'super' '.' ID binaryOp expression 
  | 'new' ID dimension+ binaryOp expression 
  | 'new' primitiveType dimension+ binaryOp expression 
  | literal binaryOp expression 
  | 'this' binaryOp expression 
  | '(' expression ')' binaryOp expression 
  | 'new' ID actualArgs binaryOp expression 
  | ID actualArgs binaryOp expression 
  | primary '.' ID actualArgs binaryOp expression 
  | 'super' '.' ID actualArgs binaryOp expression 
  | ID binaryOp expression 
  | unaryOp expression mExpression binaryOp expression 
  | ID dimension mExpression binaryOp expression 
  | nonNewArrayExpr dimension mExpression binaryOp expression 
  | primary '.' ID mExpression binaryOp expression 
  | 'super' '.' ID mExpression binaryOp expression 
  | 'new' ID dimension+ mExpression binaryOp expression 
  | 'new' primitiveType dimension+ mExpression binaryOp expression 
  | literal mExpression binaryOp expression 
  | 'this' mExpression binaryOp expression 
  | '(' expression ')' mExpression binaryOp expression 
  | 'new' ID actualArgs mExpression binaryOp expression 
  | ID actualArgs mExpression binaryOp expression 
  | primary '.' ID actualArgs mExpression binaryOp expression 
  | 'super' '.' ID actualArgs mExpression binaryOp expression 
  | ID mExpression binaryOp expression
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