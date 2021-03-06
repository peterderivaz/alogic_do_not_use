////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017 Argon Design Ltd.
// All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

lexer grammar VLexer;

channels {
  WHITESPACE,
  COMMENT
}

fragment LCMT: '//' .*? NL;                 // Line comment
fragment BCMT: '/*'  .*? '*/';              // Block comment
CMT: (LCMT | BCMT) -> channel(COMMENT) ;    // Any comment

UINTTYPE: 'u' [0-9]+;

INTTYPE: 'i' [0-9]+;

TICKNUM: '\'' 's'? [bdhx] [0-9a-fA-F_]+ ; // TODO remove 'x' ?

DOLLARID: '$' SIMPLEID;

DOLLAR: '$';

GOESTO: '->';


MUL:        '*'   ;
DIV:        '/'   ;
MOD:        '%'   ;

PLUS:       '+'   ;
MINUS:      '-'   ;

NOT:        '~'   ;
AND:        '&'   ;
NAND:       '~&'  ;
OR:         '|'   ;
NOR:        '~|'  ;
XOR:        '^'   ;
XNOR:       '~^'  ;

LSHIFT:     '<<'  ;
URSHIFT:    '>>'  ;
SRSHIFT:    '>>>' ;

PLING:      '!'   ;
ANDAND:     '&&'  ;
OROR:       '||'  ;

EQ:         '=='  ;
NE:         '!='  ;

GT:         '>'   ;
GE:         '>='  ;
LE:         '<='  ;
LT:         '<'   ;


QUESTIONMARK: '?';

COMMA:      ','   ;
SEMICOLON:  ';'   ;

PLUSPLUS:   '++'  ;
MINUSMINUS: '--'  ;
EQUALS:     '='   ;
ASSIGNOP
  : '+='
  | '-='
  | '&='
  | '|='
  | '^='
  | '>>='
  | '<<='
  | '>>>='
  ;

DOT: '.';

LEFTCURLY:    '{' ;
RIGHTCURLY:   '}' ;
LEFTSQUARE:   '[' ;
RIGHTSQUARE:  ']' ;
LEFTBRACKET:  '(' ;
RIGHTBRACKET: ')' ;

COLON: ':';
MINUSCOLON: '-:';
PLUSCOLON: '+:';

// Keywords
FSM     : 'fsm';
NETWORK : 'network';
PIPELINE: 'pipeline';
TYPEDEF : 'typedef';
STRUCT  : 'struct';
IN      : 'in';
OUT     : 'out';
CONST   : 'const' ;
PARAM   : 'param' ;
FENCE   : 'fence' ;
TRUE    : 'true' ;
FALSE   : 'false' ;
VOID    : 'void' ;
BOOL    : 'bool';
UINT    : 'uint' ;
INT     : 'int' ;
LOOP    : 'loop' ;
WHILE   : 'while' ;
DO      : 'do' ;
FOR     : 'for' ;
IF      : 'if' ;
GOTO    : 'goto' ;
ELSE    : 'else' ;
BREAK   : 'break' ;
RETURN  : 'return' ;
CASE    : 'case' ;
DEFAULT : 'default' ;
VERILOG : 'verilog' ;
NEW     : 'new' ;


SYNC        : 'sync';
SYNC_READY  : 'sync' (WS|CMT)* 'ready';
SYNC_ACCEPT : 'sync' (WS|CMT)* 'accept';

WIRE        : 'wire';
BUBBLE      : 'bubble';

LITERAL: '"' ~["]* '"';

VERILOGFUNC: 'void' (WS|CMT)* 'verilog' (WS|CMT)* '(' (WS|CMT)* ')' (WS|CMT)* -> pushMode(VMODE);

CONSTANT: [0-9_]+;

IDENTIFIER: SIMPLEID;

fragment SIMPLEID: [a-zA-Z_][a-zA-Z0-9_$]* ;

fragment NL
  : '\r'? '\n'
  ;

WS
  : ([ \t] | NL)+ -> channel(WHITESPACE)
  ;

ERRORCHAR : . ;

////////////////////////////////////////////////////////////////////////////////

mode VMODE;

VERILOGBODY:  '{' ( VERILOGBODY | ~[{}] )* '}'  -> popMode;

