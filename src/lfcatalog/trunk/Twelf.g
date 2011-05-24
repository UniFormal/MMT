grammar Twelf;
options { k = *; }

@header {
/*******************************************************************************
 * Copyright (c) 2009 Kevin Bierhoff and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Kevin Bierhoff - initial API and implementation
 *******************************************************************************/
package crawler;

import java.util.List; 
import java.util.LinkedList;
import crawler.Storage;
import scala.collection.JavaConversions;
}
	
LINEDELIMITER  // a line delimiter
	:	'\r\n' | '\r' | '\n' | '\f';

LINESPACE 	 // a line white space character
	:	' ' | '\t';
	/*
IDCHAR
	:	~(' ' | '\t' | '\r' | '\n' | '\f' | ':' | '(' | ')' | '[' | ']' | '{' | '}' | '%' | '.' | '"');
	
RESERVED
	:	':' | '(' | ')' | '[' | ']' | '{' | '}' | '%';
	
DOT 	
	:	'.';
QUOTE 	
	:	'"';
	
identifierPart  // an identifier part, i.e. the a, b, c in the identifier a.b.c
	:	IDCHAR+;

identifier // a Twelf identifier
	:	identifierPart (DOT identifierPart)*;*/
/*
multiLineNonSemanticComment // a comment of type %{ ... }%
	:	'\%{' ({ input.LA(2) != '\%{' }? .)* '}\%';

singleLineComment  // a comment of type % ...
	:	'\%' ((LINESPACE | '\%') (~LINEDELIMITER)*)? LINEDELIMITER;*/
/*	
comment
	:   '\%{'
        (options {greedy=false;} : (comment | ({ !(input.LA(0)=='\%' && input.LA(1)=='{') }?=> .)) )*
        '}\%'
    ;
    
tst
	:   '\%{'
        ( { !(input.LA(0)=='\%' && input.LA(1)=='{') }?=> . )
        '}\%'
    ;
    
bla 	:	(comment)*;

data
@init {int n=1;}
	:	( {n<=4}?=> . {n++;} )+;
	*/

// greedy=false fails because the parser backtracks, since the first alternative it finds doesn't work (there are still characters at the end)

/*
WS 
	:	(' '
    |   '\t'
    |   '\n'
    |   '\r')+
    ;


fragment 
RESERVED 
	: ':' | '.' | '(' | ')' | '[' | ']' | '{' | '}' 
	;

ID
	: (~(RESERVED | WS))+
	;
	
COMMENT
	: '%' 
	(	'\n'
	|	('%'|' ') (~'\n')* '\n'
	|	'{' 
		(	{ input.LA(2) != '\%' }? '}'
		|	'\n'
		|	~('}'|'\n')
		)* 
		'}%'
	) 
	;

	
pi_term
@init {  }
	: 	'{' x=ID (':' vt=term)? '}' t=pi_term
	| 	'[' c=ID (':' vt=term)? ']' t=pi_term
	| 	ascr_term
	;
	
ascr_term 
@init {  }
	: 	t=arr_term 
		(	':' a=term 
			{ }
		| 	{ } 
		)
	;
	
arr_term
@init {  }
	: 	t1=app_term 
		(	'->' t2=app_term
			{ } 
		|	'<-' t2=app_term 
			{ } 
		)*
		{ }
	;
	
app_term
@init {  }
	:	t1=atom_term {  }
		(	t2=atom_term
			{  } 
		)*
	;
	
atom_term 
@init {  }
	: 'type'
	| w='_'	
	| x=ID	
	| '(' term { } ')'
	;


term 
@init {  }
	:	ret=pi_term {  } 
	;


decl
@init { }
	:	constantDecl
	;
	
constantDecl
@init {  }
	: 	c=ID 
		(	':' t=term ('=' v=term)? 
		| 	'=' v=term
		) e1='.'
	| 	b='_' (':' t=term)? '=' v=term e2='.'
	;


startRule
@init {  }
	: 	(d=decl)*
	;*/