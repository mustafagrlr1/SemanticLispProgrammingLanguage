%{
//Declarations
    	#include <stdbool.h>
	#include <stdio.h>
	#include <string.h>
	#include <stdlib.h>
	#include "y.tab.h"
	void yyerror(char *);
%}



%%

"and"		{yylval = *yytext; return KW_AND;}
"or"		{yylval = *yytext; return KW_OR;} 
"not"		{yylval = *yytext; return KW_NOT;}
"equal"		{yylval = *yytext; return KW_EQUAL;}
"append"	{yylval = *yytext; return KW_APPEND;}
"concat"	{yylval = *yytext; return KW_CONCAT;}
"set"		{yylval = *yytext; return KW_SET;}
"defvar" 	{yylval = *yytext; return KW_DEFVAR;}
"for"		{yylval = *yytext; return KW_FOR;}
"if"		{yylval = *yytext; return KW_IF;}
"exit"		{yylval = *yytext; return KW_EXIT;}
"true"		{yylval = *yytext; return KW_TRUE;}
"false"		{yylval = *yytext; return KW_FALSE;}
"list" 		{yylval = *yytext; return KW_LIST;} 

"+"		{yylval = *yytext; return OP_PLUS;}
"-"		{yylval = *yytext; return OP_MINUS;}
"/"		{yylval = *yytext; return OP_DIV;}
"*"		{yylval = *yytext; return OP_MULT;}
"("		{yylval = *yytext; return OP_OP;}
")"		{yylval = *yytext; return OP_CP;}
"**"	{yylval = *yytext; return OP_DBMULT;}
[ \t\r]  { }
";;".*$	{yylval = *yytext;} // it does not have to return anythin

-?[0-9]+		{yylval = atoi(yytext); return VALUE;}

[a-zA-Z_]*[a-zA-Z0-9_]*	{yylval = *yytext; return IDENTIFIER;}

[a-zA-Z][,+-/]	 	  	  {printf("Invalid Identifier\n"); return 0;} 
[,][A-zA-Z0-9+-/,]	 	  {printf("Invalid Identifier\n"); return 0;}
[+][A-zA-Z0-9,-/+]	 	  {printf("Invalid Identifier\n"); return 0;}	
[-][A-zA-Z0-9+,/-]	 	  {printf("Invalid Identifier\n"); return 0;}	
[/][A-zA-Z0-9+-,/]	 	  {printf("Invalid Identifier\n"); return 0;}
[A-zA-Z0-9+-,/]*	 	  {printf("Invalid Identifier\n"); return 0;}                       	
.          { return *yytext; }  /* should be the LAST rule */


%%


int yywrap(){
	return 1;
}

