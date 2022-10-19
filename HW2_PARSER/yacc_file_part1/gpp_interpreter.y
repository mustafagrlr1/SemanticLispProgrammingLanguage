
%{

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

int print = 0;
int errorValue = 0;
int result = 1;
int expiResult = 0;
int expbResult = 0;
int explisti = 0;
int list_number = 0;
int ifliste = 0;

int arr[1000];
int j = 0;
int k = 0;
int i = 0;
int yylex();
void yyerror(char *s);
void yyrestart (FILE* input_file);
extern FILE *yyin;
%}

%start start // start symbol of cfg

%token KW_AND
%token KW_OR 
%token KW_NOT
%token KW_EQUAL
%token KW_APPEND
%token KW_CONCAT
%token KW_SET
%token KW_FOR
%token KW_IF
%token KW_DEFVAR
%token KW_EXIT
%token KW_TRUE
%token KW_FALSE
%token KW_LIST



%token OP_PLUS
%token OP_MINUS
%token OP_DIV
%token OP_MULT
%token OP_OP
%token OP_CP
%token OP_DBMULT

%token VALUE

%token IDENTIFIER

%%
		
start	: 
		'\n'
		| input {}
		| input start
		;


input 	:
		OP_OP KW_EXIT OP_CP { exit(1);}
		|	expi {
				if(errorValue == 1){
					printf("SYNTAX_ERROR Expression not recognized\n");
					exit(1);
				}
				printf("SYNTAX OK.\n");
				if(errorValue == 0 && print == 1 && expiResult == 1 && ifliste == 0){
					
					printf("Result:%d\n", $$ );
					print = 0;
					expiResult = 0;
				}
				if(errorValue == 0 && print == 1 && expiResult == 1 && ifliste == 1){
 					printf("Result:(");
					for(int i=list_number ; i>0 ; i--){
						printf("%d ",arr[j-i]);
					}
					printf(")\n");
					print = 0;
 					expiResult = 0;
					ifliste = 0;
 				}
				list_number = 0;
			}
 		| explisti {
			 	if(errorValue == 1){
					printf("SYNTAX_ERROR Expression not recognized\n");
					exit(1);
				}
				printf("SYNTAX OK.\n");
 				if(errorValue == 0 && print == 1 && explisti == 1){
 					
 					print = 0;
 					explisti = 0;
 				}
				 
				 list_number = 0;
 			}
 		| expb	{	
			 	if(errorValue == 1){
					printf("SYNTAX_ERROR Expression not recognized\n");
					exit(1);
				}
				printf("SYNTAX OK.\n");
				if(errorValue == 0 && print == 1 && expbResult == 1){
 					
 						if($$ == 1){
							printf("True\n");
 						}else{
 							printf("False\n");
 						}
 					print = 0;
 					expbResult = 0;
 				}
				 list_number = 0;
 			}
 				 
 		;

values 	:  VALUE {
				arr[j] = $1; 
				j = j + 1; }
		;
listvalues 	:  
			listvalues VALUE {arr[j] = $2; j = j + 1;list_number = list_number + 1;}
		 | VALUE {
				arr[j] = $1; 
				j = j + 1; 
				list_number = list_number + 1;}
		;
	  
explisti  	: OP_OP KW_CONCAT explisti values OP_CP	
			| OP_OP KW_APPEND explisti values OP_CP	
			| values
			;


expb 		: OP_OP KW_AND expi expi OP_CP	{$$ = $3 && $4; print = 1; expbResult = 1;}
		| OP_OP KW_OR expi expi OP_CP		{$$ = $3 || $4; print = 1; expbResult = 1;}
		| OP_OP KW_NOT expi OP_CP		{$$ = !$3; print = 1;expbResult = 1;}
		| OP_OP KW_EQUAL expi expb OP_CP 	{$$ = $3 == $4; print = 1; expbResult = 1;}
		| OP_OP KW_EQUAL expi expi OP_CP 	{$$ = $3 == $4; print = 1; expbResult = 1;}
		| binaryvalues
		;

binaryvalues : KW_TRUE {$$ = 1; print = 1; expbResult = 1;}
			 | KW_FALSE {$$ = 0; print = 1 ; expbResult = 1;}



expi 	: values 
		|OP_OP OP_PLUS expi expi OP_CP	{$$ = $3 + $4;print = 1; expiResult = 1;}
		| OP_OP OP_MINUS expi expi OP_CP	{$$ = $3 - $4; print = 1; expiResult = 1;}
		| OP_OP OP_DIV expi expi OP_CP 	{$$ = $3 / $4; print = 1; expiResult = 1;}
		| OP_OP OP_MULT expi expi OP_CP 	{$$ = $3 * $4; print = 1; expiResult = 1;}
		| OP_OP OP_DBMULT expi expi OP_CP	{	
								for(int i=0 ; i<$4 ; i++){
									result = result * $3; 
									printf("result=>%d",result);
								};
								$$ = result;
								result = 1 ;
								print = 1;
								expiResult = 1;
							}
									
		| IDENTIFIER
		| OP_OP IDENTIFIER explisti OP_CP {$$ = $3 ; print = 1; expiResult = 1;}
		| OP_OP KW_SET IDENTIFIER values OP_CP 	{$$ = $4; print = 1; expiResult = 1;}
		| OP_OP KW_IF expb explisti OP_CP 	{ if($3 == 1){
													expbResult = 1;
													print = 1;
													$$ = 1;
												}else{
													expbResult = 1;
													print = 1;
													$$ = 0;
												}

											}
		| OP_OP KW_IF expb explisti explisti OP_CP { if($3 == 1 && $4 == 1){
														expbResult = 1;
														print = 1;
														$$ = 1;
													}else{
														expbResult = 0;
														print = 1;
														$$ = 0;
													}
												}
		| OP_OP KW_IF expb OP_CP { if($3 == 1){
											expbResult = 1;
											print = 1;
											$$ = 1;
										}else{
											expbResult = 0;
											print = 1;
											$$ = 0;
										}
									}
		| OP_OP KW_FOR OP_OP IDENTIFIER expi expi OP_CP explisti OP_CP { print = 1;expiResult = 1 ; $$ = 1;}
		| OP_OP KW_LIST listvalues OP_CP { ifliste = 1;
										print = 1;
										expiResult = 1;}
		;

%%


void yyerror(char *s) {
  errorValue = 1;
}
 
int main(int argc, char** argv){
    // if(argc > 1) {
    //     yyin = fopen(argv[1], "r");
	// 	if(yyparse()) {
    //         fclose(yyin);
    //     }
    //     return 0;
    // }
    yyin = stdin;
    yyrestart(yyin);
    return yyparse();
}
