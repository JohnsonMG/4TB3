%{
/* need this for string methods */
#include <string.h>
#include <stdio.h>

char san[256];
int pop();
%}
END	 	[ ]*"\n"
START	^[ ]*
%%
{START}					/* swallow leading and trailing spaces*/
[^a-zA-Z0-9\.\n/]		{fprintf(stderr, "invalid character\n");
						memset(san, '\0', sizeof(san));
						return 1;}
[a-zA-Z0-9\.]{15,}		{fprintf(stderr, "component too long\n");
						memset(san, '\0', sizeof(san));
						return 1;}
[/a-zA-Z0-9\.]{255,}	{fprintf(stderr, "pathname too long\n");
						memset(san, '\0', sizeof(san));
						return 1;}
{END}					{if(san[strlen(san) -1] == '/') san[strlen(san) -1] = '\0'; printf("%s\n", san);
						memset(san, '\0', sizeof(san));}
"\.\."					{if(pop(san)) return 1;}
{START}[/]+				{strcat(san, "/");}
{START}"\./"			/* swallow current directory*/
"\."					/* swallow current directory*/
[/]+					{if(san[strlen(san) -1] != '/' && strlen(san) != 0) strcat(san, "/");}
[a-zA-Z0-9\.]{1,14}		{strcat(san, yytext);}			
%%

int pop(char a[])
{
	int i = strlen(a) - 1;
	if(i < 0 || (i == 0 && a[i] == '/')){
		fprintf(stderr, "malformed pathname\n");
		return 1;			
	}
	a[i] = '\0';
	i--;
	while(a[i] != '/')
	{
		a[i] = '\0';
		if(i <= 0){
			return 0;		
		}		
		i--;
	}
	return 0;
}

int main()
{
   yylex();
	return 0;
}
