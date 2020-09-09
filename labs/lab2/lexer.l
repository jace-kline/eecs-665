/*Definitions*/
%{
    #include "scanner.hpp"

    #undef YY_DECL
    #define YY_DECL int LAB2::Scanner::yylex(LAB2::Parser::semantic_type * const lval)
    #define yyterminate() { return TokenTag::END; }
    using TokenTag = LAB2::Parser::token;

%}

%option debug
%option nodefault
%option yyclass="LAB2::Scanner"
%option noyywrap
%option c++

INT [0]|([1-9][0-9]*)
WS [ \t]

%%

%{
/* Code executed on each yylex invocation */
yylval = lval;
%}

{INT} { 
    charNum += yyleng;
    yylval->intval = std::stoi(std::string(yytext));
    return TokenTag::INTLIT; 
}
[(] { charNum++; return TokenTag::LPAR; }
[)] { charNum++; return TokenTag::RPAR; }
[+] { charNum++; return TokenTag::PLUS; }
[-] { charNum++; return TokenTag::MINUS; }
[/] { charNum++; return TokenTag::DIV; }
[*] { charNum++; return TokenTag::MULT; }
{WS}+ { charNum++; }
\n { charNum = 1; lineNum++; }
<<EOF>> { yyterminate(); }
. { error(lineNum, charNum, "Invalid input character " + std::string(yytext) + "\n");}
%%