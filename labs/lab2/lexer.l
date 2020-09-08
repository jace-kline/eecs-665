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

{INT} { charNum += yyleng; return TokenTag::INTLIT; }
[(] { charNum++; return TokenTag::LVAR; }
[)] { charNum++; return TokenTag::RVAR; }
[+] { charNum++; return TokenTag::PLUS; }
[-] { charNum++; return TokenTag::MINUS; }
[/] { charNum++; return TokenTag::DIV; }
[*] { charNum++; return TokenTag::MULT; }
{WS}+ { charNum++; }
\n { charNum = 1; lineNum++; }
<<EOF>> { yyterminate(); }
. { error(lineNum, charNum, "Invalid input character " + std::string(yytext) + "\n")}
%%