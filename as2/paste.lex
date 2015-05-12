
<INITIAL>":="			=> (Tokens.ASSIGN(yypos,yypos+2));
<INITIAL>"|"			=> (Tokens.OR(yypos,yypos+1));
<INITIAL>"&"			=> (Tokens.AND(yypos,yypos+1));
<INITIAL>">="	 		=> (Tokens.GE(yypos,yypos+2));
<INITIAL>">"			=> (Tokens.GT(yypos,yypos+1));
<INITIAL>"<=" 			=> (Tokens.LE(yypos,yypos+2));
<INITIAL>"<"	 		=> (Tokens.LT(yypos,yypos+1));
<INITIAL>"!=" 			=> (Tokens.NEQ(yypos,yypos+size(yytext)));
<INITIAL>"=" 			=> (Tokens.EQ(yypos,yypos+size(yytext)));
<INITIAL>"/" 			=> (Tokens.DIVIDE(yypos,yypos+size(yytext)));
<INITIAL>"*"	 		=> (Tokens.TIMES(yypos,yypos+size(yytext)));
<INITIAL>"-" 			=> (Tokens.MINUS(yypos,yypos+size(yytext)));
<INITIAL>"+" 			=> (Tokens.PLUS(yypos,yypos+size(yytext)));
<INITIAL>"." 			=> (Tokens.DOT(yypos,yypos+size(yytext)));
<INITIAL>"}" 			=> (Tokens.RBRACE(yypos,yypos+size(yytext)));
<INITIAL>"{" 			=> (Tokens.LBRACE(yypos,yypos+size(yytext)));
<INITIAL>"]" 			=> (Tokens.RBRACK(yypos,yypos+size(yytext)));
<INITIAL>"[" 			=> (Tokens.LBRACK(yypos,yypos+size(yytext)));
<INITIAL>")" 			=> (Tokens.RPAREN(yypos,yypos+size(yytext)));
<INITIAL>"(" 			=> (Tokens.LPAREN(yypos,yypos+size(yytext)));
<INITIAL>";" 			=> (Tokens.SEMICOLON(yypos,yypos+size(yytext)));
<INITIAL>":" 			=> (Tokens.COLON(yypos,yypos+size(yytext)));
<INITIAL>"," 			=> (Tokens.COMMA(yypos,yypos+size(yytext)));

<INITIAL>[^\t\n\r\f]+ 	=> (Tokens.STRING(yytext,yypos,yypos+size(yytext)));

<INITIAL>[0-9]+ 			=> (Tokens.INT(Int.fromString yytext, yypos,yypos+size(yytext)));

<INITIAL>[0-9A-Za-z_]	=> (Tokens.ID(yytext,yypos,yypos+size(yytext)));
.				=> (ErrorMsg.error yypos "illegal character " ^ yytext); continue());