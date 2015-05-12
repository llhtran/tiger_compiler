(* CPSC 421 Compilers and Interpreters Spring 2015*)
(* Student: Lien Tran BR 2016 *)
(* Assignment #1 due February 4th 2015 *)

type pos = int
type lexresult = Tokens.token
val linePos = ErrorMsg.linePos
val lineNum = ErrorMsg.lineNum
fun err(pos,str) = ErrorMsg.error pos str
val comm = ref 0
val tempStr = ref ""
val initSt = ref true

(* Handling EOF, making sure nothing unterminated. *)
val eof = fn () => 
	let 
		val pos = hd(!linePos) 
	in 
		if !initSt = false 
		then err(pos, "ERROR: Unterminated string or comment.")
		else ();
		Tokens.EOF(pos,pos)
	end

(* Taking care of conversion from string to integers, handles overflow. *)
fun intHandle(pos,text) = 
	let
		val x = Int.fromString(text) 
	in 
		Option.getOpt(x,~1)
	end
	handle Overflow => (err(pos, "ERROR: Integer overflow."); ~1)

(* Function used specifically to check ASCII chars when escaped \ddd. *)
fun ascii(text,pos) =
	let
		val x = 
			let 
				val y = Int.fromString(text)
			in 
				Option.getOpt(y,~1)
			end
	in
		if x>0 andalso x<256 then tempStr := !tempStr ^ str(chr x)
		else err(pos, "ERROR: Invalid ASCII code")
	end


(* Handles and confirms control codes when they appear in strings. *)
fun control(text,pos) = 
	let
		val x = ord(hd(explode(text)))
	in
		if x>63 andalso x<96 then tempStr:= !tempStr ^ str(chr(x-64))
		else err(pos, "ERROR: Invalid control code.")
	end

%%
%full 
id = [a-zA-Z][a-zA-Z0-9_]*;
space = [\t\n\012\032];
%s COMMENT STRING ESCAPE CONTROL;
%%
<INITIAL>\n 			=> (lineNum := !lineNum +1; linePos := yypos :: !linePos;
						continue());
<INITIAL,COMMENT>"/*"	=> (YYBEGIN COMMENT; initSt := false; comm:= !comm+1;
							 continue());
<COMMENT>"*/"			=> (comm:= !comm-1; if !comm=0 then (YYBEGIN INITIAL; 
							initSt := true) else (); continue());
<COMMENT>.				=> (continue());
<COMMENT>\n 			=> (lineNum := !lineNum +1; linePos := yypos :: !linePos;
						continue());

<INITIAL>\"				=> (YYBEGIN STRING; tempStr:=""; initSt := false; 
							continue());
<STRING>\"				=> (YYBEGIN INITIAL; initSt := true; 
							Tokens.STRING(!tempStr,yypos,yypos+size(!tempStr)));

<STRING>\\				=> (YYBEGIN ESCAPE; continue());
<ESCAPE>t                => (YYBEGIN STRING; tempStr:= !tempStr ^ "\t"; 
							continue());
<ESCAPE>n                => (YYBEGIN STRING; tempStr:= !tempStr ^ "\n"; 
							continue());
<ESCAPE>\"               => (YYBEGIN STRING; tempStr:= !tempStr ^ "\""; 
							continue());
<ESCAPE>\\               => (YYBEGIN STRING; tempStr:= !tempStr ^ "\\"; 
							continue());
<ESCAPE>[0-9]{3}	     => (YYBEGIN STRING; ascii(yytext,yypos); continue());
<ESCAPE>"^"              => (YYBEGIN CONTROL; continue());
<CONTROL>.				 => (YYBEGIN STRING; control(yytext,yypos); continue());
<ESCAPE>{space}+\\       => (YYBEGIN STRING; continue());
<ESCAPE>.|\n             => (YYBEGIN STRING; err(yypos,"ERROR: Illegal escape"); 
							continue());
<STRING>.				=> (tempStr := !tempStr ^ yytext; continue());
<STRING>\n				=> (tempStr := !tempStr ^ yytext; lineNum := !lineNum +1;
						 linePos := yypos :: !linePos;continue());
<INITIAL>type 			=> (Tokens.TYPE(yypos,yypos+size(yytext)));
<INITIAL>var			=> (Tokens.VAR(yypos,yypos+size(yytext)));
<INITIAL>function		=> (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL>break			=> (Tokens.BREAK(yypos,yypos+5));
<INITIAL>of				=> (Tokens.OF(yypos,yypos+2));
<INITIAL>end			=> (Tokens.END(yypos,yypos+3));
<INITIAL>in 			=> (Tokens.IN(yypos,yypos+2));
<INITIAL>nil			=> (Tokens.NIL(yypos,yypos+3));
<INITIAL>let 			=> (Tokens.LET(yypos,yypos+3));
<INITIAL>do 			=> (Tokens.DO(yypos,yypos+2));
<INITIAL>to 			=> (Tokens.TO(yypos,yypos+2));
<INITIAL>for 			=> (Tokens.FOR(yypos,yypos+3));
<INITIAL>while 			=> (Tokens.WHILE(yypos,yypos+5));
<INITIAL>else 			=> (Tokens.ELSE(yypos,yypos+4));
<INITIAL>then 			=> (Tokens.THEN(yypos,yypos+4));
<INITIAL>if				=> (Tokens.IF(yypos,yypos+2));
<INITIAL>array 			=> (Tokens.ARRAY(yypos,yypos+5));
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
<INITIAL>"."     			=> (Tokens.DOT(yypos,yypos+size(yytext)));
<INITIAL>"}" 			=> (Tokens.RBRACE(yypos,yypos+size(yytext)));
<INITIAL>"{" 			=> (Tokens.LBRACE(yypos,yypos+size(yytext)));
<INITIAL>"]" 			=> (Tokens.RBRACK(yypos,yypos+size(yytext)));
<INITIAL>"[" 			=> (Tokens.LBRACK(yypos,yypos+size(yytext)));
<INITIAL>")" 			=> (Tokens.RPAREN(yypos,yypos+size(yytext)));
<INITIAL>"(" 			=> (Tokens.LPAREN(yypos,yypos+size(yytext)));
<INITIAL>";" 			=> (Tokens.SEMICOLON(yypos,yypos+size(yytext)));
<INITIAL>":" 			=> (Tokens.COLON(yypos,yypos+size(yytext)));
<INITIAL>"," 			=> (Tokens.COMMA(yypos,yypos+size(yytext)));
<INITIAL>[0-9]+ 		=> (Tokens.INT(intHandle(yypos,yytext),yypos,yypos+size yytext));
<INITIAL>{id}			=> (Tokens.ID(yytext,yypos,yypos+size(yytext)));
[\t\012\032]+			=> (continue());	
.						=> (ErrorMsg.error yypos ("ERROR: Illegal character " ^ yytext); 
						continue());