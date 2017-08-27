
#Region Constants

Var Keywords;                  // enum
Var Tokens;                    // enum
Var UnaryOperators;            // array (one of Tokens)
Var BasicLiterals;             // array (one of Tokens)
Var RelationalOperators;       // array (one of Tokens)
Var IgnoredTokens;             // array (one of Tokens)
Var InitialTokensOfExpression; // array (one of Tokens)
Var EmptyArray;                // array

#EndRegion // Constants

#Region Init

Procedure Init()

	InitEnums();

	UnaryOperators = New Array;
	UnaryOperators.Add(Tokens.Add);
	UnaryOperators.Add(Tokens.Sub);

	BasicLiterals = New Array;
	BasicLiterals.Add(Tokens.Number);
	BasicLiterals.Add(Tokens.String);
	BasicLiterals.Add(Tokens.True);
	BasicLiterals.Add(Tokens.False);
	BasicLiterals.Add(Tokens.Undefined);
	BasicLiterals.Add(Tokens.Null);

	RelationalOperators = New Array;
	RelationalOperators.Add(Tokens.Eql);
	RelationalOperators.Add(Tokens.Neq);
	RelationalOperators.Add(Tokens.Lss);
	RelationalOperators.Add(Tokens.Gtr);
	RelationalOperators.Add(Tokens.Leq);
	RelationalOperators.Add(Tokens.Geq);

	IgnoredTokens = New Array;
	IgnoredTokens.Add(Tokens.Comment);

	InitialTokensOfExpression = New Array;
	InitialTokensOfExpression.Add(Tokens.Add);
	InitialTokensOfExpression.Add(Tokens.Sub);
	InitialTokensOfExpression.Add(Tokens.Not);
	InitialTokensOfExpression.Add(Tokens.Ident);
	InitialTokensOfExpression.Add(Tokens.Lparen);
	InitialTokensOfExpression.Add(Tokens.Number);
	InitialTokensOfExpression.Add(Tokens.String);
	InitialTokensOfExpression.Add(Tokens.Case);
	InitialTokensOfExpression.Add(Tokens.True);
	InitialTokensOfExpression.Add(Tokens.False);
	InitialTokensOfExpression.Add(Tokens.Undefined);
	InitialTokensOfExpression.Add(Tokens.Null);

	EmptyArray = New Array;

EndProcedure // Init()

Procedure InitEnums()
	Keywords = Keywords();
	Tokens = Tokens(Keywords);
	ObjectKinds = ObjectKinds();
EndProcedure // InitEnums()

#EndRegion // Init

#Region Enums

Function Keywords() Export
	Var Keywords;

	Keywords = Enum(New Structure,
		"Select.Выбрать, Allowed.Разрешенные, Distinct.Различные, Top.Первые, As.Как, EmptyTable.ПустаяТаблица, Into.Поместить,
		|From.Из, Inner.Внутреннее, Left.Левое, Right.Правое, Full.Полное, Outer.Внешнее, Join.Соединение,
		|Index.Индексировать, Where.Где, Group.Сгруппировать, Having.Имеющие, For.Для, Update.Изменения,
		|Union.Объединить, All.Все, Order.Упорядочить, Asc.Возр, Desc.Убыв, Autoorder.Автоупорядочивание,
		|Totals.Итоги, Overall.Общие, Only.Только, Periods.Периодами, Drop.Уничтожить,
		|
		|Case.Выбор, When.Когда, Then.Тогда, Else.Иначе, End.Конец,
		|
		|Count.Количество, Avg.Среднее, Sum.Сумма, Max.Максимум, Min.Минимум, 
		|
		|And.И, Or.Или, Not.Не,
		|Between.Между, In.В, Is.Есть, IsNull.ЕстьNull, Like.Подобно, Refs.Ссылка,
		|Cast.Выразить, BooleanType.Булево, StringType.Строка, NumberType.Число,
		|True.Истина, False.Ложь, Undefined.Неопределено,
		|
		|Year.Год, Halfyear.Полугодие, Quarter.Квартал, Month.Месяц, Tendays.Декада,
		|Week.Неделя, Day.День, Hour.Час, Minute.Минута, Second.Секунда,
		|
		|Substring.Подстрока, BeginOfPeriod.НачалоПериода, EndOfPeriod.КонецПериода,
		|DayOfYear.ДеньГода, WeekDay.ДеньНедели, 
		|Date.Дата, DateAdd.ДобавитьКДате, DateDiff.РазностьДат, DateTime.ДатаВремя,
		|
		|Escape.Спецсимвол, Presentation.Представление, RefPresentation.ПредставлениеСсылки, Type.Тип, Value.Значение, ValueType.ТипЗначения,     
		|
		//бастарды:
		|On, By, По, Of, Hierarchy, Иерархии, Иерархия, Null"
	);

	Return Keywords;
EndFunction // Keywords()

Function Tokens(Keywords = Undefined) Export
	Var Tokens;

	If Keywords = Undefined Then
		Keywords = Keywords();
	EndIf;

	Tokens = Enum(New Structure(Keywords),

		// Literals

		"Ident, Number, String,
		
		// Operators

		// =   <>    <    >   <=   >=    +    -    *    /
		|Eql, Neq, Lss, Gtr, Leq, Geq, Add, Sub, Mul, Div,
		//    (       ) 
		|Lparen, Rparen,
		//   ,       .          ;
		|Comma, Period, Semicolon,

		// Other

		//         //          &
		|Eof, Comment, Parameter"

	);

	Return Tokens;
EndFunction // Tokens()

Function ObjectKinds() Export
	Var ObjectKinds;

	ObjectKinds = Enum(New Structure,
		"Variable,"
		"Parameter,"
		"Procedure,"
		"Function,"
		"Constructor,"
		"Unknown,"
	);

	Return ObjectKinds;
EndFunction // ObjectKinds()

Function Enum(Structure, Keys)
	Var ItemList, Value;

	For Each Items In StrSplit(Keys, ",", False) Do
		ItemList = StrSplit(Items, ".", False);
		Value = TrimAll(ItemList[0]);
		For Each Item In ItemList Do
			Structure.Insert(Item, Value);
		EndDo;
	EndDo;

	Return New FixedStructure(Structure);
EndFunction // Enum()

#EndRegion // Enums

#Region Scanner

Function Scanner(Source) Export
	Var Scanner;

	Scanner = New Structure(
		"Source," // string
		"Len,"    // number
		"Pos,"    // number
		"Tok,"    // string (one of Tokens)
		"Lit,"    // string
		"Char,"   // string
		"Line,"   // number
	);

	Scanner.Source = Source;
	Scanner.Len = StrLen(Source);
	Scanner.Line = 1;
	Scanner.Pos = 0;
	Scanner.Lit = "";

	Init();

	Return Scanner;
EndFunction // Scanner()

Function Scan(Scanner) Export
	Var Char, Tok, Lit;
	SkipWhitespace(Scanner);
	Char = Scanner.Char;
	If IsLetter(Char) Then
		Lit = ScanIdentifier(Scanner);
		Tok = Lookup(Lit);
	ElsIf IsDigit(Char) Then
		Lit = ScanNumber(Scanner);
		Tok = Tokens.Number;
	ElsIf Char = """" Then
		Lit = ScanString(Scanner);
		Tok = Tokens.String;
	ElsIf Char = "=" Then
		Tok = Tokens.Eql;
		NextChar(Scanner);
	ElsIf Char = "<" Then
		If NextChar(Scanner) = "=" Then
			Lit = "<=";
			Tok = Tokens.Leq;
			NextChar(Scanner);
		ElsIf Scanner.Char = ">" Then
			Lit = "<>";
			Tok = Tokens.Neq;
			NextChar(Scanner);
		Else
			Tok = Tokens.Lss;
		EndIf;
	ElsIf Char = ">" Then
		If NextChar(Scanner) = "=" Then
			Lit = ">=";
			Tok = Tokens.Geq;
			NextChar(Scanner);
		Else
			Tok = Tokens.Gtr;
		EndIf;
	ElsIf Char = "+" Then
		Tok = Tokens.Add;
		NextChar(Scanner);
	ElsIf Char = "-" Then
		Tok = Tokens.Sub;
		NextChar(Scanner);
	ElsIf Char = "*" Then
		Tok = Tokens.Mul;
		NextChar(Scanner);
	ElsIf Char = "/" Then
		If NextChar(Scanner) = "/" Then
			Lit = ScanComment(Scanner);
			Tok = Tokens.Comment;
		Else
			Tok = Tokens.Div;
		EndIf;
	ElsIf Char = "(" Then
		Tok = Tokens.Lparen;
		NextChar(Scanner);
	ElsIf Char = ")" Then
		Tok = Tokens.Rparen;
		NextChar(Scanner);
	ElsIf Char = "," Then
		Tok = Tokens.Comma;
		NextChar(Scanner);
	ElsIf Char = "." Then
		Tok = Tokens.Period;
		NextChar(Scanner);
	ElsIf Char = ";" Then
		Tok = Tokens.Semicolon;
		NextChar(Scanner);
	ElsIf Char = "" Then
		Tok = Tokens.Eof;
	ElsIf Char = "&" Then
		NextChar(Scanner);
		Lit = ScanIdentifier(Scanner);
		Tok = Tokens.Parameter;
	Else
		Error(Scanner, "Unknown char");
	EndIf;
	If ValueIsFilled(Lit) Then
		Scanner.Lit = Lit;
	Else
		Scanner.Lit = Char;
	EndIf;
	Scanner.Tok = Tok;
	Return Tok;
EndFunction // Scan()

Function NextChar(Scanner)
	If Scanner.Char <> "" Then
		Scanner.Pos = Scanner.Pos + 1;
		Scanner.Char = Mid(Scanner.Source, Scanner.Pos, 1);
	EndIf;
	Return Scanner.Char;
EndFunction // NextChar()

Function SkipWhitespace(Scanner)
	Var Char;
	Char = Scanner.Char;
	While IsBlankString(Char) And Char <> "" Do
		If Char = Chars.LF Then
			Scanner.Line = Scanner.Line + 1;
		EndIf;
		Char = NextChar(Scanner);
	EndDo;
EndFunction // SkipWhitespace()

Function ScanComment(Scanner)
	Var Len, Char;
	Len = 0;
	Char = NextChar(Scanner);
	While Char <> Chars.LF And Char <> "" Do
		Len = Len + 1;
		Char = NextChar(Scanner);
	EndDo;
	Return Mid(Scanner.Source, Scanner.Pos - Len, Len);
EndFunction // ScanComment()

Function ScanIdentifier(Scanner)
	Var Len, Char;
	Len = 1;
	Char = NextChar(Scanner);
	While IsLetter(Char) Or IsDigit(Char) Do
		Len = Len + 1;
		Char = NextChar(Scanner);
	EndDo;
	Return Mid(Scanner.Source, Scanner.Pos - Len, Len);
EndFunction // ScanIdentifier()

Function ScanNumber(Scanner)
	Var Len;
	Len = ScanIntegerLen(Scanner); // Len >= 1
	If Scanner.Char = "." Then
		Len = Len + ScanIntegerLen(Scanner);
	EndIf;
	Return Mid(Scanner.Source, Scanner.Pos - Len, Len);
EndFunction // ScanNumber()

Function ScanIntegerLen(Scanner)
	Var Len;
	Len = 1;
	While IsDigit(NextChar(Scanner)) Do
		Len = Len + 1;
	EndDo;
	Return Len;
EndFunction // ScanIntegerLen()

Function ScanString(Scanner)
	Var Len;
	Len = ScanStringLen(Scanner);
	While NextChar(Scanner) = """" Do
		Len = Len + ScanStringLen(Scanner);
	EndDo;
	Return Mid(Scanner.Source, Scanner.Pos - Len, Len);
EndFunction // ScanString()

Function ScanStringLen(Scanner)
	Var Len, Char;
	Len = 1;
	Char = NextChar(Scanner);
	While Char <> """" And Char <> Chars.LF And Char <> "" Do
		Len = Len + 1;
		Char = NextChar(Scanner);
	EndDo;
	If Char = Chars.LF Then
		Scanner.Line = Scanner.Line + 1;
	EndIf;
	Return Len + ?(Char <> "", 1, 0);
EndFunction // ScanStringLen()

#EndRegion // Scanner

#Region AbstractSyntaxTree

Function QueryBatch(Items)
	Var QueryBatch;
	
	QueryBatch = New Structure(
		"NodeType," // string (type of this structure)
		"Items"     // array
	,
	"QueryBatch", Items);
	
	Return QueryBatch;
EndFunction // QueryBatch()

#Region Expressions

Function BasicLitExpr(Kind, Value)
	Var BasicLitExpr;

	BasicLitExpr = New Structure(
		"NodeType," // string (type of this structure)
		"Kind,"     // string (one of Tokens)
		"Value,"    // one of basic types
	,
	"BasicLitExpr", Kind, Value);

	Return BasicLitExpr;
EndFunction // BasicLitExpr()

Function DesignatorExpr(Object, Selectors)
	Var DesignatorExpr;

	DesignatorExpr = New Structure(
		"NodeType,"  // string (type of this structure)
		"Object,"    // string
		"Selectors," // array (string)
	,
	"DesignatorExpr", Object, Selectors);

	Return DesignatorExpr;
EndFunction // DesignatorExpr()

Function UnaryExpr(Operator, Operand)
	Var UnaryExpr;

	UnaryExpr = New Structure(
		"NodeType," // string (type of this structure)
		"Operator," // string (one of Tokens)
		"Operand,"  // structure (one of expressions)
	,
	"UnaryExpr", Operator, Operand);

	Return UnaryExpr;
EndFunction // UnaryExpr()

Function BinaryExpr(Left, Operator, Right)
	Var BinaryExpr;

	BinaryExpr = New Structure(
		"NodeType," // string (type of this structure)
		"Left,"     // structure (one of expressions)
		"Operator," // string (one of Tokens)
		"Right,"    // structure (one of expressions)
	,
	"BinaryExpr", Left, Operator, Right);

	Return BinaryExpr;
EndFunction // BinaryExpr()

Function CaseExpr(Condition, WhenPart, ElsePart = Undefined)
	Var CaseExpr;

	CaseExpr = New Structure(
		"NodeType,"  // string (type of this structure)
		"Condition," // structure (one of expressions)
		"WhenParts," // array (WhenPart)
	,
	"CaseExpr", Condition, WhenPart);

	If ElsePart <> Undefined Then
		CaseExpr.Insert("ElsePart", ElsePart); // array (one of statements)
	EndIf;

	Return CaseExpr;
EndFunction // CaseExpr()

Function ParenExpr(Expr)
	Var ParenExpr;

	ParenExpr = New Structure(
		"NodeType," // string (type of this structure)
		"Expr,"     // structure (one of expressions)
	,
	"ParenExpr", Expr);

	Return ParenExpr;
EndFunction // ParenExpr()

Function NotExpr(Expr)
	Var NotExpr;

	NotExpr = New Structure(
		"NodeType," // string (type of this structure)
		"Expr,"     // structure (one of expressions)
	,
	"NotExpr", Expr);

	Return NotExpr;
EndFunction // NotExpr()

#EndRegion // Expressions

#Region Statements 

Function SelectStmt(
		Allowed,
		Distinct,
		Fields,
		Top = Undefined,
		Into = Undefined,
		From = Undefined,
		Where = Undefined,
		Group = Undefined,
		Having = Undefined,
		ForUpdate = Undefined,
		Index = Undefined
	)
	Var SelectStmt;

	SelectStmt = New Structure(
		"NodeType,"  // string (type of this structure)
		"Allowed,"   // boolean
		"Distinct,"  // boolean
		"Fields,"    // array (Field)
		
	,
	"SelectStmt", Allowed, Distinct, Fields);

	If Top <> Undefined Then
		SelectStmt.Insert("Top", Top); // number
	EndIf;
	
	If Into <> Undefined Then
		SelectStmt.Insert("Into", Into); // number
	EndIf;
	
	If From <> Undefined Then
		SelectStmt.Insert("From", From); // array (TableExpr)
	EndIf;
	
	If Where <> Undefined Then
		SelectStmt.Insert("Where", Where); // structure (one of expressions)
	EndIf;
	
	If Group <> Undefined Then
		SelectStmt.Insert("Group", Group); // array (DesignatorExpr or number)
	EndIf;
	
	If Having <> Undefined Then
		SelectStmt.Insert("Having", Having); // array (one of expressions)
	EndIf;
	
	If ForUpdate <> Undefined Then
		SelectStmt.Insert("ForUpdate", ForUpdate); // array (DesignatorExpr)
	EndIf;
	
	If Index <> Undefined Then
		SelectStmt.Insert("Index", Index); // array (DesignatorExpr or number)
	EndIf;	
	
	Return SelectStmt;
EndFunction // SelectStmt()

Function UnionStmt()
	Var UnionStmt;

	UnionStmt = New Structure(
		"NodeType," // string (type of this structure)
		"Items,"    // array (SelectStmt)
	,
	"UnionStmt");

	Return UnionStmt;
EndFunction // UnionStmt()

Function OrderStmt()
	Var OrderStmt;

	OrderStmt = New Structure(
		"NodeType," // string (type of this structure)
		"Items,"    // array (OrderExpr)
	,
	"OrderStmt");

	Return OrderStmt;
EndFunction // OrderStmt()

Function TotalsStmt()
	Var TotalsStmt;

	TotalsStmt = New Structure(
		"NodeType," // string (type of this structure)
		"Items,"    // array (OrderExpr)
	,
	"TotalsStmt");

	Return TotalsStmt;
EndFunction // TotalsStmt()

#EndRegion // Statements

#EndRegion // AbstractSyntaxTree

#Region Parser

Function Parser(Source) Export
	Var Parser;

	Parser = New Structure(
		"Scanner," // structure (Scanner)
		"Pos,"     // number
		"PrevPos," // number
		"Tok,"     // string (one of Tokens)
		"Lit,"     // string
		"Val,"     // number, string, date, true, false, undefined
		"Unknown," // structure as map[string](Object)
	);

	Parser.Scanner = Scanner(Source);
	Parser.Pos = 0;
	Parser.PrevPos = 0;
	Parser.Unknown = New Structure;

	Return Parser;

EndFunction // Parser()

Function Next(Parser)
	Var Scanner, Tok, Lit;
	Scanner = Parser.Scanner;
	Parser.PrevPos = Scanner.Pos;
	Tok = Scan(Scanner);
	While IgnoredTokens.Find(Tok) <> Undefined Do
		Tok = Scan(Scanner);
	EndDo;
	Lit = Scanner.Lit;
	Parser.Pos = Scanner.Pos - StrLen(Lit);
	Parser.Tok = Tok;
	Parser.Lit = Lit;
	Parser.Val = Value(Tok, Lit);
	Return Parser.Tok;
EndFunction // Next()

Function SkipIgnoredTokens(Parser)
	Var Tok;
	Tok = Parser.Tok;
	If IgnoredTokens.Find(Tok) <> Undefined Then
		Tok = Next(Parser);
	EndIf;
	Return Tok;
EndFunction // SkipIgnoredTokens()

Function ParseUnaryExpr(Parser)
	Var Operator, Expr, Pos;
	Pos = Parser.Pos;
	Operator = Parser.Tok;
	If UnaryOperators.Find(Parser.Tok) <> Undefined Then
		Next(Parser);
		Expr = UnaryExpr(Operator, ParseOperand(Parser));
	ElsIf Parser.Tok = Tokens.Eof Then
		Expr = Undefined;
	Else
		Expr = ParseOperand(Parser);
	EndIf;
	Return Locate(Expr, Parser, Pos);
EndFunction // ParseUnaryExpr()

Function ParseOperand(Parser)
	Var Tok, StrList, Operand;
	Tok = Parser.Tok;
	If BasicLiterals.Find(Tok) <> Undefined Then
		If Tok = Tokens.String Then
			StrList = New Array;
			StrList.Add(Parser.Val);
			While Next(Parser) = Tokens.String Do
				StrList.Add(Parser.Val);
			EndDo;
			Operand = BasicLitExpr(Tok, StrConcat(StrList, Chars.LF));
		Else
			Operand = BasicLitExpr(Tok, Parser.Val);
			Next(Parser);
		EndIf;
	ElsIf Tok = Tokens.Ident Then
		Operand = ParseDesignatorExpr(Parser);
	ElsIf Tok = Tokens.Lparen Then
		Next(Parser);
		Operand = ParenExpr(ParseExpression(Parser));
		Expect(Parser, Tokens.Rparen);
		Next(Parser);
	Else
		Raise "Expected operand";
	EndIf;
	Return Operand;
EndFunction // ParseOperand()

Function ParseDesignatorExpr(Parser)
	Var Object, Selectors, Pos;
	Pos = Parser.Pos;
	Object = Parser.Lit;
	Selectors = New Array;
	Next(Parser);
	While Parser.Tok = Tokens.Period Do
		Next(Parser);
		Selectors.Add(Parser.Lit);
		Next(Parser);
	EndDo; 
	Return Locate(DesignatorExpr(Object, Selectors), Parser, Pos);
EndFunction // ParseDesignatorExpr()

Function ParseExpression(Parser)
	Var Expr, Operator, Pos;
	Pos = Parser.Pos;
	Expr = ParseAndExpr(Parser);
	While Parser.Tok = Tokens.Or Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = Locate(BinaryExpr(Expr, Operator, ParseAndExpr(Parser)), Parser, Pos);
	EndDo;
	Return Expr;
EndFunction // ParseExpression()

Function ParseAndExpr(Parser)
	Var Expr, Operator, Pos;
	Pos = Parser.Pos;
	Expr = ParseNotExpr(Parser);
	While Parser.Tok = Tokens.And Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = Locate(BinaryExpr(Expr, Operator, ParseNotExpr(Parser)), Parser, Pos);
	EndDo;
	Return Expr;
EndFunction // ParseAndExpr()

Function ParseNotExpr(Parser)
	Var Expr, Pos;
	Pos = Parser.Pos;
	If Parser.Tok = Tokens.Not Then
		Next(Parser);
		Expr = Locate(NotExpr(ParseRelExpr(Parser)), Parser, Pos);
	Else
		Expr = ParseRelExpr(Parser);
	EndIf;
	Return Expr;
EndFunction // ParseNotExpr()

Function ParseRelExpr(Parser)
	Var Expr, Operator, Pos;
	Pos = Parser.Pos;
	Expr = ParseAddExpr(Parser);
	While RelationalOperators.Find(Parser.Tok) <> Undefined Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = Locate(BinaryExpr(Expr, Operator, ParseAddExpr(Parser)), Parser, Pos);
	EndDo;
	Return Expr;
EndFunction // ParseRelExpr()

Function ParseAddExpr(Parser)
	Var Expr, Operator, Pos;
	Pos = Parser.Pos;
	Expr = ParseMulExpr(Parser);
	While Parser.Tok = Tokens.Add Or Parser.Tok = Tokens.Sub Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = Locate(BinaryExpr(Expr, Operator, ParseMulExpr(Parser)), Parser, Pos);
	EndDo;
	Return Expr;
EndFunction // ParseAddExpr()

Function ParseMulExpr(Parser)
	Var Expr, Operator, Pos;
	Pos = Parser.Pos;
	Expr = ParseUnaryExpr(Parser);
	While Parser.Tok = Tokens.Mul Or Parser.Tok = Tokens.Div Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = Locate(BinaryExpr(Expr, Operator, ParseUnaryExpr(Parser)), Parser, Pos);
	EndDo;
	Return Expr;
EndFunction // ParseMulExpr()

Function ParseExprList(Parser, HeadExpr = Undefined)
	Var ExprList;
	If HeadExpr = Undefined Then
		HeadExpr = ParseExpression(Parser);
	EndIf;
	ExprList = New Array;
	ExprList.Add(HeadExpr);
	While Parser.Tok = Tokens.Comma And InitialTokensOfExpression.Find(Next(Parser)) <> Undefined Do
		ExprList.Add(ParseExpression(Parser));
	EndDo;
	Return ExprList;
EndFunction // ParseExprList()

Function ParseArguments(Parser)
	Var ExprList, ExpectExpression;
	ExprList = New Array;
	ExpectExpression = True;
	While ExpectExpression Do
		If InitialTokensOfExpression.Find(Parser.Tok) <> Undefined Then
			ExprList.Add(ParseExpression(Parser));
		Else
			ExprList.Add(Undefined);
		EndIf;
		If Parser.Tok = Tokens.Comma Then
			Next(Parser);
		Else
			ExpectExpression = False;
		EndIf;
	EndDo;
	Return ExprList;
EndFunction // ParseArguments()

Function ParseQueryBatch(Parser) Export
	Var Items;
	Items = New Array;
	Next(Parser);
	Items.Add(ParseSelectStmt(Parser));
	While Parser.Tok = Tokens.Comma Do
		Next(Parser);
		Items.Add(ParseSelectStmt(Parser));
	EndDo; 	
	Return QueryBatch(Items);
EndFunction 

Function Field(Expr, Name = Undefined)
	Var Field;
	
	Field = New Structure(
		"Expr,"
	,
	Expr);
	
	If Name <> Undefined Then
		Field.Insert("Name", Name);
	EndIf; 
	
	Return Field;
EndFunction // Field() 

Function ParseField(Parser)
	Var Expr, Name, Pos;
	Pos = Parser.Pos;
	Expr = ParseExpression(Parser);
	If Parser.Tok = Tokens.As Then
		Next(Parser);
		Expect(Parser, Tokens.Ident);
		Name = Parser.Lit;
		Next(Parser);
	EndIf; 
	Return Locate(Field(Expr, Name), Parser, Pos);
EndFunction // ParseField() 

Function ParseSelectStmt(Parser)
	Var Tok, Allowed, Distinct, Fields, Top, Into, From, Where, Group, Having, ForUpdate, Index;
	Fields = New Array;
	Expect(Parser, Tokens.Select);
	Tok = Next(Parser);
	Allowed = False;
	Distinct = False;
	GrabOptions = True;	
	While GrabOptions Do
		If Tok = Tokens.Allowed Then
			Allowed = True;
			Tok = Next(Parser);
		ElsIf Tok = Tokens.Distinct Then
			Distinct = True;
			Tok = Next(Parser);
		ElsIf Tok = Tokens.Top Then
			Next(Parser);
			Expect(Parser, Tokens.Number);
			Top = Parser.Val;
			Tok = Next(Parser);
		Else
			GrabOptions = False;
		EndIf; 		
	EndDo;	
	Fields.Add(ParseField(Parser));
	While Parser.Tok = Tokens.Comma Do
		Next(Parser);
		Fields.Add(ParseField(Parser));
	EndDo; 
	Tok = Parser.Tok;
	If Tok = Tokens.Into Then
		Next(Parser);
		Expect(Parser, Tokens.Ident);
		Into = Parser.Lit;
		Tok = Next(Parser);
	EndIf; 
	If Tok = Tokens.From Then
		Next(Parser);
		From = New Array;
		From.Add(ParseTableExpr(Parser));
		While Parser.Tok = Tokens.Comma Do
			Next(Parser);
			From.Add(ParseTableExpr(Parser));
		EndDo;
	EndIf; 
	If Parser.Tok = Tokens.Where Then
		Where = ParseExpression(Parser);
	EndIf;
	Return SelectStmt(Allowed, Distinct, Fields, Top, Into, From, Where, Group, Having, ForUpdate, Index);
EndFunction // ParseSelectStmt() 

Function ParseTableExpr(Parser)
	Var Table, Name, Join, Pos;
	Pos = Parser.Pos;
	Table = ParseDesignatorExpr(Parser);
	If Parser.Tok = Tokens.As Then
		Next(Parser);
		Expect(Parser, Tokens.Ident);
		Name = Parser.Lit;
		Next(Parser);
	EndIf; 
	Join = ParseJoinExpr(Parser);
	Return Locate(TableExpr(Table, Name, Join), Parser, Pos);
EndFunction // ParseTableExpr()

Function ParseJoinExpr(Parser)
	Var JoinExpr, Kind, Table, Condition, Pos;
	Pos = Parser.Pos;
	Kind = Parser.Tok;
	If Kind = Tokens.Left
		Or Kind = Tokens.Full
		Or Kind = Tokens.Inner
		Or Kind = Tokens.Right Then
		Next(Parser);
		Expect(Parser, Tokens.Join);
		Next(Parser);
		Table = ParseTableExpr(Parser);
		Expect(Parser, Tokens.On);
		Next(Parser);
		Condition = ParseExpression(Parser);
		JoinExpr = JoinExpr(Kind, Table, Condition, ParseJoinExpr(Parser))
	//ElsIf Kind = Tokens.Comma Then
	//	Next(Parser);
	//	Table = ParseTableExpr(Parser);
	//	JoinExpr = JoinExpr(Kind, Table)
	ElsIf Kind = Tokens.Join Then
		Next(Parser);
		Table = ParseTableExpr(Parser);
		Expect(Parser, Tokens.On);
		Next(Parser);
		Condition = ParseExpression(Parser);
		JoinExpr = JoinExpr(Tokens.Inner, Table, Condition, ParseJoinExpr(Parser))
	EndIf;
	Return Locate(JoinExpr, Parser, Pos);
EndFunction // ParseJoinExpr()

Function TableExpr(Table, Name, Join = Undefined)
	Var TableExpr;
	
	TableExpr = New Structure(
	    "NodeType," // string (type of this structure)
		"Table,"    // structure (DesignatorExpr)
	,
	"TableExpr", Table);
	
	If Name <> Undefined Then
		TableExpr.Insert("Name", Name); // string
	EndIf;
	
	If Join <> Undefined Then
		TableExpr.Insert("Join", Join); // structure (JoinExpr)
	EndIf;
	
	Return TableExpr;
EndFunction // TableExpr() 

Function JoinExpr(Kind, Table, Condition = Undefined, Join = Undefined)
	Var JoinExpr;
	
	JoinExpr = New Structure(
		"NodeType," // string (type of this structure)
		"Kind,"     // string (one of Tokens)
		"Table,"    // structure (TableExpr)
	,
	"JoinExpr", Kind, Table);
	
	If Condition <> Undefined Then
		JoinExpr.Insert("Condition", Condition); // structure (one of expressions)
	EndIf; 
	
	If Join <> Undefined Then
		JoinExpr.Insert("Join", Join); // structure (JoinExpr)
	EndIf;
	
	Return JoinExpr;
EndFunction // JoinExpr() 

#EndRegion // Parser

#Region Auxiliary

Function Locate(Node, Parser, Pos)
	If Node = Undefined Then
		Return Undefined;
	EndIf;
	If Location Then
		Node.Insert("Pos", Pos);
		Node.Insert("Len", Parser.PrevPos - Pos);
	EndIf; 
	If Debug Then
		Node.Insert("Str", Mid(Parser.Scanner.Source, Pos, Parser.PrevPos - Pos));
	EndIf;
	Return Node;
EndFunction // Locate()

Function Value(Tok, Lit)
	If Tok = Tokens.Number Then
		Return Number(Lit);
	ElsIf Tok = Tokens.String Then
		Return Mid(Lit, 2, StrLen(Lit) - 2);
	ElsIf Tok = Tokens.True Then
		Return True;
	ElsIf Tok = Tokens.False Then
		Return False;
	EndIf;
	Return Undefined;
EndFunction // Value()

Procedure Expect(Parser, Tok)
	If Parser.Tok <> Tok Then
		Error(Parser.Scanner, "Expected " + Tok,, True);
	EndIf;
EndProcedure // Expect()

Function Lookup(Lit)
	Var Tok;
	If Not Keywords.Property(Lit, Tok) Then
		Tok = Tokens.Ident;
	EndIf;
	Return Tok;
EndFunction // Lookup()

Function IsLetter(Char)
	Return Char <> "" And StrFind("_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZабвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ", Char) > 0;
EndFunction // IsLetter()

Function IsDigit(Char)
	Return "0" <= Char And Char <= "9";
EndFunction // IsDigit()

Procedure Error(Scanner, Note, Pos = Undefined, Stop = False)
	Var ErrorText;
	If Pos = Undefined Then
		Pos = Scanner.Pos - StrLen(Scanner.Lit);
	EndIf;
	ErrorText = StrTemplate("[ Ln: %1; Col: %2 ] %3",
		StrOccurrenceCount(Mid(Scanner.Source, 1, Pos), Chars.LF) + 1,
		Pos - StrFind(Scanner.Source, Chars.LF, SearchDirection.FromEnd, Pos),
		Note
	);
	If Stop Then
		Raise ErrorText;
	Else
		Message(ErrorText);
	EndIf;
EndProcedure // Error()

#EndRegion // Auxiliary
