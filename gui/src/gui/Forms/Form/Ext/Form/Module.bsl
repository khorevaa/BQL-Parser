﻿
&AtServer
Procedure OnCreateAtServer(Cancel, StandardProcessing)

	If StrFind(InfoBaseConnectionString(), "File=") = 0 Then
		Message("Only for file bases")
	EndIf; 
	
	Output = "AST";
	
	SetVisibilityOfAttributes(ThisObject);
	
EndProcedure // OnCreateAtServer()

&AtClient
Procedure Translate(Command)
	
	Result.Clear();
	ClearMessages();
	TranslateAtServer();
	
EndProcedure // Translate()

&AtServer
Procedure TranslateAtServer()
	Var Start;
	
	This = FormAttributeToValue("Object");
	ThisFile = New File(This.UsedFileName);
	
	OneShellProcessor = ExternalDataProcessors.Create(ThisFile.Path + "QL-Parser.epf", False);
	
	OneShellProcessor.Verbose = Verbose;
	OneShellProcessor.Location = Location;
	OneShellProcessor.Debug = Debug;
	
	Start = CurrentUniversalDateInMilliseconds();
	
	If Output = "Lexems" Then
		
		Eof = OneShellProcessor.Tokens().Eof;
		
		Scanner = OneShellProcessor.Scanner(Source.GetText());
		While OneShellProcessor.Scan(Scanner) <> Eof Do
			Result.AddLine(StrTemplate("%1: %2 -- `%3`", Scanner.Line, Scanner.Tok, Scanner.Lit));
		EndDo;
		
	ElsIf Output = "AST" Then
		
		Parser = OneShellProcessor.Parser(Source.GetText());
		QueryBatch = OneShellProcessor.ParseQueryBatch(Parser);
		JSONWriter = New JSONWriter;
		FileName = GetTempFileName(".json");
		JSONWriter.OpenFile(FileName,,, New JSONWriterSettings(, Chars.Tab));
		WriteJSON(JSONWriter, QueryBatch);
		JSONWriter.Close();
		Result.Read(FileName, TextEncoding.UTF8);		
		
	ElsIf Output = "Backend" Then
		
		BackendProcessor = ExternalDataProcessors.Create(BackendPath, False);
		BackendProcessor.Init(OneShellProcessor);
		Parser = OneShellProcessor.Parser(Source.GetText());
		QueryBatch = OneShellProcessor.ParseQueryBatch(Parser);
		BackendResult = BackendProcessor.VisitModule(QueryBatch); 
		Result.SetText(BackendResult);
		
	EndIf;
	
	If Measure Then
		Message(StrTemplate("%1 sec.", (CurrentUniversalDateInMilliseconds() - Start) / 1000));
	EndIf;
		
EndProcedure // TranslateAtServer() 

&AtClientAtServerNoContext
Procedure SetVisibilityOfAttributes(ThisObject, Reason = Undefined)
	
	Items = ThisObject.Items;
		
	If Reason = Items.Output Or Reason = Undefined Then
		
		Items.BackendPath.Visible = (ThisObject.Output = "Backend");
		
	EndIf;
	
EndProcedure // SetVisibilityOfAttributes()

&AtClient
Procedure OutputOnChange(Item)
	
	SetVisibilityOfAttributes(ThisObject, Item);
	
EndProcedure // OutputOnChange()

&AtClient
Procedure BackendPathStartChoice(Item, ChoiceData, StandardProcessing)
	
	StandardProcessing = False;
	ChoosePath(Item, ThisObject, FileDialogMode.Open, "(*.epf)|*.epf");
	
EndProcedure // BackendPathStartChoice()

&AtClient
Procedure ChoosePath(Item, Form, DialogMode = Undefined, Filter = Undefined)
	
	If DialogMode = Undefined Then
		DialogMode = FileDialogMode.ChooseDirectory;
	EndIf; 
	
	FileOpeningDialog = New FileDialog(DialogMode);
	FileOpeningDialog.Filter = Filter;
	
	FileOpeningDialog.Show(New NotifyDescription("ChoosePathNotifyChoice", ThisObject));
	
EndProcedure // ChoosePath()

&AtClient
Procedure ChoosePathNotifyChoice(Result, AdditionalParameters) Export
	
	If Result <> Undefined Then
		BackendPath = Result[0];
	EndIf; 
	
EndProcedure // ChoosePathHandle()

