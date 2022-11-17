1) It is necessary to add the full directory path of this project to AutoCAD's search path.

	To add a search path

	1	From the Tools menu, choose Options.
	2	Select the "Support File Search Path" folder on the Files tab 
		and choose the Add button.
	3	Add a new search path beneath the project name by entering a new path, or choose 		
		Browse and select a new path.
	4	Choose OK or Apply.

	The new path is indented and placed beneath the project name.


2) At command line:
	(load "main_mdi")

	This LISP file will prompt you to select a number of drawings.  Hit Cancel button when
	satisfied with the set of DWGs you've designated.  The drawings will be sequentially opened
	in separate documents using MDI.  Upon start-up, each document will have access to a set 
	of utility functions (defined in the file doc_utils.lsp).  Further steps will be provided
	by issuing a "sample-instrs" at the command-line. 
	
3) As directed by the message delivered by the command "sample-instrs", run a "modify-trans-set" 
	command in each open document from which you're interested in extracting a selection set of 
	drawing objects to another document (see next step)
	
4) Type "load-vlxfile" in the new or open document that will receive copies of the selected entities
	from the presently open documents.  This command loads a VLX application which has one exposed 
	command, "import_objs".  Make sure you issue this command in the same document into which
	you've loaded the VLX, because the application will not be visible except in the documents
	into which it's been explicitly loaded.

	You will be prompted to select an open document from which the selected entities will be imported. 
	The process can be repeated for any open drawings with a selection set created by the prior invocations
	of "modify-trans-set".