;(function()
{
	// CommonJS
	SyntaxHighlighter = SyntaxHighlighter || (typeof require !== 'undefined'? require('shCore').SyntaxHighlighter : null);

	function Brush()
	{
		var funcs	=	'AAdd Abs AChoice AClone ADel AEval AFill AIns Alert Alias AllTrim AltD ApplyDefau Array Asc AScan ASize ASort At ATail Bin2L Bin2W Bof Break Browse Chr CMonth Col ColorSelect CToD CurDir Date Day dbAppend dbCloseAll dbCloseArea dbCommit dbCreate dbDelete dbEval dbFilter dbGoBottom dbGoto dbGoTop dbInfo dbOrderInfo dbRecall dbRecordInfo dbRLock dbRLockList dbRUnlock dbSeek dbSelectArea dbSetIndex dbSkip dbStruct dbUnlock dbUseArea Deleted DevOut DevPos Directory DispBegin DispBox DispEnd DispOutAt DoW DToC DToS Empty Eof ErrorBlock ErrorLevel ErrorNew Eval Exp FClose FCount FCreate FErase FError FieldGet FieldName FieldPos FieldPut File FLock FOpen Found FRead FReadStr FSeek FWrite GetActive GetEnv GUIReader hb_ADel hb_AIns hb_AParams hb_argv hb_AScan hb_asciiUpper hb_At hb_ATokens hb_base64Decode hb_base64Encode hb_BChar hb_BCode hb_bitAnd hb_bitOr hb_bitShift hb_BLeft hb_BLen hb_BSubStr hb_cdpSelect hb_ColorIndex hb_ColorToN hb_CurDrive hb_cwd hb_CStr hb_DateTime hb_dbDrop hb_dbExists hb_default hb_Deserialize hb_DirBase hb_DirBuild hb_DirCreate hb_DirExists hb_DirScan hb_DirSepAdd hb_DirSepDel hb_DirSepToOS hb_DispBox hb_DispOutAt hb_DispOutAtBox hb_DynCall hb_eol hb_ExecFromArray hb_FCopy hb_FCreate hb_FGetDateTime hb_FileExists hb_FileMatch hb_FNameDir hb_FNameExt hb_FNameExtSet hb_FNameExtSetDef hb_FNameMerge hb_FNameName hb_FNameNameExt hb_FNameSplit hb_FTempCreateEx hb_GetEnv hb_gfxPrimitive hb_gtInfo hb_gtSelect hb_gtVersion hb_HCaseMatch hb_HDel hb_HEval hb_HexToNum hb_HGet hb_HGetDef hb_HHasKey hb_HKeepOrder hb_HKeyAt hb_HKeys hb_HPos hb_hrbLoad hb_HScan hb_HSet hb_HSetCaseMatch hb_HValueAt hb_idleAdd hb_idleDel hb_idleSleep hb_inetClose hb_inetErrorCode hb_inetRecvAll hb_inetSendAll hb_inetTimeout HB_ISARRAY HB_ISBLOCK HB_ISDATE hb_IsFunction HB_ISHASH HB_ISLOGICAL HB_ISNIL HB_ISNUMERIC HB_ISOBJECT HB_ISSTRING hb_keyChar hb_keyCode hb_keyIns hb_keyPut hb_langErrMsg hb_langMessage hb_langSelect hb_libName hb_macroBlock hb_MemoRead hb_MemoWrit hb_MilliSeconds hb_mtvm hb_mutexCreate hb_mutexLock hb_mutexNotify hb_mutexSubscribe hb_mutexUnlock hb_ntos hb_NumToHex hb_osDriveSeparator hb_osFileMask hb_osPathListSeparator hb_PathNormalize hb_processRun hb_ProgName hb_ps hb_PValue hb_Random hb_RAt hb_rddInfo hb_regex hb_regexAll hb_regexComp hb_regexSplit hb_Scroll hb_SecondsCPU hb_Serialize hb_SetEnv hb_Shadow hb_socketClose hb_socketGetError hb_SToD hb_StrFormat hb_StrToHex hb_threadID hb_threadJoin hb_threadOnce hb_threadQuitRequest hb_threadStart hb_tokenGet hb_TSToStr hb_TToC hb_UTF8ToStr hb_UTF8ToStrBox hb_ValToExp hb_ValToStr hb_Version HBClass HBTextLine I2Bin Inkey Int IsAlpha IsDefColor IsDigit L2Bin LastKey LastRec Left Len Log Lower LTrim Max MaxCol MaxRow MCol MemoEdit MemoLine MemoRead Memory Min MLCount Month MRow MSetCursor NetErr NetName NextKey ordCreate ordDescend ordKeyCount ordKeyNo ordScope ordSetFocus ordSkipUnique OS OutErr OutStd PadC PadL PadR PCol PCount ProcFile ProcLine ProcName PRow QOut QQOut RangeCheck RAt rddInfo rddList rddName rddRegister rddSetDefault ReadInsert ReadModal ReadVar RecCount RecNo Replicate RestScreen Right Round Row RTrim SaveScreen Seconds Select Set SetBlink SetColor SetCursor SetKey SetMode SetPos Space Sqrt Str StrTran StrZero Stuff SubStr sx_TagOrder TBColumnNew TBrowseNew Time Tone Transform Type Upper UR_SUPER_ERROR Used USRRDD_AREADATA Val ValType Version Year';

		var keywords =	'IF ELSE ELSEIF END ENDIF DO WHILE ENDDO WITH CASE OTHERWISE ENDCASE BEGIN ANNOUNCE REQUEST THREAD DYNAMIC EXTERNAL '+
						'FUNCTION PROCEDURE RETURN CLASS ENDCLASS METHOD DATA LOCAL PRIVATE PUBLIC STATIC FIELD MEMVAR PARAMETERS DECLARE '+
						'ACCEPT APPEND AVERAGE CLEAR CLOSE COMMIT CONTINUE COPY COUNT CREATE DEFAULT '+
						'DELETE DISPLAY EJECT ERASE EXIT FOR GO GOTO INDEX INIT INPUT JOIN KEYBOARD LABEL LIST LOCATE '+
						'LOOP MENU NEXT PACK PRINT QUIT READ RECALL REINDEX RELEASE RENAME REQUEST REPLACE RESTORE '+
						'RUN SAVE SEEK SELECT SET SKIP SORT STORE SUM TEXT TOTAL UNLOCK USE VAR WAIT ZAP';

		var constants	= '__FILE__ __LINE__ __HARBOUR__';

		this.regexList = [
			{ regex: SyntaxHighlighter.regexLib.singleLineCComments,	css: 'comments' },			// one line comments
			{ regex: SyntaxHighlighter.regexLib.multiLineCComments,		css: 'comments' },			// multiline comments
			{ regex: SyntaxHighlighter.regexLib.doubleQuotedString,		css: 'string' },			// double quoted strings
			{ regex: SyntaxHighlighter.regexLib.singleQuotedString,		css: 'string' },			// single quoted strings
			{ regex: /\$\w+/g,											css: 'variable' },			// variables
			{ regex: /^ *#.*/gm,										css: 'preprocessor' },
			{ regex: new RegExp(this.getKeywords(funcs), 'gmi'),		css: 'functions' },			// common functions
			{ regex: new RegExp(this.getKeywords(constants), 'gmi'),	css: 'constants' },			// constants
			{ regex: new RegExp(this.getKeywords(keywords), 'gm'),		css: 'keyword' }			// keyword
			];

		this.forHtmlScript(SyntaxHighlighter.regexLib.phpScriptTags);
	};

	Brush.prototype	= new SyntaxHighlighter.Highlighter();
	Brush.aliases	= ['harbour'];

	SyntaxHighlighter.brushes.Php = Brush;

	// CommonJS
	typeof(exports) != 'undefined' ? exports.Brush = Brush : null;
})();
