/*
 * $Id$
 */

/*
 * File......: FT_HELPC.PRG
 * Author....: Luiz Rafael Culik
 * Date......: $Date$
 * Revision..: $Revision$
 * Log file..: $Logfile:     $
 *
 *
 * Modification history:
 * ---------------------
 *
 *    V1.00 
 *    1/05/2000 Initial Version. Based on Leo Letendre FT_DOC
 *
 *    1/06/2000 Added the ProccAlso Function
 *
 *    1/08/2000  Fixed the Line between the Title and the Text
 *               Functions Description is now in font Arial size 12
 *    V1.01
 *    1/09/2000  Added RTF Source output Format
 *
 *    1/11/2000  Remove the code to add the Author name and Source file
 *                Name to the output file.
 *
 *     V1.02
 *    1/12/2000  Added suport for WWW output Format
 *              Striped out the "<" and ">" delimeter for WWW outPut,
 *              since the "<>" are HTML Command delimeters
 *              Output files names are in lower case to Linux Compatibility
 *
 *    1/13/2000  Added the link for the  HARBOUR GPL LICENSE
 */

/*
 * Harbour Project source code:
 * Ft_helpC document Extractoy
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#include "directry.ch"
#include "fileio.ch"
#include "inkey.ch"

* output lines on the screen

#define INFILELINE   10
#define MODULELINE   12
#define LINELINE     14
#define ERRORLINE    20
#define LONGLINE     78
#define LONGONELINE  66
#define CRLF HB_OSNewLine()
* The delimiter
#define DELIM   "$"                 // keyword delimiter

STATIC nReadHandle, nWriteHandle
STATIC aDirList
STATIC aDocInfo:={}
STATIC aLinkInfo:={}
STATIC aAuthorList:={}
STATIC lAscii:=.F.   // Create ascii output instead of NG/EH input
STATIC lContinuous:=.F.   // Create continuous ascii output instead of 
					// NG/EH input
STATIC lAuthor:=.T.		// Include author in output of ascii output
STATIC lRtf:=.F.
STATIC oRtf
STATIC oHtm
STATIC lNgi:=.F.
STATIC lOs2:=.F.
STATIC lWww:=.F.
STATIC lNorton:=.F.
STATIC aWWW:={}    
* The following variables are used to allow one to change the delimiter 
* around the keywords.

STATIC cDoc:=DELIM+"DOC"+DELIM          // DOC keyword
STATIC cEnd:=DELIM+"END"+DELIM          // END keyword
STATIC cFunc:=DELIM+"FUNCNAME"+DELIM    // FUNCNAME keyword
STATIC cCat:=DELIM+"CATEGORY"+DELIM     // CATEGORY keyword
STATIC cOne:=DELIM+"ONELINER"+DELIM     // ONELINER keyword
STATIC cSyn:=DELIM+"SYNTAX"+DELIM       // SYNTAX keyword
STATIC cArg:=DELIM+"ARGUMENTS"+DELIM    // ARGUMENTS keyword
STATIC cRet:=DELIM+"RETURNS"+DELIM      // RETURNS keyword
STATIC cDesc:=DELIM+"DESCRIPTION"+DELIM // DESCRIPTION keyword
STATIC cExam:=DELIM+"EXAMPLES"+DELIM    // EXAMPLES keyword
STATIC cSee:=DELIM+"SEEALSO"+DELIM      // SEEALSO keyword
STATIC cInc:=DELIM+"INCLUDE"+DELIM      // INCLUDE keyword
STATIC cComm:=DELIM+"COMMANDNAME"+DELIM     // COMMAND keyword
STATIC cCompl:=DELIM+"COMPLIANCE"+DELIM
STATIC cTest:=DELIM+'TESTS'+DELIM
STATIC cStatus:=DELIM+'STATUS'+DELIM
STATIC cPlat:=DELIM+'PLATFORMS'+DELIM
STATIC cFiles:=DELIM+'FILES'+DELIM
STATIC theHandle



FUNCTION FT_HELPC(cFlags,cLinkName,cAtFile,cDir)

* LOCAL variables: 
LOCAL aExtensions:={"*.PRG","*.C","*.ASM","*.CH"}
LOCAL i, j, nItem, cBatName
LOCAL nSec1
LOCAL cCompiler     // Compiler type
  // Include norton compatable switch for EH
LOCAL cCompileString // Compiler switches string
LOCAL lDone         // Done with a loop
LOCAL cMi, cLName, cFName  // Name params
LOCAL aName               // Tokenized name
LOCAL nLen                // Length of the token array
*
* Entry Point
*

*nSec1=SECONDS()
* Delete log file if present

IF FILE("DOCERR.LOG")
	DELETE FILE "DOCERR.LOG"
ENDIF

* See if flag is there

IF .NOT.EMPTY(cFlags)
	IF LEFT(cFlags,1)=="-".OR.LEFT(cFlags,1)=="/"
          IF (cFlags:=UPPER(RIGHT(cFlags,3)))=="TXT"
			lAscii=.T.
			lContinuous=.F.
          ELSEIF cFlags="HPC"
			lNorton=.T.
          ELSEIF cFlags="NGI"
               lNgi=.T.
          ELSEIF cFlags="OS2"
               lOs2=.T.
          ELSEIF cFlags="RTF"
               lRtf=.T.
          ELSEIF cFlags="HTM"
               lWww=.T.                          
          ELSEIF cFlags="DOC"
			lAscii=.T.
			lContinuous=.T.
			lAuthor=.F.
		ENDIF
	ELSE
		cAtFIle=cLinkName
		cLinkName=cFlags
	ENDIF
ENDIF

* Get the linkfile name and get the info in it

IF cLinkName=NIL
     ? "Syntax: FT_DOC [-txt][-con][-ngi][-doc][-HPC][-RTF][-HTM][-OS2] <linkname> [<ifile>]"
     ? "        Where -txt creates an ascii file instead of a Norton Guide"
     ? "              -con creates an ascii file without formfeeds"
     ? "              -HPC Helpc source file"
     ? "              -Ngi adds the -NG switch to EHC command for compile FOR DOS/WINDOWS/LINUX."
     ? "              -RTF Winhelp source Code For Windows" 
     ? "              -OS2 OS2 help Source Code For OS2"
     ? "              -HTM Generate HTML Output FOR WINDOWS/LINUX/OS2/MAC"
     ? "              -DOC creates continuous ascii file w/o author information"
	? " "
     ? "              NOTE: -txt,-con,-hpc,-ngi,-rtf,-os2,-www and -doc cannot be used together."
	? "              linkname is the name of the Norton Guide Link file"
	? "              iFile is a file containing a list of files to process"
     ? "                    otherwise *.PRG, *.C, *.ASM and *.CH are used"
	RETURN NIL
ENDIF

* check to see if input files are present
IF .NOT.FILE(cLinkName)
	? "Link file Not Found:",cLinkName
	RETURN NIL
ENDIF

IF .NOT.EMPTY(cAtFIle).AND..NOT.FILE(cAtFile)
	? "Indirect file Not Found:",cAtFile
	RETURN NIL
ENDIF

CLEAR SCREEN
SET CURSOR OFF

cCompiler=fill_Link_info(cLinkName)

* See if ngi subdirectory is present
IF lNorton
     IF EMPTY(DIRECTORY("hdf.*","D"))
          FT_MKDIR("hdf")
     ENDIF
ELSEIF lRtf
     IF EMPTY(DIRECTORY("rtf.*","D"))
          FT_MKDIR("rtf")
     ENDIF
ELSEIF lWww
     IF EMPTY(DIRECTORY("www.*","D"))
          FT_MKDIR("www")
     ENDIF

ENDIF
IF cAtFile=NIL  // use all files in directory

* Loop through each of the types of files

	FOR i=1 TO LEN(aExtensions)

* Get the list of

//                aDirList=DIRECTORY(&cDir+aExtensions[i])
            aDirList=DIRECTORY(aExtensions[i])

* If there are any files then process them

		IF LEN(aDirList)>0 

			IF lAscii
				ASCIIFiles()
               ELSEIF    lNorton
				ProcessFiles()
               ELSEIF  lRtf
                    ProcessRtf()
               ELSEIF  lWww
                    ProcessWww()

			ENDIF
		ENDIF
	NEXT
ELSE
* an indirect file was given so read it and use it

	aDirList=ReadAtFile(cAtFile)

* If there are any files then process them

	IF LEN(aDirList)>0 

		IF lAscii
			ASCIIFILES()
          ELSEIF    lNorton
               ProcessFiles()
          ELSEIF  lRtf
               ProcessRtf()
         ELSEIF  lWww
               ProcessWww()
		ENDIF
	ENDIF

ENDIF

* Now build text files for norton compiler based upon link file
* first sort based upon category and filename. Not Fast but easy.

@ INFILELINE,0 CLEAR TO INFILELINE,MAXCOL()
@ MODULELINE,0 CLEAR TO MODULELINE,MAXCOL()
@ LINELINE,0 CLEAR TO LINELINE,MAXCOL()
@ INFILELINE,30 SAY "Sorting input files"

ASORT(aDocInfo,,,{|a,b| UPPER(a[1]+" "+a[2])<UPPER(b[1]+" "+b[2])})


* Now actually build the info

@ INFILELINE,0 CLEAR TO INFILELINE,MAXCOL()
if lnorton
     @ INFILELINE,30 SAY "Assembling "+IIF(lAscii,"documentation","NG");
	+" input files"
elseif lRTF
     @ INFILELINE,30 SAY "Assembling "+IIF(lAscii,"documentation","WINHELP");
	+" input files"
elseif lWww
     @ INFILELINE,30 SAY "Assembling "+IIF(lAscii,"documentation","Html");
	+" input files"

endif
IF FILE("assembl.bat")
	DELETE FILE "assembl.bat"
ENDIF
SET ALTERNATE TO "assembl.bat"
SET ALTERNATE ON
SET CONSOLE OFF

? "@Echo OFF"
? "ECHO Assembling input files"
lfirst:=.t.
if lNorton
FOR i=1 TO LEN(aDocInfo)

* Find match

	nItem=ASCAN(aLinkInfo,{|a| UPPER(ALLTRIM(a[1]))==UPPER(ALLTRIM(aDocInfo[i,1]))})
	IF nItem>0

          IF i=1.OR..NOT.(ALLTRIM(aDocInfo[i-1,1])==ALLTRIM(aDocInfo[i,1]))
* Make the first copy
			? "ECHO Creating",aLinkinfo[nItem,2]
               ? "COPY hdf\"+ALLTRIM(aDocInfo[i,4])+ " HarDoc.hdf  > NUL"
               lfirst:=.f.
		ELSE
* This may be slow but I don't have to worry about line length
               ? "TYPE hdf\"+ALLTRIM(aDocInfo[i,4])+" >> HarDoc.hdf "
		ENDIF
		aLinkInfo[nItem,3]=.T.
	ELSE
* Write the error message
		SET ALTERNATE TO
		SET ALTERNATE OFF
		SET CONSOLE ON
		write_error("Category not found: "+aDocInfo[i,1],,,,aDocInfo[i,4])
		@ ERRORLINE,0 CLEAR TO ERRORLINE,MAXCOL()
		@ ERRORLINE,20 SAY "Category not found: "+aDocInfo[i,1]+" in "+aDocInfo[i,4]
		SET ALTERNATE TO "assembl.bat" ADDITIVE
		SET ALTERNATE ON
		SET CONSOLE OFF
	ENDIF

NEXT
ELSEIF lRtf
     nHpj:=FCREATE('HARBOUR.HPJ')
     FWRITE(nHpj,'[OPTIONS]'+CRLF)

     FWRITE(nHpj,'COMPRESS=HIGH'+CRLF) 

     FWRITE(nHpj,'REPORT=Yes'+CRLF)
     FWRITE(nHpj,'CONTENTS=OVERVIEW'+CRLF)
     FWRITE(nHpj,'TITLE=Harbour Winhelp'+CRLF)
     FWRITE(nHpj,'COPYRIGHT=Harbour (C) http://www.Harbour-project.org'+CRLF)
     FWRITE(nHpj,'HLP=.\harbour.hlp'+CRLF)
     FWRITE(nHpj,'ROOT='+CURDIR()+"\RTF"+CRLF)
     FWRITE(nHpj,'[FILES]'+CRLF)
FOR i=1 TO LEN(aDocInfo)

* Find match

	nItem=ASCAN(aLinkInfo,{|a| UPPER(ALLTRIM(a[1]))==UPPER(ALLTRIM(aDocInfo[i,1]))})
	IF nItem>0

          IF i=1.OR..NOT.(ALLTRIM(aDocInfo[i-1,1])==ALLTRIM(aDocInfo[i,1]))
* Make the first copy

               FWRITE(nHpj,ALLTRIM(aDocInfo[i,4])+CRLF)

		ELSE
* This may be slow but I don't have to worry about line length
               fWRITE(nHpj,ALLTRIM(aDocInfo[i,4])+CRLF)
		ENDIF
		aLinkInfo[nItem,3]=.T.
	ELSE
* Write the error message
		SET ALTERNATE TO
		SET ALTERNATE OFF
		SET CONSOLE ON
		write_error("Category not found: "+aDocInfo[i,1],,,,aDocInfo[i,4])
		@ ERRORLINE,0 CLEAR TO ERRORLINE,MAXCOL()
		@ ERRORLINE,20 SAY "Category not found: "+aDocInfo[i,1]+" in "+aDocInfo[i,4]
		SET ALTERNATE TO "assembl.bat" ADDITIVE
		SET ALTERNATE ON
		SET CONSOLE OFF
	ENDIF

NEXT
ELSEIF lWWW
     oHtm:=THTML():New("www\harbour.html")
     oHtm:WriteTitle("Harbour Reference Guide")
     oHtm:WritePar("HARBOUR")
     oHtm:WriteLink("OverView")
     oHtm:WriteLink("License")
     oHtm:WriteLink("http://www.gnu.org/copyleft/gpl")
     oHtm:WritePar("")
     oHtm:WritePar("Functions A-M")
     asort(awww)
     for nPos:=1 to len(aWww)
          cTemp:=aWww[nPos]
          if left(cTemp,1)>="A" .and. left(cTemp,1)<"N" .and. at("()",cTemp)>0
               oHtm:WriteLink(aWww[nPos])
          endif
     next
     oHtm:WritePar("Functions N-_")
     for nPos:=1 to len(aWww)
          cTemp:=aWww[nPos]
          if left(cTemp,1)>="N" .and. left(cTemp,1)<"_" .and. at("()",cTemp)>0
               oHtm:WriteLink(aWww[nPos])
          endif
     next
     oHtm:WritePar("Commands")
     for nPos:=1 to len(aWww)
          cTemp:=aWww[nPos]
          if  at("()",cTemp)==0 .and. ctemp<>"LICENSE" .and. cTemp<>"OVERVIEW"
               oHtm:WriteLink(aWww[nPos])
          endif
     next
oHtm:Close()
ENDIF

* Now assemble the output
IF .NOT. lAscii

	? "REM Compile the sources"
	? "Echo Compiling the sources"
if lNorton
//
//     FOR i=1 TO LEN(aLinkInfo)
//         IF aLinkInfo[i,3]
//                        cCompileString=IIF(cCompiler="NgxC","-E"+STRTRAN(aLinkInfo[i,2];
//                         ,".TXT",".ERR")+IIF(lNorton," -NG -NP"," -NP"),"")
//               ? cCompiler,aLinkInfo[i,2],cCompileString
               ? "Helpc /W31   hardoc.hdf"
//          ENDIF
//     NEXT

	? "REM  Link the files"
	? "Echo Linking library"
     // ? IIF(cCompiler="NGXC","Ngxl","NGXL"),cLinkName ;
     //         ,IIF(cCompiler="ngxc","-EEHLINK.ERR -NP","")
     ? "hcw hardoc.hpj"
     ? " "
elseif lRtf

	? "REM  Link the files"
	? "Echo Linking library"
     // ? IIF(cCompiler="NGXC","Ngxl","NGXL"),cLinkName ;
     //         ,IIF(cCompiler="ngxc","-EEHLINK.ERR -NP","")
     ? "hcw harbour.hpj"
     ? " "
endif
* Send out list of files

	@ INFILELINE,0 CLEAR TO INFILELINE,MAXCOL()
	@ INFILELINE,30 SAY "Writing summary file"

ENDIF

SET ALTERNATE TO "FT_DOC.LOG"
SET ALTERNATE ON
SET CONSOLE OFF
FOR i=1 TO LEN(aDocInfo)
	? PAD(aDocInfo[i,1],15),PAD(aDocInfo[i,2],15),PAD(aDocInfo[i,4],15)
NEXT

* Send out list of authors

@ INFILELINE,0 CLEAR TO INFILELINE,MAXCOL()
@ INFILELINE,30 SAY "Sorting Author file"

FOR i=1 TO LEN(aAuthorList)

	aName=ListAsArray(aAuthorList[i,1]," ")
	nLen=0
	aeval(aName,{|a,b| nLen:=IIF(!EMPTY(a),b,nLen)},1,LEN(aName))

	IF nLen>0
		cFName=aName[1]

		IF nLen>1
* Middle initial
			cMI=aName[2]
			IF !(LEN(cMi)=1.OR.(LEN(cMi)=2.AND.RIGHT(cMi,1)="."))
				cMi=NIL
			ENDIF
* Last name
			cLName=""
			FOR j=IIF(!EMPTY(cMi),3,2) TO nLen
					cLname:=ALLTRIM(cLName)+" "+aName[j]
			NEXT
			cLName:=LTRIM(cLName)
		ENDIF

* Add to array
		
		aAuthorList[i,3]=cLName+","+cFname+IIF(!EMPTY(cMi)," "+cMi," ")

	ENDIF

NEXT
* sort the list bring any CIS ID to the top so it gets printed out

ASORT(aAuthorList,,,{|a,b| IIF(UPPER(a[3])==UPPER(b[3]), a[2]>b[2] ,;
		UPPER(a[3])<UPPER(b[3]))})

@ INFILELINE,0 CLEAR TO INFILELINE,MAXCOL()
@ INFILELINE,30 SAY "Writing Author file"

IF LEN(aAuthorList)>1
	i=2
	lDone=.F.
	DO WHILE !lDone

		IF UPPER(ALLTRIM(aAuthorList[i,1]))==UPPER(ALLTRIM(aAuthorList[i-1,1]))

* Remove duplicate names but capture a CIS ID if we don't have one

			IF EMPTY(aAuthorList[i-1,2]).AND.!EMPTY(aAuthorList[i,2])
				aAuthorList[i-1,2]=aAuthorList[i,2]
			ENDIF
			ADEL(aAuthorList,i)
			ASIZE(aAuthorList,LEN(aAuthorList)-1)
		ELSE
			i++
		ENDIF
		lDone=(i>LEN(aAuthorList))
	ENDDO
ENDIF


* Now write it out

SET ALTERNATE TO "author.txt"
SET ALTERNATE ON
SET CONSOLE OFF

FOR i=1 TO LEN(aAuthorList)

? " ",aAuthorList[i,1], IIF(!EMPTY(aAuthorList[i,2]),"["+aAuthorList[i,2]+"]","")

NEXT

SET CONSOLE ON
SET ALTERNATE OFF
SET ALTERNATE TO

@MAXROW(),0 SAY "Execute ASSEMBL.BAT to compile and link Guides"

* Return to caller

RETURN NIL

* End of MAIN()

*+
STATIC FUNCTION ProcessFiles
*
* This routine and all accompaning database structures are 
* Copyright (C) 1992 Leo J. Letendre. Modified to generate
*  HELPC Source code By Luiz Rafael Culik
*
* Purpose: Process each of the files in the directory
*
* Modification History:
*        Version    Date      Who       Notes
*         V1.00     1/19/92   LJL       Initial Version
*         V1.01     2/25/92   LJL       Trimmed spaces from see also and header
*         V1.02     10/17/92  LJL       Added multi-line one-liner check
*         V1.03     10/22/92  LJL       Insured that there was a blank line
*                                       before and after each catagory header
*         V1.04     10/30/93  LJL       added changable delimiter support and
*                                       COMMANDNAME keyword
*
* Calling parameters: None
*
* Notes: None
*-
* LOCAL variables: 

#define D_NORMAL  1
#define D_ARG     2
#define D_SYNTAX  3
#define D_IGNORE  4
#define D_SEEALSO 5
#define D_INCLUDE 6
#define D_ONELINE 7
#define D_STATUS 8
LOCAL i, j, nFiles:=LEN(aDirList)
LOCAL nCommentLen
LOCAL lEof, lDoc, lDone
LOCAL cBuffer
LOCAL nEnd, nCount

LOCAL cBar:="컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�"+CRLF
LOCAL nMode
LOCAL cAuthor
LOCAL cCISID
LOCAL cFuncName
LOCAL cOneLine
LOCAL cCategory
LOCAL cFileName
LOCAL nLineCnt
LOCAL cSeeAlso
LOCAL cTemp, cChar
LOCAL nNonBlank
LOCAL lBlankLine:=.F.  // Blank line encountered and sent out
LOCAL lAddBlank:=.F.   // Need to add a blank line if next line is not blank

*
* Entry Point
*
* Put up information labels
@ INFILELINE, 20 SAY "Extracting: "
@ MODULELINE, 20 SAY "Documenting: "
* loop through all of the files

FOR i=1 TO nFiles

* Open file for input

	nCommentLen:=IIF(AT(".ASM",UPPER(aDirList[i,F_NAME]))>0,2,3)
	nReadHandle=FT_FUSE(aDirList[i,F_NAME])
	@ INFILELINE,33 CLEAR TO INFILELINE, MAXCOL()
	@ INFILELINE,33 SAY PAD(aDirList[i,F_NAME],47)
	@ MODULELINE,33 CLEAR TO LINELINE, MAXCOL()
	@ LINELINE,27 SAY "Line:"

	nLineCnt=0

	IF nReadHandle<0
		write_error("Can't open file: (Dos Error "+STR(FERROR())+")",,,,aDirList[i,F_NAME])
		@ ERRORLINE,0 CLEAR TO ERRORLINE,MAXCOL()
		@ ERRORLINE,20 SAY "Can't open file: (Dos Error "+STR(FERROR())+") File="+aDirList[i,F_NAME]
		LOOP
	ENDIF
	lEof=.F.
	lDoc=.F.
* First find the author



* loop to go through file

	DO WHILE .NOT.lEof

* Read a line

		cBuffer=TRIM(SUBSTR(ReadLN(@lEof),nCommentLen))
		nLineCnt++
		IF nLineCnt%10=0
			@ LINELINE,33 SAY STR(nLineCnt,5,0)
		ENDIF
* check to see if we are in doc mode or getting out of doc mode

		IF AT(cDoc,cBuffer)>0
			IF lDoc
				write_error(cDoc+" encountered during extraction of Doc";
					+" at line"+STR(nLinecnt,5,0),,,,aDirList[i,F_NAME])
			ENDIF
			lDoc=.T.
			cBuffer=TRIM(SUBSTR(ReadLN(@lEof),;
					nCommentLen))
			nLineCnt++
			cCategory:=cFuncName:=cSeeAlso:=""
			nMode=D_IGNORE
		ELSEIF AT(cEnd,cBuffer)>0
			IF .NOT.lDoc
				write_error(cEnd+" encountered outside of Doc area at line";
					+STR(nLinecnt,5,0),,,,aDirList[i,F_NAME])
			ELSE
* Add a new entry to our list of files

				IF EMPTY(cCategory)
					write_error("Blank Category",,,,aDirList[i,F_NAME])
					cCategory="Unknown"
				ENDIF
				IF EMPTY(cFuncName)
					write_error("Blank Function Name",,,,aDirList[i,F_NAME])
					cFuncName="Unknown"
				ENDIF
				AADD(aDocInfo,{cCategory,cFuncName,cOneLine,cFileName})
* Now close down this little piece
				lDoc=.F.
				IF .NOT.EMPTY(cSeeAlso)
                         FWRITE(nWriteHandle,"See Also ")
                           Proccalso(nWriteHandle,cSeealso)
				ENDIF
                    Fwrite(nWriteHandle,".end-topic"+CRLF)
				FCLOSE(nWriteHandle)
				nMode=D_IGNORE
			ENDIF

			@ MODULELINE, 33 CLEAR TO MODULELINE, MAXCOL()
		ENDIF

* Act on the input
		IF lDoc
* 1) function name

			IF AT(cFunc,cBuffer)>0.OR.AT(cComm,cBuffer)>0
				cBuffer=ReadLN(@lEof)
				nLineCnt++
* Save the function name
				cFuncName=UPPER(ALLTRIM(SUBSTR(cBuffer,nCommentLen)))
				@ MODULELINE, 33 CLEAR TO MODULELINE, MAXCOL()
				@ MODULELINE, 33 SAY cFuncName

				nMode=D_NORMAL

* Open a new file
				IF AT("FT_",cFuncName)>0
					cTemp=SUBSTR(cFuncName,4)
				ELSE
					cTemp=cFuncName
				ENDIF

				IF (nEnd:=AT("(",cTemp))>0
					cTemp=LEFT(cTemp,nEnd-1)
				ENDIF
				cFileName=""

* Strip off any other non-alphabetic/numeric characters
				FOR j=1 TO LEN(cTemp)
					cChar=SUBSTR(cTemp,j,1)
					IF (cChar>="0".AND.cChar<="9").OR.;
						(cChar>="A".AND.cChar<="Z").OR.cChar="_"
						cFileName+=cChar
					ENDIF
				NEXT

* See if file name is present already. If so then modify

				cFileName=LEFT(cFileName,8)
				nEnd=1
				nCount=0
				DO WHILE nEnd>0
                         nEnd=ASCAN(aDocInfo,{|a| a[4]==cFileName+".hdf"})
					IF nEnd>0

* This will break if there are more than 10 files with the same first
* seven characters. We take our chances.

						IF LEN(cFileName)=8
							cFileName=STUFF(cFileName,8,1,STR(nCount,1,0))
						ELSE
							cFileName+=STR(nCount,1,0)
						ENDIF
						nCount++
					ENDIF
				ENDDO
* Add on the extension

                    cFileName=LEFT(cFileName,8)+".hdf"

                    nWriteHandle=FCREATE("hdf\"+cFileName)
				IF nWriteHandle<1
                         ? "Error creating",cFileName,".hdf"
                         write_error("Error creating",,,,cFileName+".hdf")
				ENDIF
* 2) Category
			ELSEIF AT(cCat,cBuffer)>0
				cBuffer=ReadLN(@lEof)
				nLineCnt++
* get the category
				cCategory=UPPER(ALLTRIM(SUBSTR(cBuffer,nCommentLen)))

* 3) One line description

			ELSEIF AT(cOne,cBuffer)>0
				cBuffer=ReadLN(@lEof)
				nLineCnt++
				cOneLine=ALLTRIM(SUBSTR(cBuffer,nCommentLen))
				IF LEN(cOneLine)>LONGONELINE
					write_error("OneLine",cOneLine,nLineCnt,LONGONELINE,;
						aDirList[i,F_NAME])
				ENDIF

				nMode=D_ONELINE
* Now start writing out what we know
                    FWRITE(nWriteHandle,'.topic '+CheckTop(PAD(cFuncName,20)) +CRLF)
//                    FWRITE(nWriteHandle,"!short: "+PAD(cFuncName,17)+cOneLine+CRLF)
                    FWRITE(nWriteHandle,'.title ' +cFuncName+CRLF)
//                  FWRITE(nWriteHandle," "+cFuncName+CRLF)
                    FWRITE(nWriteHandle,".par font 6 size 12 align center "+CRLF+cOneLine+ CRLF+".endpar"+CRLF)
                    FWRITE(nWriteHandle,HB_OEMTOANSI(cBar))
* 4) all other stuff

			ELSE

				IF AT(cSyn,cBuffer)>0
                         fwrite(nWriteHandle,".par bold on"+CRLF)
                         FWRITE(nWriteHandle," Syntax"+CRLF)
                         fwrite(nWriteHandle,".endpar"+CRLF)
					nMode=D_SYNTAX
					lAddBlank=.T.

				ELSEIF AT(cArg,cBuffer)>0

					IF !lBlankLine
                         fwrite(nWriteHandle,".par bold on"+CRLF)
                           FWRITE(nWriteHandle," Arguments"+CRLF)
                         fwrite(nWriteHandle,".endpar"+CRLF)
					ENDIF

					nMode=D_ARG
					lAddBlank=.T.

				ELSEIF AT(cRet,cBuffer)>0

					IF !lBlankLine
						FWRITE(nWriteHandle,CRLF)
					ENDIF
                         fwrite(nWriteHandle,".par bold on"+CRLF)
                          FWRITE(nWriteHandle," Returns"+CRLF)
                         fwrite(nWriteHandle,".endpar"+CRLF)
					nMode=D_ARG
					lAddBlank=.T.

				ELSEIF AT(cDesc,cBuffer)>0

					IF !lBlankLine
						FWRITE(nWriteHandle,CRLF)
					ENDIF
                         fwrite(nWriteHandle,".par bold on"+CRLF)
                         
                         

                         FWRITE(nWriteHandle," Description"+CRLF)
                         fwrite(nWriteHandle,".endpar"+CRLF)
					nMode=D_NORMAL
					lAddBlank=.T.

				ELSEIF AT(cExam,cBuffer)>0

					IF !lBlankLine
						FWRITE(nWriteHandle,CRLF)
					ENDIF
                         
                         
                         fwrite(nWriteHandle,".par bold on"+CRLF)
                         FWRITE(nWriteHandle," Examples"+CRLF)
                         fwrite(nWriteHandle,".endpar"+CRLF)

					nMode=D_NORMAL
					lAddBlank=.T.
                                ELSEIF AT(cTest,cBuffer)>0

					IF !lBlankLine
						FWRITE(nWriteHandle,CRLF)
					ENDIF
                         fwrite(nWriteHandle,".par bold on"+CRLF)
                         FWRITE(nWriteHandle," Tests"+CRLF)
                         fwrite(nWriteHandle,".endpar"+CRLF)
                         
                
					nMode=D_NORMAL
					lAddBlank=.T.


                                ELSEIF AT(cCompl,cBuffer)>0

					IF !lBlankLine
						FWRITE(nWriteHandle,CRLF)
					ENDIF
                         fwrite(nWriteHandle,".par bold on"+CRLF)
                         FWRITE(nWriteHandle," Compilance"+CRLF)

                         fwrite(nWriteHandle,".endpar"+CRLF)


                                        nMode=D_NORMAL       
					lAddBlank=.T.
                                ELSEIF AT(cPlat,cBuffer)>0

					IF !lBlankLine
						FWRITE(nWriteHandle,CRLF)
					ENDIF
                         fwrite(nWriteHandle,".par bold on"+CRLF)
                         FWRITE(nWriteHandle," Plataforms"+CRLF)
                         fwrite(nWriteHandle,".endpar"+CRLF)

                         
                                        nMode=D_NORMAL       
					lAddBlank=.T.
                                ELSEIF AT(cFiles,cBuffer)>0

					IF !lBlankLine
						FWRITE(nWriteHandle,CRLF)
					ENDIF
                         fwrite(nWriteHandle,".par bold on"+CRLF)
                         FWRITE(nWriteHandle," Files"+CRLF)
                         fwrite(nWriteHandle,".endpar"+CRLF)


                                        nMode=D_NORMAL       
					lAddBlank=.T.
                    ELSEIF AT(cStatus,cBuffer)>0
                        nMode=D_STATUS
				ELSEIF AT(cSee,cBuffer)>0
					nMode=D_SEEALSO
				ELSEIF AT(cInc,cBuffer)>0
					nMode=D_INCLUDE

* All other input is trimmed of comments and sent out

				ELSE
* translate any \$ into $
					cBuffer=STRTRAN(cBuffer,"\"+DELIM,DELIM)
					IF nMode=D_SYNTAX
						IF LEN(cBuffer)>LONGLINE
							write_error("Syntax",cBuffer,nLineCnt,;
									LONGLINE,aDirList[i,F_NAME])
						ENDIF
						lBlankLine=EMPTY(cBuffer)
						IF lAddBlank
							FWRITE(nWriteHandle,CRLF)
							lAddBlank=.F.
						ENDIF
						nNonBlank:=FirstNB(cBuffer)
						cBuffer=STUFF(cBuffer,nNonBlank,0,"^a1f ")
						FWRITE(nWriteHandle,cBuffer+CRLF)
					ELSEIF nMode=D_ARG
						IF LEN(cBuffer)>LONGLINE
							write_error("Arguments",cBuffer,nLineCnt,;
									LONGLINE,aDirList[i,F_NAME])
						ENDIF
						lBlankLine=EMPTY(cBuffer)
						IF lAddBlank
							FWRITE(nWriteHandle,CRLF)
							lAddBlank=.F.
						ENDIF
                              cBuffer=STRTRAN(cBuffer,"<","<",1)
                              cBuffer=STRTRAN(cBuffer,">",">",1)
						FWRITE(nWriteHandle,cBuffer+CRLF)
					ELSEIF nMode=D_NORMAL
						IF LEN(cBuffer)>LONGLINE
							write_error("General",cBuffer,nLineCnt,;
									LONGLINE,aDirList[i,F_NAME])
						ENDIF
						lBlankLine=EMPTY(cBuffer)
						IF lAddBlank
							FWRITE(nWriteHandle,CRLF)
							lAddBlank=.F.
						ENDIF
						FWRITE(nWriteHandle,cBuffer+CRLF)
					ELSEIF nMode=D_SEEALSO
						IF .NOT.EMPTY(cBuffer)
                                   cSeeAlso=StripFiles(ALLTRIM(cBuffer))
						ENDIF
					ELSEIF nMode=D_INCLUDE
* read next line
						IF .NOT.EMPTY(cBuffer)
							IF !lBlankLine
								FWRITE(nWriteHandle,CRLF)
							ENDIF
                                   FWRITE(nWriteHandle," Header File: ";
								+ALLTRIM(cBuffer)+CRLF)
						ENDIF
                         ELSEIF nMode=D_STATUS
                         If !empty(cBuffer)
                         fwrite(nWriteHandle,".par bold on"+CRLF)
                         FWRITE(nWriteHandle," Status"+CRLF)
                         fwrite(nWriteHandle,".endpar"+CRLF)
                         Endif
                         ProcStatus(nWriteHandle,cBuffer)
                                          

					ELSE

* unknown data from somewhere

						write_error("Unknown Data Type "+cBuffer,,;
							nLineCnt,;
							LONGONELINE,aDirList[i,F_NAME])

					ENDIF
				ENDIF
			ENDIF
		ENDIF
	ENDDO
* Close down the input file

	FT_FUSE()
NEXT


RETURN NIL

* End of file process files

*+
FUNCTION fill_link_info(cLinkName)
*
* This routine and all accompaning database structures are 
* Copyright (C) 1992 Leo J. Letendre. 
*
* Purpose: read the link information to learn how to assemble database files
*
* Modification History:
*        Version    Date      Who       Notes
*         V1.00     1/22/92   LJL       Initial Version
*         V1.01     4/25/92   LJL       Get the compiler type based upon
*                                       "object" file extension
*
* Calling parameters: cLinkName - The name of the link file
*
* Returns: cCompiler - The name of the compiler to use
*
* Notes: looks for the !menu command and then reads the lines after it to
*        get the catagories and filenames associated with each.
*-
* LOCAL variables: 
LOCAL cBuffer, lEof, cSpace
LOCAL cCategory, cFile
LOCAL lMenuMode:=.F. // Menu infomation being read
LOCAL lGetType:=.T.  // Get the compiled file type to determine complier
LOCAL cCompiler      // Compiler Type
*
* Entry Point
*
nReadHandle=FT_FUSE(cLinkName)
lEof=.F.

DO WHILE .NOT. lEof

* Read a line
	cBuffer=UPPER(ReadLN(@lEof))

* Does it have a !menu?

	IF AT("!MENU",cBuffer)>0
		lMenuMode=.T.
		cBuffer=UPPER(ReadLN(@lEof))
	ELSEIF LEFT(cBuffer,1)="!"
		lMenuMode=.F.
	ENDIF

* If we are in menu mode and the line has information on it then parse it

	cBuffer=ALLTRIM(cBuffer)

	IF .NOT.EMPTY(cBuffer).AND.lMenuMode
		cSpace=AT("  ",cBuffer)
		cCategory=UPPER(RTRIM(LEFT(cBuffer,cSpace-1)))
		cFile=UPPER(LTRIM(SUBSTR(cBuffer,cSpace)))
		IF lGetType
                        cCompiler=IIF(".NGO"$cFile,"NGXC","EHC")
			lGetType=.NOT.(".NGO"$cFile.OR."EHO"$cFile)
		ENDIF
                cFile=STRTRAN(cFile,IIF(cCompiler="NGXC",".NGO",".EHO"),".TXT")
		AADD(aLinkInfo,{cCategory,cFile,.F.})
	ENDIF

ENDDO

* Close the file

FT_FUSE()
* Return to caller

RETURN cCompiler

* End of fill_link_info8

*+
STATIC FUNCTION ReadAtFile(cAtFile)
*
* This routine and all accompaning database structures are 
* Copyright (C) 1992 Leo J. Letendre. 
*
* Purpose: read in the users list of files to act on
*
* Modification History:
*        Version    Date      Who       Notes
*         V1.00     1/1/92   LJL       Initial Version
*
* Calling parameters: cAtFile - The name of the file containing a list of
*                              files to be processed
*
* Returns: an array containing information that looks like it came
*          from DIRECTORY() but only has the name present.
*
* Notes: 
*-
* LOCAL variables: 
LOCAL aDirList:={}
LOCAL cBuffer, lEof, nCount:=0

*
* Entry Point
*
IF FT_FUSE(cAtFile)<>NIL

* Read each line
	lEof=.F.
	DO WHILE .NOT. lEof

		cBuffer=ALLTRIM(ReadLN(@lEof))
		IF .NOT. EMPTY(cBuffer)
			AADD(aDirList,ARRAY(F_LEN))
			nCount++
			aDirList[nCount,F_NAME]=UPPER(cBuffer)
		ENDIF
	ENDDO
ENDIF

FT_FUSE()			

RETURN aDirList

* End of ReadAtFile


STATIC FUNCTION write_error(cDescrip, cBadLine, nLineCnt, nMax, cFile)

* This routine will send error messages to the error log - DOCERR.LOG
*
* Calling parameters: cDesc - Description of info being written
*                     cBadLine - The offending line - IF NIL then just 
*                                output cDesc and filename
*                     nLineCnt - The line number of the bad line
*                     nMax - The maximum length of the bad line
*                     cFile - The file currently being processed
*
* Returns: NIL
*
* Entry point
*
* Point output to the log file
SET ALTERNATE TO "DOCERR.LOG" ADDITIVE
SET CONSOLE OFF
SET ALTERNATE ON

* Send out the output
IF cBadLine=NIL
	? cDescrip,"in file",cFile
	? " "
ELSE
	? "Line too long in file",cFile,"at line",ALLTRIM(STR(nLineCnt,10,0))
	? "Reading",cDescrip,"information when line greater than",STR(nMax,2,0),"encountered:"
	? cBadLine
	? " "
ENDIF

* Turn off the log file and return

SET ALTERNATE OFF
SET CONSOLE ON
SET ALTERNATE TO

RETURN NIL

* End of Write_Error

/***
*  ReadLn( lEof ) --> cBuffer
*
* Read a line from the currently open file
*
* Parameters: lEof - Passed by reference - Logical indicating end of file
*
* Returns: The next line in the file without delimiters
*
*/
FUNCTION ReadLN( leof )

   LOCAL cBuffer := ""

	cBuffer=FT_FREADLN()
	FT_FSKIP(1)
	lEof=FT_FEOF()

   RETURN cBuffer
* End of ReadLN

*+
STATIC FUNCTION ASCIIFiles
*
* This routine and all accompaning database structures are 
* Copyright (C) 1992 Leo J. Letendre. 
*
* Purpose: Process each of the files in the directory making an ascii file
*
* Modification History:
*        Version    Date      Who       Notes
*         V1.00     1/19/92   LJL       Initial Version
*         V1.01     2/25/92   LJL       Trimmed spaces from see also and header
*         V1.02     10/30/92  LJL       Added multi-line one liner check and
*                                       insured blank line prior to and after
*                                       category headers
*         V1.03     10/30/93  LJL       added changable delimiter support,
*                                       COMMANDNAME keyword and removed
*                                       filename references in see alsos
*
* Calling parameters: None
*
* Notes: None
*-
* LOCAL variables: 

LOCAL i, j, nFiles:=LEN(aDirList)
LOCAL nCommentLen
LOCAL lEof, lDoc, lDone
LOCAL cBuffer
LOCAL nEnd, nCount

LOCAL cBar:="컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴"+CRLF
LOCAL nMode
LOCAL cAuthor
LOCAL cCISID
LOCAL cFuncName
LOCAL cOneLine
LOCAL cCategory
LOCAL cFileName
LOCAL nLineCnt
LOCAL cSeeAlso
LOCAL cTemp, cChar
LOCAL nDocCnt
LOCAL lBlankLine:=.F.  // Blank line encountered and sent out
LOCAL lAddBlank:=.F.   // Need to add a blank line if next line is not blank
LOCAL lFunc:=.T.       // currently a function rather than a command
*
* Entry Point
*
* Put up information labels
@ INFILELINE, 20 SAY "Extracting: "
@ MODULELINE, 20 SAY "Documenting: "
* loop through all of the files

FOR i=1 TO nFiles

* Open file for input

	nCommentLen:=IIF(AT(".ASM",UPPER(aDirList[i,F_NAME]))>0,2,3)
	nReadHandle=FT_FUSE(aDirList[i,F_NAME])
	@ INFILELINE,33 CLEAR TO INFILELINE, MAXCOL()
	@ INFILELINE,33 SAY PAD(aDirList[i,F_NAME],47)
	@ MODULELINE,33 CLEAR TO LINELINE, MAXCOL()
	@ LINELINE,27 SAY "Line:"

	nLineCnt=0

	IF nReadHandle<0
		write_error("Can't open file: (Dos Error "+STR(FERROR())+")",,,,aDirList[i,F_NAME])
		@ ERRORLINE,0 CLEAR TO ERRORLINE,MAXCOL()
		@ ERRORLINE,20 SAY "Can't open file: (Dos Error "+STR(FERROR())+") File="+aDirList[i,F_NAME]
		LOOP
	ENDIF
	lEof=.F.
	lDoc=.F.
* First find the author


	DO WHILE .NOT.lEof

* Read a line

		cBuffer=TRIM(SUBSTR(ReadLN(@lEof),nCommentLen))
		nLineCnt++
		IF nLineCnt%10=0
			@ LINELINE,33 SAY STR(nLineCnt,5,0)
		ENDIF

* check to see if we are in doc mode or getting out of doc mode

		IF AT(cDoc,cBuffer)>0
			IF lDoc
				write_error(cDoc+" encountered during extraction of Doc";
					+" at line"+STR(nLinecnt,5,0),,,,aDirList[i,F_NAME])
			ENDIF
			lDoc=.T.
			cBuffer=TRIM(SUBSTR(ReadLN(@lEof),;
					nCommentLen))
			nLineCnt++
			cCategory:=cFuncName:=cSeeAlso:=""
			nMode=D_IGNORE
		ELSEIF AT(cEnd,cBuffer)>0
			IF .NOT.lDoc
				write_error(cEnd+" encountered outside of Doc area at line";
					+STR(nLinecnt,5,0),,,,aDirList[i,F_NAME])

			ELSE
* Add a new entry to our list of files

				IF EMPTY(cCategory)
					write_error("Blank Category",,,,aDirList[i,F_NAME])
					cCategory="Unknown"
				ENDIF
				IF EMPTY(cFuncName)
					write_error("Blank Function Name",,,,aDirList[i,F_NAME])
					cFuncName="Unknown"
				ENDIF
				AADD(aDocInfo,{cCategory,cFuncName,cOneLine,cFileName})
* Now close down this little piece
				lDoc=.F.
				IF nDocCnt>60.AND..NOT.lContinuous
					FWRITE(nWriteHandle,CHR(K_CTRL_L)+CRLF)
					nDocCnt=0
				ENDIF
				FWRITE(nWriteHandle,CRLF)
				FWRITE(nWriteHandle," Source: "+aDirList[i,F_NAME]+CRLF+CRLF)
				IF lAuthor
					FWRITE(nWriteHandle," Author: "+cAuthor+CRLF)
				ENDIF
				IF .NOT.EMPTY(cSeeAlso)
					FWRITE(nWriteHandle,"See also: "+cSeeAlso+CRLF)
				ENDIF
				IF .NOT.lContinuous
					FWRITE(nWriteHandle,CHR(K_CTRL_L)+CRLF)
				ELSE
					FWRITE(nWriteHandle,CRLF+CRLF)
				ENDIF
				nDocCnt=0
				FCLOSE(nWriteHandle)
			ENDIF
			nMode=D_IGNORE
			@ MODULELINE, 33 CLEAR TO MODULELINE, MAXCOL()
		ENDIF

* Act on the input
		IF lDoc
* 1) function name

			IF AT(cFunc,cBuffer)>0.OR.AT(cComm,cBuffer)>0
				lFunc=AT(cFunc,cBuffer)>0
				cBuffer=ReadLN(@lEof)
				nLineCnt++
* Save the function name
				cFuncName=UPPER(ALLTRIM(SUBSTR(cBuffer,nCommentLen)))
				@ MODULELINE, 33 CLEAR TO MODULELINE, MAXCOL()
				@ MODULELINE, 33 SAY cFuncName

				nMode=D_NORMAL

* Open a new file
				IF AT("FT_",cFuncName)>0
					cTemp=SUBSTR(cFuncName,4)
				ELSE
					cTemp=cFuncName
				ENDIF

				IF (nEnd:=AT("(",cTemp))>0
					cTemp=LEFT(cTemp,nEnd-1)
				ENDIF
				cFileName=""

* Strip off any other non-alphabetic/numeric characters
				FOR j=1 TO LEN(cTemp)
					cChar=SUBSTR(cTemp,j,1)
					IF (cChar>="0".AND.cChar<="9").OR.;
						(cChar>="A".AND.cChar<="Z").OR.cChar="_"
						cFileName+=cChar
					ENDIF
				NEXT

* See if file name is present already. If so then modify

				cFileName=LEFT(cFileName,8)
				nEnd=1
				nCount=0
				DO WHILE nEnd>0
                         nEnd=ASCAN(aDocInfo,{|a| a[4]==cFileName+".hdf"})
					IF nEnd>0

* This will break if there are more than 10 files with the same first
* seven characters. We take our chances.

						IF LEN(cFileName)=8
							cFileName=STUFF(cFileName,8,1,STR(nCount,1,0))
						ELSE
							cFileName+=STR(nCount,1,0)
						ENDIF
						nCount++
					ENDIF
				ENDDO
* Add on the extension

                    cFileName=LEFT(cFileName,8)+".hdf"

                    nWriteHandle=FCREATE("hdf\"+cFileName)
				IF nWriteHandle<1
                         ? "Error creating",cFileName,".hdf"
                         write_error("Error creating",,,,cFileName+".hdf")
				ENDIF
* 2) Category
			ELSEIF AT(cCat,cBuffer)>0
				cBuffer=ReadLN(@lEof)
				nLineCnt++
* get the category
				cCategory=UPPER(ALLTRIM(SUBSTR(cBuffer,nCommentLen)))

* 3) One line description

			ELSEIF AT(cOne,cBuffer)>0
				cBuffer=ReadLN(@lEof)
				nLineCnt++
				cOneLine=ALLTRIM(SUBSTR(cBuffer,nCommentLen))
				IF LEN(cOneLine)>LONGONELINE
					write_error("OneLine",cOneLine,nLineCnt,LONGONELINE,;
						aDirList[i,F_NAME])
				ENDIF
				nMode=D_ONELINE

* Now start writing out what we know
				IF nDocCnt>60.AND..NOT.lContinuous
					FWRITE(nWriteHandle,CHR(K_CTRL_L)+CRLF)
					nDocCnt=0
				ENDIF

				FWRITE(nWriteHandle,IIF(lFunc,"FUNCTION: ","COMMAND: ");
					+cFuncName+CRLF)
				FWRITE(nWriteHandle," "+cOneLine+CRLF)
				FWRITE(nWriteHandle,cBar)
				nDocCnt+=3
* 4) all other stuff

			ELSE

				IF AT(cSyn,cBuffer)>0

					IF nDocCnt>62.AND..NOT.lContinuous
						FWRITE(nWriteHandle,CHR(K_CTRL_L)+CRLF)
						nDocCnt=0
					ENDIF
					FWRITE(nWriteHandle," Syntax:"+CRLF)
					nDocCnt++
					nMode=D_SYNTAX

				ELSEIF AT(cArg,cBuffer)>0

					IF nDocCnt>62.AND..NOT.lContinuous
						FWRITE(nWriteHandle,CHR(K_CTRL_L)+CRLF)
						nDocCnt=0
					ELSEIF !lBlankLine
						FWRITE(nWriteHandle,CRLF)
						nDocCnt++
					ENDIF
					FWRITE(nWriteHandle," Arguments:"+CRLF)
					nDocCnt++
					nMode=D_ARG
					lAddBlank=.T.

				ELSEIF AT(cRet,cBuffer)>0

					IF nDocCnt>62.AND..NOT.lContinuous
						FWRITE(nWriteHandle,CHR(K_CTRL_L)+CRLF)
						nDocCnt=0
					ELSEIF !lBlankLine
						FWRITE(nWriteHandle,CRLF)
						nDocCnt++
					ENDIF
					FWRITE(nWriteHandle," Returns:"+CRLF)
					nDocCnt++
					nMode=D_ARG
					lAddBlank=.T.

				ELSEIF AT(cDesc,cBuffer)>0

					IF nDocCnt>62.AND..NOT.lContinuous
						FWRITE(nWriteHandle,CHR(K_CTRL_L)+CRLF)
						nDocCnt=0
					ELSEIF !lBlankLine
						FWRITE(nWriteHandle,CRLF)
						nDocCnt++
					ENDIF

					FWRITE(nWriteHandle," Description:"+CRLF)
					nDocCnt++
					nMode=D_NORMAL
					lAddBlank=.T.

				ELSEIF AT(cExam,cBuffer)>0

					IF nDocCnt>62.AND..NOT.lContinuous
						FWRITE(nWriteHandle,CHR(K_CTRL_L)+CRLF)
						nDocCnt=0
					ELSEIF !lBlankLine
						FWRITE(nWriteHandle,CRLF)
						nDocCnt++
					ENDIF

					FWRITE(nWriteHandle," Examples:"+CRLF)
					nDocCnt++
					nMode=D_NORMAL
					lAddBlank=.T.
                                ELSEIF AT(cTest,cBuffer)>0

					IF nDocCnt>62.AND..NOT.lContinuous
						FWRITE(nWriteHandle,CHR(K_CTRL_L)+CRLF)
						nDocCnt=0
					ELSEIF !lBlankLine
						FWRITE(nWriteHandle,CRLF)
						nDocCnt++
					ENDIF

                                        FWRITE(nWriteHandle," Tests:"+CRLF)
					nDocCnt++
					nMode=D_NORMAL
					lAddBlank=.T.

                                ELSEIF AT(cStatus,cBuffer)>0
					IF nDocCnt>62.AND..NOT.lContinuous
						FWRITE(nWriteHandle,CHR(K_CTRL_L)+CRLF)
						nDocCnt=0
					ELSEIF !lBlankLine
						FWRITE(nWriteHandle,CRLF)
						nDocCnt++
					ENDIF

                                        FWRITE(nWriteHandle," Status:"+CRLF)
					nDocCnt++
					nMode=D_NORMAL
					lAddBlank=.T.

                                


                                ELSEIF AT(cCompl,cBuffer)>0
					IF nDocCnt>62.AND..NOT.lContinuous
						FWRITE(nWriteHandle,CHR(K_CTRL_L)+CRLF)
						nDocCnt=0
					ELSEIF !lBlankLine
						FWRITE(nWriteHandle,CRLF)
						nDocCnt++
					ENDIF

                                        FWRITE(nWriteHandle," Compilance:"+CRLF)
					nDocCnt++
					nMode=D_NORMAL
					lAddBlank=.T.

                                ELSEIF AT(cPlat,cBuffer)>0
					IF nDocCnt>62.AND..NOT.lContinuous
						FWRITE(nWriteHandle,CHR(K_CTRL_L)+CRLF)
						nDocCnt=0
					ELSEIF !lBlankLine
						FWRITE(nWriteHandle,CRLF)
						nDocCnt++
					ENDIF

                                        FWRITE(nWriteHandle," Plataforms:"+CRLF)
					nDocCnt++
					nMode=D_NORMAL
					lAddBlank=.T.
                                ELSEIF AT(cFiles,cBuffer)>0
					IF nDocCnt>62.AND..NOT.lContinuous
						FWRITE(nWriteHandle,CHR(K_CTRL_L)+CRLF)
						nDocCnt=0
					ELSEIF !lBlankLine
						FWRITE(nWriteHandle,CRLF)
						nDocCnt++
					ENDIF

                                        FWRITE(nWriteHandle," Files:"+CRLF)
					nDocCnt++
					nMode=D_NORMAL
					lAddBlank=.T.

				ELSEIF AT(cSee,cBuffer)>0
					nMode=D_SEEALSO
				ELSEIF AT(cInc,cBuffer)>0
					nMode=D_INCLUDE

* All other input is trimmed of comments and sent out

				ELSE
* translate any \$ into $
					cBuffer=STRTRAN(cBuffer,"\"+DELIM,DELIM)
					IF nMode=D_SYNTAX
						IF LEN(cBuffer)>LONGLINE
							write_error("Syntax",cBuffer,nLineCnt,;
									LONGLINE,aDirList[i,F_NAME])
						ENDIF
						IF nDocCnt>62.AND..NOT.lContinuous
							FWRITE(nWriteHandle,CHR(K_CTRL_L)+CRLF)
							nDocCnt=0
							lAddBlank=.F.
						ELSEIF lAddBlank
							FWRITE(nWriteHandle,CRLF)
							lAddBlank=.F.
							nDocCnt++
						ENDIF

						lBlankLine=EMPTY(cBuffer)
						FWRITE(nWriteHandle," "+cBuffer+CRLF)
						nDocCnt++
					ELSEIF nMode=D_ARG
						IF LEN(cBuffer)>LONGLINE
							write_error("Arguments",cBuffer,nLineCnt,;
									LONGLINE,aDirList[i,F_NAME])
						ENDIF
						IF nDocCnt>62.AND..NOT.lContinuous
							FWRITE(nWriteHandle,CHR(K_CTRL_L)+CRLF)
							nDocCnt=0
							lAddBlank=.F.
						ELSEIF lAddBlank
							FWRITE(nWriteHandle,CRLF)
							lAddBlank=.F.
							nDocCnt++
						ENDIF
						lBlankLine=EMPTY(cBuffer)
						FWRITE(nWriteHandle,cBuffer+CRLF)
						nDocCnt++
					ELSEIF nMode=D_NORMAL
						IF LEN(cBuffer)>LONGLINE
							write_error("General",cBuffer,nLineCnt,;
									LONGLINE,aDirList[i,F_NAME])
						ENDIF
						IF nDocCnt>62.AND..NOT.lContinuous
							FWRITE(nWriteHandle,CHR(K_CTRL_L)+CRLF)
							nDocCnt=0
							lAddBlank=.F.
						ELSEIF lAddBlank
							FWRITE(nWriteHandle,CRLF)
							lAddBlank=.F.
							nDocCnt++
						ENDIF
						FWRITE(nWriteHandle,cBuffer+CRLF)
						nDocCnt++
					ELSEIF nMode=D_SEEALSO
						IF .NOT.EMPTY(cBuffer)
							cSeeAlso=StripFiles(ALLTRIM(cBuffer))
						ENDIF
					ELSEIF nMode=D_INCLUDE
						IF .NOT.EMPTY(cBuffer)
							IF nDocCnt>62.AND..NOT.lContinuous
								FWRITE(nWriteHandle,CHR(K_CTRL_L)+CRLF)
								nDocCnt=0
							ELSEIF !lBlankLine
								FWRITE(nWriteHandle,CRLF)
								lAddBlank=.F.
								nDocCnt++
							ENDIF
							FWRITE(nWriteHandle," Header File: ";
								+ALLTRIM(cBuffer)+CRLF)
							nDocCnt++
						ENDIF
                         ELSEIF nMode=D_STATUS
                              If !empty(cBuffer)
                                   fwrite(nWriteHandle,".par bold on"+CRLF)
                                   FWRITE(nWriteHandle," Status"+CRLF)
                                   fwrite(nWriteHandle,".endpar"+CRLF)
                              Endif
                              ProcStatus(nWriteHandle,cBuffer)

					ELSE
* unknown data from somewhere
						
						write_error("Unknown Data Type "+cBuffer,,;
							nLineCnt,;
							LONGONELINE,aDirList[i,F_NAME])

					ENDIF
				ENDIF
			ENDIF
		ENDIF
	ENDDO
* Close down the input file

	FT_FUSE()
NEXT


RETURN NIL

* End of ASCIIfiles

*+
FUNCTION StripFiles(cString)
*
* This routine and all accompaning database structures are 
* Copyright (C) 1993 Leo J. Letendre. All rights reserved.
*
* Purpose: Determine the position of the first non-blank character
*
* Modification History:
*        Version    Date      Who       Notes
*         V1.00     10/30/93  LJL       Initial Version
*
* Calling parameters: cString - The string to remove filenames from
*
* Returns: <cString> with filenames removed
*
* Notes: For example:  func.ngo:FUNC1() proc.ngo:PROC1()
*            becomes:  FUNC1() PROC1()
*-
* LOCAL variables: 
LOCAL nColon        // location of colon
LOCAL nSpace        // location of space prior to colon

* Look for a colon

DO WHILE (nColon:=AT(":",cString))>0

* find the space prior to it
	nSpace=RAT(" ",LEFT(cString,nColon))
	IF nSpace>0
		cString=STUFF(cString,nSpace+1,nColon-nSpace,"")
	ELSE
		cString=STUFF(cString,1,nColon,"")
	ENDIF
ENDDO

RETURN cString

* End of StripFiles

*+
FUNCTION FirstNB(cString)
*
* This routine and all accompaning database structures are 
* Copyright (C) 1992 Leo J. Letendre. All rights reserved.
*
* Purpose: Determine the position of the first non-blank character
*
* Modification History:
*        Version    Date      Who       Notes
*         V1.00     10/10/92  LJL       Initial Version
*
* Calling parameters: cString - The string to test
*
* Notes: None
*-
* LOCAL variables: 
LOCAL nLen:=LEN(cString)
LOCAL nReturn:=0
LOCAL i

*
* Entry Point
*
i=1
IF !EMPTY(cString)
	DO WHILE nReturn=0.AND.i<LEN(cString)
		IF SUBSTR(cString,i,1)!=" "
			nReturn=i
		ELSE
			i++
		ENDIF
	ENDDO
ENDIF

RETURN nReturn

* End of file FirstNB

/***
*  ListAsArray( <cList>, <cDelimiter> ) --> aList
*  Convert a delimited string to an array
* Taken from Clipper supplied routines 1/2/90
*
*/
FUNCTION ListAsArray( cList, cDelimiter )
   LOCAL nPos
   LOCAL aList := {}                            // Define an empty array

   IF cDelimiter = NIL
      cDelimiter := ","
   ENDIF
   //
   DO WHILE (nPos := AT(cDelimiter, cList)) != 0
      AADD(aList, SUBSTR(cList, 1, nPos - 1))   // Add a new element
      cList := SUBSTR(cList, nPos + 1)
   ENDDO
   AADD(aList, cList)                           // Add final element
   //
   RETURN aList                                 // Return the array

* End of ListAsArray
/****
*  Proccalso(nWriteHandle,cSeealso)  --->NIL
*  Get Each See Also and Convert to The HelpC See Also Format
*/
#define CRLF chr(13)+chr(10)
FUNCTION Proccalso(nWriteHandle,cSeeAlso)
LOCAL nPos,cTemp:='',nLen,xPos,tPos
nLen:=LEN(cSeeAlso)
WHILE .t.
     nPos:=AT(",",cSeeAlso)

          IF nPos>0
               xTemp:=SUBSTR(ALLTRIM(cSeeAlso),1,nPos-1)
               tPos:=AT("()",xTemp)
               IF tPos>0
                     nLen-=LEN(xTemp)+1

                     xTemp+="~"+xTemp+"~ "
                     cTemp:="~"+xTemp
                ELSE
                    xPos:=AT(" ",xTemp)
                    IF xPos>0
                         nLen-=LEN(xTemp)+3
                         xTemp+="~"+SUBSTR(xTemp,1,xPos-1)+'_'+SUBSTR(xTemp,xPos+1)+"~ "
                         cTemp:="~"+xTemp
                    ELSE
                         nLen-=LEN(xTemp)+2
                         xTemp+="~"+xTemp+"~ "
                         cTemp:="~"+xTemp
                    END
                    
               END
          ELSE
               xTemp:=SUBSTR(cSeeAlso,1)
               tPos:=AT("()",xTemp)
                    
               IF tPos>0
                     nLen-=LEN(xTemp)+1

                     xTemp+="~"+xTemp+"~ "
                         cTemp:="~"+xTemp
               ELSE

                         xPos:=AT(" ",xTemp)
                         IF xPos>0
                              nLen-=LEN(xTemp)+3
                              xTemp+="~"+SUBSTR(xTemp,1,xPos-1)+'_'+SUBSTR(xTemp,xPos+1)+"~"
                              cTemp:="~"+xTemp
                         ELSE
                              nLen-=LEN(xTemp)+2
                              xTemp+="~"+xTemp+"~ "
                              cTemp:="~"+xTemp
                         END
               END

          
          ENDIF
               fwrite(nWriteHandle,ALLTRIM(cTemp)+CRLF)
         cSeeAlso:=SUBSTR(cSeeAlso,nPos+1)

         IF nLen==0 .or. nLen<0
               exit
               END
ENDDO
RETURN nil

#ifdef NANFOR

#define xReadBuffer 4096
/****
*   FT_FUSE(cFile,nMode)   ---> nHandle
*   Open a File
*/
FUNCTION ft_fuse(cFile,nMode)
IF nMode==nil
     nMode:=2
ENDIF
IF cFile==Nil
     theHandle:close()
ENDIF
IF cFile<> Nil
     IF nMode<>0
          theHandle:=TFileRead():new(cFile):open(nMode)
     ELSE
          theHandle:=TFileRead():new(cFile):open()
     ENDIF
ENDIF
RETURN theHandle:nHan
FUNCTION ft_FEOF()
LOCAL lRETURN:=theHandle:lEOF
RETURN  lRETURN
 

FUNCTION FReadLn(nH,cLine)

//     cline:= thehandle:readline()
// ENDIF
IF theHandle:MoreToRead()
     cLine:= theHandle:ReadLine()
ELSE

FSEEK(theHandle:nHan,0,0)
theHandle:lEOF:=.f.

 cLine:= theHandle:ReadLine()
ENDIF
RETURN cLine
FUNCTION FT_FReadLn()
LOCAL cBuffer:=''

cBuffer:=FReadLn(theHandle:nHan,@cBuffer)

RETURN cBuffer
 FUNCTION FT_FGotop()
     Fseek(theHandle:nHan,0,0)
RETURN
FUNCTION ft_fskip(nPos)
RETURN nil
FUNCTION FT_MKDIR(CDIR)
MAKEDIR(cdir)
RETURN nil
#ENDIF

FUNCTION CheckTop(cTop)
LOCAL nPos,cTemp
cTop:=rTrim(cTop)
nPos:=AT(" ",cTop)
IF nPos>0
     cTemp:=SUBSTR(cTop,1,nPos-1)+'_'+SUBSTR(cTop,nPos+1)
ELSE
     cTemp:=cTop
ENDIF
RETURN cTemp
STATIC FUNCTION ProcessRtf
*

* Copyright (C) 2000 Luiz Rafael Culik
*
* Purpose: Process each of the files in the directory
*
* Modification History:
*        Version    Date        Who       Notes
*         V1.00     1/08/2000   LRC       Initial Version
*
* Calling parameters: None
*
* Notes: None
*-
* LOCAL variables: 

#define D_NORMAL  1
#define D_ARG     2
#define D_SYNTAX  3
#define D_IGNORE  4
#define D_SEEALSO 5
#define D_INCLUDE 6
#define D_ONELINE 7
#define D_STATUS  8
LOCAL i, j, nFiles:=LEN(aDirList)
LOCAL nCommentLen
LOCAL lEof, lDoc, lDone
LOCAL cBuffer
LOCAL nEnd, nCount

LOCAL cBar:="컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�"
LOCAL nMode
LOCAL cAuthor
LOCAL cCISID
LOCAL cFuncName
LOCAL cOneLine
LOCAL cCategory
LOCAL cFileName
LOCAL nLineCnt
LOCAL cSeeAlso
LOCAL cTemp, cChar
LOCAL nNonBlank
LOCAL lBlankLine:=.F.  // Blank line encountered and sent out
LOCAL lAddBlank:=.F.   // Need to add a blank line if next line is not blank

*
* Entry Point
*
* Put up information labels
@ INFILELINE, 20 SAY "Extracting: "
@ MODULELINE, 20 SAY "Documenting: "
* loop through all of the files

FOR i=1 TO nFiles

* Open file for input

	nCommentLen:=IIF(AT(".ASM",UPPER(aDirList[i,F_NAME]))>0,2,3)
	nReadHandle=FT_FUSE(aDirList[i,F_NAME])
	@ INFILELINE,33 CLEAR TO INFILELINE, MAXCOL()
	@ INFILELINE,33 SAY PAD(aDirList[i,F_NAME],47)
	@ MODULELINE,33 CLEAR TO LINELINE, MAXCOL()
	@ LINELINE,27 SAY "Line:"

	nLineCnt=0

	IF nReadHandle<0
		write_error("Can't open file: (Dos Error "+STR(FERROR())+")",,,,aDirList[i,F_NAME])
		@ ERRORLINE,0 CLEAR TO ERRORLINE,MAXCOL()
		@ ERRORLINE,20 SAY "Can't open file: (Dos Error "+STR(FERROR())+") File="+aDirList[i,F_NAME]
		LOOP
	ENDIF
	lEof=.F.
	lDoc=.F.
* First find the author


	DO WHILE .NOT.lEof

* Read a line

		cBuffer=TRIM(SUBSTR(ReadLN(@lEof),nCommentLen))
		nLineCnt++
		IF nLineCnt%10=0
			@ LINELINE,33 SAY STR(nLineCnt,5,0)
		ENDIF
* check to see if we are in doc mode or getting out of doc mode

		IF AT(cDoc,cBuffer)>0
			IF lDoc
				write_error(cDoc+" encountered during extraction of Doc";
					+" at line"+STR(nLinecnt,5,0),,,,aDirList[i,F_NAME])
			ENDIF
			lDoc=.T.
			cBuffer=TRIM(SUBSTR(ReadLN(@lEof),;
					nCommentLen))
			nLineCnt++
			cCategory:=cFuncName:=cSeeAlso:=""
			nMode=D_IGNORE
		ELSEIF AT(cEnd,cBuffer)>0
			IF .NOT.lDoc
				write_error(cEnd+" encountered outside of Doc area at line";
					+STR(nLinecnt,5,0),,,,aDirList[i,F_NAME])
			ELSE
* Add a new entry to our list of files

				IF EMPTY(cCategory)
					write_error("Blank Category",,,,aDirList[i,F_NAME])
					cCategory="Unknown"
				ENDIF
				IF EMPTY(cFuncName)
					write_error("Blank Function Name",,,,aDirList[i,F_NAME])
					cFuncName="Unknown"
				ENDIF
				AADD(aDocInfo,{cCategory,cFuncName,cOneLine,cFileName})
* Now close down this little piece
				lDoc=.F.
				IF .NOT.EMPTY(cSeeAlso)
                         oRtf:WritePar("See Also"):EndPar()
                         ProcRtfalso(oRtf,cSeealso)
				ENDIF
                    
                    oRtf:Close()
				nMode=D_IGNORE
			ENDIF

			@ MODULELINE, 33 CLEAR TO MODULELINE, MAXCOL()
		ENDIF

* Act on the input
		IF lDoc
* 1) function name

			IF AT(cFunc,cBuffer)>0.OR.AT(cComm,cBuffer)>0
				cBuffer=ReadLN(@lEof)
				nLineCnt++
* Save the function name
				cFuncName=UPPER(ALLTRIM(SUBSTR(cBuffer,nCommentLen)))
				@ MODULELINE, 33 CLEAR TO MODULELINE, MAXCOL()
				@ MODULELINE, 33 SAY cFuncName

				nMode=D_NORMAL

* Open a new file
				IF AT("FT_",cFuncName)>0
					cTemp=SUBSTR(cFuncName,4)
				ELSE
					cTemp=cFuncName
				ENDIF

				IF (nEnd:=AT("(",cTemp))>0
					cTemp=LEFT(cTemp,nEnd-1)
				ENDIF
				cFileName=""

* Strip off any other non-alphabetic/numeric characters
				FOR j=1 TO LEN(cTemp)
					cChar=SUBSTR(cTemp,j,1)
					IF (cChar>="0".AND.cChar<="9").OR.;
						(cChar>="A".AND.cChar<="Z").OR.cChar="_"
						cFileName+=cChar
					ENDIF
				NEXT

* See if file name is present already. If so then modify

                    cFileName=LEFT(cFileName,21)
				nEnd=1
				nCount=0
				DO WHILE nEnd>0
                         nEnd=ASCAN(aDocInfo,{|a| a[4]==cFileName+".rtf"})
					IF nEnd>0

* This will break if there are more than 10 files with the same first
* seven characters. We take our chances.

                              IF LEN(cFileName)=21
                                   cFileName=STUFF(cFileName,21,1,STR(nCount,1,0))
						ELSE
							cFileName+=STR(nCount,1,0)
						ENDIF
						nCount++
					ENDIF
				ENDDO
* Add on the extension

                    cFileName=LEFT(cFileName,21)+".rtf"

                    oRtf:=TRTF():new("rtf\"+cFileName):WriteHeader()
                    IF oRtf:nHandle<1
                         ? "Error creating",cFileName,".rtf"
                         write_error("Error creating",,,,cFileName+".rtf")
				ENDIF
* 2) Category
			ELSEIF AT(cCat,cBuffer)>0
				cBuffer=ReadLN(@lEof)
				nLineCnt++
* get the category
				cCategory=UPPER(ALLTRIM(SUBSTR(cBuffer,nCommentLen)))

* 3) One line description

			ELSEIF AT(cOne,cBuffer)>0
				cBuffer=ReadLN(@lEof)
				nLineCnt++
				cOneLine=ALLTRIM(SUBSTR(cBuffer,nCommentLen))
				IF LEN(cOneLine)>LONGONELINE
					write_error("OneLine",cOneLine,nLineCnt,LONGONELINE,;
						aDirList[i,F_NAME])
				ENDIF

				nMode=D_ONELINE
* Now start writing out what we know
                    oRtf:WriteTitle(PAD(cFuncName,21),cFuncName )
                    oRtf:WriteParBold(cOneLine)
                    oRtf:WritePar(HB_OEMTOANSI(cBar)):EndPar()
* 4) all other stuff

			ELSE

				IF AT(cSyn,cBuffer)>0

                         oRtf:WriteParBold(" Syntax")

					nMode=D_SYNTAX
					lAddBlank=.T.

				ELSEIF AT(cArg,cBuffer)>0

					IF !lBlankLine

                           oRtf:WriteParBold(" Arguments")

					ENDIF

					nMode=D_ARG
					lAddBlank=.T.

				ELSEIF AT(cRet,cBuffer)>0

					IF !lBlankLine
                              oRtf:WritePar(""):EndPar()
					ENDIF

                          oRtf:WriteParBold(" Returns")

					nMode=D_ARG
					lAddBlank=.T.

				ELSEIF AT(cDesc,cBuffer)>0

					IF !lBlankLine
                              oRtf:WritePar(""):EndPar()
					ENDIF
                         oRtf:WriteParBold(" Description")

					nMode=D_NORMAL
					lAddBlank=.T.

				ELSEIF AT(cExam,cBuffer)>0

					IF !lBlankLine
                              oRtf:WritePar(""):EndPar()
					ENDIF
                         oRtf:WriteParBold(" Examples")
					nMode=D_NORMAL
                         lAddBlank=.T.
                    ELSEIF AT(cTest,cBuffer)>0

					IF !lBlankLine
                              oRtf:WritePar(""):EndPar()
					ENDIF
                        
                         oRtf:WriteParBold(" Tests")
					nMode=D_NORMAL
					lAddBlank=.T.

                     ELSEIF AT(cStatus,cBuffer)>0
                                
                         nMode=D_STATUS

                    ELSEIF AT(cCompl,cBuffer)>0

					IF !lBlankLine
                              oRtf:WritePar(""):EndPar()
					ENDIF
                         oRtf:WriteParBold(" Compilance")
                         nMode=D_NORMAL       
					lAddBlank=.T.
                    ELSEIF AT(cPlat,cBuffer)>0

					IF !lBlankLine
                              oRtf:WritePar(""):EndPar()
					ENDIF
                         oRtf:WriteParBold(" Plataforms")
                         nMode=D_NORMAL       
					lAddBlank=.T.
                    ELSEIF AT(cFiles,cBuffer)>0

					IF !lBlankLine
                              oRtf:WritePar(""):EndPar()
					ENDIF
                         oRtf:WriteParBold(" Files")


                                        nMode=D_NORMAL       
					lAddBlank=.T.

				ELSEIF AT(cSee,cBuffer)>0
					nMode=D_SEEALSO
				ELSEIF AT(cInc,cBuffer)>0
					nMode=D_INCLUDE

* All other input is trimmed of comments and sent out

				ELSE
* translate any \$ into $
					cBuffer=STRTRAN(cBuffer,"\"+DELIM,DELIM)
					IF nMode=D_SYNTAX
						IF LEN(cBuffer)>LONGLINE
							write_error("Syntax",cBuffer,nLineCnt,;
									LONGLINE,aDirList[i,F_NAME])
						ENDIF
						lBlankLine=EMPTY(cBuffer)
						IF lAddBlank
                                   oRtf:WritePar(""):EndPar()
							lAddBlank=.F.
						ENDIF
						nNonBlank:=FirstNB(cBuffer)
						cBuffer=STUFF(cBuffer,nNonBlank,0,"^a1f ")
                              oRtf:WritePar(cBuffer):EndPar()
					ELSEIF nMode=D_ARG
						IF LEN(cBuffer)>LONGLINE
							write_error("Arguments",cBuffer,nLineCnt,;
									LONGLINE,aDirList[i,F_NAME])
						ENDIF
						lBlankLine=EMPTY(cBuffer)
						IF lAddBlank
                                   oRtf:WritePar(""):EndPar()
							lAddBlank=.F.
						ENDIF
                              cBuffer=STRTRAN(cBuffer,"<","<",1)
                              cBuffer=STRTRAN(cBuffer,">",">",1)
                              oRtf:WritePar(cBuffer):EndPar()
					ELSEIF nMode=D_NORMAL
						IF LEN(cBuffer)>LONGLINE
							write_error("General",cBuffer,nLineCnt,;
									LONGLINE,aDirList[i,F_NAME])
						ENDIF
						lBlankLine=EMPTY(cBuffer)
						IF lAddBlank
                                   oRtf:WritePar(""):EndPar()
							lAddBlank=.F.
						ENDIF
                              oRtf:WritePar(cBuffer):EndPar()
					ELSEIF nMode=D_SEEALSO
						IF .NOT.EMPTY(cBuffer)
                                   cSeeAlso=StripFiles(ALLTRIM(cBuffer))
						ENDIF
					ELSEIF nMode=D_INCLUDE
* read next line
						IF .NOT.EMPTY(cBuffer)
							IF !lBlankLine
                                        oRtf:WritePar(""):EndPar()
							ENDIF
                                   oRtf:WritePar(" Header File: ";
                                        +ALLTRIM(cBuffer)):EndPar()
						ENDIF
                         ELSEIF nMode=D_STATUS
                              If !empty(cBuffer)
                                   oRtf:WriteParBold("Status")
                              Endif
                              ProcStatusRtf(oRtf,cBuffer)

					ELSE

* unknown data from somewhere

						write_error("Unknown Data Type "+cBuffer,,;
							nLineCnt,;
							LONGONELINE,aDirList[i,F_NAME])

					ENDIF
				ENDIF
			ENDIF
		ENDIF
	ENDDO
* Close down the input file

	FT_FUSE()
NEXT


RETURN NIL
FUNCTION ProcRtfAlso(nWriteHandle,cSeeAlso)
LOCAL nPos,cTemp:='',nLen,xPos,tPos
nLen:=LEN(cSeeAlso)
WHILE .t.
     nPos:=AT(",",cSeeAlso)

          IF nPos>0
               xTemp:=SUBSTR(ALLTRIM(cSeeAlso),1,nPos-1)
               tPos:=AT("()",xTemp)
               IF tPos>0
                     nLen-=LEN(xTemp)+1


                     cTemp:=xTemp
                ELSE
                    xPos:=AT(" ",xTemp)
                    IF xPos>0
                         nLen-=LEN(xTemp)+3
         
                         cTemp:=xTemp
                    ELSE
                         nLen-=LEN(xTemp)+2

                         cTemp:=xTemp
                    END
                    
               END
          ELSE
               xTemp:=SUBSTR(cSeeAlso,1)
               tPos:=AT("()",xTemp)
                    
               IF tPos>0
                     nLen-=LEN(xTemp)+1


                         cTemp:=xTemp
               ELSE

                         xPos:=AT(" ",xTemp)
                         IF xPos>0
                              nLen-=LEN(xTemp)+3

                              cTemp:=xTemp
                         ELSE
                              nLen-=LEN(xTemp)+2

                              cTemp:=xTemp
                         END
               END

          
          ENDIF

               nWriteHandle:WriteLink(ALLTRIM(cTemp))
         cSeeAlso:=SUBSTR(cSeeAlso,nPos+1)

         IF nLen==0 .or. nLen<0
               exit
               END
ENDDO
RETURN nil
function                         ProcStatus(nWriteHandle,cBuffer)
IF substr(alltrim(cBuffer),1)=="R"
     Fwrite(nWriteHandle,"   Ready"+CRLF)
ELSEIF substr(alltrim(cBuffer),1)=="S"
     Fwrite(nWriteHandle,"   Started"+CRLF)
ELse
     Fwrite(nWriteHandle,"   Not Started"+CRLF)
endif
return nil
function                         ProcStatusRTF(nWriteHandle,cBuffer)
IF substr(alltrim(cBuffer),1)=="R"
     nWriteHandle:WritePar("   Ready"):EndPar()
ELSEIF substr(alltrim(cBuffer),1)=="S"
     nWriteHandle:WritePar("   Started"):EndPar()
ELse
     nWriteHandle:WritePar("   Not Started"):EndPar()
endif
return nil

STATIC FUNCTION ProcessWww
* Copyright (C) 2000 Luiz Rafael Culik
*
* Purpose: Process each of the files in the directory
*
* Modification History:
*        Version    Date        Who       Notes
*         V1.00     1/12/2000   LRC       Initial Version
*
* Calling parameters: None
*
* Notes: None
*-
* LOCAL variables: 

#define D_NORMAL  1
#define D_ARG     2
#define D_SYNTAX  3
#define D_IGNORE  4
#define D_SEEALSO 5
#define D_INCLUDE 6
#define D_ONELINE 7
#define D_STATUS  8
LOCAL i, j, nFiles:=LEN(aDirList)
LOCAL nCommentLen
LOCAL lEof, lDoc, lDone
LOCAL cBuffer
LOCAL nEnd, nCount

LOCAL cBar:=Replicate("-",80)+CRLF
LOCAL nMode
LOCAL cAuthor
LOCAL cCISID
LOCAL cFuncName
LOCAL cOneLine
LOCAL cCategory
LOCAL cFileName
LOCAL nLineCnt
LOCAL cSeeAlso
LOCAL cTemp, cChar
LOCAL nNonBlank
LOCAL lBlankLine:=.F.  // Blank line encountered and sent out
LOCAL lAddBlank:=.F.   // Need to add a blank line if next line is not blank

*
* Entry Point
*
* Put up information labels
@ INFILELINE, 20 SAY "Extracting: "
@ MODULELINE, 20 SAY "Documenting: "
* loop through all of the files

FOR i=1 TO nFiles

* Open file for input

	nCommentLen:=IIF(AT(".ASM",UPPER(aDirList[i,F_NAME]))>0,2,3)
	nReadHandle=FT_FUSE(aDirList[i,F_NAME])
	@ INFILELINE,33 CLEAR TO INFILELINE, MAXCOL()
	@ INFILELINE,33 SAY PAD(aDirList[i,F_NAME],47)
	@ MODULELINE,33 CLEAR TO LINELINE, MAXCOL()
	@ LINELINE,27 SAY "Line:"

	nLineCnt=0

	IF nReadHandle<0
		write_error("Can't open file: (Dos Error "+STR(FERROR())+")",,,,aDirList[i,F_NAME])
		@ ERRORLINE,0 CLEAR TO ERRORLINE,MAXCOL()
		@ ERRORLINE,20 SAY "Can't open file: (Dos Error "+STR(FERROR())+") File="+aDirList[i,F_NAME]
		LOOP
	ENDIF
	lEof=.F.
	lDoc=.F.
* First find the author


	DO WHILE .NOT.lEof

* Read a line

		cBuffer=TRIM(SUBSTR(ReadLN(@lEof),nCommentLen))
		nLineCnt++
		IF nLineCnt%10=0
			@ LINELINE,33 SAY STR(nLineCnt,5,0)
		ENDIF
* check to see if we are in doc mode or getting out of doc mode

		IF AT(cDoc,cBuffer)>0
			IF lDoc
				write_error(cDoc+" encountered during extraction of Doc";
					+" at line"+STR(nLinecnt,5,0),,,,aDirList[i,F_NAME])
			ENDIF
			lDoc=.T.
			cBuffer=TRIM(SUBSTR(ReadLN(@lEof),;
					nCommentLen))
			nLineCnt++
			cCategory:=cFuncName:=cSeeAlso:=""
			nMode=D_IGNORE
		ELSEIF AT(cEnd,cBuffer)>0
			IF .NOT.lDoc
				write_error(cEnd+" encountered outside of Doc area at line";
					+STR(nLinecnt,5,0),,,,aDirList[i,F_NAME])
			ELSE
* Add a new entry to our list of files

				IF EMPTY(cCategory)
					write_error("Blank Category",,,,aDirList[i,F_NAME])
					cCategory="Unknown"
				ENDIF
				IF EMPTY(cFuncName)
					write_error("Blank Function Name",,,,aDirList[i,F_NAME])
					cFuncName="Unknown"
				ENDIF
				AADD(aDocInfo,{cCategory,cFuncName,cOneLine,cFileName})
* Now close down this little piece
				lDoc=.F.
				IF .NOT.EMPTY(cSeeAlso)
                         oHtm:WritePar("See Also ")
                         ProcWwwalso(oHtm,cSeealso)
				ENDIF
                    
                    oHtm:Close()
				nMode=D_IGNORE
			ENDIF

			@ MODULELINE, 33 CLEAR TO MODULELINE, MAXCOL()
		ENDIF

* Act on the input
		IF lDoc
* 1) function name

			IF AT(cFunc,cBuffer)>0.OR.AT(cComm,cBuffer)>0
				cBuffer=ReadLN(@lEof)
				nLineCnt++
* Save the function name
				cFuncName=UPPER(ALLTRIM(SUBSTR(cBuffer,nCommentLen)))
				@ MODULELINE, 33 CLEAR TO MODULELINE, MAXCOL()
				@ MODULELINE, 33 SAY cFuncName

				nMode=D_NORMAL

* Open a new file
				IF AT("FT_",cFuncName)>0
					cTemp=SUBSTR(cFuncName,4)
				ELSE
					cTemp=cFuncName
				ENDIF

				IF (nEnd:=AT("(",cTemp))>0
					cTemp=LEFT(cTemp,nEnd-1)
				ENDIF
				cFileName=""

* Strip off any other non-alphabetic/numeric characters
				FOR j=1 TO LEN(cTemp)
					cChar=SUBSTR(cTemp,j,1)
					IF (cChar>="0".AND.cChar<="9").OR.;
						(cChar>="A".AND.cChar<="Z").OR.cChar="_"
						cFileName+=cChar
					ENDIF
				NEXT

* See if file name is present already. If so then modify

                    cFileName=LEFT(cFileName,21)
				nEnd=1
				nCount=0
				DO WHILE nEnd>0
                         nEnd=ASCAN(aDocInfo,{|a| a[4]==cFileName+".html"})
					IF nEnd>0

* This will break if there are more than 10 files with the same first
* seven characters. We take our chances.

                              IF LEN(cFileName)=21
                                   cFileName=STUFF(cFileName,21,1,STR(nCount,1,0))
						ELSE
							cFileName+=STR(nCount,1,0)
						ENDIF
						nCount++
					ENDIF
				ENDDO
* Add on the extension

                    cFileName=LEFT(cFileName,21)+".html"

                    oHTM:=THTML():new("www\"+Lower(cFileName))
                    IF oHtm:nHandle<1
                         ? "Error creating",cFileName,".html"
                         write_error("Error creating",,,,cFileName+".html")
				ENDIF
* 2) Category
			ELSEIF AT(cCat,cBuffer)>0
				cBuffer=ReadLN(@lEof)
				nLineCnt++
* get the category
				cCategory=UPPER(ALLTRIM(SUBSTR(cBuffer,nCommentLen)))

* 3) One line description

			ELSEIF AT(cOne,cBuffer)>0
				cBuffer=ReadLN(@lEof)
				nLineCnt++
				cOneLine=ALLTRIM(SUBSTR(cBuffer,nCommentLen))
				IF LEN(cOneLine)>LONGONELINE
					write_error("OneLine",cOneLine,nLineCnt,LONGONELINE,;
						aDirList[i,F_NAME])
				ENDIF

				nMode=D_ONELINE
* Now start writing out what we know
                    oHtm:WriteTitle(PAD(cFuncName,21))
                    aadd(aWWW,cFuncName)
                    oHtm:WriteParBold(cOneLine)
                    oHtm:WritePar(cBar)
* 4) all other stuff

			ELSE

				IF AT(cSyn,cBuffer)>0

                         oHtm:WriteParBold(" Syntax")

					nMode=D_SYNTAX
					lAddBlank=.T.

				ELSEIF AT(cArg,cBuffer)>0

					IF !lBlankLine

                           oHtm:WriteParBold(" Arguments")

					ENDIF

					nMode=D_ARG
					lAddBlank=.T.

				ELSEIF AT(cRet,cBuffer)>0

					IF !lBlankLine
                              oHtm:WritePar("")
					ENDIF

                          oHtm:WriteParBold(" Returns")

					nMode=D_ARG
					lAddBlank=.T.

				ELSEIF AT(cDesc,cBuffer)>0

					IF !lBlankLine
                              oHtm:WritePar("")
					ENDIF
                         oHtm:WriteParBold(" Description")

					nMode=D_NORMAL
					lAddBlank=.T.

				ELSEIF AT(cExam,cBuffer)>0

					IF !lBlankLine
                              oHtm:WritePar("")
					ENDIF
                         oHtm:WriteParBold(" Examples")
					nMode=D_NORMAL
                         lAddBlank=.T.
                    ELSEIF AT(cTest,cBuffer)>0

					IF !lBlankLine
                              oHtm:WritePar("")
					ENDIF
                        
                         oHtm:WriteParBold(" Tests")
					nMode=D_NORMAL
					lAddBlank=.T.

                     ELSEIF AT(cStatus,cBuffer)>0
                                
                         nMode=D_STATUS

                    ELSEIF AT(cCompl,cBuffer)>0

					IF !lBlankLine
                              oHtm:WritePar("")
					ENDIF
                         oHtm:WriteParBold(" Compilance")
                         nMode=D_NORMAL       
					lAddBlank=.T.
                    ELSEIF AT(cPlat,cBuffer)>0

					IF !lBlankLine
                              oHtm:WritePar("")
					ENDIF
                         oHtm:WriteParBold(" Plataforms")
                         nMode=D_NORMAL       
					lAddBlank=.T.
                    ELSEIF AT(cFiles,cBuffer)>0

					IF !lBlankLine
                              oHtm:WritePar("")
					ENDIF
                         oHtm:WriteParBold(" Files")


                                        nMode=D_NORMAL       
					lAddBlank=.T.

				ELSEIF AT(cSee,cBuffer)>0
					nMode=D_SEEALSO
				ELSEIF AT(cInc,cBuffer)>0
					nMode=D_INCLUDE

* All other input is trimmed of comments and sent out

				ELSE
* translate any \$ into $
					cBuffer=STRTRAN(cBuffer,"\"+DELIM,DELIM)
					IF nMode=D_SYNTAX
						IF LEN(cBuffer)>LONGLINE
							write_error("Syntax",cBuffer,nLineCnt,;
									LONGLINE,aDirList[i,F_NAME])
						ENDIF
						lBlankLine=EMPTY(cBuffer)
						IF lAddBlank
                                   oHtm:WritePar("")
							lAddBlank=.F.
						ENDIF
                              cBuffer:=ProcwwwBuf(cBuffer)

                              oHtm:WritePar(cBuffer)
					ELSEIF nMode=D_ARG
						IF LEN(cBuffer)>LONGLINE
							write_error("Arguments",cBuffer,nLineCnt,;
									LONGLINE,aDirList[i,F_NAME])
						ENDIF
						lBlankLine=EMPTY(cBuffer)
						IF lAddBlank
                                   oHtm:WritePar("")
							lAddBlank=.F.
						ENDIF
                              cBuffer=STRTRAN(cBuffer,"<","<",1)
                              cBuffer=STRTRAN(cBuffer,">",">",1)
                              oHtm:WritePar(cBuffer)
					ELSEIF nMode=D_NORMAL
						IF LEN(cBuffer)>LONGLINE
							write_error("General",cBuffer,nLineCnt,;
									LONGLINE,aDirList[i,F_NAME])
						ENDIF
						lBlankLine=EMPTY(cBuffer)
						IF lAddBlank
                                   oHtm:WritePar("")
							lAddBlank=.F.
						ENDIF
                              oHtm:WritePar(cBuffer)
					ELSEIF nMode=D_SEEALSO
						IF .NOT.EMPTY(cBuffer)
                                   cSeeAlso=StripFiles(ALLTRIM(cBuffer))
						ENDIF
					ELSEIF nMode=D_INCLUDE
* read next line
						IF .NOT.EMPTY(cBuffer)
							IF !lBlankLine
                                        oHtm:WritePar("")
							ENDIF
                                   oHtm:WritePar(" Header File: ";
                                        +ALLTRIM(cBuffer))
						ENDIF
                         ELSEIF nMode=D_STATUS
                              If !empty(cBuffer)
                                   oHtm:WriteParBold("Status")
                              Endif
                              ProcStatusWww(oHtm,cBuffer)

					ELSE

* unknown data from somewhere

						write_error("Unknown Data Type "+cBuffer,,;
							nLineCnt,;
							LONGONELINE,aDirList[i,F_NAME])

					ENDIF
				ENDIF
			ENDIF
		ENDIF
	ENDDO
* Close down the input file

	FT_FUSE()
NEXT
return nil
/***********************************
* Function ProcWwwBuf(cBuffer)   -> cTemp
* Parameter cBuffer  -> Strip the "<" and ">" symbols from the imput String
* Return    cTemp  Formated String to WWW output
*/
FUNCTION ProcWwwBuf(cBuffer)
LOCAL cTemp:=''
cTemp:=STRTRAN(cBuffer,"<","")
cTemp:=STRTRAN(cTemp,">","")
RETURN ctemp
/***********************************
* Function ProcWwwAlso(nWriteHandle,cSeeAlso)  -> NIL
* Parameter nWriteHandle Handle of the output file
*           cSeeAlso   String of all see alsos
* Return    NIL
*/


FUNCTION ProcWwwAlso(nWriteHandle,cSeeAlso)
LOCAL nPos,cTemp:='',nLen,xPos,tPos
nLen:=LEN(cSeeAlso)
WHILE .t.
     nPos:=AT(",",cSeeAlso)
          IF nPos>0
               xTemp:=SUBSTR(ALLTRIM(cSeeAlso),1,nPos-1)
               tPos:=AT("()",xTemp)
               IF tPos>0
                     nLen-=LEN(xTemp)+1


                     cTemp:=xTemp
                ELSE
                    xPos:=AT(" ",xTemp)
                    IF xPos>0
                         nLen-=LEN(xTemp)+3

                         cTemp:=xTemp
                    ELSE
                         nLen-=LEN(xTemp)+2

                         cTemp:=xTemp
                    END
                    
               END
          ELSE
               xTemp:=SUBSTR(cSeeAlso,1)
               tPos:=AT("()",xTemp)
                    
               IF tPos>0
                     nLen-=LEN(xTemp)+1


                         cTemp:=xTemp
               ELSE

                         xPos:=AT(" ",xTemp)
                         IF xPos>0
                              nLen-=LEN(xTemp)+3

                              cTemp:=xTemp
                         ELSE
                              nLen-=LEN(xTemp)+2

                              cTemp:=xTemp
                         END
               END

          
          ENDIF


               nWriteHandle:WriteLink(ALLTRIM(cTemp))
         cSeeAlso:=SUBSTR(cSeeAlso,nPos+1)

         IF nLen==0 .or. nLen<0
               exit
               END
ENDDO
RETURN nil
function                         ProcStatusWww(nWriteHandle,cBuffer)
IF substr(alltrim(cBuffer),1)=="R"
     nWriteHandle:WritePar("   Ready")
ELSEIF substr(alltrim(cBuffer),1)=="S"
     nWriteHandle:WritePar("   Started")
ELse
     nWriteHandle:WritePar("   Not Started")
endif
return nil
