/*
 * xBaseScript Project source code:
 * Pre-Processor / Dot prompt environment / Script Interpreter
 *
 * Copyright 2000-2001 Ron Pinkas <ronpinkas@profit-master.com>
 * www - http://www.xBaseScript.com
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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
 */

#ifdef PP_QUIET
   #COMMAND @ Row(), 0 SAY <nLine> =>
#endif

#DEFINE MAX_CICLES 64
#DEFINE PP_BUFFER_SIZE 8192 //16384

#ifdef __HARBOUR__
   #ifndef NO_BOOST
      #define USE_C_BOOST
   #endif

   #INCLUDE "hbextern.ch"
   #DEFINE  CRLF HB_OsNewLine()
   #ifdef FW
      #INCLUDE "fwextern.ch"
   #else
      #ifdef WIN
         #COMMAND Alert( <x> ) => MessageBox( 0, xToStr( <x> ), "PP for Windows", 0 )
         EXTERN MessageBox
      #endif
   #endif
#else
   #DEFINE __CLIPPER__

   #ifndef CRLF
      #DEFINE  CRLF Chr(13) + Chr(10)
   #endif

   STATIC s_abBlocks := {}, nBlockId := 0

   EXTERNAL BROWSE

   EXTERNAL ARRAY,ASIZE,ATAIL,AINS,ADEL,AFILL,ASCAN,AEVAL,ACOPY,ACLONE,ADIR, ASORT

   EXTERNAL ERRORLEVEL

   EXTERNAL __QQPUB,__MCLEAR,__MRELEASE,__MXRELEASE,__MSAVE,__MRESTORE ;

   EXTERNAL PROCNAME,PROCLINE,PROCFILE

   EXTERNAL BIN2W,BIN2I,BIN2L,I2BIN,L2BIN

   EXTERNAL OUTSTD,OUTERR,QQOUT,QOUT,DISPOUT,DISPOUTAT,__EJECT, ;
            SETPRC,DISPBOX,DISPBEGIN,DISPEND,DISPCOUNT,ISCOLOR, ;
            NOSNOW,DBGSHADOW,SAVESCREEN,RESTSCREEN,SETCURSOR,SETBLINK,SETMODE,__ACCEPT, ;
            __ACCEPTSTR

   EXTERNAL __COPYFILE

   EXTERNAL DESCEND,DIRECTORY

   EXTERNAL VERSION,GETENV,__RUN

   EXTERNAL ERRORNEW,DOSERROR

   EXTERNAL FERASE,FRENAME,FILE,FREADSTR,CURDIR,DISKSPACE

   EXTERNAL __KEYBOARD,NEXTKEY,LASTKEY,FKLABEL,FKMAX

   EXTERNAL ISPRINTER

   EXTERNAL MOD

   EXTERNAL MEMOREAD,MEMOWRIT,MEMOLINE,MLCOUNT,MLPOS,MEMOTRAN

   EXTERNAL NETNAME

   EXTERNAL __BOX,__BOXD,__BOXS

   EXTERNAL AMPM,DAYS,ELAPTIME,LENNUM,SECS,TSTRING

   EXTERNAL SETCANCEL,__SETCENTURY,DEFPATH,__DEFPATH

   EXTERNAL SETCOLOR,COLORSELECT

   EXTERNAL SOUNDEX

   EXTERNAL ISALPHA,ISDIGIT,ISUPPER,ISLOWER,ALLTRIM,PADR,PAD,PADL,PADC, ;
            STUFF,STRZERO

   EXTERNAL TONE

   EXTERNAL TRANSFORM

   EXTERNAL __XHELP

   EXTERNAL ACHOICE

   EXTERNAL __NONOALERT

   EXTERNAL TBROWSEDB,DBEDIT

   EXTERNAL DEVOUTPICT

   EXTERNAL __DIR

   EXTERNAL DBSETRELATION,DBCLEARREL,MEMOEDIT,MLCTOPOS,MPOSTOLC,__DBAPP,__DBCOPY, ;
            __DBDELIM,__DBJOIN,__DBLIST,__DBSDF,__DBSORT,__DBTOTAL,__DBUPDATE,__DBARRANGE,__DBFLIST, ;
            __DBOPENSDF,__DBTRANS,__DBTRANSREC

   EXTERNAL FIELDBLOCK,FIELDWBLOCK

   EXTERNAL __INPUT

   EXTERNAL MEMVARBLOCK

   EXTERNAL __ATPROMPT,__MENUTO

   EXTERNAL READKEY

   EXTERNAL SETKEY

   EXTERNAL SETTYPEAHEAD

   EXTERNAL TBCOLUMNNEW,TBROWSENEW

   EXTERNAL __TEXTSAVE,__TEXTRESTORE

   EXTERNAL __GET,__GETA

   EXTERNAL __LABELFORM, __REPORTFORM

   EXTERNAL __TYPEFILE

   EXTERNAL __WAIT

   EXTERNAL __XSAVESCREEN,__XRESTSCREEN

   /*
   EXTERNAL RDDSYS,AFIELDS,DBEVAL,DBCLEARFILTER,DBCLOSEALL, ;
            DBCOMMIT,__DBCONTINUE,DBCREATE,DBDELETE,DBFILTER,DBGOBOTTOM,DBGOTO, ;
            DBGOTOP,__DBLOCATE,__DBSETLOCATE,__DBPACK,DBRECALL,DBRLOCK,DBRLOCKLIST,DBRUNLOCK,DBSEEK, ;
            DBSELECTAREA,__DBSETFOUND,DBSKIP,DBSETFILTER,DBSTRUCT,DBTABLEEXT,DBUNLOCK,DBUNLOCKALL,DBUSEAREA, ;
            __DBZAP,DELETED,EOF,FCOUNT,FIELDGET,FIELDNAME,FIELDPOS,FIELDPUT,FLOCK,FOUND,HEADER,INDEXORD, ;
            LASTREC,LOCK,LUPDATE,NETERR,ORDBAGEXT,ORDBAGNAME,ORDCONDSET,ORDCREATE,ORDDESTROY,ORDFOR,ORDKEY, ;
            ORDLISTADD,ORDLISTCLEAR,ORDLISTREBUILD,ORDNAME,ORDNUMBER,ORDSETFOCUS,RDDLIST,RDDNAME,RDDREGISTER, ;
            RECCOUNT,RECNO,RECSIZE,RLOCK,SELECT,USED,RDDSETDEFAULT,RDDSETDEFAULT,DBSETDRIVER
   */

   EXTERNAL __DBPACK,__DBZAP,DBCLOSEALL,DBGOBOTTOM,DBGOTO,DBGOTOP

   EXTERNAL DBREINDEX,DBCREATEINDEX,DBCLEARINDEX,DBSETINDEX,DBSETORDER

   EXTERNAL __DBCOPYSTRUCT,__DBCOPYXSTRUCT,__DBCREATE,__FLEDIT

   EXTERNAL INDEXEXT,INDEXKEY

#endif

STATIC aDefRules     := {}, aDefResults   := {}
STATIC aTransRules   := {}, aTransResults := {}
STATIC aCommRules    := {}, aCommResults  := {}

STATIC bDbgMatch := .F., bDbgExp := .F., bDbgPPO := .F., bLoadRules := .T., ;
       bCount := .T., bCCH := .F., bCompile := .F., bStrict := .F.

STATIC nIfDef := 0, abIfDef := {}, nIf := 0, abIf := {}

STATIC hPP

STATIC s_asPaths := {}

STATIC s_bArrayPrefix := .F.

STATIC s_sFile := "", s_sIncludeFile

STATIC nRow, nCol

STATIC s_nProcId := 0, s_aProcedures := {}, s_xRet, s_nIfLevel := 0, ;
       s_aProcStack := {}, s_nProcStack := 0

STATIC s_asPrivates := {}, s_asPublics := {}, s_asLocals := {}, ;
       s_asStatics := {}, s_aParams := {}

STATIC s_sModule, s_aInitExit := { {}, {} }

STATIC s_nCompIf := 0,  s_nCompLoop := 0, s_aIfJumps := {}, s_aLoopJumps := {}
STATIC s_acFlowType := {},  s_nFlowId := 0

#ifdef PP_RECURSIVE
   STATIC s_bRecursive := .F.
#endif

static s_lRunLoaded := .F., s_lClsLoaded := .F., s_lFWLoaded := .F.

//--------------------------------------------------------------//
#ifdef __HARBOUR__
  STATIC PROCEDURE Main( sSource, p1, p2, p3, p4, p5, p6, p7, p8, p9 )
#else
  PROCEDURE Main( sSource, p1, p2, p3, p4, p5, p6, p7, p8, p9 )
#endif

   LOCAL sIncludePath, nNext, sPath, sSwitch := ""
   LOCAL nAt, sParams, sPPOExt, aParams
   LOCAL sDefine, sCH

   IF p1 != NIL
      sSwitch += p1
   ENDIF
   IF p2 != NIL
      sSwitch += p2
   ENDIF
   IF p3 != NIL
      sSwitch += p3
   ENDIF
   IF p4 != NIL
      sSwitch += p4
   ENDIF
   IF p5 != NIL
      sSwitch += p5
   ENDIF
   IF p6 != NIL
      sSwitch += p6
   ENDIF
   IF p7 != NIL
      sSwitch += p7
   ENDIF
   IF p8 != NIL
      sSwitch += p8
   ENDIF
   IF p9 != NIL
      sSwitch += p9
   ENDIF

   IF sSource != NIL .AND. ( Upper( sSource ) == "-H" .OR. Upper( sSource ) == "--HELP" )
      sSwitch := "   PP filename[.ext] [-CCH] [-D<id>] [-D:E] [-D:M] [-D:P] [-I<path>] [-P] [-R]" + CRLF
      sSwitch += "                     [-STRICT] [-U[ch-file]]" + CRLF + CRLF

      sSwitch += [    -CCH     = Generate a .cch file (compiled command header).] + CRLF
      sSwitch += [    -D<id>   = #define <id>.] + CRLF
      sSwitch += [    -D:E     = Show tracing information into the Expression Scanner.] + CRLF
      sSwitch += [    -D:M     = Show tracing information into the Match Engine.] + CRLF
      sSwitch += [    -D:P     = Show tracing information into the Output Generator.] + CRLF
      sSwitch += [    -I<path> = #include file search path(s) (';' seperated).] + CRLF
      sSwitch += [    -P       = Generate .pp$ pre-processed output file.] + CRLF
      sSwitch += [    -R       = Run filename as a script.] + CRLF
      sSwitch += [    -STRICT  = Strict Clipper compatability (clone Clipper PreProcessor bugs).] + CRLF
      sSwitch += [    -U       = Use command definitions set in <ch-file> (or none).] + CRLF

     	? sSwitch
      ?
      QUIT
   endif

   sIncludePath := GetE( "INCLUDE" )

   WHILE ( nNext := At( ';', sIncludePath ) ) > 0
      sPath := Left( sIncludePath, nNext - 1 )
      IF ! ( Right( sPath, 1 ) $ '\/' )
         sPath += '\'
      ENDIF
      aAdd( s_asPaths, sPath )
      sIncludePath := SubStr( sIncludePath, nNext + 1 )
   ENDDO
   IF ! ( sIncludePath == '' )
      IF ! ( Right( sIncludePath, 1 ) $ '\/' )
         sIncludePath += '\'
      ENDIF
      aAdd( s_asPaths, sIncludePath )
   ENDIF

#ifdef __CLIP__
   sIncludePath := StartPath()
   nAt := AtR( '/', sIncludePath )
   IF nAt <= 0
      nAt := AtR( "\", sIncludePath )
   ENDIF
   //? nAt,sIncludePath

   IF nAt != 0
      sIncludePath := Left( sIncludePath, nAt - 1 )
      IF ! ( Right( sIncludePath, 1 ) $ "\/" )
            sIncludePath += '\'
      ENDIF
      aAdd( s_asPaths, sIncludePath )
   ENDIF

   IF Empty(getenv("CLIPROOT"))
      aAdd( s_asPaths, cliproot()+"\include\" )
   else
      aAdd( s_asPaths, getenv("CLIPROOT")+"\include\" )
   endif
#endif

   IF ! Empty( sSwitch )
      sSwitch := Upper( sSwitch )

      /* Generate compiled header. */
      IF "-CCH" $ sSwitch
         bCCH := .T.
      ENDIF

      /* Process command line defines. */
      WHILE ( nAt := At( "-D", sSwitch ) ) > 0
         nNext := At( "-", SubStr( sSwitch, nAt + 2 ) )
         IF nNext == 0
            nNext := 256
         ENDIF

         sDefine := SubStr( sSwitch, nAt + 2, nNext - 1 )
         sSwitch := Left( sSwitch, nAt - 1 ) + SubStr( sSwitch, nAt + 1 + nNext )
         CompileDefine( sDefine )
      ENDDO

      /* Debug tracing options. */
      IF "-D:E" $ sSwitch
         bDbgExp := .T.
         sSwitch := StrTran( sSwitch, "-D:E", "" )
      ENDIF
      IF "-D:M" $ sSwitch
         bDbgMatch := .T.
         sSwitch := StrTran( sSwitch, "-D:M", "" )
      ENDIF
      IF "-D:P" $ sSwitch
         bDbgPPO := .T.
         sSwitch := StrTran( sSwitch, "-D:P", "" )
      ENDIF

      /* Process command line include paths. */
      IF ( nAt := At( "-I", sSwitch ) ) > 0
         nNext := At( "-", SubStr( sSwitch, nAt + 2 ) )
         IF nNext == 0
            nNext := 256
         ENDIF
         sIncludePath := SubStr( sSwitch, nAt + 2, nNext )

         WHILE ( nNext := At( ';', sIncludePath ) ) > 0
            sPath := Left( sIncludePath, nNext - 1 )
            IF ! ( Right( sPath, 1 ) $ '\/' )
               sPath += '\'
            ENDIF
            aAdd( s_asPaths, sPath )
            sIncludePath := SubStr( sIncludePath, nNext + 1 )
         ENDDO
         IF ! ( sIncludePath == '' )
            IF ! ( Right( sIncludePath, 1 ) $ '\/' )
               sIncludePath += '\'
            ENDIF
            aAdd( s_asPaths, sIncludePath )
         ENDIF
      ENDIF

      /* Generate .pp$ pre-processed output file. */
      IF "-P" $ sSwitch
         sPPOExt := ".pp$"
      ENDIF

      /* Run file as a script. */
      IF "-R" $ sSwitch
         bCompile := .T.
      ENDIF

      /* Clone Clipper PreProcessor bugs. */
      IF "-STRICT" $ sSwitch
         bStrict := .T.
      ENDIF

      /* Use alternate command defintions file, or none. */
      WHILE ( nAt := At( "-U", sSwitch ) ) > 0
         nNext := At( "-", SubStr( sSwitch, nAt + 2 ) )
         IF nNext == 0
            nNext := 256
         ENDIF

         sCH := SubStr( sSwitch, nAt + 2, nNext - 1 )
         sSwitch := Left( sSwitch, nAt - 1 ) + SubStr( sSwitch, nAt + 1 + nNext )

         IF( ! sCH == "" )
            ? [Loading standard definitions from: '] + sCH + "'"
            ?
            CompileDefine( "__PP__" )
            #ifdef __HARBOUR__
               CompileDefine( "__HARBOUR__" )
            #endif

            PP_PreProFile( sCH, NIL, .F., .T. ) // Process ONLY #Directives!

            /* Reset.*/
            hPP := NIL
         ENDIF

         /* Don't load standard defintions. */
         bLoadRules := .F.
      ENDDO

      /* End of command line arguments processing. */
   ENDIF

   IF bLoadRules
      InitRules()
      InitResults()
      IF Len( aDefRules ) != Len( aDefResults )
         Alert( [#DEFINE Rules size mismatch] )
      ENDIF
      IF Len( aTransRules ) != Len( aTransResults )
         Alert( [#TRANSLATE Rules size mismatch] )
      ENDIF
      IF Len( aCommRules ) != Len( aCommResults )
         Alert( [#DEFINE Rules size mismatch] )
      ENDIF

      CompileDefine( "__PP__" )
      #ifdef __HARBOUR__
         CompileDefine( "__HARBOUR__" )
      #endif
   ELSE
      IF sCH == NIL
         Alert( [Not using standard rules.] )
      ENDIF
   ENDIF

   IF sSource != NIL
      nRow := Row()
      nCol := Col()

      IF bCompile
         // Populate possible Command-line Parameters
         IF ( nAt := At( " ", sSource ) ) > 0
            sParams := LTrim( SubStr( sSource, nAt + 1 ) )
            sSource := Left( sSource, nAt - 1 )

            WHILE ( nAt := At( " ", sParams ) ) > 0
               aAdd( aParams, Left( sParams, nAt - 1 ) )
               sParams := LTrim( SubStr( sParams, nAt + 1 ) )
            ENDDO
            IF ! sParams == ""
               aAdd( aParams, sParams )
            ENDIF
         ENDIF

         PP_Run( sSource, aParams, sPPOExt )
      ELSE
         PP_PreProFile( sSource, sPPOExt )
      ENDIF
   ELSE
      nRow := 1
      nCol := 0
      RP_Dot()
   ENDIF

   DevPos( nRow, nCol )

RETURN

//------------------------------- *** RP DOT and Interpreter Functions *** -------------------------------//

FUNCTION PP_ExecMethod( sProcName, p1, p2, p3, p4, p5, p6, p7, p8, p9 )

   LOCAL i, sProc, nProc, nParams

   sProcName := Upper( sProcName )

   sProc := s_sModule + sProcName
   nProc := aScan( s_aProcedures, {|aProc| aProc[1] == sProc } )
   IF nProc == 0
      sProc := sProcName
      nProc := aScan( s_aProcedures, {|aProc| aProc[1] == sProc } )
   ENDIF

   IF nProc > 0
      s_xRet := NIL

      nParams := PCount()
      s_aParams := {}
#ifdef __CLIP__
   FOR i := 2 TO nParams
      aAdd( s_aParams, Param( i ) )
   NEXT
#else
      DO CASE
         CASE nParams == 0
         CASE nParams == 1
            aAdd( s_aParams, p1 )
         CASE nParams == 2
            aAdd( s_aParams, p1 )
            aAdd( s_aParams, p2 )
         CASE nParams == 3
            aAdd( s_aParams, p1 )
            aAdd( s_aParams, p2 )
            aAdd( s_aParams, p3 )
         CASE nParams == 4
            aAdd( s_aParams, p1 )
            aAdd( s_aParams, p2 )
            aAdd( s_aParams, p3 )
            aAdd( s_aParams, p4 )
         CASE nParams == 5
            aAdd( s_aParams, p1 )
            aAdd( s_aParams, p2 )
            aAdd( s_aParams, p3 )
            aAdd( s_aParams, p4 )
            aAdd( s_aParams, p5 )
         CASE nParams == 6
            aAdd( s_aParams, p1 )
            aAdd( s_aParams, p2 )
            aAdd( s_aParams, p3 )
            aAdd( s_aParams, p4 )
            aAdd( s_aParams, p5 )
            aAdd( s_aParams, p6 )
         CASE nParams == 7
            aAdd( s_aParams, p1 )
            aAdd( s_aParams, p2 )
            aAdd( s_aParams, p3 )
            aAdd( s_aParams, p4 )
            aAdd( s_aParams, p5 )
            aAdd( s_aParams, p6 )
            aAdd( s_aParams, p7 )
         CASE nParams == 8
            aAdd( s_aParams, p1 )
            aAdd( s_aParams, p2 )
            aAdd( s_aParams, p3 )
            aAdd( s_aParams, p4 )
            aAdd( s_aParams, p5 )
            aAdd( s_aParams, p6 )
            aAdd( s_aParams, p7 )
            aAdd( s_aParams, p8 )
         CASE nParams == 9
            aAdd( s_aParams, p1 )
            aAdd( s_aParams, p2 )
            aAdd( s_aParams, p3 )
            aAdd( s_aParams, p4 )
            aAdd( s_aParams, p5 )
            aAdd( s_aParams, p6 )
            aAdd( s_aParams, p7 )
            aAdd( s_aParams, p8 )
            aAdd( s_aParams, p9 )
      ENDCASE
#endif

      PP_ExecProcedure( s_aProcedures[nProc] )
   ELSE
      Alert( [Missing Method: ] + sProcName )
   ENDIF

RETURN s_xRet

//--------------------------------------------------------------//

FUNCTION PP_ExecProcedure( aProc, sProcName )

   LOCAL nBlock, nBlocks := Len( aProc[2] ), xErr
   LOCAL nVar, nVars

   IF s_nProcStack > 0
      /* Saving Privates of upper level. */
      nVars := Len( s_asPrivates )
      aAdd( s_aProcStack[s_nProcStack], Array( nVars, 2 ) )
      FOR nVar := 1 TO nVars
         s_aProcStack[s_nProcStack][3][nVar][1] := s_asPrivates[nVar]
         #ifdef __HARBOUR__
            s_aProcStack[s_nProcStack][3][nVar][2] := __MVGET( s_asPrivates[nVar] )
         #else
            s_aProcStack[s_nProcStack][3][nVar][2] := &( s_asPrivates[nVar] )
         #endif
         //Alert( [Saved upper Private: ] + s_asPrivates[nVar] + [ in ] + s_aProcStack[s_nProcStack][1] )
      NEXT
      aSize( s_asPrivates, 0 )

      /* Saving and Releasing Locals of upper level. */
      nVars := Len( s_asLocals )
      aAdd( s_aProcStack[s_nProcStack], Array( nVars, 2 ) )
      FOR nVar := 1 TO nVars
         s_aProcStack[s_nProcStack][4][nVar][1] := s_asLocals[nVar]
         #ifdef __HARBOUR__
            s_aProcStack[s_nProcStack][4][nVar][2] := __MVGET( s_asLocals[nVar] )
            __MVPUT( s_asLocals[nVar], NIL ) // *** Harbour __MXRelease() not working !!!
         #else
            s_aProcStack[s_nProcStack][4][nVar][2] := &( s_asLocals[nVar] )
            __MXRelease( s_asLocals[nVar] )
         #endif
         //Alert( [Released upper local: ] + s_asLocals[nVar] + [ in ] + s_aProcStack[s_nProcStack][1] )
      NEXT
      aSize( s_asLocals, 0 )
   ENDIF

   aAdd( s_aProcStack, { aProc[1], 0 } )
   s_nProcStack++

   FOR nBlock := 1 TO nBlocks
      IF aProc[2][nBlock][2] == NIL
         IF aProc[2][nBlock][1] != 0 // Uncondtional Jump.
            nBlock := aProc[2][nBlock][1]
         ENDIF
      ELSE
         s_aProcStack[ Len( s_aProcStack ) ][2] := aProc[2][nBlock][3] // Line No.

         BEGIN SEQUENCE

            IF aProc[2][nBlock][1] == 0
               //? aProc[2][nBlock][3]
               Eval( aProc[2][nBlock][2] )
            ELSE
               IF ! Eval( aProc[2][nBlock][2] ) // Jump if FALSE.
                  nBlock := aProc[2][nBlock][1]
               ENDIF
            ENDIF

         RECOVER USING xErr
            //IF ValType( xErr ) == 'N'
            //   PP_ExecProcedure( s_aProcedures[xErr] )
            //ENDIF
         END SEQUENCE
      ENDIF
   NEXT

   /* Releasing Privates created by the Procedure */
   nVars := Len( s_asPrivates )
   FOR nVar := 1 TO nVars
      #ifdef __HARBOUR__
         __MVPUT( s_asPrivates[nVar], NIL )
      #else
         __MXRelease( s_asPrivates[nVar] )
      #endif
      //Alert( [Released private: ] + s_asPrivates[nVar] + [ in ] + s_aProcStack[s_nProcStack][1] )
   NEXT
   aSize( s_asPrivates, 0 )

   /* Releasing Locals created by the Procedure */
   nVars := Len( s_asLocals )
   FOR nVar := 1 TO nVars
      #ifdef __HARBOUR__
         __MVPUT( s_asLocals[nVar], NIL )
      #else
         __MXRelease( s_asLocals[nVar] )
      #endif
      //Alert( [Released local: ] + s_asLocals[nVar] + [ in ] + s_aProcStack[s_nProcStack][1] )
   NEXT
   aSize( s_asLocals, 0 )

   s_nProcStack--
   aSize( s_aProcStack, s_nProcStack )

   IF s_nProcStack > 0
      /* Restoring Privates of parrent. */
      nVars := Len( s_aProcStack[s_nProcStack][3] )
      FOR nVar := 1 TO nVars
         aAdd( s_asPrivates, s_aProcStack[s_nProcStack][3][nVar][1] )
         __QQPub( s_aProcStack[s_nProcStack][3][nVar][1] )
         &( s_aProcStack[s_nProcStack][3][nVar][1] ) := s_aProcStack[s_nProcStack][3][nVar][2]
      NEXT

      /* Restoring Locals of parrent. */
      nVars := Len( s_aProcStack[s_nProcStack][4] )
      FOR nVar := 1 TO nVars
         aAdd( s_asLocals, s_aProcStack[s_nProcStack][4][nVar][1] )
         #ifdef __HARBOUR__
            //__QQPub( s_aProcStack[s_nProcStack][4][nVar][1] ) // *** Harbour Var was never released because of bug in __MXRelease() !!!
            __MVPUT( s_aProcStack[s_nProcStack][4][nVar][1], s_aProcStack[s_nProcStack][4][nVar][2] )
         #else
            __QQPub( s_aProcStack[s_nProcStack][4][nVar][1] )
            &( s_aProcStack[s_nProcStack][4][nVar][1] ) := s_aProcStack[s_nProcStack][4][nVar][2]
         #endif
      NEXT

      aSize( s_aProcStack[s_nProcStack], 2 )
   ENDIF

RETURN s_xRet

//--------------------------------------------------------------//

PROCEDURE RP_Dot()

   LOCAL GetList := {}, sLine := Space(256)

   #ifdef FW
       Alert( [DOT mode (no filename parameter) is Not ready for GUI yet.] + CRLF + CRLF + [Please try Interpreter mode, using the -R switch...] )
       RETURN
   #endif

   bCount := .F.

   PP_PreProFile( "rp_dot.ch" )
   #ifdef WIN
      PP_PreProLine( '#COMMAND Alert( <x> ) => MessageBox( 0, xToStr( <x> ), "TInterpreter for Windows", 0 )' )
   #endif

   ErrorBlock( {|oErr| RP_Dot_Err( oErr ) } )

   CLEAR SCREEN
   SET SCOREBOARD OFF

   @ 0,0 SAY "PP: "
   @ 0,4 SAY Space( 76 ) COLOR "N/R"

   DO WHILE .T.
      @ MaxRow(), 00 SAY '.'
      @ MaxRow(), 01 GET sLine PICTURE '@KS79'
      SET CURSOR ON
      READ

      sLine := StrTran( sLine,  Chr(9), "  " )

      ExecuteLine( PP_PreProLine( RTrim( sLine ), 1, '' ) )
   ENDDO

RETURN

//--------------------------------------------------------------//

STATIC PROCEDURE ExecuteLine( sPPed )

   LOCAL nNext, sBlock, sTemp
   LOCAL sTemp2, nLen, sLeft, sSymbol, nNextAssign

   ExtractLeadingWS( @sPPed )
   DropTrailingWS( @sPPed )
   sTemp := sPPed

   @ 0,0 SAY "PP: "
   @ 0,4 SAY Pad( sPPed, 76 ) COLOR "N/R"
   DevPos( nRow, nCol )

   BEGIN SEQUENCE

      WHILE ( nNext := nAtSkipStr( ';', sTemp ) ) > 0
         sBlock := Left( sTemp, nNext - 1 )
         ExtractLeadingWS( @sBlock )
         DropTrailingWS( @sBlock )

         sTemp2 := sBlock
         WHILE ( nNextAssign := At( ":=", sTemp2 ) ) > 0
            sLeft  := Left( sTemp2, nNextAssign - 1 )
            sTemp2 := SubStr( sTemp2, nNextAssign + 2 )

            DropTrailingWS( @sLeft )
            nLen := Len( sLeft )
            WHILE nLen > 0
               IF SubStr( sLeft, nLen, 1 ) $ " (,=><*+-\^&@["
                  EXIT
               ENDIF
               nLen--
            ENDDO
            IF nLen == 0
               sSymbol := sLeft
            ELSE
               sSymbol := SubStr( sLeft, nLen + 1 )
            ENDIF
            IF ( Type( sSymbol ) = 'U' )
               PUBLIC &sSymbol
            ENDIF
         ENDDO

         IF sBlock = "__"
            sSymbol := Upper( SubStr( sBlock, 3, 12 ) ) // Len( "SetOtherwise" )
         ELSE
            sSymbol := ""
         ENDIF
         IF nIf == 0 .OR. ;
            sSymbol = "SETIF" .OR. sSymbol = "SETELSE" .OR. sSymbol = "SETELSEIF" .OR. sSymbol = "SETEND" .OR. ;
            sSymbol = "SETDOCASE" .OR. sSymbol = "SETCASE" .OR. sSymbol = "SETOTHERWISE" .OR. sSymbol = "SETENDCASE" .OR. ;
            abIf[ nIf ]

            #ifdef __CLIPPER__
               /* Clipper Macro Compiler can't compile nested blocks! */
               CompileNestedBlocks( sBlock, @sBlock )
            #endif

            @ 0,0 SAY "PP: "
            @ 0,4 SAY Pad( sBlock, 76 ) COLOR "N/R"
            DevPos( nRow, nCol )

            Eval( &( "{|| " + sBlock + " }" ) )
            nRow := Row()
            nCol := Col()

            #ifdef __CLIPPER__
               nBlockID := 0
               aSize( s_abBlocks, 0 )
            #endif
         ENDIF

         sTemp  := RTrim( SubStr( sTemp, nNext + 1 ) )
         ExtractLeadingWS( @sTemp )
      ENDDO

      sBlock := sTemp
      DropTrailingWS( @sBlock )

      IF ! ( sBlock == '' )
         sTemp2 := sBlock
         WHILE ( nNextAssign := At( ":=", sTemp2 ) ) > 0
            sLeft  := Left( sTemp2, nNextAssign - 1 )
            sTemp2 := SubStr( sTemp2, nNextAssign + 2 )

            DropTrailingWS( @sLeft )
            nLen := Len( sLeft )
            WHILE nLen > 0
               IF SubStr( sLeft, nLen, 1 ) $ " (,=><*+-\^&@["
                  EXIT
               ENDIF
               nLen--
            ENDDO
            IF nLen == 0
               sSymbol := sLeft
            ELSE
               sSymbol := SubStr( sLeft, nLen + 1 )
            ENDIF
            IF ( Type( sSymbol ) = 'U' )
               PUBLIC &sSymbol
            ENDIF
         ENDDO

         IF sBlock = "__"
            sSymbol := Upper( SubStr( sBlock, 3, 12 ) ) // Len( "SetOtherwise" )
         ELSE
            sSymbol := ""
         ENDIF
         IF nIf == 0 .OR. ;
            sSymbol = "SETIF" .OR. sSymbol = "SETELSE" .OR. sSymbol = "SETELSEIF" .OR. sSymbol = "SETEND" .OR. ;
            sSymbol = "SETDOCASE" .OR. sSymbol = "SETCASE" .OR. sSymbol = "SETOTHERWISE" .OR. sSymbol = "SETENDCASE" .OR. ;
            abIf[ nIf ]

            #ifdef __CLIPPER__
                /* Clipper Macro Compiler can't compile nested blocks! */
                CompileNestedBlocks( sBlock, @sBlock )
            #endif

            @ 0,0 SAY "PP: "
            @ 0,4 SAY Pad( sBlock, 76 ) COLOR "N/R"
            DevPos( nRow, nCol )

            Eval( &( "{|| " + sBlock + " }" ) )

            #ifdef __CLIPPER__
               nBlockID := 0
               aSize( s_abBlocks, 0 )
            #endif
         ENDIF
      ENDIF

      nRow := Row()
      nCol := Col()

      @ 0,0 SAY "PP: "
      @ 0,4 SAY Pad( sPPed, 76 ) COLOR "N/R"

   END SEQUENCE

RETURN

//--------------------------------------------------------------//

FUNCTION PP_CompileLine( sPPed, nLine, aProcedures, aInitExit, nProcId )

   LOCAL nNext, sBlock, sTemp
   LOCAL sSymbol
   LOCAL nAt, nPos, cChr
   LOCAL nJumps, nJump
   LOCAL sCounter, sStart, sEnd, sStep
   LOCAL Dummy

   ExtractLeadingWS( @sPPed )
   DropTrailingWS( @sPPed )

   BEGIN SEQUENCE

      IF sPPed = "_HB_CLASS"
         BREAK
      ENDIF

      sTemp := sPPed
      WHILE ( nNext := nAtSkipStr( ';', sTemp ) ) > 0
         sBlock := Left( sTemp, nNext - 1 )

         sTemp  := RTrim( SubStr( sTemp, nNext + 1 ) )
         ExtractLeadingWS( @sTemp )

         ExtractLeadingWS( @sBlock )
         DropTrailingWS( @sBlock )

         IF ! ( sBlock == '' )
            IF sBlock = "#line"
               LOOP
            ENDIF

            #ifdef __CLIPPER__
                /* Clipper Macro Compiler can't compile nested blocks! */
                CompileNestedBlocks( sBlock, @sBlock )
            #endif

            IF ( nProcId == 0 .AND. sBlock = "PP_Statics" )
               Dummy := &( sBlock )
               LOOP
            ENDIF

            IF sBlock = "PP_PROC"
               sSymbol := Upper( LTrim( SubStr( sBlock, At( ' ', sBlock ) ) ) )
               aSize( aProcedures, ++nProcId )

               IF sBlock = "PP_PROC_PRG"
                  sSymbol := s_sModule + sSymbol
               ELSEIF sBlock = "PP_PROC_INIT"
                  aAdd( aInitExit[1], nProcId )
               ELSEIF sBlock = "PP_PROC_EXIT"
                  aAdd( aInitExit[2], nProcId )
               ENDIF

               //TraceLog( sSymbol , nProcId )
               aProcedures[nProcId] := { sSymbol, {} }
            ELSE
               IF sBlock = "PP__"
                  IF sBlock = "PP__FOR"
                     s_nFlowId++
                     aSize( s_acFlowType, s_nFlowId )
                     s_acFlowType[ s_nFlowId ] := "F"

                     sBlock := SubStr( sBlock, 9 )
                     sCounter := Left( sBlock, ( nAt := AT( ":=", sBlock ) ) - 1 )
                     sBlock   := SubStr( sBlock, nAt + 2 )
                     sStart   := Left( sBlock, ( nAt := At( "~TO~", sBlock ) ) - 1 )
                     sBlock   := SubStr( sBlock, nAt + 4 )
                     sEnd     := Left( sBlock, ( nAt := At( "~STEP~", sBlock ) ) - 1 )
                     sStep    := SubStr( sBlock, nAt + 6 )
                     IF sStep == ""
                        sStep := "1"
                     ENDIF

                     // No procedure declaration.
                     IF nProcId == 0
                        sSymbol := "Implied_Main"
                        aSize( aProcedures, ++nProcId )
                        aProcedures[nProcId] := { sSymbol, {} }
                     ENDIF

                     aAdd( aProcedures[ nProcId ][2], { 0, &( "{||" + sCounter + ":=" + sStart + "}" ), nLine } ) // Loop back

                     sBlock := sCounter + "<=" + sEnd

                     s_nCompLoop++
                     aSize( s_aLoopJumps, s_nCompLoop )
                     s_aLoopJumps[ s_nCompLoop ] := { Len( aProcedures[ nProcId ][2] ) + 1, {}, "F", &( "{||" + sCounter + ":=" + sCounter + "+" + sStep + "}" ) } // Address of line to later place conditional Jump instruction into.

                  ELSEIF sBlock = "PP__NEXT"

                     IF s_nCompLoop == 0 .OR. s_aLoopJumps[ s_nCompLoop ][3] != "F"
                        Alert( [NEXT does not match FOR] )
                     ELSE
                        aAdd( aProcedures[ nProcId ][2], { 0, s_aLoopJumps[ s_nCompLoop ][4], nLine } ) // STEP
                        aAdd( aProcedures[ nProcId ][2], { s_aLoopJumps[ s_nCompLoop ][1] - 1, NIL, nLine } ) // Loop back
                        aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction

                        nJumps := Len( s_aLoopJumps[s_nCompLoop][2] )
                        FOR nJump := 1 TO nJumps
                           aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][2][nJump] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the unconditional Jump Instruction
                        NEXT

                        s_nCompLoop--
                        //aSize( s_aIfJumps, s_nCompIf )
                     ENDIF

                     LOOP

                  ELSEIF sBlock = "PP__WHILE"
                     s_nFlowId++
                     aSize( s_acFlowType, s_nFlowId )
                     s_acFlowType[ s_nFlowId ] := "W"

                     sBlock := SubStr( sBlock, 11 )
                     s_nCompLoop++
                     aSize( s_aLoopJumps, s_nCompLoop )
                     s_aLoopJumps[ s_nCompLoop ] := { Len( aProcedures[ nProcId ][2] ) + 1, {}, "W" } // Address of line to later place conditional Jump instruction into.

                  ELSEIF sBlock = "PP__LOOP"

                     IF s_nCompLoop == 0
                        Alert( [LOOP with no loop in sight!] )
                     ELSE
                        IF s_aLoopJumps[ s_nCompLoop ][3] == "F"
                           aAdd( aProcedures[ nProcId ][2], { 0, s_aLoopJumps[ s_nCompLoop ][4], nLine } ) // STEP
                        ENDIF

                        aAdd( aProcedures[ nProcId ][2], { s_aLoopJumps[ s_nCompLoop ][1] - 1, NIL, nLine } ) // Loop back
                     ENDIF

                     LOOP

                  ELSEIF sBlock = "PP__EXIT"

                     sBlock := ""
                     IF s_nCompLoop == 0
                        Alert( [EXIT with no loop in sight!] )
                     ELSE
                        aAdd( s_aLoopJumps[ s_nCompLoop ][2], Len( aProcedures[ nProcId ][2] ) + 1 ) // Address of line to later place unconditional Jump instruction into.
                     ENDIF

                  ELSEIF sBlock = "PP__ENDDO"
                     s_nFlowId--
                     //aSize( s_acFlowType, s_nFlowId )

                     IF s_nCompLoop == 0
                        Alert( [ENDDO does not match WHILE] )
                     ELSE
                        aAdd( aProcedures[ nProcId ][2], { s_aLoopJumps[ s_nCompLoop ][1] - 1, NIL, nLine } ) // Loop back
                        aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction

                        nJumps := Len( s_aLoopJumps[s_nCompLoop][2] )
                        FOR nJump := 1 TO nJumps
                           aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][2][nJump] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the unconditional Jump Instruction
                        NEXT

                        s_nCompLoop--
                        //aSize( s_aIfJumps, s_nCompIf )
                     ENDIF

                     LOOP

                  ELSEIF sBlock = "PP__DOCASE"
                     s_nFlowId++
                     aSize( s_acFlowType, s_nFlowId )
                     s_acFlowType[ s_nFlowId ] := "C"

                     sBlock := ""//SubStr( sBlock, 12 )
                     s_nCompIf++
                     aSize( s_aIfJumps, s_nCompIf )
                     s_aIfJumps[ s_nCompIf ] := { 0, {}, "C", .F. } // Address of line to later place conditional Jump instruction into.

                  ELSEIF sBlock = "PP__CASE"

                     IF s_nCompIf == 0 .OR. s_aIfJumps[ s_nCompIf ][3] != "C" .OR. s_aIfJumps[ s_nCompIf ][4]
                        sBlock := ""
                        Alert( [CASE does not match DO CASE] )
                     ELSE
                        IF s_aIfJumps[ s_nCompIf ][1] > 0
                           aAdd( aProcedures[ nProcId ][2], { 0, NIL, nLine } ) // Place holder for unconditional Jump to END.
                           aAdd( s_aIfJumps[ s_nCompIf ][2], Len( aProcedures[ nProcId ][2] ) ) // Address of line to later place unconditional Jump instruction into.
                           aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction
                        ENDIF

                        sBlock := SubStr( sBlock, 10 )
                        s_aIfJumps[ s_nCompIf ][1] := Len( aProcedures[ nProcId ][2] ) + 1 // Address of line to later place conditional Jump instruction into.
                     ENDIF

                  ELSEIF sBlock = "PP__OTHERWISE"

                     sBlock := ""
                     IF s_nCompIf == 0 .OR. s_aIfJumps[ s_nCompIf ][3] != "C" .OR. s_aIfJumps[ s_nCompIf ][4]
                        Alert( [OTHERWISE does not match DO CASE] )
                     ELSE
                        s_aIfJumps[ s_nCompIf ][4] := .T.
                        IF s_aIfJumps[ s_nCompIf ][1] > 0
                           aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][1] ][1] := Len( aProcedures[ nProcId ][2] ) + 1 // Patching the previous conditional Jump Instruction
                           s_aIfJumps[ s_nCompIf ][1] := Len( aProcedures[ nProcId ][2] ) + 1 // Address of line to later place Jump instruction into.
                        ENDIF
                     ENDIF

                  ELSEIF sBlock = "PP__ENDCASE"
                     s_nFlowId--
                     //aSize( s_acFlowType, s_nFlowId )

                     IF s_nCompIf == 0
                        Alert( [ENDCASE with no DO CASE in sight!] )
                     ELSE
                        IF s_aIfJumps[ s_nCompIf ][1] > 0
                           aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction

                           nJumps := Len( s_aIfJumps[s_nCompIf][2] )
                           FOR nJump := 1 TO nJumps
                              aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][2][nJump] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the unconditional Jump Instruction
                           NEXT
                        ENDIF

                        s_nCompIf--
                        //aSize( s_aIfJumps, s_nCompIf )
                     ENDIF

                     LOOP

                  ELSEIF sBlock = "PP__IF"
                     s_nFlowId++
                     aSize( s_acFlowType, s_nFlowId )
                     s_acFlowType[ s_nFlowId ] := "I"

                     sBlock := SubStr( sBlock, 8 )
                     s_nCompIf++
                     aSize( s_aIfJumps, s_nCompIf )
                     s_aIfJumps[ s_nCompIf ] := { Len( aProcedures[ nProcId ][2] ) + 1, {}, "I", .F. } // Address of line to later place conditional Jump instruction into.

                  ELSEIF sBlock = "PP__ELSEIF"

                     IF s_nCompIf == 0 .OR. s_aIfJumps[ s_nCompIf ][3] != "I" .OR. s_aIfJumps[ s_nCompIf ][4]
                        Alert( [ELSEIF does not match IF] )
                        LOOP
                     ELSE
                        IF s_aIfJumps[ s_nCompIf ][1] > 0
                           aAdd( aProcedures[ nProcId ][2], { 0, NIL, nLine } ) // Place holder for unconditional Jump to END.
                           aAdd( s_aIfJumps[ s_nCompIf ][2], Len( aProcedures[ nProcId ][2] ) ) // Address of line to later place unconditional Jump instruction into.
                           aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction
                        ENDIF

                        sBlock := SubStr( sBlock, 12 )
                        s_aIfJumps[ s_nCompIf ][1] := Len( aProcedures[ nProcId ][2] ) + 1 // Address of line to later place Jump instruction into.
                     ENDIF

                  ELSEIF sBlock = "PP__ELSE"

                     sBlock := ""
                     IF s_nCompIf == 0 .OR. s_aIfJumps[ s_nCompIf ][3] != "I" .OR. s_aIfJumps[ s_nCompIf ][4]
                        Alert( [ELSE does not match IF] )
                        LOOP
                     ELSE
                        s_aIfJumps[ s_nCompIf ][4] := .T.
                        aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][1] ][1] := Len( aProcedures[ nProcId ][2] ) + 1 // Patching the prebvious conditional Jump Instruction
                        s_aIfJumps[ s_nCompIf ][1] := Len( aProcedures[ nProcId ][2] ) + 1 // Address of line to later place Jump instruction into.
                     ENDIF

                  ELSEIF sBlock = "PP__ENDIF"
                     s_nFlowId--
                     //aSize( s_acFlowType, s_nFlowId )

                     IF s_nCompIf == 0
                        Alert( [ENDIF does not match IF] )
                     ELSE
                        aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction

                        nJumps := Len( s_aIfJumps[s_nCompIf][2] )
                        FOR nJump := 1 TO nJumps
                           aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][2][nJump] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the unconditional Jump Instruction
                        NEXT

                        s_nCompIf--
                        //aSize( s_aIfJumps, s_nCompIf )
                     ENDIF

                     LOOP

                  ELSEIF sBlock = "PP__END"

                     IF s_nCompIf == 0 .AND. s_nCompLoop == 0
                        Alert( [END with no Flow-Control structure in sight!] )
                     ELSE
                        IF s_acFlowType[ s_nFlowId ] $ "FW"
                           IF s_acFlowType[ s_nFlowId ] $ "F"
                              aAdd( aProcedures[ nProcId ][2], { 0, s_aLoopJumps[ s_nCompLoop ][4], nLine } ) // STEP
                           ENDIF
                           aAdd( aProcedures[ nProcId ][2], { s_aLoopJumps[ s_nCompLoop ][1] - 1, NIL, nLine } ) // Loop back

                           aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction

                           nJumps := Len( s_aLoopJumps[s_nCompLoop][2] )
                           FOR nJump := 1 TO nJumps
                              aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][2][nJump] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the unconditional Jump Instruction
                           NEXT

                           s_nCompLoop--
                           //aSize( s_aLoopJumps, s_nCompLoop )
                        ELSE
                           aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction

                           nJumps := Len( s_aIfJumps[s_nCompIf][2] )
                           FOR nJump := 1 TO nJumps
                              aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][2][nJump] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the unconditional Jump Instruction
                           NEXT

                           s_nCompIf--
                           //aSize( s_aIfJumps, s_nCompIf )
                        ENDIF
                     ENDIF

                     s_nFlowId--
                     //aSize( s_acFlowType, s_nFlowId )

                     LOOP

                  ENDIF
               ELSE
                  nAt := At( '=', sBlock )
                  IF nAt > 1 .AND. SubStr( sBlock, nAt - 1, 1 ) != ':'
                     nAt--
                     FOR nPos := 1 TO nAt
                        cChr := SubStr( sBlock, nPos, 1 )
                        IF ! ( IsAlpha( cChr ) .OR. IsDigit( cChr ) .OR. cChr $ "[]:&._ " )
                           EXIT
                        ENDIF
                     NEXT
                     IF nPos > nAt
                        sBlock := Left( sBlock, nAt ) + ":" + SubStr( sBlock, nPos )
                     ENDIF
                  ENDIF
               ENDIF

               // No procedure declaration.
               IF nProcId == 0
                  sSymbol := "Implied_Main"
                  aSize( aProcedures, ++nProcId )
                  aProcedures[nProcId] := { sSymbol, {} }
               ENDIF

               IF sBlock == ""
                  aAdd( aProcedures[ nProcId ][2], { 0, NIL, nLine } )
               ELSE
                  //? nLine, nProcId, sBlock
                  //TraceLog( sBlock )
                  aAdd( aProcedures[ nProcId ][2], { 0, &( "{||" + sBlock + "}" ), nLine } )
               ENDIF
            ENDIF
         ENDIF

      ENDDO

      sBlock := sTemp
      DropTrailingWS( @sBlock )

      IF sBlock = "#line"
         BREAK
      ENDIF

      IF ! ( sBlock == '' )
         #ifdef __CLIPPER__
             /* Clipper Macro Compiler can't compile nested blocks! */
             CompileNestedBlocks( sBlock, @sBlock )
         #endif

         IF ( nProcId == 0 .AND. sBlock = "PP_Statics" )
            Dummy := &( sBlock )
            BREAK
         ENDIF

         IF sBlock = "PP_PROC"
            sSymbol := Upper( LTrim( SubStr( sBlock, At( ' ', sBlock ) ) ) )
            aSize( aProcedures, ++nProcId )

            IF sBlock = "PP_PROC_PRG"
               sSymbol := s_sModule + sSymbol
            ELSEIF sBlock = "PP_PROC_INIT"
               aAdd( aInitExit[1], nProcId )
            ELSEIF sBlock = "PP_PROC_EXIT"
               aAdd( aInitExit[2], nProcId )
            ENDIF

            //TraceLog( sSymbol , nProcId )
            aProcedures[nProcId] := { sSymbol, {} }
         ELSE
            IF sBlock = "PP__"
               IF sBlock = "PP__FOR"
                  s_nFlowId++
                  aSize( s_acFlowType, s_nFlowId )
                  s_acFlowType[ s_nFlowId ] := "F"

                  sBlock := SubStr( sBlock, 9 )
                  sCounter := Left( sBlock, ( nAt := AT( ":=", sBlock ) ) - 1 )
                  sBlock   := SubStr( sBlock, nAt + 2 )
                  sStart   := Left( sBlock, ( nAt := At( "~TO~", sBlock ) ) - 1 )
                  sBlock   := SubStr( sBlock, nAt + 4 )
                  sEnd     := Left( sBlock, ( nAt := At( "~STEP~", sBlock ) ) - 1 )
                  sStep    := SubStr( sBlock, nAt + 6 )
                  IF sStep == ""
                     sStep := "1"
                  ENDIF

                  // No procedure declaration.
                  IF nProcId == 0
                     sSymbol := "Implied_Main"
                     aSize( aProcedures, ++nProcId )
                     aProcedures[nProcId] := { sSymbol, {} }
                  ENDIF

                  aAdd( aProcedures[ nProcId ][2], { 0, &( "{||" + sCounter + ":=" + sStart + "}" ), nLine } ) // Loop back

                  sBlock := sCounter + "<=" + sEnd

                  s_nCompLoop++
                  aSize( s_aLoopJumps, s_nCompLoop )
                  s_aLoopJumps[ s_nCompLoop ] := { Len( aProcedures[ nProcId ][2] ) + 1, {}, "F", &( "{||" + sCounter + ":=" + sCounter + "+" + sStep + "}" ) } // Address of line to later place conditional Jump instruction into.

               ELSEIF sBlock = "PP__NEXT"

                  IF s_nCompLoop == 0 .OR. s_aLoopJumps[ s_nCompLoop ][3] != "F"
                     Alert( [NEXT does not match FOR] )
                  ELSE
                     aAdd( aProcedures[ nProcId ][2], { 0, s_aLoopJumps[ s_nCompLoop ][4], nLine } ) // STEP
                     aAdd( aProcedures[ nProcId ][2], { s_aLoopJumps[ s_nCompLoop ][1] - 1, NIL, nLine } ) // Loop back
                     aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction

                     nJumps := Len( s_aLoopJumps[s_nCompLoop][2] )
                     FOR nJump := 1 TO nJumps
                        aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][2][nJump] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the unconditional Jump Instruction
                     NEXT

                     s_nCompLoop--
                     //aSize( s_aIfJumps, s_nCompIf )
                  ENDIF

                  BREAK

               ELSEIF sBlock = "PP__WHILE"
                  s_nFlowId++
                  aSize( s_acFlowType, s_nFlowId )
                  s_acFlowType[ s_nFlowId ] := "W"

                  sBlock := SubStr( sBlock, 11 )
                  s_nCompLoop++
                  aSize( s_aLoopJumps, s_nCompLoop )
                  s_aLoopJumps[ s_nCompLoop ] := { Len( aProcedures[ nProcId ][2] ) + 1, {}, "W" } // Address of line to later place conditional Jump instruction into.

               ELSEIF sBlock = "PP__LOOP"

                  IF s_nCompLoop == 0
                     Alert( [LOOP with no loop in sight!] )
                  ELSE
                     IF s_aLoopJumps[ s_nCompLoop ][3] == "F"
                        aAdd( aProcedures[ nProcId ][2], { 0, s_aLoopJumps[ s_nCompLoop ][4], nLine } ) // STEP
                     ENDIF

                     aAdd( aProcedures[ nProcId ][2], { s_aLoopJumps[ s_nCompLoop ][1] - 1, NIL, nLine } ) // Loop back
                  ENDIF

                  BREAK

               ELSEIF sBlock = "PP__EXIT"

                  sBlock := ""
                  IF s_nCompLoop == 0
                     Alert( [EXIT with no loop in sight!] )
                  ELSE
                     aAdd( s_aLoopJumps[ s_nCompLoop ][2], Len( aProcedures[ nProcId ][2] ) + 1 ) // Address of line to later place unconditional Jump instruction into.
                  ENDIF

               ELSEIF sBlock = "PP__ENDDO"
                  s_nFlowId--
                  //aSize( s_acFlowType, s_nFlowId )

                  IF s_nCompLoop == 0
                     Alert( [ENDDO does not match WHILE] )
                  ELSE
                     aAdd( aProcedures[ nProcId ][2], { s_aLoopJumps[ s_nCompLoop ][1] - 1, NIL, nLine } ) // Loop back
                     aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction

                     nJumps := Len( s_aLoopJumps[s_nCompLoop][2] )
                     FOR nJump := 1 TO nJumps
                        aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][2][nJump] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the unconditional Jump Instruction
                     NEXT

                     s_nCompLoop--
                     //aSize( s_aIfJumps, s_nCompIf )
                  ENDIF

                  BREAK

               ELSEIF sBlock = "PP__DOCASE"
                  s_nFlowId++
                  aSize( s_acFlowType, s_nFlowId )
                  s_acFlowType[ s_nFlowId ] := "C"

                  sBlock := ""//SubStr( sBlock, 12 )
                  s_nCompIf++
                  aSize( s_aIfJumps, s_nCompIf )
                  s_aIfJumps[ s_nCompIf ] := { 0, {}, "C", .F. } // Address of line to later place conditional Jump instruction into.

               ELSEIF sBlock = "PP__CASE"

                  IF s_nCompIf == 0 .OR. s_aIfJumps[ s_nCompIf ][3] != "C" .OR. s_aIfJumps[ s_nCompIf ][4]
                     sBlock := ""
                     Alert( [CASE does not match DO CASE] )
                  ELSE
                     IF s_aIfJumps[ s_nCompIf ][1] > 0
                        aAdd( aProcedures[ nProcId ][2], { 0, NIL, nLine } ) // Place holder for unconditional Jump to END.
                        aAdd( s_aIfJumps[ s_nCompIf ][2], Len( aProcedures[ nProcId ][2] ) ) // Address of line to later place unconditional Jump instruction into.
                        aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction
                     ENDIF

                     sBlock := SubStr( sBlock, 10 )
                     s_aIfJumps[ s_nCompIf ][1] := Len( aProcedures[ nProcId ][2] ) + 1 // Address of line to later place conditional Jump instruction into.
                  ENDIF

               ELSEIF sBlock = "PP__OTHERWISE"

                  sBlock := ""
                  IF s_nCompIf == 0 .OR. s_aIfJumps[ s_nCompIf ][3] != "C" .OR. s_aIfJumps[ s_nCompIf ][4]
                     Alert( [OTHERWISE does not match DO CASE] )
                  ELSE
                     s_aIfJumps[ s_nCompIf ][4] := .T.
                     IF s_aIfJumps[ s_nCompIf ][1] > 0
                        aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][1] ][1] := Len( aProcedures[ nProcId ][2] ) + 1 // Patching the previous conditional Jump Instruction
                        s_aIfJumps[ s_nCompIf ][1] := Len( aProcedures[ nProcId ][2] ) + 1 // Address of line to later place Jump instruction into.
                     ENDIF
                  ENDIF

               ELSEIF sBlock = "PP__ENDCASE"
                  s_nFlowId--
                  //aSize( s_acFlowType, s_nFlowId )

                  IF s_nCompIf == 0
                     Alert( [ENDCASE with no DO CASE in sight!] )
                  ELSE
                     IF s_aIfJumps[ s_nCompIf ][1] > 0
                        aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction

                        nJumps := Len( s_aIfJumps[s_nCompIf][2] )
                        FOR nJump := 1 TO nJumps
                           aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][2][nJump] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the unconditional Jump Instruction
                        NEXT
                     ENDIF

                     s_nCompIf--
                     //aSize( s_aIfJumps, s_nCompIf )
                  ENDIF

                  BREAK

               ELSEIF sBlock = "PP__IF"
                  s_nFlowId++
                  aSize( s_acFlowType, s_nFlowId )
                  s_acFlowType[ s_nFlowId ] := "I"

                  sBlock := SubStr( sBlock, 8 )
                  s_nCompIf++
                  aSize( s_aIfJumps, s_nCompIf )
                  s_aIfJumps[ s_nCompIf ] := { Len( aProcedures[ nProcId ][2] ) + 1, {}, "I", .F. } // Address of line to later place conditional Jump instruction into.

               ELSEIF sBlock = "PP__ELSEIF"

                  IF s_nCompIf == 0 .OR. s_aIfJumps[ s_nCompIf ][3] != "I" .OR. s_aIfJumps[ s_nCompIf ][4]
                     Alert( [ELSEIF does not match IF] )
                     BREAK
                  ELSE
                     IF s_aIfJumps[ s_nCompIf ][1] > 0
                        aAdd( aProcedures[ nProcId ][2], { 0, NIL, nLine } ) // Place holder for unconditional Jump to END.
                        aAdd( s_aIfJumps[ s_nCompIf ][2], Len( aProcedures[ nProcId ][2] ) ) // Address of line to later place unconditional Jump instruction into.
                        aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction
                     ENDIF

                     sBlock := SubStr( sBlock, 12 )
                     s_aIfJumps[ s_nCompIf ][1] := Len( aProcedures[ nProcId ][2] ) + 1 // Address of line to later place Jump instruction into.
                  ENDIF

               ELSEIF sBlock = "PP__ELSE"

                  sBlock := ""
                  IF s_nCompIf == 0 .OR. s_aIfJumps[ s_nCompIf ][3] != "I" .OR. s_aIfJumps[ s_nCompIf ][4]
                     Alert( [ELSE does not match IF] )
                     BREAK
                  ELSE
                     s_aIfJumps[ s_nCompIf ][4] := .T.
                     aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][1] ][1] := Len( aProcedures[ nProcId ][2] ) + 1 // Patching the prebvious conditional Jump Instruction
                     s_aIfJumps[ s_nCompIf ][1] := Len( aProcedures[ nProcId ][2] ) + 1 // Address of line to later place Jump instruction into.
                  ENDIF

               ELSEIF sBlock = "PP__ENDIF"
                  s_nFlowId--
                  //aSize( s_acFlowType, s_nFlowId )

                  IF s_nCompIf == 0
                     Alert( [ENDIF does not match IF] )
                  ELSE
                     aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction

                     nJumps := Len( s_aIfJumps[s_nCompIf][2] )
                     FOR nJump := 1 TO nJumps
                        aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][2][nJump] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the unconditional Jump Instruction
                     NEXT

                     s_nCompIf--
                     //aSize( s_aIfJumps, s_nCompIf )
                  ENDIF

                  BREAK

               ELSEIF sBlock = "PP__END"

                  IF s_nCompIf == 0 .AND. s_nCompLoop == 0
                     Alert( [END with no Flow-Control structure in sight!] )
                  ELSE
                     IF s_acFlowType[ s_nFlowId ] $ "FW"
                        IF s_acFlowType[ s_nFlowId ] $ "F"
                           aAdd( aProcedures[ nProcId ][2], { 0, s_aLoopJumps[ s_nCompLoop ][4], nLine } ) // STEP
                        ENDIF
                        aAdd( aProcedures[ nProcId ][2], { s_aLoopJumps[ s_nCompLoop ][1] - 1, NIL, nLine } ) // Loop back

                        aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction

                        nJumps := Len( s_aLoopJumps[s_nCompLoop][2] )
                        FOR nJump := 1 TO nJumps
                           aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][2][nJump] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the unconditional Jump Instruction
                        NEXT

                        s_nCompLoop--
                        //aSize( s_aLoopJumps, s_nCompLoop )
                     ELSE
                        aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction

                        nJumps := Len( s_aIfJumps[s_nCompIf][2] )
                        FOR nJump := 1 TO nJumps
                           aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][2][nJump] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the unconditional Jump Instruction
                        NEXT

                        s_nCompIf--
                        //aSize( s_aIfJumps, s_nCompIf )
                     ENDIF
                  ENDIF

                  s_nFlowId--
                  //aSize( s_acFlowType, s_nFlowId )

                  BREAK

               ENDIF
            ELSE
               nAt := At( '=', sBlock )
               IF nAt > 1 .AND. SubStr( sBlock, nAt - 1, 1 ) != ':'
                  nAt--
                  FOR nPos := 1 TO nAt
                     cChr := SubStr( sBlock, nPos, 1 )
                     IF ! ( IsAlpha( cChr ) .OR. IsDigit( cChr ) .OR. cChr $ "[]:&._ " )
                        EXIT
                     ENDIF
                  NEXT
                  IF nPos > nAt
                     sBlock := Left( sBlock, nAt ) + ":" + SubStr( sBlock, nPos )
                  ENDIF
               ENDIF
            ENDIF

            // No procedure declaration.
            IF nProcId == 0
               sSymbol := "Implied_Main"
               aSize( aProcedures, ++nProcId )
               aProcedures[nProcId] := { sSymbol, {} }
            ENDIF

            IF sBlock == ""
               aAdd( aProcedures[ nProcId ][2], { 0, NIL, nLine } )
            ELSE
               //? nLine, nProcId, sBlock
               //TraceLog( sBlock )
               aAdd( aProcedures[ nProcId ][2], { 0, &( "{||" + sBlock + "}" ), nLine } )
            ENDIF
         ENDIF
      ENDIF

   END SEQUENCE

RETURN aProcedures

//--------------------------------------------------------------//

FUNCTION PP_ProcName( nLevel )

   IF nLevel == NIL
      nLevel := 0
   ENDIF

   IF nLevel >= 0 .AND. nLevel < s_nProcStack
      RETURN s_aProcStack[ s_nProcStack - nLevel ][1]
   ENDIF

RETURN ""

//--------------------------------------------------------------//

FUNCTION PP_ProcLine( nLevel )

   IF nLevel == NIL
      nLevel := 0
   ENDIF

   IF nLevel >= 0 .AND. nLevel < s_nProcStack
      RETURN s_aProcStack[ s_nProcStack - nLevel ][2]
   ENDIF

RETURN 0

//--------------------------------------------------------------//

PROCEDURE PP_LocalParams( aVars )

   LOCAL nVar, nVars := Len( aVars ), xInit, nParams

   FOR nVar := 1 TO nVars
      IF ( nParams := Len( s_aParams ) ) > 0
         xInit := s_aParams[1]
         aDel( s_aParams, 1 )
         aSize( s_aParams, nParams - 1 )
      ELSE
         xInit := NIL
      ENDIF

      IF Type( aVars[nVar] ) = 'U'
         __QQPUB( aVars[nVar] )
         &( aVars[nVar] ) := xInit
         aAdd( s_asLocals, aVars[nVar] )
      ELSE
         Alert( PP_ProcName() + " (" + LTrim( Str( PP_ProcLine() ) ) +  ")" + [ Declared Parameter redeclaration: ] + aVars[nVar] )
      ENDIF
   NEXT

RETURN

//--------------------------------------------------------------//

PROCEDURE PP_Params( aVars )

   LOCAL nVar, nVars := Len( aVars ), xInit, nParams

   FOR nVar := 1 TO nVars
      IF ( nParams := Len( s_aParams ) ) > 0
         xInit := s_aParams[1]
         aDel( s_aParams, 1 )
         aSize( s_aParams, nParams - 1 )
      ELSE
         xInit := NIL
      ENDIF

      IF Type( aVars[nVar] ) = 'U'
         __QQPUB( aVars[nVar] )
         &( aVars[nVar] ) := xInit
         aAdd( s_asPrivates, aVars[nVar] )
      ELSE
         Alert( PP_ProcName() + " (" + LTrim( Str( PP_ProcLine() ) ) +  ")" + [ Parameter redeclaration: ] + aVars[nVar] )
      ENDIF
   NEXT

RETURN

//--------------------------------------------------------------//

PROCEDURE PP_Privates( aVars )

   LOCAL nVar, nVars := Len( aVars ), nAt, cInit

   FOR nVar := 1 TO nVars
      IF ( nAt := At( ":=", aVars[nVar] ) ) > 0
         cInit := LTrim( SubStr( aVars[nVar], nAt + 2 ) )
         aVars[nVar] := RTrim( Left( aVars[nVar], nAt - 1 ) )
      ELSE
         cInit := "NIL"
      ENDIF

      IF aScan( s_asPrivates, aVars[nVar] ) == 0
         __QQPUB( aVars[nVar] )
         &( aVars[nVar] ) := &( cInit )
         aAdd( s_asPrivates, aVars[nVar] )
      ELSE
         Alert( PP_ProcName() + " (" + LTrim( Str( PP_ProcLine() ) ) +  ")" + [ Private redeclaration: ] + aVars[nVar] )
      ENDIF
   NEXT

RETURN

//--------------------------------------------------------------//

PROCEDURE PP_Locals( aVars )

   LOCAL nVar, nVars := Len( aVars ), nAt, cInit

   FOR nVar := 1 TO nVars
      IF ( nAt := At( ":=", aVars[nVar] ) ) > 0
         cInit := LTrim( SubStr( aVars[nVar], nAt + 2 ) )
         aVars[nVar] := RTrim( Left( aVars[nVar], nAt - 1 ) )
      ELSE
         cInit := "NIL"
      ENDIF

      IF Type( aVars[nVar] ) = 'U'
         __QQPUB( aVars[nVar] )
         &( aVars[nVar] ) := &( cInit )
         aAdd( s_asLocals, aVars[nVar] )
      ELSE
         Alert( [Local redeclaration: ] + aVars[nVar] )
      ENDIF
   NEXT

RETURN
//--------------------------------------------------------------//

PROCEDURE PP_Publics( aVars )

   LOCAL nVar, nVars := Len( aVars ), nAt, cInit

   FOR nVar := 1 TO nVars
      IF ( nAt := At( ":=", aVars[nVar] ) ) > 0
         cInit := LTrim( SubStr( aVars[nVar], nAt + 2 ) )
         aVars[nVar] := RTrim( Left( aVars[nVar], nAt - 1 ) )
      ELSE
         cInit := ".F."
      ENDIF

      IF aScan( s_asPublics, aVars[nVar] ) == 0
         __QQPUB( aVars[nVar] )
         &( aVars[nVar] ) := &( cInit )
         aAdd( s_asPublics, aVars[nVar] )
      ELSE
         Alert( [Public redeclaration: ] + aVars[nVar] )
      ENDIF
   NEXT

RETURN

//--------------------------------------------------------------//

PROCEDURE PP_Statics( aVars )

   LOCAL nVar, nVars := Len( aVars ), nAt, cInit

   FOR nVar := 1 TO nVars
      IF ( nAt := At( ":=", aVars[nVar] ) ) > 0
         cInit := LTrim( SubStr( aVars[nVar], nAt + 2 ) )
         aVars[nVar] := RTrim( Left( aVars[nVar], nAt - 1 ) )
      ELSE
         cInit := "NIL"
      ENDIF

      IF ( Left( Type( aVars[nVar] ), 1 ) ) = 'U'
         __QQPUB( aVars[nVar] )
         &( aVars[nVar] ) := &( cInit )
         aAdd( s_asStatics, aVars[nVar] )
      ELSE
         Alert( [Type: ] + Type( aVars[nVar] ) + [Static redeclaration: '] + aVars[nVar] )
      ENDIF
   NEXT

RETURN

//--------------------------------------------------------------//

PROCEDURE PP_Run( cFile, aParams, sPPOExt, bBlanks )

   LOCAL nBaseProc := s_nProcId, sPresetModule := s_sModule, nProc

   //PP_PreProFile( "rp_run.ch" )
   IF bBlanks == NIL
      bBlanks := .T.
   ENDIF

   IF ! s_lRunLoaded
      s_lRunLoaded := .T.
      InitRunRules()
      InitRunResults()
      IF Len( aDefRules ) != Len( aDefResults )
         Alert( [Run #DEFINE Rules size mismatch] )
      ENDIF
      IF Len( aTransRules ) != Len( aTransResults )
         Alert( [Run #TRANSLATE Rules size mismatch] )
      ENDIF
      IF Len( aCommRules ) != Len( aCommResults )
         Alert( [Run #DEFINE Rules size mismatch] )
      ENDIF
   ENDIF

   s_sModule := cFile
   bCompile  := .T.
   PP_PreProFile( cFile, sPPOExt, bBlanks )
   bCompile  := .F.

   PP_Exec( s_aProcedures, s_aInitExit, s_nProcId, aParams )

   #ifdef __CLIPPER__
      Memory(-1)
   #else

   #endif

   s_sModule := sPresetModule

RETURN

//--------------------------------------------------------------//

PROCEDURE PP_SetReturn( xRet )

   s_xRet := xRet

RETURN

//--------------------------------------------------------------//

PROCEDURE RP_Dot_Err( oErr )

   LOCAL Counter, xArg, sArgs := ""

   IF ValType( oErr:Args ) == 'A'
      sArgs := " - Arguments: "

      FOR Counter := 1 TO Len( oErr:Args )
         xArg := oErr:Args[Counter]

         DO CASE
            CASE xArg == NIL
               sArgs += "NIL; "

            CASE ValType( xArg ) == 'A'
               sArgs += "{}; "

            CASE ValType( xArg ) == 'B'
               sArgs += "{|| }; "

            CASE ValType( xArg ) == 'C'
               sArgs += '"' + xArg + '"; '

            CASE ValType( xArg ) == 'D'
               sArgs +=  dtoc( xArg ) + "; "

            CASE ValType( xArg ) == 'L'
               sArgs += IIF( xArg, ".T.; ", ".F.; " )

            CASE ValType( xArg ) == 'N'
               sArgs +=  Str( xArg ) + "; "

            CASE ValType( xArg ) == 'O'
               sArgs +=  "{o}"

            OTHERWISE
               sArgs +=  '[' + ValType( xArg ) + "]; "
         ENDCASE
      NEXT

      sArgs := Left( sArgs, Len( sArgs ) -2 )
   ENDIF

   Alert( [Sorry, could not execute: ] + oErr:Description + sArgs + " " + ProcName(2) + '[' + Str( ProcLine(2) ) + ']')

   BREAK

//RETURN // Unreacable code

//--------------------------------------------------------------//

PROCEDURE RP_Comp_Err( oErr, sLine, nLine )

   LOCAL Counter, xArg, sArgs := ""

   IF ValType( oErr:Args ) == 'A'
      sArgs := " - Arguments: "

      FOR Counter := 1 TO Len( oErr:Args )
         xArg := oErr:Args[Counter]

         DO CASE
            CASE xArg == NIL
               sArgs += "NIL; "

            CASE ValType( xArg ) == 'A'
               sArgs += "{}; "

            CASE ValType( xArg ) == 'B'
               sArgs += "{|| }; "

            CASE ValType( xArg ) == 'C'
               sArgs += '"' + xArg + '"; '

            CASE ValType( xArg ) == 'D'
               sArgs +=  dtoc( xArg ) + "; "

            CASE ValType( xArg ) == 'L'
               sArgs += IIF( xArg, ".T.; ", ".F.; " )

            CASE ValType( xArg ) == 'N'
               sArgs +=  Str( xArg ) + "; "

            CASE ValType( xArg ) == 'O'
               sArgs +=  "{o}"

            OTHERWISE
               sArgs +=  '[' + ValType( xArg ) + "]; "
         ENDCASE
      NEXT

      sArgs := Left( sArgs, Len( sArgs ) -2 )
   ENDIF

   TraceLog( "Line: " + Str( nLine, 4 ) + " could not compile: '" + sLine + "';" + oErr:Description + sArgs + " " + ProcName(2) + '[' + Str( ProcLine(2) ) + ']')
   Alert( [Line: ] + Str( nLine, 4 ) + [ could not compile: ]+"'" + sLine + "';" + oErr:Description + sArgs + " " + ProcName(2) + '[' + Str( ProcLine(2) ) + ']')

   BREAK

//RETURN // Unreacable code

//--------------------------------------------------------------//

FUNCTION RP_Run_Err( oErr, aProcedures )

   LOCAL Counter, xArg, sArgs := "", nProc, sProc

   IF ValType( oErr:Args ) == 'A'
      sArgs := " - Arguments: "

      FOR Counter := 1 TO Len( oErr:Args )
         xArg := oErr:Args[Counter]

         DO CASE
            CASE xArg == NIL
               sArgs += "NIL; "

            CASE ValType( xArg ) == 'A'
               sArgs += "{}; "

            CASE ValType( xArg ) == 'B'
               sArgs += "{|| }; "

            CASE ValType( xArg ) == 'C'
               sArgs += '"' + xArg + '"; '

            CASE ValType( xArg ) == 'D'
               sArgs +=  dtoc( xArg ) + "; "

            CASE ValType( xArg ) == 'L'
               sArgs += IIF( xArg, ".T.; ", ".F.; " )

            CASE ValType( xArg ) == 'N'
               sArgs +=  Str( xArg ) + "; "

            CASE ValType( xArg ) == 'O'
               sArgs +=  "{o}"

            OTHERWISE
               sArgs +=  '[' + ValType( xArg ) + "]; "
         ENDCASE
      NEXT

      sArgs := Left( sArgs, Len( sArgs ) -2 )
   ENDIF

   IF oErr:SubCode == 1001
      IF s_sModule != NIL
         sProc := s_sModule + oErr:Operation //ProcName( 2 + 2 )
         nProc := aScan( aProcedures, {|aProc| aProc[1] == sProc } )
      ELSE
         nProc := 0
      ENDIF
      IF nProc == 0
         sProc := oErr:Operation //ProcName( 2 + 2 )
         nProc := aScan( aProcedures, {|aProc| aProc[1] == sProc } )
      ENDIF

      IF nProc > 0
         s_xRet := NIL
         IF ValType( oErr:Args ) == 'A'
            s_aParams := oErr:Args
         ELSE
            s_aParams := {}
         ENDIF

         PP_ExecProcedure( aProcedures[nProc] )
         IF oErr:CanSubstitute
            RETURN ( s_xRet )
         ELSEIF oErr:CanDefault
            Alert( [Must Default: ] + "'" + oErr:Operation + "' " + oErr:Description + sArgs + " " + PP_ProcName() + '(' + LTrim( Str( PP_ProcLine() ) ) + ") " + ProcName(2)  + "(" + LTrim( Str( ProcLine(2) ) ) + ")" )
            RETURN  ( .F. )
         ELSE
            Alert( [No Recovery for: ] + "'" + oErr:Operation + "' " + oErr:Description + sArgs + " " + PP_ProcName() + '(' + LTrim( Str( PP_ProcLine() ) ) + ") " + ProcName(2)  + "(" + LTrim( Str( ProcLine(2) ) ) + ")" )
            BREAK nProc
         ENDIF
      ENDIF
   ENDIF

   TraceLog( s_sModule, "Sorry, R/T Error: [" + oErr:SubSystem + "/" + LTrim( Str( oErr:SubCode ) ) +  "] '" + oErr:Operation + "' " + oErr:Description + sArgs + " " + PP_ProcName() + '(' + LTrim( Str( PP_ProcLine() ) ) + ") " + ProcName(2)  + "(" + LTrim( Str( ProcLine(2) ) ) + ")" )
   Alert( [Sorry, R/T Error: ] + "[" + oErr:SubSystem + "/" + LTrim( Str( oErr:SubCode ) ) + "] '" + oErr:Operation + "' " + oErr:Description + sArgs + " " + PP_ProcName() + '(' + LTrim( Str( PP_ProcLine() ) ) + ") " + ProcName(2)  + "(" + LTrim( Str( ProcLine(2) ) ) + ")" )

   BREAK oErr

RETURN NIL // Unreacable code

//--------------------------------------------------------------//

FUNCTION __SetIf( bExp )

   IF nIf > 0 .AND. ! abIf[nIf]
      bExp := .F.
   ENDIF

   nIf++
   aSize( abIf, nIf )
   abIf[nIf] := bExp

RETURN abIf[nIf]

//--------------------------------------------------------------//

FUNCTION __SetElseIf( bExp )

   IF nIf > 1 .AND.  ! abIf[nIf - 1]
      RETURN .F.
   ENDIF

   abIf[nIf] := ! abIf[nIf]

   IF abIf[nIf]
      abIf[nIf] := bExp
   ENDIF

RETURN abIf[nIf]

//--------------------------------------------------------------//

FUNCTION __SetElse()

   IF nIf > 1 .AND.  ! abIf[nIf - 1]
      RETURN .F.
   ENDIF

   abIf[nIf] := ! abIf[nIf]

RETURN abIf[nIf]

//--------------------------------------------------------------//

FUNCTION __SetEnd()

   IF nIf > 0
      nIf--
   ELSE
      Alert( [END with no IF in sight!] )
   ENDIF

RETURN nIf

//--------------------------------------------------------------//

FUNCTION __SetDoCase()

   nIf++
   aSize( abIf, nIf )
   abIf[nIf] := .F.

RETURN abIf[nIf]

//--------------------------------------------------------------//

FUNCTION __SetCase( bExp )

   IF nIf > 1 .AND.  ! abIf[nIf - 1]
      RETURN .F.
   ENDIF

   abIf[nIf] := ! abIf[nIf]

   IF abIf[nIf]
      abIf[nIf] := bExp
   ENDIF

RETURN abIf[nIf]

//--------------------------------------------------------------//

FUNCTION __SetOtherwise()

   IF nIf > 1 .AND.  ! abIf[nIf - 1]
      RETURN .F.
   ENDIF

   abIf[nIf] := ! abIf[nIf]

RETURN abIf[nIf]

//--------------------------------------------------------------//

FUNCTION __SetEndCase()

   IF nIf > 0
      nIf--
   ELSE
      Alert( [ENDCASE with no DO CASE in sight!] )
   ENDIF

RETURN nIf

//--------------------------------------------------------------//

#ifdef __CLIPPER__

   //--------------------------------------------------------------//

   STATIC FUNCTION CompileNestedBlocks( sTemp, sMain )

      LOCAL asBlocks, nBlocks, Counter, aReplace

      asBlocks := asBlocks(sTemp )
      nBlocks  := Len( asBlocks )

      FOR Counter := 1 TO nBlocks
         aReplace := CompileNestedBlocks( SubStr( asBlocks[Counter], 2 ), @sMain )
      NEXT

      IF ProcName(1) == ProcName(0) // .AND. nBlocks == 0
         IF aReplace != NIL
            sTemp := StrTran( sTemp, aReplace[1], aReplace[2] )
         ELSE
            aReplace := Array(2)
         ENDIF

         aReplace[1] :=  '{' + sTemp
         aReplace[2] := "PP_Block(" + LTrim( Str( ++nBlockId, 3, 0 ) ) + ')'
         aAdd( s_abBlocks, &( aReplace[1]) )

         sMain := StrTran( sMain, aReplace[1], aReplace[2] )

         RETURN aReplace
      ENDIF

   RETURN NIL

   //--------------------------------------------------------------//

   FUNCTION asBlocks( sBlock, asBlocks )

      LOCAL nStart, nEnd, nPosition, sNested, nOpen, lBlock := .F.

      IF asBlocks == NIL
         asBlocks := {}
      ENDIF

      nStart := At( '{', sBlock )
      IF nStart > 0
         nEnd   := Len( sBlock )
         FOR nPosition := nStart + 1 TO nEnd
            IF SubStr( sBlock, nPosition, 1 ) != ' '
               EXIT
            ENDIF
         NEXT
         IF SubStr( sBlock, nPosition, 1 ) == '|'
            lBlock := .T.
         ENDIF

         nOpen := 1
         DO WHILE nOpen > 0 .AND. nPosition <= nEnd
            IF SubStr( sBlock, nPosition, 1 ) == '"'
               DO WHILE nPosition <= nEnd
                  nPosition++
                  IF SubStr( sBlock, nPosition, 1 ) == '"'
                     EXIT
                  ENDIF
               ENDDO
            ELSEIF SubStr( sBlock, nPosition, 1 ) == "'"
               DO WHILE nPosition <= nEnd
                  nPosition++
                  IF SubStr( sBlock, nPosition, 1 ) == "'"
                     EXIT
                  ENDIF
               ENDDO
            ELSEIF SubStr( sBlock, nPosition, 1 ) == '{'
               nOpen++
            ELSEIF SubStr( sBlock, nPosition, 1 ) == '}'
               nOpen--
            ENDIF
            nPosition++
         ENDDO
      ENDIF

      IF lBlock
         sNested := SubStr( sBlock, nStart, ( nPosition - nStart ) )
         aAdd( asBlocks, sNested )
         asBlocks( SubStr( sBlock, nPosition + 1 ), asBlocks )
      ENDIF

   RETURN asBlocks

   //--------------------------------------------------------------//

   FUNCTION PP_Block( nId )

   RETURN s_abBlocks[nId]

   //--------------------------------------------------------------//
#endif

//------------------------------- *** END - RP DOT Functions *** -------------------------------//

FUNCTION PP_PreProFile( sSource, sPPOExt, bBlanks, bDirectivesOnly )

   LOCAL hSource, sBuffer, sLine, nPosition, sExt, cPrev
   LOCAL nLen, nMaxPos, cChar := '', nClose, nBase, nNext, nLine := 0
   LOCAL sRight, nPath := 0, nPaths := Len( s_asPaths ), nNewLine
   LOCAL sPath := "", cError, sPrevFile := s_sFile
   LOCAL sTmp

   IF At( '.', sSource ) == 0
     sSource += ".prg"
   ENDIF

   s_sFile := sSource

   hSource := FOpen( sSource, 0 )
   IF hSource == -1
      nPath := 1
      WHILE hSource == -1 .AND. nPath <= nPaths
          hSource := FOpen( s_asPaths[nPath] + sSource, 0 )
          nPath++
      ENDDO
   ENDIF

   IF hSource == -1
      Alert( [ERROR! opening: ]+ "[" + sSource + "]" + [ O/S Error: ] + Str( FError(), 2 ) )
      s_sFile := sPrevFile
      RETURN .F.
   ENDIF

   IF nPath > 1
      sPath := s_asPaths[ nPath - 1 ]
   ENDIF

   IF hPP == NIL
      IF bBlanks == NIL
         bBlanks := .T.
      ENDIF

      IF sPPOExt == NIL
         hPP := 0
      ELSE
         sExt := SubStr( sSource, RAt( '.', sSource ) )
         IF ! ( sExt == '' )
            hPP := FCreate( StrTran( sSource, sExt, sPPOExt ) )
         ELSE
            hPP := FCreate( sSource + sPPOExt )
         ENDIF
         IF hPP == -1
            Alert( [ERROR! creating '.pp$' file, O/S Error: ] + Str( FError(), 2 ) )
            s_sFile := sPrevFile
            RETURN .F.
         ENDIF
      ENDIF
   ELSE
      FWrite( hPP, '#line 1 "' + sPath + Upper( sSource ) + '"' + CRLF )
      bBlanks := .F.
   ENDIF

   IF bDirectivesOnly == NIL
      bDirectivesOnly := .F.
   ENDIF

   sBuffer   := Space( PP_BUFFER_SIZE )
   sLine     := ''

   IF bCompile
      ErrorBlock( {|oErr| RP_Comp_Err( oErr, sLine, nLine ) } )
   ENDIF

   BEGIN SEQUENCE

   WHILE ( nLen := FRead( hSource, @sBuffer, PP_BUFFER_SIZE ) ) > 2
      nPosition := 1
      nMaxPos   := nLen - 1

      //TraceLog( sLine, sBuffer )

      WHILE nPosition < nMaxPos

          cPrev := cChar
          cChar := SubStr( sBuffer, nPosition, 1 )

          DO CASE
             CASE ( cChar == '/' .AND. SubStr( sBuffer, nPosition + 1, 1 ) == '*' )
                nPosition++
                WHILE .T.
                   nClose := At( "*/", SubStr( sBuffer, nPosition + 1 ) )

                   IF nClose == 0
                      nBase := nPosition + 2
                      WHILE ( nNext := At( Chr(10), SubStr( sBuffer, nBase ) ) ) > 0

                         nLine++
                         IF bCount
                            @ Row(), 0 SAY nLine
                         ENDIF

                         IF bBlanks
                            FWrite( hPP, CRLF )
                         ENDIF
                         nBase += ( nNext + 1 )
                      ENDDO

                      //FSeek( hSource, -1, 1 )
                      nLen := FRead( hSource, @sBuffer, PP_BUFFER_SIZE )
                      IF nLen < 2
                         Alert( [ERROR! Unterminated '/**/' ] + "[" + Str( ProcLine() ) + "]" )
                      ENDIF
                      nMaxPos   := nLen - 1
                      nPosition := 0
                      LOOP
                   ELSE
                      nBase := nPosition + 1
                      WHILE ( nNext := At( Chr(10), SubStr( sBuffer, nBase ) ) ) > 0 .AND. ( nBase + nNext - nPosition ) <= nClose + 1
                         nLine++
                         IF bCount
                            @ Row(), 0 SAY nLine
                         ENDIF
                         IF bBlanks
                            FWrite( hPP, CRLF )
                         ENDIF
                         nBase += ( nNext + 1 )
                      ENDDO

                      nPosition += ( nClose + 1 )
                      cChar := ''
                      EXIT
                   ENDIF
                ENDDO

             CASE ( cChar == '/' .AND. SubStr( sBuffer, nPosition + 1, 1 ) == '/' )
                nPosition++
                WHILE .T.
                   nClose := At( Chr(10), SubStr( sBuffer, nPosition + 1 ) )

                   IF nClose == 0
                      //FSeek( hSource, -1, 1 )
                      nLen := FRead( hSource, @sBuffer, PP_BUFFER_SIZE )
                      IF nLen < 2
                         BREAK "//"
                      ENDIF
                      nMaxPos   := nLen - 1
                      nPosition := 0
                      LOOP
                   ELSE
                      nLine++
                      IF bCount
                         @ Row(), 0 SAY nLine
                      ENDIF

                      DropTrailingWS( @sLine, @sRight )

                      IF Right( sLine, 1 ) == ';'
                         nLen  := Len( sLine )
                         sLine := DropTrailingWS( Left( sLine, nLen - 1 ), @sRight )
                         IF bBlanks
                            FWrite( hPP, CRLF )
                         ENDIF

                         /* Right after the NL */
                         nPosition += ( nClose + 1 )

                         /* Skip leading spaces in continued next line. */
                         WHILE SubStr( sBuffer, nPosition, 1 ) $ ' ' + Chr(9)
                            nPosition++
                         ENDDO
                         nPosition--
                         //sLine += sRight
                         cChar := ' '
                         EXIT
                      ELSE
                         IF LTrim( sLine ) == ''
                            IF bBlanks
                               FWrite( hPP, CRLF )
                            ENDIF
                         ELSE
                            IF bDirectivesOnly == .F. .OR. Left( sLine, 1 ) == '#'
                               sLine := PP_PreProLine( sLine, nLine, sPath + sSource )
                               IF bBlanks .OR. ! ( sLine == '' )
                                  FWrite( hPP, sLine + CRLF )
                               ENDIF
                            ENDIF
                         ENDIF

                         nPosition += ( nClose )
                         sLine := ''
                         cChar := ''
                         EXIT
                      ENDIF
                   ENDIF
                ENDDO

             CASE ( cChar == '&' .AND. SubStr( sBuffer, nPosition + 1, 1 ) == '&' )
                nPosition++
                WHILE .T.
                   nClose := At( Chr(10), SubStr( sBuffer, nPosition + 1 ) )

                   IF nClose == 0
                      //FSeek( hSource, -1, 1 )
                      nLen := FRead( hSource, @sBuffer, PP_BUFFER_SIZE )
                      IF nLen < 2
                         BREAK "&&"
                      ENDIF
                      nMaxPos   := nLen - 1
                      nPosition := 0
                      LOOP
                   ELSE
                      nLine++
                      IF bCount
                         @ Row(), 0 SAY nLine
                      ENDIF
                      IF LTrim( sLine ) == ''
                         IF bBlanks
                            FWrite( hPP, CRLF )
                         ENDIF
                      ELSE
                         IF bDirectivesOnly == .F. .OR. Left( sLine, 1 ) == '#'
                            sLine := PP_PreProLine( sLine, nLine, sPath + sSource )
                            IF bBlanks .OR. ! ( sLine == '' )
                               FWrite( hPP, sLine + CRLF )
                            ENDIF
                         ENDIF
                      ENDIF
                      nPosition += ( nClose )
                      sLine := ''
                      cChar := ''
                      EXIT
                   ENDIF
                ENDDO

             CASE ( cChar == '*' )
                IF LTrim( sLine ) == ''
                   WHILE .T.
                      nClose := At( Chr(10), SubStr( sBuffer, nPosition + 1 ) )

                      IF nClose == 0
                         //FSeek( hSource, -1, 1 )
                         nLen := FRead( hSource, @sBuffer, PP_BUFFER_SIZE )
                         IF nLen < 2
                            BREAK "*"
                         ENDIF
                         nMaxPos   := nLen - 1
                         nPosition := 1
                         LOOP
                      ELSE
                         nLine++
                         IF bCount
                            @ Row(), 0 SAY nLine
                         ENDIF
                         IF bBlanks
                            FWrite( hPP, CRLF )
                         ENDIF
                         nPosition += ( nClose )
                         sLine := ''
                         cChar := ''
                         EXIT
                      ENDIF
                   ENDDO
                ENDIF

             CASE ( cChar == '"' )
                WHILE .T.
                   nClose := At( '"', SubStr( sBuffer, nPosition + 1 ) )
                   nNewLine := At( Chr(10), SubStr( sBuffer, nPosition + 1 ) )

                   IF nNewLine > 0 .AND. ( nClose == 0 .OR. nClose > nNewLine )
                      EXIT
                   ENDIF

                   IF nClose == 0
                      sTmp      := SubStr( sBuffer, nPosition )
                      nLen      := FRead( hSource, @sBuffer, PP_BUFFER_SIZE )
                      sBuffer   := sTmp + sBuffer
                      nMaxPos   := nLen - 1
                      nPosition := 1
                      LOOP
                   ELSE
                      sLine     += SubStr( sBuffer, nPosition, nClose )
                      nPosition += ( nClose )
                      EXIT
                   ENDIF
                ENDDO

             CASE ( cChar == "'" )
                WHILE .T.
                   nClose   := At( "'", SubStr( sBuffer, nPosition + 1 ) )
                   nNewLine := At( Chr(10), SubStr( sBuffer, nPosition + 1 ) )

                   IF nNewLine > 0 .AND. ( nClose == 0 .OR. nClose > nNewLine )
                      EXIT
                   ENDIF

                   IF nClose == 0
                      sTmp      := SubStr( sBuffer, nPosition )
                      nLen      := FRead( hSource, @sBuffer, PP_BUFFER_SIZE )
                      sBuffer   := sTmp + sBuffer
                      nMaxPos   := nLen - 1
                      nPosition := 1
                      LOOP
                   ELSE
                      sLine     += SubStr( sBuffer, nPosition, nClose )
                      nPosition += ( nClose )
                      EXIT
                   ENDIF
                ENDDO

             CASE ( cChar == '[' )
                IF LTrim( sLine ) = "#" .OR. ( IsAlpha( cPrev ) .OR. IsDigit( cPrev ) .OR. cPrev $ "])}._"  )
                   sLine += cChar
                   nPosition++
                   LOOP
                ENDIF

                WHILE .T.
                   nClose   := At( ']', SubStr( sBuffer, nPosition + 1 ) )
                   nNewLine := At( Chr(10), SubStr( sBuffer, nPosition + 1 ) )

                   IF nNewLine > 0 .AND. ( nClose == 0 .OR. nClose > nNewLine )
                      EXIT
                   ENDIF

                   IF nClose == 0
                      sTmp      := SubStr( sBuffer, nPosition )
                      nLen      := FRead( hSource, @sBuffer, PP_BUFFER_SIZE )
                      sBuffer   := sTmp + sBuffer
                      nMaxPos   := nLen - 1
                      nPosition := 1
                      LOOP
                   ELSE
                      sLine     += SubStr( sBuffer, nPosition, nClose )
                      nPosition += ( nClose )
                      EXIT
                   ENDIF
                ENDDO
                IF nClose > 0 .AND. nClose < nNewLine
                   cChar := ']'
                ENDIF

             CASE cChar == Chr(9)
                sLine += "    "
                cChar := ''

             CASE cChar == Chr(10)
                DropTrailingWS( @sLine, @sRight )

                nLine++
                IF bCount
                   @ Row(), 0 SAY nLine
                ENDIF

                IF Right( sLine, 1 ) == ';'
                   nLen  := Len( sLine )
                   sLine := DropTrailingWS( Left( sLine, nLen - 1 ), @sRight )
                   IF bBlanks
                      FWrite( hPP, CRLF )
                   ENDIF

                   /* Skip leading spaces in continued next line. */
                   nPosition++
                   WHILE SubStr( sBuffer, nPosition, 1 ) $ ' ' + Chr(9)
                      nPosition++
                   ENDDO
                   nPosition--
                   //sLine += sRight
                   cChar := ' '
                ELSE
                   IF LTrim( sLine ) == ''
                      IF bBlanks
                         FWrite( hPP, CRLF )
                      ENDIF
                   ELSE
                      //sLine += sRight
                      IF bDirectivesOnly == .F. .OR. Left( sLine, 1 ) == '#'
                         sLine := PP_PreProLine( sLine, nLine, sPath + sSource )
                         IF bBlanks .OR. ! ( sLine == '' )
                            FWrite( hPP, sLine + CRLF )
                         ENDIF
                      ENDIF
                   ENDIF
                   sLine := ''
                   cChar := ''
                ENDIF

             CASE cChar == Chr(13)
                nPosition++
                LOOP

             CASE cChar == Chr(26)
                nLine++
                IF bCount
                   @ Row(), 0 SAY nLine
                ENDIF
                IF LTrim( sLine ) == ''
                   IF bBlanks
                      FWrite( hPP, CRLF )
                   ENDIF
                ELSE
                   IF bDirectivesOnly == .F. .OR. Left( sLine, 1 ) == '#'
                      sLine := PP_PreProLine( sLine, nLine, sPath + sSource )
                      IF bBlanks .OR. ! ( sLine == '' )
                         FWrite( hPP, sLine + CRLF )
                      ENDIF
                   ENDIF
                ENDIF
                sLine := ''
                cChar := ''
          ENDCASE

          sLine += cChar
          nPosition++

      ENDDO

      FSeek( hSource, -2 + ( nPosition - nMaxPos ), 1 )

   ENDDO

   RECOVER USING cError
      IF ValType( cError ) == 'C'
         TraceLog( "No EOL after: ", cError )
         //Alert( [No EOL after: ] + cError )
      ENDIF
      nPosition := nMaxPos + 2
      sLine := ""
   END SEQUENCE

   //? '"' + SubStr( Left( sBuffer, nLen ), nPosition ) + '"', nLen, nPosition

   //? "Closing: " + sSource

   FClose( hSource )

   //? sSource, nPosition, nMaxPos, nLen, SubStr( sLine, nPosition, 40 )
   //WAIT

   sLine += SubStr( sBuffer, nPosition, Max( 0, ( nMaxPos + 2 ) - nPosition ) )
   sLine := StrTran( sLine, Chr(09), "   " )
   DropTrailingWS( @sLine )
   sLine := StrTran( sLine, Chr(10), '' )
   sLine := StrTran( sLine, Chr(13), '' )
   sLine := StrTran( sLine, Chr(26), '' )

   /*
   ? '=>"' + sLine + '"<=', Asc( Right( sLine, 1 ) ), Asc( Left( sLine, 1 ) )
   FOR Counter := 1 TO Len( RTrim( Ltrim( sLine ) ) )
      ? Asc( SubStr( sLine, Counter, 1 ) )
   NEXT
   WAIT
   */

   //? "Finished: " + sSource

   nLine++
   IF bCount
      @ Row(), 0 SAY nLine
   ENDIF
   IF LTrim( sLine ) == ''
      IF bBlanks
         FWrite( hPP, sLine )
      ENDIF
   ELSE
      IF bDirectivesOnly == .F. .OR. Left( sLine, 1 ) == '#'
         sLine := PP_PreProLine( sLine, nLine, sPath + sSource )
         IF bBlanks .OR. ! ( sLine == '' )
            FWrite( hPP, sLine + CRLF )
         ENDIF
      ENDIF
   ENDIF

   IF ProcName(1) == "MAIN"
      FClose( hPP )

      IF bCCH
         CompileToCCH( sSource )
      ENDIF
   ENDIF

   //? "Done: " + sSource
   //WAIT

   s_sFile := sPrevFile

RETURN .T.

//--------------------------------------------------------------//

FUNCTION PP_PreProLine( sLine, nLine, sSource )

   LOCAL nPendingLines := 0, aPendingLines  := {}

   LOCAL sDirective, bX, sToken, nRule
   LOCAL nNewLineAt, nLines, Counter
   LOCAL sLeft, sPassed, asOutLines := {}, sOut := '', cChar
   LOCAL nLen, nCycles := 0, aDefined := {}, aTranslated := {}, aCommanded := {}
   LOCAL nPosition
   //LOCAL nIdAt, sRight
   LOCAL sError
   LOCAL sBackupLine
   LOCAL sSkipped

   IF Left( LTrim( sLine ), 1 ) != '#'
       nPosition := 0
       WHILE ( nNewLineAt := nAtSkipStr( ';', sLine ) ) > 0
          nPendingLines++
          aSize( aPendingLines, nPendingLines )

          nPosition++
          aIns( aPendingLines, nPosition )
          aPendingLines[ nPosition ] := Left( sLine, nNewLineAt - 1 )

          //? "Pending #", nPendingLines,  Left( sLine, nNewLineAt - 1 ), aPendingLines[nPendingLines]
          sLine := LTrim( SubStr( sLine, nNewLineAt + 1 ) )
       ENDDO

       IF nPosition > 0
          IF ! Empty( sLine )
              nPendingLines++
              aSize( aPendingLines, nPendingLines )

              nPosition++
              aIns( aPendingLines, nPosition )
              aPendingLines[ nPosition ] := sLine
          ENDIF

          //? "Pending #", nPendingLines, sLine, aPendingLines[nPendingLines]
          sLine := aPendingLines[1]
          aDel( aPendingLines, 1 )
          nPendingLines--
       ENDIF
   ENDIF

   WHILE .T.
      //? "Processing: '" + sLine + "'"
      //? nPendingLines, nIfDef, IIF( nIfDef > 0, abIfDef[nIfDef] , )
      //WAIT

      IF nCycles < MAX_CICLES
         nCycles++
      ELSE
         Alert( [ERROR! Circularity detected ]+"[" + sSource + "(" + LTrim( Str( nLine ) ) + ")]" )
         ? sLine
         BREAK
      ENDIF

      IF sLine == NIL
         sLine := ''
         sLeft := ''
      ELSE
         sLeft := ExtractLeadingWS( @sLine )
      ENDIF

      IF sLine == ''
         IF nPendingLines > 0
            sLine := aPendingLines[1]
            aDel( aPendingLines, 1 )
            nPendingLines--

            LOOP
         ENDIF

         EXIT
      ENDIF

      IF s_sIncludeFile != NIL
         IF ! Empty( sSource )
            aAdd( asOutLines, "#line " + LTrim( Str( nLine ) ) + ' "' + Upper( sSource ) + '"' )
         ENDIF
         s_sIncludeFile := NIL
      ENDIF

      //? "Processing: '" + sLine +"'"
      //WAIT

      IF Left( sLine, 1 ) == '#'

         sLine := LTrim( SubStr( sLine, 2 ) )
         sDirective := RTrim( Upper( NextToken( @sLine ) ) )

         IF ( nLen := Len( sDirective ) ) < 4
            Alert( [ERROR! Unknown directive: ] + "'" + sDirective + "' " + sSource )
            sLine := ''
            LOOP
         ENDIF

         IF sDirective == Left( "IFDEF", nLen ) .AND. nIfDef > 0 .AND. ! abIfDef[ nIfDef ]

            nIfDef++
            aSize( abIfDef, nIfDef )
            abIfDef[ nIfDef ] := .F.
            sLine := ''
            LOOP

         ELSEIF sDirective == Left( "IFNDEF", nLen ) .AND. nIfDef > 0 .AND. ! abIfDef[ nIfDef ]

            nIfDef++
            aSize( abIfDef, nIfDef )
            abIfDef[ nIfDef ] := .F.
            sLine := ''
            LOOP

         ELSEIF sDirective == "ELSE" .AND. nIfDef > 1 .AND. ! abIfDef[ nIfDef - 1 ]

            sLine := ''
            LOOP

         ELSEIF sDirective == "ELSE"

            abIfDef[ nIfDef ] := ! abIfDef[ nIfDef ]
            sLine := ''
            LOOP

         ELSEIF sDirective == Left( "ENDIF", nLen )

            IF nIfDef > 0
               nIfDef--
            ELSE
               Alert( [ERROR! #endif with no #ifdef in sight] )
            ENDIF

            sLine := ''
            LOOP

         ENDIF

         IF nIfDef > 0 .AND. ! abIfDef[nIfDef]
            //? "Ignored: " + sLine
            sLine := ''
            LOOP
         ENDIF

         ExtractLeadingWS( @sLine )

         IF sDirective == Left( "DEFINE", nLen )

            CompileDefine( sLine )
            sLine := ''
            LOOP

         ELSEIF sDirective == Left( "ERROR", nLen )

            Alert( "#error " + sLine )
            sLine := ''
            LOOP

         ELSEIF sDirective == Left( "UNDEF", nLen )

            RemoveDefine( sLine )
            sLine := ''
            LOOP

         ELSEIF sDirective == Left( "IFDEF", nLen )

            SetIfDef( sLine, .T. )
            sLine := ''
            LOOP

         ELSEIF sDirective == Left( "IFNDEF", nLen )

            SetIfDef( sLine, .F. )
            sLine := ''
            LOOP

         ELSEIF sDirective == Left( "INCLUDE", nLen )


            ExtractLeadingWS( @sLine )
            DropTrailingWS( @sLine )

            // Strip the ""
            sLine := SubStr( sLine, 2, Len( sLine ) - 2 )

            IF Upper( sLine ) = "HBCLASS"
               IF ! s_lClsLoaded
                  s_lClsLoaded := .T.
                  InitClsRules()
                  InitClsResults()
                  IF Len( aDefRules ) != Len( aDefResults )
                     Alert( [Class #DEFINE Rules size mismatch] )
                  ENDIF
                  IF Len( aTransRules ) != Len( aTransResults )
                     Alert( [Class #TRANSLATE Rules size mismatch] )
                  ENDIF
                  IF Len( aCommRules ) != Len( aCommResults )
                     Alert( [Class #DEFINE Rules size mismatch] )
                  ENDIF
               ENDIF
            ELSEIF Upper( sLine ) = "FIVEWIN"
               IF ! s_lFWLoaded
                  s_lFWLoaded := .T.
                  IF ! s_lClsLoaded
                     s_lClsLoaded := .T.
                     InitClsRules()
                     InitClsResults()
                     IF Len( aDefRules ) != Len( aDefResults )
                        Alert( [Class #DEFINE Rules size mismatch] )
                     ENDIF
                     IF Len( aTransRules ) != Len( aTransResults )
                        Alert( [Class #TRANSLATE Rules size mismatch] )
                     ENDIF
                     IF Len( aCommRules ) != Len( aCommResults )
                        Alert( [Class #DEFINE Rules size mismatch] )
                     ENDIF
                  ENDIF
                  InitFWRules()
                  InitFWResults()
                  IF Len( aDefRules ) != Len( aDefResults )
                     Alert( [FW #DEFINE Rules size mismatch] )
                  ENDIF
                  IF Len( aTransRules ) != Len( aTransResults )
                     Alert( [FW #TRANSLATE Rules size mismatch] )
                  ENDIF
                  IF Len( aCommRules ) != Len( aCommResults )
                     Alert( [FW #DEFINE Rules size mismatch] )
                  ENDIF
               ENDIF
            ELSE
               PP_PreProFile( sLine ) // Intentionally not using s_sIncludeFile

               /* Recursion safety - don't use the Static might be modified. */
               s_sIncludeFile := sLine
            ENDIF

            sLine := ''
            LOOP

         ELSE

            IF Left( sDirective, 1 ) == 'X'
               bX := .T.
               sDirective := SubStr( sDirective, 2 )
               nLen--
            ELSE
               bX := .F.
            ENDIF

            IF sDirective == Left( 'TRANSLATE', nLen )

               CompileRule( sLine, aTransRules, aTransResults, bX, .T. )
               sLine := ''
               LOOP

            ELSEIF sDirective == Left( 'COMMAND', nLen )

               CompileRule( sLine, aCommRules, aCommResults, bX, .T. )
               sLine := ''
               LOOP

            ELSE

               Alert( [ERROR! Unknown directive: ] + "'" + sDirective + "' " + sSource )
               sLine := ''
               LOOP

            ENDIF

         ENDIF

      ENDIF

      #ifdef PP_RECURSIVE
         s_bRecursive := .T.
      #endif

    BEGIN SEQUENCE

      IF nIfDef > 0 .AND. ! abIfDef[nIfDef]
         //? "Ignored: " + sLine
         sLine := ''
         BREAK
      ENDIF

      //TraceLog( sLine )

      sBackupLine := sLine
      sPassed     := ""
      DO WHILE ( sToken := NextIdentifier( @sLine, @sSkipped ) ) != NIL
         //? "Token = '"  + sToken + "'"
         //WAIT

         sPassed += sSkipped

         IF ( nRule := MatchRule( sToken, @sLine, aDefRules, aDefResults, .F., .F. ) ) > 0
            //? "DEFINED: " + sLine
            //WAIT

            aAdd( aDefined, nRule )

            nPosition := 0
            WHILE ( nNewLineAt := nAtSkipStr( ';', sLine ) ) > 0
               nPendingLines++
               IF nPendingLines > Len( aPendingLines )
                  aSize( aPendingLines, nPendingLines )
               ENDIF

               nPosition++
               aIns( aPendingLines, nPosition )
               aPendingLines[ nPosition ] := Left( sLine, nNewLineAt - 1 )

               //? "Pending #", nPendingLines,  Left( sLine, nNewLineAt - 1 ), aPendingLines[nPendingLines]
               sLine := LTrim( SubStr( sLine, nNewLineAt + 1 ) )
            ENDDO

            IF nPosition == 0
               sLine := sLeft + sPassed + sLine
            ELSE
               IF ! Empty( sLine )
                   nPendingLines++
                   IF nPendingLines > Len( aPendingLines )
                      aSize( aPendingLines, nPendingLines )
                   ENDIF

                   nPosition++
                   aIns( aPendingLines, nPosition )
                   aPendingLines[ nPosition ] := sLine
               ENDIF

               //? "Pending #", nPendingLines, sLine, aPendingLines[nPendingLines]
               sLine := sLeft + sPassed + aPendingLines[1]
               aDel( aPendingLines, 1 )
               nPendingLines--
            ENDIF

            // Re-Reprocess the line ...
            BREAK
         ENDIF

         sPassed += sToken
      ENDDO

      // Now process Translates...
      //? "After Defines:", sLine

      sLine := sBackupLine

      sPassed := ""
      DO WHILE ( sToken := NextToken( @sLine ) ) != NIL
         //? "Token = '"  + sToken + "'"
         //WAIT
         IF ( nRule := MatchRule( sToken, @sLine, aTransRules, aTransResults, .F., .T. ) ) > 0
            //? "TRANSLATED: " + sLine
            //WAIT

            IF sPassed == "" .AND. aScan( aTranslated, nRule ) > 0
               BREAK( "Cyclic directive: #translate " + sToken )
            ELSE
               aAdd( aTranslated, nRule )
            ENDIF


            nPosition := 0
            WHILE ( nNewLineAt := nAtSkipStr( ';', sLine ) ) > 0
               nPendingLines++
               IF nPendingLines > Len( aPendingLines )
                  aSize( aPendingLines, nPendingLines )
               ENDIF

               nPosition++
               aIns( aPendingLines, nPosition )
               aPendingLines[ nPosition ] := Left( sLine, nNewLineAt - 1 )

               //? "Pending #", nPendingLines,  Left( sLine, nNewLineAt - 1 ), aPendingLines[nPendingLines]
               sLine := SubStr( sLine, nNewLineAt + 1 )
            ENDDO

            IF nPosition == 0
               sLine := sLeft + sPassed + sLine
            ELSE
               IF ! Empty( sLine )
                  nPendingLines++
                  IF nPendingLines > Len( aPendingLines )
                     aSize( aPendingLines, nPendingLines )
                  ENDIF

                  nPosition++
                  aIns( aPendingLines, nPosition )
                  aPendingLines[ nPosition ] := sLine
               ENDIF
               //? "Pending #", nPendingLines, sLine, aPendingLines[nPendingLines]
               sLine := sLeft + sPassed + aPendingLines[1]
               aDel( aPendingLines, 1 )
               nPendingLines--
            ENDIF

            BREAK
         ENDIF

         sPassed += sToken
      ENDDO

      sLine := sPassed //sBackupLine

      sToken := NextToken( @sLine )

      IF sToken != NIL .AND. ( nRule := MatchRule( sToken, @sLine, aCommRules, aCommResults, .T., .T. ) ) > 0
         //? "COMMANDED: " + sLine
         //? '"' + sLeft +'"', '"' + sPassed + '"'
         //WAIT

         /*
         IF aScan( aCommanded, nRule ) > 0
            Alert( [Cyclic directive: #command ] + sToken )
            BREAK
         ELSE
            aAdd( aCommanded, nRule )
         ENDIF
         */

         nPosition := 0
         WHILE ( nNewLineAt := nAtSkipStr( ';', sLine ) ) > 0
            nPendingLines++
            IF nPendingLines > Len( aPendingLines )
               aSize( aPendingLines, nPendingLines )
            ENDIF

            nPosition++
            aIns( aPendingLines, nPosition )
            aPendingLines[ nPosition ] := Left( sLine, nNewLineAt - 1 )

            //? "Pending #", nPendingLines,  Left( sLine, nNewLineAt - 1 ), aPendingLines[nPosition]
            sLine := LTrim( SubStr( sLine, nNewLineAt + 1 ) )
         ENDDO

         IF nPosition == 0
            sLine := sLeft + sLine
         ELSE
            IF ! Empty( sLine )
               nPendingLines++

               IF nPendingLines > Len( aPendingLines )
                  aSize( aPendingLines, nPendingLines )
               ENDIF

               nPosition++
               aIns( aPendingLines, nPosition )
               aPendingLines[ nPosition ] := sLine
            ENDIF

            //? "Pending #", nPendingLines,  sLine, aPendingLines[nPosition]
            sLine := sLeft + aPendingLines[1]
            aDel( aPendingLines, 1 )
            nPendingLines--
         ENDIF

         BREAK
      ENDIF

      aAdd( asOutLines, sLeft + sPassed )
      sLine := ""

    RECOVER USING sError

      IF sError != NIL
         Alert( sError )
         BREAK
      ENDIF

      LOOP

    END SEQUENCE

   ENDDO

   #ifdef PP_RECURSIVE
      s_bRecursive := .F.
   #endif

   sOut   := ""
   nLines := Len( asOutLines )

   //? nLines
   //WAIT

   FOR Counter := 1 TO nLines
      //? Counter, asOutLines[Counter]
      //WAIT
       sOut += asOutLines[Counter]
       IF Counter < nLines
          sOut += ' ;'
       ENDIF
   NEXT

   IF ! Empty( sOut )
      //? "Returning: " + sOut
      //WAIT
   ENDIF

   IF bCompile
      PP_CompileLine( sOut, nLine, s_aProcedures, s_aInitExit, @s_nProcId )
   ENDIF

RETURN sOut

//--------------------------------------------------------------//

STATIC FUNCTION MatchRule( sKey, sLine, aRules, aResults, bStatement, bUpper )

   LOCAL Counter, nRules, nRule, aMarkers, xMarker
   LOCAL aMP, nOptional := 0, sAnchor, cType, aList, nMarkerId, nKeyLen
   LOCAL sToken, sWorkLine, sNextAnchor, nMatch, nMatches
   LOCAL sPad, asRevert := {}, bNext, sPreMatch, nLen
   LOCAL sPrimaryStopper, sPreStoppers, sStopper, sMultiStopper, nStopper, nStoppers
   LOCAL nSpaceAt, sStopLine, sNextStopper, nTemp
   LOCAL bRepeatableMatched
   LOCAL aaRevertMarkers := {}

   nRules   := Len( aRules )

   IF nRules == 0 .OR. sKey == NIL .OR. sKey == ""
      RETURN 0
   ENDIF

   nRule    := nRules + 1
   sPad     := ''

   DropTrailingWS( @sKey, @sPad )
   IF bUpper
      sKey  := Upper( sKey )
   ENDIF

   IF bDbgMatch
      ? "Matching Key: '" + sKey + "' Line: " + sLine
      WAIT
   ENDIF

   nKeyLen := Max( Len( sKey ), 4 )

   WHILE .T.

      nRule--

      FOR Counter := nRule TO 1 STEP -1
         IF aRules[Counter][3]
            IF aRules[ Counter ][1] == sKey
               EXIT
            ENDIF
         ELSE
            IF Left( aRules[ Counter ][1], nKeyLen ) == sKey
               EXIT
            ENDIF
         ENDIF
      NEXT

      IF Counter == 0
         IF bDbgMatch
            ? "No Prospects, returning: " + sLine
            WAIT
         ENDIF

         RETURN 0
      ELSE
         nRule := Counter
      ENDIF

      sWorkLine := sLine

      IF bDbgMatch
         ? "KEY: " + sKey + " Matching Rule: " + Str( nRule, 3 ) + " with: " + sWorkLine
         WAIT
      ENDIF

      IF aRules[nRule][2] == NIL
         nMatches := 0
      ELSE
         nMatches := Len( aRules[nRule][2] )
      ENDIF

      IF nMatches == 0
         IF bStatement .AND. ! Empty( sWorkLine )
            IF bDbgMatch
               ? "***1 Unmatched remainder: >", sWorkLine, "<"
               ? "Statement failed"
               WAIT
            ENDIF

            LOOP
         ELSEIF bStatement
            sWorkLine := ""
         ENDIF

         sLine := ( PPOut( aResults[nRule], aMarkers ) + sPad + sWorkLine )
         IF bDbgMatch
            ? "TRANSLATED to:", sLine
            WAIT
         ENDIF
         RETURN nRule
      ENDIF

      aMarkers  := aResults[nRule][3]
      IF aMarkers != NIL
         aFill( aMarkers, NIL )
      ENDIF

      nMatch    := 1
      aMP       := aRules[nRule][2][1]
      nOptional := 0
      bNext     := .F.

      DO WHILE .T. //! ( sWorkLine == '' )

         aMP       := aRules[nRule][2][nMatch]

         nMarkerId := aMP[1]
         sAnchor   := aMP[3]
         cType     := aMP[4]
         aList     := aMP[5]

         /* Might be needed - added 5-27-2001 when debugging oddity in FW CheckBox rule ???
         IF aMP[2] == 0
            nOptional := 0
            aSize( asRevert, 0 )
         ENDIF
         */

         /* "Used" non repeatable! */
         IF nMarkerID > 0 .AND. nMarkerID < 1000
            IF aMarkers != NIL .AND. aMarkers[nMarkerID] != NIL
               IF bDbgMatch
                  ? "Used:", nMatch, nMarkerId, aMarkers[nMarkerId], nOptional, aMP[2]
                  WAIT
               ENDIF

               IF nOptional <> 0 .AND. aMP[2] < 0
                  sWorkLine := asRevert[Abs(nOptional)]
                  aMarkers  := aaRevertMarkers[Abs(nOptional)]

                  IF bDbgMatch
                     ? "* Reverted: " + asRevert[Abs(nOptional)]
                     WAIT
                  ENDIF
               ENDIF

               IF aMP[2] > 0 .AND. nMatch < nMatches
                  /* Skip all same level optionals to next group. */
                  nOptional := aMP[2]
                  nMatch++
                  WHILE nMatch <= nMatches
                     aMP := aRules[nRule][2][nMatch]
                     IF ( aMP[2] >= 0 ) .AND. ( aMP[2] <= Abs( nOptional ) )
                        EXIT
                     ENDIF
                     nMatch++
                  ENDDO
                  IF bDbgMatch
                     ? "Skipped to", nMatch, "of", nMatches, aMP[2], aMP[3], nOptional
                  ENDIF
                  IF nMatch <= nMatches
                     LOOP
                  ELSE
                     EXIT
                  ENDIF
               ELSE
                  IF nMatch < nMatches
                     nMatch++
                     LOOP
                  ELSE
                     EXIT
                  ENDIF
               ENDIF
            ENDIF
         ELSEIF nMarkerId >= 1000
            nMarkerId -= 1000
         ENDIF

         /* Do we have to look for a stopper? */
         IF cType != ':' .AND. sAnchor == NIL .AND. ValType( aList ) == 'A'

            sPreStoppers := sWorkLine
            sPrimaryStopper := NextToken( @sWorkLine )

            IF sPrimaryStopper == NIL

               //? "No primary", sPrimaryStopper
               sWorkLine := sPreStoppers

            ELSE
               sPrimaryStopper := Upper( RTrim( sPrimaryStopper ) )

               /* Is it a stopper (the anchor of another acceptable match) ? */
               IF bDbgMatch
                  ? "Stopper?: '" + sPrimaryStopper +"'"
               ENDIF

               nStoppers := Len( aList )
               FOR nStopper := 1 TO nStoppers

                  sStopLine := sWorkLine
                  sToken    := sPrimaryStopper
                  sStopper  := aList[ nStopper ]

                  sMultiStopper := ''
                  WHILE ( nSpaceAt := At( ' ', sStopper ) ) > 0
                     sNextStopper := Left( sStopper, nSpaceAt - 1 )

                     IF aRules[nRule][3]
                        nLen := 64
                     ELSE
                        nLen := Max( 4, Len( sToken ) )
                     ENDIF

                     //? "Next Stopper: " + sNextStopper, sToken
                     IF Left( sNextStopper, nLen ) == sToken
                        sMultiStopper += sNextStopper
                        sStopper      := SubStr( sStopper, nSpaceAt )
                        sMultiStopper += ExtractLeadingWS( @sStopper )
                        sToken        := NextToken( @sStopLine )
                        sToken        := Upper( RTrim( sToken ) )
                     ELSE
                        EXIT
                     ENDIF
                  ENDDO

                  IF aRules[nRule][3]
                     nLen := 64
                  ELSE
                     nLen := Max( 4, Len( sToken ) )
                  ENDIF

                  IF Left( sStopper, nLen ) == sToken
                     sMultiStopper += sStopper
                     EXIT
                  ENDIF
               NEXT

               IF nStopper <= nStoppers

                  IF bDbgMatch
                     ? "Found stopper: " + sMultiStopper
                  ENDIF

                  sWorkLine := sStopLine

                  /* Current level */
                  nOptional := aMP[2]
                  IF nOptional < 0
                     nOptional := Abs( nOptional )
                  ENDIF

                  /* Commented out 07-21-2001 Seems unneeded. */
                  #ifdef WHY_REWIND
                      /* Rewind to beging of same level and then search for the stopper match */
                      WHILE nMatch > 1
                         nMatch--
                         IF aRules[nRule][2][nMatch][2] >= 0 .AND. aRules[nRule][2][nMatch][2] < nOPtional
                            EXIT
                         ENDIF
                      ENDDO
                      IF aRules[nRule][2][nMatch][2] >= 0 .AND. aRules[nRule][2][nMatch][2] < nOPtional
                         nMatch++
                      ENDIF
                  #endif

                  /* Now search for the stopper. */
                  WHILE nMatch < nMatches
                     nMatch++
                     aMP := aRules[nRule][2][nMatch]

                     IF aMP[3] == NIL .AND. aMP[4] == ':'
                        IF aScan( aMP[5], {|sWord| sWord == sMultiStopper } ) > 0
                           EXIT
                        ENDIF
                     ELSE
                        IF aMP[2] >= 0 .AND. aMP[2] <= nOptional .AND. aMP[3] == sMultiStopper
                           EXIT
                        ENDIF
                     ENDIF
                  ENDDO

                  nOptional := 0
                  LOOP

               ELSE

                  sWorkLine     := sPreStoppers
                  sMultiStopper := NIL

                  IF bDbgMatch
                     ? sToken, "Not a stopper."
                     ? "Reverted: ", sWorkLine
                  ENDIF

               ENDIF

               IF bDbgMatch
                  WAIT
               ENDIF

            ENDIF

         ENDIF

         sNextAnchor := NIL
         nTemp       := 1
         WHILE nMatch + nTemp <= nMatches
            IF aRules[Counter][2][nMatch + nTemp][2] <= 0 // Non NEW Optional ONLY!
               sNextAnchor := aRules[Counter][2][nMatch + nTemp][3]
               EXIT
            ENDIF
            nTemp++
         ENDDO

         IF bDbgMatch
            IF sAnchor == NIL
               ? nMatch, 'of', nMatches, "NO Anchore!", nMarkerId, nOptional, aMP[2], sMultiStopper, sNextAnchor
            ELSE
               ? nMatch, 'of', nMatches, "Searching for Anchore: '" + sAnchor + "'", nMarkerId, nOptional, aMP[2], sMultiStopper, sNextAnchor
            ENDIF
            WAIT
         ENDIF

         sToken    := NIL
         xMarker   := NIL
         sPreMatch := sWorkLine

         IF ( sAnchor == NIL .OR. sMultiStopper != NIL .OR. ;
              ( ( ( sToken := NextToken( @sWorkLine ) ) != NIL  .AND. ( DropTrailingWS( @sToken, @sPad ), nLen := Max( 4, Len( sToken ) ), Upper( sToken ) == Left( sAnchor, nLen ) ) ) ) ) ;
            .AND. ( nMarkerId == 0 .OR. ( sAnchor == NIL .AND. sMultiStopper != NIL ) .OR. ( ( xMarker := NextExp( @sWorkLine, cType, aList, sNextAnchor, aRules[nRule][3] ) ) != NIL ) )

            IF sMultiStopper != NIL
               IF sAnchor == NIL
                  xMarker := sMultiStopper
               ELSE
                  sToken  := sMultiStopper
               ENDIF
               IF bDbgMatch
                  ? "Using MultiStopper: " + sMultiStopper
               ENDIF
               sMultiStopper := NIL
            ENDIF

            IF bDbgMatch
               ? "sKey =", sKey, "Anchor =", sAnchor, "nMarkerId =", nMarkerId, "sToken =", sToken, "xMarker =", xMarker, "<="
            ENDIF

            IF ValType( xMarker ) == 'C'
               DropTrailingWS( @xMarker )
            ENDIF

            IF aMP[2] > 0 .AND. nOptional < 0

               nOptional := aMP[2]

               /* Save. */
               aSize( asRevert, nOptional )
               asRevert[nOptional] := sPreMatch
               aSize( aaRevertMarkers, nOptional )
               aaRevertMarkers[nOptional] := aClone( aMarkers )

               IF bDbgMatch
                  ? "*** Saved: " + asRevert[nOptional]
                  WAIT
               ENDIF

            ELSEIF aMP[2] > 0 .AND. nOptional >= 0 .AND. aMP[2] >= nOptional

               nOptional := aMP[2]

               /* Save. */
               aSize( asRevert, nOptional )
               asRevert[nOptional] := sPreMatch
               aSize( aaRevertMarkers, nOptional )
               aaRevertMarkers[nOptional] := aClone( aMarkers )

               IF bDbgMatch
                  ? "*** Saved: " + asRevert[nOptional]
                  WAIT
               ENDIF

            // Group started with nested optional, this is the 1st element in current level.
            ELSEIF aMP[2] < 0 .AND. Len( asRevert ) >= - aMP[2] .AND. asRevert[ - aMP[2] ] == NIL

               nOptional := - aMP[2]

               /* Save. */
               //aSize( asRevert, nOptional )
               asRevert[nOptional] := sPreMatch
               //aSize( aaRevertMarkers, nOptional )
               aaRevertMarkers[nOptional] := aClone( aMarkers )

               IF bDbgMatch
                  ? "*** Saved: " + asRevert[nOptional]
                  WAIT
               ENDIF

            ENDIF

            IF nMarkerId > 0
               /* Repeatable. */
               IF aMP[1] > 1000
                  IF aMarkers[nMarkerId] == NIL
                     aMarkers[nMarkerId] := {}
                  ENDIF
                  aAdd( aMarkers[nMarkerId], xMarker )

                  IF bDbgMatch
                     ? nMarkerId, "Repetable added: ", xMarker, Len( aMarkers[nMarkerId] )
                  ENDIF
               ELSE
                  IF ValType( aMarkers ) != 'A' .OR. nMarkerId > Len( aMarkers )
                     TraceLog( "Oops", nRule, sKey, nMarkerId, ValType( aMarkers ), IIF( ValType( aMarkers ) == 'A', Len( aMarkers ) , "No array" ) )
                     Alert( [Unexpected case ] + "[" + Str( Procline() ) + "]" )
                  ELSE
                     aMarkers[nMarkerId] := xMarker
                  ENDIF
               ENDIF
            ENDIF

            IF aMP[2] <> 0
               IF bDbgMatch
                  ? "Optional"
               ENDIF

               /* We reached the end of current optional group - Rewind, to 1st optional at same level. */
               IF nMatch == nMatches .OR. ( aRules[nRule][2][nMatch + 1][2] >= 0 .AND. aRules[nRule][2][nMatch + 1][2] <= Abs( aMP[2] ) ) .OR. ;
                                          ( aRules[nRule][2][nMatch + 1][2] < 0 .AND. abs( aRules[nRule][2][nMatch + 1][2] ) < Abs( aMP[2] ) )

                  // EOL - Rule will match if rest is OPTIONAL.
                  IF Empty( sWorkLine )
                     IF bDbgMatch
                        ? "EOL exiting."
                     ENDIF

                     IF ++nMatch <= nMatches
                        aMP := aRules[nRule][2][nMatch]
                        sPreMatch := ""
                     ENDIF

                     EXIT
                  ENDIF

                  /* Current level */
                  nOptional := aMP[2]
                  IF Len( asRevert ) >= Abs( nOptional )
                     asRevert[ Abs( nOptional ) ] := NIL
                  ENDIF

                  IF nOptional < 0
                     nOptional := Abs( nOptional )
                  ENDIF

                  WHILE nMatch > 1
                     nMatch--
                     IF /*aRules[nRule][2][nMatch][2] >= 0 .AND.*/ Abs( aRules[nRule][2][nMatch][2] ) < nOPtional
                        EXIT
                     ENDIF
                  ENDDO
                  IF nMatch == 0 .OR. ( /*aRules[nRule][2][nMatch][2] >= 0 .AND.*/ Abs( aRules[nRule][2][nMatch][2] ) < nOPtional )
                     nMatch++
                  ENDIF

                  nOptional := 0

                  IF bDbgMatch
                     ? "Rewinded to:", nMatch
                  ENDIF

                  LOOP

               ENDIF

            ENDIF

            IF bDbgMatch
               WAIT
            ENDIF

            IF nMatch == nMatches
               IF bStatement .AND. ! Empty( sWorkLine )
                  bNext := .T.

                  IF bDbgMatch
                     ? "Key: >", sKey, "< ***2 Unmatched remainder: >", sWorkLine, "<"
                     ? "Statement failed, try next rule...'"
                     WAIT
                  ENDIF

                  sWorkLine := ""
                  EXIT
               ELSEIF bStatement
                  sWorkLine := ""
               ENDIF

               sLine := ( PPOut( aResults[nRule], aMarkers ) + sPad + sWorkLine )
               IF bDbgMatch
                  ? "TRANSLATED to:", sLine
                  WAIT
               ENDIF
               RETURN nRule
            ELSE
               IF bDbgMatch
                  ? "Accepted:", sToken, xMarker
               ENDIF

               nMatch++
               LOOP
            ENDIF

         ELSE /* Match failed. */

            IF bDbgMatch
               ? "NO MATCH:", nMatch, "of", nMatches, sAnchor, sToken, nMarkerId, xMarker, nOptional, aMP[2]
            ENDIF

            IF aMP[2] <> 0
               /* Revert. */
               IF nOptional <> 0 .AND. aMP[2] < 0 .AND. asRevert[Abs(nOptional)] != NIL
                  sWorkLine := asRevert[Abs(nOptional)]
                  aMarkers  := aaRevertMarkers[Abs(nOptional)]

                  IF bDbgMatch
                     ? "* Reverted: " + asRevert[Abs(nOptional)]
                     WAIT
                  ENDIF
               ELSE
                  sWorkLine := sPreMatch
                  IF bDbgMatch
                     ? "*** Reclaimed token/marker: " + sWorkLine
                     WAIT
                  ENDIF

                  /* Commented out 07-21-2001 - Seems wrong !
                  IF aMP[1] > 1000 .AND. xMarker != NIL
                     IF bDbgMatch
                        ? "Removed repeatble: " + aTail( aMarkers[ aMP[1] - 1000 ] )
                        WAIT
                     ENDIF
                     aSize( aMarkers[ aMP[1] - 1000 ], Len( aMarkers[ aMP[1] - 1000 ] ) - 1 )
                  ENDIF
                  */
               ENDIF

               /* Optional (last) didn't match - Rule can still match. */
               IF nMatch == nMatches
                  IF bStatement .AND. ! Empty( sWorkLine )
                     bNext := .T.

                     IF bDbgMatch
                        ? "***3 Unmatched remainder: >", sWorkLine, "<"
                        ? "Statement failed, try next rule..."
                        WAIT
                     ENDIF

                     EXIT
                  ELSEIF bStatement
                     sWorkLine := ""
                  ENDIF

                  sLine := ( PPOut( aResults[nRule], aMarkers ) + sWorkLine )
                  IF bDbgMatch
                     ? "TRANSLATED to:", sLine
                     WAIT
                  ENDIF
                  RETURN nRule
               ELSE
                  /* Nested optional, maybe last in its group. */
                  IF aMP[2] > 1
                     nTemp := aMP[2]
                     /* Skip dependents and nested optionals, if any. */
                     nMatch++
                     WHILE nMatch < nMatches .AND. ( ( aRules[nRule][2][nMatch][2] < 0 .AND. Abs( aRules[nRule][2][nMatch][2] ) == nTemp ).OR. aRules[nRule][2][nMatch][2] > nTemp )
                        nMatch++
                     ENDDO
                     nMatch--

                     IF aRules[nRule][2][nMatch + 1][2] > 0 .AND. aRules[nRule][2][nMatch + 1][2] < nTemp
                        /* Upper level optional should be accepted - rewind to top of parent group. */
                        nOptional--

                        WHILE nMatch > 1
                           nMatch--
                           IF aRules[nRule][2][nMatch][2] == 0 .OR. ( aRules[nRule][2][nMatch][2] > 0 .AND. aRules[nRule][2][nMatch][2] < nOPtional )
                              EXIT
                           ENDIF
                        ENDDO
                        IF nMatch == 0 .OR. ( aRules[nRule][2][nMatch][2] == 0 .OR. ( aRules[nRule][2][nMatch][2] > 0 .AND. aRules[nRule][2][nMatch][2] < nOptional ) )
                           nMatch++
                        ENDIF

                        nOptional := 0

                        IF bDbgMatch
                           ? "Nested last optional, Rewinded to:", nMatch
                        ENDIF

                        LOOP
                     ENDIF

                  ENDIF

                  /* Skip all same level optionals to next group. */
                  nOptional          := aMP[2]
                  bRepeatableMatched := aMP[1] > 1000 .AND. aMarkers[ aMP[1] - 1000 ] != NIL //.AND. Len( aMarkers[ aMP[1] - 1000 ] ) > 0
                  WHILE nMatch < nMatches
                     nMatch++
                     aMP := aRules[nRule][2][nMatch]
                     IF ( aMP[2] < 0 ) .AND. ( Abs( aMP[2] ) < Abs( nOptional ) )
                        EXIT
                     ENDIF
                     IF ( aMP[2] >= 0 ) .AND. ( aMP[2] <= Abs( nOptional ) )
                        EXIT
                     ENDIF
                  ENDDO

                  // We should NOT consider this a failure, continue matching...
                  IF bRepeatableMatched
                     IF bDbgMatch
                        ? "Repeatable Matched - Skipped to", nMatch, "of", nMatches, aMP[2], aMP[3], nOptional
                     ENDIF

                     nOptional := aMP[2]
                     LOOP
                  ELSE
                     IF bDbgMatch
                        ? "Partial not allowed - Skipped to", nMatch, "of", nMatches, aMP[2], aMP[3], nOptional
                     ENDIF
                  ENDIF

                  IF nMatch == nMatches

                     IF ( aMP[2] >= 0 ) .AND. ( aMP[2] <= Abs( nOptional ) )
                        /* Ok. */
                     ELSEIF ( aMP[2] < 0 ) .AND. ( Abs( aMP[2] ) < Abs( nOptional ) ) // Added 07-21-2001 ???
                        /* Ok. */
                     ELSE
                        IF bDbgMatch
                           ? "Reached End of Rule"
                        ENDIF
                        EXIT
                     ENDIF
                  ENDIF

                  IF bDbgMatch
                     ? "Skipped to", nMatch, "of", nMatches, aMP[2], aMP[3], nOptional
                  ENDIF

                  nOptional := aMP[2]
                  LOOP
               ENDIF
            ELSE
               IF bDbgMatch
                  ? "Match failed, try next rule..."
                  WAIT
               ENDIF

               bNext := .T.
               EXIT
            ENDIF
         ENDIF

      ENDDO

      IF bNext
         IF bDbgMatch
            ? "NEXT Rule requested."
         ENDIF

         LOOP
      ELSE
         IF bStatement .AND. ! Empty( sWorkLine )
            IF bDbgMatch
               ? "***4 Unmatched remainder: >", sWorkLine, "<"
               ? "Statement failed, try next rule..."
               WAIT
            ENDIF

            LOOP
         ENDIF
      ENDIF

      IF bDbgMatch
         ? "EOL."
         WAIT
      ENDIF

      IF nMatch < nMatches

         IF bDbgMatch
            ? nMatch, "of:", nMatches, "Checking if Rule remainder is optional."
         ENDIF

         /* Current and remainder of MP NOT optional. */
         IF aMP[2] == 0
            IF bDbgMatch
               ? "NON Optional failed, Statement failed, try next rule..."
               WAIT
            ENDIF
            LOOP
         ELSE
            // Failed match is OPTIONAL.

            // defed out 2001-08-15 appear un-needed beacuse optional would have been ignored in favor of stopper.
            #ifdef NOT_NEEDED

               IF nOptional <> 0 .AND. aMP[2] < 0
                  sWorkLine := asRevert[Abs(nOptional)]
                  aMarkers  := aaRevertMarkers[Abs(nOptional)]

                  IF bDbgMatch
                     ? "*** Reverted: " + asRevert[nOptional]
                  ENDIF
               ELSE
                  sWorkLine := sPreMatch

                  IF bDbgMatch
                     ? "*** Reclaimed token/marker: " + sWorkLine
                  ENDIF

                  /* Commented out 07-21-2001 - Seems wrong !
                  IF aMP[1] > 1000 .AND. xMarker != NIL
                     IF bDbgMatch
                        ? "Removed repeatble: " + aTail( aMarkers[ aMP[1] - 1000 ] )
                        WAIT
                     ENDIF
                     aSize( aMarkers[ aMP[1] - 1000 ], Len( aMarkers[ aMP[1] - 1000 ] ) - 1 )
                  ENDIF
                  */
               ENDIF

            #endif

            IF bDbgMatch
               WAIT
            ENDIF

            //nOptional := aMP[2] // Commented 2001-08-15
            WHILE nMatch < nMatches
               nMatch++
               aMP := aRules[nRule][2][nMatch]
               IF ( aMP[2] == 0 )
                  EXIT
               ENDIF

               IF bDbgMatch
                  ? "Skipped:", nMatch, aMP[2], aMP[3]
               ENDIF
            ENDDO

            IF ( aMP[2] == 0 )
               IF bDbgMatch
                  ? "Statement failed, try next rule..."
                  WAIT
               ENDIF
               LOOP
            ENDIF
         ENDIF
      ENDIF

      sLine := ( PPOut( aResults[nRule], aMarkers ) )
      IF bDbgMatch
         ? "TRANSLATED to:", sLine
         WAIT
      ENDIF
      RETURN nRule

   ENDDO

   Alert( [ERROR! Logic failure] )

RETURN 0

//--------------------------------------------------------------//

#ifndef USE_C_BOOST

STATIC FUNCTION NextToken( sLine, lDontRecord )

   LOCAL sReturn, Counter, nLen, nClose
   LOCAL s1, s2, s3
   LOCAL sDigits

   //TRaceLog( sLine, lDontRecord )

   IF Empty( sLine )
      RETURN NIL
   ENDIF

   // *** To be removed after final testing !!!
   IF Left( sLine, 1 ) == ' '
      TraceLog( "!!!Left Pad: " + sLine )
      Alert( [!!!Left Pad: ] + sLine )
      sLine := LTrim( sLine )
   ENDIF

   nLen := Len( sLine )
   s1 := Left( sLine, 1 )

   BEGIN SEQUENCE

      IF nLen >= 2

         s2 := Left( sLine, 2 )

         IF s2 $ "++\--\->\:=\==\!=\<>\>=\<=\+=\-=\*=\^=\**\/=\%=\??"

            sReturn := s2

            BREAK

         ELSEIF s2 == "[["

            nClose := AT( ']]', sLine )
            IF nClose == 0
               //Alert( "ERROR! [NextToken()] Unterminated '[[' at: " + sLine + "[" + Str( ProcLine() ) + "]"  )
               sReturn := "["  // Clipper does NOT consider '[[' a single token
            ELSE
               sReturn := Left( sLine, nClose + 2 )
            ENDIF

            BREAK

         ENDIF

      ENDIF

      IF IsAlpha( s1 ) .OR. s1 == '_'

         sReturn := s1
         FOR Counter := 2 TO nLen
            s1 := SubStr( sLine, Counter, 1 )
            IF ! ( IsAlpha( s1 ) .OR. IsDigit( s1 ) .OR. s1 == "_" )
               EXIT
            ENDIF
            sReturn += s1
         NEXT

         BREAK

      ELSEIF IsDigit( s1 )

         sReturn := s1
         FOR Counter := 2 TO nLen
            s1 := SubStr( sLine, Counter, 1 )
            IF ! ( IsDigit( s1 ) )
               EXIT
            ENDIF
            sReturn += s1
         NEXT

         // Consume the point (and subsequent digits) only if digits follow...
         IF s1 == '.'
            sDigits := ""
            DO WHILE IsDigit( ( s1 := SubStr( sLine, ++Counter, 1 ) ) )
               sDigits += s1
            ENDDO

            IF ! ( sDigits == "" )
               sReturn += ( '.' + sDigits )
            ENDIF
         ENDIF

         // Either way we are done.
         BREAK

      ELSEIF s1 == '.'

         sDigits := ""
         FOR Counter := 2 TO nLen
            s1 := SubStr( sLine, Counter, 1 )
            IF ! ( IsDigit( s1 ) )
               EXIT
            ENDIF

            sDigits+= s1
         NEXT

         // Must have accumulated decimal digits.
         IF ! ( sDigits == "" )
            sReturn := '.' + sDigits

            BREAK
         ENDIF

         IF nLen >= 5 .AND. SubStr( sLine, 5, 1 ) == '.'

            s3 := Upper( SubStr( sLine, 2, 3 ) )
            IF s3 == 'AND'

               sReturn := ".AND."

               BREAK

            ELSEIF s3 == 'NOT'

               sReturn := "!"
               /* Skip the unaccounted letters ( .NOT. <-> ! ) */
               sLine := SubStr( sLine, 5 )

               BREAK

            ENDIF

         ENDIF

         IF nLen >= 4 .AND. SubStr( sLine, 4, 1 ) == '.' .AND. Upper( SubStr( sLine, 2, 2 ) ) == 'OR'

            sReturn := ".OR."

            BREAK

         ENDIF

         IF nLen >= 3 .AND. SubStr( sLine, 3, 1 ) == '.' .AND. Upper( SubStr( sLine, 2, 1 ) ) $ "TF"

            sReturn := Upper( Left( sLine, 3 ) )

            BREAK

         ENDIF

         sReturn := '.'

         BREAK

      ELSEIF s1 == '"'

         nClose := AT( '"', SubStr( sLine, 2 ) )
         IF nClose == 0
            //Alert( 'ERROR! [NextToken()] Unterminated ["] at: ' + sLine )
            sReturn := '"'
         ELSE
            sReturn := Left( sLine, nClose + 1 )
         ENDIF

         BREAK

      ELSEIF s1 == "'"

         nClose := AT( "'", SubStr( sLine, 2 ) )
         IF nClose == 0
            //Alert( "ERROR! [NextToken()] Unterminated ['] at: " + sLine )
            sReturn := "'"
         ELSE
            sReturn := SubStr( sLine, 2, nClose - 1 )
            IF ! ( '"' $ sReturn )
               sReturn := '"' + sReturn + '"'
            ELSE
               sReturn := "'" + sReturn + "'"
            ENDIF
         ENDIF

         BREAK

      ELSEIF s1 == '['

         IF s_bArrayPrefix
            sReturn := '['
         ELSE
            nClose := AT( ']', sLine )
            IF nClose == 0
               //Alert( "ERROR! [NextToken()] Unterminated '[' at: " + sLine + "[" + Str( ProcLine() ) + "]" )
               sReturn := '['
            ELSE
               sReturn := SubStr( sLine, 2, nClose - 2 )
               IF ! ( '"' $ sReturn )
                  sReturn := '"' + sReturn + '"'
               ELSEIF ! ( "'" $ sReturn )
                  sReturn := "'" + sReturn + "'"
               ELSE
                  sReturn := '[' + sReturn + ']'
               ENDIF
            ENDIF
         ENDIF

         BREAK

      ELSEIF s1 == "\"

         sReturn := s2

         BREAK

      ELSEIF s1 $ "+-*/:=^!&()[]{}@,|<>#%?$"

         sReturn := s1

         BREAK

      ELSE

         TraceLog( "Unexpected case: " + sLine )
         Alert( [Unexpected case: ] + sLine )
         sReturn := sLine

      ENDIF

   END SEQUENCE

   sLine := SubStr( sLine, Len( sReturn ) + 1 )

   IF lDontRecord != .T.
      IF Left( sReturn, 1 ) == '.' .AND. Len( sReturn ) > 1 .AND. Right( sReturn, 1 ) == '.'
         s_bArrayPrefix := .F.
      ELSE
         s1             := Right( sReturn, 1 )
         s_bArrayPrefix := ( IsAlpha( s1 ) .OR. IsDigit( s1 ) .OR. s1 $ "])}._" )
      ENDIF
   ENDIF

   sReturn += ExtractLeadingWS( @sLine )

   #ifdef PP_RECURSIVE

      IF s_bRecursive
         s1 := Left( sReturn, 1 )
         IF ( IsAlpha( s1 ) .OR. s1 == '_' ) .AND. MatchRule( sReturn, @sLine, aDefRules, aDefResults, .F., .F. ) > 0
            RETURN NextToken( @sLine, .T. )
         ENDIF

         IF MatchRule( sReturn, @sLine, aTransRules, aTransResults, .F., .T. ) > 0
            //? '>', sLine, '<'
            RETURN NextToken( @sLine, .T. )
         ENDIF

         //? sReturn, "not defined/translated."
         //WAIT
      ENDIF

   #endif

   //TraceLog( "TOKEN = '" + sReturn, sLine, s_bArrayPrefix )

RETURN sReturn

#endif

//--------------------------------------------------------------//

STATIC FUNCTION NextExp( sLine, cType, aWords, sNextAnchor, bX )

  LOCAL  sExp, sTemp, Counter, sPad, sToken, sList
  LOCAL  sNextLine, sNextToken, sLastToken, sJustToken, sJustNext, cLastChar
  LOCAL  s1, s2, s4, s5, sNext1, sNext2, sNext4, sNext5, nLen, nNextLen
  LOCAL  sWorkLine, sPrimaryStopper, nStoppers, nStopper, sStopLine, sStopper
  LOCAL  sMultiStopper, nSpaceAt, sNextStopper, cChar
  LOCAL  aExp

  IF Empty( sLine )
     RETURN NIL
  ENDIF

  //TraceLog( "*** Start", cType, sLine, sNextAnchor, bX )

  DO CASE
     CASE cType == '<'
        /* No prep needed */

     CASE cType == 'A'
        aExp := {}

     CASE cType == ','
        sList := ""

     CASE cType == ':'
        sWorkLine       := sLine
        sPrimaryStopper := NextToken( @sWorkLine )

        IF sPrimaryStopper == NIL
           //? "No primary", sPrimaryStopper
           RETURN NIL
        ELSE
           sPrimaryStopper := Upper( RTrim( sPrimaryStopper ) )

           /* Is it a stopper (the anchor of another acceptable match) ? */
           IF bDbgExp
              ? "Stopper?: '" + sPrimaryStopper +"'"
           ENDIF

           nStoppers := Len( aWords )
           FOR nStopper := 1 TO nStoppers

              sStopLine := sWorkLine
              sToken    := sPrimaryStopper
              sStopper  := aWords[ nStopper ]

              sMultiStopper := ""
              WHILE ( nSpaceAt := At( ' ', sStopper ) ) > 0
                 sNextStopper := Left( sStopper, nSpaceAt - 1 )

                 IF bX
                    nLen := 64
                 ELSE
                    nLen := Max( 4, Len( sToken ) )
                 ENDIF

                 //? "Next Stopper: " + sNextStopper, sToken
                 IF Left( sNextStopper, nLen ) == sToken
                    sMultiStopper += sNextStopper
                    sStopper      := SubStr( sStopper, nSpaceAt )
                    sMultiStopper += ExtractLeadingWS( @sStopper )
                    sToken        := NextToken( @sStopLine )
                    sToken        := Upper( RTrim( sToken ) )
                 ELSE
                    EXIT
                 ENDIF
              ENDDO

              IF bX
                 nLen := 64
              ELSE
                 nLen := Max( 4, Len( sToken ) )
              ENDIF

              IF Left( sStopper, nLen ) == sToken
                 sMultiStopper += sStopper
                 EXIT
              ENDIF
           NEXT

           IF nStopper <= nStoppers
              sLine := sStopLine
              //TraceLog( sMultiStopper, sStopLine )
              RETURN sMultiStopper
           ELSE
              sLine := sWorkLine
              RETURN NIL
           ENDIF
        ENDIF

     CASE cType == '*'
        sExp  := sLine
        sLine := ""
        //? "EXP <*>: " + sExp
        RETURN sExp

     CASE cType == '(' .AND. Left( sLine, 1 ) != '('
        nSpaceAt := At( ' ', sLine )

        IF nSpaceAt = 0
           sExp  := sLine
           sLine := ""
        ELSE
           sExp  := Left( sLine, nSpaceAt - 1 )
           sLine := SubStr( sLine, nSpaceAt )
           sExp  += ExtractLeadingWS( @sLine )
        ENDIF

        //? "EXP <(>: " + sExp
        RETURN sExp

     CASE cType == '!'
        IF IsAlpha( cChar := Left( sLine, 1 ) ) .OR. cChar == '_'
           RETURN NextToken( @sLine )
        ELSE
           RETURN NIL
        ENDIF

     CASE cType == NIL
        RETURN "-"
  ENDCASE

  sExp := ""
  DO WHILE .T.
     sToken := NextToken( @sLine )

     IF sToken == NIL
        EXIT
     ENDIF

     //TraceLog( sToken )

     sJustToken := RTrim( sToken )
     IF sNextAnchor != NIL  .AND. sJustToken == sNextAnchor
        // Clipper give preference to ',' in list expression.
        IF ! ( sNextAnchor $ ',' .AND. cType $ ",A" )
           //TraceLog( "Anchor: '" + sNextAnchor + "' found!" )
           sLine := sToken + sLine
           EXIT
        ENDIF
     ENDIF

     nLen := Len( sJustToken )
     s1 := Left( sJustToken, 1 )
     s2 := s4 := s5 := ""
     IF nLen == 2
        s2 := sJustToken
     ELSEIF nLen == 4
        s4 := Upper( sJustToken )
     ELSEIF nLen == 5
        s5 := Upper( sJustToken )
     ENDIF

     IF Empty( sLine )
        sNextToken := ""
        sJustNext  := ""
        sNext1     := ""
     ELSE
        sNextLine := sLine
        sNextToken := NextToken( @sNextLine, .T. )
        IF sNextToken == NIL
           sNextToken := ""
           sJustNext  := ""
           sNext1     := ""
        ELSE
           sJustNext := RTrim( sNextToken )
           sNext1    := Left( sJustNext, 1 )
        ENDIF
     ENDIF

     // ------------------
     // 1st. Level.
     // ------------------

     IF bDbgExp
        ? "1st. Level - Token: '" + sToken + "' Next: '" + sNextToken + "'"
        WAIT
     ENDIF

     //TraceLog( "Token: '" + sToken + "' Len: " + Str( nLen ) + " Next: '" + sNextToken + "'" )

     IF nLen == 1

        IF s1 $ "-+!:@|" // *** Very ODD Clipper consider '|' a continuation token !!!
           sExp += sToken
           LOOP
        ELSEIF s1 == "&"
           sExp += sToken
           IF sNext1 == '('
              LOOP
           ELSE
              IF IsAlpha( sNext1 ) .OR. sNext1 == '_'
                 sExp           += sNextToken
                 sLastToken     := sJustNext
                 sLine          := sNextLine
                 #ifdef USE_C_BOOST
                    SetArrayPrefix( .T. )
                 #else
                    s_bArrayPrefix := .T.
                 #endif
                 sNextToken     := NextToken( @sNextLine, .T. )
                 IF sNextToken != NIL .AND. Left( sNextToken, 1 ) == '.'
                    // Get the macro terminator.
                    sExp           += sNextToken
                    sLastToken     := "."
                    sLine          := sNextLine
                    #ifdef USE_C_BOOST
                       SetArrayPrefix( .T. )
                    #else
                       s_bArrayPrefix := .T.
                    #endif
                    IF sNextToken == '.' //(Last Token) No space after Macro terminator, so get the suffix.
                       sNextToken := NextToken( @sNextLine, .T. )
                       IF sNextToken != NIL
                          sNext1 := Left( sNextToken, 1 )
                          IF IsAlpha( sNext1 ) .OR. IsDigit( sNext1 ) .OR. sNext1 == '_'
                             // Get the macro sufix.
                             sExp           += sNextToken
                             sLastToken     := RTrim( sNextToken )
                             sLine          := sNextLine
                             #ifdef USE_C_BOOST
                                SetArrayPrefix( .T. )
                             #else
                                s_bArrayPrefix := .T.
                             #endif
                          ENDIF
                       ENDIF
                    ENDIF
                 ENDIF
              ELSE
                 Alert( [ERROR! Invalid '&' at: ] + sExp + sNextToken )
                 EXIT
              ENDIF
           ENDIF

            sLastToken := RTrim( sLastToken )
           // Continue  2nd level checks below.
        ELSEIF s1 == '('
           sExp += sToken
           IF Left( sNext1, 1 ) == ')'
              sExp           += sNextToken
              sLine          := sNextLine
              #ifdef USE_C_BOOST
                 SetArrayPrefix( .T. )
              #else
                 s_bArrayPrefix := .T.
              #endif
           ELSE
              //TraceLog( "Content from: " + sLine )
              sTemp := NextExp( @sLine, ',', NIL, NIL ) // Content - Ignoring sNextAnchor !!!
              IF sTemp == NIL
                 TraceLog( "ERROR!(1) No content at: '" + sLine + "' After: " + sExp, sLine  )
                 Alert( [ERROR!(1) No content at: ] + "'" + sLine + "'"+[ After: ] + sExp  )
                 EXIT
              ELSE
                 sExp +=  sTemp
                 //TraceLog( "Content: '" + sTemp + "'", sExp, sLine )
              ENDIF

              sToken := NextToken( @sLine ) // Close
              IF sToken == NIL
                 TraceLog( "ERROR!(2) Unbalanced '(' at: " + sExp, sLine )
                 Alert( [ERROR!(2) Unbalanced '(' at: ] + sExp )
                 EXIT
              ELSEIF Left( sToken, 1 ) == ')'
                 sExp += sToken
              ELSE
                 sLine := sToken + sLine
                 TraceLog( "ERROR!(3) Unbalanced '(' Found: '" +  sToken + "' at: " + sExp, sLine )
                 Alert( [ERROR!(3) Unbalanced '(' Found: ] + "'" +  sToken + "'"+[ at: ] + sExp )
                 EXIT
              ENDIF
           ENDIF

           sLastToken := ")"
           // Continue  2nd level checks below.
        ELSEIF s1 == '{'
           sExp  += sToken

         #ifdef EXPLICIT_BLOCK

           IF sNext1 == '|'
              /* Literal block */
              sExp           += sNextToken
              sLine          := sNextLine
              #ifdef USE_C_BOOST
                 SetArrayPrefix( .F. )
              #else
                 s_bArrayPrefix := .F.
              #endif
              sNextToken     := NextToken( @sNextLine, .T. )
              IF sNextToken != NIL .AND. Left( sNextToken, 1 ) == '|'
                 sExp           += sNextToken
                 sLine          := sNextLine
                 #ifdef USE_C_BOOST
                    SetArrayPrefix( .F. )
                 #else
                    s_bArrayPrefix := .F.
                 #endif
              ELSE
                 sTemp := NextExp( @sLine, ',', NIL, NIL ) // Content - Ignoring sNextAnchor !!!
                 IF sTemp == NIL
                    TraceLog( "ERROR! Unbalanced '{|...' at: " + sExp )
                    Alert( [ERROR! Unbalanced '{|...' at: ] + sExp )
                    EXIT
                 ELSE
                    sExp += sTemp
                 ENDIF

                 /* sLine was changed by NextExp()! */
                 sNextLine  := sLine
                 sNextToken := NextToken( @sNextLine, .T. )
                 IF sNextToken != NIL .AND. Left( sNextToken, 1 ) == '|'
                    sExp           += sNextToken
                    sLine          := sNextLine
                    #ifdef USE_C_BOOST
                       SetArrayPrefix( .F. )
                    #else
                       s_bArrayPrefix := .F.
                    #endif
                 ELSE
                    TraceLog( "ERROR! Unbalanced '{|...|' at: " + sExp, sNextToken, sNextLine )
                    Alert( [ERROR! Unbalanced '{|...|' at: ] + sExp )
                    EXIT
                 ENDIF
              ENDIF

              sTemp := NextExp( @sLine, ',', NIL, NIL ) // Content - Ignoring sNextAnchor !!!
              IF sTemp == NIL
                 TraceLog( "ERROR! Empty '{||'" )
                 Alert( [ERROR! Empty '{||'] )
                 EXIT
              ELSE
                 sExp +=  sTemp
              ENDIF

              sToken := NextToken( @sLine ) // Close
              IF sToken == NIL
                 TraceLog( "ERROR! Unbalanced '{' at: " + sExp )
                 Alert( [ERROR! Unbalanced '{' at: ] + sExp )
                 EXIT
              ELSEIF Left( sToken, 1 ) == '}'
                 sExp += sToken
              ELSE
                 sLine := sToken + sLine
                 TraceLog( "ERROR! Unbalanced '{' at: " + sExp )
                 Alert( [ERROR! Unbalanced '{' at: ] + sExp )
                 EXIT
              ENDIF
           ELSE

         #endif

              /* Literal array */
              IF sNext1 == '}'
                 sExp           += sNextToken
                 sLine          := sNextLine
                 #ifdef USE_C_BOOST
                    SetArrayPrefix( .T. )
                 #else
                    s_bArrayPrefix := .T.
                 #endif
              ELSE
                 sTemp := NextExp( @sLine, ',', NIL, NIL ) // Content - Ignoring sNextAnchor !!!
                 IF sTemp == NIL
                    TraceLog( "ERROR! Unbalanced '{...'", sLine )
                    Alert( [ERROR! Unbalanced '{...'] )
                    EXIT
                 ELSE
                    sExp +=  sTemp
                 ENDIF

                 sToken := NextToken( @sLine ) // Close
                 IF sToken == NIL
                    TraceLog( "ERROR! Unbalanced '{' at: " + sExp )
                    Alert( [ERROR! Unbalanced '{' at: ] + sExp )
                    EXIT
                 ELSEIF Left( sToken, 1 ) == '}'
                    sExp += sToken
                 ELSE
                    sLine := sToken + sLine
                    TraceLog( "ERROR! Unbalanced '{' at: " + sExp )
                    Alert( [ERROR! Unbalanced '{' at: ] + sExp )
                    EXIT
                 ENDIF
              ENDIF

         #ifdef EXPLICIT_BLOCK
           ENDIF
         #endif

           sLastToken := "}"
           // Continue  2nd level checks below.
        ELSEIF s1 == "["
           sExp  += sToken
           sTemp := NextExp( @sLine, ',', NIL, NIL ) // Content - Ignoring sNextAnchor !!!
           IF sTemp == NIL
              Alert( [ERROR! Unbalanced ] + "'['" + [ at: ] + sExp )
              EXIT
           ELSE
              sExp += sTemp
           ENDIF

           sToken := NextToken( @sLine ) // Close
           IF sToken == NIL
              Alert( [ERROR! Unbalanced ] + "'['" + [ at: ] + sExp )
              EXIT
           ELSEIF Left( sToken, 1 ) == ']'
              sExp += sToken
           ELSE
              sLine := sToken + sLine
              Alert( [ERROR! Unbalanced ] + "'['" + [ at: ] + sExp )
              EXIT
           ENDIF

           sLastToken := "]"
           // Continue  2nd level checks below.
        ELSEIF s1 $ ".*/=^><!$%#)}]?"
           sLine := sToken + sLine
           EXIT
        ELSEIF s1 == ","
           IF cType == ","
              sList += ( sExp + sToken )
              sExp  := ""
              LOOP
           ELSEIF cType == "A"
              aAdd( aExp, sExp )
              sExp  := ""
              LOOP
           ELSE
              //? "DONT CONTINUE: " + sLine
              sLine := sToken + sLine
              EXIT
           ENDIF
        ELSE
           sExp       += sToken
           sLastToken := sJustToken
        ENDIF

     ELSEIF nLen == 2

        IF s2 $ '++\--'
           sExp += sToken
           LOOP
        ELSEIF s2 $ "->\:=\==\!=\<>\>=\<=\+=\-=\*=\^=\**\/=\%=\??"
           sLine := sToken + sLine
           EXIT
        ELSE
           sExp       += sToken
           sLastToken := sJustToken
        ENDIF

     ELSEIF nLen == 4

        IF s4 == '.OR.'
           sLine := sToken + sLine
           EXIT
        ELSE
           sExp       += sToken
           sLastToken := sJustToken
        ENDIF

     ELSEIF nLen == 5

        IF s5 == '.AND.'
           sLine := sToken + sLine
           EXIT
        /* .NOT. is being translated to ! at NextToken() !!!
        ELSEIF s5 == ".NOT."
           sExp       += sToken
           LOOP
        */
        ELSE
           sExp       += sToken
           sLastToken := sJustToken
        ENDIF

     ELSE

        sExp       += sToken
        sLastToken := sJustToken

     ENDIF

     // ------------------
     // 2nd. Level.
     // ------------------

     //TraceLog( sExp, sLastToken, sLine, nLen, sToken, sNextToken )

     IF sLastToken == NIL .OR. Right( sLastToken, 1 ) == ' '
        TraceLog( sExp, sLastToken, sLine, nLen, sToken, sNextToken )
        Alert( "??? " + sExp )
        EXIT
     ENDIF

     nLen := Len( sLastToken )
     cLastChar := Right( sLastToken, 1 )

     IF Empty( sLine )
        EXIT
     ELSE
        sNextLine  := sLine
        sNextToken := NextToken( @sNextLine, .T. )
        IF sNextToken == NIL
           sNextToken := ""
        ENDIF
     ENDIF

     sJustNext := RTrim( sNextToken )
     nNextLen := Len( sJustNext )
     sNext1 := Left( sJustNext, 1 )
     sNext2 := sNext4 := sNext5 := ""
     IF nNextLen == 2
        sNext2 := sJustNext
     ELSEIF nNextLen == 4
        sNext4 := Upper( sJustNext )
     ELSEIF nNextLen == 5
        sNext5 := sJustNext
     ENDIF

     IF bDbgExp
        ? "2nd. Level - Token: '" + sToken + "' Next: '" + sNextToken + "'"
        WAIT
     ENDIF

     IF sNextAnchor != NIL  .AND. sJustNext == sNextAnchor
        // Clipper give preference to ',' in list expression.
        IF ! ( sNextAnchor == ',' .AND. cType $ ",A" )
           EXIT
        ENDIF
     ENDIF

     //TraceLog( sExp, sToken, sJustToken, nLen, sNextToken, sJustNext, nNextLen, sLastToken, cLastChar, sNextAnchor )

     IF nNextLen == 1

        IF sNext1 == '(' .AND. ( IsAlpha( cLastChar ) .OR. IsDigit( cLastChar ) .OR. cLastChar $ "_."  )
           LOOP
        ELSEIF sNext1 == '[' // No need to check prefix because NextToken() already has the logic.
           LOOP
        ELSEIF sNext1 $ "+-*/:=^!><!$%#|" // *** Very ODD Clipper consider '|' a continuation token !!!
           sExp           += sNextToken
           sLine          := sNextLine
           #ifdef USE_C_BOOST
              SetArrayPrefix( .F. )
           #else
              s_bArrayPrefix := .F.
           #endif
           LOOP
        ENDIF

     ELSEIF nNextLen == 2

        IF sNext2 $ "--\++"
           IF IsAlpha( cLastChar  ) .OR. IsDigit( cLastChar ) .OR. cLastChar $ "_.]"
              sExp           += sNextToken
              sLine          := sNextLine
              #ifdef USE_C_BOOST
                 SetArrayPrefix( .F. )
              #else
                 s_bArrayPrefix := .F.
              #endif
           ENDIF
        ELSEIF sNext2 $ "->\:=\==\!=\<>\>=\<=\+=\-=\*=\/=\^=\**\%="
           sExp           += sNextToken
           sLine          := sNextLine
           #ifdef USE_C_BOOST
              SetArrayPrefix( .T. )
           #else
              s_bArrayPrefix := .T.
           #endif
           LOOP
        ENDIF

     ELSEIF nNextLen == 4

        IF sNext4 == ".OR."
           sExp           += sNextToken
           sLine          := sNextLine
           #ifdef USE_C_BOOST
              SetArrayPrefix( .F. )
           #else
              s_bArrayPrefix := .F.
           #endif
           LOOP
        ENDIF

     ELSEIF nNextLen == 5

        IF sNext5 == ".AND."
           sExp           += sNextToken
           sLine          := sNextLine
           s_bArrayPrefix := .F.
           #ifdef USE_C_BOOST
              SetArrayPrefix( .F. )
           #else
              s_bArrayPrefix := .F.
           #endif
           LOOP
        /* .NOT. is being translated to ! at NextToken() !!!
        ELSEIF sNext5 == ".NOT."
           sExp           += sNextToken
           sLine          := sNextLine
           s_bArrayPrefix := .F.
           #ifdef USE_C_BOOST
              SetArrayPrefix( .F. )
           #else
              s_bArrayPrefix := .F.
           #endif */
        ENDIF

     ENDIF

     // ------------------
     // 3rd. Level.
     // ------------------

     IF sNext1 == ','
        IF cType == ","
           sList          += ( sExp + sNextToken )
           sLine          := sNextLine
           #ifdef USE_C_BOOST
              SetArrayPrefix( .F. )
           #else
              s_bArrayPrefix := .F.
           #endif
           sExp           := ""
        ELSEIF cType == "A"
           aAdd( aExp, sExp )
           sLine          := sNextLine
           #ifdef USE_C_BOOST
              SetArrayPrefix( .F. )
           #else
              s_bArrayPrefix := .F.
           #endif
           sExp           := ""
        ELSE
           //? "DONT CONTINUE: " + sLine
           EXIT
        ENDIF
     ELSE
        //? "DONT CONTINUE: " + sLine
        EXIT
     ENDIF
  ENDDO

  IF cType == 'A'
     IF sExp == ""
        IF Len( aExp ) == 0
           aExp := NIL
        ENDIF
     ELSE
        aAdd( aExp, sExp )
     ENDIF

     IF bDbgExp
        IF ! ( ProcName(1) == "NEXTEXP" )
           ? "List Exp: " + '{'
           FOR Counter := 1 TO Len( aExp )
              ?? aExp[Counter]
              IF Counter < Len( aExp )
                 ?? ','
              ENDIF
           NEXT
           ?? '}'
        ENDIF
     ENDIF
  ELSEIF cType == ','
     IF sExp == ""
        IF sList == ""
           sExp := NIL
        ELSE
           sExp := sList
        ENDIF
     ELSE
        sExp := ( sList + sExp )
     ENDIF

     IF bDbgExp
        ? "List =", sExp, " Next:", sLine
     ENDIF
  ELSE
     IF sExp == ""
        sExp := NIL
     ENDIF
     IF bDbgExp
        ? "Exp =", sExp, " Next:", sLine
     ENDIF
  ENDIF

  IF bDbgExp
     WAIT
  ENDIF

  //TraceLog( "*** Finish", cType, aExp, sExp, sLine, sNextAnchor )

RETURN IIF( cType == 'A', aExp, sExp )

//--------------------------------------------------------------//

STATIC FUNCTION PPOut( aResults, aMarkers )

  LOCAL Counter, nResults, sResult := "", nMarker, nMatches, nMatch
  LOCAL xValue, nRepeats := 0, nDependee, nGroupStart, sDumb, aBackUp := aClone( aMarkers )
  LOCAL nMarkers, anMarkers, bBuildList
  LOCAL nGroupIterator

  IF aResults[1] == NIL
     nResults := 0
  ELSE
     nResults := Len( aResults[1] )
  ENDIF

  FOR Counter := 1 TO nResults

      IF bDbgPPO
         ? Counter, "of:", nResults, sResult, nGroupStart, nRepeats
         WAIT
      ENDIF

     /* Normal mode. */
     IF nRepeats == 0

        nDependee := aResults[1][Counter][1]

        IF nDependee > 0
           nGroupStart := Counter

           nGroupIterator := Counter
           nRepeats := 0
           WHILE nGroupIterator <= nResults .AND. aResults[1][nGroupIterator][1] == nDependee
              IF ValType( aResults[1][nGroupIterator][2] ) == 'N' .AND. ValType( aMarkers[ aResults[1][nGroupIterator][2] ] ) == 'A'
                 nRepeats := Max( nRepeats, Len( aMarkers[ aResults[1][nGroupIterator][2] ] ) )
              ENDIF
              nGroupIterator++
           ENDDO
           IF nRepeats > 0
              anMarkers := {}
              bBuildList := .T.
           ENDIF

        #ifdef z
           IF aMarkers[ nDependee ] == NIL
              nRepeats := 0
           ELSE
              nRepeats := Len( aMarkers[ nDependee ] )
              anMarkers := {}
              bBuildList := .T.
           ENDIF
        #endif

           IF bDbgPPO
              ? Counter, nDependee, aMarkers, ValType( aMarkers ), nRepeats
              WAIT
           ENDIF

           IF nRepeats > 0
              IF ValType( aResults[1][Counter][2] ) == 'N'
                 IF bBuildList .AND. aScan( anMarkers, nDependee ) == 0
                    aAdd( anMarkers, nDependee )
                 ENDIF

                 // For group head nDependee and nMaker _must_ be identical.
                 IF ValType( aMarkers[ nDependee ] ) == 'A'
                    xValue := aMarkers[ nDependee ][1]
                 ELSE
                    xValue := aMarkers[ nDependee ]
                 ENDIF
              ELSE
                 sResult += aResults[1][Counter][2]
                 LOOP
              ENDIF
           ELSE
              IF bDbgPPO
                 ? "Skipping other dependants"
                 WAIT
              ENDIF

              /* Skip all other dependants. */
              Counter++
              WHILE Counter < nResults .AND. aResults[1][Counter][1] == nDependee
                 Counter++
              ENDDO
              Counter-- // LOOP will increased.
              LOOP
           ENDIF

        ELSE // IF nDependee > 0

           IF ValType( aResults[1][Counter][2] ) == 'N'
              xValue := aMarkers[ aResults[1][Counter][2] ]
           ELSE
              sResult += aResults[1][Counter][2]
              LOOP
           ENDIF

        ENDIF // IF nDependee > 0

     ELSE /* Repeat mode. */

        /* Still in repeat group? */
        IF aResults[1][Counter][1] == nDependee

           IF ValType( aResults[1][Counter][2] ) == 'N'
              //IF aMarkers[ aResults[1][Counter][2] ] != NIL
                 IF bBuildList .AND. aScan( anMarkers, aResults[1][Counter][2] ) == 0
                    aAdd( anMarkers, aResults[1][Counter][2] )
                 ENDIF
                 IF aMarkers[ aResults[1][Counter][2] ] == NIL .OR. Len( aMarkers[ aResults[1][Counter][2] ] ) == 0
                    xValue := NIL
                 ELSEIF ValType( aMarkers[ aResults[1][Counter][2] ] ) == 'A'
                    xValue := aMarkers[ aResults[1][Counter][2] ][1]
                 ELSE
                    xValue := aMarkers[ aResults[1][Counter][2] ]
                 ENDIF
                 //aDel( aMarkers[ aResults[1][Counter][2] ], 1 )
                 //aSize( aMarkers[ aResults[1][Counter][2] ], nRepeats - 1 )
              //ELSE
              //   xValue := ""
              //ENDIF
           ELSE
              sResult += aResults[1][Counter][2]
              LOOP
           ENDIF
        ELSE
           nRepeats--
           bBuildList := .F.

           nMarkers := Len( anMarkers )
           FOR nMarker := 1 TO nMarkers
              // Clipper does not remove optional nested repeatable which only has single value if main repeatable has more values.
              IF ValType( aMarkers[ anMarkers[nMarker] ] ) == 'A' .AND. Len( aMarkers[ anMarkers[nMarker] ] ) > 1
                 aDel( aMarkers[ anMarkers[nMarker] ], 1 )
                 aSize( aMarkers[ anMarkers[nMarker] ], nRepeats )
                 IF bDbgPPO
                    ? nMarker, "Removed Repeatable"
                    WAIT
                 ENDIF
              ELSE
                 IF bDbgPPO
                    ? nMarker, "Removed Repeatable skipped"
                    WAIT
                 ENDIF
              ENDIF
           NEXT

           IF nRepeats > 0
              IF bDbgPPO
                 ? "Looping to: ", nGroupStart, nRepeats
                 WAIT
              ENDIF

              Counter := nGroupStart - 1 // LOOP will increase
              LOOP
           ELSE
              IF bDbgPPO
                 ? "Repeats Finished: "
                 WAIT
              ENDIF

              // Restore for possible re-use.
              aMarkers := aClone( aBackup )

                 /* Recheck this item in "normal" mode. */
              Counter--
              LOOP
           ENDIF
        ENDIF

     ENDIF

     nMarker := aResults[1][Counter][2]

     IF bDbgPPO
        ? "Outputing:", Counter, nMarker, nGroupStart, nRepeats
        WAIT
     ENDIF

     DO CASE
        /* <-x-> Ommit. */
        CASE aResults[2][Counter] == 0

        /* <x> Regular */
        CASE aResults[2][Counter] == 1
           IF ValType( xValue ) == 'A'
              nMatches := Len( xValue )
              FOR nMatch := 1 TO nMatches
                 sResult += xValue[nMatch]
                 IF nMatch < nMatches
                    sResult += ', '
                 ENDIF
              NEXT
           ELSE
              IF xValue != NIL
                 sResult += xValue
              ENDIF
           ENDIF

        /* #<x> Dumb Stringify */
        CASE aResults[2][Counter] == 2
           IF ValType( xValue ) == 'A'
              sDumb := ""
              nMatches := Len( xValue )
              FOR nMatch := 1 TO nMatches
                 sDumb += xValue[nMatch]
                 IF nMatch < nMatches
                    sDumb += ", "
                 ENDIF
              NEXT
              IF '"' $ sDumb .AND. "'" $ sDumb .AND. ']' $ sDumb .AND. Left( sDumb, 1 ) != '['
                 sResult += '[[' + sDumb + ']]'
              ELSEIF '"' $ sDumb .AND. "'" $ sDumb
                 sResult += '[' + sDumb + "]"
              ELSEIF '"' $ sDumb
                 sResult += "'" + sDumb + "'"
              ELSE
                 sResult += '"' + sDumb + '"'
              ENDIF
           ELSE
              IF xValue == NIL
                 sResult += '""'
              ELSE
                 IF '"' $ xValue .AND. "'" $ xValue .AND. ']' $ xValue .AND. Left( xValue, 1 ) != '['
                    sResult += "[[" + xValue + "]]"
                 ELSEIF '"' $ xValue .AND. "'" $ xValue
                    sResult += '[' + xValue + "]"
                 ELSEIF '"' $ xValue
                    sResult += "'" + xValue + "'"
                 ELSE
                    sResult += '"' + xValue + '"'
                 ENDIF
              ENDIF
           ENDIF

        /* <"x"> Normal Stringify */
        CASE aResults[2][Counter] == 3
           IF ValType( xValue ) == 'A'
              nMatches := Len( xValue )
              FOR nMatch := 1 TO nMatches
                 IF Left( xValue[nMatch], 1 ) == '&'
                    IF Right( xValue[nMatch], 1 ) == '.'
                       sResult += SubStr( xValue[nMatch], 2, Len( xValue[nMatch] ) - 2 )
                    ELSE
                       sResult += SubStr( xValue[nMatch], 2 )
                    ENDIF
                 ELSEIF '"' $ xValue[nMatch] .AND. "'" $ xValue[nMatch] .AND. ']' $ xValue[nMatch] .AND. Left( xValue[nMatch], 1 ) != '['
                    sResult += "[[" + RTrim( xValue[nMatch] ) + "]]"
                 ELSEIF '"' $ xValue[nMatch] .AND. "'" $ xValue[nMatch]
                    sResult += '[' + RTrim( xValue[nMatch] ) + "]"
                 ELSEIF '"' $ xValue[nMatch]
                    sResult += "'" + RTrim( xValue[nMatch] ) + "'"
                 ELSE
                    sResult += '"' + RTrim( xValue[nMatch] ) + '"'
                 ENDIF

                 IF nMatch < nMatches
                    sResult += ', '
                 ENDIF
              NEXT
           ELSE
              IF ! ( xValue == NIL )
                 IF Left( xValue, 1 ) == '&'
                    IF Right( xValue, 1 ) == '.'
                       sResult += SubStr( xValue, 2, Len( xValue ) - 2 )
                    ELSE
                       sResult += SubStr( xValue, 2 )
                    ENDIF
                 ELSEIF '"' $ xValue .AND. "'" $ xValue .AND. ']' $ xValue .AND. Left( xValue, 1 ) != '['
                    sResult += "[[" + xValue + "]]"
                 ELSEIF '"' $ xValue .AND. "'" $ xValue
                    sResult += '[' + xValue + ']'
                 ELSEIF '"' $ xValue
                    sResult += "'" + xValue + "'"
                 ELSE
                    sResult += '"' + xValue + '"'
                 ENDIF
              ENDIF
           ENDIF

        /* <(x)> Smart Stringify */
        CASE aResults[2][Counter] == 4
           IF ValType( xValue ) == 'A'
              nMatches := Len( xValue )
              FOR nMatch := 1 TO nMatches
                 IF Left( xValue[nMatch], 1 ) $ "('[" + '"'
                    sResult += xValue[nMatch]
                 ELSE
                    IF Left( xValue[nMatch], 1 ) == '&'
                       IF Right( xValue[nMatch], 1 ) == '.'
                          sResult += SubStr( xValue[nMatch], 2, Len( xValue[nMatch] ) - 2 )
                       ELSE
                          sResult += SubStr( xValue[nMatch], 2 )
                       ENDIF
                    ELSE
                       sResult += '"' + RTrim( xValue[nMatch] ) + '"'
                    ENDIF
                 ENDIF

                 IF nMatch < nMatches
                    sResult += ', '
                 ENDIF
              NEXT
           ELSE
              IF xValue != NIL
                 IF Left( xValue, 1 ) $ "('[" + '"'
                    sResult += xValue
                 ELSE
                    IF Left( xValue, 1 ) == '&'
                       IF Right( xValue, 1 ) == '.'
                          sResult += SubStr( xValue, 2, Len( xValue ) - 2 )
                       ELSE
                          sResult += SubStr( xValue, 2 )
                       ENDIF
                    ELSE
                       sResult += '"' + RTrim( xValue ) + '"'
                    ENDIF
                 ENDIF
              ENDIF
           ENDIF

        /* <{x}> Blockify */
        CASE aResults[2][Counter] == 5
           IF ValType( xValue ) == 'A'
              nMatches := Len( xValue )
              FOR nMatch := 1 TO nMatches
                 sResult += "{||" + xValue[nMatch] + '}'
                 IF nMatch < nMatches
                    sResult += ', '
                 ENDIF
              NEXT
           ELSE
              IF xValue != NIL
                 sResult += "{||" + xValue + '}'
              ENDIF
           ENDIF

        /* <.x.> Logify */
        CASE aResults[2][Counter] == 6
           IF ValType( xValue ) == 'A'
              nMatches := Len( xValue )
              FOR nMatch := 1 TO nMatches
                 sResult += ".T."
                 IF nMatch < nMatches
                    sResult += ', '
                 ENDIF
              NEXT
           ELSE
              IF Empty( xValue )
                 sResult += ".F."
              ELSE
                 sResult += ".T."
              ENDIF
           ENDIF

     ENDCASE

     IF nRepeats > 1 .AND. Counter == nResults
        nRepeats--
        Counter := nGroupStart - 1

        bBuildList := .F.

        nMarkers := Len( anMarkers )
        FOR nMarker := 1 TO nMarkers
           // Clipper does not remove optional nested repeatable which only has single value if main repeatable has more values.
           IF ValType( aMarkers[ anMarkers[nMarker] ] ) == 'A' .AND. ( Len( aBackup[ anMarkers[1] ] ) = 1 .OR. Len( aMarkers[ anMarkers[nMarker] ] ) > 1 )
              aDel( aMarkers[ anMarkers[nMarker] ], 1 )
              aSize( aMarkers[ anMarkers[nMarker] ], nRepeats )
              IF bDbgPPO
                 ? nMarker, "Removed Repeatable"
                 WAIT
              ENDIF
           ELSE
              IF bDbgPPO
                 ? nMarker, Len( aBackup[ anMarkers[1] ] ), Len( aMarkers[ anMarkers[nMarker] ] ),"Removed Repeatable"
                 WAIT
              ENDIF
           ENDIF
        NEXT

        IF bDbgPPO
           ? "END - Looping: ", Counter, nMarker, nGroupStart, nRepeats
           WAIT
        ENDIF
     ENDIF

     IF bDbgPPO
        ? "Bottom: ", Counter, nMarker, nGroupStart, nRepeats
        WAIT
     ENDIF

  NEXT

  IF bDbgPPO
     ? "*** OUT: " + sResult
     WAIT
  ENDIF

  //TraceLog( sResult )

RETURN sResult

//--------------------------------------------------------------//

STATIC FUNCTION CompileRule( sRule, aRules, aResults, bX, bUpper )

   LOCAL nNext, sKey, sAnchor, nOptional := 0, cType, nId := 0, aRule, aMatch, aWords
   LOCAL nOptionalAt, nMarkerAt, aMarkers := {}, Counter, nType, aResult := {}, sTemp, aModifiers, aValues
   LOCAL aRP, nAt, sResult, nCloseAt, sMarker, nCloseOptionalAt, sPad, nResults, nMarker, nMP, nMatches, nOffset
   LOCAL nWord, nWords, cChar
   LOCAL nLen, s1, s2, s3
   LOCAL sRuleCopy := sRule
   LOCAL nLastOptional, nPending
   LOCAL sDots
   /*
   nMarkerID
   nOPTIONAL
   sAnchor
   cTYPE
   aLIST
   aNext
   */

   //? "=>" + sRule + "<="

   //TraceLog( sRule )

   ExtractLeadingWS( @sRule )

   sKey := NextToken( @sRule )
   IF Left( sKey, 1 ) == '\'
      sKey := SubStr( sKey, 2, 1 )
   ENDIF

   DropTrailingWS( @sKey )

   IF bUpper
      sKey := Upper( sKey )
   ENDIF

   //? "KEY: '" + sKey + "'"

   aRule := { sKey, {}, bX }

   nNext := At( "=>", sRule )
   IF nNext == 0
      Alert( [ERROR! Invalid translation format: ] + sRule )
      RETURN .F.
   ELSE
      sResult := SubStr( sRule, nNext + 2 )
      ExtractLeadingWS( @sResult )
      sRule   := Left( sRule, nNext - 1 )
   ENDIF

   DO WHILE ! ( Left( sRule, 1 ) == '' )
      //? "Scaning: " + sRule

      nLen := Len( sRule )

      s1 := Left( sRule, 1 )
      IF nLen >= 2
         s2 := Left( sRule, 2 )
      ENDIF
      IF nLen >= 3
         s3 := Upper( Left( sRule, 3 ) )
      ENDIF

      BEGIN SEQUENCE

         IF nLen >= 5
            IF s1 == '.' .AND. Upper( SubStr( sRule, 2, 3 ) ) == 'AND' .AND. SubStr( sRule, 5, 1 ) == '.'
               sTemp := ".AND."
               BREAK
            ELSEIF s1 = '.' .AND. Upper( SubStr( sRule, 2, 3 ) ) == 'NOT' .AND. SubStr( sRule, 5, 1 ) == '.'
               sTemp := "!"
               /* Skip the unaccounted letters ( .NOT. <-> ! ) */
               sRule := SubStr( sRule, 4 )
               BREAK
            ENDIF
         ENDIF

         IF nLen >= 4 .AND. s1 == '.' .AND. Upper( SubStr( sRule, 2, 2 ) ) == 'OR' .AND. SubStr( sRule, 4, 1 ) == '.'
            sTemp := ".OR."
            BREAK
         ENDIF

         IF nLen >= 3 .AND. s3 $ ".T.\.F."
            sTemp := s3
            BREAK
         ENDIF

         IF nLen >= 2
            IF s2 $ "++\--\->\:=\==\!=\<>\>=\<=\+=\-=\*=\^=\**\/=\%=\??"
               sTemp := s2
               BREAK
            ENDIF
         ENDIF

         IF nLen >= 1
            IF s1 == '\'
               sTemp := SubStr( sRule, 2, 1 )
               sRule   := SubStr( sRule, 2 )
               BREAK
            ELSEIF s1 == '_' .OR. IsAlpha( s1 )
               sTemp := Upper( RTrim( NextToken( sRule ) ) ) // Not by refernce because of SubStr() below!!!
               BREAK
            ELSEIF s1 == '.' // Might pull decimal numbers...
               sTemp := RTrim( NextToken( sRule ) ) // Not by refernce because of SubStr() below!!!
               BREAK
            ELSEIF IsDigit( s1 )
               sTemp := RTrim( NextToken( sRule ) ) // Not by refernce because of SubStr() below!!!
               BREAK
            ELSEIF s1 == ']' .AND. nOptional == 0
               sTemp := ']'
               BREAK
            ELSEIF s1 $ "+-*/:=^!&(){}@,|>#%?$"
               sTemp := s1
               BREAK
            ENDIF
         ENDIF

      END SEQUENCE

      IF sTemp != NIL
         IF ! ( sAnchor == NIL )
            //TraceLog( "ORPHAN ANCHOR: " + sAnchor )

            aMatch := { 0, nOptional, sAnchor, NIL, NIL }
            //? aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5]
            aAdd( aRule[2], aMatch )

            /* Next dependant optional will be marked as trailing. */
            IF nOptional > 0
               nOptional := ( -nOptional )
            ENDIF
         ENDIF

         sAnchor := sTemp // Next Anchor
         sRule   := SubStr( sRule, Len( sAnchor ) + 1 )
         ExtractLeadingWS( @sRule )

         sTemp := NIL // Resetting.
         LOOP
      ENDIF

      IF s1 == '<'
         nId++

         /* Skip trailing spaces...*/
         sRule := SubStr( sRule, 2 )
         ExtractLeadingWS( @sRule )

         DO CASE
            CASE SubStr( sRule, 1, 1 ) == '*'
               cType := '*'

               sRule := SubStr( sRule, 2 )
               ExtractLeadingWS( @sRule )

               nNext := At( '*', sRule )
               IF nNext > 1
                  sMarker := Left( sRule, nNext - 1 )
                  aAdd( aMarkers, RTrim( sMarker ) )

                  sRule := SubStr( sRule, nNext + 1 )
                  ExtractLeadingWS( @sRule )

                  IF Left( sRule, 1 ) == '>'
                     sRule := SubStr( sRule, 2 )
                     ExtractLeadingWS( @sRule )
                  ELSE
                     Alert( [ERROR! Unblanced MP: '<*' : '] + sRule + "'" )
                  ENDIF

                  aMatch := { nId, nOptional, sAnchor, cType, NIL }
                  //? aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5]
                  aAdd( aRule[2], aMatch )

                  /* Next dependant optional will be marked as trailing. */
                  IF nOptional > 0
                     nOptional := ( -nOptional )
                  ENDIF

                  sAnchor := NIL
                  LOOP
               ELSE
                  Alert( [ERROR! Unblanced MP: '<*' : '] + sRule + "'" )
                  RETURN .F.
               ENDIF

            CASE SubStr( sRule, 1, 1 ) == '('
               cType := '('

               sRule := SubStr( sRule, 2 )
               ExtractLeadingWS( @sRule )

               nNext := At( ')', sRule )
               IF nNext > 1
                  sMarker := Left( sRule, nNext - 1 )
                  aAdd( aMarkers, RTrim( sMarker ) )

                  sRule := SubStr( sRule, nNext + 1 )
                  ExtractLeadingWS( @sRule )

                  IF Left( sRule, 1 ) == '>'
                     sRule := SubStr( sRule, 2 )
                     ExtractLeadingWS( @sRule )
                  ELSE
                     Alert( [ERROR! Unblanced MP: '<(' : '] + sRule + "'" )
                  ENDIF

                  aMatch := { nId, nOptional, sAnchor, cType, NIL }
                  //? aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5]
                  aAdd( aRule[2], aMatch )

                  /* Next dependant optional will be marked as trailing. */
                  IF nOptional > 0
                     nOptional := ( -nOptional )
                  ENDIF

                  sAnchor := NIL
                  LOOP
               ELSE
                  Alert( [ERROR! Unblanced MP: '<(' : '] + sRule + "'" )
                  RETURN .F.
               ENDIF

            CASE SubStr( sRule, 1, 1 ) == '!'
               cType := '!'

               sRule := SubStr( sRule, 2 )
               ExtractLeadingWS( @sRule )

               nNext := At( '!', sRule )
               IF nNext > 1
                  sMarker := Left( sRule, nNext - 1 )
                  aAdd( aMarkers, RTrim( sMarker ) )

                  sRule := SubStr( sRule, nNext + 1 )
                  ExtractLeadingWS( @sRule )

                  IF Left( sRule, 1 ) == '>'
                     sRule := SubStr( sRule, 2 )
                     ExtractLeadingWS( @sRule )
                  ELSE
                     Alert( [ERROR! Unblanced MP: '<!' : '] + sRule + "'" )
                  ENDIF

                  aMatch := { nId, nOptional, sAnchor, cType, NIL }
                  //? aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5]
                  aAdd( aRule[2], aMatch )

                  /* Next dependant optional will be marked as trailing. */
                  IF nOptional > 0
                     nOptional := ( -nOptional )
                  ENDIF

                  sAnchor := NIL
                  LOOP
               ELSE
                  Alert( [ERROR! Unblanced MP: '<!' : '] + sRule + "'" )
                  RETURN .F.
               ENDIF

            OTHERWISE
               cType := NIL // Reset - not known yet.
         ENDCASE

         nCloseAt := At( '>', sRule )
         nNext    := At( ',', sRule )

         IF nNext > 1 .AND. nNext < nCloseAt
            sDots := LTrim( SubStr( sRule, nNext + 1 ) )
            IF Left( sDots, 1 ) == '.'
               sDots := LTrim( SubStr( sDots, 2 ) )
               IF Left( sDots, 1 ) == '.'
                  sDots := LTrim( SubStr( sDots, 2 ) )
                  IF Left( sDots, 1 ) == '.'
                     sDots := LTrim( SubStr( sDots, 2 ) )
                     IF Left( sDots, 1 ) != '>'
                        nNext := 0
                     ENDIF
                  ELSE
                     nNext := 0
                  ENDIF
               ELSE
                  nNext := 0
               ENDIF
            ELSE
               nNext := 0
            ENDIF
         ENDIF

         IF nNext > 1 .AND. nNext < nCloseAt
            //? "Extended: '" + sRule + "'"
            cType := 'A'

            sMarker := Left( sRule, nNext - 1 )
            ExtractLeadingWS( @sMarker )
            aAdd( aMarkers, sMarker )
            sRule := sDots

            nNext    := 0
            nCloseAt := 1
         ELSE
            nNext := At( ':', sRule )
         ENDIF

         IF nNext > 0 .AND. nNext < nCloseAt
            cType := ':'

            //? "LIST"
            sMarker := Left( sRule, nNext - 1 )
            ExtractLeadingWS( @sMarker )
            aAdd( aMarkers, sMarker )

            sRule := SubStr( sRule, nNext + 1 )
            ExtractLeadingWS( @sRule )

            aWords := {}
            DO WHILE ! ( Left( sRule, 1 ) == '>' )
               nNext := At( ',', sRule )
               IF nNext > 0 .AND. nNext < At( '>', sRule )
                  //? "Added: " + Left( sRule, nNext - 1 )
                  aAdd( aWords, Upper( RTrim( Left( sRule, nNext - 1 ) ) ) )
                  sRule := SubStr( sRule, nNext + 1 )
                  ExtractLeadingWS( @sRule )
                  LOOP
               ELSE
                  nCloseAt := At( '>', sRule )
                  IF nCloseAt > 0
                     //? "Last: " + Left( sRule, nCloseAt - 1 )
                     aAdd( aWords, Upper( RTrim( Left( sRule, nCloseAt - 1 ) ) ) )
                     EXIT
                  ELSE
                     Alert( [ERROR! Unblanced MP: ''<:' at: ] + sRule )
                     RETURN .F.
                  ENDIF
               ENDIF
            ENDDO
         ENDIF

         IF nCloseAt > 0
            IF cType == NIL
               cType := '<'
            ENDIF

            IF Len( aMarkers ) < nId
               sMarker := Left( sRule, nCloseAt - 1 )
               ExtractLeadingWS( @sMarker )
               aAdd( aMarkers, sMarker )
            ENDIF

            sRule := SubStr( sRule, nCloseAt + 1 )
            ExtractLeadingWS( @sRule )

            aMatch := { nId, nOptional, sAnchor, cType, aWords }
            //? aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5]
            aAdd( aRule[2], aMatch )

            aWords := NIL // Reset.

            /* Next dependant optional will be marked as trailing. */
            IF nOptional > 0
               nOptional := ( -nOptional )
            ENDIF
         ELSE
            Alert( [ERROR! Unbalanced MP: '<' at: ] + sRule )
            RETURN .F.
         ENDIF

         sAnchor := NIL
         LOOP

      ELSEIF s1 == '['

         IF ! ( sAnchor == NIL )
            //TraceLog( "ORPHAN ANCHOR: " + sAnchor )

            aMatch := { 0, nOptional, sAnchor, NIL, NIL }
            //? aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5]
            aAdd( aRule[2], aMatch )

            // No need to negate nOptional, because we start a new optional group below...
         ENDIF

         nOptional := Abs( nOptional )
         nOptional++
         //? "Optional:", nOptional

         sRule := SubStr( sRule, 2 )
         ExtractLeadingWS( @sRule )

         sAnchor := NIL
         LOOP

      ELSEIF s1 == ']'

         IF ! ( sAnchor == NIL )
            //TraceLog( "ORPHAN ANCHOR: " + sAnchor )

            aMatch := { 0, nOptional, sAnchor, NIL, NIL }
            //? aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5]
            aAdd( aRule[2], aMatch )

            // No need to negate nOptional, because we close optional group below...
         ENDIF

         IF nOptional > 0
            nOptional--
            nOptional := (-nOptional)
         ELSE
            nOptional++
         ENDIF

         sRule := SubStr( sRule, 2 )
         ExtractLeadingWS( @sRule )

         sAnchor := NIL
         LOOP

      ELSE

         // Some token sneaked in ...
         TraceLog( "UnExpected Case: " + sRule + "[" + Str( ProcLine() ) + "]" )
         Alert( [UnExpected Case: ] + sRule + "[" + Str( ProcLine() ) + "]" )

         IF ! ( sAnchor == NIL )
            //TraceLog( "ORPHAN ANCHOR: " + sAnchor )

            aMatch := { 0, nOptional, sAnchor, NIL, NIL }
            //? aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5]
            aAdd( aRule[2], aMatch )

            /* Next dependant optional will be marked as trailing. */
            IF nOptional > 0
               nOptional := ( -nOptional )
            ENDIF
         ENDIF

         sAnchor := NextToken( @sRule )
      ENDIF
   ENDDO

   IF sAnchor != NIL
      aMatch := { 0, 0, sAnchor, NIL, NIL }
      //? aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5]
      aAdd( aRule[2], aMatch )

      // No need to negate nOptional, because last token, and nOptional must equal 0 here!
   ENDIF

   IF nOptional <> 0
      TraceLog( "ERROR Unclose Optional group, nOptional = " + Str( nOptional, 3 ), aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5] )
      Alert( [ERROR! Unclosed Optional group, nOptional = ] + Str( nOptional, 3 ) + " [" + Str( ProcLine(0), 4 ) + "]" )
      BREAK
   ENDIF

   aAdd( aRules, aRule )

   /*
   nMarkerID
   nOPTIONAL
   sAnchor
   cTYPE
   aLIST
   */

   // *** Processing STOP Words below, because processing RP may discover repeatable rooted by non optional marker and correct the root to optional!

   /*
   ? ''
   FOR Counter := 1 TO nId
      ?? aMarkers[Counter]
      IF Counter < nId
         ?? ' , '
      ENDIF
   NEXT

   nMatches := Len( aRule[2] )
   FOR Counter := 1 TO nMatches
      ? aRule[2][Counter][1], aRule[2][Counter][2], aRule[2][Counter][3], aRule[2][Counter][4], aRule[2][Counter][5]
   NEXT
   WAIT
   */

   /* --------------------------------------------------------------- */

   //? [RP: "] + sResult + '"'

   //TraceLog( sResult )

   nOptional     := 0
   aModifiers    := {}
   aValues       := Array( nId )
   nId           := 0
   sPad          := ''

   DO WHILE ! ( sResult == '' )
      nOffset := 0
      nOptionalAt := At( '[', sResult )
      WHILE nOPtionalAt > 1 .AND. SubStr( sResult, nOffset + nOptionalAt - 1, 1 ) == '\'
         nOffset += nOptionalAt
         nOptionalAt := At( '[', SubStr( sResult, nOffset + 1 ) )
      ENDDO
      IF nOptionalAt > 0
         nOptionalAt += nOffset
      ENDIF

      nOffset := 0
      IF nOptionalAt == 0
         nMarkerAt := At( '<', sResult )
         WHILE nMarkerAt > 0
            IF nMarkerAt > 1 .AND. SubStr( sResult, nOffset + nMarkerAt - 1, 1 ) == '\'
               nOffset   += nMarkerAt
               nMarkerAt := At( '<', SubStr( sResult, nOffset + 1 ) )
            ELSEIF nMarkerAt > 0 .AND. SubStr( sResult, nOffset + nMarkerAt + 1, 1 ) $ ">=" // ignore <= and <>
               nOffset   += nMarkerAt + 1
               nMarkerAt := At( '<', SubStr( sResult, nOffset + 1 ) )
            ELSE
               EXIT
            ENDIF
         ENDDO
         IF nMarkerAt > 0
            nMarkerAt += nOffset
         ENDIF
      ELSE
         nMarkerAt := At( '<', sResult )
         WHILE nMarkerAt > 0
            IF nMarkerAt > 1 .AND. nOffset + nMarkerAt < nOptionalAt .AND. SubStr( sResult, nOffset + nMarkerAt - 1, 1 ) == '\'
               nOffset   += nMarkerAt
               nMarkerAt := At( '<', SubStr( sResult, nOffset + 1 ) )
            ELSEIF nMarkerAt > 0 .AND. nOffset + nMarkerAt < nOptionalAt .AND. SubStr( sResult, nOffset + nMarkerAt + 1, 1 ) $ ">=" // ignore <= and <>
               nOffset   += nMarkerAt + 1
               nMarkerAt := At( '<', SubStr( sResult, nOffset + 1 ) )
            ELSE
               EXIT
            ENDIF
         ENDDO
         IF nMarkerAt > 0
            nMarkerAt += nOffset
         ENDIF

         IF nMarkerAt > 0
            IF nMarkerAt > nOptionalAt
               nMarkerAt := 0
            ELSE
               nOptionalAt := 0
            ENDIF
         ENDIF
      ENDIF

      nAt := IIF( nOptionalAt == 0, nMarkerAt, nOptionalAt )

      IF nOptional == 0
         nCloseOptionalAt := 0
      ELSE
         nOffset := 0
         IF nAt == 0
            nCloseOptionalAt := At( ']', sResult )
            WHILE nCloseOptionalAt > 1 .AND. SubStr( sResult, nOffset + nCloseOptionalAt - 1, 1 ) == '\'
               nOffset += nCloseOptionalAt
               nCloseOptionalAt := At( ']', SubStr( sResult, nOffset + 1 ) )
            ENDDO
            IF nCloseOptionalAt > 0
               nCloseOptionalAt += nOffset
            ENDIF
         ELSE
            nCloseOptionalAt := At( ']', sResult )
            WHILE nCloseOptionalAt > 1 .AND. nOffset + nCloseOptionalAt <= nAt .AND. SubStr( sResult, nOffset + nCloseOptionalAt - 1, 1 ) == '\'
               nOffset += nCloseOptionalAt
               nCloseOptionalAt := At( ']', SubStr( sResult, nOffset + 1 ) )
            ENDDO
            IF nCloseOptionalAt > 0
               nCloseOptionalAt += nOffset
            ENDIF

            IF nCloseOptionalAt > 0
               IF nCloseOptionalAt > nAt
                  nCloseOptionalAt := 0
               ELSE
                  nAt         := 0
                  nOptionalAt := 0
                  nMarkerAt   := 0
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      //? "RP Scan:", nAt, nMarkerAt, nOptionalAt, nCloseOptionalAt, sResult
      //WAIT

      IF nCloseOptionalAt > 0
         IF nCloseOptionalAt > 1
            sTemp := Left( sResult, nCloseOptionalAt - 1 )
            aRP := { nOptional, sPad + sTemp }
            aAdd( aResult, aRP )
            aAdd( aModifiers, -1 )
         ENDIF

         nOptional := 0 //--
         sResult := SubStr( sResult, nCloseOptionalAt + 1 )
         ExtractLeadingWS( @sResult, @sPad )
         LOOP
      ENDIF

      IF nOptionalAt > 0

         IF nOptional <> 0
            Alert( [ERROR! Nested repeatable RP.;] + sResult )
            BREAK
         ELSE
            nOptional := -1
         ENDIF

         /* The text preceding this new repeatable group. */
         IF nOptionalAt > 1
            sTemp := Left( sResult, nOptionalAt - 1 )
            aRP := { 0, sPad + sTemp }
            aAdd( aResult, aRP )
            aAdd( aModifiers, -1 )
         ELSE
            aRP := { 0, "" }
            aAdd( aResult, aRP )
            aAdd( aModifiers, -1 )
         ENDIF

         sResult := SubStr( sResult, nOptionalAt + 1 )
         ExtractLeadingWS( @sResult, @sPad )
         LOOP

      ELSEIF nMarkerAt > 0

         /* Resetting. */
         nType := 0

         IF nMarkerAt == 1 .OR. ( nMarkerAt == 2 .AND. Left( sResult, 1 ) == '#' )
            /* I consider this a Clipper bug - it produces .ppo without the padding if none suplied,
               but treats it as if padding existed! - so at least we will generate the space. */

            //IF /*Len( aResult ) > 0 .AND. ( ValType( aTail( aResult )[2] ) == 'N' .OR. aTail( aResult )[2] == "" ) .AND.*/ Len( sPad ) > 0
            IF bStrict
               aRP := { nOptional, " " }
               aAdd( aResult, aRP )
               aAdd( aModifiers, -1 )
            ELSEIF Len( sPad ) > 0
               aRP := { nOptional, sPad }
               aAdd( aResult, aRP )
               aAdd( aModifiers, -1 )
            ENDIF
         ENDIF

         IF nMarkerAt > 1
            sTemp := RTrim( Left( sResult, nMarkerAt - 1 ) )
            IF Right( sTemp, 1 ) == '#'
               nType := 2
               IF nMarkerAt > 2
                  sTemp := Left( sTemp, Len( sTemp ) - 1 )
                  aRP := { nOptional, sPad + DropExtraTrailingWS( sTemp ) }
                  aAdd( aResult, aRP )
                  aAdd( aModifiers, -1 )
               ENDIF
            ELSE
               sTemp := Left( sResult, nMarkerAt - 1 )
               aRP := { nOptional, sPad + DropExtraTrailingWS( sTemp ) }
               aAdd( aResult, aRP )
               aAdd( aModifiers, -1 )
            ENDIF
         ENDIF

         sResult := SubStr( sResult, nMarkerAt + 1 )
         ExtractLeadingWS( @sResult )

         /* <-x-> Ommit */
         IF SubStr( sResult, 1, 1 ) == '-'

            sResult := SubStr( sResult, 2 )
            ExtractLeadingWS( @sResult )

            nNext := At( ">", sResult )
            IF nNext == 0
               Alert( [ERROR! Unbalanced RP: '<-' : '] + sTemp + "'" )
            ELSE
               sTemp := RTrim( Left( sResult, nNext - 1 ) )
               IF Right( sTemp, 1 ) == '-'
                  nType := 0
                  sTemp := RTrim( Left( sTemp, Len( sTemp ) - 1 ) )
                  nId := aScan( aMarkers, sTemp )
                  sResult := SubStr( sResult, nNext + 1 )
                  ExtractLeadingWS( @sResult, @sPad )
                  IF nId == 0
                     Alert( [ERROR! Unrecognized RP: '<-' : '] + sTemp + "'" )
                  ELSE
                     aRP := { nOptional, nId }
                     aAdd( aResult, aRP )
                     aAdd( aModifiers, nType )
                  ENDIF
               ENDIF
            ENDIF

         /* #<x> Dumb */
         ELSEIF nType == 2

            nNext := At( '>', sResult )
            IF nNext == 0
               Alert( [ERROR! Unbalanced RP: '#<' ] )
            ELSE
               /*nType := 2*/
               sTemp := RTrim( Left( sResult, nNext - 1 ) )
               nId := aScan( aMarkers, sTemp )
               sResult := SubStr( sResult, nNext + 1 )
               ExtractLeadingWS( @sResult, @sPad )
               IF nId == 0
                  Alert( [ERROR! Unrecognized RP: '#<' : ] + sTemp )
               ELSE
                  aRP := { nOptional, nId }
                  aAdd( aResult, aRP )
                  aAdd( aModifiers, nType )
               ENDIF
            ENDIF

         /* <"x"> Normal */
         ELSEIF SubStr( sResult, 1, 1 ) == '"'

            sResult := SubStr( sResult, 2 )
            ExtractLeadingWS( @sResult )

            nNext := At( ">", sResult )
            IF nNext == 0
               Alert( [ERROR! Unbalanced RP: '<"' : '] + sTemp + "'" )
            ELSE
               sTemp := RTrim( Left( sResult, nNext - 1 ) )
               IF Right( sTemp, 1 ) == '"'
                  nType := 3
                  sTemp := RTrim( Left( sTemp, Len( sTemp ) - 1 ) )
                  nId := aScan( aMarkers, sTemp )
                  sResult := SubStr( sResult, nNext + 1 )
                  ExtractLeadingWS( @sResult, @sPad )
                  IF nId == 0
                     Alert( [ERROR! Unrecognized RP: '<"' : '] + sTemp + "'" )
                  ELSE
                     aRP := { nOptional, nId }
                     aAdd( aResult, aRP )
                     aAdd( aModifiers, nType )
                  ENDIF
               ENDIF
            ENDIF

         /* <(x)> Smart */
         ELSEIF SubStr( sResult, 1, 1 ) == '('

            sResult := SubStr( sResult, 2 )
            ExtractLeadingWS( @sResult )

            nNext := At( ">", sResult )
            IF nNext == 0
               Alert( [ERROR! Unbalanced RP: '<(' : '] + sTemp + "'" )
            ELSE
               sTemp := RTrim( Left( sResult, nNext - 1 ) )
               IF Right( sTemp, 1 ) == ')'
                  nType := 4
                  sTemp := RTrim( Left( sTemp, Len( sTemp ) - 1 ) )
                  nId := aScan( aMarkers, sTemp )
                  sResult := SubStr( sResult, nNext + 1 )
                  ExtractLeadingWS( @sResult, @sPad )
                  IF nId == 0
                     Alert( [ERROR! Unrecognized RP: '<(' : '] + sTemp + "'" )
                  ELSE
                     aRP := { nOptional, nId }
                     aAdd( aResult, aRP )
                     aAdd( aModifiers, nType )
                  ENDIF
               ENDIF
            ENDIF

         /* <{x}> Blockify */
         ELSEIF SubStr( sResult, 1, 1 ) == '{'

            sResult := SubStr( sResult, 2 )
            ExtractLeadingWS( @sResult )

            nNext := At( ">", sResult )
            IF nNext == 0
               Alert( [ERROR! Unbalanced RP: '<{' : '] + sTemp + "'" )
            ELSE
               sTemp := RTrim( Left( sResult, nNext - 1 ) )
               IF Right( sTemp, 1 ) == '}'
                  nType := 5
                  sTemp := RTrim( Left( sTemp, Len( sTemp ) - 1 ) )
                  nId := aScan( aMarkers, sTemp )
                  sResult := SubStr( sResult, nNext + 1 )
                  ExtractLeadingWS( @sResult, @sPad )
                  IF nId == 0
                     Alert( [ERROR! Unrecognized RP: '<{' : '] + sTemp + "'" )
                  ELSE
                     aRP := { nOptional, nId }
                     aAdd( aResult, aRP )
                     aAdd( aModifiers, nType )
                  ENDIF
               ENDIF
            ENDIF

         /* <.x.> Logify */
         ELSEIF SubStr( sResult, 1, 1 ) == '.'

            sResult := SubStr( sResult, 2 )
            ExtractLeadingWS( @sResult )

            nNext := At( ">", sResult )
            IF nNext == 0
               Alert( [ERROR! Unbalanced RP: '<.' : '] + sTemp + "'" )
            ELSE
               sTemp := RTrim( Left( sResult, nNext - 1 ) )
               IF Right( sTemp, 1 ) == '.'
                  nType := 6
                  sTemp := RTrim( Left( sTemp, Len( sTemp ) - 1 ) )
                  nId := aScan( aMarkers, sTemp )
                  sResult := SubStr( sResult, nNext + 1 )
                  ExtractLeadingWS( @sResult, @sPad )
                  IF nId == 0
                     Alert( [ERROR! Unrecognized RP: '<.' : '] + sTemp + "'" )
                  ELSE
                     aRP := { nOptional, nId }
                     aAdd( aResult, aRP )
                     aAdd( aModifiers, nType )
                  ENDIF
               ENDIF
            ENDIF

         ELSE

            nNext := At( '>', sResult )
            IF nNext == 0
               Alert( [ERROR! Unbalanced RP: <] )
            ELSE
               /* <x> Regular */
               nType := 1
               sTemp := Left( sResult, nNext - 1 )
               nId := aScan( aMarkers, sTemp )
               sResult := SubStr( sResult, nNext + 1 )
               ExtractLeadingWS( @sResult, @sPad )
               IF nId == 0
                  aEval( aMarkers, {|sMarker| TraceLog( sTemp, sMarker ) } )
                  Alert( [ERROR! Unrecognized RP: '<']+" : '" + sTemp + "'" )
               ELSE
                  aRP := { nOptional, nId }
                  aAdd( aResult, aRP )
                  aAdd( aModifiers, nType )
               ENDIF
            ENDIF

         ENDIF

         //? "RP #", nID, "Optional:", nOptional

         IF nOptional < 0
            nOptional := nID
            aRP[1]    := nOptional
         ENDIF

      ELSE

         aRP := { 0, sPad + sResult }
         aAdd( aResult, aRP )
         aAdd( aModifiers, -1 )
         sResult := ''

      ENDIF

   ENDDO

   IF nOptional <> 0
      TraceLog( "ERROR! Internal logic failure, nOptional = " + Str( nOptional, 3 ) + " [" + Str( ProcLine(0), 4 ) + "]", aRP[1], aRP[2] )
      Alert( [ERROR! Internal logic failure, nOptional = ] + Str( nOptional, 3 ) + " [" + Str( ProcLine(0), 4 ) + "]" )
      BREAK
   ENDIF

#ifdef POSSIBLE_WORK_IN_PROGRESS
   nResults := Len( aResult )
   FOR Counter := nResults TO 1 STEP -1

      /* Correcting the ID of the Marker this result depends upon. */
      IF aResult[Counter][1] > 0
         nOptional := aResult[Counter][1]
         nMarker   := aResult[Counter][2]
      ELSEIF aResult[Counter][1] < 0
         aResult[Counter][1] := nOptional
      ENDIF

      IF ValType( aResult[Counter][2] ) == 'C'
         aResult[Counter][2] := StrTran( aResult[Counter][2], '\', '' )
         //? "RP #", Counter, aResult[Counter][1], '"' + aResult[Counter][2] + '"'
      ELSE
         /* Marking the respective Match Marker as Repeatable, if it is OPTIONAL. */
         IF nOptional > 0
            aEval( aRule[2], { |aMP| IIF( aMP[1] == nMarker .AND. aMP[2] <> 0, aMP[1] += 1000, ) } )
         ENDIF

         //? "RP #", Counter, aResult[Counter][1], aResult[Counter][2]
      ENDIF
   NEXT
#endif

   nResults := Len( aResult )
   FOR Counter := nResults TO 1 STEP -1

      /* Correcting the ID of the Marker this result depends upon. */
      IF aResult[Counter][1] > 0
         nOptional := aResult[Counter][1]
         nMarker   := aResult[Counter][2]

         //? "Repeatable: ", nMarker, "Root: ", nOptional

         IF ValType( nMarker ) == 'N'
            nMP := aScan( aRule[2], {|aMP| aMP[1] == nMarker .OR. aMP[1] - 1000 == nMarker } )
            IF nMP == 0
               Alert( [ERROR! Internal logic failed! Missing marker # ] + str( nMarker, 2 ) + " [" + Str(ProcLine(),4 ) + ']' )
            ELSE
               WHILE aRule[2][nMP][2] < 0
                  IF aRule[2][nMP][1] >= 0

                     /* Mark as Repeatable. */
                     IF aRule[2][nMP][1] < 1000
                        aRule[2][nMP][1] += ( 1000 )
                        //? "Flagged:", nMP, "As:", aRule[2][nMP][1]
                     ENDIF
                  ENDIF

                  nMP--
               ENDDO
               IF aRule[2][nMP][2] == 0
                  TraceLog( "Warning - Result #" + Str( Counter ) + " marked REPEATABLE but root #" + Str( nMarker ) + " is not OPTIONAL!", sRuleCopy )
                  //aRule[2][nMP][2] := 1
               ELSEIF aRule[2][nMP][1] < 1000
                  aRule[2][nMP][1] += ( 1000 )
                  //? "Flagged:", nMP, "As:", aRule[2][nMP][1]
               ENDIF
            ENDIF
            //WAIT
         ENDIF
      ELSEIF aResult[Counter][1] < 0
         aResult[Counter][1] := nOptional
      ENDIF

      IF ValType( aResult[Counter][2] ) == 'C'
         aResult[Counter][2] := StrTran( aResult[Counter][2], '\', '' )
         //? "RP #", Counter, aResult[Counter][1], '"' + aResult[Counter][2] + '"'
      ELSE
         //? "RP #", Counter, aResult[Counter][1], aResult[Counter][2]
      ENDIF
   NEXT

   //WAIT

   // Processing STOP words for NON Anchored optionals.
   nLastOptional := 0
   nPending      := 0
   nMatches      := Len( aRule[2] )

   FOR Counter := 1 TO nMatches
      aMatch := aRule[2][Counter]

      /* Optional group start (marker), no anchor, and not a restricted pattern - have to build stop words list! */
      IF aMatch[1] > 0 .AND. aMatch[2] > 0 .AND. aMatch[3] == NIL .AND. aMatch[4] != ':'

         aWords        := {}
         nOptional     := aMatch[2]
         nLastOptional :=  nOptional

         /*
         nMP := Counter - 1
         WHILE nMP > 0
            aMatch := aRule[2][nMP]
            IF aMatch[2] >= 0 .AND. aMatch[2] < nOptional
               EXIT
            ENDIF
            IF aMatch[2] > 0 .AND. aMatch[2] == nOptional
               IF aMatch[3] != NIL
                  aAdd( aWords, Upper( aMatch[3] ) )
               ELSEIF aMatch[4] == ':'
                  nWords := Len( aMatch[5] )
                  FOR nWord := 1 TO nWords
                     aAdd( aWords, aMatch[5][nWord] )
                  NEXT
               ENDIF
            ENDIF
            nMP--
         ENDDO
         */

         nMP := Counter + 1
         WHILE nMP <= nMatches
            aMatch := aRule[2][nMP]
            IF aMatch[2] >= 0 .AND. aMatch[2] < nOptional
               IF aMatch[3] != NIL
                  aAdd( aWords, Upper( aMatch[3] ) )
               ELSEIF aMatch[4] == ':'
                  nWords := Len( aMatch[5] )
                  FOR nWord := 1 TO nWords
                     aAdd( aWords, aMatch[5][nWord] )
                  NEXT
               ENDIF
               EXIT
            ENDIF
            IF aMatch[2] > 0 .AND. aMatch[2] == nOptional
               IF aMatch[3] != NIL
                  aAdd( aWords, Upper( aMatch[3] ) )
               ELSEIF aMatch[4] == ':'
                  nWords := Len( aMatch[5] )
                  FOR nWord := 1 TO nWords
                     aAdd( aWords, aMatch[5][nWord] )
                  NEXT
               ENDIF
            ENDIF
            nMP++
         ENDDO

         IF Len( aWords ) > 0
            aRule[2][Counter][5] := aWords
         ENDIF

      ELSEIF aMatch[2] > 0 .AND. aMatch[2] - nLastOptional == 2 // Head of new group missing because nested optional is first element.
         nPending := - ( aMatch[2] - 1 )

      ELSEIF nPending != 0 .AND. aMatch[1] > 0 .AND. aMatch[2] == nPending .AND. aMatch[3] == NIL .AND. aMatch[4] != ':'
         nPending      := 0
         aWords        := {}
         nOptional     := -aMatch[2]
         nLastOptional :=  nOptional

         nMP := Counter - 1
         WHILE nMP > 0
            aMatch := aRule[2][nMP]
            IF aMatch[2] >= 0 .AND. aMatch[2] < nOptional
               EXIT
            ENDIF
            IF aMatch[2] > 0 .AND. aMatch[2] == nOptional
               IF aMatch[3] != NIL
                  aAdd( aWords, Upper( aMatch[3] ) )
               ELSEIF aMatch[4] == ':'
                  nWords := Len( aMatch[5] )
                  FOR nWord := 1 TO nWords
                     aAdd( aWords, aMatch[5][nWord] )
                  NEXT
               ENDIF
            ENDIF
            nMP--
         ENDDO

         nMP := Counter + 1
         WHILE nMP <= nMatches
            aMatch := aRule[2][nMP]
            IF aMatch[2] >= 0 .AND. aMatch[2] < nOptional
               IF aMatch[3] != NIL
                  aAdd( aWords, Upper( aMatch[3] ) )
               ELSEIF aMatch[4] == ':'
                  nWords := Len( aMatch[5] )
                  FOR nWord := 1 TO nWords
                     aAdd( aWords, aMatch[5][nWord] )
                  NEXT
               ENDIF
               EXIT
            ENDIF
            IF aMatch[2] > 0 .AND. aMatch[2] == nOptional
               IF aMatch[3] != NIL
                  aAdd( aWords, Upper( aMatch[3] ) )
               ELSEIF aMatch[4] == ':'
                  nWords := Len( aMatch[5] )
                  FOR nWord := 1 TO nWords
                     aAdd( aWords, aMatch[5][nWord] )
                  NEXT
               ENDIF
            ENDIF
            nMP++
         ENDDO

         IF Len( aWords ) > 0
            aRule[2][Counter][5] := aWords
         ENDIF

      ENDIF

      IF aMatch[3] != NIL
         aMatch[3] := StrTran( aMatch[3], '\', '' )
      ENDIF

      //? aRule[1], aRule[2][Counter][1], aRule[2][Counter][2], aRule[2][Counter][3], aRule[2][Counter][4], aRule[2][Counter][5]
   NEXT
   //WAIT

   aAdd( aResults, { aResult, aModifiers, aValues } )

   //TraceLog( "Finished" )

RETURN NIL

//--------------------------------------------------------------//

STATIC FUNCTION RemoveDefine( sDefine )

   LOCAL nId, nLen

   sDefine := AllTrim( sDefine )

   IF ( nId := aScan( aDefRules, {|aDefine| aDefine[1] == sDefine } ) ) > 0
      aDel( aDefRules, nId )
      aSize( aDefRules, ( nLen := Len( aDefRules ) - 1 ) )
      aDel( aDefResults, nId )
      aSize( aDefResults, nLen )
   ENDIF

RETURN nId

//--------------------------------------------------------------//

STATIC FUNCTION CompileDefine( sRule )

   LOCAL sKey, sResult, aRule, nCloseAt, nId, sMarker, nCommaAt, aMP
   LOCAL sToken, aRPs, sAnchor, aMarkers := {}, aResult

   ExtractLeadingWS( @sRule )

   sKey := NextToken( @sRule )
   DropTrailingWS( @sKey )

//? "KEY: '" + sKey + "'"
//? "Rest: '" + sRule + "'"

   IF ( nId := aScan( aDefRules, {|aDefine| aDefine[1] == sKey } ) ) > 0
      Alert( [Redefinition of ] + "'" + sKey + "'" + [ in file: ] + s_sFile )
      aRule      := aDefRules[nId]
      //aRule[1]   := sKey
      aRule[2]   := {}
      aResult    := aDefResults[nId]
      aResult[1] := {} // aMPs
      aResult[2] := {} // aModifiers
      aResult[3] := {} // Markers place holders
   ELSE
      aRule   := { sKey, {}, .T. }
      aAdd( aDefRules, aRule )
      aResult := { {}, {}, {} } //1=MPs, 2=Modifiers 3=Marker place holders
      aAdd( aDefResults, aResult )
   ENDIF

   IF Left( sRule, 1 ) == '(' .AND. ( nCloseAt := At( ')', sRule ) ) > 0

      /*Pseudo Function. */
      sResult := SubStr( sRule, nCloseAt + 1 )
      sRule   := SubStr( sRule, 2, nCloseAt - 2 )
      ExtractLeadingWS( @sRule )
      DropTrailingWS( @sRule )
      ExtractLeadingWS( @sResult )

      /* No paramaets */
      IF sRule == ''

//? "Added: '" + aRule[1] + "'"
//WAIT
         aAdd( aRule[2], { 0, 0 , '(', NIL, NIL } )
         aAdd( aRule[2], { 0, 0 , ')', NIL, NIL } )

         IF sResult == ''
            aResult[1] := NIL
            aResult[2] := NIL
            aResult[3] := NIL
         ELSE
            aResult[1] := { { 0, sResult } }
         ENDIF
      ELSE

//? "***'" + sRule + "'"
//WAIT
         nId      := 1
         sAnchor  := '('

         WHILE ( nCommaAt := At( ',', sRule ) ) > 0
            sMarker := Left( sRule, nCommaAt - 1 )
            sRule   := SubStr( sRule, nCommaAt + 1 )
            ExtractLeadingWS( @sRule )
            DropTrailingWS( @sMarker )

//? nId, "Marker: '" + sMarker + "'"
//WAIT
            aAdd( aMarkers, sMarker )
            aMP := { nId, 0, sAnchor, '<', NIL }
            aAdd( aRule[2], aMP )

            sAnchor := ','
            nId++
         ENDDO

         aAdd( aMarkers, sRule )
         aMP := { nId, 0, sAnchor, '<', NIL }
         aAdd( aRule[2], aMP )

         aMP := { 0, 0, ')', NIL, NIL }
         aAdd( aRule[2], aMP )

         /*----------------------------------------- */

         aRPs := {}

         IF sResult == ''

            aResult[1] := NIL
            aResult[2] := NIL
            aResult[3] := NIL

         ELSE

            WHILE ( sToken := NextToken( @sResult ) ) != NIL

               DropTrailingWS( @sToken )

//? "Token: '" + sToken + "'"

               IF ( nId := aScan( aMarkers, {|sMarker| sMarker == sToken } ) ) > 0
                  aAdd( aRPs, { 0, nId } )
                  aAdd( aResult[2], 1 )
               ELSE
                  aAdd( aRPs, { 0, sToken } )
                  aAdd( aResult[2], -1 )
               ENDIF

//? "ID:", nID
//WAIT

            ENDDO

            aResult[1] := aRPs
            aSize( aResult[3], Len( aMarkers ) )
            aFill( aResult[3], NIL )

         ENDIF

      ENDIF

   ELSE

      /* Plain. */

      sResult := sRule

      IF sResult == ''
         aResult[1] := NIL
         aResult[2] := NIL
         aResult[3] := NIL
      ELSE
         aResult[1] := { { 0, sResult } }
         aResult[2] := { -1 }
         aResult[3] := NIL
      ENDIF

   ENDIF

//? "Defines: ", Len( aDefRules )
//? "Results: ", Len( aDefResults )
//WAIT

RETURN Len( aDefRules )

//--------------------------------------------------------------//

#ifndef USE_C_BOOST

FUNCTION ExtractLeadingWS( sLine, sWS )

   LOCAL Counter, cChar, nLen := Len( sLine )

   //? "Removing Leading: '" + sLine + "'"

   sWS := ''
   FOR Counter := 1 TO nLen
      cChar := SubStr( sLine, Counter, 1 )
      IF cChar == ' ' //$ ( ' ' + Chr(9) ) // Tabs converted to spaces
         sWS += cChar
      ELSE
         EXIT
      ENDIF
   NEXT

   IF Counter > 1
      sLine := SubStr( sLine, Counter )
   ENDIF

   //? "Removed: '" + sWs + "' sLine: " + sLine

RETURN sWS

#endif

//--------------------------------------------------------------//

#ifndef USE_C_BOOST

FUNCTION DropTrailingWS( sLine, sWS )

   LOCAL nLenSource, nLen := Len( sLine ), cChar

   nLenSource := nLen

   //? "Before Drop: '" + sLine + "'"

   /* Tabs are converted to spaces at PP_PreProFile() */

   WHILE nLen > 0 .AND. ( cChar := SubStr( sLine, nLen, 1 ) ) == ' ' //$ ( ' ' + Chr(9) ) // Tabs converted to spaces
      nLen--
   ENDDO

   sLine := Left( sLine, nLen )
   sWS   := Space( nLenSource - nLen )

   //? "After Drop: '" + sLine + "'"

RETURN sLine

#endif

//--------------------------------------------------------------//

#ifndef USE_C_BOOST

FUNCTION DropExtraTrailingWS( sLine )

   LOCAL nLen := Len( sLine )
   /* Tabs are converted to spaces at PP_PreProFile() */

   //? "Before Extra: '" + sLine + "'"

   WHILE nLen > 2 .AND. ( SubStr( sLine, nLen, 1 ) == ' ' /* $ ( ' ' + Chr(9) ) */ ) .AND. ;
                        ( SubStr( sLine, nLen - 1, 1 ) == ' ' ) //$ ( ' ' + Chr(9) ) )
      nLen--
   ENDDO

   sLine := Left( sLine, nLen )

RETURN sLine

#endif

//--------------------------------------------------------------//

STATIC FUNCTION SetIfDef( sDefine, bExist )

   LOCAL nId

   nIfDef++
   aSize( abIfDef, nIfDef )

   DropTrailingWS( @sDefine )

   nId := aScan( aDefRules, {|aDefine| aDefine[1] == sDefine } )
   IF bExist
      abIfDef[nIfDef] := ( nId > 0 )
   ELSE
      abIfDef[nIfDef] := ( nId == 0 )
   ENDIF

   //? nIfDef, nId, sDefine, abIfDef[nIfDef]

RETURN nIfDef

//--------------------------------------------------------------//

STATIC FUNCTION CompileToCCH( sSource )

   LOCAL hCCH, Counter, aRules, nRules, nRule, aRule, nMatches, nMatch, aMatch, nWords, nWord, aWords
   LOCAL aResults, nResults, nResult, aResult, nRPs, nRP, aRP, nIDs, nID, nModifier
   LOCAL sRulesArray, sResultsArray, sExt

   sExt := SubStr( sSource, RAt( '.', sSource ) )
   IF ! ( sExt == '' )
      hCCH   := FCreate( StrTran( sSource, sExt, ".cch" ) )
   ELSE
      hCCH   := FCreate( sSource + ".cch" )
   ENDIF

   FWrite( hCCH, "FUNCTION InitRules()" + CRLF )

   FOR Counter := 1 TO 3
      IF Counter == 1
         aRules      := aDefRules
         sRulesArray := "aDefRules"
         FWrite( hCCH, CRLF + "/* Defines */" + CRLF + "aDefRules := {}" + CRLF )
      ELSEIF Counter == 2
         aRules      := aTransRules
         sRulesArray := "aTransRules"
         FWrite( hCCH, CRLF + "/* Translates */" + CRLF + "aTransRules := {}" + CRLF )
      ELSE
         aRules      := aCommRules
         sRulesArray := "aCommRules"
         FWrite( hCCH, CRLF + "/* Commands */" + CRLF + "aCommRules := {}" + CRLF )
      ENDIF

      nRules := Len( aRules )

      FOR nRule := 1 TO nRules
         aRule  := aRules[nRule]

         FWrite( hCCH, "aAdd( " + sRulesArray + ", { '" + aRule[1] + "' " )

         IF aRule[2] == NIL
            nMatches := 0
         ELSE
            nMatches := Len( aRule[2] )
         ENDIF

         IF nMatches == 0
            FWrite( hCCH, ", " )
         ELSE
            FWrite( hCCH, ", { " )
            FOR nMatch := 1 TO nMatches
               aMatch := aRule[2][nMatch] //{ nId, nOptional, sAnchor, cType, aWords }
               FWrite( hCCH, "{ " + Str( aMatch[1], 4) + ", " + Str(aMatch[2],3) + ", " + IF( aMatch[3] == NIL, "NIL", "'" + aMatch[3] + "'" ) + ", " + IF( aMatch[4] == NIL, "NIL", "'" + aMatch[4] + "'" ) + ", " )
               IF aMatch[5] == NIL
                  FWrite( hCCH, "NIL" )
               ELSE
                  aWords := aMatch[5]
                  nWords := Len( aWords )
                  FWrite( hCCH, "{ " )
                  FOR nWord := 1 TO nWords
                     FWrite( hCCH, "'" + aWords[nWord] + "'" )
                     IF nWord < nWords
                        FWrite( hCCH, ", " )
                     ENDIF
                  NEXT
                  FWrite( hCCH, " }" )
               ENDIF
               FWrite( hCCH, " }" )

               IF nMatch < nMatches
                  FWrite( hCCH, ", " )
               ENDIF
            NEXT

            FWrite( hCCH, " }" )
         ENDIF

         IF aRule[3]
            FWrite( hCCH, " , .T." )
         ELSE
            FWrite( hCCH, " , .F." )
         ENDIF

         FWrite( hCCH, " } )" + CRLF )
      NEXT
   NEXT

   FWrite( hCCH, CRLF + "RETURN .T." + CRLF )

   FWrite( hCCH, CRLF + "FUNCTION InitResults()" + CRLF )

   FOR Counter := 1 TO 3

      IF Counter == 1
         aResults      := aDefResults
         sResultsArray := "aDefResults"
         FWrite( hCCH, CRLF + "/* Defines Results*/" + CRLF + "aDefResults := {}" + CRLF )
      ELSEIF Counter == 2
         aResults      := aTransResults
         sResultsArray := "aTransResults"
         FWrite( hCCH, CRLF + "/* Translates Results*/" + CRLF + "aTransResults := {}" + CRLF )
      ELSE
         aResults      := aCommResults
         sResultsArray := "aCommResults"
         FWrite( hCCH, CRLF + "/* Commands Results*/" + CRLF + "aCommResults := {}" + CRLF )
      ENDIF

      nResults := Len( aResults )

      FOR nResult := 1 TO nResults
         aResult  := aResults[nResult]

         FWrite( hCCH, "aAdd( " + sResultsArray + ", { " )

         IF aResult[1] == NIL
            nRPs := 0
         ELSE
            nRPs := Len( aResult[1] )
         ENDIF
         IF aResult[3] == NIL
            nIDs := 0
         ELSE
            nIDs := Len( aResult[3] )
         ENDIF

         IF nRPs == 0
            /*FWrite( hCCH, "NIL " )*/
         ELSE
            FWrite( hCCH, "{ " )
            FOR nRP := 1 TO nRPs
               aRP := aResult[1][nRP] //{ nLevel, xVal }

               FWrite( hCCH, "{ " + Str( aRP[1], 3) + ", " )
               IF ValType( aRP[2] ) == 'C'
                  FWrite( hCCH, "'" + aRP[2] + "' }" )
               ELSE
                  FWrite( hCCH, Str( aRP[2], 3 ) + " }" )
               ENDIF

               IF nRP < nRPs
                  FWrite( hCCH, ", " )
               ENDIF
            NEXT
            FWrite( hCCH, " }" )
         ENDIF

         IF nRPs == 0
            FWrite( hCCH, ", " )
         ELSE
            FWrite( hCCH, ", { " )
            FOR nModifier := 1 TO nRPs
               FWrite( hCCH, Str( aResult[2][nModifier], 2 ) )
               IF nModifier < nRPs
                  FWrite( hCCH, ", " )
               ENDIF
            NEXT
            FWrite( hCCH, "} " )
         ENDIF

         IF nIDs == 0
            FWrite( hCCH, ", " )
         ELSE
            FWrite( hCCH, ", { " )
            FOR nID := 1 TO nIDs
               FWrite( hCCH, "NIL" )
               IF nID < nIDs
                  FWrite( hCCH, ", " )
               ENDIF
            NEXT
            FWrite( hCCH, " } " )
         ENDIF
         FWrite( hCCH, " } )" + CRLF )
      NEXT

   NEXT

   FWrite( hCCH, CRLF + "RETURN .T. " + CRLF )

   FClose( hCCH )

RETURN .T.

//--------------------------------------------------------------//

STATIC FUNCTION InitRules()

  /* Defines */
  aDefRules := {}
  aAdd( aDefRules, { '_SET_EXACT' ,  , .T. } )
  aAdd( aDefRules, { '_SET_FIXED' ,  , .T. } )
  aAdd( aDefRules, { '_SET_DECIMALS' ,  , .T. } )
  aAdd( aDefRules, { '_SET_DATEFORMAT' ,  , .T. } )
  aAdd( aDefRules, { '_SET_EPOCH' ,  , .T. } )
  aAdd( aDefRules, { '_SET_PATH' ,  , .T. } )
  aAdd( aDefRules, { '_SET_DEFAULT' ,  , .T. } )
  aAdd( aDefRules, { '_SET_EXCLUSIVE' ,  , .T. } )
  aAdd( aDefRules, { '_SET_SOFTSEEK' ,  , .T. } )
  aAdd( aDefRules, { '_SET_UNIQUE' ,  , .T. } )
  aAdd( aDefRules, { '_SET_DELETED' ,  , .T. } )
  aAdd( aDefRules, { '_SET_CANCEL' ,  , .T. } )
  aAdd( aDefRules, { '_SET_DEBUG' ,  , .T. } )
  aAdd( aDefRules, { '_SET_TYPEAHEAD' ,  , .T. } )
  aAdd( aDefRules, { '_SET_COLOR' ,  , .T. } )
  aAdd( aDefRules, { '_SET_CURSOR' ,  , .T. } )
  aAdd( aDefRules, { '_SET_CONSOLE' ,  , .T. } )
  aAdd( aDefRules, { '_SET_ALTERNATE' ,  , .T. } )
  aAdd( aDefRules, { '_SET_ALTFILE' ,  , .T. } )
  aAdd( aDefRules, { '_SET_DEVICE' ,  , .T. } )
  aAdd( aDefRules, { '_SET_EXTRA' ,  , .T. } )
  aAdd( aDefRules, { '_SET_EXTRAFILE' ,  , .T. } )
  aAdd( aDefRules, { '_SET_PRINTER' ,  , .T. } )
  aAdd( aDefRules, { '_SET_PRINTFILE' ,  , .T. } )
  aAdd( aDefRules, { '_SET_MARGIN' ,  , .T. } )
  aAdd( aDefRules, { '_SET_BELL' ,  , .T. } )
  aAdd( aDefRules, { '_SET_CONFIRM' ,  , .T. } )
  aAdd( aDefRules, { '_SET_ESCAPE' ,  , .T. } )
  aAdd( aDefRules, { '_SET_INSERT' ,  , .T. } )
  aAdd( aDefRules, { '_SET_EXIT' ,  , .T. } )
  aAdd( aDefRules, { '_SET_INTENSITY' ,  , .T. } )
  aAdd( aDefRules, { '_SET_SCOREBOARD' ,  , .T. } )
  aAdd( aDefRules, { '_SET_DELIMITERS' ,  , .T. } )
  aAdd( aDefRules, { '_SET_DELIMCHARS' ,  , .T. } )
  aAdd( aDefRules, { '_SET_WRAP' ,  , .T. } )
  aAdd( aDefRules, { '_SET_MESSAGE' ,  , .T. } )
  aAdd( aDefRules, { '_SET_MCENTER' ,  , .T. } )
  aAdd( aDefRules, { '_SET_SCROLLBREAK' ,  , .T. } )
  aAdd( aDefRules, { '_SET_COUNT' ,  , .T. } )
  aAdd( aDefRules, { '_SET_CH' ,  , .T. } )
  aAdd( aDefRules, { '_DFSET' , { {    1,   0, '(', '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )

  /* Translates */
  aTransRules := {}

  /* Commands */
  aCommRules := {}
  aAdd( aCommRules, { 'DO' , { {    1,   0, 'WHILE', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'END' , { {    1,   0, NIL, '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'END' , { {    0,   0, 'SEQUENCE', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'ENDSEQUENCE' ,  , .F. } )
  aAdd( aCommRules, { 'ENDDO' , { {    1,   0, NIL, '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'ENDIF' , { {    1,   0, NIL, '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'ENDCASE' , { {    1,   0, NIL, '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'ENDFOR' , { {    1,   1, NIL, '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'NEXT' , { {    1,   0, NIL, '<', NIL }, {    2,   1, 'TO', '<', NIL }, {    3,   1, 'STEP', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'DO' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '.', NIL, NIL }, {    0,   0, 'PRG', NIL, NIL }, { 1002,   1, 'WITH', 'A', NIL } } , .F. } )
  aAdd( aCommRules, { 'CALL' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, {    0,   0, ')', NIL, NIL }, { 1002,   1, 'WITH', 'A', NIL } } , .F. } )
  aAdd( aCommRules, { 'STORE' , { {    1,   0, NIL, '<', NIL }, {    2,   0, 'TO', '<', NIL }, { 1003,   1, ',', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'ECHO', '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'HEADING', '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'MENU', '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'STATUS', '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'STEP', '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'SAFETY', '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'TALK', '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'PROCEDURE', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'PROCEDURE', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'EXACT', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'EXACT', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'FIXED', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'FIXED', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DECIMALS', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DECIMALS', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'PATH', NIL, NIL }, {    1,   0, 'TO', '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'PATH', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DEFAULT', NIL, NIL }, {    1,   0, 'TO', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DEFAULT', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'CENTURY', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'CENTURY', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'EPOCH', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   0, 'FORMAT', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    1,   0, NIL, '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'AMERICAN', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'ANSI', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'BRITISH', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'FRENCH', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'GERMAN', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'ITALIAN', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'JAPANESE', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'USA', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'ALTERNATE', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'ALTERNATE', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'ALTERNATE', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'ALTERNATE', NIL, NIL }, {    1,   0, 'TO', '(', NIL }, {    2,   1, NIL, ':', { 'ADDITIVE' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'CONSOLE', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'CONSOLE', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'MARGIN', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'MARGIN', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'PRINTER', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'PRINTER', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'PRINTER', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'PRINTER', NIL, NIL }, {    1,   0, 'TO', '(', NIL }, {    2,   1, NIL, ':', { 'ADDITIVE' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DEVICE', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, {    0,   0, 'SCREEN', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DEVICE', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, {    0,   0, 'PRINTER', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'COLOR', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, {    1,   1, NIL, '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'COLOR', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'COLOUR', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, { 1001,   1, NIL, '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'CURSOR', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'CURSOR', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { '?' , { {    1,   1, NIL, 'A', NIL } } , .F. } )
  aAdd( aCommRules, { '??' , { {    1,   1, NIL, 'A', NIL } } , .F. } )
  aAdd( aCommRules, { 'EJECT' ,  , .F. } )
  aAdd( aCommRules, { 'TEXT' ,  , .F. } )
  aAdd( aCommRules, { 'TEXT' , { {    0,   0, 'TO', NIL, NIL }, {    1,   0, 'FILE', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'TEXT' , { {    0,   0, 'TO', NIL, NIL }, {    0,   0, 'PRINTER', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'CLS' ,  , .F. } )
  aAdd( aCommRules, { 'CLEAR' , { {    0,   0, 'SCREEN', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'CLEAR', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'CLEAR', NIL, NIL }, {    3,   0, 'TO', '<', NIL }, {    4,   0, ',', '<', NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, ',', '<', NIL }, {    4,   0, ',', '<', NIL }, {    5,   0, 'BOX', '<', NIL }, { 1006,   1, 'COLOR', '<', NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'TO', '<', NIL }, {    4,   0, ',', '<', NIL }, {    0,   1, 'DOUBLE', NIL, NIL }, { 1005,   1, 'COLOR', '<', NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'TO', '<', NIL }, {    4,   0, ',', '<', NIL }, { 1005,   1, 'COLOR', '<', NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'SAY', '<', NIL }, {    4,   1, 'PICTURE', '<', NIL }, { 1005,   1, 'COLOR', '<', NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'SAY', '<', NIL }, { 1004,   1, 'COLOR', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'BELL', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'BELL', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'CONFIRM', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'CONFIRM', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'ESCAPE', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'ESCAPE', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'INTENSITY', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'INTENSITY', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'SCOREBOARD', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'SCOREBOARD', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'DELIMITERS', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DELIMITERS', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DELIMITERS', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DELIMITERS', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, {    0,   0, 'DEFAULT', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DELIMITERS', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'FORMAT', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'FORMAT', NIL, NIL }, {    1,   0, 'TO', '<', NIL }, {    2,   0, '.', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'FORMAT', NIL, NIL }, {    1,   0, 'TO', ':', { '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'FORMAT', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'GET', '<', NIL }, {    4,   1, 'PICTURE', '<', NIL }, {    5,   1, 'VALID', '<', NIL }, {    6,   1, 'WHEN', '<', NIL }, { 1007,   1, 'SEND', '<', NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'SAY', '<', NIL }, { 1004,   1, NIL, 'A', { 'GET' } }, {    5,   0, 'GET', '<', NIL }, { 1006,   1, NIL, 'A', NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'GET', '<', NIL }, { 1004,   1, NIL, 'A', { 'RANGE' } }, {    5,   0, 'RANGE', '<', NIL }, {    6,   0, ',', '<', NIL }, { 1007,   1, NIL, 'A', NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'GET', '<', NIL }, { 1004,   1, NIL, 'A', { 'COLOR' } }, {    5,   0, 'COLOR', '<', NIL }, { 1006,   1, NIL, 'A', NIL } } , .F. } )
  aAdd( aCommRules, { 'READ' , { {    0,   0, 'SAVE', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'READ' ,  , .F. } )
  aAdd( aCommRules, { 'CLEAR' , { {    0,   0, 'GETS', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { { 1001,   1, NIL, 'A', { 'COLOUR' } }, {    0,   0, 'COLOUR', NIL, NIL }, { 1002,   1, NIL, 'A', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'WRAP', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'WRAP', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'MESSAGE', NIL, NIL }, {    1,   0, 'TO', '<', NIL }, {    2,   1, NIL, ':', { 'CENTER', 'CENTRE' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'MESSAGE', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'PROMPT', '<', NIL }, {    4,   1, 'MESSAGE', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'MENU' , { {    1,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SAVE' , { {    0,   0, 'SCREEN', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'RESTORE' , { {    0,   0, 'SCREEN', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SAVE' , { {    0,   0, 'SCREEN', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'RESTORE' , { {    0,   0, 'SCREEN', NIL, NIL }, {    1,   0, 'FROM', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'WAIT' , { {    1,   1, NIL, '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'WAIT' , { {    1,   1, NIL, '<', { 'TO' } }, {    2,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'ACCEPT' , { {    1,   1, NIL, '<', { 'TO' } }, {    2,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'INPUT' , { {    1,   1, NIL, '<', { 'TO' } }, {    2,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'KEYBOARD' , { {    1,   0, NIL, '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'CLEAR' , { {    0,   0, 'TYPEAHEAD', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'TYPEAHEAD', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'KEY', '<', NIL }, {    2,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'KEY', '<', NIL }, {    2,   0, 'TO', '<', NIL }, {    0,   0, '(', NIL, NIL }, {    3,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'KEY', '<', NIL }, {    2,   0, 'TO', ':', { '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'KEY', '<', NIL }, {    0,   1, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'FUNCTION', '<', NIL }, {    0,   1, 'TO', NIL, NIL }, {    2,   1, NIL, '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'CLEAR' , { {    0,   0, 'MEMORY', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'RELEASE' , { {    1,   0, NIL, 'A', NIL } } , .F. } )
  aAdd( aCommRules, { 'RELEASE' , { {    0,   0, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'RELEASE' , { {    0,   0, 'ALL', NIL, NIL }, {    1,   0, 'LIKE', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'RELEASE' , { {    0,   0, 'ALL', NIL, NIL }, {    1,   0, 'EXCEPT', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'RESTORE' , { {    1,   1, 'FROM', '(', NIL }, {    2,   1, NIL, ':', { 'ADDITIVE' } } } , .F. } )
  aAdd( aCommRules, { 'SAVE' , { {    0,   0, 'ALL', NIL, NIL }, {    1,   0, 'LIKE', '<', NIL }, {    2,   0, 'TO', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'SAVE' , { {    1,   0, 'TO', '(', NIL }, {    0,   0, 'ALL', NIL, NIL }, {    2,   0, 'LIKE', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SAVE' , { {    0,   0, 'ALL', NIL, NIL }, {    1,   0, 'EXCEPT', '<', NIL }, {    2,   0, 'TO', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'SAVE' , { {    1,   0, 'TO', '(', NIL }, {    0,   0, 'ALL', NIL, NIL }, {    2,   0, 'EXCEPT', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SAVE' , { {    1,   1, 'TO', '(', NIL }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'ERASE' , { {    1,   0, NIL, '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'DELETE' , { {    1,   0, 'FILE', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'RENAME' , { {    1,   0, NIL, '(', NIL }, {    2,   0, 'TO', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'COPY' , { {    1,   0, 'FILE', '(', NIL }, {    2,   0, 'TO', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'DIR' , { {    1,   1, NIL, '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'TYPE' , { { 1,   1, NIL, '(', { 'TO PRINTER', 'TO' } }, {    2,   1, NIL, ':', { 'TO PRINTER' } }, { 1000,   1, 'TO', NIL, NIL }, { 1003,  -1, 'FILE', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'TYPE' , { {    1,   0, NIL, '(', NIL }, {    2,   1, NIL, ':', { 'TO PRINTER' } } } , .F. } )
  aAdd( aCommRules, { 'REQUEST' , { {    1,   0, NIL, 'A', NIL } } , .F. } )
  aAdd( aCommRules, { 'CANCEL' ,  , .F. } )
  aAdd( aCommRules, { 'QUIT' ,  , .F. } )
  aAdd( aCommRules, { 'RUN' , { {    1,   0, NIL, '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'RUN' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { '!' , { {    1,   0, NIL, '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'RUN' , { {    1,   0, '=', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'RUN' , { {    1,   0, ':=', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'EXCLUSIVE', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'EXCLUSIVE', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'SOFTSEEK', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'SOFTSEEK', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'UNIQUE', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'UNIQUE', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'DELETED', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DELETED', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SELECT' , { {    1,   0, NIL, '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SELECT' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, {    2,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'USE' ,  , .F. } )
  aAdd( aCommRules, { 'USE' , { {    1,   0, NIL, '(', NIL }, {    2,   1, 'VIA', '<', NIL }, {    3,   1, 'ALIAS', '<', NIL }, {    4,   1, NIL, ':', { 'NEW' } }, {    5,   1, NIL, ':', { 'EXCLUSIVE' } }, {    6,   1, NIL, ':', { 'SHARED' } }, {    7,   1, NIL, ':', { 'READONLY' } }, { 1008,   1, 'INDEX', '(', NIL }, { 1009,   2, ',', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'APPEND' , { {    0,   0, 'BLANK', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'PACK' ,  , .F. } )
  aAdd( aCommRules, { 'ZAP' ,  , .F. } )
  aAdd( aCommRules, { 'UNLOCK' ,  , .F. } )
  aAdd( aCommRules, { 'UNLOCK' , { {    0,   0, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'COMMIT' ,  , .F. } )
  aAdd( aCommRules, { 'GOTO' , { {    1,   0, NIL, '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'GO' , { {    1,   0, NIL, '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'GOTO' , { {    0,   0, 'TOP', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'GO' , { {    0,   0, 'TOP', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'GOTO' , { {    0,   0, 'BOTTOM', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'GO' , { {    0,   0, 'BOTTOM', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SKIP' ,  , .F. } )
  aAdd( aCommRules, { 'SKIP' , { {    1,   0, NIL, '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SKIP' , { {    1,   0, 'ALIAS', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SKIP' , { {    1,   0, NIL, '<', NIL }, {    2,   0, 'ALIAS', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SEEK' , { {    1,   0, NIL, '<', NIL }, {    2,   1, NIL, ':', { 'SOFTSEEK' } } } , .F. } )
  aAdd( aCommRules, { 'FIND' , { {    1,   0, NIL, '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'FIND' , { {    1,   0, ':=', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'FIND' , { {    1,   0, '=', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'CONTINUE' ,  , .F. } )
  aAdd( aCommRules, { 'LOCATE' , { {    1,   1, 'FOR', '<', NIL }, {    2,   1, 'WHILE', '<', NIL }, {    3,   1, 'NEXT', '<', NIL }, {    4,   1, 'RECORD', '<', NIL }, {    5,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'RELATION', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'RELATION', NIL, NIL }, {    1,   1, NIL, ':', { 'ADDITIVE' } }, {    2,   1, 'TO', '<', NIL }, {    3,  -1, 'INTO', '(', NIL }, {    0,   2, ',', NIL, NIL }, { 1000,   3, 'TO', NIL, NIL }, { 1004,  -2, NIL, '<', NIL }, { 1005,  -2, 'INTO', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'FILTER', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'FILTER', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'FILTER', NIL, NIL }, {    1,   0, 'TO', ':', { '&' } } } , .F. } )
  aAdd( aCommRules, { 'REPLACE' , { {    1,   1, NIL, '<', { 'FOR', 'WHILE', 'NEXT', 'RECORD', 'REST', 'ALL' } }, {    2,  -1, 'WITH', '<', NIL }, { 1003,   2, ',', '<', NIL }, { 1004,  -2, 'WITH', '<', NIL }, {    5,   1, 'FOR', '<', NIL }, {    6,   1, 'WHILE', '<', NIL }, {    7,   1, 'NEXT', '<', NIL }, {    8,   1, 'RECORD', '<', NIL }, {    9,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'REPLACE' , { {    1,   0, NIL, '<', NIL }, {    2,   0, 'WITH', '<', NIL }, { 1003,   1, ',', '<', NIL }, { 1004,  -1, 'WITH', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'DELETE' , { {    1,   1, 'FOR', '<', NIL }, {    2,   1, 'WHILE', '<', NIL }, {    3,   1, 'NEXT', '<', NIL }, {    4,   1, 'RECORD', '<', NIL }, {    5,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'RECALL' , { {    1,   1, 'FOR', '<', NIL }, {    2,   1, 'WHILE', '<', NIL }, {    3,   1, 'NEXT', '<', NIL }, {    4,   1, 'RECORD', '<', NIL }, {    5,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'DELETE' ,  , .F. } )
  aAdd( aCommRules, { 'RECALL' ,  , .F. } )
  aAdd( aCommRules, { 'CREATE' , { {    1,   0, NIL, '(', NIL }, {    2,   1, 'FROM', '(', NIL }, {    3,   1, 'VIA', '<', NIL }, {    4,   1, 'ALIAS', '<', NIL }, {    5,   1, NIL, ':', { 'NEW' } } } , .F. } )
  aAdd( aCommRules, { 'COPY' , { {    0,   1, 'STRUCTURE', NIL, NIL }, {    0,   1, 'EXTENDED', NIL, NIL }, {    1,   1, 'TO', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'COPY' , { {    0,   1, 'STRUCTURE', NIL, NIL }, {    1,   1, 'TO', '(', NIL }, {    2,   1, 'FIELDS', 'A', NIL } } , .F. } )
  aAdd( aCommRules, { 'COPY' , { {    1,   1, 'TO', '(', NIL }, {    0,   1, 'DELIMITED', NIL, NIL }, {    2,   2, 'WITH', '*', NIL }, {    3,   1, 'FIELDS', 'A', NIL }, {    4,   1, 'FOR', '<', NIL }, {    5,   1, 'WHILE', '<', NIL }, {    6,   1, 'NEXT', '<', NIL }, {    7,   1, 'RECORD', '<', NIL }, {    8,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'COPY' , { {    1,   1, 'TO', '(', NIL }, {    0,   1, 'SDF', NIL, NIL }, {    2,   1, 'FIELDS', 'A', NIL }, {    3,   1, 'FOR', '<', NIL }, {    4,   1, 'WHILE', '<', NIL }, {    5,   1, 'NEXT', '<', NIL }, {    6,   1, 'RECORD', '<', NIL }, {    7,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'COPY' , { {    1,   1, 'TO', '(', NIL }, {    2,   1, 'FIELDS', 'A', NIL }, {    3,   1, 'FOR', '<', NIL }, {    4,   1, 'WHILE', '<', NIL }, {    5,   1, 'NEXT', '<', NIL }, {    6,   1, 'RECORD', '<', NIL }, {    7,   1, NIL, ':', { 'REST' } }, {    8,   1, 'VIA', '<', NIL }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'APPEND' , { {    1,   1, 'FROM', '(', NIL }, {    0,   1, 'DELIMITED', NIL, NIL }, {    2,   2, 'WITH', '*', NIL }, {    3,   1, 'FIELDS', 'A', NIL }, {    4,   1, 'FOR', '<', NIL }, {    5,   1, 'WHILE', '<', NIL }, {    6,   1, 'NEXT', '<', NIL }, {    7,   1, 'RECORD', '<', NIL }, {    8,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'APPEND' , { {    1,   1, 'FROM', '(', NIL }, {    0,   1, 'SDF', NIL, NIL }, {    2,   1, 'FIELDS', 'A', NIL }, {    3,   1, 'FOR', '<', NIL }, {    4,   1, 'WHILE', '<', NIL }, {    5,   1, 'NEXT', '<', NIL }, {    6,   1, 'RECORD', '<', NIL }, {    7,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'APPEND' , { {    1,   1, 'FROM', '(', NIL }, {    2,   1, 'FIELDS', 'A', NIL }, {    3,   1, 'FOR', '<', NIL }, {    4,   1, 'WHILE', '<', NIL }, {    5,   1, 'NEXT', '<', NIL }, {    6,   1, 'RECORD', '<', NIL }, {    7,   1, NIL, ':', { 'REST' } }, {    8,   1, 'VIA', '<', NIL }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SORT' , { {    1,   1, 'TO', '(', NIL }, {    2,   1, 'ON', 'A', NIL }, {    3,   1, 'FOR', '<', NIL }, {    4,   1, 'WHILE', '<', NIL }, {    5,   1, 'NEXT', '<', NIL }, {    6,   1, 'RECORD', '<', NIL }, {    7,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'TOTAL' , { {    1,   1, 'TO', '(', NIL }, {    2,   1, 'ON', '<', NIL }, {    3,   1, 'FIELDS', 'A', NIL }, {    4,   1, 'FOR', '<', NIL }, {    5,   1, 'WHILE', '<', NIL }, {    6,   1, 'NEXT', '<', NIL }, {    7,   1, 'RECORD', '<', NIL }, {    8,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'UPDATE' , { {    1,   1, 'FROM', '(', NIL }, {    2,   1, 'ON', '<', NIL }, {    3,   1, 'REPLACE', '<', NIL }, {    4,  -1, 'WITH', '<', NIL }, { 1005,   2, ',', '<', NIL }, { 1006,  -2, 'WITH', '<', NIL }, {    7,   1, NIL, ':', { 'RANDOM' } } } , .F. } )
  aAdd( aCommRules, { 'JOIN' , { {    1,   1, 'WITH', '(', NIL }, {    2,   1, 'TO', '<', NIL }, {    3,   1, 'FIELDS', 'A', NIL }, {    4,   1, 'FOR', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'COUNT' , { {    1,   1, 'TO', '<', NIL }, {    2,   1, 'FOR', '<', NIL }, {    3,   1, 'WHILE', '<', NIL }, {    4,   1, 'NEXT', '<', NIL }, {    5,   1, 'RECORD', '<', NIL }, {    6,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SUM' , { {    1,   1, NIL, '<', { 'FOR', 'WHILE', 'NEXT', 'RECORD', 'REST', 'ALL' } }, { 1002,   2, ',', '<', NIL }, {    3,  -1, 'TO', '<', NIL }, { 1004,   2, ',', '<', NIL }, {    5,   1, 'FOR', '<', NIL }, {    6,   1, 'WHILE', '<', NIL }, {    7,   1, 'NEXT', '<', NIL }, {    8,   1, 'RECORD', '<', NIL }, {    9,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'AVERAGE' , { {    1,   1, NIL, '<', { 'FOR', 'WHILE', 'NEXT', 'RECORD', 'REST', 'ALL' } }, { 1002,   2, ',', '<', NIL }, {    3,  -1, 'TO', '<', NIL }, { 1004,   2, ',', '<', NIL }, {    5,   1, 'FOR', '<', NIL }, {    6,   1, 'WHILE', '<', NIL }, {    7,   1, 'NEXT', '<', NIL }, {    8,   1, 'RECORD', '<', NIL }, {    9,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'LIST' , { {    1,   1, NIL, 'A', { 'OFF', 'TO PRINTER', 'TO', 'FOR', 'WHILE', 'NEXT', 'RECORD', 'REST', 'ALL' } }, {    2,   1, NIL, ':', { 'OFF' } }, {    3,   1, NIL, ':', { 'TO PRINTER' } }, {    0,   1, 'TO', NIL, NIL }, {    4,  -1, 'FILE', '(', NIL }, {    5,   1, 'FOR', '<', NIL }, {    6,   1, 'WHILE', '<', NIL }, {    7,   1, 'NEXT', '<', NIL }, {    8,   1, 'RECORD', '<', NIL }, {    9,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'DISPLAY' , { {    1,   1, NIL, 'A', { 'OFF', 'TO PRINTER', 'TO', 'FOR', 'WHILE', 'NEXT', 'RECORD', 'REST', 'ALL' } }, {    2,   1, NIL, ':', { 'OFF' } }, {    3,   1, NIL, ':', { 'TO PRINTER' } }, {    0,   1, 'TO', NIL, NIL }, {    4,  -1, 'FILE', '(', NIL }, {    5,   1, 'FOR', '<', NIL }, {    6,   1, 'WHILE', '<', NIL }, {    7,   1, 'NEXT', '<', NIL }, {    8,   1, 'RECORD', '<', NIL }, {    9,   1, NIL, ':', { 'REST' } }, {   10,   1, NIL, ':', { 'ALL' } } } , .F. } )
  aAdd( aCommRules, { 'REPORT' , { {    1,   0, 'FORM', '<', NIL }, {    2,   1, 'HEADING', '<', NIL }, {    3,   1, NIL, ':', { 'PLAIN' } }, {    4,   1, NIL, ':', { 'NOEJECT' } }, {    5,   1, NIL, ':', { 'SUMMARY' } }, {    6,   1, NIL, ':', { 'NOCONSOLE' } }, {    7,   1, NIL, ':', { 'TO PRINTER' } }, {    0,   1, 'TO', NIL, NIL }, {    8,  -1, 'FILE', '(', NIL }, {    9,   1, 'FOR', '<', NIL }, {   10,   1, 'WHILE', '<', NIL }, {   11,   1, 'NEXT', '<', NIL }, {   12,   1, 'RECORD', '<', NIL }, {   13,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'LABEL' , { {    1,   0, 'FORM', '<', NIL }, {    2,   1, NIL, ':', { 'SAMPLE' } }, {    3,   1, NIL, ':', { 'NOCONSOLE' } }, {    4,   1, NIL, ':', { 'TO PRINTER' } }, {    0,   1, 'TO', NIL, NIL }, {    5,  -1, 'FILE', '(', NIL }, {    6,   1, 'FOR', '<', NIL }, {    7,   1, 'WHILE', '<', NIL }, {    8,   1, 'NEXT', '<', NIL }, {    9,   1, 'RECORD', '<', NIL }, {   10,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'CLOSE' , { {    1,   0, NIL, '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'CLOSE' ,  , .F. } )
  aAdd( aCommRules, { 'CLOSE' , { {    0,   0, 'DATABASES', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'CLOSE' , { {    0,   0, 'ALTERNATE', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'CLOSE' , { {    0,   0, 'FORMAT', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'CLOSE' , { {    0,   0, 'INDEXES', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'CLOSE' , { {    0,   0, 'PROCEDURE', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'CLOSE' , { {    0,   0, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'CLEAR' ,  , .F. } )
  aAdd( aCommRules, { 'CLEAR' , { {    0,   0, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'INDEX' , { {    1,   0, 'ON', '<', NIL }, {    2,   1, 'TAG', '(', NIL }, {    3,   0, 'TO', '(', NIL }, {    4,   1, 'FOR', '<', NIL }, { 1005,   1, NIL, ':', { 'ALL' } }, {    6,   1, 'WHILE', '<', NIL }, {    7,   1, 'NEXT', '<', NIL }, {    8,   1, 'RECORD', '<', NIL }, { 1009,   1, NIL, ':', { 'REST' } }, {   10,   1, 'EVAL', '<', NIL }, {   11,   1, 'EVERY', '<', NIL }, { 1012,   1, NIL, ':', { 'UNIQUE' } }, {   13,   1, NIL, ':', { 'ASCENDING' } }, { 1014,   1, NIL, ':', { 'DESCENDING' } } } , .F. } )
  aAdd( aCommRules, { 'INDEX' , { {    1,   0, 'ON', '<', NIL }, {    2,   0, 'TAG', '(', NIL }, {    3,   1, 'TO', '(', NIL }, {    4,   1, 'FOR', '<', NIL }, { 1005,   1, NIL, ':', { 'ALL' } }, {    6,   1, 'WHILE', '<', NIL }, {    7,   1, 'NEXT', '<', NIL }, {    8,   1, 'RECORD', '<', NIL }, { 1009,   1, NIL, ':', { 'REST' } }, {   10,   1, 'EVAL', '<', NIL }, {   11,   1, 'EVERY', '<', NIL }, { 1012,   1, NIL, ':', { 'UNIQUE' } }, {   13,   1, NIL, ':', { 'ASCENDING' } }, { 1014,   1, NIL, ':', { 'DESCENDING' } } } , .F. } )
  aAdd( aCommRules, { 'INDEX' , { {    1,   0, 'ON', '<', NIL }, {    2,   0, 'TO', '(', NIL }, {    3,   1, NIL, ':', { 'UNIQUE' } } } , .F. } )
  aAdd( aCommRules, { 'DELETE' , { {    1,   0, 'TAG', '(', NIL }, {    2,   1, 'IN', '(', NIL }, { 1003,   1, ',', '(', NIL }, { 1004,   2, 'IN', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'REINDEX' , { {    1,   1, 'EVAL', '<', NIL }, {    2,   1, 'EVERY', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'REINDEX' ,  , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'INDEX', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, { 1001,   1, NIL, '(', { 'ADDITIVE' } }, { 1002,   2, ',', '(', NIL }, {    3,   1, NIL, ':', { 'ADDITIVE' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'ORDER', NIL, NIL }, {    1,   0, 'TO', '<', NIL }, { 1002,   1, 'IN', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'ORDER', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, {    1,   0, 'TAG', '(', NIL }, { 1002,   1, 'IN', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'ORDER', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )

RETURN .T.

//--------------------------------------------------------------//

STATIC FUNCTION InitResults()

  /* Defines Results*/
  aDefResults := {}
  aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '3' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '4' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '5' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '6' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '7' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '8' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '9' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '10' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '11' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '12' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '13' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '14' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '15' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '16' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '17' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '18' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '19' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '20' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '21' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '22' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '23' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '24' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '25' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '26' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '27' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '28' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '29' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '30' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '31' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '32' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '33' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '34' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '35' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '36' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '37' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '38' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '38' } }, { -1} ,  } )
  aAdd( aDefResults, { , ,  } )
  aAdd( aDefResults, { { {   0, 'Set' }, {   0, '(' }, {   0, '_SET_DATEFORMAT' }, {   0, ',' }, {   0, 'if' }, {   0, '(' }, {   0, '__SetCentury' }, {   0, '(' }, {   0, ')' }, {   0, ',' }, {   0,   1 }, {   0, ',' }, {   0,   2 }, {   0, ')' }, {   0, ')' } }, { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  1, -1,  1, -1, -1} , { NIL, NIL }  } )

  /* Translates Results*/
  aTransResults := {}

  /* Commands Results*/
  aCommResults := {}
  aAdd( aCommResults, { { {   0, 'while ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'end' } }, { -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'end' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'end' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'enddo' } }, { -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'endif' } }, { -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'endcase' } }, { -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'next' } }, { -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'next' } }, { -1} , { NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'do ' }, {   0,   1 }, {   0, '' }, {   2, ' WITH ' }, {   2,   2 } }, { -1,  1, -1, -1,  1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'call ' }, {   0,   1 }, {   0, '' }, {   2, ' WITH ' }, {   2,   2 } }, { -1,  1, -1, -1,  1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, ' ' }, {   0,   2 }, {   0, ' := ' }, {   3, ' ' }, {   3,   3 }, {   3, ' := ' }, {   0, ' ' }, {   0,   1 } }, { -1,  1, -1, -1,  1, -1, -1,  1} , { NIL, NIL, NIL }  } )
  aAdd( aCommResults, { , , { NIL }  } )
  aAdd( aCommResults, { , , { NIL }  } )
  aAdd( aCommResults, { , , { NIL }  } )
  aAdd( aCommResults, { , , { NIL }  } )
  aAdd( aCommResults, { , , { NIL }  } )
  aAdd( aCommResults, { , , { NIL }  } )
  aAdd( aCommResults, { , , { NIL }  } )
  aAdd( aCommResults, { , ,  } )
  aAdd( aCommResults, { { {   0, '_ProcReq_( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_EXACT, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_EXACT, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_FIXED, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_FIXED, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DECIMALS, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DECIMALS, 0 )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_PATH, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_PATH, "" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DEFAULT, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DEFAULT, "" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__SetCentury( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__SetCentury( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_EPOCH, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DATEFORMAT, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '_DFSET( "mm/dd/yyyy", "mm/dd/yy" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '_DFSET( "yyyy.mm.dd", "yy.mm.dd" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '_DFSET( "dd/mm/yyyy", "dd/mm/yy" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '_DFSET( "dd/mm/yyyy", "dd/mm/yy" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '_DFSET( "dd.mm.yyyy", "dd.mm.yy" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '_DFSET( "dd-mm-yyyy", "dd-mm-yy" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '_DFSET( "yyyy/mm/dd", "yy/mm/dd" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '_DFSET( "mm-dd-yyyy", "mm-dd-yy" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_ALTERNATE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_ALTERNATE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_ALTFILE, "" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_ALTFILE, ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  4, -1,  6, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_CONSOLE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_CONSOLE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_MARGIN, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_MARGIN, 0 )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_PRINTER, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_PRINTER, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_PRINTFILE, "" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_PRINTFILE, ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  4, -1,  6, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DEVICE, "SCREEN" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DEVICE, "PRINTER" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'SetColor( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  2, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'SetColor( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'SET COLOR TO ' }, {   1, ' ' }, {   1,   1 } }, { -1, -1,  1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'SetCursor( if(Upper(' }, {   0,   1 }, {   0, ') == "ON", 1, 0) )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'SetCursor( if(' }, {   0,   1 }, {   0, ', 1, 0) )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'QOut( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'QQOut( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__Eject()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'text QOut, QQOut' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__TextSave( ' }, {   0,   1 }, {   0, ' ) ; text QOut, __TextRestore' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__TextSave("PRINTER") ; text QOut, __TextRestore' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Scroll() ; SetPos(0,0)' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'CLS' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Scroll( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   1 }, {   0, ' ) ; SetPos( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'Scroll( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' ) ; SetPos( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'Scroll( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ' ) ; SetPos( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'DispBox( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, '' }, {   6, ', ' }, {   6,   6 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'DispBox( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', 2 ' }, {   5, ', ' }, {   5,   5 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'DispBox( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', 1 ' }, {   5, ', ' }, {   5,   5 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'DevPos( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' ) ; DevOutPict( ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, '' }, {   5, ', ' }, {   5,   5 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'DevPos( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' ) ; DevOut( ' }, {   0,   3 }, {   0, '' }, {   4, ', ' }, {   4,   4 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1, -1,  1, -1} , { NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_BELL, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_BELL, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_CONFIRM, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_CONFIRM, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_ESCAPE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_ESCAPE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_INTENSITY, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_INTENSITY, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_SCOREBOARD, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_SCOREBOARD, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DELIMITERS, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DELIMITERS, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DELIMCHARS, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DELIMCHARS, "::" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DELIMCHARS, "::" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '_ProcReq_( ' }, {   0,   1 }, {   0, ' + ".FMT" ) ; __SetFormat( {|| ' }, {   0,   1 }, {   0, '()} )' } }, { -1,  4, -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '_ProcReq_( ' }, {   0,   1 }, {   0, ' + "." + ' }, {   0,   2 }, {   0, ' ) ; __SetFormat( {|| ' }, {   0,   1 }, {   0, '()} )' } }, { -1,  4, -1,  4, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'if ( Empty(' }, {   0,   1 }, {   0, ') ) ;   SET FORMAT TO ; else ;   __SetFormat( &("{||" + ' }, {   0,   1 }, {   0, ' + "()}") ) ; end' } }, { -1,  4, -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__SetFormat()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'SetPos( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' ) ; AAdd( GetList, _GET_( ' }, {   0,   3 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ' ):display() ) ' }, {   7, '; ATail(GetList):' }, {   7,   7 } }, { -1,  1, -1,  1, -1,  1, -1,  3, -1,  1, -1,  5, -1,  5, -1, -1,  1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '@ ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' SAY ' }, {   0,   3 }, {   0, '' }, {   4, ' ' }, {   4,   4 }, {   0, ' ; @ Row(), Col()+1 GET ' }, {   0,   5 }, {   0, '' }, {   6, ' ' }, {   6,   6 } }, { -1,  1, -1,  1, -1,  1, -1, -1,  1, -1,  1, -1, -1,  1} , { NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '@ ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' GET ' }, {   0,   3 }, {   0, '' }, {   4, ' ' }, {   4,   4 }, {   0, ' VALID {|_1| RangeCheck(_1,, ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ')} ' }, {   7, ' ' }, {   7,   7 } }, { -1,  1, -1,  1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1, -1,  1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '@ ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' GET ' }, {   0,   3 }, {   0, '' }, {   4, ' ' }, {   4,   4 }, {   0, ' SEND colorDisp(' }, {   0,   5 }, {   0, ') ' }, {   6, ' ' }, {   6,   6 } }, { -1,  1, -1,  1, -1,  1, -1, -1,  1, -1,  1, -1, -1,  1} , { NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'ReadModal(GetList)' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'ReadModal(GetList) ; GetList := {}' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'ReadKill(.T.) ; GetList := {}' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '@ ' }, {   1, ' ' }, {   1,   1 }, {   0, ' COLOR ' }, {   2, ' ' }, {   2,   2 } }, { -1, -1,  1, -1, -1,  1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_WRAP, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_WRAP, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_MESSAGE, ' }, {   0,   1 }, {   0, ' ) ; Set( _SET_MCENTER, ' }, {   0,   2 }, {   0, ' )' } }, { -1,  1, -1,  6, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_MESSAGE, 0 ) ; Set( _SET_MCENTER, .f. )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__AtPrompt( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ' , ' }, {   0,   4 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, ' ' }, {   0,   1 }, {   0, ' := __MenuTo( {|_1| if(PCount() == 0, ' }, {   0,   1 }, {   0, ', ' }, {   0,   1 }, {   0, ' := _1)}, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  2, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__XSaveScreen()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__XRestScreen()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, ' ' }, {   0,   1 }, {   0, ' := SaveScreen( 0, 0, Maxrow(), Maxcol() )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'RestScreen( 0, 0, Maxrow(), Maxcol(), ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__Wait( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, ' ' }, {   0,   2 }, {   0, ' := __Wait( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, ' ' }, {   0,   2 }, {   0, ' := __Accept( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'if ( !Empty(__Accept(' }, {   0,   1 }, {   0, ')) ) ; ' }, {   0,   2 }, {   0, ' := &( __AcceptStr() ) ; end' } }, { -1,  1, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__Keyboard( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__Keyboard()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_TYPEAHEAD, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'SetKey( ' }, {   0,   1 }, {   0, ', {|p, l, v| ' }, {   0,   2 }, {   0, '(p, l, v)} )' } }, { -1,  1, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'SET KEY ' }, {   0,   1 }, {   0, ' TO ' }, {   0,   2 } }, { -1,  1, -1,  1} , { NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'if ( Empty(' }, {   0,   2 }, {   0, ') ) ;   SetKey( ' }, {   0,   1 }, {   0, ', NIL ) ; else ;   SetKey( ' }, {   0,   1 }, {   0, ', {|p, l, v| ' }, {   0,   2 }, {   0, '(p, l, v)} ) ; end' } }, { -1,  4, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'SetKey( ' }, {   0,   1 }, {   0, ', NIL )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__SetFunction( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  1, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__MClear()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__MXRelease( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  3, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__MRelease("*", .t.)' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__MRelease( ' }, {   0,   1 }, {   0, ', .t. )' } }, { -1,  2, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__MRelease( ' }, {   0,   1 }, {   0, ', .f. )' } }, { -1,  2, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__MRestore( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  4, -1,  6, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__MSave( ' }, {   0,   2 }, {   0, ', ' }, {   0,   1 }, {   0, ', .t. )' } }, { -1,  4, -1,  4, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__MSave( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', .t. )' } }, { -1,  4, -1,  4, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__MSave( ' }, {   0,   2 }, {   0, ', ' }, {   0,   1 }, {   0, ', .f. )' } }, { -1,  4, -1,  4, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__MSave( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', .f. )' } }, { -1,  4, -1,  4, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__MSave( ' }, {   0,   1 }, {   0, ', "*", .t. )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'FErase( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'FErase( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'FRename( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  4, -1,  4, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__CopyFile( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  4, -1,  4, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__Dir( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__TypeFile( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' ) ' }, {   1, '; COPY FILE ' }, {   1,   1 }, {   1, ' TO ' }, {   1,   3 } }, { -1,  4, -1,  6, -1, -1,  4, -1,  4} , { NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__TypeFile( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  4, -1,  6, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'EXTERNAL ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__Quit()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__Quit()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__Run( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  2, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__Run( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'RUN ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '( run := ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '( run := ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_EXCLUSIVE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_EXCLUSIVE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_SOFTSEEK, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_SOFTSEEK, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_UNIQUE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_UNIQUE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DELETED, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DELETED, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbSelectArea( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbSelectArea( ' }, {   0,   1 }, {   0, '(' }, {   0,   2 }, {   0, ') )' } }, { -1,  1, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbCloseArea()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbUseArea( ' }, {   0,   4 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   1 }, {   0, ', ' }, {   0,   3 }, {   0, ', if(' }, {   0,   6 }, {   0, ' .or. ' }, {   0,   5 }, {   0, ', !' }, {   0,   5 }, {   0, ', NIL), ' }, {   0,   7 }, {   0, ' ) ' }, {   8, '; dbSetIndex( ' }, {   8,   8 }, {   8, ' )' }, {   0, '' }, {   9, '; dbSetIndex( ' }, {   9,   9 }, {   9, ' )' } }, { -1,  6, -1,  1, -1,  4, -1,  4, -1,  6, -1,  6, -1,  6, -1,  6, -1, -1,  4, -1, -1, -1,  4, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbAppend()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__dbPack()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__dbZap()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbUnlock()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbUnlockAll()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbCommitAll()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbGoto(' }, {   0,   1 }, {   0, ')' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbGoto(' }, {   0,   1 }, {   0, ')' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbGoTop()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbGoTop()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbGoBottom()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbGoBottom()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbSkip(1)' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbSkip( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, ' ' }, {   0,   1 }, {   0, ' -> ( dbSkip(1) )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, ' ' }, {   0,   2 }, {   0, ' -> ( dbSkip(' }, {   0,   1 }, {   0, ') )' } }, { -1,  1, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbSeek( ' }, {   0,   1 }, {   0, ', if( ' }, {   0,   2 }, {   0, ', .T., NIL ) )' } }, { -1,  1, -1,  6, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbSeek( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '( find := ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '( find := ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbContinue()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__dbLocate( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ' )' } }, { -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbClearRel()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'if ( !' }, {   0,   1 }, {   0, ' ) ;    dbClearRel() ; end ; dbSetRelation( ' }, {   0,   3 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   2 }, {   0, ' ) ' }, {   5, '; dbSetRelation( ' }, {   5,   5 }, {   5, ', ' }, {   5,   4 }, {   5, ', ' }, {   5,   4 }, {   5, ' )' } }, { -1,  6, -1,  4, -1,  5, -1,  3, -1, -1,  4, -1,  5, -1,  3, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbClearFilter(NIL)' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbSetFilter( ' }, {   0,   1 }, {   0, ', ' }, {   0,   1 }, {   0, ' )' } }, { -1,  5, -1,  3, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'if ( Empty(' }, {   0,   1 }, {   0, ') ) ;    dbClearFilter() ; else     ;    dbSetFilter( ' }, {   0,   1 }, {   0, ', ' }, {   0,   1 }, {   0, ' ) ; end' } }, { -1,  4, -1,  5, -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'DBEval( {|| _FIELD->' }, {   0,   1 }, {   0, ' := ' }, {   0,   2 }, {   0, '' }, {   3, ', _FIELD->' }, {   3,   3 }, {   3, ' := ' }, {   3,   4 }, {   0, '}, ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ' )' } }, { -1,  1, -1,  1, -1, -1,  1, -1,  1, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '_FIELD->' }, {   0,   1 }, {   0, ' := ' }, {   0,   2 }, {   0, '' }, {   3, '; _FIELD->' }, {   3,   3 }, {   3, ' := ' }, {   3,   4 } }, { -1,  1, -1,  1, -1, -1,  1, -1,  1} , { NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'DBEval( {|| dbDelete()}, ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ' )' } }, { -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'DBEval( {|| dbRecall()}, ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ' )' } }, { -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbDelete()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbRecall()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__dbCreate( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   4 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  1, -1,  6, -1,  4, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbCopyXStruct( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbCopyStruct( ' }, {   0,   1 }, {   0, ', { ' }, {   0,   2 }, {   0, ' } )' } }, { -1,  4, -1,  4, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbDelim( .T., ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', { ' }, {   0,   3 }, {   0, ' }, ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbSDF( .T., ' }, {   0,   1 }, {   0, ', { ' }, {   0,   2 }, {   0, ' }, ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbCopy( ' }, {   0,   1 }, {   0, ', { ' }, {   0,   2 }, {   0, ' }, ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbDelim( .F., ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', { ' }, {   0,   3 }, {   0, ' }, ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbSDF( .F., ' }, {   0,   1 }, {   0, ', { ' }, {   0,   2 }, {   0, ' }, ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbApp( ' }, {   0,   1 }, {   0, ', { ' }, {   0,   2 }, {   0, ' }, ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbSort( ' }, {   0,   1 }, {   0, ', { ' }, {   0,   2 }, {   0, ' }, ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbTotal( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', { ' }, {   0,   3 }, {   0, ' }, ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ' )' } }, { -1,  4, -1,  5, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbUpdate( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   7 }, {   0, ', {|| _FIELD->' }, {   0,   3 }, {   0, ' := ' }, {   0,   4 }, {   0, '' }, {   5, ', _FIELD->' }, {   5,   5 }, {   5, ' := ' }, {   5,   6 }, {   0, '} )' } }, { -1,  4, -1,  5, -1,  6, -1,  1, -1,  1, -1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbJoin( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', { ' }, {   0,   3 }, {   0, ' }, ' }, {   0,   4 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  4, -1,  5, -1} , { NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, ' ' }, {   0,   1 }, {   0, ' := 0 ; DBEval( {|| ' }, {   0,   1 }, {   0, ' := ' }, {   0,   1 }, {   0, ' + 1}, ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, ' ' }, {   0,   3 }, {   0, ' := ' }, {   4, ' ' }, {   4,   4 }, {   4, ' := ' }, {   0, ' 0 ; DBEval( {|| ' }, {   0,   3 }, {   0, ' := ' }, {   0,   3 }, {   0, ' + ' }, {   0,   1 }, {   0, '' }, {   4, ', ' }, {   4,   4 }, {   4, ' := ' }, {   4,   4 }, {   4, ' + ' }, {   4,   2 }, {   0, '}, ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ' )' } }, { -1,  1, -1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'M->__Avg := ' }, {   0,   3 }, {   0, ' := ' }, {   4, ' ' }, {   4,   4 }, {   4, ' := ' }, {   0, ' 0 ; DBEval( {|| M->__Avg := M->__Avg + 1, ' }, {   0,   3 }, {   0, ' := ' }, {   0,   3 }, {   0, ' + ' }, {   0,   1 }, {   0, '' }, {   4, ', ' }, {   4,   4 }, {   4, ' := ' }, {   4,   4 }, {   4, ' + ' }, {   4,   2 }, {   0, ' }, ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ' ) ; ' }, {   0,   3 }, {   0, ' := ' }, {   0,   3 }, {   0, ' / M->__Avg ' }, {   4, '; ' }, {   4,   4 }, {   4, ' := ' }, {   4,   4 }, {   4, ' / M->__Avg ' } }, { -1,  1, -1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1,  1, -1,  1, -1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbList( ' }, {   0,   2 }, {   0, ', { ' }, {   0,   1 }, {   0, ' }, .t., ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ' )' } }, { -1,  6, -1,  5, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1,  6, -1,  4, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__DBList( ' }, {   0,   2 }, {   0, ', { ' }, {   0,   1 }, {   0, ' }, ' }, {   0,  10 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ' )' } }, { -1,  6, -1,  5, -1,  6, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1,  6, -1,  4, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__ReportForm( ' }, {   0,   1 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ' )' } }, { -1,  4, -1,  6, -1,  4, -1,  6, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1,  6, -1,  1, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__LabelForm( ' }, {   0,   1 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  4, -1,  6, -1,  4, -1,  6, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, ' ' }, {   0,   1 }, {   0, '->( dbCloseArea() )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbCloseArea()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbCloseAll()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Set(_SET_ALTFILE, "")' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__SetFormat(NIL)' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbClearIndex()' } }, { -1} ,  } )
  aAdd( aCommResults, { , ,  } )
  aAdd( aCommResults, { { {   0, 'CLOSE DATABASES ; SELECT 1 ; CLOSE FORMAT' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'CLEAR SCREEN ; CLEAR GETS' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'CLOSE DATABASES ; CLOSE FORMAT ; CLEAR MEMORY ; CLEAR GETS ; SET ALTERNATE OFF ; SET ALTERNATE TO' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'ordCondSet( ' }, {   0,   4 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   5, ' ' }, {   5,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  11 }, {   0, ', RECNO(), ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   9, ' ' }, {   9,   9 }, {   0, ', ' }, {  14, ' ' }, {  14,  14 }, {   0, ' ) ;  ordCreate(' }, {   0,   3 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   1 }, {   0, ', ' }, {   0,   1 }, {   0, ', ' }, {  12, ' ' }, {  12,  12 }, {   0, '    )' } }, { -1,  3, -1,  5, -1, -1,  6, -1,  5, -1,  5, -1,  1, -1,  1, -1,  1, -1, -1,  6, -1, -1,  6, -1,  4, -1,  4, -1,  3, -1,  5, -1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'ordCondSet( ' }, {   0,   4 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   5, ' ' }, {   5,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  11 }, {   0, ',    RECNO(), ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   9, ' ' }, {   9,   9 }, {   0, ', ' }, {  14, ' ' }, {  14,  14 }, {   0, ' ) ;  ordCreate(' }, {   0,   3 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   1 }, {   0, ', ' }, {   0,   1 }, {   0, ', ' }, {  12, ' ' }, {  12,  12 }, {   0, '    )' } }, { -1,  3, -1,  5, -1, -1,  6, -1,  5, -1,  5, -1,  1, -1,  1, -1,  1, -1, -1,  6, -1, -1,  6, -1,  4, -1,  4, -1,  3, -1,  5, -1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbCreateIndex( ' }, {   0,   2 }, {   0, ', ' }, {   0,   1 }, {   0, ', ' }, {   0,   1 }, {   0, ', if( ' }, {   0,   3 }, {   0, ', .t., NIL ) )' } }, { -1,  4, -1,  3, -1,  5, -1,  6, -1} , { NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'ordDestroy( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' ) ' }, {   3, '; ordDestroy( ' }, {   3,   3 }, {   3, ', ' }, {   3,   4 }, {   3, ' ) ' } }, { -1,  4, -1,  4, -1, -1,  4, -1,  4, -1} , { NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'ordCondSet(,,,, ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ',,,,,,,) ;  ordListRebuild()' } }, { -1,  5, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'ordListRebuild()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'if !' }, {   0,   3 }, {   0, ' ; ordListClear() ; end ' }, {   1, '; ordListAdd( ' }, {   1,   1 }, {   1, ' )' }, {   0, '' }, {   2, '; ordListAdd( ' }, {   2,   2 }, {   2, ' )' } }, { -1,  6, -1, -1,  4, -1, -1, -1,  4, -1} , { NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'ordSetFocus( ' }, {   0,   1 }, {   0, '' }, {   2, ', ' }, {   2,   2 }, {   0, ' )' } }, { -1,  1, -1, -1,  4, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'ordSetFocus( ' }, {   0,   1 }, {   0, '' }, {   2, ', ' }, {   2,   2 }, {   0, ' )' } }, { -1,  4, -1, -1,  4, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'ordSetFocus(0)' } }, { -1} ,  } )

RETURN .T.

//--------------------------------------------------------------//

STATIC FUNCTION InitClsRules()

 #ifdef __HARBOUR__

   /* Defines */
   aAdd( aDefRules, { 'HB_CLASS_CH_' ,  , .T. } )
   aAdd( aDefRules, { 'HB_SETUP_CH_' ,  , .T. } )
   aAdd( aDefRules, { 'HB_EXTENSION' ,  , .T. } )
   aAdd( aDefRules, { 'HB_C52_UNDOC' ,  , .T. } )
   aAdd( aDefRules, { 'HB_COMPAT_C53' ,  , .T. } )
   aAdd( aDefRules, { 'HB_COMPAT_XPP' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_CH_' ,  , .T. } )
   aAdd( aDefRules, { 'HB_MSGLISTALL' ,  , .T. } )
   aAdd( aDefRules, { 'HB_MSGLISTCLASS' ,  , .T. } )
   aAdd( aDefRules, { 'HB_MSGLISTPURE' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_CLSTP_EXPORTED' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_CLSTP_PROTECTED' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_CLSTP_HIDDEN' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_CLSTP_CTOR' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_CLSTP_READONLY' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_CLSTP_SHARED' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_CLSTP_CLASS' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_CLSTP_SUPER' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_MSG_METHOD' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_MSG_DATA' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_MSG_CLASSDATA' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_MSG_INLINE' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_MSG_VIRTUAL' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_MSG_SUPER' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_MSG_ONERROR' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_MSG_CLSMTHD' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_DATA_SYMBOL' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_DATA_VALUE' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_DATA_TYPE' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_DATA_SCOPE' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_DATA_PERSISTENT' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_CLSD_SYMBOL' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_CLSD_VALUE' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_CLSD_TYPE' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_CLSD_SCOPE' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_MTHD_SYMBOL' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_MTHD_PFUNCTION' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_MTHD_SCOPE' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_MTHD_PERSISTENT' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_CLSM_SYMBOL' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_CLSM_PFUNCTION' ,  , .T. } )
   aAdd( aDefRules, { 'HB_OO_CLSM_SCOPE' ,  , .T. } )
   aAdd( aDefRules, { '__HB_CLS_PAR' ,  , .T. } )
   aAdd( aDefRules, { '__HB_CLS_NOINI' ,  , .T. } )
   aAdd( aDefRules, { 'HB_CLS_FWO' ,  , .T. } )
   aAdd( aDefRules, { 'HB_CLS_CSY' ,  , .T. } )
   aAdd( aDefRules, { 'HB_CLS_VO' ,  , .T. } )
   aAdd( aDefRules, { 'HB_CLS_TOP' ,  , .T. } )
   aAdd( aDefRules, { 'HB_CLS_NO_OO_ERR' ,  , .T. } )

   /* Translates */
   aAdd( aTransRules, { '__ERR' , { {    0,   0, '(', NIL, NIL }, { 1001,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL } } , .T. } )
   aAdd( aTransRules, { ')' , { {    0,   0, '(', NIL, NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
   aAdd( aTransRules, { 'HBCLSCHOICE' , { {    1,   0, '(', '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, ',', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
   aAdd( aTransRules, { 'CREATE' , { {    0,   0, 'CLASS', NIL, NIL } } , .T. } )
   aAdd( aTransRules, { 'DECLMETH' , { {    1,   0, NIL, '<', NIL }, {    2,   0, NIL, '<', NIL } } , .T. } )
   aAdd( aTransRules, { ':' , { {    0,   0, 'CLASS', NIL, NIL } } , .T. } )
   aAdd( aTransRules, { ':' , { {    0,   0, 'CLASS', NIL, NIL }, {    0,   0, ':', NIL, NIL } } , .T. } )
   aAdd( aTransRules, { 'EXPORTED' , { {    0,   0, ':', NIL, NIL } } , .T. } )
   aAdd( aTransRules, { 'EXPORT' , { {    0,   0, ':', NIL, NIL } } , .T. } )
   aAdd( aTransRules, { 'VISIBLE' , { {    0,   0, ':', NIL, NIL } } , .T. } )
   aAdd( aTransRules, { 'HIDDEN' , { {    0,   0, ':', NIL, NIL } } , .T. } )
   aAdd( aTransRules, { 'PROTECTED' , { {    0,   0, ':', NIL, NIL } } , .T. } )
   aAdd( aTransRules, { 'CLASS' , { {    0,   0, 'VAR', NIL, NIL } } , .T. } )
   aAdd( aTransRules, { 'CLASS' , { {    0,   0, 'METHOD', NIL, NIL } } , .T. } )
   aAdd( aTransRules, { '(' , { {    1,   0, NIL, '!', NIL }, {    0,   0, '{', NIL, NIL }, {    2,   1, NIL, 'A', { '}' } }, {    0,   0, '}', NIL, NIL } } , .T. } )
   aAdd( aTransRules, { '=' , { {    1,   0, NIL, '!', NIL }, {    0,   0, '{', NIL, NIL }, {    2,   1, NIL, 'A', { '}' } }, {    0,   0, '}', NIL, NIL } } , .T. } )
   aAdd( aTransRules, { ',' , { {    1,   0, NIL, '!', NIL }, {    0,   0, '{', NIL, NIL }, {    2,   1, NIL, 'A', { '}' } }, {    0,   0, '}', NIL, NIL } } , .T. } )
   aAdd( aTransRules, { 'END' , { {    0,   0, 'CLASS', NIL, NIL } } , .T. } )
   aAdd( aTransRules, { ':' , { {    0,   0, 'SUPER', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL }, {    0,   0, ':', NIL, NIL } } , .T. } )
   aAdd( aTransRules, { ':' , { {    0,   0, 'SUPER', NIL, NIL }, {    0,   0, '(', NIL, NIL }, {    0,   0, ')', NIL, NIL }, {    0,   0, ':', NIL, NIL } } , .T. } )
   aAdd( aTransRules, { ':' , { {    0,   0, 'SUPER', NIL, NIL }, {    0,   0, '(', NIL, NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )

   /* Commands */
   aAdd( aCommRules, { 'CLASS' , { {    1,   0, NIL, '<', NIL }, {    2,   1, 'METACLASS', '<', NIL }, { 1003,   1, NIL, ':', { 'FROM', 'INHERIT' } }, { 1004,  -1, NIL, '<', NIL }, { 1005,   2, ',', '<', NIL }, {    6,   1, NIL, ':', { 'STATIC' } } } , .T. } )
   aAdd( aCommRules, { 'VAR' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'TYPE', '<', NIL }, {    3,   1, 'ASSIGN', '<', NIL }, {    4,   1, NIL, ':', { 'EXPORTED', 'VISIBLE' } }, {    5,   1, NIL, ':', { 'PROTECTED' } }, {    6,   1, NIL, ':', { 'HIDDEN' } }, {    7,   1, NIL, ':', { 'READONLY', 'RO' } }, {    8,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
   aAdd( aCommRules, { 'VAR' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   1, 'INIT', '<', NIL }, {    4,   1, NIL, ':', { 'EXPORTED', 'VISIBLE' } }, {    5,   1, NIL, ':', { 'PROTECTED' } }, {    6,   1, NIL, ':', { 'HIDDEN' } }, {    7,   1, NIL, ':', { 'READONLY', 'RO' } }, {    8,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
   aAdd( aCommRules, { 'VAR' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'IN', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'VAR' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'IS', '<', NIL }, {    4,   0, 'IN', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'VAR' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'IS', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'VAR' , { {    1,   0, NIL, '<', NIL }, {    2,   0, 'IS', '<', NIL }, {    3,   0, 'TO', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    0,   0, 'DEFERRED', NIL, NIL } } , .T. } )
   aAdd( aCommRules, { 'EXPORT' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   1, 'INIT', '<', NIL }, {    4,   1, NIL, ':', { 'READONLY', 'RO' } }, {    5,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
   aAdd( aCommRules, { 'EXPORT' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'TYPE', '<', NIL }, {    3,   1, 'ASSIGN', '<', NIL }, {    4,   1, NIL, ':', { 'READONLY', 'RO' } }, {    5,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
   aAdd( aCommRules, { 'PROTECT' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   1, 'INIT', '<', NIL }, {    4,   1, NIL, ':', { 'READONLY', 'RO' } }, {    5,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
   aAdd( aCommRules, { 'PROTECT' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'TYPE', '<', NIL }, {    3,   1, 'ASSIGN', '<', NIL }, {    4,   1, NIL, ':', { 'READONLY', 'RO' } }, {    5,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
   aAdd( aCommRules, { 'HIDDE' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   1, 'INIT', '<', NIL }, {    4,   1, NIL, ':', { 'READONLY', 'RO' } }, {    5,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
   aAdd( aCommRules, { 'HIDDE' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'TYPE', '<', NIL }, {    3,   1, 'ASSIGN', '<', NIL }, {    4,   1, NIL, ':', { 'READONLY', 'RO' } }, {    5,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
   aAdd( aCommRules, { 'CLASSVAR' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'TYPE', '<', NIL }, {    3,   1, 'ASSIGN', '<', NIL }, {    4,   1, NIL, ':', { 'EXPORTED', 'VISIBLE' } }, {    5,   1, NIL, ':', { 'PROTECTED' } }, {    6,   1, NIL, ':', { 'HIDDEN' } }, {    7,   1, NIL, ':', { 'READONLY', 'RO' } }, {    8,   1, NIL, ':', { 'SHARED' } } } , .T. } )
   aAdd( aCommRules, { 'CLASSVAR' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   1, 'INIT', '<', NIL }, {    4,   1, NIL, ':', { 'EXPORTED', 'VISIBLE' } }, {    5,   1, NIL, ':', { 'PROTECTED' } }, {    6,   1, NIL, ':', { 'HIDDEN' } }, {    7,   1, NIL, ':', { 'READONLY', 'RO' } }, {    8,   1, NIL, ':', { 'SHARED' } } } , .T. } )
   aAdd( aCommRules, { 'DATA' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   1, 'INIT', '<', NIL }, {    4,   1, NIL, ':', { 'EXPORTED', 'VISIBLE' } }, {    5,   1, NIL, ':', { 'PROTECTED' } }, {    6,   1, NIL, ':', { 'HIDDEN' } }, {    7,   1, NIL, ':', { 'READONLY', 'RO' } }, {    8,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
   aAdd( aCommRules, { 'CLASSDATA' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   1, 'INIT', '<', NIL }, {    4,   1, NIL, ':', { 'EXPORTED', 'VISIBLE' } }, {    5,   1, NIL, ':', { 'PROTECTED' } }, {    6,   1, NIL, ':', { 'HIDDEN' } }, {    7,   1, NIL, ':', { 'READONLY', 'RO' } }, {    8,   1, NIL, ':', { 'SHARED' } } } , .T. } )
   aAdd( aCommRules, { 'CLASSMETHOD' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   1, NIL, ':', { 'EXPORTED', 'VISIBLE' } }, {    4,   1, NIL, ':', { 'PROTECTED' } }, {    5,   1, NIL, ':', { 'HIDDEN' } }, {    6,   1, NIL, ':', { 'SHARED' } } } , .T. } )
   aAdd( aCommRules, { 'CONSTRUCTOR' , { {    1,   0, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, NIL, ':', { 'CONSTRUCTOR' } }, { 1003,   1, 'AS', '<', NIL }, {    4,   1, NIL, ':', { 'EXPORTED', 'VISIBLE' } }, {    5,   1, NIL, ':', { 'PROTECTED' } }, {    6,   1, NIL, ':', { 'HIDDEN' } }, {    0,   1, '_CLASS_DECLARATION_', NIL, NIL }, {    7,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
   aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, NIL, ':', { 'CONSTRUCTOR' } }, { 1004,   1, 'AS', '<', NIL }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN' } }, {    0,   1, '_CLASS_DECLARATION_', NIL, NIL }, {    8,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
   aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'BLOCK', '<', NIL }, { 1004,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN' } }, {    8,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
   aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'EXTERN', '<', NIL }, { 1004,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN' } }, {    8,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
   aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'INLINE', 'A', NIL }, { 1004,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN' } }, {    8,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
   aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, 'AS', '<', NIL }, {    4,   0, 'INLINE', 'A', NIL }, { 1005,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    6,   1, NIL, ':', { 'EXPORTED', 'VISIBLE' } }, {    7,   1, NIL, ':', { 'PROTECTED' } }, {    8,   1, NIL, ':', { 'HIDDEN' } }, {    9,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
   aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    0,   0, 'INLINE', NIL, NIL }, { 1003,   1, 'LOCAL', '<', NIL }, {    0,  -1, ',', NIL, NIL }, {    4,   0, NIL, 'A', NIL }, { 1005,   1, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, 'AS', '<', NIL }, {    0,   0, 'INLINE', NIL, NIL }, { 1004,   1, 'LOCAL', '<', NIL }, {    0,  -1, ',', NIL, NIL }, {    5,   0, NIL, 'A', NIL }, { 1006,   1, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    0,   0, 'VIRTUAL', NIL, NIL } } , .T. } )
   aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'OPERATOR', '<', NIL }, {    4,   1, NIL, ':', { 'EXPORTED', 'VISIBLE' } }, {    5,   1, NIL, ':', { 'PROTECTED' } }, {    6,   1, NIL, ':', { 'HIDDEN' } } } , .T. } )
   aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, 'AS', '<', NIL }, {    4,   0, 'OPERATOR', '<', NIL }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN' } } } , .T. } )
   aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'METHOD', '<', NIL }, { 1004,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN' } } } , .T. } )
   aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, 'AS', '<', NIL }, {    4,   0, 'METHOD', '<', NIL }, { 1005,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    6,   1, NIL, ':', { 'EXPORTED', 'VISIBLE' } }, {    7,   1, NIL, ':', { 'PROTECTED' } }, {    8,   1, NIL, ':', { 'HIDDEN' } } } , .T. } )
   aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'METHOD', '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1004,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1005,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    6,   1, NIL, ':', { 'EXPORTED', 'VISIBLE' } }, {    7,   1, NIL, ':', { 'PROTECTED' } }, {    8,   1, NIL, ':', { 'HIDDEN' } } } , .T. } )
   aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, 'AS', '<', NIL }, {    4,   0, 'METHOD', '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1005,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1006,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    7,   1, NIL, ':', { 'EXPORTED', 'VISIBLE' } }, {    8,   1, NIL, ':', { 'PROTECTED' } }, {    9,   1, NIL, ':', { 'HIDDEN' } } } , .T. } )
   aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'IN', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, 'AS', '<', NIL }, {    4,   0, 'IN', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'IS', '<', NIL }, {    4,   0, 'IN', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'IS', '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1004,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, {    5,   0, 'IN', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, 'AS', '<', NIL }, {    4,   0, 'IS', '<', NIL }, {    5,   0, 'IN', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, 'AS', '<', NIL }, {    4,   0, 'IS', '<', NIL }, {    0,   0, '(', NIL, NIL }, {    5,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, {    6,   0, 'IN', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'IS', '<', NIL }, { 1004,   1, NIL, 'A', NIL } } , .T. } )
   aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'TO', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, 'AS', '<', NIL }, {    4,   0, 'TO', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'DELEGATE' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'TO', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'DELEGATE' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, 'AS', '<', NIL }, {    4,   0, 'TO', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } }, {    0,   0, 'SETGET', NIL, NIL } } , .T. } )
   aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, 'AS', '<', NIL }, {    4,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } }, {    0,   0, 'SETGET', NIL, NIL } } , .T. } )
   aAdd( aCommRules, { 'ACCESS' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
   aAdd( aCommRules, { 'ACCESS' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, 'AS', '<', NIL }, {    4,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
   aAdd( aCommRules, { 'ACCESS' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    0,   0, 'INLINE', NIL, NIL }, { 1003,   1, 'LOCAL', '<', NIL }, {    0,  -1, ',', NIL, NIL }, {    4,   0, NIL, 'A', NIL }, {    5,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
   aAdd( aCommRules, { 'ACCESS' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    0,   0, 'DEFERRED', NIL, NIL } } , .T. } )
   aAdd( aCommRules, { 'ASSIGN' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'ASSIGN' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, 'AS', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'ASSIGN' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, 'AS', '<', NIL }, {    0,   0, 'INLINE', NIL, NIL }, { 1004,   1, 'LOCAL', '<', NIL }, {    0,  -1, ',', NIL, NIL }, {    5,   0, NIL, 'A', NIL } } , .T. } )
   aAdd( aCommRules, { 'ON' , { {    1,   0, 'ERROR', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'ERROR' , { {    1,   0, 'HANDLER', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'ERROR' , { {    1,   0, 'HANDLER', '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL } } , .T. } )
   aAdd( aCommRules, { 'ENDCLASS' ,  , .T. } )
   aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, {    2,   0, 'CLASS', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, {    2,   0, 'CLASS', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '_CLASS_IMPLEMENTATION_', NIL, NIL } } , .T. } )
   aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, {    2,   0, 'CLASS', '<', NIL }, {    0,   0, '_CLASS_IMPLEMENTATION_', NIL, NIL } } , .T. } )
   aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, {    2,   0, 'DECLCLASS', '<', NIL }, {    0,   0, '_CLASS_IMPLEMENTATION_', NIL, NIL } } , .T. } )
   aAdd( aCommRules, { 'DECLARED' , { {    1,   0, 'METHOD', '<', NIL }, {    2,   0, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'ACCESS' , { {    1,   0, NIL, '<', NIL }, {    2,   0, 'CLASS', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'ASSIGN' , { {    1,   0, NIL, '<', NIL }, {    2,   0, 'CLASS', '<', NIL } } , .T. } )

 #endif

RETURN .T.

//--------------------------------------------------------------//

STATIC FUNCTION InitClsResults()

 #ifdef __HARBOUR__

   /* Defines Results*/
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { { {   0, '0' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '4' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '8' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '16' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '32' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '64' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '128' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '0' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '3' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '4' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '5' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '6' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '7' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '3' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '4' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '5' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '3' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '4' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '3' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '4' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '3' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '__CLS_PARAM' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '.F.' } }, { -1} ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )

   /* Translates Results*/
   aAdd( aTransResults, { { {   0, '#error ' }, {   1,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aTransResults, { { {   0, ')' } }, { -1} ,  } )
   aAdd( aTransResults, { { {   0, 'iif( ' }, {   0,   1 }, {   0, ', HB_OO_CLSTP_EXPORTED , iif( ' }, {   0,   2 }, {   0, ', HB_OO_CLSTP_PROTECTED, iif( ' }, {   0,   3 }, {   0, ', HB_OO_CLSTP_HIDDEN, nScope) ) )' } }, { -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL }  } )
   aAdd( aTransResults, { { {   0, 'CLASS' } }, { -1} ,  } )
   aAdd( aTransResults, { { {   0,   1 }, {   0, '_' }, {   0,   2 } }, {  1, -1,  1} , { NIL, NIL }  } )
   aAdd( aTransResults, { , ,  } )
   aAdd( aTransResults, { { {   0, ':' } }, { -1} ,  } )
   aAdd( aTransResults, { { {   0, 'nScope := HB_OO_CLSTP_EXPORTED' } }, { -1} ,  } )
   aAdd( aTransResults, { { {   0, 'nScope := HB_OO_CLSTP_EXPORTED' } }, { -1} ,  } )
   aAdd( aTransResults, { { {   0, 'nScope := HB_OO_CLSTP_EXPORTED' } }, { -1} ,  } )
   aAdd( aTransResults, { { {   0, 'nScope := HB_OO_CLSTP_HIDDEN' } }, { -1} ,  } )
   aAdd( aTransResults, { { {   0, 'nScope := HB_OO_CLSTP_PROTECTED' } }, { -1} ,  } )
   aAdd( aTransResults, { { {   0, 'CLASSVAR' } }, { -1} ,  } )
   aAdd( aTransResults, { { {   0, 'CLASSMETHOD' } }, { -1} ,  } )
   aAdd( aTransResults, { { {   0, '( ' }, {   0,   1 }, {   0, '():New( ' }, {   0,   2 }, {   0, ' )' } }, { -1,  1, -1,  1, -1} , { NIL, NIL }  } )
   aAdd( aTransResults, { { {   0, '= ' }, {   0,   1 }, {   0, '():New( ' }, {   0,   2 }, {   0, ' )' } }, { -1,  1, -1,  1, -1} , { NIL, NIL }  } )
   aAdd( aTransResults, { { {   0, ', ' }, {   0,   1 }, {   0, '():New( ' }, {   0,   2 }, {   0, ' )' } }, { -1,  1, -1,  1, -1} , { NIL, NIL }  } )
   aAdd( aTransResults, { { {   0, 'ENDCLASS' } }, { -1} ,  } )
   aAdd( aTransResults, { { {   0, ':' }, {   0,   1 }, {   0, ':' } }, { -1,  1, -1} , { NIL }  } )
   aAdd( aTransResults, { { {   0, ':Super:' } }, { -1} ,  } )
   aAdd( aTransResults, { { {   0, ':Super' } }, { -1} ,  } )

   /* Commands Results*/
   aAdd( aCommResults, { { {   0, '_HB_CLASS ' }, {   0,   1 }, {   0, ' ; ' }, {   0,   6 }, {   0, ' function ' }, {   0,   1 }, {   0, '() ; static s_oClass ; local MetaClass,nScope := HB_OO_CLSTP_EXPORTED ; if s_oClass == NIL ; s_oClass := IIF(' }, {   0,   2 }, {   0, ', ' }, {   0,   2 }, {   0, ' ,HBClass():new( ' }, {   0,   1 }, {   0, ' , __HB_CLS_PAR ( ' }, {   4,   4 }, {   0, '' }, {   5, ' ,' }, {   5,   5 }, {   0, ' ) ) ) ; #undef  _CLASS_NAME_ ; #define _CLASS_NAME_ ' }, {   0,   1 }, {   0, ' ; #undef  _CLASS_MODE_ ; #define _CLASS_MODE_ _CLASS_DECLARATION_ ; #xtranslate CLSMETH ' }, ;
         {   0,   1 }, {   0, ' <MethodName> => @' }, {   0,   1 }, {   0, '_<MethodName> ; #xtranslate DECLCLASS ' }, {   0,   1 }, {   0, ' => ; ' }, {   5, ' ; #translate Super( ' }, {   5,   5 }, {   5, ' ) : => ::' }, {   5,   5 }, {   5, ': ' }, {   0, '' }, {   4, ' ; #translate Super( ' }, {   4,   4 }, {   4, ' ) : => ::' }, {   4,   4 }, {   4, ': ' }, {   0, '' }, {   4, ' ; #translate Super() : => ::' }, {   4,   4 }, {   4, ': ' }, {   0, '' }, {   4, ' ; #translate Super : => ::' }, {   4,   4 }, {   4, ': ' }, {   0, '' }, {   4, ' ; #translate ::Super : => ::' }, {   4,   4 }, {   4, ': ' }, ;
         {   0, '' }, {   4, ' ; REQUEST ' }, {   4,   4 }, {   0, '' }, {   5, ' ,' }, {   5,   5 } }, { -1,  1, -1,  1, -1,  1, -1,  6, -1,  4, -1,  4, -1,  4, -1, -1,  4, -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1,  1, -1, -1, -1,  1, -1,  1, -1, -1, -1,  1, -1, -1, -1,  1, -1, -1, -1,  1, -1, -1, -1,  1, -1, -1,  1} , { NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER {' }, {   2, 'AS ' }, {   2,   2 }, {   0, ' ' }, {   0,   1 }, {   0, '} ; s_oClass:AddMultiData( ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', HBCLSCHOICE( ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ' ) + iif( ' }, {   0,   7 }, {   0, ', HB_OO_CLSTP_READONLY, 0 ), {' }, {   0,   1 }, {   0, '}, __HB_CLS_NOINI, ' }, {   0,   8 }, {   0, ' )' } }, { -1, -1,  1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  6, -1,  6, -1,  6, -1,  4, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER {' }, {   2, 'AS ' }, {   2,   2 }, {   0, ' ' }, {   0,   1 }, {   0, '} ; s_oClass:AddMultiData( ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', HBCLSCHOICE( ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ' ) + iif( ' }, {   0,   7 }, {   0, ', HB_OO_CLSTP_READONLY, 0 ), {' }, {   0,   1 }, {   0, '}, __HB_CLS_NOINI, ' }, {   0,   8 }, {   0, ' )' } }, { -1, -1,  1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  6, -1,  6, -1,  6, -1,  4, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER {' }, {   2, 'AS ' }, {   2,   2 }, {   0, ' ' }, {   0,   1 }, {   0, '} ; s_oClass:AddInline( ' }, {   0,   1 }, {   0, ', {|Self| Self:' }, {   0,   3 }, {   0, ':' }, {   0,   1 }, {   0, ' }, HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY ) ; s_oClass:AddInline( "_" + ' }, {   0,   1 }, {   0, ', {|Self, param| Self:' }, {   0,   3 }, {   0, ':' }, {   0,   1 }, {   0, ' := param }, HB_OO_CLSTP_EXPORTED )' } }, { -1, -1,  1, -1,  1, -1,  4, -1,  1, -1,  1, -1,  4, -1,  1, -1,  1, -1} , { NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER {' }, {   2, 'AS ' }, {   2,   2 }, {   0, ' ' }, {   0,   1 }, {   0, '} ; s_oClass:AddInline( ' }, {   0,   1 }, {   0, ', {|Self| Self:' }, {   0,   4 }, {   0, ':' }, {   0,   3 }, {   0, ' }, HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY ) ; s_oClass:AddInline( "_" + ' }, {   0,   1 }, {   0, ', {|Self, param| Self:' }, {   0,   4 }, {   0, ':' }, {   0,   3 }, {   0, ' := param }, HB_OO_CLSTP_EXPORTED )' } }, { -1, -1,  1, -1,  1, -1,  4, -1,  1, -1,  1, -1,  4, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER {' }, {   2, 'AS ' }, {   2,   2 }, {   0, ' ' }, {   0,   1 }, {   0, '} ; s_oClass:AddInline( ' }, {   0,   1 }, {   0, ', {|Self| Self:' }, {   0,   3 }, {   0, ' }, HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY ) ; s_oClass:AddInline( "_" + ' }, {   0,   1 }, {   0, ', {|Self, param| Self:' }, {   0,   3 }, {   0, ' := param }, HB_OO_CLSTP_EXPORTED )' } }, { -1, -1,  1, -1,  1, -1,  4, -1,  1, -1,  4, -1,  1, -1} , { NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 's_oClass:AddInline( ' }, {   0,   1 }, {   0, ', {|Self| Self:' }, {   0,   3 }, {   0, ':' }, {   0,   2 }, {   0, ' }, HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY ) ; s_oClass:AddInline( "_" + ' }, {   0,   1 }, {   0, ', {|Self, param| Self:' }, {   0,   3 }, {   0, ':' }, {   0,   2 }, {   0, ' := param }, HB_OO_CLSTP_EXPORTED )' } }, { -1,  4, -1,  1, -1,  1, -1,  4, -1,  1, -1,  1, -1} , { NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '() ' }, {   2, 'AS ' }, {   2,   2 }, {   0, '; s_oClass:AddVirtual( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1, -1,  1, -1,  4, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER {' }, {   2, 'AS ' }, {   2,   2 }, {   0, ' ' }, {   0,   1 }, {   0, '} ; s_oClass:AddMultiData( ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', HB_OO_CLSTP_EXPORTED + iif( ' }, {   0,   4 }, {   0, ', HB_OO_CLSTP_READONLY, 0 ), {' }, {   0,   1 }, {   0, '}, __HB_CLS_NOINI, ' }, {   0,   5 }, {   0, ' )' } }, { -1, -1,  1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  4, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER {' }, {   2, 'AS ' }, {   2,   2 }, {   0, ' ' }, {   0,   1 }, {   0, '} ; s_oClass:AddMultiData( ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', HB_OO_CLSTP_EXPORTED + iif( ' }, {   0,   4 }, {   0, ', HB_OO_CLSTP_READONLY, 0 ), {' }, {   0,   1 }, {   0, '}, __HB_CLS_NOINI, ' }, {   0,   5 }, {   0, ' )' } }, { -1, -1,  1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  4, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER {' }, {   2, 'AS ' }, {   2,   2 }, {   0, ' ' }, {   0,   1 }, {   0, '} ; s_oClass:AddMultiData( ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', HB_OO_CLSTP_PROTECTED + iif( ' }, {   0,   4 }, {   0, ', HB_OO_CLSTP_READONLY, 0 ), {' }, {   0,   1 }, {   0, '}, __HB_CLS_NOINI, ' }, {   0,   5 }, {   0, ' )' } }, { -1, -1,  1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  4, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER {' }, {   2, 'AS ' }, {   2,   2 }, {   0, ' ' }, {   0,   1 }, {   0, '} ; s_oClass:AddMultiData( ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', HB_OO_CLSTP_PROTECTED + iif( ' }, {   0,   4 }, {   0, ', HB_OO_CLSTP_READONLY, 0 ), {' }, {   0,   1 }, {   0, '}, __HB_CLS_NOINI, ' }, {   0,   5 }, {   0, ' )' } }, { -1, -1,  1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  4, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER {' }, {   2, 'AS ' }, {   2,   2 }, {   0, ' ' }, {   0,   1 }, {   0, '} ; s_oClass:AddMultiData( ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', HB_OO_CLSTP_HIDDEN + iif( ' }, {   0,   4 }, {   0, ', HB_OO_CLSTP_READONLY, 0 ), {' }, {   0,   1 }, {   0, '}, __HB_CLS_NOINI, ' }, {   0,   5 }, {   0, ' )' } }, { -1, -1,  1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  4, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER {' }, {   2, 'AS ' }, {   2,   2 }, {   0, ' ' }, {   0,   1 }, {   0, '} ; s_oClass:AddMultiData( ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', HB_OO_CLSTP_HIDDEN + iif( ' }, {   0,   4 }, {   0, ', HB_OO_CLSTP_READONLY, 0 ), {' }, {   0,   1 }, {   0, '}, __HB_CLS_NOINI, ' }, {   0,   5 }, {   0, ' )' } }, { -1, -1,  1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  4, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER {' }, {   2, 'AS ' }, {   2,   2 }, {   0, ' ' }, {   0,   1 }, {   0, '} ; s_oClass:AddMultiClsData(' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', HBCLSCHOICE( ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ' ) + iif( ' }, {   0,   7 }, {   0, ', HB_OO_CLSTP_READONLY, 0 ) + iif( ' }, {   0,   8 }, {   0, ', HB_OO_CLSTP_SHARED, 0 ), {' }, {   0,   1 }, {   0, '}, __HB_CLS_NOINI )' } }, { -1, -1,  1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  6, -1,  6, -1,  6, -1,  6, -1,  4, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER {' }, {   2, 'AS ' }, {   2,   2 }, {   0, ' ' }, {   0,   1 }, {   0, '} ; s_oClass:AddMultiClsData(' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', HBCLSCHOICE( ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ' ) + iif( ' }, {   0,   7 }, {   0, ', HB_OO_CLSTP_READONLY, 0 ) + iif( ' }, {   0,   8 }, {   0, ', HB_OO_CLSTP_SHARED, 0 ), {' }, {   0,   1 }, {   0, '}, __HB_CLS_NOINI )' } }, { -1, -1,  1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  6, -1,  6, -1,  6, -1,  6, -1,  4, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER {' }, {   2, 'AS ' }, {   2,   2 }, {   0, ' ' }, {   0,   1 }, {   0, '} ; s_oClass:AddMultiData( ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', HBCLSCHOICE( ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ' ) + iif( ' }, {   0,   7 }, {   0, ', HB_OO_CLSTP_READONLY, 0 ), {' }, {   0,   1 }, {   0, '}, __HB_CLS_NOINI, ' }, {   0,   8 }, {   0, ' )' } }, { -1, -1,  1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  6, -1,  6, -1,  6, -1,  4, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER {' }, {   2, 'AS ' }, {   2,   2 }, {   0, ' ' }, {   0,   1 }, {   0, '} ; s_oClass:AddMultiClsData(' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', HBCLSCHOICE( ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ' ) + iif( ' }, {   0,   7 }, {   0, ', HB_OO_CLSTP_READONLY, 0 ) + HB_OO_CLSTP_SHARED, {' }, {   0,   1 }, {   0, '}, __HB_CLS_NOINI )' } }, { -1, -1,  1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  6, -1,  6, -1,  6, -1,  4, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '() ' }, {   2, 'AS ' }, {   2,   2 }, {   0, '; s_oClass:AddClsMthds( ' }, {   0,   1 }, {   0, ', CLSMETH _CLASS_NAME_ ' }, {   0,   1 }, {   0, '(), HBCLSCHOICE( ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ' ) + iif( ' }, {   0,   6 }, {   0, ', HB_OO_CLSTP_SHARED, 0 ) )' } }, { -1,  1, -1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  6, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'METHOD ' }, {   0,   1 }, {   0, ' CONSTRUCTOR' } }, { -1,  1, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '() ' }, {   2,   2 }, {   2, ' AS CLASS _CLASS_NAME_' }, {   0, '' }, {   3, ' AS ' }, {   3,   3 }, {   0, '; #xcommand METHOD ' }, {   0,   1 }, {   0, ' [([<anyParams,...>])] ' }, {   0, ' DECLCLASS _CLASS_NAME_' }, {   0, ' _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ ' }, {   0,   1 }, {   0, '([<anyParams>]); s_oClass:AddMethod( ' }, {   0,   1 }, {   0, ', CLSMETH _CLASS_NAME_ ' }, {   0,   1 }, {   0, '(), HBCLSCHOICE( ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ' ) + iif( ' }, {   0,   2 }, {   0, ', HB_OO_CLSTP_CTOR, 0 ), ' }, {   0,   7 }, {   0, ' )' } }, { -1,  1, -1,  0, -1, -1, -1,  1, -1,  1, -1, -1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  6, -1,  6, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '(' }, {   2,   2 }, {   0, ') ' }, {   3,   3 }, {   3, ' AS CLASS _CLASS_NAME_' }, {   0, '' }, {   4, ' AS ' }, {   4,   4 }, {   0, '; #xcommand METHOD ' }, {   0,   1 }, {   0, ' [([<anyParams,...>])] ' }, {   0, ' DECLCLASS _CLASS_NAME_' }, {   0, ' _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ ' }, {   0,   1 }, {   0, '([<anyParams>]); s_oClass:AddMethod( ' }, {   0,   1 }, {   0, ', CLSMETH _CLASS_NAME_ ' }, {   0,   1 }, {   0, '(), HBCLSCHOICE( ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ' ) + iif( ' }, {   0,   3 }, {   0, ', HB_OO_CLSTP_CTOR, 0 ), ' }, {   0,   8 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  0, -1, -1, -1,  1, -1,  1, -1, -1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  6, -1,  6, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '() ' }, {   4,   4 }, {   4, ' AS CLASS _CLASS_NAME_' }, {   0, '' }, {   2, ' AS ' }, {   2,   2 }, {   0, '; s_oClass:AddInline( ' }, {   0,   1 }, {   0, ', ' }, {   0,   3 }, {   0, ', HBCLSCHOICE( ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ' ) + iif( ' }, {   0,   4 }, {   0, ', HB_OO_CLSTP_CTOR, 0 ), ' }, {   0,   8 }, {   0, ' )' } }, { -1,  1, -1,  0, -1, -1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  6, -1,  6, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '() ' }, {   4,   4 }, {   4, ' AS CLASS _CLASS_NAME_' }, {   0, '' }, {   2, ' AS ' }, {   2,   2 }, {   0, '; s_oClass:AddMethod( ' }, {   0,   1 }, {   0, ', @' }, {   0,   3 }, {   0, '(), HBCLSCHOICE( ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ' ) + iif( ' }, {   0,   4 }, {   0, ', HB_OO_CLSTP_CTOR, 0 ), ' }, {   0,   8 }, {   0, ' )' } }, { -1,  1, -1,  0, -1, -1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  6, -1,  6, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '() ' }, {   4,   4 }, {   4, ' AS CLASS _CLASS_NAME_' }, {   0, '' }, {   2, ' AS ' }, {   2,   2 }, {   0, '; s_oClass:AddInline( ' }, {   0,   1 }, {   0, ', {|Self | ' }, {   0,   3 }, {   0, ' }, HBCLSCHOICE( ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ' ) + iif( ' }, {   0,   4 }, {   0, ', HB_OO_CLSTP_CTOR, 0 ), ' }, {   0,   8 }, {   0, ' )' } }, { -1,  1, -1,  0, -1, -1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  6, -1,  6, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '(' }, {   2,   2 }, {   0, ') ' }, {   5,   5 }, {   5, ' AS CLASS _CLASS_NAME_' }, {   0, '' }, {   3, ' AS ' }, {   3,   3 }, {   0, '; s_oClass:AddInline( ' }, {   0,   1 }, {   0, ', {|Self ' }, {   2, ',' }, {   2,   2 }, {   0, ' | ' }, {   0,   4 }, {   0, ' }, HBCLSCHOICE( ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ' ) + iif( ' }, {   0,   5 }, {   0, ', HB_OO_CLSTP_CTOR, 0 ), ' }, {   0,   9 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  0, -1, -1, -1,  1, -1,  4, -1, -1,  1, -1,  1, -1,  6, -1,  6, -1,  6, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'METHOD ' }, {   0,   1 }, {   0, '' }, {   2, ' AS ' }, {   2,   2 }, {   0, ' BLOCK {|Self ' }, {   3, ' ,' }, {   3,   3 }, {   0, ' | ' }, {   0,   4 }, {   0, ' } ' }, {   5,   5 } }, { -1,  1, -1, -1,  1, -1, -1,  1, -1,  1, -1,  1} , { NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'METHOD ' }, {   0,   1 }, {   0, '' }, {   3, ' AS ' }, {   3,   3 }, {   0, ' BLOCK {|Self ' }, {   2, ' ,' }, {   2,   2 }, {   0, '' }, {   4, ' ,' }, {   4,   4 }, {   0, ' | ' }, {   0,   5 }, {   0, ' } ' }, {   6,   6 } }, { -1,  1, -1, -1,  1, -1, -1,  1, -1, -1,  1, -1,  1, -1,  1} , { NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '() ' }, {   2, 'AS ' }, {   2,   2 }, {   0, '; s_oClass:AddVirtual( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1, -1,  1, -1,  4, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '()  ' }, {   2, 'AS ' }, {   2,   2 }, {   0, '; #xcommand METHOD ' }, {   0,   1 }, {   0, ' [([<anyParams,...>])] ' }, {   0, ' DECLCLASS _CLASS_NAME_' }, {   0, ' _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ ' }, {   0,   1 }, {   0, '([<anyParams>]); s_oClass:AddMethod( ' }, {   0,   1 }, {   0, ', CLSMETH _CLASS_NAME_ ' }, {   0,   1 }, {   0, '(), HBCLSCHOICE( ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ' ) ) ; s_oClass:AddInline( ' }, {   0,   3 }, {   0, ', {|Self [,<params>] | Self:' }, {   0,   1 }, {   0, '( [<params>] ) }, HBCLSCHOICE( ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ' ) )' } }, { -1,  1, -1, -1,  1, -1,  1, -1, -1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  6, -1,  6, -1,  4, -1,  1, -1,  6, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '(' }, {   2,   2 }, {   0, ')  ' }, {   3, 'AS ' }, {   3,   3 }, {   0, '; #xcommand METHOD ' }, {   0,   1 }, {   0, ' [([<anyParams,...>])] ' }, {   2, ' DECLCLASS _CLASS_NAME_' }, {   0, ' _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ ' }, {   0,   1 }, {   0, '([<anyParams>]); s_oClass:AddMethod( ' }, {   0,   1 }, {   0, ', CLSMETH _CLASS_NAME_ ' }, {   0,   1 }, {   0, '(), HBCLSCHOICE( ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ' ) ) ; s_oClass:AddInline( ' }, {   0,   4 }, {   0, ', {|Self ' }, {   2, ',' }, {   2,   2 }, {   0, ' | Self:' }, {   0,   1 }, {   0, '( ' }, {   2,   2 }, {   0, ' ) }, HBCLSCHOICE( ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ' ) )' } }, ;
         { -1,  1, -1,  1, -1, -1,  1, -1,  1, -1, -1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  6, -1,  6, -1,  4, -1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '() ' }, {   4,   4 }, {   4, ' AS CLASS _CLASS_NAME_' }, {   0, '' }, {   2, ' AS ' }, {   2,   2 }, {   0, '; #xcommand METHOD ' }, {   0,   3 }, {   0, ' [([<anyParams,...>])] ' }, {   0, ' DECLCLASS _CLASS_NAME_' }, {   0, ' _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ ' }, {   0,   3 }, {   0, '([<anyParams>]); s_oClass:AddMethod( ' }, {   0,   1 }, {   0, ', CLSMETH _CLASS_NAME_ ' }, {   0,   3 }, {   0, '(), HBCLSCHOICE( ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ' ) + iif( ' }, {   0,   4 }, {   0, ', HB_OO_CLSTP_CTOR, 0 ) )' } }, { -1,  1, -1,  0, -1, -1, -1,  1, -1,  1, -1, -1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  6, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '(' }, {   2,   2 }, {   0, ') ' }, {   5,   5 }, {   5, ' AS CLASS _CLASS_NAME_' }, {   0, '' }, {   3, ' AS ' }, {   3,   3 }, {   0, '; #xcommand METHOD ' }, {   0,   4 }, {   0, ' [([<anyParams,...>])] ' }, {   0, ' DECLCLASS _CLASS_NAME_' }, {   0, ' _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ ' }, {   0,   4 }, {   0, '([<anyParams>]); s_oClass:AddMethod( ' }, {   0,   1 }, {   0, ', CLSMETH _CLASS_NAME_ ' }, {   0,   4 }, {   0, '(), HBCLSCHOICE( ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ' ) + iif( ' }, {   0,   5 }, {   0, ', HB_OO_CLSTP_CTOR, 0 ) )' } }, { -1,  1, -1,  1, -1,  0, -1, -1, -1,  1, -1,  1, -1, -1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  6, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '(' }, {   4,   4 }, {   0, ') ' }, {   5,   5 }, {   5, ' AS CLASS _CLASS_NAME_' }, {   0, '' }, {   2, ' AS ' }, {   2,   2 }, {   0, '; #xcommand METHOD ' }, {   0,   3 }, {   0, ' [([<anyParams,...>])] ' }, {   0, ' DECLCLASS _CLASS_NAME_' }, {   0, ' _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ ' }, {   0,   3 }, {   0, '([<anyParams>]); s_oClass:AddMethod( ' }, {   0,   1 }, {   0, ', CLSMETH _CLASS_NAME_ ' }, {   0,   3 }, {   0, '(), HBCLSCHOICE( ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ' ) + iif( ' }, {   0,   5 }, {   0, ', HB_OO_CLSTP_CTOR, 0 ) )' } }, { -1,  1, -1,  1, -1,  0, -1, -1, -1,  1, -1,  1, -1, -1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  6, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '(' }, {   5,   5 }, {   0, ') ' }, {   2,   2 }, {   0, '' }, {   6,   6 }, {   6, ' AS CLASS _CLASS_NAME_' }, {   0, '' }, {   3, ' AS ' }, {   3,   3 }, {   0, '; #xcommand METHOD ' }, {   0,   4 }, {   0, ' [([<anyParams,...>])] ' }, {   0, ' DECLCLASS _CLASS_NAME_' }, {   0, ' _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ ' }, {   0,   4 }, {   0, '([<anyParams>]); s_oClass:AddMethod( ' }, {   0,   1 }, {   0, ', CLSMETH _CLASS_NAME_ ' }, {   0,   4 }, {   0, '(), HBCLSCHOICE( ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ' ) + iif( ' }, {   0,   6 }, {   0, ', HB_OO_CLSTP_CTOR, 0 ) )' } }, { -1,  1, -1,  1, -1,  0, -1,  0, -1, -1, -1,  1, -1,  1, -1, -1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  6, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '() ' }, {   2, 'AS ' }, {   2,   2 }, {   0, '; s_oClass:AddInline( ' }, {   0,   1 }, {   0, ', {|Self| Self:' }, {   0,   3 }, {   0, ':' }, {   0,   1 }, {   0, '() } )' } }, { -1,  1, -1, -1,  1, -1,  4, -1,  1, -1,  1, -1} , { NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '(' }, {   2,   2 }, {   0, ') ' }, {   3, 'AS ' }, {   3,   3 }, {   0, '; s_oClass:AddInline( ' }, {   0,   1 }, {   0, ', {|Self ' }, {   2, ',' }, {   2,   2 }, {   0, '| Self:' }, {   0,   4 }, {   0, ':' }, {   0,   1 }, {   0, '( ' }, {   2,   2 }, {   0, ' ) } )' } }, { -1,  1, -1,  1, -1, -1,  1, -1,  4, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '() ' }, {   2, 'AS ' }, {   2,   2 }, {   0, '; s_oClass:AddInline( ' }, {   0,   1 }, {   0, ', {|Self| Self:' }, {   0,   4 }, {   0, ':' }, {   0,   3 }, {   0, '() } )' } }, { -1,  1, -1, -1,  1, -1,  4, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '() ' }, {   2, 'AS ' }, {   2,   2 }, {   0, '; s_oClass:AddInline( ' }, {   0,   1 }, {   0, ', {|Self ' }, {   4, ',' }, {   4,   4 }, {   0, '| Self:' }, {   0,   5 }, {   0, ':' }, {   0,   3 }, {   0, '( ' }, {   4,   4 }, {   0, ' ) } )' } }, { -1,  1, -1, -1,  1, -1,  4, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '(' }, {   2,   2 }, {   0, ') ' }, {   3, 'AS ' }, {   3,   3 }, {   0, '; s_oClass:AddInline( ' }, {   0,   1 }, {   0, ', {|Self ' }, {   2, ',' }, {   2,   2 }, {   0, '| Self:' }, {   0,   5 }, {   0, ':' }, {   0,   4 }, {   0, '( ' }, {   2,   2 }, {   0, ' ) } )' } }, { -1,  1, -1,  1, -1, -1,  1, -1,  4, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '(' }, {   2,   2 }, {   0, ') ' }, {   3, 'AS ' }, {   3,   3 }, {   0, '; s_oClass:AddInline( ' }, {   0,   1 }, {   0, ', {|Self ' }, {   2, ',' }, {   2,   2 }, {   0, '| Self:' }, {   0,   6 }, {   0, ':' }, {   0,   4 }, {   0, '( ' }, {   2,   2 }, {   0, ' ) } )' } }, { -1,  1, -1,  1, -1, -1,  1, -1,  4, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'MESSAGE ' }, {   0,   1 }, {   0, '' }, {   2, ' AS ' }, {   2,   2 }, {   0, ' METHOD ' }, {   0,   3 }, {   0, '' }, {   4,   4 } }, { -1,  1, -1, -1,  1, -1,  1, -1,  1} , { NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '() ' }, {   2, 'AS ' }, {   2,   2 }, {   0, '; s_oClass:AddInline( ' }, {   0,   1 }, {   0, ', {|Self| Self:' }, {   0,   3 }, {   0, ':' }, {   0,   1 }, {   0, ' } )' } }, { -1,  1, -1, -1,  1, -1,  4, -1,  1, -1,  1, -1} , { NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '(' }, {   2,   2 }, {   0, ') ' }, {   3, 'AS ' }, {   3,   3 }, {   0, '; s_oClass:AddInline( ' }, {   0,   1 }, {   0, ', {|Self ' }, {   2, ',' }, {   2,   2 }, {   0, '| Self:' }, {   0,   4 }, {   0, ':' }, {   0,   1 }, {   0, '( ' }, {   2,   2 }, {   0, ' ) } )' } }, { -1,  1, -1,  1, -1, -1,  1, -1,  4, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '() ' }, {   2, 'AS ' }, {   2,   2 }, {   0, '; s_oClass:AddInline( ' }, {   0,   1 }, {   0, ', {|Self| Self:' }, {   0,   3 }, {   0, ':' }, {   0,   1 }, {   0, ' } )' } }, { -1,  1, -1, -1,  1, -1,  4, -1,  1, -1,  1, -1} , { NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '(' }, {   2,   2 }, {   0, ') ' }, {   3, 'AS ' }, {   3,   3 }, {   0, '; s_oClass:AddInline( ' }, {   0,   1 }, {   0, ', {|Self ' }, {   2, ',' }, {   2,   2 }, {   0, '| Self:' }, {   0,   4 }, {   0, ':' }, {   0,   1 }, {   0, '( ' }, {   2,   2 }, {   0, ' ) } )' } }, { -1,  1, -1,  1, -1, -1,  1, -1,  4, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '() ' }, {   2, 'AS ' }, {   2,   2 }, {   0, '; _HB_MEMBER _' }, {   0,   1 }, {   0, '() ' }, {   2, 'AS ' }, {   2,   2 }, {   0, '; #xcommand METHOD ' }, {   0,   1 }, {   0, ' [([<anyParams,...>])] ' }, {   0, ' DECLCLASS _CLASS_NAME_' }, {   0, ' _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ ' }, {   0,   1 }, {   0, '([<anyParams>]); s_oClass:AddMethod( ' }, {   0,   1 }, {   0, ', CLSMETH _CLASS_NAME_ ' }, {   0,   1 }, {   0, '(), HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY , ' }, {   0,   3 }, {   0, ' ) ; s_oClass:AddMethod( "_" + ' }, {   0,   1 }, {   0, ', CLSMETH _CLASS_NAME_ ' }, {   0,   1 }, {   0, '() )' } }, { -1,  1, -1, -1,  1, -1,  1, -1, -1,  1, -1,  1, -1, -1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  4, -1,  1, -1} , { NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '(' }, {   2,   2 }, {   0, ') ' }, {   3, 'AS ' }, {   3,   3 }, {   0, '; _HB_MEMBER _' }, {   0,   1 }, {   0, '(' }, {   2,   2 }, {   0, ') ' }, {   3, 'AS ' }, {   3,   3 }, {   0, '; #xcommand METHOD ' }, {   0,   1 }, {   0, ' [([<anyParams,...>])] ' }, {   0, ' DECLCLASS _CLASS_NAME_' }, {   0, ' _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ ' }, {   0,   1 }, {   0, '([<anyParams>]); s_oClass:AddMethod( ' }, {   0,   1 }, {   0, ', CLSMETH _CLASS_NAME_ ' }, {   0,   1 }, {   0, '(), HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY, ' }, {   0,   4 }, {   0, ' ) ; s_oClass:AddMethod( "_" + ' }, {   0,   1 }, {   0, ', CLSMETH _CLASS_NAME_ ' }, {   0,   1 }, {   0, '() )' } }, { -1,  1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1,  1, -1, -1, -1,  1, -1,  4, -1,  1, -1,  6, -1,  4, -1,  1, -1} , { NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '() ' }, {   2, 'AS ' }, {   2,   2 }, {   0, '; #xcommand METHOD ' }, {   0,   1 }, {   0, ' [([<anyParams,...>])] ' }, {   0, ' DECLCLASS _CLASS_NAME_' }, {   0, ' _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ ' }, {   0,   1 }, {   0, '([<anyParams>]); s_oClass:AddMethod( ' }, {   0,   1 }, {   0, ', CLSMETH _CLASS_NAME_ ' }, {   0,   1 }, {   0, '(), HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY , ' }, {   0,   3 }, {   0, ' )' } }, { -1,  1, -1, -1,  1, -1,  1, -1, -1, -1,  1, -1,  4, -1,  1, -1,  6, -1} , { NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '(' }, {   2,   2 }, {   0, ') ' }, {   3, 'AS ' }, {   3,   3 }, {   0, '; #xcommand METHOD ' }, {   0,   1 }, {   0, ' [([<anyParams,...>])] ' }, {   0, ' DECLCLASS _CLASS_NAME_' }, {   0, ' _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ ' }, {   0,   1 }, {   0, '([<anyParams>]); s_oClass:AddMethod( ' }, {   0,   1 }, {   0, ', CLSMETH _CLASS_NAME_ ' }, {   0,   1 }, {   0, '(), HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY , ' }, {   0,   4 }, {   0, ' )' } }, { -1,  1, -1,  1, -1, -1,  1, -1,  1, -1, -1, -1,  1, -1,  4, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '() ' }, {   2, 'AS ' }, {   2,   2 }, {   0, '; s_oClass:AddInline( ' }, {   0,   1 }, {   0, ', {|Self ' }, {   3, ',' }, {   3,   3 }, {   0, ' | ' }, {   0,   4 }, {   0, ' }, HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY , ' }, {   0,   5 }, {   0, ' )' } }, { -1,  1, -1, -1,  1, -1,  4, -1, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '() ' }, {   2, 'AS ' }, {   2,   2 }, {   0, '; s_oClass:AddVirtual( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1, -1,  1, -1,  4, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER _' }, {   0,   1 }, {   0, '() ' }, {   2, 'AS ' }, {   2,   2 }, {   0, '; #xcommand METHOD ' }, {   0,   1 }, {   0, ' [([<anyParams,...>])] ' }, {   0, ' DECLCLASS _CLASS_NAME_' }, {   0, ' _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ ' }, {   0,   1 }, {   0, '([<anyParams>]); s_oClass:AddMethod( "_" + ' }, {   0,   1 }, {   0, ', CLSMETH _CLASS_NAME_ _' }, {   0,   1 }, {   0, '(), HB_OO_CLSTP_EXPORTED )' } }, { -1,  1, -1, -1,  1, -1,  1, -1, -1, -1,  1, -1,  4, -1,  1, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER _' }, {   0,   1 }, {   0, '(' }, {   2,   2 }, {   0, ') ' }, {   3, 'AS ' }, {   3,   3 }, {   0, '; #xcommand METHOD ' }, {   0,   1 }, {   0, ' [([<anyParams,...>])] ' }, {   0, ' DECLCLASS _CLASS_NAME_' }, {   0, ' _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ ' }, {   0,   1 }, {   0, '([<anyParams>]); s_oClass:AddMethod( "_" + ' }, {   0,   1 }, {   0, ', CLSMETH _CLASS_NAME_ _' }, {   0,   1 }, {   0, '(), HB_OO_CLSTP_EXPORTED )' } }, { -1,  1, -1,  1, -1, -1,  1, -1,  1, -1, -1, -1,  1, -1,  4, -1,  1, -1} , { NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER _' }, {   0,   1 }, {   0, '(' }, {   2,   2 }, {   0, ') ' }, {   3, 'AS ' }, {   3,   3 }, {   0, '; s_oClass:AddInline( "_" + ' }, {   0,   1 }, {   0, ', {|Self ' }, {   2, ',' }, {   2,   2 }, {   0, '' }, {   4, ' ,' }, {   4,   4 }, {   0, ' | ' }, {   0,   5 }, {   0, ' }, HB_OO_CLSTP_EXPORTED )' } }, { -1,  1, -1,  1, -1, -1,  1, -1,  4, -1, -1,  1, -1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'ERROR HANDLER ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '(); #xcommand METHOD ' }, {   0,   1 }, {   0, ' [([<anyParams,...>])] ' }, {   0, ' DECLCLASS _CLASS_NAME_' }, {   0, ' _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ ' }, {   0,   1 }, {   0, '([<anyParams>]); s_oClass:SetOnError( CLSMETH _CLASS_NAME_ ' }, {   0,   1 }, {   0, '() )' } }, { -1,  1, -1,  1, -1, -1, -1,  1, -1,  1, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, '_HB_MEMBER ' }, {   0,   1 }, {   0, '(' }, {   2,   2 }, {   0, '); #xcommand METHOD ' }, {   0,   1 }, {   0, ' [([<anyParams,...>])] ' }, {   0, ' DECLCLASS _CLASS_NAME_' }, {   0, ' _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ ' }, {   0,   1 }, {   0, '([<anyParams>]); s_oClass:SetOnError( CLSMETH _CLASS_NAME_ ' }, {   0,   1 }, {   0, '() )' } }, { -1,  1, -1,  1, -1,  1, -1, -1, -1,  1, -1,  1, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '; s_oClass:Create() ; endif ; return s_oClass:Instance() AS CLASS _CLASS_NAME_ ; #undef  _CLASS_MODE_ ; #define _CLASS_MODE_ _CLASS_IMPLEMENTATION_' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'METHOD ' }, {   0,   1 }, {   0, '  _CLASS_MODE_' } }, { -1,  1, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'METHOD ' }, {   0,   1 }, {   0, '  CLASS ' }, {   0,   2 }, {   0, '  _CLASS_MODE_' } }, { -1,  1, -1,  1, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'METHOD ' }, {   0,   1 }, {   0, '     CLASS ' }, {   0,   2 }, {   0, ' _CLASS_IMPLEMENTATION_' } }, { -1,  1, -1,  1, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'DECLARED METHOD _CLASS_NAME_ ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'DECLARED METHOD ' }, {   0,   2 }, {   0, ' ' }, {   0,   1 } }, { -1,  1, -1,  1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, '#error Class ' }, {   0,   2 }, {   0, ' not declared for method: ' }, {   0,   1 }, {   0, ' ; function ' }, {   0,   1 }, {   0, ' ; local self := QSelf()' } }, { -1,  3, -1,  1, -1,  1, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'static function DECLMETH ' }, {   0,   1 }, {   0, ' ' }, {   0,   2 }, {   0, ' ; local Self AS CLASS ' }, {   0,   1 }, {   0, ' := QSelf() AS CLASS ' }, {   0,   1 } }, { -1,  1, -1,  1, -1,  1, -1,  1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'static function ' }, {   0,   2 }, {   0, '_' }, {   0,   1 }, {   0, ' ; local Self AS CLASS ' }, {   0,   2 }, {   0, ' := QSelf() AS CLASS ' }, {   0,   2 } }, { -1,  1, -1,  1, -1,  1, -1,  1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'static function ' }, {   0,   2 }, {   0, '__' }, {   0,   1 }, {   0, ' ; local Self AS CLASS ' }, {   0,   2 }, {   0, ' := QSelf() AS CLASS ' }, {   0,   2 } }, { -1,  1, -1,  1, -1,  1, -1,  1} , { NIL, NIL }  } )

 #endif

RETURN .T.
//--------------------------------------------------------------//
INIT PROCEDURE PPInit

   local FileHandle

   FileHandle := FCreate('Trace.Log')
   FClose(FileHandle)

RETURN

//--------------------------------------------------------------//
FUNCTION TraceLog(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15 )

   LOCAL FileHandle, ProcName, Counter := 1, aEntries

   aEntries := {p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15}

   FileHandle := FOpen( 'Trace.Log', 1 )

   FSeek(FileHandle, 0, 2)

   FWrite( FileHandle, '[' + ProcName(1) + '] (' + Str( Procline(1), 5 ) + ') Called from: '  + CRLF )

   DO WHILE ! ( ( ProcName := ProcName( ++Counter ) ) == '' )
      FWrite( FileHandle, space(30) + ProcName + '(' + Str( Procline( Counter), 5 ) + ')' + CRLF )
   ENDDO

   IF ! ( PP_ProcName(0) == "" )
      FWrite( FileHandle, "Interpreter:"  + CRLF )
      Counter := -1
      DO WHILE ! ( ( ProcName := PP_ProcName( ++Counter ) ) == "" )
         FWrite( FileHandle, space(30) + ProcName + '(' + Str( PP_Procline( Counter), 5 ) + ')' + CRLF )
      ENDDO
   ENDIF

   FOR Counter := 1 to PCount()
      FWrite( FileHandle, '>>>' + xToStr( aEntries[Counter] ) + '<<<' + CRLF )
   NEXT

   FWrite( FileHandle, CRLF )

   FClose(FileHandle)

RETURN .T.

//--------------------------------------------------------------//
FUNCTION xToStr( xExp )

   LOCAL cType

   IF xExp == NIL
      RETURN 'NIL'
   ENDIF

   cType := ValType( xExp )

   DO CASE
      CASE cType = 'C'
         RETURN xExp

      CASE cType = 'D'
         RETURN dToc( xExp )

      CASE cType = 'L'
         RETURN IIF( xExp, '.T.', '.F.' )

      CASE cType = 'N'
         RETURN Str( xExp )

      CASE cType = 'M'
         RETURN xExp

      CASE cType = 'A'
         RETURN "{ Array of " +  LTrim( Str( Len( xExp ) ) ) + " Items }"

      CASE cType = 'B'
         RETURN '{|| Block }'

      CASE cType = 'O'
         RETURN "{ Object }"

      OTHERWISE
         RETURN "Type: " + cType
   ENDCASE

RETURN ""

//--------------------------------------------------------------//
FUNCTION PP_QSelf( o )

   STATIC s_oSelf
   LOCAL  oPreset := s_oSelf

   IF ValType( o ) == 'O'
      s_oSelf := o
      RETURN oPreset
   ENDIF

RETURN s_oSelf

//--------------------------------------------------------------//

#ifndef USE_C_BOOST

STATIC FUNCTION NextIdentifier( sLine, sSkipped )

   LOCAL nAt, nLen := Len( sLine ), cChar, cLastChar, nStart, sIdentifier, sTmp

   FOR nAt := 1 TO nLen
       cChar := SubStr( sLine, nAt, 1 )

       IF cChar $ ' ,([{|^*/+-=!#<>:&$'
          IF nStart != NIL
             EXIT
          ENDIF
          LOOP // No need to record cLastChar
       ELSEIF cChar $ ')]}'
          IF nStart != NIL
             EXIT
          ENDIF
       ELSEIF cChar $ '"'+"'"
          DO WHILE ( nAt < nLen ) .AND. SubStr( sLine, ++nAt, 1 ) != cChar
          ENDDO
          LOOP // No need to record cLastChar
       ELSEIF cChar == '['
          IF ! ( IsAlpha( cLastChar  ) .OR. IsDigit( cLastChar ) .OR. cLastChar $ "])}_." )
             DO WHILE ( nAt < nLen ) .AND. SubStr( sLine, ++nAt, 1 ) != ']'
             ENDDO
          ENDIF
          cLastChar := ']'
          LOOP // Recorded cLastChar
       ELSEIF cChar == '.'
          IF cLastChar == '_' .OR. IsAlpha( cLastChar )
             EXIT
          ENDIF

          sTmp := Upper( SubStr( sLine, nAt + 1, 4 ) )
          IF sTmp = "T."
             nAt += 2
             LOOP
          ELSEIF sTmp = "F."
             nAt += 2
             LOOP
          ELSEIF sTmp = "OR."
             nAt += 3
             LOOP
          ELSEIF sTmp = "AND."
             nAt += 4
             LOOP
          ELSEIF sTmp = "NOT."
             nAt += 4
             LOOP
          ENDIF
       ELSEIF nStart == NIL .AND. ( IsAlpha( cChar ) .OR. cChar == '_' )
          nStart := nAt
       ENDIF

       cLastChar := cChar
    NEXT

    IF nStart != NIL
       sIdentifier := SubStr( sLine, nStart, nAt - nStart )
       sSkipped    := Left( sLine, nStart - 1 )
       sLine       := SubStr( sLine, nAt )
    ENDIF

    //TraceLog( sIdentifier, sLine, sSkipped, cChar, cLastChar, nStart, nAt, nLen )

RETURN sIdentifier

#endif

//--------------------------------------------------------------//

FUNCTION nAtSkipStr( sFind, sLine, nStart )

   LOCAL nAt, nLen := Len( sLine ), cChar, cLastChar := ' ', sTmp, nLenFind := Len( sFind )

   IF nStart == NIL
      nStart := 1
   ENDIF

   FOR nAt := nStart TO nLen
       IF SubStr( sLine, nAt, nLenFind ) == sFind
          RETURN nAt
       ENDIF

       cChar := SubStr( sLine, nAt, 1 )

       IF cChar $ '"'+"'"
          DO WHILE ( nAt < nLen ) .AND. SubStr( sLine, ++nAt, 1 ) != cChar
          ENDDO
          LOOP // No need to record cLastChar
       ELSEIF cChar == '['
          IF ! ( IsAlpha( cLastChar  ) .OR. IsDigit( cLastChar ) .OR. cLastChar $ "])}_." )
             DO WHILE ( nAt < nLen ) .AND. SubStr( sLine, ++nAt, 1 ) != ']'
             ENDDO
          ENDIF
          cLastChar := ']'
          LOOP // Recorded cLastChar
       ENDIF

       cLastChar := cChar
    NEXT

RETURN 0

//--------------------------------------------------------------//

STATIC FUNCTION InitRunRules()

   /* Defines */
   aAdd( aDefRules, { 'HB_CLS_NO_OO_ERR' ,  , .T. } )

   /* Translates */
   aAdd( aTransRules, { 'AS' , { {    1,   0, NIL, ':', { 'ANYTYPE', 'ARRAY', 'CHARACTER', 'CODEBLOCK', 'DATE', 'LOGICAL', 'NUMERIC', 'OBJECT', 'STRING', 'USUAL' } } } , .F. } )
   aAdd( aTransRules, { 'AS' , { {    0,   0, 'ARRAY', NIL, NIL }, {    1,   0, 'OF', '<', NIL } } , .F. } )
   aAdd( aTransRules, { 'AS' , { {    1,   0, 'CLASS', '!', NIL } } , .F. } )
   aAdd( aTransRules, { 'AS' , { {    1,   0, 'CLASS', '!', NIL }, {    0,   0, ':=', NIL, NIL } } , .F. } )
   aAdd( aTransRules, { 'QSELF' , { {    0,   0, '(', NIL, NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
   aAdd( aTransRules, { 'ADDMETHOD' , { {    1,   0, '(', '<', NIL }, {    0,   0, ',', NIL, NIL }, {    2,   0, '@', '!', NIL }, {    0,   0, '(', NIL, NIL }, {    0,   0, ')', NIL, NIL }, {    3,   0, ',', '<', NIL }, {    4,   0, ',', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
   aAdd( aTransRules, { ':' , { {    0,   0, ':', NIL, NIL } } , .F. } )
   aAdd( aTransRules, { '_GET_' , { {    1,   0, '(', '<', NIL }, {    2,   0, ',', '!', NIL }, {    0,   0, ',', NIL, NIL }, {    3,   1, NIL, '<', { ',' } }, {    0,   0, ',', NIL, NIL }, {    4,   1, NIL, '<', { ',' } }, {    0,   0, ',', NIL, NIL }, {    5,   1, NIL, '<', { ')' } }, {    0,   0, ')', NIL, NIL } } , .F. } )
   aAdd( aTransRules, { '__GET' , { {    1,   0, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL }, {    0,   0, ':', NIL, NIL }, {    0,   0, 'DISPLAY', NIL, NIL }, {    0,   0, '(', NIL, NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
   aAdd( aTransRules, { 'PROCNAME' , { {    0,   0, '(', NIL, NIL }, {    1,   1, NIL, '<', { ')' } }, {    0,   0, ')', NIL, NIL } } , .F. } )
   aAdd( aTransRules, { 'PROCLINE' , { {    0,   0, '(', NIL, NIL }, {    1,   1, NIL, '<', { ')' } }, {    0,   0, ')', NIL, NIL } } , .F. } )

   /* Commands */
 #ifdef WIN
   aAdd( aCommRules, { 'ALERT' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
 #endif
   aAdd( aCommRules, { '_HB_CLASS' , { {    1,   0, NIL, '*', NIL } } , .F. } )
   aAdd( aCommRules, { '_HB_MEMBER' , { {    1,   0, NIL, '*', NIL } } , .F. } )
   aAdd( aCommRules, { 'MEMVAR' , { {    1,   0, NIL, '*', NIL } } , .F. } )
   aAdd( aCommRules, { 'EXTERNAL' , { {    1,   0, NIL, '!', NIL }, {    2,   1, ',', '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'DECLARE' , { {    1,   0, NIL, '!', NIL }, {    2,   0, NIL, '<', NIL }, {    3,   0, NIL, '*', NIL } } , .F. } )
   aAdd( aCommRules, { 'IF' , { {    1,   0, NIL, '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'ELSEIF' , { {    1,   0, NIL, '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'ELSE' ,  , .F. } )
   aAdd( aCommRules, { 'ENDIF' , { {    1,   1, NIL, '*', NIL } } , .F. } )
   aAdd( aCommRules, { 'END' , { {    1,   1, NIL, '*', NIL } } , .F. } )
   aAdd( aCommRules, { 'DO' , { {    0,   0, 'CASE', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'CASE' , { {    1,   0, NIL, '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'OTHERWISE' ,  , .F. } )
   aAdd( aCommRules, { 'ENDCASE' , { {    1,   1, NIL, '*', NIL } } , .F. } )
   aAdd( aCommRules, { 'FOR' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ':=', '<', NIL }, {    3,   0, 'TO', '<', NIL }, {    4,   1, 'STEP', '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'FOR' , { {    1,   0, NIL, '<', NIL }, {    2,   0, '=', '<', NIL }, {    3,   0, 'TO', '<', NIL }, {    4,   1, 'STEP', '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'LOOP' , { {    1,   1, NIL, '*', NIL } } , .F. } )
   aAdd( aCommRules, { 'EXIT' , { {    1,   1, NIL, '*', NIL } } , .F. } )
   aAdd( aCommRules, { 'NEXT' , { {    1,   1, NIL, '*', NIL } } , .F. } )
   aAdd( aCommRules, { 'DO' , { {    1,   0, 'WHILE', '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'WHILE' , { {    1,   0, NIL, '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'ENDDO' , { {    1,   1, NIL, '*', NIL } } , .F. } )
   aAdd( aCommRules, { 'DO' , { {    1,   0, NIL, '(', NIL }, {    0,   0, '.', NIL, NIL }, {    0,   0, 'PRG', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'INIT' , { {    1,   0, 'PROCEDURE', '!', NIL }, {    0,   1, '(', NIL, NIL }, {    0,  -1, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'EXIT' , { {    1,   0, 'PROCEDURE', '!', NIL }, {    0,   1, '(', NIL, NIL }, {    0,  -1, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'STATIC' , { {    1,   0, 'PROCEDURE', '!', NIL }, {    2,   0, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'STATIC' , { {    1,   0, 'PROCEDURE', '!', NIL }, {    0,   1, '(', NIL, NIL }, {    0,  -1, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'STATIC' , { {    1,   0, 'FUNCTION', '!', NIL }, {    2,   0, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'STATIC' , { {    1,   0, 'FUNCTION', '!', NIL }, {    0,   1, '(', NIL, NIL }, {    0,  -1, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'PROCEDURE' , { {    1,   0, NIL, '!', NIL }, {    2,   0, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'PROCEDURE' , { {    1,   0, NIL, '!', NIL }, {    0,   1, '(', NIL, NIL }, {    0,  -1, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'FUNCTION' , { {    1,   0, NIL, '!', NIL }, {    2,   0, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'FUNCTION' , { {    1,   0, NIL, '!', NIL }, {    0,   1, '(', NIL, NIL }, {    0,  -1, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'RETURN' , { {    1,   1, NIL, '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'PARAMETERS' , { {    1,   0, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'PRIVATE' , { {    1,   0, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'DECLARE' , { {    1,   0, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'PUBLIC' , { {    1,   0, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'LOCAL' , { {    1,   0, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'STATIC' , { {    1,   0, NIL, 'A', NIL } } , .F. } )

RETURN .T.

//--------------------------------------------------------------//

STATIC FUNCTION InitRunResults()

   /* Defines Results*/
   aAdd( aDefResults, { { {   0, '1' } }, { -1} , { }  } )

   /* Translates Results*/
   aAdd( aTransResults, { , , { NIL }  } )
   aAdd( aTransResults, { , , { NIL }  } )
   aAdd( aTransResults, { , , { NIL }  } )
   aAdd( aTransResults, { { {   0, ':=' } }, { -1} , { NIL }  } )
   aAdd( aTransResults, { { {   0, 'PP_Qself()' } }, { -1} ,  } )
   aAdd( aTransResults, { { {   0, 'AddInLine( ' }, {   0,   1 }, {   0, ', {|Self,p1,p2,p3,p4,p5,p6,p7,p8,p9| PP_QSelf(Self), PP_ExecMethod( ' }, {   0,   2 }, {   0, ', p1,p2,p3,p4,p5,p6,p7,p8,p9 ) }, ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ' )' } }, { -1,  1, -1,  3, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL }  } )
   aAdd( aTransResults, { { {   0, 'Self:' } }, { -1} ,  } )
   aAdd( aTransResults, { { {   0, '__GET( MEMVARBLOCK(' }, {   0,   2 }, {   0, '), ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aTransResults, { { {   0, '__GET(' }, {   0,   1 }, {   0, ')' } }, { -1,  1, -1} , { NIL }  } )
   aAdd( aTransResults, { { {   0, 'PP_ProcName( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
   aAdd( aTransResults, { { {   0, 'PP_ProcLine( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )

   /* Commands Results*/
 #ifdef WIN
   aAdd( aCommResults, { { {   0, 'MessageBox( 0, xToStr( ' }, {   0,   1 }, {   0, ' ), "xBaseScript for Windows", 0 )' } }, { -1,  1, -1} , { NIL }  } )
 #endif
   aAdd( aCommResults, { , , { NIL }  } )
   aAdd( aCommResults, { , , { NIL }  } )
   aAdd( aCommResults, { , , { NIL }  } )
   aAdd( aCommResults, { , , { NIL, NIL }  } )
   aAdd( aCommResults, { , , { NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__IF ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__ELSEIF ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__ELSE' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'PP__ENDIF' } }, { -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__END' } }, { -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__DOCASE' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'PP__CASE ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__OTHERWISE' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'PP__ENDCASE' } }, { -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__FOR ' }, {   0,   1 }, {   0, ':=' }, {   0,   2 }, {   0, '~TO~' }, {   0,   3 }, {   0, '~STEP~' }, {   0,   4 } }, { -1,  1, -1,  1, -1,  1, -1,  1} , { NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__FOR ' }, {   0,   1 }, {   0, ':=' }, {   0,   2 }, {   0, '~TO~' }, {   0,   3 }, {   0, '~STEP~' }, {   0,   4 } }, { -1,  1, -1,  1, -1,  1, -1,  1} , { NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__LOOP' } }, { -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__EXIT' } }, { -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__NEXT' } }, { -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__WHILE ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__WHILE ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__ENDDO' } }, { -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_Run( ' }, {   0,   1 }, {   0, ' + ".prg" )' } }, { -1,  2, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC_INIT ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC_EXIT ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC_PRG ' }, {   0,   1 }, {   0, ' ; PP_LocalParams( { ' }, {   0,   2 }, {   0, ' } )' } }, { -1,  1, -1,  3, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC_PRG ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC_PRG ' }, {   0,   1 }, {   0, ' ; PP_LocalParams( { ' }, {   0,   2 }, {   0, ' } )' } }, { -1,  1, -1,  3, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC_PRG ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC ' }, {   0,   1 }, {   0, ' ; PP_LocalParams( { ' }, {   0,   2 }, {   0, ' } )' } }, { -1,  1, -1,  3, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC ' }, {   0,   1 }, {   0, ' ; PP_LocalParams( { ' }, {   0,   2 }, {   0, ' } )' } }, { -1,  1, -1,  3, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_SetReturn( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_Params( { ' }, {   0,   1 }, {   0, ' } )' } }, { -1,  3, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_Privates( { ' }, {   0,   1 }, {   0, ' } )' } }, { -1,  3, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_Privates( { ' }, {   0,   1 }, {   0, ' } )' } }, { -1,  3, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_Publics( { ' }, {   0,   1 }, {   0, ' } )' } }, { -1,  3, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_Locals( { ' }, {   0,   1 }, {   0, ' } )' } }, { -1,  3, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_Statics( { ' }, {   0,   1 }, {   0, ' } )' } }, { -1,  3, -1} , { NIL }  } )

RETURN .T.

//--------------------------------------------------------------//
PROCEDURE PP_RunInit( aProcedures, aInitExit )

   IF ValType( aProcedures ) != 'A' .OR. ValType( aInitExit ) != 'A'
      Alert( [Invalid parameters to: ] + ProcName() + [ must be Arrays!] )
   ELSE
      aSize( aProcedures, 0 )

      aSize( aInitExit, 2 )
      aInitExit[1] := {}
      aInitExit[2] := {}
   ENDIF

   ErrorBlock( {|oErr| RP_Run_Err( oErr, aProcedures ) } )

   InitRules()
   InitResults()

   InitRunRules()
   InitRunResults()

RETURN

//--------------------------------------------------------------//
FUNCTION PP_PreProText( sLines, asLines )

   LOCAL nOpen, nClose, sTemp := "", nLine, nLines

   IF asLines == NIL
      asLines := {}
   ENDIF

   sLines := StrTran( sLines, Chr(13), "" )
   sLines := StrTran( sLines, Chr(9), "   " )

   WHILE ( nOpen := nAtSkipStr( "/*", sLines ) ) > 0
      sTemp += Left( sLines, nOpen - 1 )
      nClose := nAtSkipStr( "*/", sLines, nOpen + 2 )
      WHILE ( nOpen := nAtSkipStr( Chr(10), sLines, nOpen + 1 ) ) > 0 .AND. nOpen < nClose
         sTemp += Chr(10)
      ENDDO
      sLines := SubStr( sLines, nClose + 2 )
   ENDDO
   sLines := ( sTemp + sLines )

   nOpen  := 0
   nClose := 0
   WHILE ( nOpen := nAtSkipStr( Chr(10), sLines, nOpen + 1 ) ) > 0
      aAdd( asLines, SubStr( sLines, nClose + 1, nOpen - ( nClose + 1 ) ) )
      nClose := nOpen
   ENDDO
   IF Len( sLines ) > nClose
      aAdd( asLines, SubStr( sLines, nClose + 1 ) )
   ENDIF

   sLines := ""
   nLines := Len( asLines )
   FOR nLine := 1 TO nLines
      IF Left( asLines[nLine], 1 ) == '*'
         LOOP
      ENDIF

      nOpen := nAtSkipStr( "&&", asLines[nLine] )
      IF nOpen > 0
         sTemp := Left( asLines[nLine], nOpen - 1 )
      ELSE
         sTemp := asLines[nLine]
      ENDIF

      nOpen := nAtSkipStr( "//", sTemp )
      IF nOpen > 0
         sTemp := Left( sTemp, nOpen - 1 )
      ENDIF

      sTemp := PP_PreProLine( sTemp )
      sLines += sTemp
      IF nLine < nLines
         sLines += ";"
      ENDIF

      asLines[nLine] := sTemp
   NEXT

RETURN sLines

//--------------------------------------------------------------//
FUNCTION PP_RunText( sLines, bPP, aParams )

   LOCAL aProcedures := {}, aInitExit := { {}, {} }, nProcId := 0, ;
         nLine, nLines, xRet, asLines := {}, nOpen, nClose

   IF bPP == NIL
      bPP := .T.
   ENDIF

   PP_RunInit( aProcedures, aInitExit )

   IF bPP
      PP_PreProText( sLines, asLines )
   ELSE
      sLines := StrTran( sLines, Chr(13), "" )
      sLines := StrTran( sLines, Chr(9), "   " )
      nOpen  := 0
      nClose := 0
      WHILE ( nOpen := nAtSkipStr( Chr(10), sLines, nOpen + 1 ) ) > 0
         aAdd( asLines, SubStr( sLines, nClose + 1, nOpen - ( nClose + 1 ) ) )
         nClose := nOpen
      ENDDO
      IF Len( sLines ) > nClose
         aAdd( asLines, SubStr( sLines, nClose + 1 ) )
      ENDIF
   ENDIF

   ErrorBlock( {|oErr| RP_Comp_Err( oErr, asLines[nLine], nLine ) } )

   nLines := Len( asLines )
   FOR nLine := 1 TO nLines
      PP_CompileLine( asLines[nLine], nLine, aProcedures, aInitExit, @nProcId )
   NEXT

RETURN PP_Exec( aProcedures, aInitExit, nProcId, aParams )

//--------------------------------------------------------------//
FUNCTION PP_RunArray( asLines, aParams )

   LOCAL aProcedures := {}, aInitExit := { {}, {} }, nProcId := 0, ;
         nLine, nLines, nOpen, nClose

   PP_RunInit( aProcedures, aInitExit )

   ErrorBlock( {|oErr| RP_Comp_Err( oErr, asLines[nLine], nLine ) } )

   nLines := Len( asLines )
   FOR nLine := 1 TO nLines
      PP_CompileLine( asLines[nLine], nLine, aProcedures, aInitExit, @nProcId )
   NEXT

RETURN PP_Exec( aProcedures, aInitExit, nProcId, aParams )

//--------------------------------------------------------------//
FUNCTION PP_Exec( aProcedures, aInitExit, nProcId, aParams )

   LOCAL nProc, nProcs, xRet

   IF ValType( aParams ) == 'A'
      s_aParams := aParams
   ELSE
      s_aParams := {}
   ENDIF

   ErrorBlock( {|oErr| RP_Run_Err( oErr, aProcedures ) } )

   InitRules()
   InitResults()

   InitRunRules()
   InitRunResults()

   nProcs := Len( aInitExit[1] )
   FOR nProc := 1 TO nProcs
      PP_ExecProcedure( aProcedures[ aInitExit[1][nProc] ] )
   NEXT

   FOR nProc := 1 TO nProcId
      IF aScan( aInitExit[1], nProc ) == 0 .AND. aScan( aInitExit[2], nProc ) == 0
         xRet := PP_ExecProcedure( aProcedures[nProc] )
         EXIT
      ENDIF
   NEXT

   nProcs := Len( aInitExit[2] )
   FOR nProc := 1 TO nProcs
      PP_ExecProcedure( aProcedures[ aInitExit[2][nProc] ] )
   NEXT

RETURN xRet

//--------------------------------------------------------------//
PROCEDURE PP_ResetRules()

   aDefRules     := {}; aDefResults   := {}
   aTransRules   := {}; aTransResults := {}
   aCommRules    := {}; aCommResults  := {}

   s_lRunLoaded := .F.
   s_lClsLoaded := .F.
   s_lFWLoaded  := .F.

RETURN

//--------------------------------------------------------------//
PROCEDURE PP_InitStd()

   InitRules()
   InitResults()

   s_lRunLoaded := .F.
   s_lClsLoaded := .F.
   s_lFWLoaded  := .F.

RETURN

//--------------------------------------------------------------//
PROCEDURE PP_LoadRun()

   IF ! s_lRunLoaded
      s_lRunLoaded := .T.
      InitRunRules()
      InitRunResults()
   ENDIF

RETURN

//--------------------------------------------------------------//
PROCEDURE PP_LoadClass()

   IF ! s_lClsLoaded
      s_lClsLoaded := .T.
      InitClsRules()
      InitClsResults()
   ENDIF

RETURN

//--------------------------------------------------------------//
PROCEDURE PP_LoadFW()

   IF ! s_lFWLoaded
      s_lFWLoaded := .T.
      InitFWRules()
      InitFWResults()
   ENDIF

RETURN

//--------------------------------------------------------------//
#ifdef __HARBOUR__
   FUNCTION PP_CompileText( sLines )
   RETURN PP_CompileLine( sLines )
#endif

//--------------------------------------------------------------//
#ifdef __HARBOUR__
   #include "pp_harb.ch"
#else
   STATIC FUNCTION InitFWRules()
   RETURN .T.

   STATIC FUNCTION InitFWResults()
   RETURN .T.
#endif

//--------------------------------------------------------------//
/*
Function Alert( cMsg )

   //? ProcName(1), ProcLine(1), cMsg
   TraceLog( cMsg )

return NIL
*/
