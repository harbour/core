/*
 * $Id$
 */
/*
 * Harbour Project source code:
 * hbmake.Prg Harbour make utility main file
 *
 * Copyright 2000,2001 Luiz Rafael Culik <culik@sl.conex.net>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
#include "fileio.ch"
#include "common.ch"
#include "radios.ch"
//#include "checks.ch"
#include "hbgetcmt.ch"

#ifdef __HARBOUR__
#define EOL hb_osnewline()
#define CRLF hb_osnewline()
#else
#define EOL chr(13)+chr(10)
#define hb_osnewline() chr(13)+chr(10)
#define CRLF hb_osnewline()
#include "hbclip.ch"
#endif
#xtranslate timetosec(<x>) => ((val(substr(<x>,1,2))*3600)+(val(substr(<x>,4,2))*60)+(val(substr(<x>,7,2))))
DECLARE TestforPrg(cFile as String)
DECLARE findHarbourcfg(@cCfg AS STRING) AS LOGICAL
declare listasArray2( cString as String , cSep as String ) as Array
DECLARE GetGccDir() as String
#ifdef __HARBOUR__
#define datediff(<x>,<y>) (<x>-<y>)
#else
#translate datediff(<x>,<y>) => (<x>-<y>)
#endif
Static lPrint    AS LOGICAL  := .f.
Static nHandle  AS NUMERIC
Static aDefines    := {}
Static aBuildOrder := {}
Static aCommands   := {}
Static aMacros  := {}
Static aPrgs       := {}
Static aCs       := {}
Static aObjs     := {}
Static aObjsc    := {}
Static lEof      := .F.
Static aRes      := {}
Static nLinkHandle 
Static cLinker      := "makefile.tmp"
Static cLinkcomm    := ''
Static lBcc        := .T.
Static lGcc        := .F.
Static lVcc        := .F.
Static lForce      := .F.
Static lLinux      := .F.
Static szProject  :=""
Static lLibrary  :=.f.
Static lIgnoreErrors  :=.F.
Static lExtended  := .F.
Static lOs2      := .F.
Static lRecurse  := .F.
Static lEditMode := .F.
Static aDir
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function main()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
Function main( cFile, p1, p2, p3, p4, p5, p6 )

Local nPos
Local aDef := {}
Local cOs:=OS()
Local allParam
//__traceprgcalls(.t.)
Local oProfile := HBProfile():new()
Default p1 To ""
Default p2 To ""
Default p3 To ""
Default p4 To ""
Default p5 To ""
Default p6 To ""
if at("OS/2",cOs)>0
    lGcc:=.t.
    lLinux:=.t.
    lBcc:=.f.
endif
allParam:=p1 + p2 +p3+p4 + p5 +p6
Allparam:=ConvertParams(AllParam) 
If Pcount() == 0
   ShowHelp()
   Return NIL
Endif
If cFile == NIL .and. !lEditMode
   ? "File not Found"
   Return Nil
Endif
If Pcount() >= 1
   ProcessParameters(AllParam)
Endif
//if !file(cfile) 
//   return nil
//endif
if lEditMode
   if lLibrary
      crtlibmakfile( cFile )
   else
      crtmakfile( cFile )
   endif

Return nil
endif
cls
parsemakfi( cFile )
If lPrint
   PrintMacros()
Endif
if lForce
compfiles()
else
CompUpdatedfiles()
endif
outstd(cLinkComm)
! ( cLinkcomm )

Return nil

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function parsemakfi()
*+
*+    Called from ( hbmake.prg   )   1 - function main()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
Function parsemakfi( cFile )

Local nPos
Local cBuffer   := {}
Local cMacro    := "#BCC"
Local cDep      := "#DEPENDS"
Local cOpt      := "#OPTS"
Local cCom      := "#COMMANDS"
Local cBuild    := "#BUILD"
Local cTemp  as String   := ""
Local cTemp1    := ''
Local aTemp     := {}
Local lMacrosec := .f.
Local lBuildSec := .f.
Local lComSec   := .f.
Local aTemp1    := {}
Local cCfg :=""
Local lCfgFound := .F.
Local aTempCFiles := {}
nHandle := FT_FUSE( cFile )
If nHandle < 0
   Return nil
Endif
cBuffer := Trim( Substr( ReadLN( @lEof ), 1 ) )

Aadd( aDefines, { "HMAKEDIR",  GetMakeDir() } )
If lBcc
   Aadd( aDefines, { "MAKEDIR", GetBccDir() } )
Elseif lGcc
   Aadd( aDefines, { "MAKEDIR", GetGccDir() } )
Elseif lVcc
   Aadd( aDefines, { "MAKEDIR", GetVccDir() } )

Endif
While !leof

  If At( cMacro, cBuffer ) > 0
     lMacroSec := .T.
     lBuildSec := .f.
     lComSec   := .f.

  Elseif At( cBuild, cBuffer ) > 0
     lMacroSec := .f.
     lBuildSec := .T.
     lComSec   := .f.
  Elseif At( cCom, cBuffer ) > 0
     lBuildSec := .f.
     lComSec   := .t.
     lMacroSec := .f.
  Endif

  cTemp := Trim( Substr( ReadLN( @lEof ), 1 ) )

  aTemp := listasArray2( Alltrim( cTemp ), "=" )
  If lmacrosec
     If Alltrim( Left( ctemp, 7 ) ) <> '!ifndef' .and. Alltrim( Left( ctemp, 6 ) ) <> "!endif" .and. Alltrim( Left( ctemp, 7 ) ) <> '!iffile' .and. Alltrim( Left( ctemp, 7 ) ) <> '!stdout' .and. Alltrim( Left( ctemp, 6 ) ) <> '!ifdef'

        If Len( atemp ) > 1
           If At( "$", atemp[ 2 ] ) > 0
              If lgcc .and. aTemp[ 1 ] = "CFLAG1" .or. lGcc .and.  aTemp[ 1 ] = "CFLAG2"
                 Aadd( amacros, { aTemp[ 1 ], Strtran( Replacemacros( atemp[ 2 ] ), "\", "/" ) } )
              Else
                 Aadd( amacros, { aTemp[ 1 ], Replacemacros( atemp[ 2 ] ) } )
              Endif
           Else
              If lgcc .and. aTemp[ 1 ] = "CFLAG1" .or. lGcc .and. aTemp[ 1 ] = "CFLAG2"
                 Aadd( aMacros, { aTemp[ 1 ], Strtran( atemp[ 2 ], "\", "/" ) } )
              Else
                 Aadd( aMacros, { aTemp[ 1 ], atemp[ 2 ] } )
              Endif
           Endif
        Endif
        if aTemp[ 1 ] = "PROJECT"
                if at('.lib',atemp[2])>0 .or. at('.a',atemp[2])>0
                        lLibrary:=.t.
                endif
        endif
        If aTemp[ 1 ] = "OBJFILES"
           aObjs := listasArray2( replacemacros(atemp[ 2 ]), " " )
        Endif
        If aTemp[ 1 ] = "OBJCFILES"
            aTemp1 := listasArray2( replacemacros(atemp[ 2 ]), " " )
            if len(atemp1) ==1
               if !empty(atemp[1])
                 aObjsC := listasArray2( replacemacros(atemp[ 2 ]), " " )
               endif
            else
             aObjsC := listasArray2( replacemacros(atemp[ 2 ]), " " )
            endif
        Endif
        if aTemp[ 1 ] = "PRGFILES"
           aPrgs := listasArray2( replacemacros(atemp[ 2 ]), " " )
           lExtended := .T.
           lCfgFound := findHarbourcfg(@cCfg)
        Endif
        if aTemp[ 1 ] = "PRGFILE"
           aPrgs := listasArray2( replacemacros(atemp[ 2 ]), " " )
        Endif

        If atemp[ 1 ] = "CFILES"
           if lExtended
               aTempCFiles := listasArray2( replacemacros(atemp[ 2 ]), " " )
               if (len(aTempCFiles) ==1 )
                  if !empty(aTempCFiles[1])
                    aCs := listasArray2( replacemacros(atemp[ 2 ]), " " )
                  endif
               else
                   aCs := listasArray2( replacemacros(atemp[ 2 ]), " " )
               endif
           else
               aCs := listasArray2( replacemacros(atemp[ 2 ]), " " )
           endif
        Endif
        If atemp[ 1 ] = "RESFILES"
           aRes := listasArray2( replacemacros(atemp[ 2 ]), " " )

        Endif

     Else
        //           cTemp1:=TRIM( SUBSTR( ReadLN( @lEof ),1 ) )


        if at('!ifndef',cTemp)>0
           checkDefine( cTemp )
        elseif  at('!ifdef',ctemp)>0
            CheckifDef(cTemp)
        elseif at('!iffile',cTemp)>0
            checkiffile(cTemp)
        elseif at('!stdout',cTemp)>0
            checkstdout(cTemp)            
        endif
        //   endif
     Endif
  Endif
  If lbuildSec
     szProject:=cTemp
     aBuildOrder := listasarray2( ctemp, ":" )
     // ? cTemp
     if !llibrary
   
     SetBuild()
    else
        SetLibBuild()
    endif
  Endif
  If lComSec
     If !Empty( ctemp )
        Setcommands( cTemP )
     Endif
  Endif
  If cTemp = "#BUILD"
     cBuffer := cTEmp
  Elseif cTemp == "#COMMANDS"
     cbuffer := ctemp
  Endif
Enddo

   if lExtended .and. !lCfgFound
      if lBcc
         BuildBorCfgFile()
      elseif lVcc
         BuildMSCCfgFile()
      endif
endif

Return nil

Function checkDefine( cTemp )

Local cDef
Local nPos
Local cRead
Local aSet     := {}
Local nMakePos

If cTemp == "!endif"
   Return nil
Endif
cTemp := Trim( Substr( ReadLN( @lEof ), 1 ) )
cTemp := Strtran( cTemp, "!ifndef ", "" )
cTemp :=strtran(cTemp,"\..","")
cTemp :=strtran(cTemp,"/..","")

if at("\..",cTemp)>0
   cTemp:=substr(cTemp,1,at("\..",cTemp)-1)
elseif at("/..",cTemp)>0
   cTemp:=substr(cTemp,1,at("/..",cTemp)-1)
endif
*/
aSet  := listasarray2( ctemp, "=" )
nPos  := Ascan( adefines, { | x | x[ 1 ] == aset[ 1 ] } )
If nPos = 0

   cRead    := Alltrim( Strtran( aset[ 2 ], "$(", "" ) )
   cRead    := Strtran( cRead, ")", "" )
   nMakePos := Ascan( aDefines, { | x | x[ 1 ] == cRead } )
   If nMakePos > 0
      Aadd( aDefines, { aset[ 1 ], aDefines[ nMakePos, 2 ] } )
      Aadd( amacros, { aset[ 1 ], aDefines[ nMakePos, 2 ] } )
   Endif
Endif
Return nil

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function Setcommands()
*+
*+    Called from ( hbmake.prg   )   1 - function parsemakfi()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
Function Setcommands( cTemP )

Local cRead       := Alltrim( readln( @leof ) )
Local nPos
Local nCount      := 0
Local aTempMacros := {}
Local aLocalMacros :={}
aTempMacros := listasarray2( cREad, " " )
/*
For nCount := 1 To Len( atempmacros )
   If At( "$", atempmacros[ ncount ] ) > 0
      if At( ";", atempmacros[ ncount ] ) > 0
      aLocalMacros:=listasarray2( atempmacros[ ncount ], ";" )
      for nPos:=1 to len( aLocalmacros)
      findmacro( aLocalmacros[ nPos ], @cRead )
      next
      else
      findmacro( atempmacros[ ncount ], @cRead )
      endif
   Endif
Next
*/
aeval(aTempMacros,{|xMacro|if(at("$",xMacro)>0,      if(At( ";", xMacro ) > 0,( aLocalMacros:=listasarray2( xMacro, ";" ),aeval(aLocalMacros,{|x| findmacro( x, @cRead )})), findmacro( xMacro, @cRead )),)})
Aadd( aCommands, { cTemp, cRead } )
Return nil

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function Findmacro()
*+
*+    Called from ( hbmake.prg   )   1 - function setcommands()
*+                                   1 - function replacemacros()
*+                                   2 - function setbuild()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
Function Findmacro( cMacro, cRead )

Local nPos
Local cTemp
local aLocalMacros:={}
cMacro := Substr( cMacro, 1, At( ")", cMacro ) )
If At( "-", cMacro ) > 0
   cMacro := Substr( cMacro, 3 )
Endif
If At( ";", cMacro ) > 0
   cMacro := Substr( cMacro, At( ";", cMacro ) + 1 )
Endif
nPos := Ascan( aMacros, { | x | "$(" + Alltrim( x[ 1 ] ) + ")" == cMacro } )
If nPos = 0
   cTemp := Strtran( cmacro, "$(", "" )
   cTemp := Strtran( ctemp, ")", "" )
   If !Empty( cTemp )
      cRead := Alltrim( Strtran( cRead, cmacro, Gete( cTemp ) ) )
   Endif
Else
   cRead := Alltrim( Strtran( cRead, cmacro, amacros[ npos, 2 ] ) )
Endif
Return cRead

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function Replacemacros()
*+
*+    Called from ( hbmake.prg   )   2 - function parsemakfi()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
Function Replacemacros( cMacros )

Local nPos
Local nCount      := 0
Local aTempMacros := {}
local aLocalMacros:={}
// ? "replacing macros"
aTempMacros := listasarray2( cMacros, " " )
aeval(aTempMacros,{|xMacro|if(at("$",xMacro)>0,      if(At( ";", xMacro ) > 0,( aLocalMacros:=listasarray2( xMacro, ";" ),aeval(aLocalMacros,{|x| findmacro( x, @cMacros )})), findmacro( xMacro, @cMacros )),)})
/*
For nCount := 1 To Len( atempmacros )
   If At( "$", atempmacros[ ncount ] ) > 0
      if At( ";", atempmacros[ ncount ] ) > 0
      aLocalMacros:=listasarray2( atempmacros[ ncount ], ";" )
      for nPos:=1 to len( aLocalmacros)
      findmacro( aLocalmacros[ nPos ], @cmacros )
      next
      else
      findmacro( atempmacros[ ncount ], @cmacros )
      endif

   Endif
Next
*/
Return cmacros

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function setBuild()
*+
*+    Called from ( hbmake.prg   )   1 - function parsemakfi()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
Function setBuild()

Local cRead
Local nPos
Local aMacro
Local aTemp
Local nCount
Local cCurrentRead:=''
// ? "setting link file"
cRead  := Alltrim( readln( @leof ) )
     szProject:=cRead
amacro := listasarray2( cRead, ":" )
If Len( amacro ) > 1
   aTemp := listasarray2( amacro[ 2 ], " " )
   aeval(atemp,{|xItem|       Aadd( aBuildOrder, xItem )})
Endif
Aadd( aBuildOrder, amacro[ 1 ] )
cRead := Strtran( cRead, "@&&!", "" )

amacro := listasarray2( cRead, '\' )
aeval(amacro,{|xMacro| if(at("$",xmacro)>0, findmacro( xMacro, @cRead ),)})
if !lLinux
   cLinkcomm   := cRead + "  @" + cLinker
   nLinkHandle := Fcreate( clinker )
else
   cLinkComm := cRead+ " "
endif
//#define CRLF hb_osnewline()

For nPos := 1 To 7
   cRead  := Alltrim( readln( @leof ) )
   cCurrentRead:=cRead
   amacro := listasarray2( cRead, " " )
   For ncount := 1 To Len( amacro )
      If At( "$", amacro[ nCount ] ) > 0
         findmacro( amacro[ nCount ], @cRead )
         /*comment*/
         if at('$(PROJECT)',cCurrentRead)>0
            if !lGcc
               if !lLinux
                Fwrite( nLinkhandle, cRead + CRLF )
               endif
            ELSEIF lGcc .and. lLinux
               cLinkComm +=  "-o " + cRead + " " 
            ELSEIF lGcc .and. !lLinux .and. at('.exe',cread)>0              
               Fwrite( nLinkhandle, "-o " + cRead + CRLF )
            endif
         else
            if !lLinux
               Fwrite( nLinkhandle, cRead + CRLF )
            else
               cLinkComm += cRead +" "
            endif
         endif
     Endif
   Next
Next
if !lLinux
Fclose( nLinkhandle )
Endif
outstd(cLinkComm)
Return nil

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function Compfiles()
*+
*+    Called from ( hbmake.prg   )   1 - function main()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
Function Compfiles()

Local cComm
Local cOld
Local nPos
Local nCount
Local nFiles
Local cErrText:=""
Local aOrder := listasarray2( aBuildOrder[ 2 ], " " )
Local lEnd:=.f.
Local xItem
For nCount := 1 To Len( aOrder )
   if !lExtended
   
   If aOrder[ nCount ] == "$(CFILES)"
      nPos := Ascan( aCommands, { | x | x[ 1 ] == ".prg.c:" } )
      If nPos > 0
         cComm := aCommands[ nPos, 2 ]
         cOld  := cComm
      else
         nPos := Ascan( aCommands, { | x | x[ 1 ] == ".PRG.C:" } )
            If nPos > 0
            cComm := aCommands[ nPos, 2 ]
            cOld  := cComm
         endif

      Endif
      For nFiles := 1 To Len( aPrgs )
         xItem := substr(aPrgs[ nFiles ],rat(if(lgcc,'/','\'),aPrgs[ nFiles ])+1)

         nPos := Ascan( aCs, { | x | x:=substr(x,rat(if(lgcc,'/','\'),x)+1),Left( x, At( ".", x ) ) == Left(xItem, At( ".", xItem ) ) } )
         If nPos > 0
            cComm := Strtran( cComm, "o$*", "o" + aCs[ nPos ] )
            cComm := Strtran( cComm, "$**", aPrgs[ nFiles ] )

          cComm += " > test.out"
            outstd(cComm)
            ! ( cComm )
                  cErrText := memoread( 'test.out' )
                  lEnd := 'C2006' $ cErrText .or. 'No code generated' $ cErrText

            if !lIgnoreErrors .and. lEnd
                quit
            else
                ferase('test.out')
            endif

            cComm := cold
         Endif
      Next
   Endif
   If aOrder[ nCount ] == "$(OBJFILES)"
      If lGcc
         nPos := Ascan( aCommands, { | x | x[ 1 ] == ".c.o:" } )
      Else
         nPos := Ascan( aCommands, { | x | x[ 1 ] == ".c.obj:" } )
      Endif
      If nPos > 0
         cComm := aCommands[ nPos, 2 ]
         cOld  := ccomm
      else
         if lGcc
            nPos := Ascan( aCommands, { | x | x[ 1 ] == ".C.O:" } )
            If nPos > 0
               cComm := aCommands[ nPos, 2 ]
               cOld  := cComm
            endif
         else
         nPos := Ascan( aCommands, { | x | x[ 1 ] == ".C.OBJ:" } )
            If nPos > 0
            cComm := aCommands[ nPos, 2 ]
            cOld  := cComm
         endif

      endif
      Endif
      For nFiles := 1 To Len( aCs )
         xItem := substr(aCs[ nFiles ],rat(if(lgcc,'/','\'),aCs[ nFiles ])+1)

         nPos := Ascan( aObjs, { | x | x:=substr(x,rat(if(lgcc,'/','\'),x)+1),Left( x, At( ".", x ) ) == Left(xItem, At( ".", xItem ) ) } )



         If nPos > 0
            cComm := Strtran( cComm, "o$*", "o" + aObjs[ nPos ] )
            cComm := Strtran( cComm, "$**", acs[ nFiles ] )
            outstd( " ")
            // ? cComm
            outstd(cComm)
            ! ( cComm )
            ccomm := cold
         Endif
      Next
   Endif
else /****** Extended mode *****/
   If aOrder[ nCount ] == "$(CFILES)"
      If lGcc
         nPos := Ascan( aCommands, { | x | x[ 1 ] == ".c.o:" } )
      else
         nPos := Ascan( aCommands, { | x | x[ 1 ] == ".c.obj:" } )
      endif
      If nPos > 0
         cComm := aCommands[ nPos, 2 ]
         cOld  := cComm
      else
         nPos := Ascan( aCommands, { | x | x[ 1 ] == ".C.OBJ:" } )
            If nPos > 0
            cComm := aCommands[ nPos, 2 ]
            cOld  := cComm
         endif
      Endif
   if len(acs)>0
      For nFiles := 1 To Len( acs )
         xItem := substr(acs[nFiles],rat(if(lgcc,'/','\'),acs[nFiles])+1)
         nPos := Ascan( aObjsc, { | x | x:=substr(x,rat(if(lgcc,'/','\'),x)+1),Left( x, At( ".", x ) ) == Left( xitem, At( ".", xitem ) ) } )
         
            If nPos > 0
               cComm := Strtran( cComm, "o$*", "o" + aobjsc[ nPos ] )
               cComm := Strtran( cComm, "$**", acs[ nFiles ] )
               cComm += " > test.out"
               outstd(cComm)
               ! ( cComm )
                  cErrText := memoread( 'test.out' )
                  lEnd := 'Error' $ cErrText 
               /*   if file('test.out'  )
                    ferase('test.out'  )
                  endif*/
            if !lIgnoreErrors .and. lEnd
                quit
            else
                ferase('test.out')

            endif
               cComm := cold
            Endif
         
      Next
endif
   Endif
   If aOrder[ nCount ] == "$(OBJFILES)"
      If lGcc
         nPos := Ascan( aCommands, { | x | x[ 1 ] == ".prg.o:" } )
      Else
         nPos := Ascan( aCommands, { | x | x[ 1 ] == ".prg.obj:" } )
      Endif
      If nPos > 0
         cComm := aCommands[ nPos, 2 ]
         cOld  := ccomm
      else
         If lGcc
            nPos := Ascan( aCommands, { | x | x[ 1 ] == ".PRG.O:" } )
         Else
            nPos := Ascan( aCommands, { | x | x[ 1 ] == ".PRG.OBJ:" } )
         endif
      Endif

      for nFiles := 1 to len(aprgs)
         xItem := substr(aprgs[nFiles],rat(if(lgcc,'/','\'),aprgs[nFiles])+1)
         nPos := ascan( aobjs, { | x | x:=substr(x,rat(if(lgcc,'/','\'),x)+1), Left( x, At( ".", x ) ) == Left( xItem, At( ".", xitem ) ) } )
         
            If nPos > 0
               cComm := Strtran( cComm, "o$*", "o" + aObjs[ nPos ] )
               cComm := Strtran( cComm, "$**", aprgs[ nFiles ] )
               cComm += " > test.out"
               outstd( " ")
               // ? cComm
               outstd(cComm)
               ! ( cComm )
                cErrText := memoread( 'test.out' )
               lEnd := 'C2006' $ cErrText .or. 'No code generated' $ cErrText
               if !lIgnoreErrors .and. lEnd
                  quit
            else
                ferase('test.out')

               endif

               ccomm := cold
            Endif
         
      Next
   Endif

endif
   If aOrder[ nCount ] == "$(RESDEPEN)"
      nPos := Ascan( aCommands, { | x | x[ 1 ] == ".rc.res:" } )
      If nPos > 0
         cComm := aCommands[ nPos, 2 ]
      Endif
      For nFiles := 1 To Len( aRes )
         //            nPos:=ascan(aObjs,{|x| left(x,at(".",x)) == left(acs[nFiles],at(".",acs[nFiles]))})
         If !Empty( ares[ nFiles ] )
            cComm := Strtran( cComm, "$<", aRes[ nFiles ] )
            outstd(" ")
            ! ( cComm )
         Endif
      Next
   Endif

Next
Return nil

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function GetParaDefines()
*+
*+    Called from ( hbmake.prg   )   2 - function main()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
Function GetParaDefines( cTemp )


Local nPos
Local cRead
Local aSet     := {}
Local nMakePos
if at("\..",cTemp)>0
   cTemp:=substr(cTemp,1,at("\..",cTemp)-1)
elseif at("/..",cTemp)>0
   cTemp:=substr(cTemp,1,at("/..",cTemp)-1)
endif
aSet := listasarray2( ctemp, "=" )
nPos := Ascan( adefines, { | x | x[ 1 ] == aset[ 1 ] } )
If nPos = 0
   cRead    := Alltrim( Strtran( aset[ 2 ], "$(", "" ) )
   cRead    := Strtran( cRead, ")", "" )

   nMakePos := Ascan( aDefines, { | x | x[ 1 ] == cRead } )
   If nMakePos = 0
      aset[2]:=strtran(aset[2],","," ")
      Aadd( aDefines, { aset[ 1 ], aset[ 2 ] } )
      Aadd( amacros, { aset[ 1 ], aset[ 2 ] } )

   Endif
Endif
Return nil

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function PrintMacros()
*+
*+    Called from ( hbmake.prg   )   1 - function main()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
Function PrintMacros()

Local nPos

Outstd( "HBMAKE Version ", Version(), "CopyRight (c) 2000-2001 The Harbour Project" + CRLF )
Outstd( "" + CRLF )
Outstd( "Macros:" + CRLF )
aeval(aMacros,{|xItem| Outstd( "     " + xItem[ 1 ] + " = " + xItem[ 2 ] + CRLF )})
/*
For nPos := 1 To Len( aMacros )
   Outstd( "     " + aMacros[ nPos, 1 ] + " = " + aMacros[ nPos, 2 ] + CRLF )
Next*/
Outstd( "Implicit Rules:" + CRLF )
aeval(aCommands,{|xItem|  Outstd( "     " + xItem[ 1 ] + hb_osnewline() + "        " + xItem[ 2 ] + CRLF )})
/*
For nPos := 1 To Len( aCommands )
   Outstd( "     " + aCommands[ nPos, 1 ] + hb_osnewline() + "        " + aCommands[ nPos, 2 ] + CRLF )
Next
*/
Outstd( "" + CRLF )
Outstd( "Targets:" )
Outstd( "    " + szProject + ":" + CRLF )
Outstd( "        " + "Flags :" + CRLF )
Outstd( "        " + "Dependents :" )
aeval(acs,{|xItem|   Outstd( xitem + " ")})
/*For nPos := 1 To Len( aCs )
   Outstd( acs[ nPos ] + " ")
Next*/
aeval(aobjs,{|xItem|   Outstd( xitem + " ")})
/*For nPos := 1 To Len( aobjs )
   Outstd( aobjs[ nPos ]  + " ")
Next*/
Outstd( " " + CRLF )
Outstd( "        commands:" + aBuildOrder[ Len( aBuildOrder )  ] )
Outstd( " " + CRLF )
Outstd( " " + CRLF )
Outstd( " " + CRLF )
Return Nil

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function crtmakfile()
*+
*+    Called from ( hbmake.prg   )   2 - function main()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
Func crtmakfile( cFile )

Local ain          := {}
Local aOut         := {}
Local aOutc        := {}
Local aSrc         := Directory( "*.prg" )
Local nLenaSrc     := Len( asrc )
Local nLenaOut
Local lFwh         := .f.
Local lCw         := .f.
Local lRddAds      := .f.
Local cOs          := "Win32"
Local cCompiler    := "BCC"
Local cfwhpath     := space(40)
Local ccwpath      :=  space(40)
local cObjDir      := "obj"+space(20)
Local lAutomemvar  := .f.
Local lvarismemvar := .f.
Local ldebug       := .f.
Local lSupressline := .f.
local nPos
Local cDefHarOpts  := ""


Local lCompMod     := .f.
Local x
Local lGenppo      := .f.
Local getlist      := {}
Local cTopFile     := ""
Local cDefBccLibs  := "lang.lib vm.lib rtl.lib rdd.lib macro.lib pp.lib dbfntx.lib dbfcdx.lib common.lib gtwin.lib"
Local cDefGccLibs  := "-lvm -lrtl -lgtstd -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon"
Local cgcclibsos2  := "-lvm -lrtl -lgtos2 -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon"
Local cDeflibGccLibs := "-lvm -lrtl -lgtstd -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon -lm"

local citem:=""
Local cExt:=""
Local cDrive:=""
local cPath:=""
Local cTest:=""
Local cUserdef:=space(40)
Local cUserInclude:=space(40)

nLinkHandle := Fcreate( cFile )
WriteMakeFileHeader()

Cls
Setcolor( 'w/b+,b+/w,w+/b,w/b+,w/b,w+/b' )
@  0,  0, Maxrow(), Maxcol() Box( Chr( 201 ) + Chr( 205 ) + Chr( 187 ) + Chr( 186 ) + Chr( 188 ) + Chr( 205 ) + Chr( 200 ) + Chr( 186 ) + Space( 1 ) )
ATTENTION( "Enviroment options", 0 )
@  1,  1 Say "Select Os"
@  1, 12 Get cos radio { "Win32", "OS/2", "Linux" } valid !empty(cos)
@  1, 23 Say "Select C Compiler"
@  1, 40 Get cCompiler radio { "BCC", "MSVC", "GCC" } valid !empty(cCompiler)
@  1, 48 Say "Graphic Library"
@  1, 64 Get lFwh checkbox  caption "Use FWH" when Cos=="Win32"
@  2, 64 Get lcw checkbox  caption "Use C4W"          when Cos=="Win32"
@  3, 64 Get lRddads checkbox  caption "Use RddAds"   when Cos=="Win32"
Read
//set cursor on
If lFwh
   @  4,  1 Say "FWH path" Get cfwhpath
Elseif lCw
   @  4,  1 Say "C4H path" Get ccwpath
Endif
   @  4,40   Say "Obj Files Dir" get cObjDir
ATTENTION( "Harbour Options", 5 )

@  6,  1 Get lautomemvar checkbox  caption  "Automatic memvar declaration"
@  6, 43 Get lvarismemvar checkbox caption  "Variables are assumed M->"
@  7,  1 Get lDebug checkbox caption  "Debug info"
@  7, 43 Get lSupressline checkbox  caption "Suppress line number information"
@  8,  1 Get lGenppo checkbox  caption "Generate pre-processed output"
@  8, 43 Get lCompMod checkbox caption  "compile module only"
@  9,  1 Say "User Defines " get cUserDef pict "@s20"
@  9, 43 Say "User include Path" get cUserInclude pict "@s20"
Read
if !empty(cUserDef)
      cDefHarOpts+= " -D"+alltrim(cUserDef) +" "
endif
if !empty(cUserInclude)
      cDefHarOpts+= " -I"+alltrim(cUserInclude)+" "
endif
lBcc := If( At( "BCC", cCompiler ) > 0, .t., .f. )
lVcc := If( At( "MSVC", cCompiler ) > 0, .t., .f. )
lGcc := If( At( "GCC", cCompiler ) > 0, .t., .f. )
cObjDir:=alltrim(cObjDir)
if !empty(cobjDir)
   if dirchange(cobjDir)!=0
      makedir(cobjDir)
   else
      dirchange('..')
   endif
endif
amacros:=GetSourceDirMacros(lGcc,cos)
if lGcc
cObjDir:=alltrim(cObjDir)
if !empty(cObjDir)
cObjDir+='/'
endif
cTest:=upper(cObjDir)+'/'
else
cObjDir:=alltrim(cObjDir)
if !empty(cObjDir)
cObjDir+='\'
endif

cTest:=upper(cObjDir)+'\'
endif

aeval(amacros,{|x,y|cItem:=substr(x[2],1,len(x[2])),if(at(citem,cTest)>0,(amacros[y,1]:='OBJ',amacros[y,2]:=cObjDir),)})

if lAutomemvar
cDefHarOpts+=" -a "
endif
if lvarismemvar
cDefHarOpts+=" -v "
endif
if ldebug
cDefHarOpts+=" -b "
 cDefBccLibs  += "debug.lib"
 cDefGccLibs  += "-ldebug"
 cgcclibsos2  += "-ldebug"
 cDeflibGccLibs += "-ldebug"
endif
if lSupressline
cDefHarOpts+=" -l "
endif
if lGenppo
cDefHarOpts+=" -p "
endif
if lCompmod
cDefHarOpts+=" -m "
endif


If lBcc
   Aadd( aCommands, { ".cpp.obj:", "$(BCB)\BIN\bcc32 $(CFLAG1) $(CFLAG2) -o$* $*" } )

   Aadd( aCommands, { ".c.obj:", "$(BCB)\BIN\bcc32 -I$(BHC)\include $(CFLAG1) $(CFLAG2) -o$* $**" } )
   if lExtended
      Aadd( aCommands, { ".prg.obj:", "$(BHC)\bin\harbour -n -go -I$(BHC)\include $(HARBOURFLAGS) -I$(FWH)\include -o$* $**" } )
   else
      Aadd( aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)\include $(HARBOURFLAGS) -I$(FWH)\include -o$* $**" } )
   endif
   Aadd( aCommands, { ".rc.res:", "$(BCB)\BIN\brcc32 $(RFLAGS) $<" } )

Elseif lGcc
   if at("linux",Getenv("HB_ARCHITECTURE"))>0 .or. cOs=="Linux"
   Aadd( aCommands, { ".cpp.o:", "gcc $(CFLAG1) $(CFLAG2) -o$* $*" } )

   Aadd( aCommands, { ".c.o:", "gcc -I$(HB_INC_INSTALL) $(CFLAG1) $(CFLAG2) -I. -g -o$* $**" } )
   if lExtended
      Aadd( aCommands, { ".prg.o:", "harbour -n  -go -I$(HB_INC_INSTALL) -I.  -o$* $**" } )
   else
      Aadd( aCommands, { ".prg.c:", "harbour -n -I$(HB_INC_INSTALL) -I.  -o$* $**" } )
   endif
else
   Aadd( aCommands, { ".cpp.o:", "$(BCB)\bin\gcc $(CFLAG1) $(CFLAG2) -o$* $*" } )

   Aadd( aCommands, { ".c.o:", "$(BCB)\bin\gcc -I$(BHC)/include $(CFLAG1) $(CFLAG2) -I. -o$* $**" } )
   if lExtended
      Aadd( aCommands, { ".prg.o:", "$(BHC)\bin\harbour -n -go -I$(BHC)/include $(HARBOURFLAGS)  -o$* $**" } )
   else
      Aadd( aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)/include $(HARBOURFLAGS)  -o$* $**" } )
   endif
endif

Elseif lVcc
   Aadd( aCommands, { ".cpp.obj:", "$(BCB)\bin\cl $(CFLAG1) $(CFLAG2) -Fo$* $*" } )

   Aadd( aCommands, { ".c.obj:", "$(BCB)\bin\cl -I$(BHC)\include $(CFLAG1) $(CFLAG2) -Fo$* $**" } )
   if lExtended
   Aadd( aCommands, { ".prg.obj:", "$(BHC)\bin\harbour -n -I$(BHC)\include $(HARBOURFLAGS) -go  -I$(C4W)\include -o$* $**" } )
   else
   Aadd( aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)\include $(HARBOURFLAGS) -I$(C4W)\include -o$* $**" } )
   endif
   Aadd( aCommands, { ".rc.res:", "$(BCB)\BIN\rc $(RFLAGS) $<" } )
Endif

attention( 'Spacebar to select, Enter to continue process', 22 )
if !lRecurse
   ain:=GetSourceFiles(.f.,lGcc,cOs) 
   nLenaSrc:=Len(ain)
else
   ain:=GetSourceFiles(,lGcc,cOs) 
   nLenaSrc:=Len(asrc)
endif
   aOut := Aclone( aIn )
pickarry( 10, 15, 19, 64, aIn, aOut )

nLenaOut := Len( aOut )


aeval(aout,{|x,y| aout[y]:=Trim(  substr(aOut[ y ],1,at(' ',aout[y])))})
aOut := Asort( aOut )

If Len( aOut ) == 1
   cTopFile := aOut[ 1 ]
Else
   attention( 'Select the TOP MODULE of your executable', 22 )
    if !lrecurse
      cTopFile := pickfile( "*.prg" )
    else
      cTopFile:=pickafile(ain)
    endif
Endif

aeval(aout,{|xItem| if( at('.c',xItem)>0 .or. at('.C',xItem)>0,aadd(aoutc,xitem),)})
aeval(aoutc,{|x,z| citem:=x,z:=ascan(aout,{|t| t=citem}), if(z>0,asize(adel(aout,z),len(aout)-1),)})



   aOut := Asort( aOut )
   aPrgs   := aclone(aout)

   aObjs := aClone(aout)
x:=ascan(aobjs,{|x| lower(x)==lower(cTopFile)})
if x>0
    adel(aobjs,x)
    asize(aobjs,len(aobjs)-1)
    asize(aobjs,len(aobjs)+1)
    ains(aobjs,1)
    aobjs[1]:=cTopFile
endif 
x:=ascan(aPrgs,{|x| lower(x)==lower(cTopFile)})
if x>0
    adel(aPrgs,x)
    asize(aPrgs,len(aPrgs)-1)
    asize(aPrgs,len(aPrgs)+1)
    ains(aPrgs,1)
    aPrgs[1]:=cTopFile
endif 

   aeval(aobjs,{|xItem,x|  hb_FNAMESPLIT(xiTem,@cPath ,@cTest, @cExt , @cDrive),cext:=substr(cExt,2),if(!lGcc,aObjs[ x ]:=cObjDir+cTest+"."+exte( cExt,2),aObjs[ x ]:=cObjDir+cTest+"."+exte( cExt,3))})
   aCs:=aclone(aoutc)
                                                                                                                                                                                       
if !lextended
   aeval(aOutc,{|xItem,x| hb_FNAMESPLIT(xiTem,@cPath ,@cTest, @cExt , @cDrive),cext:=substr(cExt,2),   if(!lGcc,aadd(aObjs,cObjDir+cTest+"."+exten( cExt,2)),aadd(aObjs,cObjDir+cTest+"."+exten( cExt,1)))})
   aeval(aout,{|xItem| hb_FNAMESPLIT(xiTem,@cPath ,@cTest, @cExt , @cDrive), cExt:=substr(cExt,2),aadd(aCs,cObjDir+cTest+"."+exte( cExt,1))})
else
aObjsc := aClone(aoutc)
 aeval(aoutc,{|xItem,x|  hb_FNAMESPLIT(xiTem,@cPath ,@cTest, @cExt , @cDrive),cext:=substr(cExt,2),if(!lGcc,aObjsc[ x ]:=if(!empty(cObjDir),cObjDir,'')+cTest+"."+exten( cExt,2),aObjsc[ x ]:=if(!empty(cObjDir),cObjDir,'')+cTest+"."+exten( cExt,1))})
endif


If lFwh
   Fwrite( nLinkHandle, "FWH = " + cfwhpath + CRLF )
Elseif lCw

   Fwrite( nLinkHandle, "C4W =" + ccwpath + CRLF )

Endif
//Fwrite( nLinkHandle, "OBJ =" + cObjDir + CRLF )

for x:=1 to len(amacros)
   if !empty(amacros[x,2])       
         cItem := amacros[x,2]
         nPos:=ascan(aprgs,{|z| hb_FNAMESPLIT(z,@cPath ,@cTest, @cExt , @cDrive),cpath==citem})
      if nPos>0
      AEVAL(aprgs,{|a,b| hb_FNAMESPLIT(a,@cPath ,@cTest, @cExt , @cDrive),if(cPath==citem,aprgs[b]:=strtran(a,cpath,"$("+amacros[x,1]+')\'),)})
       if !amacros[x,3]
            Fwrite( nLinkHandle, amacros[x,1] + ' = ' +left(amacros[x,2],len(amacros[x,2])-1) +" " + CRLF )
            amacros[x,3]:=.t.
        endif
      endif
      nPos:=ascan(acs,{|z| hb_FNAMESPLIT(z,@cPath ,@cTest, @cExt , @cDrive),cpath==citem})
      if nPos>0
       if !amacros[x,3]
         AEVAL(acs,{|a,b| hb_FNAMESPLIT(a,@cPath ,@cTest, @cExt , @cDrive),if(cPath==citem,acs[b]:=strtran(a,cpath,"$("+amacros[x,1]+if(lgcc,")/",')\')),)})
            Fwrite( nLinkHandle, amacros[x,1] + ' = ' +left(amacros[x,2],len(amacros[x,2])-1) +" " + CRLF )
            amacros[x,3]:=.t.
        endif

      endif
      nPos:=ascan(aObjs,{|z| hb_FNAMESPLIT(z,@cPath ,@cTest, @cExt , @cDrive),cpath==citem})
      if nPos>0
         if !empty(cObjDir)
            AEVAL(aObjs,{|a,b| hb_FNAMESPLIT(a,@cPath ,@cTest, @cExt , @cDrive),if(cPath==citem,aObjs[b]:=strtran(a,cpath,"$("+amacros[x,1]+if(lgcc,")/",')\')),)})
            Fwrite( nLinkHandle, amacros[x,1] + ' = ' +left(amacros[x,2],len(amacros[x,2])-1) +" " + CRLF )
         endif
      endif
      if lExtended
      nPos:=ascan(aObjsc,{|z| hb_FNAMESPLIT(z,@cPath ,@cTest, @cExt , @cDrive),cpath==citem})
      if nPos>0
         if !empty(cObjDir)
            AEVAL(aObjsc,{|a,b| hb_FNAMESPLIT(a,@cPath ,@cTest, @cExt , @cDrive),if(cPath==citem,aObjsc[b]:=strtran(a,cpath,"$("+amacros[x,1]+if(lgcc,")/",')\')),)})
         endif
      endif
      endif

endif
next


if lGcc
   if at("linux",Getenv("HB_ARCHITECTURE"))>0 .or.  cOs=="Linux"

        hb_FNAMESPLIT(cTopfile,@cPath ,@cTest, @cExt , @cDrive)
        cExt:=substr(cExt,2)
          Fwrite( nLinkHandle, "PROJECT = " + if(isupper(cExt),Strtran( cTopfile, ".PRG", "" ),Strtran( cTopfile, ".prg", "" )) + " $(PR) "+CRLF )
   else
        hb_FNAMESPLIT(cTopfile,@cPath ,@cTest, @cExt , @cDrive)
        cExt:=substr(cExt,2)
        Fwrite( nLinkHandle, "PROJECT = " + if(isupper(cExt),cTest+"."+Strtran( cExt, "PRG", "EXE" ),cTest+"."+Strtran( cExt, "prg", "exe" )) +" $(PR) "+ CRLF )
   endif
else
        hb_FNAMESPLIT(cTopfile,@cPath ,@cTest, @cExt , @cDrive)
        cExt:=substr(cExt,2)
Fwrite( nLinkHandle, "PROJECT = " + if(isupper(cExt),cTest+"."+Strtran( cExt, "PRG", "exe" ),cTest+"."+Strtran( cExt, "prg", "exe" )) +" $(PR) "+ CRLF )
endif
  hb_FNAMESPLIT(cTopfile,@cPath ,@cTest, @cExt , @cDrive)
  cExt:=substr(cExt,2)
if !lextended
   Fwrite( nLinkHandle, "OBJFILES = " )
   if len(aObjs)<1

      Fwrite( nLinkHandle,  +" $(OB) "+ CRLF )
   else
   aeval(aObjs,{|x,i| if( (i<> Len( aObjs ) .and. x<>cTopfile), Fwrite( nLinkHandle, ' '+ alltrim( x ) ),      Fwrite( nLinkHandle," " +  " "+alltrim( x ) +" $(OB) "+ CRLF ))})
   endif
   hb_FNAMESPLIT(cTopfile,@cPath ,@cTest, @cExt , @cDrive)
   cExt:=substr(cExt,2)
   Fwrite( nLinkHandle, "CFILES =" )

   if len(aCs)<1
      Fwrite( nLinkHandle,  +" $(CF)"+ CRLF )

   else
aeval(aCs,{|x,i| if( (i<> Len( aCs ).and. x<>cTopfile), Fwrite( nLinkHandle, ' '+ alltrim( x ) ),      Fwrite( nLinkHandle," " +  alltrim( x ) +" $(CF) "+ CRLF ))})
   endif
   Fwrite( nLinkHandle, "PRGFILE ="  )

   if len(aPrgs)<1
      Fwrite( nLinkHandle,  +" $(PS)"+ CRLF )
   else  
      aeval(aPrgs,{|x,i| if(i<> Len( aPrgs ), Fwrite( nLinkHandle, ' '+ alltrim( x ) ),      Fwrite( nLinkHandle," " +  alltrim( x ) +" $(PS) "+ CRLF ))})
   endif

else
   Fwrite( nLinkHandle, "OBJFILES ="  )
   if len(aObjs)<1

      Fwrite( nLinkHandle,  +" $(OB) "+ CRLF )
   else
   aeval(aobjs,{|x,i| if((i<> Len( aobjs ) .and. x<>cTopfile), Fwrite( nLinkHandle, ' '+ alltrim( x ) ),      Fwrite( nLinkHandle," " + alltrim( x ) +" $(OB) "+ CRLF ))})
   endif
   Fwrite( nLinkHandle, "PRGFILES ="  )

   if len(aPrgs)<1
      Fwrite( nLinkHandle,  +" $(PS)"+ CRLF )
   else  
   aeval(aPrgs,{|x,i| if(i<> Len( aPrgs ) , Fwrite( nLinkHandle, ' '+ alltrim( x ) ),      Fwrite( nLinkHandle," " +  alltrim( x ) +" $(PS) "+ CRLF ))})
   endif
Fwrite( nLinkHandle, "OBJCFILES =" )
if len(aObjsc)<1

Fwrite( nLinkHandle,  +" $(OB) "+ CRLF )
else

//Fwrite( nLinkHandle, "OBJFILES =" + if(isupper(cTopfile),Strtran( cTopfile, ".PRG", ".OBJ" ),Strtran( cTopfile, ".prg", ".obj" )))
aeval(aObjsc,{|x,i| if(i<> Len( aobjsc ), Fwrite( nLinkHandle, ' '+ alltrim( x ) ),      Fwrite( nLinkHandle," " +  alltrim( x ) +" $(OB) "+ CRLF ))})
endif

   Fwrite( nLinkHandle, "CFILES =" )
   if len(aCs)<1
   Fwrite( nLinkHandle,  +" $(CF)"+ CRLF )
   //
else

//Fwrite( nLinkHandle, "CFILES =" + if(isupper(cTopfile),Strtran( cTopfile, ".PRG", ".C" ),Strtran( cTopfile, ".prg", ".c" )))
aeval(aCs,{|x,i| if(i<> Len( aCs ), Fwrite( nLinkHandle, ' '+ alltrim( x ) ),      Fwrite( nLinkHandle," " +  alltrim( x ) +" $(OB) "+ CRLF ))})
endif

endif
Fwrite( nLinkHandle, "RESFILES = " + CRLF )
Fwrite( nLinkHandle, "RESDEPEN = $(RESFILES)" + CRLF )
if lRddads
    cDefBccLibs+=" rddads.lib ace32.lib"
endif
if lBcc .or. lVcc
    If lFwh
        Fwrite( nLinkHandle, "LIBFILES = $(FWH)\lib\fiveh.lib $(FWH)\lib\fivehc.lib " + cDefBccLibs + CRLF )
   elseif lCw
        Fwrite( nLinkHandle, "LIBFILES = $(C4W)\c4wclass.lib $(C4W)\wbrowset.lib $(C4W)\otabt.lib $(C4W)\clip4win.lib" + cDefBccLibs + CRLF )
   else
        Fwrite( nLinkHandle, "LIBFILES = " +cDefBccLibs + CRLF )
   endif
elseif lGcc
      if  cOs=="Linux"
          Fwrite( nLinkHandle, "LIBFILES = " +cDeflibGccLibs + CRLF )
      elseif cOs=="OS/2"
          Fwrite( nLinkHandle, "LIBFILES = " + cgcclibsos2 + CRLF )
      else
          Fwrite( nLinkHandle, "LIBFILES = " +cDefgccLibs + CRLF )
      endif
endif
 Fwrite( nLinkHandle, "DEFFILE = "+CRLF)
 fWrite( nLinkHandle, "HARBOURFLAGS = " +cDefHarOpts+CRLF)
if lBcc
 Fwrite( nLinkHandle, "CFLAG1 =  -OS $(CFLAGS) -d -L$(BHC)\lib;$(FWH)\lib -c"+CRLF)
 Fwrite( nLinkHandle, "CFLAG2 =  -I$(BHC)\include;$(BCB)\include" +CRLF)

 Fwrite( nLinkHandle, "RFLAGS = "+CRLF)
 Fwrite( nLinkHandle, "LFLAGS = -L$(BCB)\lib\obj;$(BCB)\lib;$(BHC)\lib;$(FWH)\lib -Gn -M -m -s" + if(lFwh,"-Tpe","")+CRLF)
 Fwrite( nLinkHandle, "IFLAGS = " +CRLF)
 Fwrite( nLinkHandle, "LINKER = ilink32"+CRLF)
 Fwrite( nLinkHandle, " "+CRLF)
 Fwrite( nLinkHandle, "ALLOBJ = " +if(lFwh,"c0w32.obj","c0x32.obj")+ " $(OBJFILES)"+CRLF)
 Fwrite( nLinkHandle, "ALLRES = $(RESFILES)"+CRLF)
 Fwrite( nLinkHandle, "ALLLIB = $(LIBFILES) import32.lib cw32.lib"+CRLF)
 Fwrite( nLinkHandle, ".autodepend"+CRLF)
elseif lVcc
 Fwrite( nLinkHandle, "CFLAG1 =  -I$(INCLUDE_DIR) -TP -W3 -nologo $(C_USR) $(CFLAGS)"+CRLF)
 Fwrite( nLinkHandle, "CFLAG2 =  -c"+CRLF)
 Fwrite( nLinkHandle, "RFLAGS = "+CRLF)
 Fwrite( nLinkHandle, "LFLAGS = /LIBPATH:$(BCB)\lib;$(BHC)\lib;$(C4W)\lib /SUBSYSTEM:CONSOLE"+CRLF)
 Fwrite( nLinkHandle, "IFLAGS = "+CRLF)
 Fwrite( nLinkHandle, "LINKER = link"+CRLF)
 Fwrite( nLinkHandle, " "+CRLF)
 Fwrite( nLinkHandle, "ALLOBJ = "+if(lCw,"$(C4W)\initc.obj","")+"$(OBJFILES)"+CRLF)
 Fwrite( nLinkHandle, "ALLRES = $(RESFILES)"+CRLF)
 Fwrite( nLinkHandle, "ALLLIB = $(LIBFILES) comdlg32.lib shell32.lib user32.lib gdi32.lib"+CRLF)

elseif lGcc
 Fwrite( nLinkHandle, "CFLAG1 = "+if(at("Linux",cOs)>0 ,"-I$(HB_INC_INSTALL)"," -I$(BHC)/include")+ " -c -Wall"+CRLF)
 Fwrite( nLinkHandle, "CFLAG2 = "+if(at("Linux",cOs)>0 ,"-L $(HB_LIB_INSTALL)"," -L $(BHC)/lib")+CRLF)
 Fwrite( nLinkHandle, "RFLAGS = "+CRLF)
 Fwrite( nLinkHandle, "LFLAGS = $(CFLAG2)"+CRLF)
 Fwrite( nLinkHandle, "IFLAGS = "+CRLF)
 Fwrite( nLinkHandle, "LINKER = gcc"+CRLF)
 Fwrite( nLinkHandle, " "+CRLF)
 Fwrite( nLinkHandle, "ALLOBJ = $(OBJFILES) "+CRLF)
 Fwrite( nLinkHandle, "ALLRES = $(RESFILES) "+CRLF)
 Fwrite( nLinkHandle, "ALLLIB = $(LIBFILES) "+CRLF)
 Fwrite( nLinkHandle, ".autodepend"+CRLF)
endif
Fwrite( nLinkHandle, " "+CRLF)
Fwrite( nLinkHandle, "#COMMANDS"+CRLF)

aeval(aCommands,{|xItem| Fwrite( nLinkHandle, xitem[1]+CRLF),Fwrite( nLinkHandle, xitem[2]+CRLF),Fwrite( nLinkHandle, " "+CRLF)})
if lBcc
        Fwrite( nLinkHandle, "#BUILD"+CRLF)
        Fwrite( nLinkHandle, " "+CRLF)
        Fwrite( nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)"+CRLF)
        Fwrite( nLinkHandle, "    $(BCB)\BIN\$(LINKER) @&&!"+CRLF)
        Fwrite( nLinkHandle, "    $(LFLAGS) +"+CRLF)
        Fwrite( nLinkHandle, "    $(ALLOBJ), +"+CRLF)
        Fwrite( nLinkHandle, "    $(PROJECT),, +"+CRLF)
        Fwrite( nLinkHandle, "    $(ALLLIB), +"+CRLF)
        Fwrite( nLinkHandle, "    $(DEFFILE), +"+CRLF)
        Fwrite( nLinkHandle, "    $(ALLRES) "+CRLF)
        Fwrite( nLinkHandle, "!"+CRLF)


elseif lVcc
        Fwrite( nLinkHandle, "#BUILD"+CRLF)
        Fwrite( nLinkHandle, ""+CRLF)
        Fwrite( nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)"+CRLF)
        Fwrite( nLinkHandle, "    $(BCB)\BIN\$(LINKER) @&&!"+CRLF)
        Fwrite( nLinkHandle, "    $(LFLAGS)"+CRLF)
        Fwrite( nLinkHandle, "    $(ALLOBJ) "+CRLF)
        Fwrite( nLinkHandle, "    $(PROJECT)"+CRLF)
        Fwrite( nLinkHandle, "    $(PROJECTMAP)"+CRLF)
        Fwrite( nLinkHandle, "    $(ALLLIB) "+CRLF)
        Fwrite( nLinkHandle, "    $(DEFFILE) "+CRLF)
        Fwrite( nLinkHandle, "    $(ALLRES) "+CRLF)
        Fwrite( nLinkHandle, "!"+CRLF)


elseif lGcc
        Fwrite( nLinkHandle, "#BUILD"+CRLF)
        Fwrite( nLinkHandle, " "+CRLF)
        Fwrite( nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)"+CRLF)
        if at('Linux',cOs)>0
           Fwrite( nLinkHandle, "    $(LINKER) @&&!"+CRLF)
        else
           Fwrite( nLinkHandle, "    $(BCB)\bin\$(LINKER) @&&!"+CRLF)
        endif
        Fwrite( nLinkHandle, "    $(PROJECT) "+CRLF)
        Fwrite( nLinkHandle, "    $(ALLOBJ)  "+CRLF)
        Fwrite( nLinkHandle, "    $(LFLAGS)  "+CRLF)
        Fwrite( nLinkHandle, "    $(ALLLIB)  "+CRLF)
        Fwrite( nLinkHandle, "!"+CRLF)

endif


Return nil

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Procedure ATTENTION()
*+
*+    Called from ( hbmake.prg   )   4 - function crtmakfile()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+ EOF: HBMAKE.PRG

Function CompUpdatedfiles()

Local cComm
Local cOld
Local nPos
Local nCount
Local nFiles
Local aCtocompile:={}
Local aOrder := listasarray2( aBuildOrder[ 2 ], " " )
local lEnd
Local cErrText:=""
Local xItem
Local nObjPos
For nCount := 1 To Len( aOrder )
if !lextended
   If aOrder[ nCount ] == "$(CFILES)"
      nPos := Ascan( aCommands, { | x | x[ 1 ] == ".prg.c:" } )
      If nPos > 0
         cComm := aCommands[ nPos, 2 ]
         cOld  := cComm
      Endif
      For nFiles := 1 To Len( aPrgs )
         xItem := substr(aPrgs[ nFiles ],rat(if(lgcc,'/','\'),aPrgs[ nFiles ])+1)
         nPos := Ascan( aCs, { | x | x:=substr(x,rat(if(lgcc,'/','\'),x)+1),Left( x, At( ".", x ) ) == Left(xItem, At( ".", xItem ) ) } )
         nObjPos:=Ascan( aobjs, { | x | x:=substr(x,rat(if(lgcc,'/','\'),x)+1),Left( x, At( ".", x ) ) == Left(xItem, At( ".", xItem ) ) } )
         if  fileisnewer(aprgs[nFiles], aobjs[nObjPos])
            If nPos > 0
               aadd(aCtocompile,acs[nPos])
               cComm := Strtran( cComm, "o$*", "o" + aCs[ nPos ] )
               cComm := Strtran( cComm, "$**", aPrgs[ nFiles ] )
               outstd(cComm)
               ! ( cComm )
                  cErrText := memoread( 'test.out' )
                  lEnd := 'C2006' $ cErrText .or. 'No code generated' $ cErrText
            if !lIgnoreErrors .and. lEnd
                quit
            else
                ferase('test.out')

            endif


               cComm := cold
            Endif
            endif

      Next
   Endif
   If aOrder[ nCount ] == "$(OBJFILES)"
      If lGcc
         nPos := Ascan( aCommands, { | x | x[ 1 ] == ".c.o:" } )
      Else
         nPos := Ascan( aCommands, { | x | x[ 1 ] == ".c.obj:" } )
      Endif
      If nPos > 0
         cComm := aCommands[ nPos, 2 ]
         cOld  := ccomm
      Endif
         if len(aCtoCompile)>=1
               for nFiles:=1 to len(aCs)
                  nPos := Ascan( aCs, { | x | Left( x, At( ".", x ) ) == Left( aCtoCompile[nfiles], At( ".", aCtoCompile[nfiles] ) ) } )
                  if nPos==0
                     aadd(aCtoCompile,acs[nFiles])
                  endif
               next
      endif
      For nFiles := 1 To Len( aCtocompile )

         xItem:=substr(aCtocompile[ nFiles ],rat(if(lgcc,'/','\'),aCtocompile[ nFiles ])+1)
         nPos := Ascan( aObjs, { | x | x:=substr(x,rat(if(lgcc,'/','\'),x)+1),Left( x, At( ".", x ) ) == Left( aCtocompile[ nFiles ], At( ".", xItem ) ) } )
         If nPos > 0
            cComm := Strtran( cComm, "o$*", "o" + aObjs[ nPos ] )
            cComm := Strtran( cComm, "$**", aCtocompile[ nFiles ] )
            outstd( " ")
            // ? cComm
            outstd(cComm)
            ! ( cComm )
            ccomm := cold
         Endif
      Next
   Endif
else /**************Extended mode ******/////
   If aOrder[ nCount ] == "$(CFILES)"
      nPos := Ascan( aCommands, { | x | x[ 1 ] == ".c.obj:" } )
      If nPos > 0
         cComm := aCommands[ nPos, 2 ]
         cOld  := cComm
      else
         nPos := Ascan( aCommands, { | x | x[ 1 ] == ".C.OBJ:" } )
            If nPos > 0
            cComm := aCommands[ nPos, 2 ]
            cOld  := cComm
         endif
      Endif
      For nFiles := 1 To Len( acs )
         xItem := substr(acs[nFiles],rat(if(lgcc,'/','\'),acs[nFiles])+1)
         nPos := Ascan( aObjsc, { | x | x:=substr(x,rat(if(lgcc,'/','\'),x)+1),Left( x, At( ".", x ) ) == Left( xitem, At( ".", xitem ) ) } )
         if  fileisnewer(aCs[nFiles],aobjsc[nPos])
            If nPos > 0
               cComm := Strtran( cComm, "o$*", "o" + aobjs[ nPos ] )
               cComm := Strtran( cComm, "$**", acs[ nFiles ] )
               cComm += " > test.out"
            outstd(cComm)
               ! ( cComm )
                  cErrText := memoread( 'test.out' )
                  lEnd := 'Error' $ cErrText 
            if !lIgnoreErrors .and. lEnd
                quit
            else
                ferase('test.out')

            endif
               cComm := cold
            Endif
            endif
      Next
   Endif
   If aOrder[ nCount ] == "$(OBJFILES)"
      If lGcc
         nPos := Ascan( aCommands, { | x | x[ 1 ] == ".prg.o:" } )
      Else
         nPos := Ascan( aCommands, { | x | x[ 1 ] == ".prg.obj:" } )
      Endif
      If nPos > 0
         cComm := aCommands[ nPos, 2 ]
         cOld  := ccomm
      else
         If lGcc
            nPos := Ascan( aCommands, { | x | x[ 1 ] == ".PRG.O:" } )
         Else
            nPos := Ascan( aCommands, { | x | x[ 1 ] == ".PRG.OBJ:" } )
         endif
      Endif

      for nFiles := 1 to len(aprgs)
         xItem := substr(aprgs[nFiles],rat(if(lgcc,'/','\'),aprgs[nFiles])+1)
         nPos := ascan( aobjs, { | x | x:=substr(x,rat(if(lgcc,'/','\'),x)+1), Left( x, At( ".", x ) ) == Left( xItem, At( ".", xitem ) ) } )
         if  fileisnewer(aprgs[nFiles],aobjs[npos])
            If nPos > 0
               cComm := Strtran( cComm, "o$*", "o" + aObjs[ nPos ] )
               cComm := Strtran( cComm, "$**", aprgs[ nFiles ] )
               cComm += " > test.out"
               outstd( " ")

                ? cComm
                  outstd(cComm)
               ! ( cComm )
                cErrText := memoread( 'test.out' )
               lEnd := 'C2006' $ cErrText .or. 'No code generated' $ cErrText
               if !lIgnoreErrors .and. lEnd
                  quit
            else
                ferase('test.out')

               endif

               ccomm := cold
            Endif
         endif
      Next
   Endif


endif

   If aOrder[ nCount ] == "$(RESDEPEN)"
      nPos := Ascan( aCommands, { | x | x[ 1 ] == ".rc.res:" } )
      If nPos > 0
         cComm := aCommands[ nPos, 2 ]
      Endif
      For nFiles := 1 To Len( aRes )
         //            nPos:=ascan(aObjs,{|x| left(x,at(".",x)) == left(acs[nFiles],at(".",acs[nFiles]))})
         If !Empty( ares[ nFiles ] )
            cComm := Strtran( cComm, "$<", aRes[ nFiles ] )
            outstd(" ")
            ! ( cComm )
         Endif
      Next
   Endif

Next
Return nil

function fileisnewer(cFile,as)
local nCount := 0
IF !lextended
For nCount:=1 to len(aPrgs)
         adir := { cFile,, filedate( cFile ), filetime( cFile ), ;
                   as[nCount], filedate( as[nCount] ), filetime( as[nCount] )}
         if empty( adir[ 7 ] )
            adir[ 2 ] := .t.
         else
            adir[ 2 ] := td2jul( adir[ 4 ], adir[ 3 ] ) > td2jul( adir[ 7 ], adir[ 6 ] )
         endif
next
else
         adir := { cFile,, filedate( cFile ), filetime( cFile ), ;
                   as, filedate( as ), filetime( as )}
         if empty( adir[ 7 ] )
            adir[ 2 ] := .t.
         else
            adir[ 2 ] := td2jul( adir[ 4 ], adir[ 3 ] ) > td2jul( adir[ 7 ], adir[ 6 ] )
         endif


endif
return aDir[2]


Func crtlibmakfile( cFile )

Local ain          := {}
Local aOut         := {}
Local aSrc         := Directory( "*.prg" )
Local nLenaSrc     := Len( asrc )
Local nLenaOut

Local aOutC         := {}
Local aSrcC         := Directory( "*.c" )
Local cOs          := "Win32"
Local cCompiler    := "BCC"
Local cfwhpath     := left(cfile,at('.',cfile)-1) + space(40)



Local lAutomemvar  := .f.
Local lvarismemvar := .f.
Local ldebug       := .f.
Local lSupressline := .f.

Local cDefHarOpts  := ""
Local cObjDir:='obj'+space(20)

Local lCompMod     := .f.
Local x,y,nPos as numeric
Local lGenppo      := .f.
Local getlist      := {}

local citem:=""
Local cExt:=""
Local cDrive:=""
local cPath:=""
Local cTest:=""
local cLast:=''
Local cUserdef:=space(40)
Local cUserInclude:=space(40)
nLinkHandle := Fcreate( cFile )
WriteMakeFileHeader()

Cls
Setcolor( 'w/b+,b+/w,w+/b,w/b+,w/b,w+/b' )
@  0,  0, Maxrow(), Maxcol() Box( Chr( 201 ) + Chr( 205 ) + Chr( 187 ) + Chr( 186 ) + Chr( 188 ) + Chr( 205 ) + Chr( 200 ) + Chr( 186 ) + Space( 1 ) )
ATTENTION( "Enviroment options", 0 )
@  1,  1 Say "Select Os"
@  1, 12 Get cos radio { "Win32", "OS/2", "Linux" }
@  1, 23 Say "Select C Compiler"
@  1, 40 Get cCompiler radio { "BCC", "MSVC", "GCC" }
Read
set cursor on
   @  4,  1 Say "Library name with our extention" Get cfwhpath pict "@s15"
   @  4,55   Say "Obj Files Dir" get cObjDir
ATTENTION( "Harbour Options", 5 )

@  6,  1 Get lautomemvar   checkbox  caption "Automatic memvar declaration"
@  6, 43 Get lvarismemvar checkbox  caption "Variables are assumed M->"
@  7,  1 Get lDebug checkbox  caption "Debug info"
@  7, 43 Get lSupressline checkbox  caption "Suppress line number information"
@  8,  1 Get lGenppo checkbox  caption "Generate pre-processed output"
@  8, 43 Get lCompMod checkbox  caption "compile module only"
@  9,  1 Say "User Defines " get cUserDef pict "@s20"
@  9, 43 Say "User include Path" get cUserInclude pict "@s20"
Read
if !empty(cUserDef)
      cDefHarOpts+= " -D"+alltrim(cUserDef) +" "
endif
if !empty(cUserInclude)
      cDefHarOpts+= " -I"+alltrim(cUserInclude) +" "
endif

lBcc := If( At( "BCC", cCompiler ) > 0, .t., .f. )
lVcc := If( At( "MSVC", cCompiler ) > 0, .t., .f. )
lGcc := If( At( "GCC", cCompiler ) > 0, .t., .f. )
cObjDir:=Alltrim(cObjDir)
if !empty(cobjDir)
   if dirchange(cobjDir)!=0
      makedir(cobjDir)
   else
      dirchange('..')
   endif
endif

amacros:=GetSourceDirMacros(lgcc,cos)

if lGcc
   cObjDir:=alltrim(cObjDir)
   if !empty(cObjDir)
      cObjDir+='/'
   endif
   cTest:=cObjDir+'/'
else
   cObjDir:=alltrim(cObjDir)
   if !empty(cObjDir)
      cObjDir+='\'
   endif
   cTest:=cObjDir+'\'
endif

aeval(amacros,{|x,y|cItem:=substr(x[2],1,len(x[2])),if(at(citem,cTest)>0,(amacros[y,1]:='OBJ',amacros[y,2]:=cObjDir),)})

if lAutomemvar
cDefHarOpts+=" -a "
endif
if lvarismemvar
cDefHarOpts+=" -v "
endif
if ldebug
cDefHarOpts+=" -b "
endif
if lSupressline
cDefHarOpts+=" -l "
endif
if lGenppo
cDefHarOpts+=" -p "
endif
if lCompmod
cDefHarOpts+=" -m "
endif


If lBcc
   Aadd( aCommands, { ".cpp.obj:", "$(BCB)\BIN\bcc32 $(CFLAG1) $(CFLAG2) -o$* $*" } )

   Aadd( aCommands, { ".c.obj:", "$(BCB)\BIN\bcc32 -I$(BHC)\include $(CFLAG1) $(CFLAG2) -o$* $**" } )
if lextended
   Aadd( aCommands, { ".prg.obj:", "$(BHC)\bin\harbour -n -go -I$(BHC)\include $(HARBOURFLAGS) -I$(FWH)\include -o$* $**" } )
else
   Aadd( aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)\include $(HARBOURFLAGS) -I$(FWH)\include -o$* $**" } )
endif
   Aadd( aCommands, { ".rc.res:", "$(BCB)\BIN\brcc32 $(RFLAGS) $<" } )

Elseif lGcc
   if at("linux",Getenv("HB_ARCHITECTURE"))>0 .or. cOs=="Linux"
   Aadd( aCommands, { ".cpp.o:", "gcc $(CFLAG1) $(CFLAG2) -o$* $*" } )

   Aadd( aCommands, { ".c.o:", "gcc -I$(HB_INC_INSTALL) $(CFLAG1) $(CFLAG2) -I. -o$* $**" } )
if lextended
   Aadd( aCommands, { ".prg.o:", "harbour -n $(HARBOURFLAGS) -I$(HB_INC_INSTALL) -I. -go  -o$* $**" } )
else
   Aadd( aCommands, { ".prg.c:", "harbour -n $(HARBOURFLAGS) -I$(HB_INC_INSTALL) -I.  -o$* $**" } )
endif
else
   Aadd( aCommands, { ".cpp.o:", "$(BCB)\bin\gcc $(CFLAG1) $(CFLAG2) -o$* $*" } )

   Aadd( aCommands, { ".c.o:", "$(BCB)\bin\gcc -I$(BHC)/include $(CFLAG1) $(CFLAG2) -I. -o$* $**" } )
if lextended
   Aadd( aCommands, { ".prg.o:", "$(BHC)\bin\harbour -n -go -I$(BHC)/include $(HARBOURFLAGS)  -o$* $**" } )
else
   Aadd( aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)/include $(HARBOURFLAGS)  -o$* $**" } )
endif
endif

Elseif lVcc
   Aadd( aCommands, { ".cpp.obj:", "$(BCB)\bin\cl $(CFLAG1) $(CFLAG2) -Fo$* $*" } )

   Aadd( aCommands, { ".c.obj:", "$(BCB)\bin\cl -I$(BHC)\include $(CFLAG1) $(CFLAG2) -Fo$* $**" } )
if lextended
   Aadd( aCommands, { ".prg.obj:", "$(BHC)\bin\harbour -go -n -I$(BHC)\include $(HARBOURFLAGS) -I$(C4W)\include -o$* $**" } )
else
   Aadd( aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)\include $(HARBOURFLAGS) -I$(C4W)\include -o$* $**" } )
endif
   Aadd( aCommands, { ".rc.res:", "$(BCB)\BIN\rc $(RFLAGS) $<" } )
Endif

attention( 'Spacebar to select, Enter to continue process', 22 )
if !lRecurse
   ain:=GetSourceFiles(.f.,lGcc,cOs) 
   nLenaSrc:=Len(ain)
else
   ain:=GetSourceFiles(,lGcc,cOs)
   nLenaSrc:=Len(ain)
endif
aOut := Aclone( aIn )

pickarry( 10, 15, 19, 64, aIn, aOut )

nLenaOut := Len( aOut )
aeval(aout,{|x,y| aout[y]:=Trim(  substr(aOut[ y ],1,at(' ',aout[y])))})
aeval(aout,{|xItem| if( at('.c',xItem)>0 .or. at('.C',xItem)>0,aadd(aoutc,xitem),)})
aeval(aoutc,{|x,z| citem:=x,z:=ascan(aout,{|t| t=citem}), if(z>0,asize(adel(aout,z),len(aout)-1),)})

   aOut := Asort( aOut )
   aPrgs   := aclone(aout)

   aObjs := aClone(aout)
   aeval(aobjs,{|xItem,x|  hb_FNAMESPLIT(xiTem,@cPath ,@cTest, @cExt , @cDrive),cext:=substr(cExt,2),if(!lGcc,aObjs[ x ]:=cObjDir+cTest+"."+exte( cExt,2),aObjs[ x ]:=cObjDir+cTest+"."+exte( cExt,3))})
   aCs:=aclone(aoutc)

if !lextended
   
   aeval(aOutc,{|xItem,x| hb_FNAMESPLIT(xiTem,@cPath ,@cTest, @cExt , @cDrive),cext:=substr(cExt,2),   if(!lGcc,aadd(aObjs,cObjDir+cTest+"."+exten( cExt,2)),aadd(aObjs,cObjDir+cTest+"."+exten( cExt,1)))})
   aeval(aout,{|xItem| hb_FNAMESPLIT(xiTem,@cPath ,@cTest, @cExt , @cDrive), cExt:=substr(cExt,2),aadd(aCs,cObjDir+cTest+"."+exte( cExt,1))})
else
aObjsc := aClone(aoutc)
 aeval(aoutc,{|xItem,x|  hb_FNAMESPLIT(xiTem,@cPath ,@cTest, @cExt , @cDrive),cext:=substr(cExt,2),if(!lGcc,aObjsc[ x ]:=cObjDir+cTest+"."+exten( cExt,2),aObjsc[ x ]:=cObjDir+cTest+"."+exten( cExt,1))})
endif

for x:=1 to len(amacros)
   if !empty(amacros[x,2])       
         cItem := amacros[x,2]
         
       nPos:=ascan(aprgs,{|z| hb_FNAMESPLIT(z,@cPath ,@cTest, @cExt , @cDrive),cpath==citem})
      if nPos>0
      AEVAL(aprgs,{|a,b| hb_FNAMESPLIT(a,@cPath ,@cTest, @cExt , @cDrive),if(cPath==citem,aprgs[b]:=strtran(a,cpath,"$("+amacros[x,1]+')\'),)})
      if !amacros[x,3]
            Fwrite( nLinkHandle, amacros[x,1] + ' = ' +left(amacros[x,2],len(amacros[x,2])-1) +" " + CRLF )
            amacros[x,3]:=.t.
      endif
      endif
      nPos:=ascan(acs,{|z| hb_FNAMESPLIT(z,@cPath ,@cTest, @cExt , @cDrive),cpath==citem})
      if nPos>0
            AEVAL(acs,{|a,b| hb_FNAMESPLIT(a,@cPath ,@cTest, @cExt , @cDrive),if(cPath==citem,acs[b]:=strtran(a,cpath,"$("+amacros[x,1]+if(lgcc,")/",')\')),)})
            if !amacros[x,3]
                  Fwrite( nLinkHandle, amacros[x,1] + ' = ' +left(amacros[x,2],len(amacros[x,2])-1) +" " + CRLF )
                  amacros[x,3]:=.t.
            endif
      endif
      nPos:=ascan(aObjs,{|z| hb_FNAMESPLIT(z,@cPath ,@cTest, @cExt , @cDrive),cpath==citem})
      if nPos>0
         if !empty(cObjDir)
            AEVAL(aObjs,{|a,b| hb_FNAMESPLIT(a,@cPath ,@cTest, @cExt , @cDrive),if(cPath==citem,aObjs[b]:=strtran(a,cpath,"$("+amacros[x,1]+if(lgcc,")/",')\')),)})
            Fwrite( nLinkHandle, amacros[x,1] + ' = ' +left(amacros[x,2],len(amacros[x,2])-1) +" " + CRLF )
         endif
      endif
      if lExtended
      nPos:=ascan(aObjsc,{|z| hb_FNAMESPLIT(z,@cPath ,@cTest, @cExt , @cDrive),cpath==citem})
      if nPos>0
         if !empty(cObjDir)
            AEVAL(aObjsc,{|a,b| hb_FNAMESPLIT(a,@cPath ,@cTest, @cExt , @cDrive),if(cPath==citem,aObjsc[b]:=strtran(a,cpath,"$("+amacros[x,1]+if(lgcc,")/",')\')),)})
          endif
      endif
      endif

endif
next

if lGcc
   if at("linux",Getenv("HB_ARCHITECTURE"))>0 .or.  cOs=="Linux"
        Fwrite( nLinkHandle, "PROJECT = " + alltrim(cfwhpath)+".a "+CRLF )
   else
        Fwrite( nLinkHandle, "PROJECT = " + alltrim(lower(cfwhpath))+".a "+CRLF )
   endif
else
    Fwrite( nLinkHandle, "PROJECT = " + alltrim(lower(cfwhpath))+".lib "+CRLF )

endif
if !lextended
Fwrite( nLinkHandle, "OBJFILES =" )
if len(aObjs)<1

Fwrite( nLinkHandle,  +" $(OB) "+ CRLF )
else

aeval(aObjs,{|x,i| if(i<> Len( aobjs ), Fwrite( nLinkHandle, ' '+ alltrim( x ) ),      Fwrite( nLinkHandle," " +  alltrim( x ) +" $(OB) "+ CRLF ))})
endif
Fwrite( nLinkHandle, "CFILES =" )
if len(aCs)<1
Fwrite( nLinkHandle,  +" $(CF)"+ CRLF )
//
else

aeval(aCs,{|x,i| if(i<> Len( aCs ), Fwrite( nLinkHandle, ' '+ alltrim( x ) ),      Fwrite( nLinkHandle," " +  alltrim( x ) +" $(CF) "+ CRLF ))})
endif
   Fwrite( nLinkHandle, "PRGFILE ="  )

   if len(aPrgs)<1
      Fwrite( nLinkHandle,  +" $(PS)"+ CRLF )
   else  
      aeval(aPrgs,{|x,i| if(i<> Len( aPrgs ), Fwrite( nLinkHandle, ' '+ alltrim( x ) ),      Fwrite( nLinkHandle," " +  alltrim( x ) +" $(PS) "+ CRLF ))})
   endif

else /****extended moded ****/
   Fwrite( nLinkHandle, "OBJFILES ="    )
   if len(aObjs)<1
      Fwrite( nLinkHandle,  +" $(OB) "+ CRLF )
   else
      aeval(aObjs,{|x,i| if(i<> Len( aobjs ), Fwrite( nLinkHandle, ' '+ alltrim( x ) ),      Fwrite( nLinkHandle," " +  alltrim( x ) +" $(OB) "+ CRLF ))})
   endif
   Fwrite( nLinkHandle, "PRGFILES ="  )

   if len(aPrgs)<1
      Fwrite( nLinkHandle,  +" $(PS)"+ CRLF )
   else  
      aeval(aPrgs,{|x,i| if(i<> Len( aPrgs ), Fwrite( nLinkHandle, ' '+ alltrim( x ) ),      Fwrite( nLinkHandle," " +  alltrim( x ) +" $(PS) "+ CRLF ))})
   endif
if Len(aObjsc)>0
   Fwrite( nLinkHandle, "OBJCFILES =" )
   if len(aObjsc)<1
      Fwrite( nLinkHandle,  +" $(OB) "+ CRLF )
   else
      aeval(aObjsc,{|x,i| if(i<> Len( aobjsc ), Fwrite( nLinkHandle, ' '+ alltrim( x ) ),      Fwrite( nLinkHandle," " +  alltrim( x ) +" $(OB) "+ CRLF ))})
   endif
endif
if len(acs)>0
   Fwrite( nLinkHandle, "CFILES =" )
   if len(aCs)<1
      Fwrite( nLinkHandle,  +" $(CF)"+ CRLF )
   else
      aeval(aCs,{|x,i| if(i<> Len( aCs ), Fwrite( nLinkHandle, ' '+ alltrim( x ) ),      Fwrite( nLinkHandle," " +  alltrim( x ) +" $(OB) "+ CRLF ))})
   endif
endif
endif
Fwrite( nLinkHandle, "RESFILES =" + CRLF )
Fwrite( nLinkHandle, "RESDEPEN = $(RESFILES)" + CRLF )
 Fwrite( nLinkHandle, "DEFFILE = "+CRLF)
 fWrite( nLinkHandle, "HARBOURFLAGS = " +cDefHarOpts+CRLF)
if lBcc
 Fwrite( nLinkHandle, "CFLAG1 =  -OS $(CFLAGS) -d -L$(BHC)\lib;$(FWH)\lib -c"+CRLF)
 Fwrite( nLinkHandle, "CFLAG2 =  -I$(BHC)\include;$(BCB)\include" +CRLF)

 Fwrite( nLinkHandle, "RFLAGS = "+CRLF)
 Fwrite( nLinkHandle, "LFLAGS = /P32 /0"+CRLF)
 Fwrite( nLinkHandle, "IFLAGS = " +CRLF)
 Fwrite( nLinkHandle, "LINKER = tlib $(LFLAGS) $(PROJECT)"+CRLF)
 Fwrite( nLinkHandle, " "+CRLF)
 Fwrite( nLinkHandle, "ALLOBJ =  $(OBJFILES) $(OBJCFILES)"+CRLF)
 Fwrite( nLinkHandle, "ALLRES = $(RESFILES)"+CRLF)
 Fwrite( nLinkHandle, "ALLLIB = "+CRLF)
 Fwrite( nLinkHandle, ".autodepend"+CRLF)
elseif lVcc
 Fwrite( nLinkHandle, "CFLAG1 =  -I$(INCLUDE_DIR) -TP -W3 -nologo $(C_USR) $(CFLAGS)"+CRLF)
 Fwrite( nLinkHandle, "CFLAG2 =  -c"+CRLF)
 Fwrite( nLinkHandle, "RFLAGS = "+CRLF)
 Fwrite( nLinkHandle, "LFLAGS = "+CRLF)
 Fwrite( nLinkHandle, "IFLAGS = "+CRLF)
 Fwrite( nLinkHandle, "LINKER = lib $(PROJECT)"+CRLF)
 Fwrite( nLinkHandle, " "+CRLF)
 Fwrite( nLinkHandle, "ALLOBJ = "+CRLF)
 Fwrite( nLinkHandle, "ALLRES = $(RESFILES)"+CRLF)
 Fwrite( nLinkHandle, "ALLLIB = "+CRLF)

elseif lGcc
 Fwrite( nLinkHandle, "CFLAG1 = "+if(at("linux",Getenv("HB_ARCHITECTURE"))>0 ,"-I$(HB_INC_INSTALL)"," -I$(BHC)/include")+ " -c -Wall"+CRLF)
 Fwrite( nLinkHandle, "CFLAG2 = "+if(at("linux",Getenv("HB_ARCHITECTURE"))>0 ,"-L $(HB_LIB_INSTALL)"," -L $(BHC)/lib")+CRLF)
 Fwrite( nLinkHandle, "RFLAGS = "+CRLF)
 Fwrite( nLinkHandle, "LFLAGS = "+CRLF)
 Fwrite( nLinkHandle, "IFLAGS = "+CRLF)
 if at("linux",Getenv("HB_ARCHITECTURE"))>0 .or.  cOs=="Linux" .or. at("linux",lower(os()))>0
    Fwrite( nLinkHandle, "LINKER = ar -M "+CRLF)
 else
    Fwrite( nLinkHandle, "LINKER = $(BCB)\ar -M <"+CRLF)
 endif
 Fwrite( nLinkHandle, " "+CRLF)
 Fwrite( nLinkHandle, "ALLOBJ = $(OBJFILES) $(OBJCFILES) "+CRLF)
 Fwrite( nLinkHandle, "ALLRES = $(RESFILES) "+CRLF)
 Fwrite( nLinkHandle, "ALLLIB = $(LIBFILES) "+CRLF)
 Fwrite( nLinkHandle, ".autodepend"+CRLF)
endif
Fwrite( nLinkHandle, " "+CRLF)
Fwrite( nLinkHandle, "#COMMANDS"+CRLF)
aeval(aCommands,{|xItem| Fwrite( nLinkHandle, xitem[1]+CRLF),Fwrite( nLinkHandle, xitem[2]+CRLF),Fwrite( nLinkHandle, " "+CRLF)})

if lBcc
        Fwrite( nLinkHandle, "#BUILD"+CRLF)
        Fwrite( nLinkHandle, " "+CRLF)
        Fwrite( nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES)"+CRLF)
        Fwrite( nLinkHandle, "    $(BCB)\BIN\$(LINKER) @&&!"+CRLF)
        Fwrite( nLinkHandle, "    $(ALLOBJ)"+CRLF)
        Fwrite( nLinkHandle, "!"+CRLF)


elseif lVcc
        Fwrite( nLinkHandle, "#BUILD"+CRLF)
        Fwrite( nLinkHandle, ""+CRLF)
        Fwrite( nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES)"+CRLF)
        Fwrite( nLinkHandle, "    $(BCB)\BIN\$(LINKER) @&&!"+CRLF)
        Fwrite( nLinkHandle, "    $(ALLOBJ) "+CRLF)
        Fwrite( nLinkHandle, "!"+CRLF)


elseif lGcc
        Fwrite( nLinkHandle, "#BUILD"+CRLF)
        Fwrite( nLinkHandle, " "+CRLF)
        Fwrite( nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES) "+CRLF)
        if at("linux",Getenv("HB_ARCHITECTURE"))>0 .or.  cOs=="Linux"
           Fwrite( nLinkHandle, "    $(LINKER) @&&!"+CRLF)
        else
           Fwrite( nLinkHandle, "    $(BCB)\$(LINKER) @&&!"+CRLF)
        endif
        Fwrite( nLinkHandle, "    $(PROJECT) "+CRLF)
        Fwrite( nLinkHandle, "    $(ALLOBJ)  "+CRLF)

        Fwrite( nLinkHandle, "!"+CRLF)

endif


Return nil

Function setlibBuild()

Local cRead as String
Local nPos as Numeric
Local aMacro as Array
Local aTemp as Array
Local nCount as Numeric
Local aCurobjs as Array
Local nObjPos as Numeric
Local cLib
// ? "setting link file"
cRead  := Alltrim( readln( @leof ) )

nLinkHandle := Fcreate( clinker )

   szProject:=cRead
amacro := listasarray2( cRead, ":" )
If Len( amacro ) > 1
   aTemp := listasarray2( amacro[ 2 ], " " )
   aeval(aTemp,{|xItem|      Aadd( aBuildOrder, xItem)} )
Endif
Aadd( aBuildOrder, amacro[ 1 ] )
cRead := Strtran( cRead, "@&&!", "" )

amacro := listasarray2( cRead, '\' )
   aeval(amacro,{|xMacro|      findmacro( xMacro, @cRead )})
if lbcc .or. lVcc
cLinkcomm   := cRead + "  @" + cLinker
else
cLinkcomm   :=  cRead + " < " + cLinker 

endif

//#define CRLF hb_osnewline()
For nPos := 1 To 7
   cRead  := Alltrim( readln( @leof ) )
   amacro := listasarray2( cRead, " " )
   For ncount := 1 To Len( amacro )
      If At( "$", amacro[ nCount ] ) > 0
             if (amacro[ nCount ] = "$(PROJECT)") .and. lGcc 
             Findmacro(amacro[ nCount ], @cRead )
             fwrite(nLinkHandle,"CREATE " + "lib"+cRead+CRLF)
	     cLib:="lib"+cRead	
         elseif (amacro[ nCount ] =="$(ALLOBJ)")
             findmacro( amacro[ nCount ], @cRead )
             aCurObjs:=ListasArray2(cRead," ")
             for nObjPos:=1 to Len(aCurObjs)
                 if lGcc
                     fWrite(nLinkhandle, "ADDMOD " + aCurObjs[nObjPos] +CRLF)
                 endif
                 if lBcc .or. lVcc
                   if nObjPos<  Len(aCurObjs)
                       fWrite(nLinkhandle, "+-" + aCurObjs[nObjPos] + " &"+CRLF)
                   else
                       fWrite(nLinkhandle, "+-" + aCurObjs[nObjPos] +CRLF)
                   endif
                 endif
              next
         endif
      Endif
   Next
Next
if lGcc 
fwrite(nLinkHandle, "SAVE" +CRLF)
fwrite(nLinkHandle, "END " +CRLF)
endif

Fclose( nLinkhandle )
if lLinux 
cLinkComm += " || rm -f " +cLib
endif
outstd(cLinkComm)

Return nil

func FindCfile(citem,aSrcc)
local nReturnPos:=0
    nReturnPos:=aScan(aSrcc,{|x| lower(x[1])==cItem})
return nReturnPos

#ifndef __HARBOUR__
function HB_OSNEWLINE()
RETURn CHR(13)+CHR(10)
#endif

function checkiffile(cFile)
Local cNextLine:=''
Local cCommand:=''
Local cTemp
cTemp:=substr(cFile,at(" ",cFile)+1)
if file(cTemp)
    cNextLine := Trim( Substr( ReadLN( @lEof ), 1 ) )
    if at("! ",cNextLine)>0
        cCommand:=substr(cNextLine,at(' ',cNextLine)+1)
        run (ccommand)
    endif
    return .t.
endif
return .f.
function checkstdout(cText)
cText:=strtran(cText,"!stdout","")
outstd(cText)
return nil
function CheckifDef(cTemp)

Local nPos
Local cRead:=""
Local aSet     := {}
Local nMakePos

If cTemp == "!endif"
   Return nil
Endif

While at("!endif",cRead)==0
   cRead := Trim( Substr( ReadLN( @lEof ), 1 ) )
   if at("!endif",cRead)>0
      FT_FSKIP(-1)
      exit
   endif

   cTemp := Strtran( cTemp, "!ifdef ", "" )
   if at('=',cRead)>0
if at("\..",cRead)>0
   cRead:=substr(cRead,1,at("\..",cRead)-1)
elseif at("/..",cRead)>0
   cRead:=substr(cRead,1,at("/..",cRead)-1)
endif

      aSet  := listasarray2( cRead, "=" )
      nPos  := Ascan( adefines, { | x | x[ 1 ] == cTemp } )

      If nPos > 0
         cRead    := Alltrim( Strtran( aset[ 1 ], "$(", "" ) )
         cRead    := Strtran( cRead, ")", "" )
         nMakePos := Ascan( amaCros, { | x | x[ 1 ] == cRead } )
         If nMakePos == 0
            Aadd( amacros, { aset[ 1 ], aset[ 2 ] } )
         Endif
      ELSE  /* Locate For !Else    */
         While at("!endif",cRead)==0 
           cRead := Trim( Substr( ReadLN( @lEof ), 1 ) )
               if at("!else",cRead)>0
                  While at("!endif",cRead)==0 
                  cRead := Trim( Substr( ReadLN( @lEof ), 1 ) )
                  if at("!endif",cRead)>0
                     FT_FSKIP(-1)
                     exit
                  endif
                  aSet  := listasarray2( cRead, "=" )
                  Aadd( amacros, { aset[ 1 ], aset[ 2 ] } )
                  enddo
               endif
         Enddo
      Endif
   Elseif at('!stdout',cRead)>0
      checkstdout(cRead)            
   Endif
enddo
Return nil
/*comment*/



function BuildBorCfgFile()
Local nCfg 
If !file(GetMakeDir()  +'\bin\harbour.cfg')
   nCfg:=FCREATE( GetMakeDir()  +'\bin\harbour.cfg')
   fwrite(nCfg,"CC=BCC32"+CRLF)
   fWrite(nCfg,"CFLAGS= -c " +Replacemacros( "-I$(BHC)\include -OS $(CFLAGS) -d -L$(BHC)\lib")+CRLF)
   Fwrite(nCfg,"VERBOSE=NO"+CRLF)
   Fwrite(nCfg,"DELTMP=YES"+CRLF)
   Fclose(nCfg)
Endif
return Nil
function BuildMSCCfgFile()
Local nCfg 
If !file(GetMakeDir()  +'\bin\harbour.cfg')
   nCfg:=FCREATE( GetMakeDir()  +'\bin\harbour.cfg')
   fwrite(nCfg,"CC=cl"+CRLF)
   fWrite(nCfg,"CFLAGS= -c " +Replacemacros( "-I$(INCLUDE_DIR) -TP -W3 -nologo $(C_USR) $(CFLAGS)")+CRLF)

   Fwrite(nCfg,"VERBOSE=NO"+CRLF)
   Fwrite(nCfg,"DELTMP=YES"+CRLF)
   Fclose(nCfg)
Endif
return Nil

Function findHarbourcfg(cCfg)
Local cPath AS STRING := ''
Local lFound AS LOGICAL := .f.
Local cEnv  AS STRING 
Local aEnv as Array of String

Local nPos
if !lLinux .or. lOs2
   cEnv:= Gete( "PATH" )+";"+curdir()
   aEnv   := listasarray2( cEnv, ";" )


   For nPos := 1 To Len( aEnv )
      If File( aenv[ nPos ] + '\harbour.cfg' )
         cPath := aenv[ nPos ]
         lFound := .T.
         Exit
      Endif
   Next
else
   if file('/etc/harbour.cfg')
      lFound:=.t.
      cPath='/etc/harbour.cfg'
   endif
   if file('/usr/local/etc/harbour.cfg')
      lFound:=.t.
      cPath='/usr/local/etc/harbour.cfg'
   endif
endif
cCfg:=cPath
Return lFound
function TestforPrg(cFile)
Local aFiles AS ARRAY :={}
Local cPath AS STRING :=''
Local cTest AS STRING :=""
Local cDrive AS STRING :=""
Local cExt AS STRING :=""
Local cItem AS STRING :=""
Local aDir AS ARRAY 
Local nPos  AS NUMERIC,nFiles AS NUMERIC
      hb_FNAMESPLIT(cFile,@cPath ,@cTest, @cExt , @cDrive)
      cExt:=substr(cExt,2)

aDir:=directory(cTest+'.*')

   For nPos:=1 to 7
      cItem:=cTest+"."+extenprg( cExt,nPos)
      aadd(aFiles,cItem)
   next
   For nFiles:=1 to len(aFiles)
         nPos:=ascan(aDir,{|a| a[1]==aFiles[nFiles]})
         if nPos>0
         Aadd( aPrgs, aFiles[nFiles] )
         endif
next

return nil
Function GetGccDir()

Local cPath AS STRING := ''
Local cEnv  AS STRING 
Local aEnv  AS Array of string
Local nPos as Numeric
if lLinux 
    cpath:="."
else
    cEnv  := Gete( "PATH" )
    aEnv  := listasarray2( cEnv, ";" )

    For nPos := 1 To Len( aEnv )
       If File( aenv[ nPos ] + '\gcc.exe' ) .or. File( Upper( aenv[ nPos ] ) + '\GCC.EXE' )
          cPath := aenv[ nPos ]
          cPath:=left(cPath,rat('\',cPath)-1)
          Exit
       Endif
    Next
endif
Return cPath
Function ConvertParams(cParam) 
   cParam:=strtran(cParam,"/","-")
   cParam:=strtran(cParam,"-elx","-ELX")
   cParam:=strtran(cParam,"-el","-ELX")
   cParam:=strtran(cParam,"-ex","-EX")
   cParam:=strtran(cParam,"-e","-EX")
   cParam:=strtran(cParam,"-i","-I")
   cParam:=strtran(cParam,"-p","-P")
   cParam:=strtran(cParam,"-b","-B")
   cParam:=strtran(cParam,"-gl","-GL")
   cParam:=strtran(cParam,"-g","-G")
   cParam:=strtran(cParam,"-v","-V")
   cParam:=strtran(cParam,"-f","-F")
   cParam:=strtran(cParam,"-r","-R")
return cParam
Function ShowHelp()
   Local cOs:=OS()
   ?? "Harbour Make Utility"
   ? "Copyright 2000,2001 Luiz Rafael Culik <culik@sl.conex.net>"
   ? ""
   ? "Syntax:  hbmake cFile [options]"
   ? ""
   ? "Options:  /e[x]  Create a new Makefile. If /ex is"
   ? "          used it creates a new make file in extended mode"
   ? "          /el[x]  Create a new Makefile. If /elx is"
   ? "          used it creates a new make file to build a library in extended mode"
   ? "          /D  Define a macro"
   ? "          /p  Print all commands and depencies"
   if at("OS/2",cOs)>0
      ? "          /b  Use BCC as C compiler"
      ? "          /g+ Use GCC as C compiler"
   else
      ? "          /b+ Use BCC as C compiler"
      ? "          /g  Use GCC as C compiler"
   endif
   ? "          /gl Use GCC as C compiler in Linux"   
   ? "          /v  Use MSVC as C compiler"
   ? "          /f  Force recompiltion of all files"
   ? "          /i  Ignore errors returned by command"
   ? "          /r  Recurse Source Directory"

   ? "          Note: /p and /D can be used together"
   ? "          Note: /r and /e[x]/el[x] can be used together"
   ? "          Options with + are the default values"
   ? "          -D switch can accept multiple macros on the same line"
   ? "          or use one macro per -D switch"
Return Nil
Function ProcessParameters(cParams)
   Local aDef
   if at("-F",cParams)>0
      lForce := .T.
      cParams:=strtran(cParams,"-F","")
   Endif

   if at("-B",cParams)>0
      lBcc := .T.
      lGcc := .F.
      lVcc := .F.
      cParams:=strtran(cParams,"-B","")

   Endif
if at("-GL",cParams)>0
      lBcc := .F.
      lGcc := .T.
      lVcc := .F.
      lLinux := .T.
      cParams:=strtran(cParams,"-GL","")

   Endif

if at("-G",cParams)>0
      lBcc := .F.
      lGcc := .T.
      lVcc := .F.
      cParams:=strtran(cParams,"-G","")

   Endif
   if at("-V",cParams)>0

      lBcc := .F.
      lGcc := .F.
      lVcc := .T.
      cParams:=strtran(cParams,"-V","")

   Endif

if at("-I",cParams)>0

      lIgnoreErrors := .T.
      cParams:=strtran(cParams,"-I","")

   Endif
if at("-R",cParams)>0
   lRecurse:=.T.
      cParams:=strtran(cParams,"-R","")

   Endif

    if at("-P",cParams)>0
      lPrint := .t.
      cParams:=strtran(cParams,"-P","")

   Endif
    if at("-D",cParams)>0
      cParams:="-D"+strtran(cParams,"-D",";")
         cParams=strtran(cParams,"-D;","-D")

      adef := ListAsArray2( alltrim(Substr( cParams, 3 )), ";" )
      aeval(aDef,{|xDef| if(at('=',xDef)>0,GetParaDefines( xDef ),)})
   Endif
if at("-EL",cParams)>0 .or. at("-ELX",cParams)>0
      if at("-ELX",cParams)>0
         lExtended := .T.
         cParams:=strtran(cParams,"-ELX","")
      Else
         cParams:=strtran(cParams,"-EL","")
      endif
      lLibrary:=.T.
      lEditMode:=.T.
   Endif

if at("-E",cParams)>0 .or. at("-EX",cParams)>0
      if at("-EX",cParams)>0
       
         cParams:=strtran(cParams,"-EX","")
      else
         cParams:=strtran(cParams,"-E","")
      endif
         lExtended := .T.
         lEditMode:=.T.
   Endif

Return Nil
function WriteMakeFileHeader()
Fwrite( nLinkHandle, "#BCC" + CRLF )
Fwrite( nLinkHandle, "VERSION=BCB.01" + CRLF )
Fwrite( nLinkHandle, "!ifndef BCB" + CRLF )
Fwrite( nLinkHandle, "BCB = $(MAKEDIR)" + CRLF )
Fwrite( nLinkHandle, "!endif" + CRLF )
Fwrite( nLinkHandle,  CRLF )
Fwrite( nLinkHandle, "!ifndef BHC" + CRLF )
Fwrite( nLinkHandle, "BHC = $(HMAKEDIR)" + CRLF )
Fwrite( nLinkHandle, "!endif" + CRLF )
Fwrite( nLinkHandle, " " + CRLF )


return nil
