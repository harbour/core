/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * hbmake.Prg Harbour make utility main file
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



#include 'fileio.ch'
#include "common.ch"
#include "radios.ch"
#include "checks.ch"
#ifdef __HARBOUR__
#define EOL hb_osnewline()
#define CRLF hb_osnewline()
#else
#define EOL chr(13)+chr(10)
#define hb_osnewline() chr(13)+chr(10)
#define CRLF hb_osnewline()
#endif
#xtranslate timetosec(<x>) => ((val(substr(<x>,1,2))*3600)+(val(substr(<x>,4,2))*60)+(val(substr(<x>,7,2))))
#define datediff(<x>,<y>) (<x>-<y>)
Static lPrint      := .f.
Static nHandle
Static aDefines    := {}
Static aBuildOrder := {}
Static aCommands   := {}
Static aMacros     := {}
Static aPrgs       := {}
Static aCs         := {}
Static aObjs       := {}
Static lEof        := .F.
Static aRes        := {}
Static nLinkHandle
Static cLinker     := "makefile.@@@"
Static cLinkcomm   := ''
Static nFilePos    := 1
Static aFile       := {}
Static lBcc        := .T.
Static lGcc        := .F.
Static lVcc        := .F.
Static lForce      := .F.
Static szProject:=""

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function main()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
Function main( cFile, p1, p2, p3 )

Local nPos
Local aDef := {}
Local allParam
Default p1 To ""
Default p2 To ""
Default p3 To ""
allParam:=Upper(p1) + Upper(p2) +p3
allparam:=strtran(allparam,"/","-")
If Pcount() == 0
   ?? "Harbour Make Utility"
   ? "Copyright 1999-2000, http://www.harbour-project.org"
   ? ""
   ? "Syntax:  hbmake cFile [options]"
   ? ""
   ? "Options:  /e  Create an New Makefile"
   ? "          /d  Define an macro"
   ? "          /p  Print all command and depencies"
   ? "          /b+ Use BCC as C compiler"
   ? "          /g  Use GCC as C compiler"
   ? "          /v  Use MSVC as C compiler"
   ? "          /f  Force Recompiltion of all files"
   ? "          Note: /p and /d can be used together"
   ? "          Options with + are the default Value"
   Return NIL
Endif
If cFile == NIL
   ? "File not Found"
   Return Nil
Endif
If Pcount() == 2
   if at("-F",allparam)>0
      lforce := .T.
      allparam:=strtran(allparam,"-F","")
   Endif

   if at("-B",allparam)>0
      lBcc := .T.
      lGcc := .F.
      lVcc := .F.
      allparam:=strtran(allparam,"-B","")

   Endif
if at("-G",allparam)>0 
      lBcc := .F.
      lGcc := .T.
      lVcc := .F.
      allparam:=strtran(allparam,"-G","")

   Endif
   if at("-V",allparam)>0

      lBcc := .F.
      lGcc := .F.
      lVcc := .T.
      allparam:=strtran(allparam,"-V","")

   Endif
if at("-E",allparam)>0 

      allparam:=strtran(allparam,"-E","")

      crtmakfile( cFile )
      Return nil
   Endif

    if at("-P",allparam)>0
      lPrint := .t.
      allparam:=strtran(allparam,"-P","")


   Endif
    if at("-D",allparam)>0
      adef := listasarray2( alltrim(Substr( allparam, 3 )), ";" )
      For nPos := 1 To Len( aDef )
         If At( "=", adef[ nPos ] ) > 0
            GetParaDefines( aDef[ nPos ] )
         Endif
      Next
   Endif
Endif
If Pcount() > 2

   if at("-F",allparam)>0
      lforce := .T.
      allparam:=strtran(allparam,"-F","")
   Endif

   if at("-B",allparam)>0
      lBcc := .T.
      lGcc := .F.
      lVcc := .F.
      allparam:=strtran(allparam,"-B","")

   Endif
if at("-G",allparam)>0 
      lBcc := .F.
      lGcc := .T.
      lVcc := .F.
      allparam:=strtran(allparam,"-G","")


   Endif
   if at("-V",allparam)>0 

      lBcc := .F.
      lGcc := .F.
      lVcc := .T.
      allparam:=strtran(allparam,"-V","")


   Endif
if at("-E",allparam)>0
      allparam:=strtran(allparam,"-E","")


      crtmakfile( cFile )
      Return nil
   Endif

    if at("-P",allparam)>0 
      lPrint := .t.
      allparam:=strtran(allparam,"-P","")


   Endif
    if at("-D",allparam)>0 
      adef := listasarray2( alltrim(Substr( allparam, 3 )), ";" )
      For nPos := 1 To Len( aDef )
         If At( "=", adef[ nPos ] ) > 0
            GetParaDefines( aDef[ nPos ] )
         Endif
      Next
   Endif
Endif

parsemakfi( cFile )
If lPrint
   PrintMacros()
Endif
if lForce
compfiles()
else
CompUpdatedfiles()
endif
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
Local cTemp     := ""
Local cTemp1    := ''
Local aTemp     := {}
Local lMacrosec := .f.
Local lBuildSec := .f.
Local lComSec   := .f.
nHandle := FT_FUSE( cFile )
If nHandle < 0
   Return nil
Endif
cBuffer := Trim( Substr( ReadLN( @lEof ), 1 ) )

Aadd( aDefines, { "HMAKEDIR", If( lgcc, GetMakeDir(), GetMakeDir() + "\.." ) } )
If lBcc
   Aadd( aDefines, { "MAKEDIR", GetBccDir() + "\.." } )
Elseif lGcc
   Aadd( aDefines, { "MAKEDIR", GetGccDir() } )
Elseif lVcc
   Aadd( aDefines, { "MAKEDIR", GetVccDir() + "\.." } )

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
     If Alltrim( Left( ctemp, 7 ) ) <> '!ifndef' .and. Alltrim( Left( ctemp, 6 ) ) <> "!endif"

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
        If aTemp[ 1 ] = "OBJFILES"
           aObjs := listasArray2( atemp[ 2 ], " " )
        Endif
        If atemp[ 1 ] = "CFILES"
           aCs := listasArray2( atemp[ 2 ], " " )
        Endif
        If atemp[ 1 ] = "RESFILES"
           aRes := listasArray2( atemp[ 2 ], " " )
        Endif

     Else
        //           cTemp1:=TRIM( SUBSTR( ReadLN( @lEof ),1 ) )
        checkDefine( cTemp )
        //   endif
     Endif
  Endif
  If lbuildSec
     szProject:=cTemp
     aBuildOrder := listasarray2( ctemp, ":" )
     // ? cTemp
     SetBuild()

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

If Len( aCs ) > 0
   For nPos := 1 To Len( aCs )
      If !Empty( acs[ nPos ] )
         cTemp := Strtran( acs[ nPos ], ".c", ".prg" )
         If File( cTemp )
            Aadd( aPrgs, Strtran( acs[ nPos ], ".c", ".prg" ) )
         Endif
      Endif
   Next
Endif
Fclose( nhandle )
Return nil

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function ListAsArray2()
*+
*+    Called from ( bccdir.prg   )   1 - function getbccdir()
*+                ( hbmake.prg   )   2 - function main()
*+                                   5 - function parsemakfi()
*+                                   1 - function getbccdir()
*+                                   1 - function getvccdir()
*+                                   1 - function getgccdir()
*+                                   1 - function checkdefine()
*+                                   1 - function setcommands()
*+                                   1 - function replacemacros()
*+                                   4 - function setbuild()
*+                                   1 - function compfiles()
*+                                   1 - function getparadefines()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
Function ListAsArray2( cList, cDelimiter )

Local nPos
Local aList := {}   // Define an empty array

If cDelimiter = NIL
   cDelimiter := ","
Endif
//
Do While ( nPos := At( cDelimiter, cList ) ) != 0
  Aadd( aList, Alltrim( Substr( cList, 1, nPos - 1 ) ) )    // Add a new element
  cList := Substr( cList, nPos + 1 )
Enddo
Aadd( aList, Alltrim( cList ) )         // Add final element
//
Return aList        // Return the array

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function GetMakeDir()
*+
*+    Called from ( hbmake.prg   )   2 - function parsemakfi()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
Function GetMakeDir()

Local cPath := ""
Local cExe  := HB_ARGV( 0 )

cPath := Left( cexe, Rat( "\", cexe ) - 1 )
Return cPath

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function GetBccDir()
*+
*+    Called from ( hbmake.prg   )   1 - function parsemakfi()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
Function GetBccDir()

Local cPath := ''
Local cEnv  := Gete( "PATH" )
Local aEnv  := listasarray2( cEnv, ";" )
Local nPos


For nPos := 1 To Len( aEnv )
   If File( aenv[ nPos ] + '\bcc32.exe' ) .or. File( Upper( aenv[ nPos ] ) + '\BCC32.EXE' )
      cPath := aenv[ nPos ]

      Exit
   Endif
Next

Return cPath

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function GetVccDir()
*+
*+    Called from ( hbmake.prg   )   1 - function parsemakfi()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
Function GetVccDir()

Local cPath := ''
Local cEnv  := Gete( "PATH" )
Local aEnv  := listasarray2( cEnv, ";" )
Local nPos


For nPos := 1 To Len( aEnv )
   If File( aenv[ nPos ] + '\cl.exe' ) .or. File( Upper( aenv[ nPos ] ) + '\cl.EXE' )
      cPath := aenv[ nPos ]

      Exit
   Endif
Next

Return cPath

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function GetGccDir()
*+
*+    Called from ( hbmake.prg   )   1 - function parsemakfi()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
Function GetGccDir()

Local cPath := ''
Local cEnv  
Local aEnv  
Local nPos
if at("linux",GetEnv("HB_ARCHITECTURE"))>0
    cpath:="/usr/bin"
else
    cEnv  := Gete( "PATH" )
    aEnv  := listasarray2( cEnv, ";" )

    For nPos := 1 To Len( aEnv )
       If File( aenv[ nPos ] + '\gcc.exe' ) .or. File( Upper( aenv[ nPos ] ) + '\GCC.EXE' )
          cPath := aenv[ nPos ]
          Exit
       Endif
    Next
endif
Return cPath

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function ReadLN()
*+
*+    Called from ( hbmake.prg   )   2 - function parsemakfi()
*+                                   1 - function checkdefine()
*+                                   1 - function setcommands()
*+                                   2 - function setbuild()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
Function ReadLN( leof )

Local cBuffer := ""
cBuffer := FT_FREADLN()
cBuffer := Strtran( cBuffer, Chr( 13 ), '' )
cBuffer := Strtran( cBuffer, Chr( 10 ), '' )
FT_FSKIP( 1 )
leof := ft_FEOF()
Return cBuffer

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function checkDefine()
*+
*+    Called from ( hbmake.prg   )   1 - function parsemakfi()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
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
aSet  := listasarray2( ctemp, "=" )
nPos  := Ascan( adefines, { | x, y | x[ 1 ] == aset[ 1 ] } )
If nPos = 0
   cRead    := Alltrim( Strtran( aset[ 2 ], "$(", "" ) )
   cRead    := Strtran( cRead, ")\..", "" )
   nMakePos := Ascan( aDefines, { | x, y | x[ 1 ] == cRead } )
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
nPos := Ascan( aMacros, { | x, y | "$(" + Alltrim( x[ 1 ] ) + ")" == cMacro } )
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
// ? "setting link file"
cRead  := Alltrim( readln( @leof ) )
     szProject:=cRead
amacro := listasarray2( cRead, ":" )
If Len( amacro ) > 1
   aTemp := listasarray2( amacro[ 2 ], " " )
   For nPos := 1 To Len( aTemp )
      Aadd( aBuildOrder, atemp[ nPos ] )
   Next

Endif
Aadd( aBuildOrder, amacro[ 1 ] )
cRead := Strtran( cRead, "@&&!", "" )

amacro := listasarray2( cRead, '\' )

For nPos := 1 To Len( amacro )
   If At( "$", amacro[ nPos ] ) > 0
      findmacro( amacro[ nPos ], @cRead )
   Endif
Next
cLinkcomm   := cRead + "  @" + cLinker
nLinkHandle := Fcreate( clinker )
//#define CRLF hb_osnewline()
For nPos := 1 To 7
   cRead  := Alltrim( readln( @leof ) )
   amacro := listasarray2( cRead, " " )
   For ncount := 1 To Len( amacro )
      If At( "$", amacro[ nCount ] ) > 0
         findmacro( amacro[ nCount ], @cRead )
         If At( ".exe", cRead ) > 0 .and. lGcc
            Fwrite( nLinkhandle, "-o" + cRead + CRLF )
         Else
            Fwrite( nLinkhandle, cRead + CRLF )
         Endif
      Endif
   Next
Next
Fclose( nLinkhandle )

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
Local aOrder := listasarray2( aBuildOrder[ 2 ], " " )

For nCount := 1 To Len( aOrder )
   If aOrder[ nCount ] == "$(CFILES)"
      nPos := Ascan( aCommands, { | x, y | x[ 1 ] == ".prg.c:" } )
      If nPos > 0
         cComm := aCommands[ nPos, 2 ]
         cOld  := cComm
      Endif
      For nFiles := 1 To Len( aPrgs )
         nPos := Ascan( aCs, { | x | Left( x, At( ".", x ) ) == Left( aPrgs[ nFiles ], At( ".", aPrgs[ nFiles ] ) ) } )
         If nPos > 0
            cComm := Strtran( cComm, "o$*", "o" + aCs[ nPos ] )
            cComm := Strtran( cComm, "$**", aPrgs[ nFiles ] )
            outstd( " ")
            ! ( cComm )
            cComm := cold
         Endif
      Next
   Endif
   If aOrder[ nCount ] == "$(OBJFILES)"
      If lGcc
         nPos := Ascan( aCommands, { | x, y | x[ 1 ] == ".c.o:" } )
      Else
         nPos := Ascan( aCommands, { | x, y | x[ 1 ] == ".c.obj:" } )
      Endif
      If nPos > 0
         cComm := aCommands[ nPos, 2 ]
         cOld  := ccomm
      Endif
      For nFiles := 1 To Len( aCs )
         nPos := Ascan( aObjs, { | x | Left( x, At( ".", x ) ) == Left( acs[ nFiles ], At( ".", acs[ nFiles ] ) ) } )
         If nPos > 0
            cComm := Strtran( cComm, "o$*", "o" + aObjs[ nPos ] )
            cComm := Strtran( cComm, "$**", acs[ nFiles ] )
            outstd( " ")
            // ? cComm
            ! ( cComm )
            ccomm := cold
         Endif
      Next
   Endif
   If aOrder[ nCount ] == "$(RESDEPEN)"
      nPos := Ascan( aCommands, { | x, y | x[ 1 ] == ".rc.res:" } )
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

Local cDef
Local nPos
Local cRead
Local aSet     := {}
Local nMakePos

aSet := listasarray2( ctemp, "=" )
nPos := Ascan( adefines, { | x, y | x[ 1 ] == aset[ 1 ] } )
If nPos = 0
   cRead    := Alltrim( Strtran( aset[ 2 ], "$(", "" ) )
   cRead    := Strtran( cRead, ")\..", "" )
   nMakePos := Ascan( aDefines, { | x, y | x[ 1 ] == cRead } )
   If nMakePos = 0
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
Local cRead := ""
Outstd( "HBMAKE Version ", Version(), "CopyRight (c) 2000 The Harbour Project" + CRLF )
Outstd( "" + CRLF )
Outstd( "Macros:" + CRLF )
For nPos := 1 To Len( aMacros )
   Outstd( "     " + aMacros[ nPos, 1 ] + " = " + aMacros[ nPos, 2 ] + CRLF )
Next
Outstd( "Implicit Rules:" + CRLF )
For nPos := 1 To Len( aCommands )
   Outstd( "     " + aCommands[ nPos, 1 ] + hb_osnewline() + "        " + aCommands[ nPos, 2 ] + CRLF )
Next
Outstd( "" + CRLF )
Outstd( "Targets:" )
Outstd( "    " + szProject + ":" + CRLF )
Outstd( "        " + "Flags :" + CRLF )
Outstd( "        " + "Dependents :" )
For nPos := 1 To Len( aCs )
   Outstd( acs[ nPos ] + " ")
Next
For nPos := 1 To Len( aobjs )
   Outstd( aobjs[ nPos ]  + " ")
Next
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
Local aSrc         := Directory( "*.prg" )
Local nLenaSrc     := Len( asrc )
Local nLenaOut
Local lFwh         := .f.
Local lCw         := .f.
Local lRddAds      := .f.
Local cOs          := "Win32"
Local cCompiler    := "BCC"
Local cfwhpath     := space(40)
Local ccwpath     :=  space(40)
Local cmainfile    := ""
Local cRddAds      := ""
Local lAutomemvar  := .f.
Local lvarismemvar := .f.
Local ldebug       := .f.
Local lSupressline := .f.
Local cGrap        := "NONE"
Local cDefHarOpts  := ""
Local cDefcOpts    := ""
Local cDefLinkOpts := ""
Local lCompMod     := .f.
Local x
Local lGenppo      := .f.
Local getlist      := {}
Local cTopFile     := ""
Local cDefBccLibs  := "lang.lib vm.lib rtl.lib rdd.lib macro.lib pp.lib dbfntx.lib dbfcdx.lib common.lib gtwin.lib"
Local cDefGccLibs  := "-lvm -lrtl -lgtdos -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon"
Local cscreen      := Savescreen( 0, 0, Maxrow(), Maxcol() )
local citem:=""
nLinkHandle := Fcreate( cFile )
Fwrite( nLinkHandle, "#BCC" + CRLF )
Fwrite( nLinkHandle, "VERSION=BCB01" + CRLF )
Fwrite( nLinkHandle, "!ifndef BCB" + CRLF )
Fwrite( nLinkHandle, "BCB = $(MAKEDIR)\.." + CRLF )
Fwrite( nLinkHandle, "!endif" + CRLF )
Fwrite( nLinkHandle, "!ifndef BHC" + CRLF )
Fwrite( nLinkHandle, "BHC = $(HMAKEDIR)\.." + CRLF )
Fwrite( nLinkHandle, "!endif" + CRLF )
Fwrite( nLinkHandle, " " + CRLF )
Cls
Setcolor( 'w/b+,w/b,w+/b,w/b+,w/b,w+/b' )
@  0,  0, Maxrow(), Maxcol() Box( Chr( 201 ) + Chr( 205 ) + Chr( 187 ) + Chr( 186 ) + Chr( 188 ) + Chr( 205 ) + Chr( 200 ) + Chr( 186 ) + Space( 1 ) )
ATTENTION( "Enviroment options", 0 )
@  1,  1 Say "Select Os"                                      
@  1, 12 Get cos radio { "Win32", "OS/2", "Linux" }
@  1, 23 Say "Select C Compiler"                              
@  1, 40 Get cCompiler radio { "BCC", "MSVC", "GCC" }  
@  1, 48 Say "Graphic Library"                                
@  1, 64 Get lFwh checkbox "Use FWH" when Cos=="Win32"                          
@  2, 64 Get lcw checkbox "Use C4W"          when Cos=="Win32" 
@  3, 64 Get lRddads checkbox "Use RddAds"   when Cos=="Win32"  
Read

If lFwh
   @  4,  1 Say "FWH path" Get cfwhpath        
Elseif lCw
   @  4,  1 Say "C4H path" Get ccwpath        
Endif
ATTENTION( "Harbour Options", 5 )

@  6,  1 Get lautomemvar checkbox "Automatic memvar declaration"                  
@  6, 43 Get lvarismemvar checkbox "Variables are assumed M->"                    
@  7,  1 Get lDebug checkbox "Debug info"                                         
@  7, 43 Get lSupressline checkbox "Suppress line number information"             
@  8,  1 Get lGenppo checkbox "Generate pre-processed output"         
@  8, 43 Get lCompMod checkbox "compile module only"                              
Read
lBcc := If( At( "BCC", cCompiler ) > 0, .t., .f. )
lVcc := If( At( "MSVC", cCompiler ) > 0, .t., .f. )
lGcc := If( At( "GCC", cCompiler ) > 0, .t., .f. )
if lAutomemvar
cDefHarOpts+="-a "
endif
if lvarismemvar
cDefHarOpts+="-v "
endif
if ldebug
cDefHarOpts+="-b "
endif
if lSupressline
cDefHarOpts+="-l "
endif
if lGenppo
cDefHarOpts+="-p "
endif
if lCompmod
cDefHarOpts+="-m "
endif


If lBcc
   Aadd( aCommands, { ".cpp.obj:", "$(BCB)\BIN\bcc32 $(CFLAG1) $(CFLAG2) -o$* $*" } )

   Aadd( aCommands, { ".c.obj:", "$(BCB)\BIN\bcc32 -I$(BHC)\include $(CFLAG1) $(CFLAG2) -o$* $**" } )

   Aadd( aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)\include $(HARBOURFLAGS) -I$(FWH)\include -o$* $**" } )

   Aadd( aCommands, { ".rc.res:", "$(BCB)\BIN\brcc32 $(RFLAGS) $<" } )

Elseif lGcc
   if at("linux",Getenv("HB_ARCHITECTURE"))>0 .or. cOs=="Linux"
   Aadd( aCommands, { ".cpp.o:", "$(BCB)/gcc $(CFLAG1) $(CFLAG2) -o$* $*" } )

   Aadd( aCommands, { ".c.o:", "$(BCB)/gcc -I$(HB_INC_INSTALL) $(CFLAG1) $(CFLAG2) -I. -o$* $**" } )

   Aadd( aCommands, { ".prg.c:", "$(BHC)/harbour -n -I$(HB_INC_INSTALL) -I.  -o$* $**" } )
else
   Aadd( aCommands, { ".cpp.o:", "$(BCB)\gcc $(CFLAG1) $(CFLAG2) -o$* $*" } )

   Aadd( aCommands, { ".c.o:", "$(BCB)\gcc -I$(BHC)/../include $(CFLAG1) $(CFLAG2) -I. -o$* $**" } )

   Aadd( aCommands, { ".prg.c:", "$(BHC)\harbour -n -I$(BHC)/../include $(HARBOURFLAGS)  -o$* $**" } )

endif

Elseif lVcc
   Aadd( aCommands, { ".cpp.obj:", "$(BCB)\bin\cl $(CFLAG1) $(CFLAG2) -Fo$* $*" } )

   Aadd( aCommands, { ".c.obj:", "$(BCB)\bin\cl -I$(BHC)\include $(CFLAG1) $(CFLAG2) -Fo$* $**" } )

   Aadd( aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)\include $(HARBOURFLAGS) -I$(C4W)\include -o$* $**" } )

   Aadd( aCommands, { ".rc.res:", "$(BCB)\BIN\rc $(RFLAGS) $<" } )
Endif

attention( 'Spacebar to select, Enter to continue process', 22 )

Asize( aIn, nLenaSrc )
For x := 1 To nLenaSrc
   aIn[ x ] := Pad( aSrc[ x, 1 ], 13 ) + ;
                    Str( aSrc[ x, 2 ], 8 ) + '  ' + ;
                    Dtoc( aSrc[ x, 3 ] ) + '  ' + ;
                    aSrc[ x, 4 ]
Next

aOut := Aclone( aIn )

pickarry( 10, 15, 19, 64, aIn, aOut )

nLenaOut := Len( aOut )

For x := 1 To nLenaOut
   aOut[ x ] := lower(Trim( Left( aOut[ x ], 12 ) ))
Next

aOut := Asort( aOut )

If Len( aOut ) == 1
   cTopFile := aOut[ 1 ]
Else
   attention( 'Select the TOP MODULE of your executable', 22 )
   cTopFile := pickfile( "*.prg" )
Endif

x:=ascan(aOut,{|x| lower(x)==lower(cTopFile)})
if x>0
    adel(aout,x)
    asize(aout,len(aout)-1)
endif
aCs   := aclone(aout)

For x := 1 To Len( aCs )
       cItem:= aCs[ x ]
       aCs[ x ]:=strtran( cItem, ".prg", ".c" )
Next
aObjs := aClone(aout)
For x := 1 To Len( aObjs )
      cItem:=aObjs[ x ]
   If !lGcc
      aObjs[ x ]:=strtran( cItem, ".prg", ".obj" )
   Else
      aObjs[ x ]:=strtran( cItem, ".prg", ".o" )
   Endif
Next



If lFwh
   Fwrite( nLinkHandle, "FWH = " + cfwhpath + CRLF )
Elseif lCw

   Fwrite( nLinkHandle, "C4W =" + ccwpath + CRLF )

Endif
if lGcc
   if at("linux",Getenv("HB_ARCHITECTURE"))>0 .or.  cOs=="Linux"
        Fwrite( nLinkHandle, "PROJECT = " + if(isupper(cTopfile),Strtran( cTopfile, ".PRG", "" ),Strtran( cTopfile, ".prg", "" )) + CRLF )
   else

        Fwrite( nLinkHandle, "PROJECT = " + if(isupper(cTopfile),Strtran( cTopfile, ".PRG", ".EXE" ),Strtran( cTopfile, ".prg", ".exe" )) + CRLF )
   endif     
else

Fwrite( nLinkHandle, "PROJECT = " + if(isupper(cTopfile),Strtran( cTopfile, ".PRG", ".EXE" ),Strtran( cTopfile, ".prg", ".exe" )) + CRLF )
endif
if len(aObjs)<2

Fwrite( nLinkHandle, "OBJFILES =  " + if(isupper(cTopfile),Strtran( cTopfile, ".PRG", ".OBJ" ),Strtran( cTopfile, ".prg", ".obj" )) + CRLF )
else

Fwrite( nLinkHandle, "OBJFILES = " + if(isupper(cTopfile),Strtran( cTopfile, ".PRG", ".OBJ" ),Strtran( cTopfile, ".prg", ".obj" )))

For x := 1 To Len( aobjs )
   If x <> Len( aobjs ) .and. aObjs[x]<>cTopfile

      Fwrite( nLinkHandle, " " + aobjs[ x ] )
   Else
      Fwrite( nLinkHandle, " " + aobjs[ x ] + CRLF )
   Endif
Next
endif
if len(aCs)<2

Fwrite( nLinkHandle, "CFILES = " + if(isupper(cTopfile),Strtran( cTopfile, ".PRG", ".C" ),Strtran( cTopfile, ".prg", ".c" ))+CRLF)
else

Fwrite( nLinkHandle, "CFILES = " + if(isupper(cTopfile),Strtran( cTopfile, ".PRG", ".C" ),Strtran( cTopfile, ".prg", ".c" )))
For x := 1 To Len( acs )
   If x <> Len( acs ) .and. aCs[x]<>cTopfile
      Fwrite( nLinkHandle, " " + aCs[ x ] )
   Else
      Fwrite( nLinkHandle, " " + aCs[ x ] + CRLF )
   Endif
Next
endif

Fwrite( nLinkHandle, "RESFILES = " + CRLF )
Fwrite( nLinkHandle, "RESDEPEN = $(RESFILES)" + CRLF )
if lRddads
    cDefBccLibs+=" rddads.lib ace2.lib"
endif
if lBcc .or. lVcc
    If lFwh 
        Fwrite( nLinkHandle, "LIBFILES = $(FWH)\lib\fiveh.lib $(FWH)\lib\fivec.lib " + cDefBccLibs + CRLF )
   elseif lCw
        Fwrite( nLinkHandle, "LIBFILES = $(C4W)\c4wclass.lib $(C4W)\wbrowset.lib $(C4W)\otabt.lib $(C4W)\clip4win.lib" + cDefBccLibs + CRLF )
   else
        Fwrite( nLinkHandle, "LIBFILES = " +cDefBccLibs + CRLF )
   endif
elseif lGcc
        Fwrite( nLinkHandle, "LIBFILES = " +cDefgccLibs + CRLF )
endif
 Fwrite( nLinkHandle, "DEFFILE = "+CRLF)
 fWrite( nLinkHandle, "HARBOURFLAGS =" +cDefHarOpts+CRLF)
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
 Fwrite( nLinkHandle, "CFLAG1 = "+if(at("linux",Getenv("HB_ARCHITECTURE"))>0 ,"-I$(HB_INC_INSTALL)"," -I$(BHC)/../include")+ " -c -Wall"+CRLF)
 Fwrite( nLinkHandle, "CFLAG2 = "+if(at("linux",Getenv("HB_ARCHITECTURE"))>0 ,"-L$(HB_LIB_INSTALL)"," -L$(BHC)/../lib")+CRLF)
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

For x:=1 to len(aCommands)
    if lBcc
        Fwrite( nLinkHandle, aCommands[x,1]+CRLF)
        Fwrite( nLinkHandle, aCommands[x,2]+CRLF)
        Fwrite( nLinkHandle, " "+CRLF)
    elseif lVcc
        Fwrite( nLinkHandle, aCommands[x,1]+CRLF)
        Fwrite( nLinkHandle, aCommands[x,2]+CRLF)
        Fwrite( nLinkHandle, " "+CRLF)
    elseif lGcc
        Fwrite( nLinkHandle, aCommands[x,1]+CRLF)
        Fwrite( nLinkHandle, aCommands[x,2]+CRLF)
        Fwrite( nLinkHandle, " "+CRLF)
    endif
next
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
        Fwrite( nLinkHandle, "    $(BCB)\$(LINKER) @&&!"+CRLF)
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
Procedure ATTENTION( CSTRING, NLINENUM, CCOLOR )

Local COLDCOLOR

Default NLINENUM To 24
Default CCOLOR To 'GR+/R'

COLDCOLOR := Setcolor( CCOLOR )

CSTRING := '  ' + Alltrim( CSTRING ) + '  '

Devpos( NLINENUM, c( CSTRING ) )

Devout( CSTRING )

Setcolor( COLDCOLOR )

Return

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function c()
*+
*+    Called from ( hbmake.prg   )   1 - procedure attention()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
Function c( CSTRING )

Return Max( ( Maxcol() / 2 ) - Int( Len( CSTRING ) / 2 ), 0 )

*+ EOF: HBMAKE.PRG

Function CompUpdatedfiles()

Local cComm
Local cOld
Local nPos
Local nCount
Local nFiles
Local aCtocompile:={}
Local aOrder := listasarray2( aBuildOrder[ 2 ], " " )

For nCount := 1 To Len( aOrder )
   If aOrder[ nCount ] == "$(CFILES)"
      nPos := Ascan( aCommands, { | x, y | x[ 1 ] == ".prg.c:" } )
      If nPos > 0
         cComm := aCommands[ nPos, 2 ]
         cOld  := cComm
      Endif
      For nFiles := 1 To Len( aPrgs )
         nPos := Ascan( aCs, { | x | Left( x, At( ".", x ) ) == Left( aPrgs[ nFiles ], At( ".", aPrgs[ nFiles ] ) ) } )
         if  fileisnewer(aprgs[nPos])
            If nPos > 0
               aadd(aCtocompile,acs[nPos])
               cComm := Strtran( cComm, "o$*", "o" + aCs[ nPos ] )
               cComm := Strtran( cComm, "$**", aPrgs[ nFiles ] )
               outstd( " ")
               ! ( cComm )
               cComm := cold
            Endif
            endif
      Next
   Endif
   If aOrder[ nCount ] == "$(OBJFILES)"
      If lGcc
         nPos := Ascan( aCommands, { | x, y | x[ 1 ] == ".c.o:" } )
      Else
         nPos := Ascan( aCommands, { | x, y | x[ 1 ] == ".c.obj:" } )
      Endif
      If nPos > 0
         cComm := aCommands[ nPos, 2 ]
         cOld  := ccomm
      Endif
      For nFiles := 1 To Len( aCtocompile )
         nPos := Ascan( aObjs, { | x | Left( x, At( ".", x ) ) == Left( aCtocompile[ nFiles ], At( ".", aCtocompile[ nFiles ] ) ) } )
         If nPos > 0
            cComm := Strtran( cComm, "o$*", "o" + aObjs[ nPos ] )
            cComm := Strtran( cComm, "$**", aCtocompile[ nFiles ] )
            outstd( " ")
            // ? cComm
            ! ( cComm )
            ccomm := cold
         Endif
      Next
   Endif
   If aOrder[ nCount ] == "$(RESDEPEN)"
      nPos := Ascan( aCommands, { | x, y | x[ 1 ] == ".rc.res:" } )
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

function fileisnewer(cFile)
local aFile
local aobj
local lReturn:=.f.
local nDate,nTime
if !file(strtran(cFile,".prg",".c"))
   return .t.
endif
aFile:=directory(cFile)
aObj:=if(lGcc,directory(strtran(cFile,".prg",".o")),directory(strtran(cFile,".prg",".obj")))
   nDate:=datediff(afile[1,3],aobj[1,3])
   if nDate>0
      lReturn:=.t.
   else
      nTime:=timetosec(afile[1,4])-timetosec(aobj[1,4])
      if nTime>0
          lreturn:=.t.
      else
          lReturn:=.f.
      endif
   endif
return lReturn


