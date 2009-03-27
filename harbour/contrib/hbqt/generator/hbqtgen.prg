/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT Source Generator for Harbour
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
/*----------------------------------------------------------------------*/

#include 'fileio.ch'

#define _EOL   chr( 10 )

STATIC s_NewLine
STATIC s_PathSep

/*----------------------------------------------------------------------*/

FUNCTION Main( ... )
   LOCAL aParam, cLParam
   LOCAL cParam, cPathOut, cPathIn, cPrjFile, cProFile
   LOCAL x, cPath, cFile, cExt
   LOCAL aPrjFiles := {}
   LOCAL aProFiles := {}
   LOCAL lCompile  := .f.

   IF PCount() == 0
      DispHelp()
      RETURN nil
   ENDIF

   s_NewLine := hb_OsNewLine()
   s_PathSep := hb_OsPathSeparator()

   aParam := hb_AParams()

   FOR EACH cParam IN aParam
      cLParam := lower( cParam )

      DO CASE
      CASE left( cParam,1 ) == '@'
         x := substr( cParam,2 )
         hb_fNameSplit( x, @cPath, @cFile, @cExt )
         IF empty( cExt ) .or. lower( cExt ) != 'qtp'
            cExt := 'qtp'
         ENDIF
         x := cPath + s_PathSep + cFile + '.' + cExt
         aadd( aPrjFiles, x )

      CASE right( cLParam,4 ) == '.qtp'
         aadd( aPrjFiles, cParam )

      CASE right( cLParam,4 ) == '.qth'
         aadd( aProFiles, cParam )

      CASE left( cParam,2 ) == '-O'
         cPathOut := substr( cParam,3 )

      CASE left( cParam,2 ) == '-I'
         cPathIn := substr( cParam,3 )

      CASE cParam == '-c'
         lCompile := .t.

      CASE cLParam == '-help'
         DispHelp()
         RETURN nil

      ENDCASE
   NEXT

   IF empty( aPrjFiles ) .and. empty( aProFiles )
      DispHelp()
      RETURN nil
   ENDIF

   /* Please fix for Linux */
   IF empty( cPathOut )
      cPathOut := DiskName() +':'+ s_PathSep + CurDir()
   ENDIF
   IF empty( cPathIn )
      cPathIn := DiskName() +':'+ s_PathSep + CurDir()
   ENDIF

   /* Generate .CPP Sources */
   FOR EACH cProFile IN aProFiles
      GenSource( cProFile, cPathIn, cPathOut )
   NEXT

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION GenSource( cProFile, cPathIn, cPathOut )
   LOCAL cFile, cWidget, cExt, cPath, cOrg, cCode
   LOCAL cPHP, cARGs, cPre, cPost, cFunc, cRet, cArg, ss, cQth, cAr
   LOCAL s, j, n, n1, hHandle
   LOCAL a_, b_, txt_, enum_, code_, x_
   LOCAL types_:= { 'void', 'int', 'bool', 'quint32', 'double', 'QString', 'QIcon', 'qreal' }
   LOCAL lSupported

   hb_fNameSplit( cProFile, @cPath, @cWidget, @cExt )

   IF empty( cPath )
      cFile := cPathIn + s_PathSep + cProFile
   ELSE
      cFile := cProFile
   ENDIF
   IF !file( cFile )
      RETURN nil
   ENDIF

   cQth := memoread( cFile )
   IF empty( cQth )
      RETURN nil
   ENDIF

   /* Prepare to be parsed properly */
   cQth := strtran( cQth, s_NewLine, _EOL )
   cQth := strtran( cQth, chr( 13 )+chr( 10 ), _EOL )
   cQth := strtran( cQth, chr( 13 ), _EOL )

   /* Pull out Code Section */
   IF( n := at( '<CODE>', cQth ) ) > 0
      IF ( n1 := at( '</CODE>', cQth ) ) == 0
         RETURN nil
      ENDIF
      cCode := substr( cQth, n+6, n1-n-1-6 )
      cQth  := substr( cQth,1,n-1 ) + substr( cQth, n1+7 )
   ENDIF
   IF !empty( cCode )
      code_:= hb_ATokens( cCode, _EOL )
   ENDIF

   a_:= hb_ATokens( cQth, _EOL )

   enum_:={}
   n := 0
   FOR EACH s IN a_
      n++
      IF ( 'enum ' $ s )
         b_:= hb_ATokens( alltrim( s ),' ' )
         aadd( enum_, b_[ 2 ] )
         a_[ n ] := ''
      ENDIF
   NEXT

   txt_:={}
   BuildHeader( @txt_ )

   /* Needs to be changed manually */
   aadd( txt_, "#include <QtGui/"+ cWidget +">" )
   aadd( txt_, "" )
   aadd( txt_, "" )

   /* Insert user defined code */
   IF !empty( code_ )
      aeval( code_, {|e| aadd( txt_, strtran( e, chr( 13 ), '' ) ) } )
      aadd( txt_, "" )
   ENDIF

   /* Body */
   FOR EACH s IN a_
      cOrg := s

      /* Normalize */
      s := strtran( s, ' (', '(' )
      s := strtran( s, ' *', '*' )
      s := alltrim( s )

      IF empty( s ) .or. left( s,1 ) == '#' .or. left( s,2 ) == '//' .or. ( 'virtual' $ s )
         LOOP
      ENDIF

      IF( n := at( '(', s ) ) > 0
         n1 := rat( ')', s )
         IF n+1 == n1
            cARGs := ''
         ELSE
            cARGs := alltrim( substr( s, n+1, n1-n-2 ) )
         ENDIF
         cPre  := alltrim( substr( s, 1, n-1 ) )
         cPost := alltrim( substr( s, n1+2 ) )

         IF ( n := rat( ' ', cPre ) ) > 0
            cFunc := alltrim( substr( cPre, n+1 ) )
            cRet  := alltrim( substr( cPre, 1, n-1 ) )
         ELSE
            cFunc := alltrim( cPre )
            cRet  := ''
         ENDIF

         /* There must be a return type */
         IF !empty( cRet )
            /* If return type is supported by auto engine */
            IF ( '::' $ cRet ) .or. ascan( types_, cRet ) > 0 .or. ;
                                 ( ( '*' $ cRet ) .and. !( '<' $ cRet ) )
               lSupported := .t.
               cArg := ''
               IF !empty( cARGs )
                  b_:= hb_ATokens( cARGs, ',' )
                  IF !empty( b_ )
                     FOR j := 1 TO len( b_ )
                        ss := alltrim( b_[ j ] )
                        ss := strtran( ss, 'const ', '' )
                        IF ( n := at( ' ', ss ) ) > 0
                           cAr := substr( ss, 1, n-1 )
                        ELSE
                           cAr := ss
                        ENDIF

                        /* If argument type is supported by the engine */
                        DO CASE
                        CASE '<' $ cAr
                           lSupported := .f.

                        CASE ( '::' $ cAr )

                        CASE ( '*'  $ ss )
                           IF ascan( types_, strtran( cAr,'*','' ) ) > 0
                              lSupported := .f.
                           ENDIF

                        CASE ascan( types_, cAr ) == 0
                           lSupported := .f.

                        ENDCASE

                        IF !lSupported
                           EXIT
                        ENDIF
                        cArg += cAr + ','
                     NEXT
                     cArg := substr( cArg, 1, len( cArg )-1 )
                  ENDIF
               ENDIF

               IF lSupported
                  BuildFunction( @txt_, cWidget, cOrg, cFunc, cArg, cRet, enum_, types_ )
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   NEXT

   /* Footer */
   BuildFooter( @txt_ )

   IF !empty( txt_ )
      hHandle := fcreate( cPathOut + s_PathSep + 'hbqt_'+ lower( cWidget ) +'.cpp' )
      IF hHandle != -1
         aeval( txt_, { |e| fWrite( hHandle, e + s_NewLine, len( e ) + len( s_NewLine ) ) } )

         fClose( hHandle )
      ENDIF
   ENDIF

   RETURN cPHP

/*----------------------------------------------------------------------*/

STATIC FUNCTION BuildFunction( txt_, cWidget, cProtoType, cFunc, cArgs, cRet, enum_ )
   LOCAL cParPtr := "hbqt_par_" + cWidget + "( 1 )"
   LOCAL pars    := ""
   LOCAL aArgs, cArg, n, nn, pp, cTxt

   IF !empty( cArgs )
      aArgs := hb_ATokens( cArgs, ',' )

      n := 0
      FOR EACH cArg IN aArgs
         n++

         DO CASE
         CASE ( nn := ascan( enum_, cArg ) ) > 0
            pars += '( '+ cWidget +'::'+ enum_[ nn ] +' ) hb_parni( '+ hb_ntos( n ) +' )'

         CASE cArg == 'int'
            pars += 'hb_parni( '+ hb_ntos( n ) +' )'

         CASE cArg == 'quint32'
            pars += 'hb_parnint( '+ hb_ntos( n ) +' )'

         CASE cArg == 'double' .or. cArg == 'qreal'
            pars += 'hb_parnd( '+ hb_ntos( n ) +' )'

         CASE cArg == 'bool'
            pars += 'hb_parl( '+ hb_ntos( n ) +' )'

         CASE cArg == 'QString'
            pars += 'hbqt_par_QString( '+ hb_ntos( n ) +' )'

         CASE cArg == 'QIcon'
            pars += 'QIcon( hbqt_par_QString( '+ hb_ntos( n ) +' ) )'

         CASE ( '::' $ cArg )
            pars += "( "+ cArg +" ) hb_parni( "+ hb_ntos( n ) +' )'

         CASE ( '*' $ cArg )
            pp := rtrim( cArg )
            pp := rtrim( substr( pp, 1, at( "*", pp ) - 1 ) )

            pars += "hbqt_par_" + pp + "( " + hb_ntos( n ) + " )"

         CASE cArg == cWidget
            pars += "hbqt_par_" + cWidget + "( " + hb_ntos( n ) + " )"

         ENDCASE

         pars += ', '
      NEXT

      pars := alltrim( pars )
      pars := substr( pars, 1, len( pars )-1 )

   ENDIF

   DO CASE
   CASE ( nn := ascan( enum_, cRet ) ) > 0
      cTxt := "   hb_retni( "+ cParPtr +"->"+ cFunc +"( " + pars +" ) );"

   CASE cRet == "void"
      cTxt := "   "+ cParPtr +"->"+ cFunc +"( " + pars +" );"

   CASE cRet == "bool"
      cTxt := "   hb_retl( "+ cParPtr +"->"+ cFunc +"( "+ pars +" ) );"

   CASE cRet == "int"
      cTxt := "   hb_retni( "+ cParPtr +"->"+ cFunc +"( "+ pars +" ) );"

   CASE cRet == 'quint32'
      cTxt := "   hb_retnint( "+ cParPtr +"->"+ cFunc +"( "+ pars +" ) );"

   CASE cRet == 'double' .or. cRet == 'qreal'
      cTxt := "   hb_retnd( "+ cParPtr +"->"+ cFunc +"( "+ pars +" ) );"

   CASE cRet == "QString"
      cTxt := "   hb_retc( "+ cParPtr +"->"+ cFunc +"( "+ pars +").toLatin1().data() );"

   CASE ( "*" $ cRet )
      cTxt := "   hb_retptr( ( " + cRet +" ) "+ cParPtr +"->"+ cFunc +"( " + pars +" ) );"

   CASE ( "::" $ cRet )
      cTxt := "   hb_retni( "+ cParPtr +"->"+ cFunc +"( "+ pars +" ) );"

   OTHERWISE
      cTxt := ''

   ENDCASE

   /* Again check if we have something to insert */
   IF !empty( cTxt )
      aadd( txt_, "/*" )
      aadd( txt_, strtran( cProtoType, chr(13), '' ) )
      aadd( txt_, "*/" )

      aadd( txt_, "HB_FUNC( QT_" + upper( cWidget ) +"_"+ upper( cFunc ) +" )" )
      aadd( txt_, "{"  )

      aadd( txt_, cTxt )

      aadd( txt_, "}"  )
      aadd( txt_, ""   )
   ENDIF

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION BuildHeader( txt_ )

   aadd( txt_, "/*"                                                                            )
   aadd( txt_, " * $Id$"        )
   aadd( txt_, " */"                                                                           )
   aadd( txt_, "   "                                                                           )
   aadd( txt_, "/* "                                                                           )
   aadd( txt_, " * Harbour Project source code:"                                               )
   aadd( txt_, " * QT wrapper main header"                                                     )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>"     )
   aadd( txt_, " * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>"                         )
   aadd( txt_, " * www - http://www.harbour-project.org"                                       )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * This program is free software; you can redistribute it and/or modify"       )
   aadd( txt_, " * it under the terms of the GNU General Public License as published by"       )
   aadd( txt_, " * the Free Software Foundation; either version 2, or (at your option)"        )
   aadd( txt_, " * any later version."                                                         )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * This program is distributed in the hope that it will be useful,"            )
   aadd( txt_, " * but WITHOUT ANY WARRANTY; without even the implied warranty of"             )
   aadd( txt_, " * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"              )
   aadd( txt_, " * GNU General Public License for more details."                               )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * You should have received a copy of the GNU General Public License"          )
   aadd( txt_, " * along with this software; see the file COPYING.  If not, write to"          )
   aadd( txt_, " * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,"            )
   aadd( txt_, " * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/)."     )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * As a special exception, the Harbour Project gives permission for"           )
   aadd( txt_, " * additional uses of the text contained in its release of Harbour."           )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * The exception is that, if you link the Harbour libraries with other"        )
   aadd( txt_, " * files to produce an executable, this does not by itself cause the"          )
   aadd( txt_, " * resulting executable to be covered by the GNU General Public License."      )
   aadd( txt_, " * Your use of that executable is in no way restricted on account of"          )
   aadd( txt_, " * linking the Harbour library code into it."                                  )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * This exception does not however invalidate any other reasons why"           )
   aadd( txt_, " * the executable file might be covered by the GNU General Public License."    )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * This exception applies only to the code released by the Harbour"            )
   aadd( txt_, " * Project under the name Harbour.  If you copy code from other"               )
   aadd( txt_, " * Harbour Project or Free Software Foundation releases into a copy of"        )
   aadd( txt_, " * Harbour, as the General Public License permits, the exception does"         )
   aadd( txt_, " * not apply to the code that you add in this way.  To avoid misleading"       )
   aadd( txt_, " * anyone as to the status of such modified files, you must delete"            )
   aadd( txt_, " * this exception notice from them."                                           )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * If you write modifications of your own for Harbour, it is your choice"      )
   aadd( txt_, " * whether to permit this exception to apply to your modifications."           )
   aadd( txt_, " * If you do not wish that, delete this exception notice."                     )
   aadd( txt_, " *"                                                                            )
   aadd( txt_, " */"                                                                           )
   aadd( txt_, "/*----------------------------------------------------------------------*/"    )
   aadd( txt_, ""                                                                              )
   aadd( txt_, '#include "hbapi.h"'                                                            )
   aadd( txt_, '#include "hbqt.h"'                                                             )
   aadd( txt_, ""                                                                              )
   aadd( txt_, "/*----------------------------------------------------------------------*/"    )
   aadd( txt_, "#if QT_VERSION >= 0x040500"                                                    )
   aadd( txt_, ""                                                                              )
   aadd( txt_, ""                                                                              )
   aadd( txt_, ""                                                                              )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION BuildFooter( txt_ )

   aadd( txt_, ""                                                                             )
   aadd( txt_, ""                                                                             )
   aadd( txt_, "#endif"                                                                       )
   aadd( txt_, "/*----------------------------------------------------------------------*/"   )
   aadd( txt_, ""                                                                             )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION DispHelp()

   ?
   ?
   ? 'SYNTAX:'
   ? '   hbqtgen.exe [Options] [[@]<QtProjectFile.qtp>] [<QtHeaderFile.qth, ...>]'
   ?
   ? 'Options:'
   ? '   -O<OutputPath> [ e.g. c:\harbour\contrib\hbqt ]        [D] Current folder'
   ? '   -I<InputPath>  [ e.g. c:\harbour\contrib\hbqt\protos ] [D] Current folder'
   ? '   -c<compile>    If QT env is set, attempts to compile resulting .cpp'
   ?
   ? 'Press a key ...'
   ?
   inkey( 0 )

   RETURN nil

/*----------------------------------------------------------------------*/

