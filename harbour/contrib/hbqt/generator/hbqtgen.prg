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
   LOCAL cParam, cPathOut, cPathIn, cPrjFile, cProFile, cPathDoc
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

      CASE left( cParam,2 ) == '-D'
         cPathDoc := substr( cParam,3 )

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
      cPathIn  := DiskName() +':'+ s_PathSep + CurDir()
   ENDIF
   IF empty( cPathDoc )
      cPathDoc := DiskName() +':'+ s_PathSep + CurDir()
   ENDIF

   /* Manage Project File */
   FOR EACH cProFile IN aPrjFiles
      ManageProject( cProFile, cPathIn, cPathOut, cPathDoc )
   NEXT

   /* Generate .CPP Sources */
   FOR EACH cProFile IN aProFiles
      GenSource( cProFile, cPathIn, cPathOut, cPathDoc )
   NEXT

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION ManageProject( cProFile, cPathIn, cPathOut, cPathDoc )
   LOCAL cFile, cPath, cExt, cPrj, cTkn, cVal
   LOCAL cPIn, cPOut, cPDoc
   LOCAL n, nn
   LOCAL prj_

   hb_fNameSplit( cProFile, @cPath, @cFile, @cExt )

   IF empty( cPath )
      cFile := cPathIn + s_PathSep + cProFile
   ELSE
      cFile := cProFile
   ENDIF
   IF !file( cFile )
      RETURN nil
   ENDIF

   cPIn  := cPathIn
   cPOut := cPathOut
   cPDoc := cPathDoc

   DispProgress( cFile )

   cPrj  := memoread( cFile )

   /* Pullout all ANSI C style comments */
   DO WHILE .t.
      IF ( n := at( '/*', cPrj ) ) == 0
         EXIT
      ENDIF
      /* We must have a matching pair */
      nn := at( '*/', cPrj )
      IF nn == 0
         ? 'Project file has unbalanced comment section...'
         RETURN nil
      ENDIF
      cPrj := substr( cPrj,1,n-1 ) + substr( cPrj,nn+2 )
   ENDDO

   /* Prepare to be parsed properly */
   cPrj := strtran( cPrj, s_NewLine, _EOL )
   cPrj := strtran( cPrj, chr( 13 )+chr( 10 ), _EOL )
   cPrj := strtran( cPrj, chr( 13 ), _EOL )

   prj_:= hb_ATokens( cPrj, _EOL )

   FOR EACH cPrj IN prj_
      cPrj := alltrim( cPrj )

      IF left( cPrj,1 ) $ ':#;/*'
         LOOP
      ENDIF

      IF ( n := at( '=', cPrj ) ) > 0
         cTkn := alltrim( substr( cPrj,1,n-1 ) )
         cVal := alltrim( substr( cPrj,n+1   ) )

         IF !empty( cVal )
            DO CASE
            CASE cTkn == '-I'
               cPIn := cVal

            CASE cTkn == '-O'
               cPOut := cVal

            CASE cTkn == '-D'
               cPDoc := cVal

            ENDCASE
         ENDIF

      ELSEIF lower( right( cPrj,4 ) ) == '.qth'
         GenSource( cPrj, cPIn, cPOut, cPDoc )

      ENDIF
   NEXT

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION GenSource( cProFile, cPathIn, cPathOut, cPathDoc )
   LOCAL cFile, cWidget, cExt, cPath, cOrg, cCode, cHBFunc, lSupported
   LOCAL cPHP, cARGs, cPre, cPost, cFunc, cRet, cArg, ss, cQth, cAr, cName, cNames
   LOCAL s, j, n, n1, hHandle, nFuncs, nCnvrtd, cRetName, lOk
   LOCAL a_, b_, txt_, enum_, code_, x_, func_, dummy_, types_, cpp_, hdr_, ftr_, cmntd_, doc_
   LOCAL nam_, dcc_

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

   DispProgress( cFile )

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
      IF ( 'enum ' $ s .or. 'flags ' $ s )
         b_:= hb_ATokens( alltrim( s ),' ' )
         aadd( enum_, b_[ 2 ] )
         a_[ n ] := ''
      ENDIF
   NEXT

   types_  := { 'void', 'int', 'bool', 'quint32', 'double', 'QString', 'QIcon', 'qreal', ;
                                                              'QRect', 'QSize', 'QPoint' }
   dummy_  := {}
   func_   := { { "", 0 } }
   txt_    := {}
   cpp_    := {}
   hdr_    := {}
   ftr_    := {}
   cmntd_  := {}
   doc_    := {}
   nFuncs  := 0
   nCnvrtd := 0
   cName   := ''
   cNames  := ''

   /* Body */
   FOR EACH s IN a_
      cOrg := s

      /* Normalize */
      s := strtran( s, ' (', '(' )
      s := strtran( s, ' *', '*' )
      s := strtran( s, 'virtual ', '' )
      s := alltrim( s )

      IF left( s,2 ) == '//'
         aadd( cmntd_, cOrg )
         LOOP
      ENDIF
      IF empty( s ) .or. left( s,1 ) == '#' .or. ( 'virtual' $ s )
         LOOP
      ENDIF

      IF ( n := at( '(', s ) ) > 0
         nFuncs++

         n1 := rat( ')', s )
         IF n+1 == n1
            cARGs := ''
         ELSE
            cARGs := alltrim( substr( s, n+1, n1-n-2 ) )
         ENDIF
         cPre  := alltrim( substr( s, 1, n-1 ) )
         cPost := alltrim( substr( s, n1+2 ) )

         /* Normalize cPre */
         cPre := strtran( cPre, 'const ', '' )

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
            IF ( '::' $ cRet ) .or. ;
                     ascan( types_, cRet ) > 0 .or. ;
                           ascan( enum_, cRet ) > 0 .or. ;
                                 ( ( '*' $ cRet ) .and. !( '<' $ cRet ) )
               lSupported := .t.
               cArg := ''
               cNames := ''
               IF !empty( cARGs )
                  b_:= hb_ATokens( cARGs, ',' )
                  IF !empty( b_ )
                     FOR j := 1 TO len( b_ )
                        ss := alltrim( b_[ j ] )
                        ss := strtran( ss, 'const ', '' )
                        ss := strtran( ss, '& ', '' )
                        ss := strtran( ss, '&', '' )
                        ss := alltrim( ss )

                        nam_:= hb_ATokens( ss, ' ' )

                        IF len( nam_ ) > 1
                           cAr   := nam_[ 1 ]
                           cName := nam_[ 2 ]
                        ELSE
                           cAr   := nam_[ 1 ]
                           cName := nam_[ 1 ]
                        ENDIF

                        /* If argument type is supported by the engine */
                        DO CASE
                        CASE ( '::' $ cAr )

                        CASE ascan( enum_, cAr ) > 0

                        CASE '<' $ cAr
                           lSupported := .f.

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

                        cArg   += cAr + ','
                        cName  := upper( left( cName,1 ) ) + substr( cName,2 )
                        cNames += cName + ','
                     NEXT
                     cArg := substr( cArg, 1, len( cArg )-1 )
                     cNames := substr( cNames, 1, len( cNames )-1 )
                  ENDIF
               ENDIF

               IF lSupported
                  IF ( n := ascan( func_, {|e_| e_[ 1 ] == cFunc } ) ) > 0
                     func_[ n,2 ]++
                     cHBFunc := cFunc + '_' + hb_ntos( func_[ n,2 ] )
                  ELSE
                     cHBFunc := cFunc
                     aadd( func_, { cFunc, 0 } )
                  ENDIF

                  lOk := BuildFunction( @txt_, cWidget, cOrg, cFunc, cArg, cRet, enum_, types_, ;
                                                                         cHBFunc, @doc_, cNames )
                  IF !lOk
                     aadd( dummy_, cOrg )
                  ELSE
                     nCnvrtd++
                  ENDIF
               ELSE
                  /* Build an array of protos not converted to functions */
                  aadd( dummy_, cOrg )
               ENDIF
            ELSE
               aadd( dummy_, cOrg )
            ENDIF
         ENDIF
      ENDIF
   NEXT

   /* Assemble components */
   IF .t.  /* !empty( txt_ ) */

      /* Pull .cpp copyright text */
      BuildHeader( @hdr_ )
      aeval( hdr_, {|e| aadd( cpp_, e ) } )
      aadd( cpp_, '' )

      /* Insert information about prototypes not converted to functions */
      IF !empty( dummy_ )
         aadd( cpp_, '/*' )
         aadd( cpp_, ' *  Constructed[ '+ hb_ntos( nCnvrtd ) +'/'+ hb_ntos( nFuncs ) +' [ '+ hb_ntos( nCnvrtd/nFuncs*100 ) +'% ] ]' )
         aadd( cpp_, ' *  ' )
         aadd( cpp_, ' *  *** Unconvered Prototypes ***' )
         aadd( cpp_, ' *  -----------------------------' )
         aadd( cpp_, ' *  ' )
         aeval( dummy_, {|e| aadd( cpp_, ' *  ' + e ) } )
         IF !empty( cmntd_ )
            aadd( cpp_, ' *  ' )
            aadd( cpp_, ' *  ' + '*** Commented out protos which construct fine but do not compile ***' )
            aadd( cpp_, ' *  ' )
            aeval( cmntd_, {|e| aadd( cpp_, ' *  ' + e ) } )
         ENDIF
         aadd( cpp_, ' */ ' )
         aadd( cpp_, '' )
      ENDIF

      /* Needs to be changed manually */
      #if 0
      aadd( cpp_, "#include <QtGui/"+ cWidget +">" )
      aadd( cpp_, "" )
      #endif

      /* Insert user defined code */
      IF !empty( code_ )
         aeval( code_, {|e| aadd( cpp_, strtran( e, chr( 13 ), '' ) ) } )
         aadd( cpp_, "" )
      ENDIF

      /* Insert Functions */
      aeval( txt_, {|e| aadd( cpp_, strtran( e, chr( 13 ), '' ) ) } )

      /* Footer */
      BuildFooter( @ftr_ )
      aeval( ftr_, {|e| aadd( cpp_, e ) } )

      /* And create .cpp source */
      hHandle := fcreate( cPathOut + s_PathSep + 'hbqt_'+ lower( cWidget ) +'.cpp' )
      IF hHandle != -1
         aeval( cpp_, { |e| fWrite( hHandle, e + s_NewLine, len( e ) + len( s_NewLine ) ) } )

         fClose( hHandle )
      ENDIF

      IF !empty( doc_ )
         BuildDocument( cWidget, doc_, cPathDoc )
      ENDIF
   ENDIF

   RETURN cPHP

/*----------------------------------------------------------------------*/

STATIC FUNCTION BuildFunction( txt_, cWidget, cProtoType, cFunc, cArgs, cRet, enum_, ;
                                                     types_, cHBFunc, doc_, cParNames )
   LOCAL cParPtr  := "hbqt_par_" + cWidget + "( 1 )"
   LOCAL pars     := ""
   LOCAL docs     := ""
   LOCAL cDocFunc := ""
   LOCAL aArgs, cArg, n, nn, pp, cTxt, aNames

   IF !empty( cArgs )
      aArgs := hb_ATokens( cArgs, ','  )
      aNames := hb_ATokens( cParNames, ',' )

      n := 0
      FOR EACH cArg IN aArgs
         n++

         DO CASE
         CASE ( nn := ascan( enum_, cArg ) ) > 0
            pars += '( '+ cWidget +'::'+ enum_[ nn ] +' ) hb_parni( '+ hb_ntos( n ) +' )'
            docs += 'n'+cArg

         CASE cArg == 'int'
            pars += 'hb_parni( '+ hb_ntos( n ) +' )'
            docs += 'n'+aNames[ n ]

         CASE cArg == 'quint32'
            pars += 'hb_parnint( '+ hb_ntos( n ) +' )'
            docs += 'n'+aNames[ n ]

         CASE cArg == 'double' .or. cArg == 'qreal'
            pars += 'hb_parnd( '+ hb_ntos( n ) +' )'
            docs += 'n'+aNames[ n ]

         CASE cArg == 'bool'
            pars += 'hb_parl( '+ hb_ntos( n ) +' )'
            docs += 'l'+aNames[ n ]

         CASE cArg == 'QString'
            pars += 'hbqt_par_QString( '+ hb_ntos( n ) +' )'
            docs += 'c'+IF( empty( aNames[ n ] ), 'Str', aNames[ n ] )

         CASE cArg == 'QIcon'
            pars += 'QIcon( hbqt_par_QString( '+ hb_ntos( n ) +' ) )'
            docs += 'c'+IF( empty( aNames[ n ] ), 'IconName', aNames[ n ] )

         CASE ( '::' $ cArg )
            pars += "( "+ cArg +" ) hb_parni( "+ hb_ntos( n ) +' )'
            docs += 'n'+strtran( aNames[ n ], '::', '_' )

         CASE ( '*' $ cArg )
            pp := rtrim( cArg )
            pp := rtrim( substr( pp, 1, at( "*", pp ) - 1 ) )

            pars += "hbqt_par_" + pp + "( " + hb_ntos( n ) + " )"
            docs += 'p'+strtran( aNames[ n ], '*', '' )

         CASE cArg == cWidget
            pars += "hbqt_par_" + cWidget + "( " + hb_ntos( n ) + " )"
            docs += 'p'+cWidget

         CASE cArg == 'QRect'
            pars += "hbqt_const_QRect( " + hb_ntos( n ) + " )"
            docs += 'aRect'

         CASE cArg == 'QSize'
            pars += "hbqt_const_QSize( " + hb_ntos( n ) + " )"
            docs += 'aSize'

         CASE cArg == 'QPoint'
            pars += "hbqt_const_QPoint( " + hb_ntos( n ) + " )"
            docs += 'aPoint'

         ENDCASE

         pars += ', '
         docs += ', '
      NEXT

      pars := alltrim( pars )
      pars := substr( pars, 1, len( pars )-1 )

      docs := alltrim( docs )
      docs := substr( docs, 1, len( docs )-1 )

   ENDIF

   DO CASE
   CASE ( nn := ascan( enum_, cRet ) ) > 0
      cTxt := "   hb_retni( "+ cParPtr +"->"+ cFunc +"( " + pars +" ) );"
      cDocFunc := 'n' + enum_[ nn ]

   CASE cRet == "void"
      cTxt := "   "+ cParPtr +"->"+ cFunc +"( " + pars +" );"
      cDocFunc := 'NIL'

   CASE cRet == "bool"
      cTxt := "   hb_retl( "+ cParPtr +"->"+ cFunc +"( "+ pars +" ) );"
      cDocFunc := 'lValue'

   CASE cRet == "int"
      cTxt := "   hb_retni( "+ cParPtr +"->"+ cFunc +"( "+ pars +" ) );"
      cDocFunc := 'nValue'

   CASE cRet == 'quint32'
      cTxt := "   hb_retnint( "+ cParPtr +"->"+ cFunc +"( "+ pars +" ) );"
      cDocFunc := 'nValue'

   CASE cRet == 'double' .or. cRet == 'qreal'
      cTxt := "   hb_retnd( "+ cParPtr +"->"+ cFunc +"( "+ pars +" ) );"
      cDocFunc := 'nValue'

   CASE cRet == "QString"
      cTxt := "   hb_retc( "+ cParPtr +"->"+ cFunc +"( "+ pars +").toLatin1().data() );"
      cDocFunc := 'cValue'

   CASE ( "*" $ cRet )
      cTxt := "   hb_retptr( ( " + cRet +" ) "+ cParPtr +"->"+ cFunc +"( " + pars +" ) );"
      cDocFunc := 'p' + strtran( cRet, '*', '' )

   CASE ( "::" $ cRet )
      cTxt := "   hb_retni( "+ cParPtr +"->"+ cFunc +"( "+ pars +" ) );"
      cDocFunc := 'n' + strtran( cRet, '::', '_' )

   CASE cRet == 'QRect'
      cTxt := "   hbqt_ret_QRect( "+ cParPtr +"->"+ cFunc +"( " + pars +" ) );"
      cDocFunc := 'aRect'

   CASE cRet == 'QSize'
      cTxt := "   hbqt_ret_QSize( "+ cParPtr +"->"+ cFunc +"( " + pars +" ) );"
      cDocFunc := 'aSize'

   CASE cRet == 'QPoint'
      cTxt := "   hbqt_ret_QPoint( "+ cParPtr +"->"+ cFunc +"( " + pars +" ) );"
      cDocFunc := 'aPoint'

   OTHERWISE
      cTxt := ''
      cDocFunc := ''

   ENDCASE

   /* Again check if we have something to insert */
   IF !empty( cTxt )
      aadd( txt_, "/*" )
      aadd( txt_, " * "+ strtran( cProtoType, chr(13), '' ) )
      aadd( txt_, " */" )

      aadd( txt_, "HB_FUNC( QT_" + upper( cWidget ) +"_"+ upper( cHBFunc ) +" )" )
      aadd( txt_, "{"  )

      aadd( txt_, cTxt )

      aadd( txt_, "}"  )
      aadd( txt_, ""   )

      aadd( doc_, 'Qt_'+ cWidget + '_' + cHBFunc +'( p'+ cWidget + ;
                                 IF( empty( docs ), '', ', '+ docs ) +' ) -> '+ cDocFunc )
      aadd( doc_, '' )
   ENDIF

   RETURN !empty( cTxt )

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
   aadd( txt_, "/*----------------------------------------------------------------------*/"    )
   aadd( txt_, ""                                                                              )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION BuildFooter( txt_ )

   aadd( txt_, ""                                                                             )
   aadd( txt_, "/*----------------------------------------------------------------------*/"   )
   aadd( txt_, "#endif             /* #if QT_VERSION >= 0x040500 */"                          )
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
   ? '   -O<OutputPath>   [ e.g. c:\harbour\contrib\hbqt ]        [D] Current folder'
   ? '   -I<InputPath>    [ e.g. c:\harbour\contrib\hbqt\protos ] [D] Current folder'
   ? '   -d<DocFilesPath> [ e.g. c:\harbour\contrib\hbqt\doc    ] [D] Current folder'
   ? ' '
   ? '   -c<compile>      If QT env is set, attempts to compile resulting .cpp'
   ?
   ? 'Press a key ...'
   ?
   inkey( 0 )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION BuildDocument( cWidget, doc_, cPathDoc )
   LOCAL dcc_:={}
   LOCAL hHandle, cFile

   aadd( dcc_, ' ' )
   aadd( dcc_, '<      HBQtGen v1.0 - Harbour Callable Wrappers Generator for QT v4.5      >' )
   aadd( dcc_, ' ' )
   aadd( dcc_, '< Please do not modify this document as it is subject to change in future. >' )
   aadd( dcc_, ' ' )
   aadd( dcc_, '<                    Pritpal Bedi <pritpal@vouchcac.com>                   >' )
   aadd( dcc_, ' ' )
   aadd( dcc_, '<                            '+dtoc( date() ) + ' - ' + time() + '                           >' )
   aadd( dcc_, ' ' )
   aadd( dcc_, ' ' )

   aeval( doc_, {|e| aadd( dcc_, e ) } )

   cFile := cPathDoc + s_PathSep + cWidget +'.txt'

   hHandle := fcreate( cFile )
   IF hHandle != -1
      aeval( dcc_, { |e| fWrite( hHandle, e + s_NewLine, len( e ) + len( s_NewLine ) ) } )
      fClose( hHandle )
   ENDIF

   RETURN file( cFile )

/*----------------------------------------------------------------------*/

STATIC FUNCTION DispProgress( cFile )

   ? cFile

   RETURN nil

/*----------------------------------------------------------------------*/
