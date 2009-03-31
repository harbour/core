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

   DispLogo()

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

   ?
   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION ManageProject( cProFile, cPathIn, cPathOut, cPathDoc )
   LOCAL cFile, cPath, cExt, cPrj, cTkn, cVal
   LOCAL cPIn, cPOut, cPDoc
   LOCAL n, nn
   LOCAL prj_, cpp_, prg_, a_

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

   cpp_:={}
   prg_:={}

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
         a_:= GenSource( cPrj, cPIn, cPOut, cPDoc )
         IF !empty( a_[ 1 ] )
            aadd( cpp_, a_[ 1 ] )
            IF !empty( a_[ 2 ] )
               aadd( prg_, a_[ 2 ] )
            ENDIF
         ENDIF

      ENDIF
   NEXT

   Build_HBQT_H( cPOut )
   Build_HBQT_SLOTS_H( cPOut )
   Build_HBQT_BASE_CPP( cPOut )
   Build_HBQT_UTILS_CPP( cPOut )
   Build_HBQT_SLOTS_CPP( cPOut )
   Build_MOC_SLOTS_CPP( cPOut )
   IF !empty( cpp_ )
      Build_Makefile( cpp_, prg_, cPOut )
   ENDIF
   Build_Demo( cPOut )

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION GenSource( cProFile, cPathIn, cPathOut, cPathDoc )
   LOCAL cFile, cWidget, cExt, cPath, cOrg, cCode, cHBFunc, lSupported, cCPP, cPRG
   LOCAL cPHP, cARGs, cPre, cPost, cFunc, cRet, cArg, ss, cQth, cAr, cName, cNames, cClass
   LOCAL s, j, n, n1, hHandle, nFuncs, nCnvrtd, cRetName, lOk
   LOCAL a_, b_, txt_, enum_, code_, x_, func_, dummy_, types_, cpp_, hdr_, ftr_, cmntd_, doc_
   LOCAL nam_, dcc_, class_, cls_

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

   /* Pull out Class Section */
   IF( n := at( '<CLASS>', cQth ) ) > 0
      IF ( n1 := at( '</CLASS>', cQth ) ) == 0
         RETURN nil
      ENDIF
      cClass := substr( cQth, n+6, n1-n-1-6 )
      cQth   := substr( cQth,1,n-1 ) + substr( cQth, n1+7 )
   ENDIF
   cls_:={}
   IF !empty( cClass )
      class_:= hb_ATokens( cClass, _EOL )
      /* Parse Ingredients */
      FOR EACH s IN class_
         IF ( n := at( '=', s ) ) > 0
            aadd( cls_, { alltrim( substr( s, 1, n-1 ) ), alltrim( substr( s, n+1 ) ) } )
         ENDIF
      NEXT
   ENDIF

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

                  lOk := Build_Function( @txt_, cWidget, cOrg, cFunc, cArg, cRet, enum_, types_, ;
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
      BuildHeader( @hdr_, 0 )
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

      /* Build Document File */
      IF !empty( doc_ )
         BuildDocument( cWidget, doc_, cPathDoc )
      ENDIF

      /* Build Class PRG Source */
      IF !empty( cls_ )
         Build_Class( cWidget, cls_, doc_, cPathOut, hdr_ )
         cPRG := cWidget
      ELSE
         cPRG := ''
      ENDIF
      cCPP := cWidget
   ENDIF

   RETURN { cCPP, cPRG }

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_Function( txt_, cWidget, cProtoType, cFunc, cArgs, cRet, enum_, ;
                                                     types_, cHBFunc, doc_, cParNames )
   LOCAL cParPtr  := "hbqt_par_" + cWidget + "( 1 )"
   LOCAL pars     := ""
   LOCAL docs     := ""
   LOCAL cDocFunc := ""
   LOCAL aArgs, cArg, n, nn, pp, cTxt, aNames
   LOCAL nParSz   := 0
   LOCAL nParRc   := 0
   LOCAL nPar

   IF !empty( cArgs )
      aArgs := hb_ATokens( cArgs, ','  )
      aNames := hb_ATokens( cParNames, ',' )

      n := 1
      FOR EACH cArg IN aArgs
         n++

         DO CASE
         CASE ( nn := ascan( enum_, cArg ) ) > 0
            pars += '( '+ cWidget +'::'+ enum_[ nn ] +' ) hb_parni( '+ hb_ntos( n ) +' )'
            docs += 'n'+cArg

         CASE cArg == 'int'
            pars += 'hb_parni( '+ hb_ntos( n ) +' )'
            docs += 'n'+aNames[ n-1 ]

         CASE cArg == 'quint32'
            pars += 'hb_parnint( '+ hb_ntos( n ) +' )'
            docs += 'n'+aNames[ n-1 ]

         CASE cArg == 'double' .or. cArg == 'qreal'
            pars += 'hb_parnd( '+ hb_ntos( n ) +' )'
            docs += 'n'+aNames[ n-1 ]

         CASE cArg == 'bool'
            pars += 'hb_parl( '+ hb_ntos( n ) +' )'
            docs += 'l'+aNames[ n-1 ]

         CASE cArg == 'QString'
            pars += 'hbqt_par_QString( '+ hb_ntos( n ) +' )'
            docs += 'c'+IF( empty( aNames[ n-1 ] ), 'Str', aNames[ n-1 ] )

         CASE cArg == 'QIcon'
            pars += 'QIcon( hbqt_par_QString( '+ hb_ntos( n ) +' ) )'
            docs += 'c'+IF( empty( aNames[ n-1 ] ), 'IconName', aNames[ n-1 ] )

         CASE ( '::' $ cArg )
            pars += "( "+ cArg +" ) hb_parni( "+ hb_ntos( n ) +' )'
            docs += 'n'+strtran( aNames[ n-1 ], '::', '_' )

         CASE ( '*' $ cArg )
            pp := rtrim( cArg )
            pp := rtrim( substr( pp, 1, at( "*", pp ) - 1 ) )

            pars += "hbqt_par_" + pp + "( " + hb_ntos( n ) + " )"
            docs += 'p'+strtran( aNames[ n-1 ], '*', '' )

         CASE cArg == cWidget
            pars += "hbqt_par_" + cWidget + "( " + hb_ntos( n ) + " )"
            docs += 'p'+cWidget

         CASE cArg == 'QRect'
            pars += "hbqt_const_QRect( " + hb_ntos( n ) + " )"
            docs += 'aRect'+aNames[ n-1 ] //'aRect'

         CASE cArg == 'QSize'
            pars += "hbqt_const_QSize( " + hb_ntos( n ) + " )"
            docs += 'aSize'+aNames[ n-1 ] //'aSize'

         CASE cArg == 'QPoint'
            pars += "hbqt_const_QPoint( " + hb_ntos( n ) + " )"
            docs += 'aPoint'+aNames[ n-1 ] //'aPoint'

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

STATIC FUNCTION DispProgress( cFile )

   ? cFile

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION BuildHeader( txt_, nMode )

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
   IF nMode == 0
   aadd( txt_, '#include "hbapi.h"'                                                            )
   aadd( txt_, '#include "hbqt.h"'                                                             )
   aadd( txt_, ""                                                                              )
   aadd( txt_, "/*----------------------------------------------------------------------*/"    )
   aadd( txt_, "#if QT_VERSION >= 0x040500"                                                    )
   aadd( txt_, "/*----------------------------------------------------------------------*/"    )
   aadd( txt_, ""                                                                              )
   ELSEIF nMode == 1
   aadd( txt_, ""                                                                              )
   aadd( txt_, '#include "hbclass.ch"'                                                         )
   aadd( txt_, ""                                                                              )
   ELSEIF nMode == 2
   /* Pure Header */
   ENDIF

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
   ? 'SYNTAX:'
   ? '   hbqtgen.exe [Options] [[@]<QtProjectFile.qtp>] [<QtHeaderFile.qth, ...>]'
   ?
   ? 'Options:'
   ? '   -O<OutputPath>   [ e.g. c:\harbour\contrib\hbqt ]        [D] Current folder'
   ? '   -I<InputPath>    [ e.g. c:\harbour\contrib\hbqt\protos ] [D] Current folder'
   ? '   -D<DocFilesPath> [ e.g. c:\harbour\contrib\hbqt\doc    ] [D] Current folder'
   ? ' '
   ? '   -c<compile>      If QT env is set, attempts to compile resulting .cpp'
   ?
   inkey( 0 )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION DispLogo()

   OutStd( hb_osNewLine() + "Harbour Source Gennerator for QT " + HBRawVersion() + hb_osNewLine() +;
           "Copyright (c) 2009, Pritpal Bedi <pritpal@vouchcac.com>" + hb_osNewLine() +;
           "http://www.harbour-project.org/" + hb_osNewLine() +;
           hb_osNewLine() )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour ", "" )

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

STATIC FUNCTION CreateTarget( cFile, txt_ )
   LOCAL hHandle := fcreate( cFile )

   /* Truncate entries */
   aeval( txt_, {|e,i| txt_[ i ] := trim( e ) } )

   IF hHandle != -1
      aeval( txt_, { |e| fWrite( hHandle, e + s_NewLine, len( e ) + len( s_NewLine ) ) } )
      fClose( hHandle )
   ENDIF

   RETURN file( cFile )

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_Class( cWidget, cls_, doc_, cPathOut )
   LOCAL cFile := cPathOut + s_PathSep + 'T'+cWidget +'.prg'
   LOCAL s, n, cMtd, cRet, cM, ss, cCall, sm
   LOCAL nLen := len( cWidget )
   LOCAL txt_  :={}
   LOCAL hdr_ :={}

   BuildHeader( @txt_, 1 )

   aadd( txt_, '' )

   n := ascan( cls_, {|e_| lower( e_[ 1 ] ) == 'inherit' } )
   s := 'CREATE CLASS '+ cWidget + IF( n > 0, ' INHERIT ' + cls_[ n,2 ], '' )

   aadd( txt_, s  )
   aadd( txt_, '' )
   aadd( txt_, '   VAR     pPtr'  )
   aadd( txt_, '' )
   aadd( txt_, '   METHOD  New()' )
   aadd( txt_, '' )

   /* Populate METHODS */
   FOR EACH s IN doc_
      n := at( '-> ', s )
      IF n > 0
         cRet  := substr( s, n+3 )
         s     := substr( s, 1, n-1 )

         n     := at( cWidget, s )
         sm    := substr( s, n+nLen+1 )

         ss    := 'p'+cWidget+','
         cM    := strtran( sm, ss, '' )
         ss    := 'p'+cWidget
         cM    := strtran( cM, ss, '' )
         cM    := strtran( cM, '(  )', '()' )
         cM    := strtran( cM, '(  ', '( ' )
         cM    := IF( len( cM ) > 35, cM, pad( cM,35 ) )

         ss    := 'p'+cWidget
         cCall := strtran( s, ss, '::pPtr' )

         aadd( txt_, '   METHOD  ' + cM + ' INLINE  ' + cCall )
      ENDIF
   NEXT
   aadd( txt_, '' )
   aadd( txt_, '   ENDCLASS' )
   aadd( txt_, '' )
   aadd( txt_, '/*----------------------------------------------------------------------*/'   )
   aadd( txt_, ''                                                                             )

   n  := ascan( cls_, {|e_| lower( e_[ 1 ] ) == 'new' } )
   cM := 'New( pParent )'

   aadd( txt_, 'METHOD '+ cM + ' CLASS '+ cWidget )
   aadd( txt_, '' )
   aadd( txt_, '   ::pPtr := Qt_'+ cWidget +'( pParent )' )
   aadd( txt_, '' )
   aadd( txt_, '   RETURN Self' )
   aadd( txt_, '' )
   aadd( txt_, '/*----------------------------------------------------------------------*/'   )
   aadd( txt_, ''                                                                             )

   RETURN CreateTarget( cFile, txt_ )

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_MakeFile( cpp_, prg_, cPathOut )
   LOCAL cFile := cPathOut + s_PathSep + 'Makefile'
   LOCAL txt_:={}
   LOCAL s

   aadd( txt_, "#                                                       " )
   aadd( txt_, "# $Id$   " )
   aadd( txt_, "#                                                       " )
   aadd( txt_, "                                                        " )
   aadd( txt_, "ROOT = ../../                                           " )
   aadd( txt_, "                                                        " )
   aadd( txt_, "LIBNAME=hbqt                                            " )
   aadd( txt_, "                                                        " )
   aadd( txt_, "ifeq ($(HB_WITH_QT),)                                   " )
   aadd( txt_, "HB_WITH_QT=yes                                          " )
   aadd( txt_, "ifeq ($(HB_ARCHITECTURE),dos)                           " )
   aadd( txt_, "HB_WITH_QT=no                                           " )
   aadd( txt_, "endif                                                   " )
   aadd( txt_, "ifeq ($(HB_ARCHITECTURE),os2)                           " )
   aadd( txt_, "HB_WITH_QT=no                                           " )
   aadd( txt_, "endif                                                   " )
   aadd( txt_, "ifeq ($(HB_COMPILER),mingw64)                           " )
   aadd( txt_, "HB_WITH_QT=no                                           " )
   aadd( txt_, "endif                                                   " )
   aadd( txt_, "ifeq ($(HB_COMPILER),owatcom)                           " )
   aadd( txt_, "HB_WITH_QT=no                                           " )
   aadd( txt_, "endif                                                   " )
   aadd( txt_, "ifeq ($(HB_COMPILER),bcc)                               " )
   aadd( txt_, "HB_WITH_QT=no                                           " )
   aadd( txt_, "endif                                                   " )
   aadd( txt_, "ifeq ($(HB_COMPILER),pocc)                              " )
   aadd( txt_, "HB_WITH_QT=no                                           " )
   aadd( txt_, "endif                                                   " )
   aadd( txt_, "ifeq ($(HB_COMPILER),pocc64)                            " )
   aadd( txt_, "HB_WITH_QT=no                                           " )
   aadd( txt_, "endif                                                   " )
   aadd( txt_, "ifeq ($(HB_COMPILER),poccce)                            " )
   aadd( txt_, "HB_WITH_QT=no                                           " )
   aadd( txt_, "endif                                                   " )
   aadd( txt_, "endif                                                   " )
   aadd( txt_, "                                                        " )
   aadd( txt_, "ifeq ($(HB_WITH_QT),yes)                                " )
   aadd( txt_, "                                                        " )
   aadd( txt_, "ifeq ($(HB_INC_QT),)                                    " )
   aadd( txt_, "ifeq ($(HB_XBUILD),)                                    " )
   aadd( txt_, "HB_INC_QT = /usr/include/qt4 /Developer/qt/include      " )
   aadd( txt_, "endif                                                   " )
   aadd( txt_, "endif                                                   " )
   aadd( txt_, "                                                        " )
   aadd( txt_, "HB_INC_QT_OK += $(foreach d, $(HB_INC_QT), $(if $(wildcard $(d)/Qt/qglobal.h),$(d),))" )
   aadd( txt_, "                                                        " )
   aadd( txt_, "ifneq ($(strip $(HB_INC_QT_OK)),)                       " )
   aadd( txt_, "                                                        " )
   aadd( txt_, "HB_USER_CFLAGS += $(foreach d, $(HB_INC_QT_OK), -I$(d)) " )
   aadd( txt_, "                                                        " )
   aadd( txt_, "CPP_SOURCES=\                                           " )

   /* Insert .cpp sources */
   aadd( cpp_, 'base'  )
   aadd( cpp_, 'utils' )
   aadd( cpp_, 'slots' )
   FOR EACH s IN cpp_
      aadd( txt_, chr( 9 ) + 'hbqt_' + lower( s ) + '.cpp \' )
   NEXT
   aadd( txt_, chr( 9 ) + 'moc_slots.cpp \' )

   aadd( txt_, "                                                        " )
   aadd( txt_, "                                                        " )
   aadd( txt_, "C_HEADERS=\                                             " )
   aadd( txt_, chr( 9 )+"hbqt.h \                                       " )
   aadd( txt_, chr( 9 )+"hbqt_slots.h \                                 " )
   aadd( txt_, "                                                        " )
   aadd( txt_, "#PRG_HEADERS=\                                          " )
   aadd( txt_, "#   hbqt.ch \                                           " )
   aadd( txt_, "#   hbqtextern.ch \                                     " )
   aadd( txt_, "                                                        " )
   IF !empty( prg_ )
      aadd( txt_, "PRG_SOURCES=\                                        " )
      FOR EACH s IN prg_
         aadd( txt_, chr( 9 ) + 'T' + s + '.prg \' )
      NEXT
   ENDIF
   aadd( txt_, "                                                        " )
   aadd( txt_, "include $(TOP)$(ROOT)config/header.cf                   " )
   aadd( txt_, "INSTALL_RULE_HEADERS := $(INSTALL_RULE)                 " )
   aadd( txt_, "include $(TOP)$(ROOT)config/lib.cf                      " )
   aadd( txt_, "                                                        " )
   aadd( txt_, "install::                                               " )
   aadd( txt_, chr( 9 )+"$(INSTALL_RULE_HEADERS)                        " )
   aadd( txt_, "                                                        " )
   aadd( txt_, "else                                                    " )
   aadd( txt_, "include $(TOP)$(ROOT)config/none.cf                     " )
   aadd( txt_, "endif                                                   " )
   aadd( txt_, "else                                                    " )
   aadd( txt_, "include $(TOP)$(ROOT)config/none.cf                     " )
   aadd( txt_, "endif                                                   " )
   aadd( txt_, "                                                        " )
   #if 0
   aadd( txt_, "else                                                    " )
   aadd( txt_, "include $(TOP)$(ROOT)config/none.cf                     " )
   aadd( txt_, "endif                                                   " )
   aadd( txt_, "else                                                    " )
   aadd( txt_, "include $(TOP)$(ROOT)config/none.cf                     " )
   aadd( txt_, "endif                                                   " )
   aadd( txt_, "                                                        " )
   #endif

   RETURN CreateTarget( cFile, txt_ )

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_HBQT_H( cPathOut )
   LOCAL cFile := cPathOut + s_PathSep + 'hbqt.h'
   LOCAL txt_:={}

   BuildHeader( @txt_, 2 )

   aadd( txt_, "                                                                                              " )
   aadd( txt_, "#ifndef __HBQT_H                                                                              " )
   aadd( txt_, "#define __HBQT_H                                                                              " )
   aadd( txt_, "                                                                                              " )
   aadd( txt_, "                                                                                              " )
   aadd( txt_, "#define QT_VERSION 0x040500                                                                   " )
   aadd( txt_, "                                                                                              " )
   aadd( txt_, "#include <Qt/qglobal.h>                                                                       " )
   aadd( txt_, "                                                                                              " )
   aadd( txt_, "#if QT_VERSION >= 0x040500                                                                    " )
   aadd( txt_, "                                                                                              " )
   aadd( txt_, "                                                                                              " )
   aadd( txt_, "#define hbqt_par_QAbstractButton( n )        ( ( QAbstractButton* ) hb_parptr( n ) )          " )
   aadd( txt_, "#define hbqt_par_QAbstractItemView( n )      ( ( QAbstractItemView* ) hb_parptr( n ) )        " )
   aadd( txt_, "#define hbqt_par_QAbstractItemModel( n )     ( ( QAbstractItemModel* ) hb_parptr( n ) )       " )
   aadd( txt_, "#define hbqt_par_QAbstractItemDelegate( n )  ( ( QAbstractItemDelegate* ) hb_parptr( n ) )    " )
   aadd( txt_, "#define hbqt_par_QAbstractPrintDialog( n )   ( ( QAbstractPrintDialog* ) hb_parptr( n ) )     " )
   aadd( txt_, "#define hbqt_par_QAbstractProxyModel( n )    ( ( QAbstractProxyModel* ) hb_parptr( n ) )      " )
   aadd( txt_, "#define hbqt_par_QAbstractScrollArea( n )    ( ( QAbstractScrollArea* ) hb_parptr( n ) )      " )
   aadd( txt_, "#define hbqt_par_QAbstractSlider( n )        ( ( QAbstractSlider* ) hb_parptr( n ) )          " )
   aadd( txt_, "#define hbqt_par_QAbstractSpinBox( n )       ( ( QAbstractSpinBox* ) hb_parptr( n ) )         " )
   aadd( txt_, "#define hbqt_par_QAction( n )                ( ( QAction* ) hb_parptr( n ) )                  " )
   aadd( txt_, "#define hbqt_par_QActionGroup( n )           ( ( QActionGroup* ) hb_parptr( n ) )             " )
   aadd( txt_, "#define hbqt_par_QApplication( n )           ( ( QApplication* ) hb_parptr( n ) )             " )
   aadd( txt_, "#define hbqt_par_QBoxLayout( n )             ( ( QBoxLayout* ) hb_parptr( n ) )               " )
   aadd( txt_, "#define hbqt_par_QBrush( n )                 ( ( QBrush* ) hb_parptr( n ) )                   " )
   aadd( txt_, "#define hbqt_par_QCalendarWidget( n )        ( ( QCalendarWidget* ) hb_parptr( n ) )          " )
   aadd( txt_, "#define hbqt_par_QCheckBox( n )              ( ( QCheckBox* ) hb_parptr( n ) )                " )
   aadd( txt_, "#define hbqt_par_QColor( n )                 ( ( QColor* ) hb_parptr( n ) )                   " )
   aadd( txt_, "#define hbqt_par_QColorDialog( n )           ( ( QColorDialog* ) hb_parptr( n ) )             " )
   aadd( txt_, "#define hbqt_par_QComboBox( n )              ( ( QComboBox* ) hb_parptr( n ) )                " )
   aadd( txt_, "#define hbqt_par_QCommandLinkButton( n )     ( ( QCommandLinkButton* ) hb_parptr( n ) )       " )
   aadd( txt_, "#define hbqt_par_QCompleter( n )             ( ( QCompleter* ) hb_parptr( n ) )               " )
   aadd( txt_, "#define hbqt_par_QCoreApplication( n )       ( ( QCoreApplication* ) hb_parptr( n ) )         " )
   aadd( txt_, "#define hbqt_par_QDateTimeEdit( n )          ( ( QDateTimeEdit* ) hb_parptr( n ) )            " )
   aadd( txt_, "#define hbqt_par_QDial( n )                  ( ( QDial* ) hb_parptr( n ) )                    " )
   aadd( txt_, "#define hbqt_par_QDialog( n )                ( ( QDialog* ) hb_parptr( n ) )                  " )
   aadd( txt_, "#define hbqt_par_QDockWidget( n )            ( ( QDockWidget* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QDoubleSpinBox( n )         ( ( QDoubleSpinBox* ) hb_parptr( n ) )           " )
   aadd( txt_, "#define hbqt_par_QEvent( n )                 ( ( QEvent* ) hb_parptr( n ) )                   " )
   aadd( txt_, "#define hbqt_par_QFileDialog( n )            ( ( QFileDialog* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QFileIconProvider( n )      ( ( QFileIconProvider* ) hb_parptr( n ) )        " )
   aadd( txt_, "#define hbqt_par_QFocusFrame( n )            ( ( QFocusFrame* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QFont( n )                  ( ( QFont* ) hb_parptr( n ) )                    " )
   aadd( txt_, "#define hbqt_par_QFontComboBox( n )          ( ( QFontComboBox* ) hb_parptr( n ) )            " )
   aadd( txt_, "#define hbqt_par_QFontDialog( n )            ( ( QFontDialog* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QFormLayout( n )            ( ( QFormLayout* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QFrame( n )                 ( ( QFrame* ) hb_parptr( n ) )                   " )
   aadd( txt_, "#define hbqt_par_QGroupBox( n )              ( ( QGroupBox* ) hb_parptr( n ) )                " )
   aadd( txt_, "#define hbqt_par_QHeaderView( n )            ( ( QHeaderView* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QInputContext( n )          ( ( QInputContext* ) hb_parptr( n ) )            " )
   aadd( txt_, "#define hbqt_par_QInputDialog( n )           ( ( QInputDialog* ) hb_parptr( n ) )             " )
   aadd( txt_, "#define hbqt_par_QItemSelectionModel( n )    ( ( QItemSelectionModel* ) hb_parptr( n ) )      " )
   aadd( txt_, "#define hbqt_par_QLabel( n )                 ( ( QLabel* ) hb_parptr( n ) )                   " )
   aadd( txt_, "#define hbqt_par_QLayout( n )                ( ( QLayout* ) hb_parptr( n ) )                  " )
   aadd( txt_, "#define hbqt_par_QLayoutItem( n )            ( ( QLayoutItem* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QLCDNumber( n )             ( ( QLCDNumber* ) hb_parptr( n ) )               " )
   aadd( txt_, "#define hbqt_par_QLine( n )                  ( ( QLine* ) hb_parptr( n ) )                    " )
   aadd( txt_, "#define hbqt_par_QLineEdit( n )              ( ( QLineEdit* ) hb_parptr( n ) )                " )
   aadd( txt_, "#define hbqt_par_QLineF( n )                 ( ( QLineF* ) hb_parptr( n ) )                   " )
   aadd( txt_, "#define hbqt_par_QListView( n )              ( ( QListView* ) hb_parptr( n ) )                " )
   aadd( txt_, "#define hbqt_par_QLocale( n )                ( ( QLocale* ) hb_parptr( n ) )                  " )
   aadd( txt_, "#define hbqt_par_QMainWindow( n )            ( ( QMainWindow* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QMenu( n )                  ( ( QMenu* ) hb_parptr( n ) )                    " )
   aadd( txt_, "#define hbqt_par_QMenuBar( n )               ( ( QMenuBar* ) hb_parptr( n ) )                 " )
   aadd( txt_, "#define hbqt_par_QMessageBox( n )            ( ( QMessageBox* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QMovie( n )                 ( ( QMovie* ) hb_parptr( n ) )                   " )
   aadd( txt_, "#define hbqt_par_QObject( n )                ( ( QObject* ) hb_parptr( n ) )                  " )
   aadd( txt_, "#define hbqt_par_QPageSetupDialog( n )       ( ( QPageSetupDialog* ) hb_parptr( n ) )         " )
   aadd( txt_, "#define hbqt_par_QPainter( n )               ( ( QPainter* ) hb_parptr( n ) )                 " )
   aadd( txt_, "#define hbqt_par_QPaintDevice( n )           ( ( QPaintDevice* ) hb_parptr( n ) )             " )
   aadd( txt_, "#define hbqt_par_QPen( n )                   ( ( QPen* ) hb_parptr( n ) )                     " )
   aadd( txt_, "#define hbqt_par_QPoint( n )                 ( ( QPoint* ) hb_parptr( n ) )                   " )
   aadd( txt_, "#define hbqt_par_QPointF( n )                ( ( QPointF* ) hb_parptr( n ) )                  " )
   aadd( txt_, "#define hbqt_par_QPrintDialog( n )           ( ( QPrintDialog* ) hb_parptr( n ) )             " )
   aadd( txt_, "#define hbqt_par_QPrintPreviewDialog( n )    ( ( QPrintPreviewDialog* ) hb_parptr( n ) )      " )
   aadd( txt_, "#define hbqt_par_QPrinter( n )               ( ( QPrinter* ) hb_parptr( n ) )                 " )
   aadd( txt_, "#define hbqt_par_QProgressBar( n )           ( ( QProgressBar* ) hb_parptr( n ) )             " )
   aadd( txt_, "#define hbqt_par_QProgressDialog( n )        ( ( QProgressDialog* ) hb_parptr( n ) )          " )
   aadd( txt_, "#define hbqt_par_QPushButton( n )            ( ( QPushButton* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QRect( n )                  ( ( QRect* ) hb_parptr( n ) )                    " )
   aadd( txt_, "#define hbqt_par_QRectF( n )                 ( ( QRectF* ) hb_parptr( n ) )                   " )
   aadd( txt_, "#define hbqt_par_QScrollArea( n )            ( ( QScrollArea* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QScrollBar( n )             ( ( QScrollBar* ) hb_parptr( n ) )               " )
   aadd( txt_, "#define hbqt_par_QSpacerItem( n )            ( ( QSpacerItem* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QSpinBox( n )               ( ( QSpinBox* ) hb_parptr( n ) )                 " )
   aadd( txt_, "#define hbqt_par_QSplitter( n )              ( ( QSplitter* ) hb_parptr( n ) )                " )
   aadd( txt_, "#define hbqt_par_QStatusBar( n )             ( ( QStatusBar* ) hb_parptr( n ) )               " )
   aadd( txt_, "#define hbqt_par_QStyle( n )                 ( ( QStyle* ) hb_parptr( n ) )                   " )
   aadd( txt_, "#define hbqt_par_QSlider( n )                ( ( QSlider* ) hb_parptr( n ) )                  " )
   aadd( txt_, "#define hbqt_par_QTabBar( n )                ( ( QTabBar* ) hb_parptr( n ) )                  " )
   aadd( txt_, "#define hbqt_par_QTableView( n )             ( ( QTableView* ) hb_parptr( n ) )               " )
   aadd( txt_, "#define hbqt_par_QTableWidget( n )           ( ( QTableWidget* ) hb_parptr( n ) )             " )
   aadd( txt_, "#define hbqt_par_QTableWidgetItem( n )       ( ( QTableWidgetItem* ) hb_parptr( n ) )         " )
   aadd( txt_, "#define hbqt_par_QTabWidget( n )             ( ( QTabWidget* ) hb_parptr( n ) )               " )
   aadd( txt_, "#define hbqt_par_QTextEdit( n )              ( ( QTextEdit* ) hb_parptr( n ) )                " )
   aadd( txt_, "#define hbqt_par_QTextDocument( n )          ( ( QTextDocument* ) hb_parptr( n ) )            " )
   aadd( txt_, "#define hbqt_par_QThread( n )                ( ( QThread* ) hb_parptr( n ) )                  " )
   aadd( txt_, "#define hbqt_par_QToolBar( n )               ( ( QToolBar* ) hb_parptr( n ) )                 " )
   aadd( txt_, "#define hbqt_par_QToolBox( n )               ( ( QToolBox* ) hb_parptr( n ) )                 " )
   aadd( txt_, "#define hbqt_par_QToolButton( n )            ( ( QToolButton* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QTreeView( n )              ( ( QTreeView* ) hb_parptr( n ) )                " )
   aadd( txt_, "#define hbqt_par_QTreeWidget( n )            ( ( QTreeWidget* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QTreeWidgetItem( n )        ( ( QTreeWidgetItem* ) hb_parptr( n ) )          " )
   aadd( txt_, "#define hbqt_par_QValidator( n )             ( ( QValidator* ) hb_parptr( n ) )               " )
   aadd( txt_, "#define hbqt_par_QWebView( n )               ( ( QWebView* ) hb_parptr( n ) )                 " )
   aadd( txt_, "#define hbqt_par_QWidget( n )                ( ( QWidget* ) hb_parptr( n ) )                  " )
   aadd( txt_, "#define hbqt_par_QWindowSurface( n )         ( ( QWindowSurface* ) hb_parptr( n ) )           " )
   aadd( txt_, "#define hbqt_par_QWizard( n )                ( ( QWizard* ) hb_parptr( n ) )                  " )
   aadd( txt_, "#define hbqt_par_QWizardPage( n )            ( ( QWizardPage* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QWSEvent( n )               ( ( QWSEvent* ) hb_parptr( n ) )                 " )
   aadd( txt_, "#define hbqt_par_QWebPage( n )               ( ( QWebPage* ) hb_parptr( n ) )                 " )
   aadd( txt_, "#define hbqt_par_QNetworkAccessManager( n )  ( ( QNetworkAccessManager* ) hb_parptr( n ) )    " )
   aadd( txt_, "#define hbqt_par_QWebPluginFactory( n )      ( ( QWebPluginFactory* ) hb_parptr( n ) )        " )
   aadd( txt_, "#define hbqt_par_QContextMenuEvent( n )      ( ( QContextMenuEvent* ) hb_parptr( n ) )        " )
   aadd( txt_, "                                                                                              " )
   aadd( txt_, "#define hbqt_par_QIcon( n )                  ( ( QIcon ) hb_parc( n ) )                       " )
   aadd( txt_, "#define hbqt_par_QString( n )                ( ( QString ) hb_parc( n ) )                     " )
   aadd( txt_, "#define hbqt_par_QRgb( n )                   ( hb_parnint( n ) )                              " )
   aadd( txt_, "#define hbqt_par_Bool( n )                   ( hb_parl( n ) )                                 " )
   aadd( txt_, "#define hbqt_par_char( n )                   ( hb_parc( n ) )                                 " )
   aadd( txt_, "                                                                                              " )
   aadd( txt_, "#define hbqt_ret_QWidget( p )                ( hb_retptr( ( QWidget* ) p ) )                  " )
   aadd( txt_, "#define hbqt_ret_QAbstractItemDelegate( p )  ( hb_retptr( ( QAbstractItemDelegate* ) p ) )    " )
   aadd( txt_, "#define hbqt_ret_QAbstractItemModel( p )     ( hb_retptr( ( QAbstractItemModel* ) p ) )       " )
   aadd( txt_, "#define hbqt_ret_QPrinter( p )               ( hb_retptr( ( QPrinter* ) p ) )                 " )
   aadd( txt_, "                                                                                              " )
   aadd( txt_, "#include <QtGui/QWidget>                                                                      " )
   aadd( txt_, "                                                                                              " )
   aadd( txt_, "void    hbqt_ret_QRect( QRect );                                                              " )
   aadd( txt_, "void    hbqt_ret_QSize( QSize );                                                              " )
   aadd( txt_, "void    hbqt_ret_QPoint( QPoint );                                                            " )
   aadd( txt_, "                                                                                              " )
   aadd( txt_, "QRect   hbqt_const_QRect( int );                                                              " )
   aadd( txt_, "QSize   hbqt_const_QSize( int );                                                              " )
   aadd( txt_, "QPoint  hbqt_const_QPoint( int );                                                             " )
   aadd( txt_, "                                                                                              " )
   aadd( txt_, "void    hb_ToOutDebug( const char * sTraceMsg, ... );                                         " )
   aadd( txt_, "                                                                                              " )
   aadd( txt_, "#endif                                                                                        " )
   aadd( txt_, "                                                                                              " )
   aadd( txt_, "#endif /* __HBQT_H */                                                                         " )
   aadd( txt_, "                                                                                              " )

   RETURN CreateTarget( cFile, txt_ )

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_HBQT_UTILS_CPP( cPathOut )
   LOCAL cFile := cPathOut + s_PathSep + 'hbqt_utils.cpp'
   LOCAL txt_:={}

   BuildHeader( @txt_, 2 )

   aadd( txt_, '#include "hbapi.h"                                                        ' )
   aadd( txt_, '#include "hbapiitm.h"                                                     ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '#include "hbqt.h"                                                         ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/' )
   aadd( txt_, '#if QT_VERSION >= 0x040500                                                ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '#include <QtGui/QWidget>                                                  ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, 'void hbqt_ret_QRect( QRect qrc )                                          ' )
   aadd( txt_, '{                                                                         ' )
   aadd( txt_, '   PHB_ITEM info = hb_itemArrayNew( 4 );                                  ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '   hb_arraySetNI( info, 1, qrc.x() );                                     ' )
   aadd( txt_, '   hb_arraySetNI( info, 2, qrc.y() );                                     ' )
   aadd( txt_, '   hb_arraySetNI( info, 3, qrc.x()+qrc.width() );                         ' )
   aadd( txt_, '   hb_arraySetNI( info, 4, qrc.y()+qrc.height() );                        ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '   hb_itemReturnRelease( info );                                          ' )
   aadd( txt_, '}                                                                         ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, 'QRect hbqt_const_QRect( int i )                                           ' )
   aadd( txt_, '{                                                                         ' )
   aadd( txt_, '   QRect qrc;                                                             ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '   qrc.setX( hb_parni( i,1 ) );                                           ' )
   aadd( txt_, '   qrc.setY( hb_parni( i,2 ) );                                           ' )
   aadd( txt_, '   qrc.setWidth( hb_parni( i,3 ) - hb_parni( i,1 ) + 1 );                 ' )
   aadd( txt_, '   qrc.setHeight( hb_parni( i,4 ) - hb_parni( i,2 ) + 1 );                ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '   return qrc;                                                            ' )
   aadd( txt_, '}                                                                         ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, 'void hbqt_ret_QSize( QSize qsz )                                          ' )
   aadd( txt_, '{                                                                         ' )
   aadd( txt_, '   PHB_ITEM info = hb_itemArrayNew( 2 );                                  ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '   hb_arraySetNI( info, 1, qsz.width() );                                 ' )
   aadd( txt_, '   hb_arraySetNI( info, 2, qsz.height() );                                ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '   hb_itemReturnRelease( info );                                          ' )
   aadd( txt_, '}                                                                         ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, 'QSize hbqt_const_QSize( int i )                                           ' )
   aadd( txt_, '{                                                                         ' )
   aadd( txt_, '   QSize qsz;                                                             ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '   qsz.setWidth( hb_parni( i,1 ) );                                       ' )
   aadd( txt_, '   qsz.setHeight( hb_parni( i,2 ) );                                      ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '   return qsz;                                                            ' )
   aadd( txt_, '}                                                                         ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, 'void hbqt_ret_QPoint( QPoint qpt )                                        ' )
   aadd( txt_, '{                                                                         ' )
   aadd( txt_, '   PHB_ITEM info = hb_itemArrayNew( 2 );                                  ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '   hb_arraySetNI( info, 1, qpt.x() );                                     ' )
   aadd( txt_, '   hb_arraySetNI( info, 2, qpt.y() );                                     ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '   hb_itemReturnRelease( info );                                          ' )
   aadd( txt_, '}                                                                         ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, 'QPoint hbqt_const_QPoint( int i )                                         ' )
   aadd( txt_, '{                                                                         ' )
   aadd( txt_, '   QPoint qpt;                                                            ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '   qpt.setX( hb_parni( i,1 ) );                                           ' )
   aadd( txt_, '   qpt.setY( hb_parni( i,2 ) );                                           ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '   return qpt;                                                            ' )
   aadd( txt_, '}                                                                         ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/' )
   aadd( txt_, '#endif             /* #if QT_VERSION >= 0x040500 */                       ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/' )
   aadd( txt_, '                                                                          ' )

   RETURN CreateTarget( cFile, txt_ )

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_HBQT_BASE_CPP( cPathOut )
   LOCAL cFile := cPathOut + s_PathSep + 'hbqt_base.cpp'
   LOCAL txt_:={}

   BuildHeader( @txt_, 2 )

   aadd( txt_, '#include "hbapi.h"                  ' )
   aadd( txt_, '                                    ' )
   aadd( txt_, '#include "hbqt.h"                   ' )
   aadd( txt_, '                                    ' )
   aadd( txt_, 'HB_FUNC( QT_VERSION )               ' )
   aadd( txt_, '{                                   ' )
   aadd( txt_, '   hb_retnint( QT_VERSION );        ' )
   aadd( txt_, '}                                   ' )
   aadd( txt_, '                                    ' )
   aadd( txt_, 'HB_FUNC( QT_VERSION_STR )           ' )
   aadd( txt_, '{                                   ' )
   aadd( txt_, '   hb_retc_const( QT_VERSION_STR ); ' )
   aadd( txt_, '}                                   ' )
   aadd( txt_, '                                    ' )

   RETURN CreateTarget( cFile, txt_ )

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_HBQT_SLOTS_H( cPathOut )
   LOCAL cFile := cPathOut + s_PathSep + 'hbqt_slots.h'
   LOCAL txt_:={}

   BuildHeader( @txt_, 2 )

   aadd( txt_, '                                   ' )
   aadd( txt_, '#ifndef SLOTS_H                    ' )
   aadd( txt_, '                                   ' )
   aadd( txt_, '#define SLOTS_H                    ' )
   aadd( txt_, '                                   ' )
   aadd( txt_, '#include <QtCore/QObject>          ' )
   aadd( txt_, '#include <QtCore/QList>            ' )
   aadd( txt_, '                                   ' )
   aadd( txt_, '#include "hbapi.h"                 ' )
   aadd( txt_, '#include "hbapiitm.h"              ' )
   aadd( txt_, '                                   ' )
   aadd( txt_, 'class Slots: public QObject        ' )
   aadd( txt_, '{                                  ' )
   aadd( txt_, '   Q_OBJECT                        ' )
   aadd( txt_, '                                   ' )
   aadd( txt_, '   public:                         ' )
   aadd( txt_, '   Slots( QObject *parent = 0 );   ' )
   aadd( txt_, '   ~Slots();                       ' )
   aadd( txt_, '   QList<QWidget*> list1;          ' )
   aadd( txt_, '   QList<QString> list2;           ' )
   aadd( txt_, '   QList<PHB_ITEM> list3;          ' )
   aadd( txt_, '   QList<bool> list4;              ' )
   aadd( txt_, '                                   ' )
   aadd( txt_, '   public slots:                   ' )
   aadd( txt_, '   void clicked();                 ' )
   aadd( txt_, '   void triggered();               ' )
   aadd( txt_, '   void triggered( bool checked ); ' )
   aadd( txt_, '   void hovered();                 ' )
   aadd( txt_, '   void stateChanged( int state ); ' )
   aadd( txt_, '   void pressed();                 ' )
   aadd( txt_, '   void released();                ' )
   aadd( txt_, '};                                 ' )
   aadd( txt_, '                                   ' )
   aadd( txt_, '#endif                             ' )
   aadd( txt_, '                                   ' )

   RETURN CreateTarget( cFile, txt_ )

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_HBQT_SLOTS_CPP( cPathOut )
   LOCAL cFile := cPathOut + s_PathSep + 'hbqt_slots.cpp'
   LOCAL txt_:={}

   BuildHeader( @txt_, 2 )

   aadd( txt_, '#include "hbapi.h"                                                                                                        ' )
   aadd( txt_, '#include "hbvm.h"                                                                                                         ' )
   aadd( txt_, '#include "hbapiitm.h"                                                                                                     ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '#include "hbqt.h"                                                                                                         ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '#if QT_VERSION >= 0x040500                                                                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '#include "hbqt_slots.h"                                                                                                   ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '#include <QtGui/QWidget>                                                                                                  ' )
   aadd( txt_, '#include <QtCore/QString>                                                                                                 ' )
   aadd( txt_, '#include <QtCore/QList>                                                                                                   ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '#define HBQT_EVT_CLICKED          1                                                                                       ' )
   aadd( txt_, '#define HBQT_EVT_TRIGGERED        2                                                                                       ' )
   aadd( txt_, '#define HBQT_EVT_TRIGGERED_BOOL   3                                                                                       ' )
   aadd( txt_, '#define HBQT_EVT_HOVERED          4                                                                                       ' )
   aadd( txt_, '#define HBQT_EVT_STATECHANGED     5                                                                                       ' )
   aadd( txt_, '#define HBQT_EVT_PRESSED          6                                                                                       ' )
   aadd( txt_, '#define HBQT_EVT_RELEASED         7                                                                                       ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'static Slots *s = NULL;                                                                                                   ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'static void SlotsExec( QWidget* widget, QString event, PHB_ITEM pItem )                                                   ' )
   aadd( txt_, '{                                                                                                                         ' )
   aadd( txt_, '   for( int i = 0; i < s->list1.size(); ++i )                                                                             ' )
   aadd( txt_, '   {                                                                                                                      ' )
   aadd( txt_, '      if( ( QWidget* ) s->list1.at( i ) == widget )                                                                       ' )
   aadd( txt_, '      {                                                                                                                   ' )
   aadd( txt_, '        if( ( ( QString ) s->list2.at( i ) == event ) && ( ( bool ) s->list4.at( i ) == true ) )                          ' )
   aadd( txt_, '        {                                                                                                                 ' )
   aadd( txt_, '           PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QWidget* ) widget );                                                 ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '           hb_vmEvalBlockV( ( PHB_ITEM ) s->list3.at( i ), 1, pWidget );                                                  ' )
   aadd( txt_, '           hb_itemRelease( pWidget );                                                                                     ' )
   aadd( txt_, '        }                                                                                                                 ' )
   aadd( txt_, '      }                                                                                                                   ' )
   aadd( txt_, '   }                                                                                                                      ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   if( pItem != NULL )                                                                                                    ' )
   aadd( txt_, '      hb_itemRelease( pItem );                                                                                            ' )
   aadd( txt_, '}                                                                                                                         ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'Slots::Slots( QObject* parent ) : QObject( parent )                                                                       ' )
   aadd( txt_, '{                                                                                                                         ' )
   aadd( txt_, '}                                                                                                                         ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'Slots::~Slots()                                                                                                           ' )
   aadd( txt_, '{                                                                                                                         ' )
   aadd( txt_, '}                                                                                                                         ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'void Slots::clicked()                                                                                                     ' )
   aadd( txt_, '{                                                                                                                         ' )
   aadd( txt_, '   QWidget *widget = qobject_cast<QWidget *>( sender() );                                                                 ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   SlotsExec( widget, ( QString ) "clicked()", NULL );                                                                    ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   #if 0                                                                                                                  ' )
   aadd( txt_, '   for( int i = 0; i < list1.size(); ++i )                                                                                ' )
   aadd( txt_, '   {                                                                                                                      ' )
   aadd( txt_, '      if( ( QWidget* ) list1.at( i ) == ( QWidget* ) widget )                                                             ' )
   aadd( txt_, '      {                                                                                                                   ' )
   aadd( txt_, '         if( ( ( QString ) list2.at( i ) == ( QString ) "clicked()" ) && ( ( bool ) list4.at( i ) == true ) )             ' )
   aadd( txt_, '         {                                                                                                                ' )
   aadd( txt_, '            PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QWidget* ) widget );                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '            hb_vmEvalBlockV( ( PHB_ITEM ) list3.at( i ), 1, pWidget );                                                    ' )
   aadd( txt_, '            hb_itemRelease( pWidget );                                                                                    ' )
   aadd( txt_, '         }                                                                                                                ' )
   aadd( txt_, '      }                                                                                                                   ' )
   aadd( txt_, '   }                                                                                                                      ' )
   aadd( txt_, '   #endif                                                                                                                 ' )
   aadd( txt_, '}                                                                                                                         ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'void Slots::triggered()                                                                                                   ' )
   aadd( txt_, '{                                                                                                                         ' )
   aadd( txt_, '   QObject *widget = qobject_cast<QWidget *>( sender() );                                                                 ' )
   aadd( txt_, '   for( int i = 0; i < list1.size(); ++i )                                                                                ' )
   aadd( txt_, '   {                                                                                                                      ' )
   aadd( txt_, '      if( ( QObject* ) list1.at( i ) == ( QObject* ) widget )                                                             ' )
   aadd( txt_, '      {                                                                                                                   ' )
   aadd( txt_, '         if( ( ( QString ) list2.at( i ) == ( QString ) "triggered()" ) && ( ( bool ) list4.at( i ) == true ) )           ' )
   aadd( txt_, '         {                                                                                                                ' )
   aadd( txt_, '            PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QObject * ) widget );                                               ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '            hb_vmEvalBlockV( ( PHB_ITEM ) list3.at( i ), 1, pWidget );                                                    ' )
   aadd( txt_, '            hb_itemRelease( pWidget );                                                                                    ' )
   aadd( txt_, '         }                                                                                                                ' )
   aadd( txt_, '      }                                                                                                                   ' )
   aadd( txt_, '   }                                                                                                                      ' )
   aadd( txt_, '}                                                                                                                         ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'void Slots::triggered( bool checked )                                                                                     ' )
   aadd( txt_, '{                                                                                                                         ' )
   aadd( txt_, '   QObject *widget = qobject_cast<QObject *>( sender() );                                                                 ' )
   aadd( txt_, '   for( int i = 0; i < list1.size(); ++i )                                                                                ' )
   aadd( txt_, '   {                                                                                                                      ' )
   aadd( txt_, '      if( ( QObject* ) list1.at( i ) == ( QObject* ) widget )                                                             ' )
   aadd( txt_, '      {                                                                                                                   ' )
   aadd( txt_, '         if( ( ( QString ) list2.at( i ) == ( QString ) "triggered(bool)" ) && ( ( bool ) list4.at( i ) == true ) )       ' )
   aadd( txt_, '         {                                                                                                                ' )
   aadd( txt_, '            PHB_ITEM pWidget  = hb_itemPutPtr( NULL, ( QObject * ) widget );                                              ' )
   aadd( txt_, '            PHB_ITEM pChecked = hb_itemPutL( NULL, checked );                                                             ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '            hb_vmEvalBlockV( ( PHB_ITEM ) list3.at( i ), 2, pWidget, pChecked );                                          ' )
   aadd( txt_, '            hb_itemRelease( pWidget );                                                                                    ' )
   aadd( txt_, '            hb_itemRelease( pChecked );                                                                                   ' )
   aadd( txt_, '         }                                                                                                                ' )
   aadd( txt_, '      }                                                                                                                   ' )
   aadd( txt_, '   }                                                                                                                      ' )
   aadd( txt_, '}                                                                                                                         ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'void Slots::hovered()                                                                                                     ' )
   aadd( txt_, '{                                                                                                                         ' )
   aadd( txt_, '   QObject *widget = qobject_cast<QObject *>( sender() );                                                                 ' )
   aadd( txt_, '   for( int i = 0; i < list1.size(); ++i )                                                                                ' )
   aadd( txt_, '   {                                                                                                                      ' )
   aadd( txt_, '      if( ( QObject* ) list1.at( i ) == ( QObject* ) widget )                                                             ' )
   aadd( txt_, '      {                                                                                                                   ' )
   aadd( txt_, '         if( ( ( QString ) list2.at( i ) == ( QString ) "hovered()" ) && ( ( bool ) list4.at( i ) == true ) )             ' )
   aadd( txt_, '         {                                                                                                                ' )
   aadd( txt_, '            PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QObject * ) widget );                                               ' )
   aadd( txt_, '            hb_vmEvalBlockV( ( PHB_ITEM ) list3.at( i ), 1, pWidget );                                                    ' )
   aadd( txt_, '            hb_itemRelease( pWidget );                                                                                    ' )
   aadd( txt_, '         }                                                                                                                ' )
   aadd( txt_, '      }                                                                                                                   ' )
   aadd( txt_, '   }                                                                                                                      ' )
   aadd( txt_, '}                                                                                                                         ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'void Slots::stateChanged( int state )                                                                                     ' )
   aadd( txt_, '{                                                                                                                         ' )
   aadd( txt_, '   QWidget * widget = qobject_cast<QWidget *>( sender() );                                                                ' )
   aadd( txt_, '   for( int i = 0; i < list1.size(); ++i )                                                                                ' )
   aadd( txt_, '   {                                                                                                                      ' )
   aadd( txt_, '      if( ( QWidget * ) list1.at( i ) == ( QWidget * ) widget )                                                           ' )
   aadd( txt_, '      {                                                                                                                   ' )
   aadd( txt_, '         if( ( ( QString ) list2.at( i ) == ( QString ) "stateChanged(int)" ) && ( ( bool ) list4.at( i ) == true ) )     ' )
   aadd( txt_, '         {                                                                                                                ' )
   aadd( txt_, '            PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QWidget * ) widget );                                               ' )
   aadd( txt_, '            PHB_ITEM pState = hb_itemPutNI( NULL, state );                                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '            hb_vmEvalBlockV( ( PHB_ITEM ) list3.at( i ), 2, pWidget, pState );                                            ' )
   aadd( txt_, '            hb_itemRelease( pWidget );                                                                                    ' )
   aadd( txt_, '            hb_itemRelease( pState );                                                                                     ' )
   aadd( txt_, '         }                                                                                                                ' )
   aadd( txt_, '      }                                                                                                                   ' )
   aadd( txt_, '   }                                                                                                                      ' )
   aadd( txt_, '}                                                                                                                         ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'void Slots::pressed()                                                                                                     ' )
   aadd( txt_, '{                                                                                                                         ' )
   aadd( txt_, '   QWidget * widget = qobject_cast<QWidget *>(sender());                                                                  ' )
   aadd( txt_, '   for( int i = 0; i < list1.size(); ++i )                                                                                ' )
   aadd( txt_, '   {                                                                                                                      ' )
   aadd( txt_, '      if( ( QWidget * ) list1.at( i ) == ( QWidget * ) widget )                                                           ' )
   aadd( txt_, '      {                                                                                                                   ' )
   aadd( txt_, '         if( ( ( QString ) list2.at( i ) == ( QString ) "pressed()" ) && ( ( bool ) list4.at( i ) == true ) )             ' )
   aadd( txt_, '         {                                                                                                                ' )
   aadd( txt_, '            PHB_ITEM pWidget = hb_itemPutPtr( NULL, (QWidget *) widget );                                                 ' )
   aadd( txt_, '            hb_vmEvalBlockV( ( PHB_ITEM ) list3.at( i ), 1, pWidget );                                                    ' )
   aadd( txt_, '            hb_itemRelease( pWidget );                                                                                    ' )
   aadd( txt_, '         }                                                                                                                ' )
   aadd( txt_, '      }                                                                                                                   ' )
   aadd( txt_, '   }                                                                                                                      ' )
   aadd( txt_, '}                                                                                                                         ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'void Slots::released()                                                                                                    ' )
   aadd( txt_, '{                                                                                                                         ' )
   aadd( txt_, '   QWidget* widget = qobject_cast<QWidget *>( sender() );                                                                 ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   for( int i = 0; i < list1.size(); ++i )                                                                                ' )
   aadd( txt_, '   {                                                                                                                      ' )
   aadd( txt_, '      if( ( QWidget* ) list1.at( i ) == ( QWidget* ) widget )                                                             ' )
   aadd( txt_, '      {                                                                                                                   ' )
   aadd( txt_, '         if( ( ( QString ) list2.at( i ) == ( QString ) "released()" ) && ( ( bool ) list4.at( i ) == true ) )            ' )
   aadd( txt_, '         {                                                                                                                ' )
   aadd( txt_, '            PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QWidget * ) widget );                                               ' )
   aadd( txt_, '            hb_vmEvalBlockV( ( PHB_ITEM ) list3.at( i ), 1, pWidget );                                                    ' )
   aadd( txt_, '            hb_itemRelease( pWidget );                                                                                    ' )
   aadd( txt_, '         }                                                                                                                ' )
   aadd( txt_, '      }                                                                                                                   ' )
   aadd( txt_, '   }                                                                                                                      ' )
   aadd( txt_, '}                                                                                                                         ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '#if 0                                                                                                                     ' )
   aadd( txt_, 'HB_FUNC( QT_CONNECT_SIGNAL_1 )                                                                                            ' )
   aadd( txt_, '{                                                                                                                         ' )
   aadd( txt_, '   QWidget * widget    = ( QWidget* ) hb_parptr( 1 ); /* get sender */                                                    ' )
   aadd( txt_, '   int       signal    = hb_parni( 2 );                /* get signal */                                                   ' )
   aadd( txt_, '   PHB_ITEM  codeblock = hb_itemNew( hb_param( 3, HB_IT_BLOCK | HB_IT_BYREF ) ); /* get codeblock */                      ' )
   aadd( txt_, '   bool      ret       = false;                       /* return value */                                                  ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   /* create object s, if not created yet */                                                                              ' )
   aadd( txt_, '   if( s == NULL )                                                                                                        ' )
   aadd( txt_, '      s = new Slots();                                                                                                    ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   /* connect signal with slot                                                                                            ' )
   aadd( txt_, '   // if the list become to long, more classes can be created                                                             ' )
   aadd( txt_, '   // TODO: parameter Qt::AutoConnection                                                                                  ' )
   aadd( txt_, '   */                                                                                                                     ' )
   aadd( txt_, '   switch( signal )                                                                                                       ' )
   aadd( txt_, '   {                                                                                                                      ' )
   aadd( txt_, '   case HBQT_EVT_CLICKED:                                                                                                 ' )
   aadd( txt_, '      ret = widget->connect( widget, SIGNAL( clicked() )          , s, SLOT( clicked() )          , Qt::AutoConnection ); ' )
   aadd( txt_, '      break;                                                                                                              ' )
   aadd( txt_, '   case HBQT_EVT_TRIGGERED:                                                                                               ' )
   aadd( txt_, '      ret = widget->connect( widget, SIGNAL( triggered() )        , s, SLOT( triggered() )        , Qt::AutoConnection ); ' )
   aadd( txt_, '      break;                                                                                                              ' )
   aadd( txt_, '   case HBQT_EVT_TRIGGERED_BOOL:                                                                                          ' )
   aadd( txt_, '      ret = widget->connect( widget, SIGNAL( triggered( bool ) )  , s, SLOT( triggered( bool ) )  , Qt::AutoConnection ); ' )
   aadd( txt_, '      break;                                                                                                              ' )
   aadd( txt_, '   case HBQT_EVT_HOVERED:                                                                                                 ' )
   aadd( txt_, '      ret = widget->connect( widget, SIGNAL( hovered() )          , s, SLOT( hovered() )          , Qt::AutoConnection ); ' )
   aadd( txt_, '      break;                                                                                                              ' )
   aadd( txt_, '   case HBQT_EVT_STATECHANGED:                                                                                            ' )
   aadd( txt_, '      ret = widget->connect( widget, SIGNAL( stateChanged( int ) ), s, SLOT( stateChanged( int ) ), Qt::AutoConnection ); ' )
   aadd( txt_, '      break;                                                                                                              ' )
   aadd( txt_, '   case HBQT_EVT_PRESSED:                                                                                                 ' )
   aadd( txt_, '      ret = widget->connect( widget, SIGNAL( pressed() )          , s, SLOT( pressed() )          , Qt::AutoConnection ); ' )
   aadd( txt_, '      break;                                                                                                              ' )
   aadd( txt_, '   case HBQT_EVT_RELEASED:                                                                                                ' )
   aadd( txt_, '      ret = widget->connect( widget, SIGNAL( released() )         , s, SLOT( released() )         , Qt::AutoConnection ); ' )
   aadd( txt_, '      break;                                                                                                              ' )
   aadd( txt_, '   }                                                                                                                      ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   // return connect result                                                                                               ' )
   aadd( txt_, '   hb_retl( ret );                                                                                                        ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   // if connected: store widget, signal, codeblock and flag                                                              ' )
   aadd( txt_, '   // TODO: locate a inactive entry and use it                                                                            ' )
   aadd( txt_, '   if( ret == true )                                                                                                      ' )
   aadd( txt_, '   {                                                                                                                      ' )
   aadd( txt_, '      s->list1 << widget;                                                                                                 ' )
   aadd( txt_, '      s->list2 << signal;                                                                                                 ' )
   aadd( txt_, '      s->list3 << codeblock;                                                                                              ' )
   aadd( txt_, '      s->list4 << true;                                                                                                   ' )
   aadd( txt_, '   }                                                                                                                      ' )
   aadd( txt_, '}                                                                                                                         ' )
   aadd( txt_, '#endif                                                                                                                    ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*                                                                                                                        ' )
   aadd( txt_, 'harbour function to connect signals with slots                                                                            ' )
   aadd( txt_, '*/                                                                                                                        ' )
   aadd( txt_, 'HB_FUNC( QT_CONNECT_SIGNAL )                                                                                              ' )
   aadd( txt_, '{                                                                                                                         ' )
   aadd( txt_, '   QWidget * widget    = ( QWidget* ) hb_parptr( 1 ); /* get sender */                                                    ' )
   aadd( txt_, '   QString   signal    = hb_parc( 2 );                /* get signal */                                                    ' )
   aadd( txt_, '   PHB_ITEM  codeblock = hb_itemNew( hb_param( 3, HB_IT_BLOCK | HB_IT_BYREF ) ); /* get codeblock */                      ' )
   aadd( txt_, '   bool      ret       = false;                       /* return value */                                                  ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   /* create object s, if not created yet */                                                                              ' )
   aadd( txt_, '   if( s == NULL )                                                                                                        ' )
   aadd( txt_, '      s = new Slots();                                                                                                    ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   /* connect signal with slot                                                                                            ' )
   aadd( txt_, '   // if the list become to long, more classes can be created                                                             ' )
   aadd( txt_, '   // TODO: parameter Qt::AutoConnection                                                                                  ' )
   aadd( txt_, '   */                                                                                                                     ' )
   aadd( txt_, '   if( signal == ( QString ) "clicked()" )                                                                                ' )
   aadd( txt_, '   {                                                                                                                      ' )
   aadd( txt_, '      ret = widget->connect( widget, SIGNAL( clicked() )          , s, SLOT( clicked() )          , Qt::AutoConnection ); ' )
   aadd( txt_, '   }                                                                                                                      ' )
   aadd( txt_, '   if( signal == ( QString ) "triggered()" )                                                                              ' )
   aadd( txt_, '   {                                                                                                                      ' )
   aadd( txt_, '      ret = widget->connect( widget, SIGNAL( triggered() )        , s, SLOT( triggered() )        , Qt::AutoConnection ); ' )
   aadd( txt_, '   }                                                                                                                      ' )
   aadd( txt_, '   if( signal == ( QString ) "triggered(bool)" )                                                                          ' )
   aadd( txt_, '   {                                                                                                                      ' )
   aadd( txt_, '      ret = widget->connect( widget, SIGNAL( triggered( bool ) )  , s, SLOT( triggered( bool ) )  , Qt::AutoConnection ); ' )
   aadd( txt_, '   }                                                                                                                      ' )
   aadd( txt_, '   if( signal == ( QString ) "hovered()" )                                                                                ' )
   aadd( txt_, '   {                                                                                                                      ' )
   aadd( txt_, '      ret = widget->connect( widget, SIGNAL( hovered() )          , s, SLOT( hovered() )          , Qt::AutoConnection ); ' )
   aadd( txt_, '   }                                                                                                                      ' )
   aadd( txt_, '   if( signal == ( QString ) "stateChanged(int)" )                                                                        ' )
   aadd( txt_, '   {                                                                                                                      ' )
   aadd( txt_, '      ret = widget->connect( widget, SIGNAL( stateChanged( int ) ), s, SLOT( stateChanged( int ) ), Qt::AutoConnection ); ' )
   aadd( txt_, '   }                                                                                                                      ' )
   aadd( txt_, '   if( signal == ( QString ) "pressed()" )                                                                                ' )
   aadd( txt_, '   {                                                                                                                      ' )
   aadd( txt_, '      ret = widget->connect( widget, SIGNAL( pressed() )          , s, SLOT( pressed() )          , Qt::AutoConnection ); ' )
   aadd( txt_, '   }                                                                                                                      ' )
   aadd( txt_, '   if( signal == ( QString ) "released()" )                                                                               ' )
   aadd( txt_, '   {                                                                                                                      ' )
   aadd( txt_, '      ret = widget->connect( widget, SIGNAL( released() )         , s, SLOT( released() )         , Qt::AutoConnection ); ' )
   aadd( txt_, '   }                                                                                                                      ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   // return connect result                                                                                               ' )
   aadd( txt_, '   hb_retl( ret );                                                                                                        ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   // if connected: store widget, signal, codeblock and flag                                                              ' )
   aadd( txt_, '   // TODO: locate a inactive entry and use it                                                                            ' )
   aadd( txt_, '   if( ret == true )                                                                                                      ' )
   aadd( txt_, '   {                                                                                                                      ' )
   aadd( txt_, '      s->list1 << widget;                                                                                                 ' )
   aadd( txt_, '      s->list2 << signal;                                                                                                 ' )
   aadd( txt_, '      s->list3 << codeblock;                                                                                              ' )
   aadd( txt_, '      s->list4 << true;                                                                                                   ' )
   aadd( txt_, '   }                                                                                                                      ' )
   aadd( txt_, '}                                                                                                                         ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*                                                                                                                        ' )
   aadd( txt_, 'harbour function to disconnect signals                                                                                    ' )
   aadd( txt_, '*/                                                                                                                        ' )
   aadd( txt_, 'HB_FUNC( QT_DISCONNECT_SIGNAL )                                                                                           ' )
   aadd( txt_, '{                                                                                                                         ' )
   aadd( txt_, '   /* TODO */                                                                                                             ' )
   aadd( txt_, '}                                                                                                                         ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*                                                                                                                        ' )
   aadd( txt_, 'harbour function to release all codeblocks storeds                                                                        ' )
   aadd( txt_, '*/                                                                                                                        ' )
   aadd( txt_, '#if 0                                                                                                                     ' )
   aadd( txt_, 'HB_FUNC( RELEASE_CODEBLOCKS )                                                                                             ' )
   aadd( txt_, '{                                                                                                                         ' )
   aadd( txt_, '   if( s )                                                                                                                ' )
   aadd( txt_, '   {                                                                                                                      ' )
   aadd( txt_, '      for( int i = 0; i < s->list1.size(); ++i )                                                                          ' )
   aadd( txt_, '      {                                                                                                                   ' )
   aadd( txt_, '         if( ( bool ) s->list4.at( i ) == true )                                                                          ' )
   aadd( txt_, '         {                                                                                                                ' )
   aadd( txt_, '            hb_itemRelease( ( PHB_ITEM ) s->list3.at( i ) );                                                              ' )
   aadd( txt_, '         }                                                                                                                ' )
   aadd( txt_, '      }                                                                                                                   ' )
   aadd( txt_, '   }                                                                                                                      ' )
   aadd( txt_, '}                                                                                                                         ' )
   aadd( txt_, '#endif                                                                                                                    ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*                                                                                                                        ' )
   aadd( txt_, 'C function to release all codeblocks storeds                                                                              ' )
   aadd( txt_, 'called at end of the program                                                                                              ' )
   aadd( txt_, 'see qapplication.cpp                                                                                                      ' )
   aadd( txt_, '*/                                                                                                                        ' )
   aadd( txt_, 'void release_codeblocks( void )                                                                                           ' )
   aadd( txt_, '{                                                                                                                         ' )
   aadd( txt_, '   if( s )                                                                                                                ' )
   aadd( txt_, '   {                                                                                                                      ' )
   aadd( txt_, '      for( int i = 0; i < s->list1.size(); ++i )                                                                          ' )
   aadd( txt_, '      {                                                                                                                   ' )
   aadd( txt_, '         if( ( bool ) s->list4.at( i ) == true )                                                                          ' )
   aadd( txt_, '         {                                                                                                                ' )
   aadd( txt_, '            hb_itemRelease( ( PHB_ITEM ) s->list3.at( i ) );                                                              ' )
   aadd( txt_, '         }                                                                                                                ' )
   aadd( txt_, '      }                                                                                                                   ' )
   aadd( txt_, '   }                                                                                                                      ' )
   aadd( txt_, '}                                                                                                                         ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/                                                ' )
   aadd( txt_, '#endif                                                                                                                    ' )
   aadd( txt_, '' )


   RETURN CreateTarget( cFile, txt_ )

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_MOC_SLOTS_CPP( cPathOut )
   LOCAL cFile := cPathOut + s_PathSep + 'moc_slots.cpp'
   LOCAL txt_:={}

   aadd( txt_, '/**************************************************************************** ' )
   aadd( txt_, '** Meta object code from reading C++ file "slots.h"                           ' )
   aadd( txt_, '**                                                                            ' )
   aadd( txt_, '** Created: Wed 18. Mar 17:25:39 2009                                         ' )
   aadd( txt_, '**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.0)                 ' )
   aadd( txt_, '**                                                                            ' )
   aadd( txt_, '** WARNING! All changes made in this file will be lost!                       ' )
   aadd( txt_, '*****************************************************************************/' )
   aadd( txt_, '                                                                              ' )
   aadd( txt_, '#include "hbqt.h"                                                             ' )
   aadd( txt_, '                                                                              ' )
   aadd( txt_, '                                                                              ' )
   aadd( txt_, '#if QT_VERSION >= 0x040500                                                    ' )
   aadd( txt_, '                                                                              ' )
   aadd( txt_, '#include "hbqt_slots.h"                                                       ' )
   aadd( txt_, '#if !defined(Q_MOC_OUTPUT_REVISION)                                           ' )
   aadd( txt_, '#error "The header file [slots.h] doesn,t include <QObject>."                 ' )
   aadd( txt_, '#elif Q_MOC_OUTPUT_REVISION != 61                                             ' )
   aadd( txt_, '#error "This file was generated using the moc from 4.5.0. It"                 ' )
   aadd( txt_, '#error "cannot be used with the include files from this version of Qt."       ' )
   aadd( txt_, '#error "(The moc has changed too much.)"                                      ' )
   aadd( txt_, '#endif                                                                        ' )
   aadd( txt_, '                                                                              ' )
   aadd( txt_, 'QT_BEGIN_MOC_NAMESPACE                                                        ' )
   aadd( txt_, 'static const uint qt_meta_data_Slots[] = {                                    ' )
   aadd( txt_, '                                                                              ' )
   aadd( txt_, ' // content:                                                                  ' )
   aadd( txt_, '       2,       // revision                                                   ' )
   aadd( txt_, '       0,       // classname                                                  ' )
   aadd( txt_, '       0,    0, // classinfo                                                  ' )
   aadd( txt_, '       7,   12, // methods                                                    ' )
   aadd( txt_, '       0,    0, // properties                                                 ' )
   aadd( txt_, '       0,    0, // enums/sets                                                 ' )
   aadd( txt_, '       0,    0, // constructors                                               ' )
   aadd( txt_, '                                                                              ' )
   aadd( txt_, ' // slots: signature, parameters, type, tag, flags                            ' )
   aadd( txt_, '       7,    6,    6,    6, 0x0a,                                             ' )
   aadd( txt_, '      17,    6,    6,    6, 0x0a,                                             ' )
   aadd( txt_, '      37,   29,    6,    6, 0x0a,                                             ' )
   aadd( txt_, '      53,    6,    6,    6, 0x0a,                                             ' )
   aadd( txt_, '      69,   63,    6,    6, 0x0a,                                             ' )
   aadd( txt_, '      87,    6,    6,    6, 0x0a,                                             ' )
   aadd( txt_, '      97,    6,    6,    6, 0x0a,                                             ' )
   aadd( txt_, '                                                                              ' )
   aadd( txt_, '       0        // eod                                                        ' )
   aadd( txt_, '};                                                                            ' )
   aadd( txt_, '                                                                              ' )
   aadd( txt_, 'static const char qt_meta_stringdata_Slots[] = {                              ' )
   aadd( txt_, '    "Slots\0\0clicked()\0triggered()\0checked\0"                              ' )
   aadd( txt_, '    "triggered(bool)\0hovered()\0state\0"                                     ' )
   aadd( txt_, '    "stateChanged(int)\0pressed()\0released()\0"                              ' )
   aadd( txt_, '};                                                                            ' )
   aadd( txt_, '                                                                              ' )
   aadd( txt_, 'const QMetaObject Slots::staticMetaObject = {                                 ' )
   aadd( txt_, '    { &QObject::staticMetaObject, qt_meta_stringdata_Slots,                   ' )
   aadd( txt_, '      qt_meta_data_Slots, 0 }                                                 ' )
   aadd( txt_, '};                                                                            ' )
   aadd( txt_, '                                                                              ' )
   aadd( txt_, 'const QMetaObject *Slots::metaObject() const                                  ' )
   aadd( txt_, '{                                                                             ' )
   aadd( txt_, '    return &staticMetaObject;                                                 ' )
   aadd( txt_, '}                                                                             ' )
   aadd( txt_, '                                                                              ' )
   aadd( txt_, 'void *Slots::qt_metacast(const char *_clname)                                 ' )
   aadd( txt_, '{                                                                             ' )
   aadd( txt_, '    if (!_clname) return 0;                                                   ' )
   aadd( txt_, '    if (!strcmp(_clname, qt_meta_stringdata_Slots))                           ' )
   aadd( txt_, '        return static_cast<void*>(const_cast< Slots*>(this));                 ' )
   aadd( txt_, '    return QObject::qt_metacast(_clname);                                     ' )
   aadd( txt_, '}                                                                             ' )
   aadd( txt_, '                                                                              ' )
   aadd( txt_, 'int Slots::qt_metacall(QMetaObject::Call _c, int _id, void **_a)              ' )
   aadd( txt_, '{                                                                             ' )
   aadd( txt_, '    _id = QObject::qt_metacall(_c, _id, _a);                                  ' )
   aadd( txt_, '    if (_id < 0)                                                              ' )
   aadd( txt_, '        return _id;                                                           ' )
   aadd( txt_, '    if (_c == QMetaObject::InvokeMetaMethod) {                                ' )
   aadd( txt_, '        switch (_id) {                                                        ' )
   aadd( txt_, '        case 0: clicked(); break;                                             ' )
   aadd( txt_, '        case 1: triggered(); break;                                           ' )
   aadd( txt_, '        case 2: triggered((*reinterpret_cast< bool(*)>(_a[1]))); break;       ' )
   aadd( txt_, '        case 3: hovered(); break;                                             ' )
   aadd( txt_, '        case 4: stateChanged((*reinterpret_cast< int(*)>(_a[1]))); break;     ' )
   aadd( txt_, '        case 5: pressed(); break;                                             ' )
   aadd( txt_, '        case 6: released(); break;                                            ' )
   aadd( txt_, '        default: ;                                                            ' )
   aadd( txt_, '        }                                                                     ' )
   aadd( txt_, '        _id -= 7;                                                             ' )
   aadd( txt_, '    }                                                                         ' )
   aadd( txt_, '    return _id;                                                               ' )
   aadd( txt_, '}                                                                             ' )
   aadd( txt_, 'QT_END_MOC_NAMESPACE                                                          ' )
   aadd( txt_, '                                                                              ' )
   aadd( txt_, '                                                                              ' )
   aadd( txt_, '#endif                                                                        ' )
   aadd( txt_, '                                                                              ' )

   RETURN CreateTarget( cFile, txt_ )

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_Demo( cPathOut )
   LOCAL cFile := '..\tests\demoqt.prg'
   LOCAL txt_:={}

   BuildHeader( @txt_, 2 )

   aadd( txt_, '#define QT_PTROF( oObj )  ( oObj:pPtr )                                        ' )
   aadd( txt_, '                                                                               ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/     ' )
   aadd( txt_, '                                                                               ' )
   aadd( txt_, 'INIT PROCEDURE Qt_Start()                                                      ' )
   aadd( txt_, '   qt_qapplication()                                                           ' )
   aadd( txt_, '   RETURN                                                                      ' )
   aadd( txt_, '                                                                               ' )
   aadd( txt_, 'EXIT PROCEDURE Qt_End()                                                        ' )
   aadd( txt_, '   qt_qapplication_exec()                                                      ' )
   aadd( txt_, '   RETURN                                                                      ' )
   aadd( txt_, '                                                                               ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/     ' )
   aadd( txt_, '                                                                               ' )
   aadd( txt_, 'PROCEDURE Main()                                                               ' )
   aadd( txt_, '   Local oLabel                                                                ' )
   aadd( txt_, '   Local oWnd                                                                  ' )
   aadd( txt_, '   Local oMenuBar                                                              ' )
   aadd( txt_, '   Local oMenuA                                                                ' )
   aadd( txt_, '   LOCAL oPS, oPPrv, oMB, oWZ, oCD, oWP                                        ' )
   aadd( txt_, '                                                                               ' )
   aadd( txt_, '   oWnd := QMainWindow():New()                                                 ' )
   aadd( txt_, '   oWnd:SetWindowTitle("Testing - QMainWindow, QMenu, QMenuBar and QLabel" )   ' )
   aadd( txt_, '   oWnd:Resize( { 640, 400 } )                                                 ' )
   aadd( txt_, '                                                                               ' )
   aadd( txt_, '   oMenuBar := QMenuBar():new( QT_PTROF( oWnd ) )                              ' )
   aadd( txt_, '   oMenuBar:resize( { oWnd:width(), 20 } )                                     ' )
   aadd( txt_, '   oMenuBar:addAction( "First" )                                               ' )
   aadd( txt_, '   oMenuBar:addSeparator()                                                     ' )
   aadd( txt_, '   oMenuBar:addAction( "Second" )                                              ' )
   aadd( txt_, '                                                                               ' )
   aadd( txt_, '   oMenuA := QMenu():new( QT_PTROF( oMenuBar ) )                               ' )
   aadd( txt_, '   oMenuA:setTitle( "New" )                                                    ' )
   aadd( txt_, '   oMenuA:addAction( "File" )                                                  ' )
   aadd( txt_, '   oMenuA:addAction( "Open" )                                                  ' )
   aadd( txt_, '   oMenuA:addSeparator()                                                       ' )
   aadd( txt_, '   oMenuA:addAction( "Close" )                                                 ' )
   aadd( txt_, '   oMenuBar:addMenu( QT_PTROF( oMenuA ) )                                      ' )
   aadd( txt_, '                                                                               ' )
   aadd( txt_, '   oLabel := QLabel():New( QT_PTROF( oWnd ) )                                  ' )
   aadd( txt_, '   oLabel:SetText( "Testing Harbour + Qt" )                                    ' )
   aadd( txt_, '   oLabel:move( { 100,100 } )                                                  ' )
   aadd( txt_, '   oLabel:Show()                                                               ' )
   aadd( txt_, '                                                                               ' )
   aadd( txt_, '   oWnd:Show()                                                                 ' )
   aadd( txt_, '                                                                               ' )
   aadd( txt_, '   oPS := QPageSetupDialog():new()                                             ' )
   aadd( txt_, '   oPS:setWindowTitle( "Harbour-QT PageSetup Dialog" )                         ' )
   aadd( txt_, '   oPS:show()                                                                  ' )
   aadd( txt_, '   oPPrv := QPrintPreviewDialog():new()                                        ' )
   aadd( txt_, '   oPPrv:setWindowTitle( "Harbour-QT Preview Preview Dialog" )                 ' )
   aadd( txt_, '   oPPrv:show()                                                                ' )
   aadd( txt_, '   oWZ := QWizard():new()                                                      ' )
   aadd( txt_, '   oWZ:setWindowTitle( "Harbour-QT Wizard to Show Slides etc." )               ' )
   aadd( txt_, '   oWZ:show()                                                                  ' )
   aadd( txt_, '   oCD := QColorDialog():new()                                                 ' )
   aadd( txt_, '   oCD:setWindowTitle( "Harbour-QT Color Selection Dialog" )                   ' )
   aadd( txt_, '   oCD:show()                                                                  ' )
   aadd( txt_, '   oWP := QWebView():new()                                                     ' )
   aadd( txt_, '   oWP:setWindowTitle( "Harbour-QT Web Page Navigator" )                       ' )
   aadd( txt_, '   oWP:show()                                                                  ' )
   aadd( txt_, '                                                                               ' )
   aadd( txt_, '   RETURN                                                                      ' )
   aadd( txt_, '                                                                               ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/     ' )
   aadd( txt_, '                                                                               ' )
   aadd( txt_, 'PROCEDURE HB_GtSys()                                                           ' )
   aadd( txt_, '   HB_GT_GUI_DEFAULT()                                                         ' )
   aadd( txt_, '   RETURN                                                                      ' )
   aadd( txt_, '                                                                               ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/     ' )
   aadd( txt_, '                                                                               ' )

   RETURN CreateTarget( cFile, txt_ )

/*----------------------------------------------------------------------*/
