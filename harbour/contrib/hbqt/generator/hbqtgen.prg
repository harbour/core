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

#include "fileio.ch"

#define _EOL   chr( 10 )

STATIC s_NewLine
STATIC s_PathSep

/*----------------------------------------------------------------------*/

FUNCTION Main( ... )
   LOCAL aParam, cLParam
   LOCAL cParam, cPathOut, cPathIn, cProFile, cPathDoc
   LOCAL x, cPath, cFile, cExt
   LOCAL aPrjFiles := {}
   LOCAL aProFiles := {}

   s_NewLine := hb_OsNewLine()
   s_PathSep := hb_OsPathSeparator()

   DispLogo()

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
         cPathOut := substr( cParam, 3 )

      CASE left( cParam,2 ) == '-I'
         cPathIn := substr( cParam, 3 )

      CASE left( cParam,2 ) == '-D'
         cPathDoc := substr( cParam, 3 )

      CASE cParam == '-c'

      CASE cLParam == '-help'
         DispHelp()
         RETURN nil

      ENDCASE
   NEXT

   IF empty( aPrjFiles ) .and. hb_fileExists( "qt45.qtp" )
      aadd( aPrjFiles, "qt45.qtp" )
   ENDIF

   IF empty( aPrjFiles ) .and. empty( aProFiles )
      DispHelp()
      RETURN nil
   ENDIF

   IF empty( cPathOut )
      cPathOut := hb_dirBase()
   ENDIF
   IF empty( cPathIn )
      cPathIn  := hb_dirBase()
   ENDIF
   IF empty( cPathDoc )
      cPathDoc := hb_dirBase()
   ENDIF
   IF Right( cPathOut, 1 ) == s_PathSep
      cPathOut := hb_StrShrink( cPathOut, 1 )
   ENDIF
   IF Right( cPathIn, 1 ) == s_PathSep
      cPathIn := hb_StrShrink( cPathIn, 1 )
   ENDIF
   IF Right( cPathDoc, 1 ) == s_PathSep
      cPathDoc := hb_StrShrink( cPathDoc, 1 )
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

   OutStd( "Processing: " + cFile + s_NewLine )

   cPrj  := memoread( cFile )

   /* Pullout all ANSI C style comments */
   DO WHILE .t.
      IF ( n := at( '/*', cPrj ) ) == 0
         EXIT
      ENDIF
      /* We must have a matching pair */
      nn := at( '*/', cPrj )
      IF nn == 0
         OutStd( 'Project file has unbalanced comment section...' + s_NewLine )
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

STATIC FUNCTION PullOutSection( cQth, cSec )
   LOCAL cTxt, n, nn, cTknB, cTknE
   LOCAL a_:={}

   cTknB := '<'+cSec+'>'
   cTknE := '</'+cSec+'>'

   IF ( n := at( cTknB, cQth ) ) > 0
      IF( nn := at( cTknE, cQth ) ) > 0
         cTxt := substr( cQth, n+len( cTknB ), nn-1-( n+len( cTknB ) ) )
      ENDIF
      IF !empty( cTxt )
         a_:= hb_ATokens( cTxt, _EOL )
      ENDIF
   ENDIF

   RETURN a_

/*----------------------------------------------------------------------*/

STATIC FUNCTION GenSource( cProFile, cPathIn, cPathOut, cPathDoc )
   LOCAL cFile, cWidget, cExt, cPath, cOrg, cCPP, cPRG
   LOCAL cQth, cFileCpp
   LOCAL s, n, hHandle, nFuncs, nCnvrtd
   LOCAL b_, txt_, enum_, code_, func_, dummy_, cpp_, cmntd_, doc_, vrb_, varbls_
   LOCAL class_, cls_, protos_, slots_, enums_

   hb_fNameSplit( cProFile, @cPath, @cWidget, @cExt )

   IF empty( cPath )
      cFile := cPathIn + s_PathSep + cProFile
   ELSE
      cFile := cProFile
   ENDIF
   IF !file( cFile )
      OutStd( "Cannot find: " + cFile + s_NewLine )
      RETURN { nil }
   ENDIF

   cQth := memoread( cFile )
   IF empty( cQth )
      OutStd( "Cannot read: " + cFile + s_NewLine )
      RETURN { nil }
   ENDIF

   OutStd( "Processing: " + cFile + s_NewLine )

   /* Prepare to be parsed properly */
   IF ! hb_osNewLine() == _EOL
      cQth := StrTran( cQth, hb_osNewLine(), _EOL )
   ENDIF
   IF ! hb_osNewLine() == Chr( 13 ) + Chr( 10 )
      cQth := StrTran( cQth, Chr( 13 ) + Chr( 10 ), _EOL )
   ENDIF

   cls_:={}
   IF !empty( class_:= PullOutSection( @cQth, 'CLASS' ) )
      FOR EACH s IN class_
         IF ( n := at( '=', s ) ) > 0
            aadd( cls_, { alltrim( substr( s, 1, n-1 ) ), alltrim( substr( s, n+1 ) ) } )
         ENDIF
      NEXT
   ENDIF

   /* Pull out Code Section */
   code_   := PullOutSection( @cQth, 'CODE'   )

   /* Pull out Enumerators  */
   enums_  := PullOutSection( @cQth, 'ENUMS'  )
   enum_:={}
   FOR EACH s IN enums_
      IF ( 'enum ' $ s .or. 'flags ' $ s )
         b_:= hb_ATokens( alltrim( s ),' ' )
         aadd( enum_, b_[ 2 ] )
      ENDIF
   NEXT

   /* Pull out Prototypes   */
   protos_ := PullOutSection( @cQth, 'PROTOS' )

   /* Pull out Variables */
   varbls_ := PullOutSection( @cQth, 'VARIABLES' )

   /* Pull Out Signals      */
   slots_  := PullOutSection( @cQth, 'SLOTS'  )

   /* Combine signals and protos : same nature */
   aeval( slots_, {|e| aadd( protos_, e ) } )

   dummy_  := {}
   func_   := { { "", 0 } }
   txt_    := {}
   cpp_    := {}
   cmntd_  := {}
   doc_    := {}
   nFuncs  := 0
   nCnvrtd := 0

   /* Body */
   FOR EACH s IN protos_
      cOrg := s

      IF empty( s := alltrim( s ) )
         LOOP
      ENDIF
      /* Check if it is not ANSI C Comment */
      IF left( alltrim( cOrg ),1 ) $ '/*'
         LOOP
      ENDIF
      /* Another comment tokens */
      IF empty( s ) .or. left( s,1 ) $ '#;'
         LOOP
      ENDIF

      nFuncs++

      /* Check if proto is commented out */
      IF left( s,2 ) == '//'
         aadd( cmntd_, cOrg )
         LOOP
      ENDIF
      /* Lists - Later */
      IF '<' $ s
         aadd( dummy_, cOrg )
         LOOP
      ENDIF

      IF ParseProto( s, cWidget, @txt_, @doc_, enum_, func_ )
         nCnvrtd++
      ELSE
         aadd( dummy_, cOrg )
      ENDIF
   NEXT

   FOR EACH s IN varbls_
      cOrg := s

      IF empty( s := alltrim( s ) )
         LOOP
      ENDIF
      /* Check if it is not ANSI C Comment */
      IF left( alltrim( cOrg ),1 ) $ '/*'
         LOOP
      ENDIF
      /* Another comment tokens */
      IF empty( s ) .or. left( s,1 ) $ '#;'
         LOOP
      ENDIF

      nFuncs++

      /* Check if proto is commented out */
      IF left( s,2 ) == '//'
         aadd( cmntd_, cOrg )
         LOOP
      ENDIF
      /* Lists - Later */
      IF '<' $ s
         aadd( dummy_, cOrg )
         LOOP
      ENDIF

      IF ParseVariables( s, cWidget, @txt_, @doc_, enum_, func_ )
         nCnvrtd++
      ELSE
         aadd( dummy_, cOrg )
      ENDIF
   NEXT

   /* Assemble components */
   IF .t.  /* !empty( txt_ ) */

      /* Pull .cpp copyright text */
      BuildHeader( @cpp_, 0 )

      /* Place ENUM definitions into the source */
      #if 0
      IF !empty( enums_ )
         aadd( cpp_, '/*' )
         aeval( enums_, {|e| IF( !empty( e ), aadd( cpp_, ' *  ' + e ), NIL ) } )
         aadd( cpp_, ' */ ' )
         aadd( cpp_, '' )
      ENDIF
      #endif

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
      BuildFooter( @cpp_ )

      /* Build Document File */
      IF !empty( doc_ )
         BuildDocument( cWidget, doc_, cPathDoc )
      ENDIF

      /* Build Class PRG Source */
      cFileCpp := cPathOut + s_PathSep + 'hbqt_'+ lower( cWidget ) +'.cpp'
      CreateTarget( cFileCpp, cpp_ )

      /* Build CLASS */
      IF !empty( cls_ )
         Build_Class( cWidget, cls_, doc_, cPathOut )
         cPRG := cWidget
      ELSE
         cPRG := ''
      ENDIF
      cCPP := cWidget
   ENDIF

   RETURN { cCPP, cPRG }

/*----------------------------------------------------------------------*/

#define PRT_L_CONST    1
#define PRT_L_FAR      2
#define PRT_L_AND      3
#define PRT_L_VIRT     4
#define PRT_NAME       5
#define PRT_CAST       6
#define PRT_DEFAULT    7
#define PRT_RAW        8
#define PRT_BODY       9
#define PRT_DOC       10
#define PRT_BODY_PRE  11

#define PRT_ATTRB_MAX 11

#define THIS_PROPER( s )   ( upper( left( s,1 ) ) + substr( s,2 ) )

STATIC FUNCTION ParseProto( cProto, cWidget, txt_, doc_, aEnum, func_ )
   LOCAL aRet, aA, aArgus, aArg, aPar, aPre
   LOCAL n, nn, nHBIdx
   LOCAL cPre, cPar, cRet, cFun, cParas, cDocs, cCmd, cPas, s, ss
   LOCAL cWdg, cCmn, cPrgRet, cHBFunc, cHBIdx, cDocNM
   LOCAL lSuccess
   LOCAL cIntegers := 'int,qint16,qint32,qint64,quint16,quint32,quint64,qlonglong,qulonglong,QRgb,QChar'

   cParas := ''
   cDocs  := ''
   //aArgus := {}

   aRet := {}; aArgus := {}
   n := at( '(', cProto )
   IF n > 0
      nn := rat( ')', cProto )
      IF nn > 0
         /* Pull out pre-mid-post components */
         cPre := alltrim( substr( cProto,   1, n-1    ) )
         cPar := alltrim( substr( cProto, n+1, nn-1-n ) )
         cPas := alltrim( substr( cProto, nn+1 ) )

         /* parse cPre, it has two components */
         n := rat( ' ', cPre )
         IF n > 0   /* And it must be, otherwise it is constructor function which we write in <CODE>section */
            cFun := alltrim( substr( cPre, n+1    ) )
            cRet := alltrim( substr( cPre, 1, n-1 ) )
         ELSE
            cFun := cPre
            cRet := ''
         ENDIF

         aRet := array( PRT_ATTRB_MAX )

         aRet[ PRT_L_CONST ] := 'const'   $ cRet  .or. 'const'   $ cPas
         aRet[ PRT_L_AND   ] := '&'       $ cRet
         aRet[ PRT_L_FAR   ] := '*'       $ cRet
         aRet[ PRT_L_VIRT  ] := 'virtual' $ cRet

         cRet := strtran( cRet, 'const '  , '' )
         cRet := strtran( cRet, '& '      , '' )
         cRet := strtran( cRet, '&'       , '' )
         cRet := strtran( cRet, '* '      , '' )
         cRet := strtran( cRet, '*'       , '' )
         cRet := strtran( cRet, 'virtual ', '' )

         /* Normalize */
         cRet := alltrim( cRet )
         n := at( ' ', cRet )
         IF n > 0
            aRet[ PRT_CAST ] := substr( cRet, 1, n-1 )
         ELSE
            aRet[ PRT_CAST ] := cRet
         ENDIF
         aRet[ PRT_NAME ] := aRet[ PRT_CAST ]

         IF ascan( aEnum, {|e| IF( empty( e ), .f., e == aRet[ PRT_CAST ] ) } ) > 0
            aRet[ PRT_CAST ] := cWidget + '::' + aRet[ PRT_CAST ]
         ENDIF

         /* Parse arguments */
         aArg := hb_ATokens( cPar, ',' )
         /* Normalize */
         aeval( aArg, {|e,i| aArg[ i ] := alltrim( e ) } )

         cParas := ''
         cDocs  := ''

         /* TO hold arguments by reference */
         aPre := {}

         FOR EACH cPre IN aArg
            aPar := array( PRT_ATTRB_MAX )
            aA := aPar

            aA[ PRT_RAW     ] := cPre

            aA[ PRT_L_CONST ] := 'const'   $ cPre
            aA[ PRT_L_AND   ] := '&'       $ cPre
            aA[ PRT_L_FAR   ] := '*'       $ cPre
            aA[ PRT_L_VIRT  ] := 'virtual' $ cPre
            /* Check if default value is defined */
            n := at( '=', cPre )
            IF n > 0
               aA[ PRT_DEFAULT ] := alltrim( substr( cPre, n+1 ) )
               cPre := substr( cPre, 1, n-1 )
            ENDIF
            /* Normalize */
            cPre := strtran( cPre, 'const '  , ''  )
            cPre := strtran( cPre, '& '      , ''  )
            cPre := strtran( cPre, '&'       , ''  )
            cPre := strtran( cPre, '* '      , ''  )
            cPre := strtran( cPre, '*'       , ''  )
            cPre := strtran( cPre, 'virtual ', ''  )
            cPre := strtran( cPre, '   '     , ' ' )
            cPre := strtran( cPre, '  '      , ' ' )

            cPre := alltrim( cPre )
            /* left may be two elements, name and cast */
            n := at( ' ', cPre )
            IF n > 0
               aA[ PRT_CAST ] := substr( cPre, 1, n-1 )
               aA[ PRT_NAME ] := substr( cPre, n+1 )
            ELSE
               aA[ PRT_CAST ] := cPre
               aA[ PRT_NAME ] := cPre
            ENDIF

            IF ascan( aEnum, {|e| IF( empty( e ), .f., e == aA[ PRT_CAST ] ) } ) > 0
               aA[ PRT_CAST ] := cWidget + '::' + aA[ PRT_CAST ]
            ENDIF

            /* Add to main array */
            aadd( aArgus, aA )

            nHBIdx := cPre:__enumIndex() + 1
            cHBIdx := hb_ntos( nHBIdx )
            cDocNM := THIS_PROPER( aA[ PRT_NAME ] )

            DO CASE
            CASE aA[ PRT_CAST ] == 'T'
               aA[ PRT_BODY ] := 'hb_param( '+ cHBIdx +', HB_IT_ANY )'
               aA[ PRT_DOC  ] := 'x'+ cDocNM

            /* Values by reference */
            CASE aA[ PRT_CAST ] $ cIntegers .and. aA[ PRT_L_FAR ]
               aadd( aPre, { 'int i'+cDocNM+' = 0;', nHBIdx, 'i'+ cDocNM, 'hb_storni' } )
               aA[ PRT_BODY ] := '&i'+cDocNM
               aA[ PRT_DOC  ] := '@n'+ cDocNM

            CASE aA[ PRT_CAST ] $ cIntegers
               s := 'hb_parni( '+ cHBIdx +' )'
               IF !empty( aA[ PRT_DEFAULT ] ) .AND. !( aA[ PRT_DEFAULT ] == "0" )
                  aA[ PRT_BODY ] := '( HB_ISNUM( '+cHBIdx+' ) ? ' + s + ' : ' + aA[ PRT_DEFAULT ] + ' )'
               ELSE
                  aA[ PRT_BODY ] := s
               ENDIF
               aA[ PRT_DOC  ] := 'n'+ cDocNM

            CASE aA[ PRT_CAST ] $ 'double,qreal' .and. aA[ PRT_L_FAR ]
               aadd( aPre, { 'qreal qr'+cDocNM+' = 0;', nHBIdx, 'qr'+ cDocNM, 'hb_stornd'  } )
               aA[ PRT_BODY ] := '&qr'+cDocNM
               aA[ PRT_DOC  ] := '@n'+ cDocNM

            CASE aA[ PRT_CAST ] $ 'double,qreal,float'
               aA[ PRT_BODY ] := 'hb_parnd( '+ cHBIdx +' )'
               aA[ PRT_DOC  ] := 'n'+ cDocNM

            CASE aA[ PRT_CAST ] == 'uchar'
               aA[ PRT_BODY ] := '( char ) hb_parni( '+ cHBIdx +' )'
               aA[ PRT_DOC  ] := 'n'+ cDocNM

            CASE ( '::' $ aA[ PRT_CAST ] ) .and. aA[ PRT_L_FAR ]
               aadd( aPre, { aA[ PRT_CAST ]+' i'+cDocNM+';', nHBIdx, 'i'+ cDocNM, 'hb_storni' } )
               aA[ PRT_BODY ] := '&i'+cDocNM
               aA[ PRT_DOC  ] := '@n'+ cDocNM

            CASE ( '::' $ aA[ PRT_CAST ] )
               s := '( '+ aA[ PRT_CAST ] +' ) hb_parni( '+ cHBIdx +' )'
               IF !empty( aA[ PRT_DEFAULT ] ) .AND. !( aA[ PRT_DEFAULT ] == "0" )
                  IF ascan( aEnum, aA[ PRT_DEFAULT ] ) > 0
                     ss := cWidget+'::'+aA[ PRT_DEFAULT ]
                  ELSE
                     ss := IF( '::' $ aA[ PRT_DEFAULT ], aA[ PRT_DEFAULT ], ;
                        IF( isDigit( left( aA[ PRT_DEFAULT ], 1 ) ), aA[ PRT_DEFAULT ], cWidget+'::'+aA[ PRT_DEFAULT ] ) )
                  ENDIF
                  ss := '( '+ aA[ PRT_CAST ] +' ) '+ss
                  aA[ PRT_BODY ] := '( HB_ISNUM( '+cHBIdx+' ) ? ' + s + ' : ' + ss + ' )'
               ELSE
                  aA[ PRT_BODY ] := s
               ENDIF
               aA[ PRT_DOC  ] := 'n'+ cDocNM

            CASE aA[ PRT_CAST ] == 'bool' .and. aA[ PRT_L_FAR ]
               aadd( aPre, { 'bool i'+cDocNM+' = 0;', nHBIdx, 'i'+ cDocNM, 'hb_stornl' } )
               aA[ PRT_BODY ] := '&i'+cDocNM
               aA[ PRT_DOC  ] := '@l'+ cDocNM

            CASE aA[ PRT_CAST ] == 'bool'
               aA[ PRT_BODY ] := 'hb_parl( '+ cHBIdx +' )'
               aA[ PRT_DOC  ] := 'l'+ cDocNM

            CASE aA[ PRT_CAST ] == 'QString'
               aA[ PRT_BODY ] := 'hbqt_par_QString( '+ cHBIdx +' )'
               aA[ PRT_DOC  ] := 'c'+ cDocNM

            CASE aA[ PRT_CAST ] == 'FT_Face'
               aA[ PRT_BODY ] := 'hbqt_par_FT_Face( '+ cHBIdx +' )'
               aA[ PRT_DOC  ] := 'c'+ cDocNM

            CASE aA[ PRT_CAST ] == 'QIcon'
               aA[ PRT_BODY ] := 'QIcon( hbqt_par_QString( '+ cHBIdx +' ) )'
               aA[ PRT_DOC  ] := 'c'+ cDocNM

            CASE aA[ PRT_L_FAR ]
               aA[ PRT_BODY ] := 'hbqt_par_' + aA[ PRT_CAST ] + '( ' + cHBIdx + ' )'
               aA[ PRT_DOC  ] := 'p'+ cDocNM

            CASE aA[ PRT_L_AND ] .and. aA[ PRT_L_CONST ]
               s := '*hbqt_par_' + aA[ PRT_CAST ] + '( ' + cHBIdx + ' )'
               IF !empty( aA[ PRT_DEFAULT ] ) .and. ( '(' $ aA[ PRT_DEFAULT ] )
                  aA[ PRT_BODY ] := '( HB_ISNIL( '+cHBIdx+' ) ? ' + aA[ PRT_DEFAULT ] +' : '+ s +' )'
               ELSE
                  aA[ PRT_BODY ] := s
               ENDIF
               aA[ PRT_DOC  ] := 'p'+ cDocNM
               #if 0
               aA[ PRT_BODY ] := '*hbqt_par_' + aA[ PRT_CAST ] + '( ' + cHBIdx + ' )'
               aA[ PRT_DOC  ] := 'p'+ cDocNM
               #endif

            CASE aA[ PRT_L_AND ]
               #if 0
                  aA[ PRT_BODY ] := '( '+ aA[ PRT_CAST ]+'& )' + 'hbqt_par_' + aA[ PRT_CAST ] + '( ' + cHBIdx + ' )'
               #else
                  aA[ PRT_BODY ] := '*hbqt_par_' + aA[ PRT_CAST ] + '( ' + cHBIdx + ' )'
               #endif
               aA[ PRT_DOC  ] := 'p'+ cDocNM

            OTHERWISE
               aA[ PRT_BODY ] := ''
               aA[ PRT_DOC  ] := ''

            ENDCASE

            cParas += aA[ PRT_BODY ] +', '
            cDocs  += aA[ PRT_DOC  ] +', '
         NEXT

         IF right( cParas, 2 ) == ', '
            cParas := substr( cParas, 1, len( cParas ) - 2 )
            cDocs  := substr( cDocs , 1, len( cDocs  ) - 2 )
         ENDIF

         /* Build complete code line */
         IF .t.
            aA     := aRet
            cWdg   := 'hbqt_par_'+cWidget+'( 1 )->'
            cParas := '( '+ cParas +' )'
            cCmn   := cWdg + cFun + cParas
            cDocNM := THIS_PROPER( aA[ PRT_NAME ] )

            DO CASE
            CASE aA[ PRT_CAST ] == 'T'
               #if 0
               cCmd := 'hb_ret( '+ cCmn +' )'
               cPrgRet := 'x'+cDocNM
               #endif
               cCmd := 'hb_retptr( '+ cCmn +' )'
               cPrgRet := 'p'+cDocNM

            CASE aA[ PRT_CAST ] == 'void'
               cCmd := cCmn
               cPrgRet := 'NIL'

            CASE aA[ PRT_CAST ] $ cIntegers
               cCmd := 'hb_retni( '+ cCmn +' )'
               cPrgRet := 'n'+cDocNM

            CASE aA[ PRT_CAST ] $ 'double,qreal'
               cCmd := 'hb_retnd( '+ cCmn +' )'
               cPrgRet := 'n'+cDocNM

            CASE ( '::' $ aA[ PRT_CAST ] )
               cCmd := 'hb_retni( ( '+ aA[ PRT_CAST ] +' ) ' + cCmn +' )'
               cPrgRet := 'n'+cDocNM

            CASE aA[ PRT_CAST ] == 'bool'
               cCmd := 'hb_retl( '+ cCmn +' )'
               cPrgRet := 'l'+cDocNM

            CASE aA[ PRT_CAST ] == 'char' .AND. aA[ PRT_L_FAR ]
               cCmd := 'hb_retc( '+ cCmn +' )'
               cPrgRet := 'c'+cDocNM

            CASE aA[ PRT_CAST ] == 'char'
               cCmd := 'hb_retni( '+ cCmn +' )'
               cPrgRet := 'c'+cDocNM

            CASE aA[ PRT_CAST ] == 'QString'
               cCmd := 'hb_retc( '+ cCmn +'.toLatin1().data()' +' )'
               cPrgRet := 'c'+cDocNM

            CASE aA[ PRT_CAST ] == 'FT_Face'
               cCmd := 'hb_retc( '+ cCmn +' )'
               cPrgRet := 'c'+cDocNM

            CASE aA[ PRT_L_FAR ]
               cCmd := 'hb_retptr( ( '+ aA[ PRT_CAST ] + '* ) ' + cCmn + ' )'
               cPrgRet := 'p'+cDocNM

            CASE aA[ PRT_L_AND ] .and. aA[ PRT_L_CONST ]
               #if 0
                  cCmd := 'hb_retptr( &( ( '+ aA[ PRT_CAST ] + '& ) ' + cCmn + ' ) )'
               #else
                  cCmd := 'hb_retptr( new '+ aA[ PRT_CAST ] + '( ' + cCmn + ' ) )'
               #endif
               cPrgRet := 'p'+cDocNM

            CASE aA[ PRT_L_CONST ]
               #if 0
                  cCmd := 'hb_retptr( &( ( '+ aA[ PRT_CAST ] + ' ) ' + cCmn + ' ) )'
               #else
                  cCmd := 'hb_retptr( new '+ aA[ PRT_CAST ] + '( ' + cCmn + ' ) )'
               #endif
               cPrgRet := 'p'+cDocNM

            CASE aA[ PRT_L_AND ]
               cCmd := 'hb_retptr( ( '+ aA[ PRT_CAST ] + '* ) ' + cCmn + ' )'
               cPrgRet := 'p'+cDocNM
#if 0
            CASE aA[ PRT_CAST ] == 'HFONT'
               cCmd := 'hb_retptr( ( HFONT ) '+ cCmn +' )'
               cPrgRet := 'h'+cDocNM

            CASE aA[ PRT_CAST ] == 'HDC'
               cCmd := 'hb_retptr( ( HDC ) '+ cCmn +' )'
               cPrgRet := 'h'+cDocNM

            CASE aA[ PRT_CAST ] == 'WId'
               cCmd := 'hb_retptr( ( HWND ) '+ cCmn +' )'
               cPrgRet := 'h'+cDocNM

            CASE aA[ PRT_CAST ] == 'HRGN'
               cCmd := 'hb_retptr( ( HRGN ) '+ cCmn +' )'
               cPrgRet := 'h'+cDocNM

            CASE aA[ PRT_CAST ] == 'QRect'
               cCmd := 'hbqt_ret_QRect( '+ cCmn +' )'
               cPrgRet := 'a'+cDocNM

            CASE aA[ PRT_CAST ] == 'QRectF'
               cCmd := 'hbqt_ret_QRectF( '+ cCmn +' )'
               cPrgRet := 'a'+cDocNM

            CASE aA[ PRT_CAST ] == 'QSize'
               cCmd := 'hbqt_ret_QSize( '+ cCmn +' )'
               cPrgRet := 'a'+cDocNM

            CASE aA[ PRT_CAST ] == 'QSizeF'
               cCmd := 'hbqt_ret_QSizeF( '+ cCmn +' )'
               cPrgRet := 'a'+cDocNM

            CASE aA[ PRT_CAST ] == 'QPoint'
               cCmd := 'hbqt_ret_QPoint( '+ cCmn +' )'
               cPrgRet := 'a'+cDocNM

            CASE aA[ PRT_CAST ] == 'QPointF'
               cCmd := 'hbqt_ret_QPointF( '+ cCmn +' )'
               cPrgRet := 'a'+cDocNM
#endif
            OTHERWISE
               /* No attribute is attached to return value */
               IF left( aA[ PRT_CAST ], 1 ) == 'Q'
                  #if 0
                     cCmd := 'hb_retptr( &( ( '+ aA[ PRT_CAST ] + '& ) ' + cCmn + ' ) )'
                  #else
                     cCmd := 'hb_retptr( new '+ aA[ PRT_CAST ] + '( ' + cCmn + ' ) )'
                  #endif
                  cPrgRet := 'p'+cDocNM

               ELSE
                  OutStd( '<<< '+cProto + ' | ' + aA[ PRT_CAST ]+' >>>'  + s_NewLine )
                  cCmd := ''
                  cPrgRet := ''

               ENDIF

            ENDCASE

            IF !empty( cCmd )
               cCmd := strtran( cCmd, '(  )', '()' ) +';'
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   IF ( lSuccess := !empty( cCmd ) )
      IF ( n := ascan( func_, {|e_| e_[ 1 ] == cFun } ) ) > 0
         func_[ n,2 ]++
         cHBFunc := cFun + '_' + hb_ntos( func_[ n,2 ] )
      ELSE
         cHBFunc := cFun
         aadd( func_, { cFun, 0 } )
      ENDIF

      aadd( txt_, "/*" )
      aadd( txt_, " * "+ strtran( cProto, chr(13), '' ) )
      aadd( txt_, " */" )

      aadd( txt_, "HB_FUNC( QT_" + upper( cWidget ) +"_"+ upper( cHBFunc ) +" )" )
      aadd( txt_, "{"  )

      /* Insert parameters by reference */
      IF !empty( aPre )
         FOR n := 1 TO len( aPre )
            aadd( txt_, "   "+ aPre[ n, 1 ] )
         NEXT
         aadd( txt_, ""   )
      ENDIF

      /* One line function body */
      aadd( txt_, "   "+ cCmd )

      /* Return values back to PRG */
      IF !empty( aPre )
         aadd( txt_, ""   )
         FOR n := 1 TO len( aPre )
            aadd( txt_, "   "+ aPre[ n,4 ]+"( " + aPre[ n,3 ] +", "+ hb_ntos( aPre[ n,2 ] ) +" );" )
         NEXT
      ENDIF

      aadd( txt_, "}"  )
      aadd( txt_, ""   )

      aadd( doc_, 'Qt_'+ cWidget + '_' + cHBFunc +'( p'+ cWidget + ;
                        IF( empty( cDocs ), '', ', '+ cDocs ) +' ) -> '+ cPrgRet )
      aadd( doc_, '' )
   ENDIF

   RETURN lSuccess

/*----------------------------------------------------------------------*/

STATIC FUNCTION ParseVariables( cProto, cWidget, txt_, doc_, aEnum, func_ )
   LOCAL aRet, aA, aArgus, aArg, aPar, aPre
   LOCAL n, nn, nHBIdx
   LOCAL cPre, cPar, cRet, cFun, cParas, cDocs, cCmd, cPas, s, ss
   LOCAL cWdg, cCmn, cPrgRet, cHBFunc, cHBIdx, cDocNM
   LOCAL lSuccess
   LOCAL cIntegers := 'int,qint16,qint32,qint64,quint16,quint32,quint64,qlonglong,qulonglong,QRgb,QChar'

   cParas := ''
   cDocs  := ''

   aRet := {}; aArgus := {}
   n := at( ' ', cProto )
   IF n > 0
      IF .t.
         cRet := alltrim( substr( cProto, 1, n-1 ) )
         cFun := alltrim( substr( cProto, n+1    ) )

         aRet := array( PRT_ATTRB_MAX )

         /* Normalize */
         aRet[ PRT_CAST ] := cRet
         aRet[ PRT_NAME ] := aRet[ PRT_CAST ]

         IF ascan( aEnum, {|e| IF( empty( e ), .f., e == aRet[ PRT_CAST ] ) } ) > 0
            aRet[ PRT_CAST ] := cWidget + '::' + aRet[ PRT_CAST ]
         ENDIF

         /* Build complete code line */
         IF .t.
            aA     := aRet
            cWdg   := 'hbqt_par_'+ cWidget +'( 1 )->'
            cCmn   := cWdg + cFun
            cDocNM := THIS_PROPER( aA[ PRT_NAME ] )

            DO CASE
            CASE aA[ PRT_CAST ] == 'T'
               cCmd := 'hb_ret( '+ cCmn +' )'
               cPrgRet := 'x'+cDocNM

            CASE aA[ PRT_CAST ] $ cIntegers
               cCmd := 'hb_retni( '+ cCmn +' )'
               cPrgRet := 'n'+cDocNM

            CASE aA[ PRT_CAST ] $ 'double,qreal'
               cCmd := 'hb_retnd( '+ cCmn +' )'
               cPrgRet := 'n'+cDocNM

            CASE ( '::' $ aA[ PRT_CAST ] )
               cCmd := 'hb_retni( ( '+ aA[ PRT_CAST ] +' ) ' + cCmn +' )'
               cPrgRet := 'n'+cDocNM

            CASE aA[ PRT_CAST ] == 'bool'
               cCmd := 'hb_retl( '+ cCmn +' )'
               cPrgRet := 'l'+cDocNM

            CASE aA[ PRT_CAST ] == 'char*'
               cCmd := 'hb_retc( '+ cCmn +' )'
               cPrgRet := 'c'+cDocNM

            CASE aA[ PRT_CAST ] == 'char'
               cCmd := 'hb_retni( '+ cCmn +' )'
               cPrgRet := 'c'+cDocNM

            CASE aA[ PRT_CAST ] == 'QString'
               cCmd := 'hb_retc( '+ cCmn +'.toLatin1().data()' +' )'
               cPrgRet := 'c'+cDocNM

            CASE aA[ PRT_CAST ] == 'FT_Face'
               cCmd := 'hb_retc( '+ cCmn +' )'
               cPrgRet := 'c'+cDocNM

            OTHERWISE
               /* No attribute is attached to return value */
               IF left( aA[ PRT_CAST ], 1 ) == 'Q'
                  cCmd := 'hb_retptr( new '+ aA[ PRT_CAST ] + '( ' + cCmn + ' ) )'
                  cPrgRet := 'p'+cDocNM

               ELSE
                  OutStd( '<<< '+cProto + ' | ' + aA[ PRT_CAST ]+' >>>'  + s_NewLine )
                  cCmd := ''
                  cPrgRet := ''

               ENDIF

            ENDCASE

            IF !empty( cCmd )
               cCmd := strtran( cCmd, '(  )', '()' ) +';'
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   IF ( lSuccess := !empty( cCmd ) )
      IF ( n := ascan( func_, {|e_| e_[ 1 ] == cFun } ) ) > 0
         func_[ n,2 ]++
         cHBFunc := cFun + '_' + hb_ntos( func_[ n,2 ] )
      ELSE
         cHBFunc := cFun
         aadd( func_, { cFun, 0 } )
      ENDIF

      aadd( txt_, "/*" )
      aadd( txt_, " * "+ strtran( cProto, chr(13), '' ) )
      aadd( txt_, " */" )

      aadd( txt_, "HB_FUNC( QT_" + upper( cWidget ) +"_"+ upper( cHBFunc ) +" )" )
      aadd( txt_, "{"  )

      /* Insert parameters by reference */
      IF !empty( aPre )
         FOR n := 1 TO len( aPre )
            aadd( txt_, "   "+ aPre[ n, 1 ] )
         NEXT
         aadd( txt_, ""   )
      ENDIF

      /* One line function body */
      aadd( txt_, "   "+ cCmd )

      /* Return values back to PRG */
      IF !empty( aPre )
         aadd( txt_, ""   )
         FOR n := 1 TO len( aPre )
            aadd( txt_, "   "+ aPre[ n,4 ]+"( " + aPre[ n,3 ] +", "+ hb_ntos( aPre[ n,2 ] ) +" );" )
         NEXT
      ENDIF

      aadd( txt_, "}"  )
      aadd( txt_, ""   )

      aadd( doc_, 'Qt_'+ cWidget + '_' + cHBFunc +'( p'+ cWidget + ;
                        IF( empty( cDocs ), '', ', '+ cDocs ) +' ) -> '+ cPrgRet )
      aadd( doc_, '' )
   ENDIF

   RETURN lSuccess

/*----------------------------------------------------------------------*/

STATIC FUNCTION BuildHeader( txt_, nMode )

   aadd( txt_, "/*"                                                                            )
   aadd( txt_, " * $Id$"                     )
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
   LOCAL cHlp := ''

   cHlp += ''                                                                               + s_NewLine
   cHlp += 'SYNTAX:'                                                                        + s_NewLine
   cHlp += '   hbqtgen.exe [Options] [[@]<QtProjectFile.qtp>] [<QtHeaderFile.qth, ...>]'    + s_NewLine
   cHlp += ''                                                                               + s_NewLine
   cHlp += 'Options:'                                                                       + s_NewLine
   cHlp += '   -O<OutputPath>   [ e.g. c:\harbour\contrib\hbqt ]        [D] Current folder' + s_NewLine
   cHlp += '   -I<InputPath>    [ e.g. c:\harbour\contrib\hbqt\protos ] [D] Current folder' + s_NewLine
   cHlp += '   -D<DocFilesPath> [ e.g. c:\harbour\contrib\hbqt\doc    ] [D] Current folder' + s_NewLine
   cHlp += ' '                                                                              + s_NewLine
   cHlp += '   -c<compile>      If QT env is set, attempts to compile resulting .cpp'       + s_NewLine
   cHlp += ''                                                                               + s_NewLine

   OutStd( cHlp )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION DispLogo()
   LOCAL cHlp := ''

   cHlp += ''                                                        + s_NewLine
   cHlp += "Harbour Source Gennerator for QT " + HBRawVersion()      + s_NewLine
   cHlp += "Copyright (c) 2009, Pritpal Bedi <pritpal@vouchcac.com>" + s_NewLine
   cHlp += "http://www.harbour-project.org/"                         + s_NewLine
   cHlp += ''                                                        + s_NewLine

   OutStd( cHlp )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour ", "" )

/*----------------------------------------------------------------------*/

STATIC FUNCTION BuildDocument( cWidget, doc_, cPathDoc )
   LOCAL cFile := cPathDoc + s_PathSep + cWidget +'.txt'
   LOCAL dcc_:={}

   BuildHeader( @dcc_ )

   aadd( dcc_, '/* '  )
   aadd( dcc_, ' *      HBQtGen v1.0 - Harbour Callable Wrappers Generator for QT v4.5      ' )
   aadd( dcc_, ' * '  )
   aadd( dcc_, ' * Please do not modify this document as it is subject to change in future. ' )
   aadd( dcc_, ' * '  )
   aadd( dcc_, ' *                   Pritpal Bedi <pritpal@vouchcac.com>                    ' )
   aadd( dcc_, ' * '  )
   aadd( dcc_, ' *                          '+dtoc( date() ) + ' - ' + time()                 )
   aadd( dcc_, ' * '  )
   aadd( dcc_, ' */ ' )
   aadd( dcc_, '   '  )
   aadd( dcc_, '   '  )

   aeval( doc_, {|e| aadd( dcc_, e ) } )

   aadd( dcc_, ' '    )
   aadd( dcc_, "/*----------------------------------------------------------------------*/"   )
   aadd( dcc_, ' '    )
   aadd( dcc_, ' '    )

   RETURN CreateTarget( cFile, dcc_ )

/*----------------------------------------------------------------------*/

STATIC FUNCTION CreateTarget( cFile, txt_ )
   LOCAL hHandle := fcreate( cFile )

   OutStd( "Creating: " + cFile + hb_osNewLine() )

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
   LOCAL s, n, cM, ss, cCall, sm
   LOCAL nLen := len( cWidget )
   LOCAL txt_  :={}

   BuildHeader( @txt_, 1 )

   aadd( txt_, '' )

   n := ascan( cls_, {|e_| left( lower( e_[ 1 ] ),7 ) == 'inherit' .and. !empty( e_[ 2 ] ) } )
   s := 'CREATE CLASS '+ cWidget + IF( n > 0, ' INHERIT ' + cls_[ n,2 ], '' )

   aadd( txt_, s  )
   aadd( txt_, '' )
   aadd( txt_, '   VAR     pParent'  )
   aadd( txt_, '   VAR     pPtr'     )
   aadd( txt_, '' )
   aadd( txt_, '   METHOD  New()'    )
   aadd( txt_, '' )

   /* Populate METHODS */
   FOR EACH s IN doc_
      n := at( '-> ', s )
      IF n > 0
         s     := substr( s, 1, n-1 )
         s     := strtran( s, '@', '' )    /* Just in Case */
         s     := strtran( s, '::', '_' )  /* Just in Case */

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

   #if 0
   n  := ascan( cls_, {|e_| lower( e_[ 1 ] ) == 'new' } )
   #endif
   cM := 'New( pParent )'

   aadd( txt_, 'METHOD '+ cM + ' CLASS '+ cWidget )
   aadd( txt_, '' )
   aadd( txt_, '   ::pParent := pParent' )
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
   LOCAL cFile := cPathOut + s_PathSep + 'Makefile_gen'
   LOCAL txt_:={}
   LOCAL s

   aadd( txt_, "#                                                       " )
   aadd( txt_, "# $Id$" )
   aadd( txt_, "#                                                       " )
   aadd( txt_, "                                                        " )
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
   aadd( txt_, "                                                        " )

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
   aadd( txt_, "#include <qglobal.h>                                                                          " )
   aadd( txt_, "                                                                                              " )
   aadd( txt_, "#include <QtGui/QTextDocumentFragment>                                                        " )
   aadd( txt_, "#include <QtGui/QTextDocument>                                                                " )
   aadd( txt_, "#include <QtGui/QTextBlock>                                                                   " )
   aadd( txt_, "#include <QtGui/QTextCursor>                                                                  " )
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
   aadd( txt_, "#define hbqt_par_QDesktopWidget( n )         ( ( QDesktopWidget* ) hb_parptr( n ) )           " )
   aadd( txt_, "#define hbqt_par_QFontInfo( n )              ( ( QFontInfo* ) hb_parptr( n ) )                " )
   aadd( txt_, "#define hbqt_par_QDir( n )                   ( ( QDir* ) hb_parptr( n ) )                     " )
   aadd( txt_, "#define hbqt_par_QDockWidget( n )            ( ( QDockWidget* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QGridLayout( n )            ( ( QGridLayout* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QHeaderView( n )            ( ( QHeaderView* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QListWidget( n )            ( ( QListWidget* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QListWidgetItem( n )        ( ( QListWidgetItem* ) hb_parptr( n ) )          " )
   aadd( txt_, "#define hbqt_par_QTimer( n )                 ( ( QTimer* ) hb_parptr( n ) )                   " )
   aadd( txt_, "#define hbqt_par_QUrl( n )                   ( ( QUrl* ) hb_parptr( n ) )                     " )
   aadd( txt_, "#define hbqt_par_QWebPage( n )               ( ( QWebPage* ) hb_parptr( n ) )                 " )
   aadd( txt_, "#define hbqt_par_QNetworkAccessManager( n )  ( ( QNetworkAccessManager* ) hb_parptr( n ) )    " )
   aadd( txt_, "#define hbqt_par_QWebPluginFactory( n )      ( ( QWebPluginFactory* ) hb_parptr( n ) )        " )
   aadd( txt_, "#define hbqt_par_QContextMenuEvent( n )      ( ( QContextMenuEvent* ) hb_parptr( n ) )        " )
   aadd( txt_, "#define hbqt_par_QAxBase( n )                ( ( QAxBase* ) hb_parptr( n ) )                  " )
   aadd( txt_, "#define hbqt_par_IUnknown( n )               ( ( IUnknown* ) hb_parptr( n ) )                 " )
   aadd( txt_, "#define hbqt_par_QSignalMapper( n )          ( ( QSignalMapper* ) hb_parptr( n ) )            " )
   aadd( txt_, "#define hbqt_par_QSplashScreen( n )          ( ( QSplashScreen* ) hb_parptr( n ) )            " )
   aadd( txt_, "#define hbqt_par_QHttp( n )                  ( ( QHttp* ) hb_parptr( n ) )                    " )
   aadd( txt_, "#define hbqt_par_QFtp( n )                   ( ( QFtp* ) hb_parptr( n ) )                     " )
   aadd( txt_, "#define hbqt_par_QIODevice( n )              ( ( QIODevice* ) hb_parptr( n ) )                " )
   aadd( txt_, "#define hbqt_par_QTcpSocket( n )             ( ( QTcpSocket* ) hb_parptr( n ) )               " )
   aadd( txt_, "#define hbqt_par_QPainterPath( n )           ( ( QPainterPath* ) hb_parptr( n ) )             " )
   aadd( txt_, "#define hbqt_par_QTransform( n )             ( ( QTransform* ) hb_parptr( n ) )               " )
   aadd( txt_, "#define hbqt_par_QMatrix( n )                ( ( QMatrix* ) hb_parptr( n ) )                  " )
   aadd( txt_, "#define hbqt_par_QTextOption( n )            ( ( QTextOption* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QPicture( n )               ( ( QPicture* ) hb_parptr( n ) )                 " )
   aadd( txt_, "#define hbqt_par_QPixmap( n )                ( ( QPixmap* ) hb_parptr( n ) )                  " )
   aadd( txt_, "#define hbqt_par_QRegion( n )                ( ( QRegion* ) hb_parptr( n ) )                  " )
   aadd( txt_, "#define hbqt_par_QPolygon( n )               ( ( QPolygon* ) hb_parptr( n ) )                 " )
   aadd( txt_, "#define hbqt_par_QPolygonF( n )              ( ( QPolygonF* ) hb_parptr( n ) )                " )
   aadd( txt_, "#define hbqt_par_QVector( n )                ( ( QVector* ) hb_parptr( n ) )                  " )
   aadd( txt_, "#define hbqt_par_QImage( n )                 ( ( QImage* ) hb_parptr( n ) )                   " )
   aadd( txt_, "#define hbqt_par_QKeySequence( n )           ( ( QKeySequence* ) hb_parptr( n ) )             " )
   aadd( txt_, "#define hbqt_par_QSize( n )                  ( ( QSize* ) hb_parptr( n ) )                    " )
   aadd( txt_, "#define hbqt_par_QSizeF( n )                 ( ( QSizeF* ) hb_parptr( n ) )                   " )
   aadd( txt_, "#define hbqt_par_QModelIndex( n )            ( ( QModelIndex* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QVariant( n )               ( ( QVariant* ) hb_parptr( n ) )                 " )
   aadd( txt_, "#define hbqt_par_QSessionManager( n )        ( ( QSessionManager* ) hb_parptr( n ) )          " )
   aadd( txt_, "#define hbqt_par_QDate( n )                  ( ( QDate* ) hb_parptr( n ) )                    " )
   aadd( txt_, "#define hbqt_par_QTime( n )                  ( ( QTime* ) hb_parptr( n ) )                    " )
   aadd( txt_, "#define hbqt_par_QDateTime( n )              ( ( QDateTime* ) hb_parptr( n ) )                " )
   aadd( txt_, "#define hbqt_par_QTextCharFormat( n )        ( ( QTextCharFormat* ) hb_parptr( n ) )          " )
   aadd( txt_, "#define hbqt_par_QStringList( n )            ( ( QStringList* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QErrorMessage( n )          ( ( QErrorMessage* ) hb_parptr( n ) )            " )
   aadd( txt_, "#define hbqt_par_QByteArray( n )             ( ( QByteArray* ) hb_parptr( n ) )               " )
   aadd( txt_, "#define hbqt_par_QDataStream( n )            ( ( QDataStream* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QTextCursor( n )            ( ( QTextCursor* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QPalette( n )               ( ( QPalette* ) hb_parptr( n ) )                 " )
   aadd( txt_, "#define hbqt_par_QCursor( n )                ( ( QCursor* ) hb_parptr( n ) )                  " )
   aadd( txt_, "#define hbqt_par_QNetworkRequest( n )        ( ( QNetworkRequest* ) hb_parptr( n ) )          " )
   aadd( txt_, "#define hbqt_par_QTableWidgetSelectionRange( n ) ( ( QTableWidgetSelectionRange* ) hb_parptr( n ) ) " )
   aadd( txt_, "#define hbqt_par_QWSEvent( n )               ( ( QWSEvent* ) hb_parptr( n ) )                 " )
   aadd( txt_, "#define hbqt_par_QHttpRequestHeader( n )     ( ( QHttpRequestHeader* ) hb_parptr( n ) )       " )
   aadd( txt_, "#define hbqt_par_QNetworkProxy( n )          ( ( QNetworkProxy* ) hb_parptr( n ) )            " )
   aadd( txt_, "#define hbqt_par_QBitmap( n )                ( ( QBitmap* ) hb_parptr( n ) )                  " )
   aadd( txt_, "#define hbqt_par_QTextStream( n )            ( ( QTextStream* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QTextCodec( n )             ( ( QTextCodec* ) hb_parptr( n ) )               " )
   aadd( txt_, "#define hbqt_par_QEventLoop( n )             ( ( QEventLoop* ) hb_parptr( n ) )               " )
   aadd( txt_, "#define hbqt_par_QPaintEvent( n )            ( ( QPaintEvent* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QInputEvent( n )            ( ( QInputEvent* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QIcon( n )                  ( ( QIcon* ) hb_parc( n ) )                      " )
   aadd( txt_, "#define hbqt_par_QStyleOption( n )           ( ( QStyleOption* ) hb_parptr( n ) )             " )
   aadd( txt_, "#define hbqt_par_QStyleOptionComplex( n )    ( ( QStyleOptionComplex* ) hb_parptr( n ) )      " )
   aadd( txt_, "#define hbqt_par_QFontMetrics( n )           ( ( QFontMetrics* ) hb_parptr( n ) )             " )
   aadd( txt_, "#define hbqt_par_QStyleHintReturn( n )       ( ( QStyleHintReturn* ) hb_parptr( n ) )         " )
   aadd( txt_, "#define hbqt_par_QClipboard( n )             ( ( QClipboard* ) hb_parptr( n ) )               " )
   aadd( txt_, "#define hbqt_par_QMimeData( n )              ( ( QMimeData* ) hb_parptr( n ) )                " )
   aadd( txt_, "#define hbqt_par_QTextBlock( n )             ( ( QTextBlock* ) hb_parptr( n ) )               " )
   aadd( txt_, "#define hbqt_par_QTextBlockUserData( n )     ( ( QTextBlockUserData* ) hb_parptr( n ) )       " )
   aadd( txt_, "#define hbqt_par_QTextBrowser( n )           ( ( QTextBrowser* ) hb_parptr( n ) )             " )
   aadd( txt_, "#define hbqt_par_QTextFormat( n )            ( ( QTextFormat* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QTextBlockFormat( n )       ( ( QTextBlockFormat* ) hb_parptr( n ) )         " )
   aadd( txt_, "#define hbqt_par_QTextBlockGroup( n )        ( ( QTextBlockGroup* ) hb_parptr( n ) )          " )
   aadd( txt_, "#define hbqt_par_QTextBoundaryFinder( n )    ( ( QTextBoundaryFinder* ) hb_parptr( n ) )      " )
   aadd( txt_, "#define hbqt_par_QTextListFormat( n )        ( ( QTextListFormat* ) hb_parptr( n ) )          " )
   aadd( txt_, "#define hbqt_par_QTextFrameFormat( n )       ( ( QTextFrameFormat* ) hb_parptr( n ) )         " )
   aadd( txt_, "#define hbqt_par_QTextImageFormat( n )       ( ( QTextImageFormat* ) hb_parptr( n ) )         " )
   aadd( txt_, "#define hbqt_par_QTextTableFormat( n )       ( ( QTextTableFormat* ) hb_parptr( n ) )         " )
   aadd( txt_, "#define hbqt_par_QTextDocumentFragment( n )  ( ( QTextDocumentFragment* ) hb_parptr( n ) )    " )
   aadd( txt_, "#define hbqt_par_QTextDecoder( n )           ( ( QTextDecoder* ) hb_parptr( n ) )             " )
   aadd( txt_, "#define hbqt_par_QTextDecument( n )          ( ( QTextDocument* ) hb_parptr( n ) )            " )
   aadd( txt_, "#define hbqt_par_QTextDocumentWriter( n )    ( ( QTextDocumentWriter* ) hb_parptr( n ) )      " )
   aadd( txt_, "#define hbqt_par_QTextCursor( n )            ( ( QTextCursor* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QRegExp( n )                ( ( QRegExp* ) hb_parptr( n ) )                  " )
   aadd( txt_, "#define hbqt_par_QAbstractTextDocumentLayout( n ) ( ( QAbstractTextDocumentLayout* ) hb_parptr( n ) )" )
   aadd( txt_, "#define hbqt_par_QTextCodec( n )             ( ( QTextCodec* ) hb_parptr( n ) )               " )
   aadd( txt_, "#define hbqt_par_QTextEncoder( n )           ( ( QTextEncoder* ) hb_parptr( n ) )             " )
   aadd( txt_, "#define hbqt_par_QTextFragment( n )          ( ( QTextFragment* ) hb_parptr( n ) )            " )
   aadd( txt_, "#define hbqt_par_QTextFrame( n )             ( ( QTextFrame* ) hb_parptr( n ) )               " )
   aadd( txt_, "#define hbqt_par_QTextFrameFormat( n )       ( ( QTextFrameFormat* ) hb_parptr( n ) )         " )
   aadd( txt_, "#define hbqt_par_QTextImageFormat( n )       ( ( QTextImageFormat* ) hb_parptr( n ) )         " )
   aadd( txt_, "#define hbqt_par_QTextLength( n )            ( ( QTextLength* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QTextEngine( n )            ( ( QTextEngine* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QTextItem( n )              ( ( QTextItem* ) hb_parptr( n ) )                " )
   aadd( txt_, "#define hbqt_par_QTextInlineObject( n )      ( ( QTextInlineObject* ) hb_parptr( n ) )        " )
   aadd( txt_, "#define hbqt_par_QTextLayout( n )            ( ( QTextLayout* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QTextLength( n )            ( ( QTextLength* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QTextLine( n )              ( ( QTextLine* ) hb_parptr( n ) )                " )
   aadd( txt_, "#define hbqt_par_QGradient( n )              ( ( QGradient* ) hb_parptr( n ) )                " )
   aadd( txt_, "#define hbqt_par_QLinearGradient( n )        ( ( QLinearGradient* ) hb_parptr( n ) )          " )
   aadd( txt_, "#define hbqt_par_QRadialGradient( n )        ( ( QRadialGradient* ) hb_parptr( n ) )          " )
   aadd( txt_, "#define hbqt_par_QConicalGradient( n )       ( ( QConicalGradient* ) hb_parptr( n ) )         " )
   aadd( txt_, "#define hbqt_par_QGradientStops( n )         ( ( QGradientStops* ) hb_parptr( n ) )           " )
   aadd( txt_, "#define hbqt_par_QImageReader( n )           ( ( QImageReader* ) hb_parptr( n ) )             " )
   aadd( txt_, "#define hbqt_par_QImageWriter( n )           ( ( QImageWriter* ) hb_parptr( n ) )             " )
   aadd( txt_, "#define hbqt_par_QResource( n )              ( ( QResource* ) hb_parptr( n ) )                " )
   aadd( txt_, "#define hbqt_par_QSizePolicy( n )            ( ( QSizePolicy* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QSound( n )                 ( ( QSound* ) hb_parptr( n ) )                   " )
   aadd( txt_, "#define hbqt_par_QStandardItem( n )          ( ( QStandardItem* ) hb_parptr( n ) )            " )
   aadd( txt_, "#define hbqt_par_QStandardItemModel( n )     ( ( QStandardItemModel* ) hb_parptr( n ) )       " )
   aadd( txt_, "#define hbqt_par_QItemEditorFactory( n )     ( ( QItemEditorFactory* ) hb_parptr( n ) )       " )
   aadd( txt_, "#define hbqt_par_QStyledItemDelegate( n )    ( ( QStyledItemDelegate* ) hb_parptr( n ) )      " )
   aadd( txt_, "#define hbqt_par_QStyleOptionViewItem( n )   ( ( QStyleOptionViewItem* ) hb_parptr( n ) )     " )
   aadd( txt_, "#define hbqt_par_QStyleOptionButton( n )     ( ( QStyleOptionButton* ) hb_parptr( n ) )       " )
   aadd( txt_, "#define hbqt_par_QStyleOptionComboBox( n )   ( ( QStyleOptionComboBox* ) hb_parptr( n ) )     " )
   aadd( txt_, "#define hbqt_par_QStyleOptionDockWidget( n ) ( ( QStyleOptionDockWidget* ) hb_parptr( n ) )   " )
   aadd( txt_, "#define hbqt_par_QStyleOptionFocusRect( n )  ( ( QStyleOptionFocusRect* ) hb_parptr( n ) )    " )
   aadd( txt_, "#define hbqt_par_QStyleOptionFrame( n )      ( ( QStyleOptionFrame* ) hb_parptr( n ) )        " )
   aadd( txt_, "#define hbqt_par_QStyleOptionGroupBox( n )   ( ( QStyleOptionGroupBox* ) hb_parptr( n ) )     " )
   aadd( txt_, "#define hbqt_par_QStyleOptionHeader( n )     ( ( QStyleOptionHeader* ) hb_parptr( n ) )       " )
   aadd( txt_, "#define hbqt_par_QStyleOptionMenuItem( n )   ( ( QStyleOptionMenuItem* ) hb_parptr( n ) )     " )
   aadd( txt_, "#define hbqt_par_QStyleOptionProgressBar( n )( ( QStyleOptionProgressBar* ) hb_parptr( n ) )  " )
   aadd( txt_, "#define hbqt_par_QStyleOptionSizeGrip( n )   ( ( QStyleOptionSizeGrip* ) hb_parptr( n ) )     " )
   aadd( txt_, "#define hbqt_par_QStyleOptionSlider( n )     ( ( QStyleOptionSlider* ) hb_parptr( n ) )       " )
   aadd( txt_, "#define hbqt_par_QStyleOptionSpinBox( n )    ( ( QStyleOptionSpinBox* ) hb_parptr( n ) )      " )
   aadd( txt_, "#define hbqt_par_QStyleOptionTab( n )        ( ( QStyleOptionTab* ) hb_parptr( n ) )          " )
   aadd( txt_, "#define hbqt_par_QStyleOptionTabBarBase( n ) ( ( QStyleOptionTabBarBase* ) hb_parptr( n ) )   " )
   aadd( txt_, "#define hbqt_par_QStyleOptionTabWidgetFrame( n ) ( ( QStyleOptionTabWidgetFrame* ) hb_parptr( n ) )" )
   aadd( txt_, "#define hbqt_par_QStyleOptionTitleBar( n )   ( ( QStyleOptionTitleBar* ) hb_parptr( n ) )     " )
   aadd( txt_, "#define hbqt_par_QStyleOptionToolBar( n )    ( ( QStyleOptionToolBar* ) hb_parptr( n ) )      " )
   aadd( txt_, "#define hbqt_par_QStyleOptionToolBox( n )    ( ( QStyleOptionToolBox* ) hb_parptr( n ) )      " )
   aadd( txt_, "#define hbqt_par_QStyleOptionToolButton( n ) ( ( QStyleOptionToolButton* ) hb_parptr( n ) )   " )
   aadd( txt_, "#define hbqt_par_QStyleOptionViewItem( n )   ( ( QStyleOptionViewItem* ) hb_parptr( n ) )     " )
   aadd( txt_, "#define hbqt_par_QStylePainter( n )          ( ( QStylePainter* ) hb_parptr( n ) )            " )
   aadd( txt_, "#define hbqt_par_QTextObject( n )            ( ( QTextObject* ) hb_parptr( n ) )              " )
   aadd( txt_, "#define hbqt_par_QModelIndexList( n )        ( ( QModelIndexList* ) hb_parptr( n ) )          " )
   aadd( txt_, "#define hbqt_par_QDirModel( n )              ( ( QDirModel* ) hb_parptr( n ) )                " )
   aadd( txt_, "#define hbqt_par_QList( n )                  ( ( QList<void*>* ) hb_parptr( n ) )             " )
   aadd( txt_, "#define hbqt_par_QStringListModel( n )       ( ( QStringListModel* ) hb_parptr( n ) )         " )
   aadd( txt_, "#define hbqt_par_QAbstractListModel( n )     ( ( QAbstractListModel* ) hb_parptr( n ) )       " )
   aadd( txt_, "                                                                                              " )
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
   aadd( txt_, "#include <QtGui/QWidget>                                                                            " )
   aadd( txt_, "                                                                                              " )
   aadd( txt_, "void    hbqt_ret_QRect( QRect );                                                              " )
   aadd( txt_, "void    hbqt_ret_QSize( QSize );                                                              " )
   aadd( txt_, "void    hbqt_ret_QPoint( QPoint );                                                            " )
   aadd( txt_, "                                                                                              " )
   aadd( txt_, "QRect   hbqt_const_QRect( int );                                                              " )
   aadd( txt_, "QSize   hbqt_const_QSize( int );                                                              " )
   aadd( txt_, "QPoint  hbqt_const_QPoint( int );                                                             " )
   aadd( txt_, "                                                                                              " )
   aadd( txt_, "void    hbqt_ret_QRectF( QRectF );                                                            " )
   aadd( txt_, "void    hbqt_ret_QSizeF( QSizeF );                                                            " )
   aadd( txt_, "void    hbqt_ret_QPointF( QPointF );                                                          " )
   aadd( txt_, "                                                                                              " )
   aadd( txt_, "QRectF  hbqt_const_QRectF( int );                                                             " )
   aadd( txt_, "QSizeF  hbqt_const_QSizeF( int );                                                             " )
   aadd( txt_, "QPointF hbqt_const_QPointF( int );                                                            " )
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
   aadd( txt_, '#include <QWidget>                                                        ' )
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
   aadd( txt_, '                                                                          ' )
   aadd( txt_, 'void hbqt_ret_QRectF( QRectF qrc )                                        ' )
   aadd( txt_, '{                                                                         ' )
   aadd( txt_, '   PHB_ITEM info = hb_itemArrayNew( 4 );                                  ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '   hb_arraySetND( info, 1, qrc.x() );                                     ' )
   aadd( txt_, '   hb_arraySetND( info, 2, qrc.y() );                                     ' )
   aadd( txt_, '   hb_arraySetND( info, 3, qrc.x()+qrc.width() );                         ' )
   aadd( txt_, '   hb_arraySetND( info, 4, qrc.y()+qrc.height() );                        ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '   hb_itemReturnRelease( info );                                          ' )
   aadd( txt_, '}                                                                         ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, 'QRectF hbqt_const_QRectF( int i )                                         ' )
   aadd( txt_, '{                                                                         ' )
   aadd( txt_, '   QRectF qrc;                                                            ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '   qrc.setX( hb_parnd( i,1 ) );                                           ' )
   aadd( txt_, '   qrc.setY( hb_parnd( i,2 ) );                                           ' )
   aadd( txt_, '   qrc.setWidth( hb_parnd( i,3 ) - hb_parnd( i,1 ) + 1 );                 ' )
   aadd( txt_, '   qrc.setHeight( hb_parnd( i,4 ) - hb_parnd( i,2 ) + 1 );                ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '   return qrc;                                                            ' )
   aadd( txt_, '}                                                                         ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, 'void hbqt_ret_QSizeF( QSizeF qsz )                                        ' )
   aadd( txt_, '{                                                                         ' )
   aadd( txt_, '   PHB_ITEM info = hb_itemArrayNew( 2 );                                  ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '   hb_arraySetND( info, 1, qsz.width() );                                 ' )
   aadd( txt_, '   hb_arraySetND( info, 2, qsz.height() );                                ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '   hb_itemReturnRelease( info );                                          ' )
   aadd( txt_, '}                                                                         ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, 'QSizeF hbqt_const_QSizeF( int i )                                         ' )
   aadd( txt_, '{                                                                         ' )
   aadd( txt_, '   QSizeF qsz;                                                            ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '   qsz.setWidth( hb_parnd( i,1 ) );                                       ' )
   aadd( txt_, '   qsz.setHeight( hb_parnd( i,2 ) );                                      ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '   return qsz;                                                            ' )
   aadd( txt_, '}                                                                         ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, 'void hbqt_ret_QPointF( QPointF qpt )                                      ' )
   aadd( txt_, '{                                                                         ' )
   aadd( txt_, '   PHB_ITEM info = hb_itemArrayNew( 2 );                                  ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '   hb_arraySetND( info, 1, qpt.x() );                                     ' )
   aadd( txt_, '   hb_arraySetND( info, 2, qpt.y() );                                     ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '   hb_itemReturnRelease( info );                                          ' )
   aadd( txt_, '}                                                                         ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, 'QPointF hbqt_const_QPointF( int i )                                       ' )
   aadd( txt_, '{                                                                         ' )
   aadd( txt_, '   QPointF qpt;                                                           ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, '   qpt.setX( hb_parnd( i,1 ) );                                           ' )
   aadd( txt_, '   qpt.setY( hb_parnd( i,2 ) );                                           ' )
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
   aadd( txt_, '#include <QObject>                 ' )
   aadd( txt_, '#include <QList>                   ' )
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
   aadd( txt_, '#include <QWidget>                                                                                                        ' )
   aadd( txt_, '#include <QString>                                                                                                        ' )
   aadd( txt_, '#include <QList>                                                                                                          ' )
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

STATIC FUNCTION Build_Demo()
   LOCAL cFile := '..' + s_PathSep + 'tests' + s_PathSep + 'demoqt.prg'
   LOCAL txt_:= {}

   BuildHeader( @txt_, 2 )

   aadd( txt_, '#define QT_PTROF( oObj )  ( oObj:pPtr )                                                                                   ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '#define QT_EVE_TRIGGERED   "triggered(bool)"                                                                              ' )
   aadd( txt_, '#define QT_EVE_TRIGGERED_B "triggered(bool)"                                                                              ' )
   aadd( txt_, '#define QT_EVE_HOVERED     "hovered()"                                                                                    ' )
   aadd( txt_, '#define QT_EVE_CLICKED     "clicked()"                                                                                    ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/                                                ' )
   aadd( txt_, '/*                                                                                                                        ' )
   aadd( txt_, ' *                               A NOTE                                                                                   ' )
   aadd( txt_, ' *                                                                                                                        ' )
   aadd( txt_, ' *   This demo is built on auto generated classes by the engine. No attemp                                                ' )
   aadd( txt_, ' *   is exercised to refine the way the code must be written. At this moment                                              ' )
   aadd( txt_, ' *   my emphasis is on testing phase of QT wrapper functions and classes                                                  ' )
   aadd( txt_, ' *   generated thereof. In near future the actual implementation will be                                                  ' )
   aadd( txt_, ' *   based on the Xbase++ XBPParts  compatible framework. You just are                                                    ' )
   aadd( txt_, ' *   encouraged to sense the power of QT through this expression.                                                         ' )
   aadd( txt_, ' *                                                                                                                        ' )
   aadd( txt_, ' *                             Pritpal Bedi                                                                               ' )
   aadd( txt_, ' */                                                                                                                       ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'INIT PROCEDURE Qt_Start()                                                                                                 ' )
   aadd( txt_, '   qt_qapplication()                                                                                                      ' )
   aadd( txt_, '   /*qt_qapplication_setstyle( QT_PTROF( QWindowsXPStyle():new() ) )*/                                                    ' )
   aadd( txt_, '   RETURN                                                                                                                 ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'EXIT PROCEDURE Qt_End()                                                                                                   ' )
   aadd( txt_, '   qt_qapplication_exec()                                                                                                 ' )
   aadd( txt_, '   RETURN                                                                                                                 ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'PROCEDURE Main()                                                                                                          ' )
   aadd( txt_, '   Local oLabel, oBtn, oDA, oTabBar                                                                                       ' )
   aadd( txt_, '   Local oWnd, oSize                                                                                                      ' )
   aadd( txt_, '   Local oMenuBar, pIcon                                                                                                  ' )
   aadd( txt_, '   Local oMenuA, pAction                                                                                                  ' )
   aadd( txt_, '   LOCAL oPS, oPPrv, oMB, oWZ, oCD, oWP, oSBar, oStyle                                                                    ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oWnd := QMainWindow():new()                                                                                            ' )
   aadd( txt_, '   oWnd:setWindowTitle( "Testing - QMainWindow, QMenu, QMenuBar and QAction " )                                           ' )
   aadd( txt_, '   oWnd:setWindowIcon( "test" )                                                                                           ' )
   aadd( txt_, '   pIcon := oWnd:windowIcon()                                                                                             ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   /* The method 2 */                                                                                                     ' )
   aadd( txt_, '   oWnd:resize( 900, 500 )                                                                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oDA := QWidget():new( oWnd )                                                                                           ' )
   aadd( txt_, '   oWnd:setCentralWidget( QT_PTROF( oDA ) )                                                                               ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   Build_MenuBar( oWnd )                                                                                                  ' )
   aadd( txt_, '   Build_ToolBar( oWnd )                                                                                                  ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oSBar := QStatusBar():new( QT_PTROF( oWnd ) )                                                                          ' )
   aadd( txt_, '   oWnd:setStatusBar( QT_PTROF( oSBar ) )                                                                                 ' )
   aadd( txt_, '   oSBar:showMessage( "Harbour-QT Statusbar Ready!" )                                                                     ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oStyle := QWindowsXPStyle()                                                                                            ' )
   aadd( txt_, '   oStyle:standardIcon( 2 )                                                                                               ' )
   aadd( txt_, '   oWnd:setStyle( QT_PTROF( oStyle ) )                                                                                    ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   Build_Label( oDA, { 30,190 }, { 300, 30 } )                                                                            ' )
   aadd( txt_, '   Build_PushButton( oDA, { 30,240 }, { 100,50 } )                                                                        ' )
   aadd( txt_, '   Build_Grid( oDA, { 30, 30 }, { 450,150 } )                                                                             ' )
   aadd( txt_, '   Build_Tabs( oDA, { 510, 5 }, { 360, 400 } )                                                                            ' )
   aadd( txt_, '   Build_ProgressBar( oDA, { 30,300 }, { 200,30 } )                                                                       ' )
   aadd( txt_, '   Build_ListBox( oDA, { 310,240 }, { 150, 100 } )                                                                        ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oWnd:Show()                                                                                                            ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   RETURN                                                                                                                 ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'STATIC FUNCTION Build_MenuBar( oWnd )                                                                                     ' )
   aadd( txt_, '   LOCAL oMenuBar, oMenu                                                                                                  ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oMenuBar := QMenuBar():new( QT_PTROF( oWnd ) )                                                                         ' )
   aadd( txt_, '   oMenuBar:resize( oWnd:width(), 25 )                                                                                    ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oMenu := QMenu():new( QT_PTROF( oMenuBar ) )                                                                           ' )
   aadd( txt_, '   oMenu:setTitle( "&File" )                                                                                              ' )
   aadd( txt_, '   Qt_Connect_Signal( oMenu:addAction_1( "new.png" , "&New"  ), QT_EVE_TRIGGERED_B, {|w,l| FileDialog( "New" , w, l ) } ) ' )
   aadd( txt_, '   Qt_Connect_Signal( oMenu:addAction_1( "open.png", "&Open" ), QT_EVE_TRIGGERED_B, {|w,l| FileDialog( "Open", w, l ) } ) ' )
   aadd( txt_, '   oMenu:addSeparator()                                                                                                   ' )
   aadd( txt_, '   Qt_Connect_Signal( oMenu:addAction_1( "save.png", "&Save" ), QT_EVE_TRIGGERED_B, {|w,l| FileDialog( "Save", w, l ) } ) ' )
   aadd( txt_, '   oMenu:addSeparator()                                                                                                   ' )
   aadd( txt_, '   Qt_Connect_Signal( oMenu:addAction( "E&xit" ), QT_EVE_TRIGGERED_B, {|w,l| MsgInfo( "Exit ?" ) } )                      ' )
   aadd( txt_, '   oMenuBar:addMenu( QT_PTROF( oMenu ) )                                                                                  ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oMenu := QMenu():new( QT_PTROF( oMenuBar ) )                                                                           ' )
   aadd( txt_, '   oMenu:setTitle( "&Dialogs" )                                                                                           ' )
   aadd( txt_, '   Qt_Connect_Signal( oMenu:addAction( "&Colors"    ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Colors"   , w, l ) } )        ' )
   aadd( txt_, '   Qt_Connect_Signal( oMenu:addAction( "&Fonts"     ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Fonts"    , w, l ) } )        ' )
   aadd( txt_, '   oMenu:addSeparator()                                                                                                   ' )
   aadd( txt_, '   Qt_Connect_Signal( oMenu:addAction( "&PageSetup" ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "PageSetup", w, l ) } )        ' )
   aadd( txt_, '   Qt_Connect_Signal( oMenu:addAction( "P&review"   ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Preview"  , w, l ) } )        ' )
   aadd( txt_, '   oMenu:addSeparator()                                                                                                   ' )
   aadd( txt_, '   Qt_Connect_Signal( oMenu:addAction( "&Wizard"    ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Wizard"   , w, l ) } )        ' )
   aadd( txt_, '   Qt_Connect_Signal( oMenu:addAction( "W&ebPage"   ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "WebPage"  , w, l ) } )        ' )
   aadd( txt_, '   oMenuBar:addMenu( QT_PTROF( oMenu ) )                                                                                  ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oWnd:setMenuBar( QT_PTROF( oMenuBar ) )                                                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   RETURN nil                                                                                                             ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'STATIC FUNCTION Build_ToolBar( oWnd )                                                                                     ' )
   aadd( txt_, '   LOCAL oTB, oAct                                                                                                        ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   /* Create a Toolbar Object */                                                                                          ' )
   aadd( txt_, '   oTB := QToolBar():new( QT_PTROF( oWnd ) )                                                                              ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   /* Create an action */                                                                                                 ' )
   aadd( txt_, '   oAct := QAction():new( QT_PTROF( oWnd ) )                                                                              ' )
   aadd( txt_, '   oAct:setText( "&New" )                                                                                                 ' )
   aadd( txt_, '   oAct:setIcon( "new.png" )                                                                                              ' )
   aadd( txt_, '   oAct:setToolTip( "A New File" )                                                                                        ' )
   aadd( txt_, '   /* Attach codeblock to be triggered */                                                                                 ' )
   aadd( txt_, '   Qt_Connect_Signal( QT_PTROF( oAct ), QT_EVE_TRIGGERED_B, {|w,l| FileDialog( "New" , w, l ) } )                         ' )
   aadd( txt_, '   /* Attach Action with Toolbar */                                                                                       ' )
   aadd( txt_, '   oTB:addAction( QT_PTROF( oAct ) )                                                                                      ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   /* Create another action */                                                                                            ' )
   aadd( txt_, '   oAct := QAction():new( QT_PTROF( oWnd ) )                                                                              ' )
   aadd( txt_, '   oAct:setText( "&Open" )                                                                                                ' )
   aadd( txt_, '   oAct:setIcon( "open.png" )                                                                                             ' )
   aadd( txt_, '   oAct:setToolTip( "Select a file to be opened!" )                                                                       ' )
   aadd( txt_, '   /* Attach codeblock to be triggered */                                                                                 ' )
   aadd( txt_, '   Qt_Connect_Signal( QT_PTROF( oAct ), QT_EVE_TRIGGERED_B, {|w,l| FileDialog( "Open" , w, l ) } )                        ' )
   aadd( txt_, '   /* Attach Action with Toolbar */                                                                                       ' )
   aadd( txt_, '   oTB:addAction( QT_PTROF( oAct ) )                                                                                      ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oTB:addSeparator()                                                                                                     ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   /* Create another action */                                                                                            ' )
   aadd( txt_, '   oAct := QAction():new( QT_PTROF( oWnd ) )                                                                              ' )
   aadd( txt_, '   oAct:setText( "&Save" )                                                                                                ' )
   aadd( txt_, '   oAct:setIcon( "save.png" )                                                                                             ' )
   aadd( txt_, '   oAct:setToolTip( "Save this file!" )                                                                                   ' )
   aadd( txt_, '   /* Attach codeblock to be triggered */                                                                                 ' )
   aadd( txt_, '   Qt_Connect_Signal( QT_PTROF( oAct ), QT_EVE_TRIGGERED_B, {|w,l| FileDialog( "Save" , w, l ) } )                        ' )
   aadd( txt_, '   /* Attach Action with Toolbar */                                                                                       ' )
   aadd( txt_, '   oTB:addAction( QT_PTROF( oAct ) )                                                                                      ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   /* Add this toolbar with main window */                                                                                ' )
   aadd( txt_, '   oWnd:addToolBar_1( QT_PTROF( oTB ) )                                                                                   ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   ///////////////////////////////////////////////////////////                                                            ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   /* Build another toolbar - we will have two toolbats now */                                                            ' )
   aadd( txt_, '   oTB := QToolBar():new( QT_PTROF( oWnd ) )                                                                              ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oAct := QAction():new( QT_PTROF( oWnd ) )                                                                              ' )
   aadd( txt_, '   oAct:setText( "&Colors" )                                                                                              ' )
   aadd( txt_, '   oAct:setToolTip( "Colors Dialog" )                                                                                     ' )
   aadd( txt_, '   Qt_Connect_Signal( QT_PTROF( oAct ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Colors", w, l ) } )                          ' )
   aadd( txt_, '   oTB:addAction( QT_PTROF( oAct ) )                                                                                      ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oAct := QAction():new( QT_PTROF( oWnd ) )                                                                              ' )
   aadd( txt_, '   oAct:setText( "&Fonts" )                                                                                               ' )
   aadd( txt_, '   oAct:setToolTip( "Fonts Dialog" )                                                                                      ' )
   aadd( txt_, '   Qt_Connect_Signal( QT_PTROF( oAct ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Fonts", w, l ) } )                           ' )
   aadd( txt_, '   oTB:addAction( QT_PTROF( oAct ) )                                                                                      ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oTB:addSeparator()                                                                                                     ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oAct := QAction():new( QT_PTROF( oWnd ) )                                                                              ' )
   aadd( txt_, '   oAct:setText( "&PgSetup" )                                                                                             ' )
   aadd( txt_, '   oAct:setToolTip( "Page Setup Dialog" )                                                                                 ' )
   aadd( txt_, '   Qt_Connect_Signal( QT_PTROF( oAct ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "PageSetup", w, l ) } )                       ' )
   aadd( txt_, '   oTB:addAction( QT_PTROF( oAct ) )                                                                                      ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oAct := QAction():new( QT_PTROF( oWnd ) )                                                                              ' )
   aadd( txt_, '   oAct:setText( "&Preview" )                                                                                             ' )
   aadd( txt_, '   oAct:setToolTip( "Page Preview Dialog" )                                                                               ' )
   aadd( txt_, '   Qt_Connect_Signal( QT_PTROF( oAct ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Preview", w, l ) } )                         ' )
   aadd( txt_, '   oTB:addAction( QT_PTROF( oAct ) )                                                                                      ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oTB:addSeparator()                                                                                                     ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oAct := QAction():new( QT_PTROF( oWnd ) )                                                                              ' )
   aadd( txt_, '   oAct:setText( "&Webpage" )                                                                                             ' )
   aadd( txt_, '   oAct:setToolTip( "Web Browser Dialog" )                                                                                ' )
   aadd( txt_, '   Qt_Connect_Signal( QT_PTROF( oAct ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "WebPage", w, l ) } )                         ' )
   aadd( txt_, '   oTB:addAction( QT_PTROF( oAct ) )                                                                                      ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oAct := QAction():new( QT_PTROF( oWnd ) )                                                                              ' )
   aadd( txt_, '   oAct:setText( "&Wizard" )                                                                                              ' )
   aadd( txt_, '   oAct:setToolTip( "Generic Wizard Dialog" )                                                                             ' )
   aadd( txt_, '   Qt_Connect_Signal( QT_PTROF( oAct ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Wizard", w, l ) } )                          ' )
   aadd( txt_, '   oTB:addAction( QT_PTROF( oAct ) )                                                                                      ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   /* Add this toolbar with main window */                                                                                ' )
   aadd( txt_, '   oWnd:addToolBar_1( QT_PTROF( oTB ) )                                                                                   ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   RETURN nil                                                                                                             ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'STATIC FUNCTION Build_PushButton( oWnd, aPos, aSize )                                                                     ' )
   aadd( txt_, '   LOCAL oBtn                                                                                                             ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oBtn := QPushButton():new( QT_PTROF( oWnd ) )                                                                          ' )
   aadd( txt_, '   oBtn:setText( "Push Button" )                                                                                          ' )
   aadd( txt_, '   oBtn:move( aPos[ 1 ],aPos[ 2 ] )                                                                                       ' )
   aadd( txt_, '   oBtn:resize( aSize[ 1 ],aSize[ 2 ] )                                                                                   ' )
   aadd( txt_, '   oBtn:show()                                                                                                            ' )
   aadd( txt_, '   Qt_Connect_Signal( QT_PTROF( oBtn ), QT_EVE_CLICKED, {|| MsgInfo( "Push Button Pressed" ) } )                          ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   RETURN nil                                                                                                             ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'STATIC FUNCTION Build_Grid( oWnd, aPos, aSize )                                                                           ' )
   aadd( txt_, '   LOCAL oGrid, oBrushBackItem0x0, oBrushForeItem0x0, oGridItem0x0                                                        ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oGrid := QTableWidget():new( QT_PTROF( oWnd ) )                                                                        ' )
   aadd( txt_, '   oGrid:setRowCount( 2 )                                                                                                 ' )
   aadd( txt_, '   oGrid:setColumnCount( 4 )                                                                                              ' )
   aadd( txt_, '   //                                                                                                                     ' )
   aadd( txt_, '   oBrushBackItem0x0 := QBrush():new()                                                                                    ' )
   aadd( txt_, '   oBrushBackItem0x0:setStyle( 1 )        // Solid Color                                                                  ' )
   aadd( txt_, '   oBrushBackItem0x0:setColor_1( 10 )     // http://doc.qtsoftware.com/4.5/qt.html#GlobalColor-enum                       ' )
   aadd( txt_, '   //                                                                                                                     ' )
   aadd( txt_, '   oBrushForeItem0x0 := QBrush():new()                                                                                    ' )
   aadd( txt_, '   oBrushForeItem0x0:setColor_1( 7 )                                                                                      ' )
   aadd( txt_, '   //                                                                                                                     ' )
   aadd( txt_, '   oGridItem0x0 := QTableWidgetItem():new()                                                                               ' )
   aadd( txt_, '   oGridItem0x0:setBackground( QT_PTROF( oBrushBackItem0x0 ) )                                                            ' )
   aadd( txt_, '   oGridItem0x0:setForeground( QT_PTROF( oBrushForeItem0x0 ) )                                                            ' )
   aadd( txt_, '   oGridItem0x0:setText( "Item 0x0" )                                                                                     ' )
   aadd( txt_, '   //                                                                                                                     ' )
   aadd( txt_, '   oGrid:setItem( 0, 0, QT_PTROF( oGridItem0x0 ) )                                                                        ' )
   aadd( txt_, '   //                                                                                                                     ' )
   aadd( txt_, '   oGrid:Move( aPos[ 1 ], aPos[ 2 ] )                                                                                     ' )
   aadd( txt_, '   oGrid:ReSize( aSize[ 1 ], aSize[ 2 ] )                                                                                 ' )
   aadd( txt_, '   //                                                                                                                     ' )
   aadd( txt_, '   oGrid:Show()                                                                                                           ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   RETURN nil                                                                                                             ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'STATIC FUNCTION Build_Tabs( oWnd, aPos, aSize )                                                                           ' )
   aadd( txt_, '   LOCAL oTabWidget, oTab1, oTab2, oTab3                                                                                  ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oTabWidget := QTabWidget():new( QT_PTROF( oWnd ) )                                                                     ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oTab1 := QWidget():new( QT_PTROF( oTabWidget ) )                                                                       ' )
   aadd( txt_, '   oTab2 := QWidget():new( QT_PTROF( oTabWidget ) )                                                                       ' )
   aadd( txt_, '   oTab3 := QWidget():new( QT_PTROF( oTabWidget ) )                                                                       ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oTabWidget:addTab( QT_PTROF( oTab1 ), "Folders"  )                                                                     ' )
   aadd( txt_, '   oTabWidget:addTab( QT_PTROF( oTab2 ), "Controls" )                                                                     ' )
   aadd( txt_, '   oTabWidget:addTab( QT_PTROF( oTab3 ), "TextBox"  )                                                                     ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oTabWidget:Move( aPos[ 1 ], aPos[ 2 ] )                                                                                ' )
   aadd( txt_, '   oTabWidget:ReSize( aSize[ 1 ], aSize[ 2 ] )                                                                            ' )
   aadd( txt_, '   oTabWidget:show()                                                                                                      ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   Build_Treeview( oTab1 )                                                                                                ' )
   aadd( txt_, '   Build_Controls( oTab2 )                                                                                                ' )
   aadd( txt_, '   Build_TextBox( oTab3 )                                                                                                 ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   RETURN { oTab1, oTab2, oTab3 }                                                                                         ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'STATIC FUNCTION Build_TreeView( oWnd )                                                                                    ' )
   aadd( txt_, '   LOCAL oTV, oDirModel                                                                                                   ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oTV := QTreeView():new( QT_PTROF( oWnd ) )                                                                             ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oDirModel := QDirModel():new( QT_PTROF( oTV ) )                                                                        ' )
   aadd( txt_, '   oTV:setModel( QT_PTROF( oDirModel ) )                                                                                  ' )
   aadd( txt_, '   oTV:move( 5, 7 )                                                                                                       ' )
   aadd( txt_, '   oTV:resize( 345, 365 )                                                                                                 ' )
   aadd( txt_, '   OTV:show()                                                                                                             ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   RETURN nil                                                                                                             ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'STATIC FUNCTION Build_ListBox( oWnd, aPos, aSize )                                                                        ' )
   aadd( txt_, '   LOCAL oListBox, oStrList, oStrModel                                                                                    ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oListBox := QListView():New( QT_PTROF( oWnd ) )                                                                        ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oStrList := QStringList():new( QT_PTROF( oListBox ) )                                                                  ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oStrList:append( "India"          )                                                                                    ' )
   aadd( txt_, '   oStrList:append( "United States"  )                                                                                    ' )
   aadd( txt_, '   oStrList:append( "England"        )                                                                                    ' )
   aadd( txt_, '   oStrList:append( "Japan"          )                                                                                    ' )
   aadd( txt_, '   oStrList:append( "Hungary"        )                                                                                    ' )
   aadd( txt_, '   oStrList:append( "Argentina"      )                                                                                    ' )
   aadd( txt_, '   oStrList:append( "China"          )                                                                                    ' )
   aadd( txt_, '   oStrList:sort()                                                                                                        ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oStrModel := QStringListModel():new( QT_PTROF( oListBox ) )                                                            ' )
   aadd( txt_, '   oStrModel:setStringList( QT_PTROF( oStrList ) )                                                                        ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oListBox:setModel( QT_PTROF( oStrModel ) )                                                                             ' )
   aadd( txt_, '   oListBox:Move( aPos[ 1 ], aPos[ 2 ] )                                                                                  ' )
   aadd( txt_, '   oListBox:ReSize( aSize[ 1 ], aSize[ 2 ] )                                                                              ' )
   aadd( txt_, '   oListBox:Show()                                                                                                        ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   RETURN nil                                                                                                             ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'STATIC FUNCTION Build_TextBox( oWnd )                                                                                     ' )
   aadd( txt_, '   LOCAL oTextBox                                                                                                         ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oTextBox := QTextEdit():new( QT_PTROF( oWnd ) )                                                                        ' )
   aadd( txt_, '   oTextBox:Move( 5, 7 )                                                                                                  ' )
   aadd( txt_, '   oTextBox:Resize( 345,365 )                                                                                             ' )
   aadd( txt_, '   oTextBox:setAcceptRichText( .t. )                                                                                      ' )
   aadd( txt_, '   oTextBox:setPlainText( "This is Harbour QT implementation" )                                                           ' )
   aadd( txt_, '   oTextBox:Show()                                                                                                        ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   RETURN nil                                                                                                             ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'STATIC FUNCTION Build_Controls( oWnd )                                                                                    ' )
   aadd( txt_, '   LOCAL oEdit, oCheckBox, oComboBox, oSpinBox, oRadioButton, oVariant                                                    ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oEdit := QLineEdit():new( QT_PTROF( oWnd ) )                                                                           ' )
   aadd( txt_, '   oEdit:move( 5, 10 )                                                                                                    ' )
   aadd( txt_, '   oEdit:resize( 345, 30 )                                                                                                ' )
   aadd( txt_, '   oEdit:setMaxLength( 40 )                                                                                               ' )
   aadd( txt_, '   oEdit:setText( "TextBox Testing Max Length = 40" )                                                                     ' )
   aadd( txt_, '   oEdit:setAlignment( 1 )   // 1: Left  2: Right  4: center 8: use all textbox length                                    ' )
   aadd( txt_, '   oEdit:show()                                                                                                           ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oComboBox := QComboBox():New( QT_PTROF( oWnd ) )                                                                       ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oComboBox:addItem( "First"  )                                                                                          ' )
   aadd( txt_, '   oComboBox:addItem( "Second" )                                                                                          ' )
   aadd( txt_, '   oComboBox:addItem( "Third"  )                                                                                          ' )
   aadd( txt_, '   oComboBox:move( 5, 60 )                                                                                                ' )
   aadd( txt_, '   oComboBox:resize( 345, 30 )                                                                                            ' )
   aadd( txt_, '   oComboBox:show()                                                                                                       ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oCheckBox := QCheckBox():New( QT_PTROF( oWnd ) )                                                                       ' )
   aadd( txt_, '   oCheckBox:setText( "Testing CheckBox HbQt" )                                                                           ' )
   aadd( txt_, '   oCheckBox:move( 5, 110 )                                                                                               ' )
   aadd( txt_, '   oCheckBox:resize( 345, 30 )                                                                                            ' )
   aadd( txt_, '   oCheckBox:show()                                                                                                       ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oSpinBox := QSpinBox():New( QT_PTROF( oWnd ) )                                                                         ' )
   aadd( txt_, '   oSpinBox:Move( 5, 160 )                                                                                                ' )
   aadd( txt_, '   oSpinBox:ReSize( 345, 30 )                                                                                             ' )
   aadd( txt_, '   oSpinBox:Show()                                                                                                        ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oRadioButton := QRadioButton():New( QT_PTROF( oWnd ) )                                                                 ' )
   aadd( txt_, '   oRadioButton:Move( 5, 210 )                                                                                            ' )
   aadd( txt_, '   oRadioButton:ReSize( 345, 30 )                                                                                         ' )
   aadd( txt_, '   oRadioButton:Show()                                                                                                    ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   RETURN nil                                                                                                             ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'STATIC FUNCTION Build_ProgressBar( oWnd, aPos, aSize )                                                                    ' )
   aadd( txt_, '   LOCAL oProgressBar                                                                                                     ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oProgressBar := QProgressBar():New( QT_PTROF( oWnd ) )                                                                 ' )
   aadd( txt_, '   oProgressBar:SetRange( 1, 1500 )                                                                                       ' )
   aadd( txt_, '   oProgressBar:Setvalue( 500 )                                                                                           ' )
   aadd( txt_, '   oProgressBar:Move( aPos[ 1 ], aPos[ 2 ] )                                                                              ' )
   aadd( txt_, '   oProgressBar:ReSize( aSize[ 1 ], aSize[ 2 ] )                                                                          ' )
   aadd( txt_, '   oProgressBar:Show()                                                                                                    ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   RETURN nil                                                                                                             ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'STATIC FUNCTION Build_Label( oWnd, aPos, aSize )                                                                          ' )
   aadd( txt_, '   LOCAL oLabel                                                                                                           ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oLabel := QLabel():New( QT_PTROF( oWnd ) )                                                                             ' )
   aadd( txt_, '   oLabel:SetTextFormat( 1 )  // 0 text plain  1 RichText                                                                 ' )
   aadd( txt_, '   oLabel:SetText( [<font color="Blue" size=6 ><u>This is a</u> <i>Label</i> in <b>Harbour QT</b></font>] )               ' )
   aadd( txt_, '   oLabel:Move( aPos[ 1 ], aPos[ 2 ] )                                                                                    ' )
   aadd( txt_, '   oLabel:ReSize( aSize[ 1 ], aSize[ 2 ] )                                                                                ' )
   aadd( txt_, '   oLabel:Show()                                                                                                          ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   RETURN nil                                                                                                             ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'STATIC FUNCTION MsgInfo( cMsg )                                                                                           ' )
   aadd( txt_, '   LOCAL oMB                                                                                                              ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oMB := QMessageBox():new()                                                                                             ' )
   aadd( txt_, '   oMB:setInformativeText( cMsg )                                                                                         ' )
   aadd( txt_, '   oMB:setWindowTitle( "Harbour-QT" )                                                                                     ' )
   aadd( txt_, '   oMB:show()                                                                                                             ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   RETURN nil                                                                                                             ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'STATIC FUNCTION FileDialog( cType, w, l )                                                                                 ' )
   aadd( txt_, '   LOCAL oFD := QFileDialog():new()                                                                                       ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oFD:setWindowTitle( "Select a File" )                                                                                  ' )
   aadd( txt_, '   oFD:show()                                                                                                             ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   RETURN nil                                                                                                             ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'STATIC FUNCTION Dialogs( cType, w, l )                                                                                    ' )
   aadd( txt_, '   LOCAL oDlg, oUrl                                                                                                       ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   DO CASE                                                                                                                ' )
   aadd( txt_, '   CASE cType == "PageSetup"                                                                                              ' )
   aadd( txt_, '      oDlg := QPageSetupDialog():new()                                                                                    ' )
   aadd( txt_, '      oDlg:setWindowTitle( "Harbour-QT PageSetup Dialog" )                                                                ' )
   aadd( txt_, '      oDlg:show()                                                                                                         ' )
   aadd( txt_, '   CASE cType == "Preview"                                                                                                ' )
   aadd( txt_, '      oDlg := QPrintPreviewDialog():new()                                                                                 ' )
   aadd( txt_, '      oDlg:setWindowTitle( "Harbour-QT Preview Dialog" )                                                                  ' )
   aadd( txt_, '      oDlg:show()                                                                                                         ' )
   aadd( txt_, '   CASE cType == "Wizard"                                                                                                 ' )
   aadd( txt_, '      oDlg := QWizard():new()                                                                                             ' )
   aadd( txt_, '      oDlg:setWindowTitle( "Harbour-QT Wizard to Show Slides etc." )                                                      ' )
   aadd( txt_, '      oDlg:show()                                                                                                         ' )
   aadd( txt_, '   CASE cType == "Colors"                                                                                                 ' )
   aadd( txt_, '      oDlg := QColorDialog():new()                                                                                        ' )
   aadd( txt_, '      oDlg:setWindowTitle( "Harbour-QT Color Selection Dialog" )                                                          ' )
   aadd( txt_, '      oDlg:show()                                                                                                         ' )
   aadd( txt_, '   CASE cType == "WebPage"                                                                                                ' )
   aadd( txt_, '      oDlg := QWebView():new()                                                                                            ' )
   aadd( txt_, '      oUrl := QUrl():new()                                                                                                ' )
   aadd( txt_, '      oUrl:setUrl( "http://www.harbour.vouch.info" )                                                                      ' )
   aadd( txt_, '      QT_QWebView_SetUrl( QT_PTROF( oDlg ), QT_PTROF( oUrl ) )                                                            ' )
   aadd( txt_, '      oDlg:setWindowTitle( "Harbour-QT Web Page Navigator" )                                                              ' )
   aadd( txt_, '      oDlg:show()                                                                                                         ' )
   aadd( txt_, '   CASE cType == "Fonts"                                                                                                  ' )
   aadd( txt_, '      oDlg := QFontDialog():new()                                                                                         ' )
   aadd( txt_, '      oDlg:setWindowTitle( "Harbour-QT Font Selector" )                                                                   ' )
   aadd( txt_, '      oDlg:show()                                                                                                         ' )
   aadd( txt_, '   ENDCASE                                                                                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   RETURN nil                                                                                                             ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '#ifdef __PLATFORM__WINDOWS                                                                                                ' )
   aadd( txt_, 'PROCEDURE hb_GtSys()                                                                                                      ' )
   aadd( txt_, '   HB_GT_GUI_DEFAULT()                                                                                                    ' )
   aadd( txt_, '   RETURN                                                                                                                 ' )
   aadd( txt_, '#endif                                                                                                                    ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/                                                ' )
   aadd( txt_, '/*                                                                                                                        ' )
   aadd( txt_, ' * Just to Link Every New Widget                                                                                          ' )
   aadd( txt_, ' */                                                                                                                       ' )
   aadd( txt_, 'STATIC FUNCTION Dummies()                                                                                                 ' )
   aadd( txt_, '   LOCAL oSome                                                                                                            ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   oSome := QAbstractButton():new()                                                                                       ' )
   aadd( txt_, '   oSome := QAbstractItemView():new()                                                                                     ' )
   aadd( txt_, '   oSome := QAbstractPrintDialog():new()                                                                                  ' )
   aadd( txt_, '   oSome := QAbstractScrollArea():new()                                                                                   ' )
   aadd( txt_, '   oSome := QAbstractSlider():new()                                                                                       ' )
   aadd( txt_, '   oSome := QAbstractSpinBox():new()                                                                                      ' )
   aadd( txt_, '   oSome := QAction():new()                                                                                               ' )
   aadd( txt_, '   //oSome := QApplication():new()                                                                                        ' )
   aadd( txt_, '   oSome := QBitmap():new()                                                                                               ' )
   aadd( txt_, '   oSome := QBoxLayout():new()                                                                                            ' )
   aadd( txt_, '   oSome := QBrush():new()                                                                                                ' )
   aadd( txt_, '   oSome := QCalendarWidget():new()                                                                                       ' )
   aadd( txt_, '   oSome := QCheckBox():new()                                                                                             ' )
   aadd( txt_, '   oSome := QClipboard():new()                                                                                            ' )
   aadd( txt_, '   oSome := QColor():new()                                                                                                ' )
   aadd( txt_, '   oSome := QColorDialog():new()                                                                                          ' )
   aadd( txt_, '   oSome := QComboBox():new()                                                                                             ' )
   aadd( txt_, '   oSome := QCommandLinkButton():new()                                                                                    ' )
   aadd( txt_, '   oSome := QCommonStyle():new()                                                                                          ' )
   aadd( txt_, '   oSome := QConicalGradient():new()                                                                                      ' )
   aadd( txt_, '   //oSome := QCoreApplication():new()                                                                                    ' )
   aadd( txt_, '   oSome := QDateEdit():new()                                                                                             ' )
   aadd( txt_, '   oSome := QDateTimeEdit():new()                                                                                         ' )
   aadd( txt_, '   oSome := QDesktopWidget():new()                                                                                        ' )
   aadd( txt_, '   oSome := QDial():new()                                                                                                 ' )
   aadd( txt_, '   oSome := QDialog():new()                                                                                               ' )
   aadd( txt_, '   oSome := QDir():new()                                                                                                  ' )
   aadd( txt_, '   oSome := QDockWidget():new()                                                                                           ' )
   aadd( txt_, '   oSome := QDoubleSpinBox():new()                                                                                        ' )
   aadd( txt_, '   oSome := QErrorMessage():new()                                                                                         ' )
   aadd( txt_, '   oSome := QEvent():new()                                                                                                ' )
   aadd( txt_, '   oSome := QEventLoop():new()                                                                                            ' )
   aadd( txt_, '   oSome := QFileDialog():new()                                                                                           ' )
   aadd( txt_, '   oSome := QFocusFrame():new()                                                                                           ' )
   aadd( txt_, '   oSome := QFont():new()                                                                                                 ' )
   aadd( txt_, '   oSome := QFontComboBox():new()                                                                                         ' )
   aadd( txt_, '   oSome := QFontDialog():new()                                                                                           ' )
   aadd( txt_, '   oSome := QFontInfo():new()                                                                                             ' )
   aadd( txt_, '   oSome := QFormLayout():new()                                                                                           ' )
   aadd( txt_, '   oSome := QFrame():new()                                                                                                ' )
   aadd( txt_, '   oSome := QFtp():new()                                                                                                  ' )
   aadd( txt_, '   oSome := QGradient():new()                                                                                             ' )
   aadd( txt_, '   oSome := QGridLayout():new()                                                                                           ' )
   aadd( txt_, '   oSome := QGroupBox():new()                                                                                             ' )
   aadd( txt_, '   oSome := QHBoxLayout():new()                                                                                           ' )
   aadd( txt_, '   oSome := QHeaderView():new()                                                                                           ' )
   aadd( txt_, '   oSome := QHttp():new()                                                                                                 ' )
   aadd( txt_, '   oSome := QIcon():new()                                                                                                 ' )
   aadd( txt_, '   oSome := QImage():new()                                                                                                ' )
   aadd( txt_, '   oSome := QImageReader():new()                                                                                          ' )
   aadd( txt_, '   oSome := QImageWriter():new()                                                                                          ' )
   aadd( txt_, '   oSome := QInputDialog():new()                                                                                          ' )
   aadd( txt_, '   oSome := QInputEvent():new()                                                                                           ' )
   aadd( txt_, '   oSome := QLabel():new()                                                                                                ' )
   aadd( txt_, '   oSome := QLayout():new()                                                                                               ' )
   aadd( txt_, '   oSome := QLayoutItem():new()                                                                                           ' )
   aadd( txt_, '   oSome := QLCDNumber():new()                                                                                            ' )
   aadd( txt_, '   oSome := QLine():new()                                                                                                 ' )
   aadd( txt_, '   oSome := QLinearGradient():new()                                                                                       ' )
   aadd( txt_, '   oSome := QLineEdit():new()                                                                                             ' )
   aadd( txt_, '   oSome := QListView():new()                                                                                             ' )
   aadd( txt_, '   oSome := QListWidget():new()                                                                                           ' )
   aadd( txt_, '   oSome := QListWidgetItem():new()                                                                                       ' )
   aadd( txt_, '   oSome := QMainWindow():new()                                                                                           ' )
   aadd( txt_, '   oSome := QMenu():new()                                                                                                 ' )
   aadd( txt_, '   oSome := QMenuBar():new()                                                                                              ' )
   aadd( txt_, '   oSome := QMessageBox():new()                                                                                           ' )
   aadd( txt_, '   oSome := QObject():new()                                                                                               ' )
   aadd( txt_, '   oSome := QPaintDevice():new()                                                                                          ' )
   aadd( txt_, '   oSome := QPageSetupDialog():new()                                                                                      ' )
   aadd( txt_, '   oSome := QPainter():new()                                                                                              ' )
   aadd( txt_, '   oSome := QPaintEvent():new()                                                                                           ' )
   aadd( txt_, '   oSome := QPalette():new()                                                                                              ' )
   aadd( txt_, '   oSome := QPen():new()                                                                                                  ' )
   aadd( txt_, '   oSome := QPicture():new()                                                                                              ' )
   aadd( txt_, '   oSome := QPixmap():new()                                                                                               ' )
   aadd( txt_, '   oSome := QPoint():new()                                                                                                ' )
   aadd( txt_, '   oSome := QPointF():new()                                                                                               ' )
   aadd( txt_, '   oSome := QPrintDialog():new()                                                                                          ' )
   aadd( txt_, '   oSome := QPrintPreviewDialog():new()                                                                                   ' )
   aadd( txt_, '   oSome := QProgressBar():new()                                                                                          ' )
   aadd( txt_, '   oSome := QProgressDialog():new()                                                                                       ' )
   aadd( txt_, '   oSome := QPushButton():new()                                                                                           ' )
   aadd( txt_, '   oSome := QRadialGradient():new()                                                                                       ' )
   aadd( txt_, '   oSome := QRect():new()                                                                                                 ' )
   aadd( txt_, '   oSome := QRectF():new()                                                                                                ' )
   aadd( txt_, '   oSome := QRadioButton():new()                                                                                          ' )
   aadd( txt_, '   oSome := QRegion():new()                                                                                               ' )
   aadd( txt_, '   oSome := QResource():new()                                                                                             ' )
   aadd( txt_, '   oSome := QScrollArea():new()                                                                                           ' )
   aadd( txt_, '   oSome := QScrollBar():new()                                                                                            ' )
   aadd( txt_, '   oSome := QSignalMapper():new()                                                                                         ' )
   aadd( txt_, '   oSome := QSize():new()                                                                                                 ' )
   aadd( txt_, '   oSome := QSizeF():new()                                                                                                ' )
   aadd( txt_, '   oSome := QSizeGrip():new()                                                                                             ' )
   aadd( txt_, '   oSome := QSizePolicy():new()                                                                                           ' )
   aadd( txt_, '   oSome := QSlider():new()                                                                                               ' )
   aadd( txt_, '   oSome := QSound():new()                                                                                                ' )
   aadd( txt_, '   oSome := QSpinBox():new()                                                                                              ' )
   aadd( txt_, '   oSome := QSplashScreen():new()                                                                                         ' )
   aadd( txt_, '   oSome := QSplitter():new()                                                                                             ' )
   aadd( txt_, '   oSome := QStandardItem():new()                                                                                         ' )
   aadd( txt_, '   oSome := QStandardItemModel():new()                                                                                    ' )
   aadd( txt_, '   oSome := QStatusBar():new()                                                                                            ' )
   aadd( txt_, '   oSome := QStyle():new()                                                                                                ' )
   aadd( txt_, '   oSome := QStyledItemDelegate():new()                                                                                   ' )
   aadd( txt_, '   oSome := QStyleHintReturn():new()                                                                                      ' )
   aadd( txt_, '   oSome := QStyleHintReturnMask():new()                                                                                  ' )
   aadd( txt_, '   oSome := QStyleHintReturnVariant():new()                                                                               ' )
   aadd( txt_, '   oSome := QStyleOption():new()                                                                                          ' )
   aadd( txt_, '   oSome := QStyleOptionButton():new()                                                                                    ' )
   aadd( txt_, '   oSome := QStyleOptionComboBox():new()                                                                                  ' )
   aadd( txt_, '   oSome := QStyleOptionComplex():new()                                                                                   ' )
   aadd( txt_, '   oSome := QStyleOptionDockWidget():new()                                                                                ' )
   aadd( txt_, '   oSome := QStyleOptionFocusRect():new()                                                                                 ' )
   aadd( txt_, '   oSome := QStyleOptionFrame():new()                                                                                     ' )
   aadd( txt_, '   oSome := QStyleOptionGroupBox():new()                                                                                  ' )
   aadd( txt_, '   oSome := QStyleOptionHeader():new()                                                                                    ' )
   aadd( txt_, '   oSome := QStyleOptionMenuItem():new()                                                                                  ' )
   aadd( txt_, '   oSome := QStyleOptionProgressBar():new()                                                                               ' )
   aadd( txt_, '   oSome := QStyleOptionSizeGrip():new()                                                                                  ' )
   aadd( txt_, '   oSome := QStyleOptionSlider():new()                                                                                    ' )
   aadd( txt_, '   oSome := QStyleOptionSpinBox():new()                                                                                   ' )
   aadd( txt_, '   oSome := QStyleOptionTab():new()                                                                                       ' )
   aadd( txt_, '   oSome := QStyleOptionTabBarBase():new()                                                                                ' )
   aadd( txt_, '   oSome := QStyleOptionTabWidgetFrame():new()                                                                            ' )
   aadd( txt_, '   oSome := QStyleOptionTitleBar():new()                                                                                  ' )
   aadd( txt_, '   oSome := QStyleOptionToolBar():new()                                                                                   ' )
   aadd( txt_, '   oSome := QStyleOptionToolBox():new()                                                                                   ' )
   aadd( txt_, '   oSome := QStyleOptionToolButton():new()                                                                                ' )
   aadd( txt_, '   oSome := QStyleOptionViewItem():new()                                                                                  ' )
   aadd( txt_, '   oSome := QStylePainter():new()                                                                                         ' )
   aadd( txt_, '   oSome := QTabBar():new()                                                                                               ' )
   aadd( txt_, '   oSome := QTableView():new()                                                                                            ' )
   aadd( txt_, '   oSome := QTableWidget():new()                                                                                          ' )
   aadd( txt_, '   oSome := QTableWidgetItem():new()                                                                                      ' )
   aadd( txt_, '   oSome := QTabWidget():new()                                                                                            ' )
   aadd( txt_, '   oSome := QTextBlock():new()                                                                                            ' )
   aadd( txt_, '   oSome := QTextBlockFormat():new()                                                                                      ' )
   aadd( txt_, '   oSome := QTextBlockGroup():new()                                                                                       ' )
   aadd( txt_, '   oSome := QTextBrowser():new()                                                                                          ' )
   aadd( txt_, '   oSome := QTextBoundaryFinder():new()                                                                                   ' )
   aadd( txt_, '   oSome := QTextCharFormat():new()                                                                                       ' )
   aadd( txt_, '   oSome := QTextCodec():new()                                                                                            ' )
   aadd( txt_, '   oSome := QTextCursor():new()                                                                                           ' )
   aadd( txt_, '   oSome := QTextDecoder():new()                                                                                          ' )
   aadd( txt_, '   oSome := QTextDocument():new()                                                                                         ' )
   aadd( txt_, '   oSome := QTextDocumentFragment():new()                                                                                 ' )
   aadd( txt_, '   oSome := QTextDocumentWriter():new()                                                                                   ' )
   aadd( txt_, '   oSome := QTextEdit():new()                                                                                             ' )
   aadd( txt_, '   oSome := QTextEncoder():new()                                                                                          ' )
   aadd( txt_, '   oSome := QTextFormat():new()                                                                                           ' )
   aadd( txt_, '   oSome := QTextFragment():new()                                                                                         ' )
   aadd( txt_, '   oSome := QTextFrame():new()                                                                                            ' )
   aadd( txt_, '   oSome := QTextFrameFormat():new()                                                                                      ' )
   aadd( txt_, '   oSome := QTextImageFormat():new()                                                                                      ' )
   aadd( txt_, '   oSome := QTextInlineObject():new()                                                                                     ' )
   aadd( txt_, '   oSome := QTextItem():new()                                                                                             ' )
   aadd( txt_, '   oSome := QTextLayout():new()                                                                                           ' )
   aadd( txt_, '   oSome := QTextLength():new()                                                                                           ' )
   aadd( txt_, '   oSome := QTextLine():new()                                                                                             ' )
   aadd( txt_, '   oSome := QTextObject():new()                                                                                           ' )
   aadd( txt_, '   oSome := QTextStream():new()                                                                                           ' )
   aadd( txt_, '   oSome := QTimeEdit():new()                                                                                             ' )
   aadd( txt_, '   oSome := QTimer():new()                                                                                                ' )
   aadd( txt_, '   oSome := QToolBar():new()                                                                                              ' )
   aadd( txt_, '   oSome := QToolBox():new()                                                                                              ' )
   aadd( txt_, '   oSome := QToolButton():new()                                                                                           ' )
   aadd( txt_, '   oSome := QTreeView():new()                                                                                             ' )
   aadd( txt_, '   oSome := QTreeWidget():new()                                                                                           ' )
   aadd( txt_, '   oSome := QTreeWidgetItem():new()                                                                                       ' )
   aadd( txt_, '   oSome := QUrl():new()                                                                                                  ' )
   aadd( txt_, '   oSome := QVariant():new()                                                                                              ' )
   aadd( txt_, '   oSome := QVBoxLayout():new()                                                                                           ' )
   aadd( txt_, '   oSome := QWebPage():new()                                                                                              ' )
   aadd( txt_, '   oSome := QWidget():new()                                                                                               ' )
   aadd( txt_, '   oSome := QWebView():new()                                                                                              ' )
   aadd( txt_, '   oSome := QWindowsStyle():new()                                                                                         ' )
   aadd( txt_, '   oSome := QWindowsXPStyle():new()                                                                                       ' )
   aadd( txt_, '   oSome := QWizard():new()                                                                                               ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '   RETURN nil                                                                                                             ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/                                                ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '#PRAGMA BEGINDUMP                                                                                                         ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '#include <windows.h>                                                                                                      ' )
   aadd( txt_, '#include "hbapi.h"                                                                                                        ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, 'HB_FUNC( UIDEBUG )                                                                                                        ' )
   aadd( txt_, '{                                                                                                                         ' )
   aadd( txt_, '   OutputDebugString( hb_parc( 1 ) );                                                                                     ' )
   aadd( txt_, '}                                                                                                                         ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '#PRAGMA ENDDUMP                                                                                                           ' )
   aadd( txt_, '                                                                                                                          ' )
   aadd( txt_, '/*----------------------------------------------------------------------*/                                                ' )
   aadd( txt_, '                                                                                                                          ' )

   RETURN CreateTarget( cFile, txt_ )

/*----------------------------------------------------------------------*/

