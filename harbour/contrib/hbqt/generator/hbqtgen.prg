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

   IF !empty( cpp_ )
      Build_Makefile( cpp_, prg_, cPOut )
   ENDIF

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
   LOCAL s, n, nFuncs, nCnvrtd
   LOCAL b_, txt_, enum_, code_, func_, dummy_, cpp_, cmntd_, doc_, varbls_
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
         aeval( enums_, {|e| iif( !empty( e ), aadd( cpp_, ' *  ' + e ), NIL ) } )
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
   LOCAL cInt := 'int,qint16,quint16,QChar'
   LOCAL cIntLong := 'qint32,quint32,QRgb'
   LOCAL cIntLongLong := 'qint64,quint64,qlonglong,qulonglong'

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

         IF ascan( aEnum, {|e| iif( empty( e ), .f., e == aRet[ PRT_CAST ] ) } ) > 0
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

            IF ascan( aEnum, {|e| iif( empty( e ), .f., e == aA[ PRT_CAST ] ) } ) > 0
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
            CASE aA[ PRT_CAST ] $ cInt .and. aA[ PRT_L_FAR ]
               aadd( aPre, { aA[ PRT_CAST ] + ' i' + cDocNM + ' = 0;', nHBIdx, 'i'+ cDocNM, 'hb_storni' } )
               aA[ PRT_BODY ] := '&i' + cDocNM
               aA[ PRT_DOC  ] := '@n'+ cDocNM

            CASE aA[ PRT_CAST ] $ cIntLong .and. aA[ PRT_L_FAR ]
               aadd( aPre, { aA[ PRT_CAST ] + ' i' + cDocNM + ' = 0;', nHBIdx, 'i'+ cDocNM, 'hb_stornl' } )
               aA[ PRT_BODY ] := '&i' + cDocNM
               aA[ PRT_DOC  ] := '@n'+ cDocNM

            CASE aA[ PRT_CAST ] $ cIntLongLong .and. aA[ PRT_L_FAR ]
               aadd( aPre, { aA[ PRT_CAST ] + ' i' + cDocNM + ' = 0;', nHBIdx, 'i'+ cDocNM, 'hb_stornint' } )
               aA[ PRT_BODY ] := '&i' + cDocNM
               aA[ PRT_DOC  ] := '@n'+ cDocNM

            CASE aA[ PRT_CAST ] $ cInt
               s := 'hb_parni( '+ cHBIdx +' )'
               IF !empty( aA[ PRT_DEFAULT ] ) .AND. !( aA[ PRT_DEFAULT ] == "0" )
                  aA[ PRT_BODY ] := '( HB_ISNUM( '+cHBIdx+' ) ? ' + s + ' : ' + aA[ PRT_DEFAULT ] + ' )'
               ELSE
                  aA[ PRT_BODY ] := s
               ENDIF
               aA[ PRT_DOC  ] := 'n'+ cDocNM

            CASE aA[ PRT_CAST ] $ cIntLong
               s := 'hb_parnl( '+ cHBIdx +' )'
               IF !empty( aA[ PRT_DEFAULT ] ) .AND. !( aA[ PRT_DEFAULT ] == "0" )
                  aA[ PRT_BODY ] := '( HB_ISNUM( '+cHBIdx+' ) ? ' + s + ' : ' + aA[ PRT_DEFAULT ] + ' )'
               ELSE
                  aA[ PRT_BODY ] := s
               ENDIF
               aA[ PRT_DOC  ] := 'n'+ cDocNM

            CASE aA[ PRT_CAST ] $ cIntLongLong
               s := 'hb_parnint( '+ cHBIdx +' )'
               IF !empty( aA[ PRT_DEFAULT ] ) .AND. !( aA[ PRT_DEFAULT ] == "0" )
                  aA[ PRT_BODY ] := '( HB_ISNUM( '+cHBIdx+' ) ? ' + s + ' : ' + aA[ PRT_DEFAULT ] + ' )'
               ELSE
                  aA[ PRT_BODY ] := s
               ENDIF
               aA[ PRT_DOC  ] := 'n'+ cDocNM

            CASE aA[ PRT_CAST ] $ 'double,qreal' .and. aA[ PRT_L_FAR ]
               aadd( aPre, { 'qreal qr' + cDocNM + ' = 0;', nHBIdx, 'qr'+ cDocNM, 'hb_stornd'  } )
               aA[ PRT_BODY ] := '&qr' + cDocNM
               aA[ PRT_DOC  ] := '@n'+ cDocNM

            CASE aA[ PRT_CAST ] $ 'double,qreal,float'
               aA[ PRT_BODY ] := 'hb_parnd( '+ cHBIdx +' )'
               aA[ PRT_DOC  ] := 'n'+ cDocNM

            CASE aA[ PRT_CAST ] == 'uchar'
               aA[ PRT_BODY ] := '( char ) hb_parni( '+ cHBIdx +' )'
               aA[ PRT_DOC  ] := 'n'+ cDocNM

            CASE ( '::' $ aA[ PRT_CAST ] ) .and. aA[ PRT_L_FAR ]
               aadd( aPre, { aA[ PRT_CAST ]+' i' + cDocNM + ';', nHBIdx, 'i'+ cDocNM, 'hb_storni' } )
               aA[ PRT_BODY ] := '&i' + cDocNM
               aA[ PRT_DOC  ] := '@n'+ cDocNM

            CASE ( '::' $ aA[ PRT_CAST ] )
               s := '( '+ aA[ PRT_CAST ] +' ) hb_parni( '+ cHBIdx +' )'
               IF !empty( aA[ PRT_DEFAULT ] ) .AND. !( aA[ PRT_DEFAULT ] == "0" )
                  IF ascan( aEnum, aA[ PRT_DEFAULT ] ) > 0
                     ss := cWidget+'::'+aA[ PRT_DEFAULT ]
                  ELSE
                     ss := iif( '::' $ aA[ PRT_DEFAULT ], aA[ PRT_DEFAULT ], ;
                        iif( isDigit( left( aA[ PRT_DEFAULT ], 1 ) ), aA[ PRT_DEFAULT ], cWidget+'::'+aA[ PRT_DEFAULT ] ) )
                  ENDIF
                  ss := '( '+ aA[ PRT_CAST ] +' ) '+ss
                  aA[ PRT_BODY ] := '( HB_ISNUM( '+cHBIdx+' ) ? ' + s + ' : ' + ss + ' )'
               ELSE
                  aA[ PRT_BODY ] := s
               ENDIF
               aA[ PRT_DOC  ] := 'n'+ cDocNM

            CASE aA[ PRT_CAST ] == 'bool' .and. aA[ PRT_L_FAR ]
               aadd( aPre, { 'bool i' + cDocNM + ' = 0;', nHBIdx, 'i'+ cDocNM, 'hb_stornl' } )
               aA[ PRT_BODY ] := '&i' + cDocNM
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
                  //aA[ PRT_BODY ] := '( HB_ISNIL( '+cHBIdx+' ) ? ' + aA[ PRT_DEFAULT ] +' : '+ s +' )'
                  aA[ PRT_BODY ] := '( HB_ISPOINTER( '+cHBIdx+' ) ? '+ s + ' : ' +aA[ PRT_DEFAULT ] +' )'
               ELSE
                  aA[ PRT_BODY ] := s
               ENDIF
               aA[ PRT_DOC  ] := 'p'+ cDocNM

            CASE aA[ PRT_L_AND ]
               aA[ PRT_BODY ] := '*hbqt_par_' + aA[ PRT_CAST ] + '( ' + cHBIdx + ' )'
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
               cCmd := 'hb_retptr( '+ cCmn +' )'
               cPrgRet := 'p' + cDocNM

            CASE aA[ PRT_CAST ] == 'void'
               cCmd := cCmn
               cPrgRet := 'NIL'

            CASE aA[ PRT_CAST ] $ cInt
               cCmd := 'hb_retni( '+ cCmn +' )'
               cPrgRet := 'n' + cDocNM

            CASE aA[ PRT_CAST ] $ cIntLong
               cCmd := 'hb_retnl( '+ cCmn +' )'
               cPrgRet := 'n' + cDocNM

            CASE aA[ PRT_CAST ] $ cIntLongLong
               cCmd := 'hb_retnint( '+ cCmn +' )'
               cPrgRet := 'n' + cDocNM

            CASE aA[ PRT_CAST ] $ 'double,qreal'
               cCmd := 'hb_retnd( '+ cCmn +' )'
               cPrgRet := 'n' + cDocNM

            CASE ( '::' $ aA[ PRT_CAST ] )
               cCmd := 'hb_retni( ( '+ aA[ PRT_CAST ] +' ) ' + cCmn +' )'
               cPrgRet := 'n' + cDocNM

            CASE aA[ PRT_CAST ] == 'bool'
               cCmd := 'hb_retl( '+ cCmn +' )'
               cPrgRet := 'l' + cDocNM

            CASE aA[ PRT_CAST ] == 'char' .AND. aA[ PRT_L_FAR ]
               cCmd := 'hb_retc( '+ cCmn +' )'
               cPrgRet := 'c' + cDocNM

            CASE aA[ PRT_CAST ] == 'char'
               cCmd := 'hb_retni( '+ cCmn +' )'
               cPrgRet := 'c' + cDocNM

            CASE aA[ PRT_CAST ] == 'QString'
               cCmd := 'hb_retc( '+ cCmn +'.toLatin1().data()' +' )'
               cPrgRet := 'c' + cDocNM

            CASE aA[ PRT_CAST ] == 'FT_Face'
               cCmd := 'hb_retc( '+ cCmn +' )'
               cPrgRet := 'c' + cDocNM

            CASE aA[ PRT_L_FAR ]
               cCmd := 'hb_retptr( ( '+ aA[ PRT_CAST ] + '* ) ' + cCmn + ' )'
               cPrgRet := 'p' + cDocNM

            CASE aA[ PRT_L_AND ] .and. aA[ PRT_L_CONST ]
               cCmd := 'hb_retptr( new '+ aA[ PRT_CAST ] + '( ' + cCmn + ' ) )'
               cPrgRet := 'p' + cDocNM

            CASE aA[ PRT_L_CONST ]
               cCmd := 'hb_retptr( new '+ aA[ PRT_CAST ] + '( ' + cCmn + ' ) )'
               cPrgRet := 'p' + cDocNM

            CASE aA[ PRT_L_AND ]
               cCmd := 'hb_retptr( ( '+ aA[ PRT_CAST ] + '* ) ' + cCmn + ' )'
               cPrgRet := 'p' + cDocNM

            OTHERWISE
               /* No attribute is attached to return value */
               IF left( aA[ PRT_CAST ], 1 ) == 'Q'
                  cCmd := 'hb_retptr( new '+ aA[ PRT_CAST ] + '( ' + cCmn + ' ) )'
                  cPrgRet := 'p' + cDocNM

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
                        iif( empty( cDocs ), '', ', '+ cDocs ) +' ) -> '+ cPrgRet )
      aadd( doc_, '' )
   ENDIF

   RETURN lSuccess

/*----------------------------------------------------------------------*/

STATIC FUNCTION ParseVariables( cProto, cWidget, txt_, doc_, aEnum, func_ )
   LOCAL aRet, aA, aPre
   LOCAL n
   LOCAL cRet, cFun, cDocs, cCmd
   LOCAL cWdg, cCmn, cPrgRet, cHBFunc, cDocNM
   LOCAL lSuccess
   LOCAL cInt := 'int,qint16,quint16,QChar'
   LOCAL cIntLong := 'qint32,quint32,QRgb'
   LOCAL cIntLongLong := 'qint64,quint64,qlonglong,qulonglong'

   aPre   := {}
   cDocs  := ''

   aRet := {}
   n := at( ' ', cProto )
   IF n > 0
      IF .t.
         cRet := alltrim( substr( cProto, 1, n-1 ) )
         cFun := alltrim( substr( cProto, n+1    ) )

         aRet := array( PRT_ATTRB_MAX )

         /* Normalize */
         aRet[ PRT_CAST ] := cRet
         aRet[ PRT_NAME ] := aRet[ PRT_CAST ]

         IF ascan( aEnum, {|e| iif( empty( e ), .f., e == aRet[ PRT_CAST ] ) } ) > 0
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
               cPrgRet := 'x' + cDocNM

            CASE aA[ PRT_CAST ] $ cInt
               cCmd := 'hb_retni( '+ cCmn +' )'
               cPrgRet := 'n' + cDocNM

            CASE aA[ PRT_CAST ] $ cIntLong
               cCmd := 'hb_retnl( '+ cCmn +' )'
               cPrgRet := 'n' + cDocNM

            CASE aA[ PRT_CAST ] $ cIntLongLong
               cCmd := 'hb_retnint( '+ cCmn +' )'
               cPrgRet := 'n' + cDocNM

            CASE aA[ PRT_CAST ] $ 'double,qreal'
               cCmd := 'hb_retnd( '+ cCmn +' )'
               cPrgRet := 'n' + cDocNM

            CASE ( '::' $ aA[ PRT_CAST ] )
               cCmd := 'hb_retni( ( '+ aA[ PRT_CAST ] +' ) ' + cCmn +' )'
               cPrgRet := 'n' + cDocNM

            CASE aA[ PRT_CAST ] == 'bool'
               cCmd := 'hb_retl( '+ cCmn +' )'
               cPrgRet := 'l' + cDocNM

            CASE aA[ PRT_CAST ] == 'char*'
               cCmd := 'hb_retc( '+ cCmn +' )'
               cPrgRet := 'c' + cDocNM

            CASE aA[ PRT_CAST ] == 'char'
               cCmd := 'hb_retni( '+ cCmn +' )'
               cPrgRet := 'c' + cDocNM

            CASE aA[ PRT_CAST ] == 'QString'
               cCmd := 'hb_retc( '+ cCmn +'.toLatin1().data()' +' )'
               cPrgRet := 'c' + cDocNM

            CASE aA[ PRT_CAST ] == 'FT_Face'
               cCmd := 'hb_retc( '+ cCmn +' )'
               cPrgRet := 'c' + cDocNM

            OTHERWISE
               /* No attribute is attached to return value */
               IF left( aA[ PRT_CAST ], 1 ) == 'Q'
                  cCmd := 'hb_retptr( new '+ aA[ PRT_CAST ] + '( ' + cCmn + ' ) )'
                  cPrgRet := 'p' + cDocNM

               ELSE
                  OutStd( '<<< ' + cProto + ' | ' + aA[ PRT_CAST ] + ' >>>'  + s_NewLine )
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
                        iif( empty( cDocs ), '', ', '+ cDocs ) +' ) -> '+ cPrgRet )
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
   s := 'CREATE CLASS '+ cWidget + iif( n > 0, ' INHERIT ' + cls_[ n,2 ], '' )

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
         cM    := iif( len( cM ) > 35, cM, pad( cM,35 ) )

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
