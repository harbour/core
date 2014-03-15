/*
 * Harbour Project source code:
 *    RDD tests
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

//#define _TEST_CREATE_

#ifndef N_LOOP
   #define N_LOOP 15
#endif

#ifndef EOL
   #define EOL Chr( 13 ) + Chr( 10 )
#endif

#command ? => OutStd( EOL )
#command ? <xx,...> => OutStd( <xx>, EOL )
#command ?? =>
#command ?? <xx,...> => OutStd( <xx> )

// #command RDDTEST <x> => rdd_test( <x> )
// #command RDDTEST <f>, <r>, <x> => rdd_test( #<f>, <{f}>, <r>, <x> )

#ifdef _TEST_CREATE_
   #command RDDTESTC <*x*> => <x>; rddtst_wr( #<x> )
   #command RDDTESTF <x> => rddtst_wr( #<x>, <x> )
   #command RDDTEST  <*x*> => RDDTESTC <x>
   #command RDDTEST  <x> => RDDTESTF <x>
#else
   #command RDDTESTC <s>, <*x*> => <x>; rddtst_tst( #<x>, <s> )
   #command RDDTESTF <r>, <s>, <x> => rddtst_tst( #<x>, <s>, <x>, <r> )
   // #command RDDTEST  <s>, <*x*> => RDDTESTC <x>
#endif
#define _DBNAME "_tst"

REQUEST DBFCDX
field FSTR, FNUM

#include "fileio.ch"

#ifdef _TEST_CREATE_
  static s_hMake := F_ERROR
#endif
static s_nTested := 0
static s_nErrors := 0

/* list of functions which may return unexpected value in Clipper
   instead of documented NIL. If you will find others please add them */
static aBadRetFunc := { "DBSKIP", "DBGOTO", "DBDELETE", "DBRECALL", ;
                        "DBUNLOCK", "DBCOMMIT" }

#ifdef _TEST_SCOPE_
   #include "ord.ch"
   #include "dbinfo.ch"
#endif

#ifdef __HARBOUR__
#ifdef _TEST_ADS_
#include "ads.ch"
REQUEST ADS
init proc adstest_init()
   rddRegister( "ADS", 1 )
   AdsSetServerType( ADS_LOCAL_SERVER )
   // rddSetDefault( "ADS" )
   return
#endif
#endif

// REQUEST dbSeek, DBGOTO, DBGOTOP, DBGOBOTTOM, ORDSETFOCUS, ORDSCOPE

#ifdef _TEST_CREATE_
procedure main( cOutFile, rdd )
   test_init( rdd, cOutFile )
   test_main()
   test_close()
   return
#else
procedure main( rdd )
   test_init( rdd )
   test_main()
   test_close()
   return
#endif

static procedure test_init( rdd, cOutFile )
   local n, cOut, aDb := { { "FSTR", "C", 10, 0 }, { "FNUM", "N", 10, 0 } }

if Empty( rdd )
   #ifdef _TESTRDD
      rdd := _TESTRDD
   #else
      rdd := "DBFCDX"
   #endif
endif
rddSetDefault( rdd )
#ifdef _TEST_CREATE_
   if Empty( cOutFile )
      ? "Syntax: <outfile.prg> [<rddname>]"
      quit
   elseif ( s_hMake := FCreate( cOutFile ) ) == F_ERROR
      ? "Cannot create file:", cOutFile
      quit
   endif
   cOut := ""
   #ifdef _TEST_ADS_
      cOut += '#define _TEST_ADS_' + EOL
   #endif
   cOut += ;
      'REQUEST ' + rdd + EOL + ;
      '#define _TESTRDD "' + rdd + '"' + EOL + ;
      '#include "rddtst.prg"' + EOL + ;
      EOL + ;
      'PROCEDURE test_main()' + EOL + ;
      EOL
   if ! FWrite( s_hMake, cOut ) == Len( cOut )
      ? "write error."
      quit
   endif
#endif

   AEval( Directory( "./" + _DBNAME + ".??x" ), {| x | FErase( x[ 1 ] ) } )
   AEval( Directory( "./TG_?.??x" ), {| x | FErase( x[ 1 ] ) } )
   FErase( "./"+_DBNAME+".dbf" )
   ? "RDD:", rdd
   ? "creating database and index..."
   dbCreate( _DBNAME, aDb )
/*
   USE _DBNAME SHARED

   FLock()
   for n := 1 to N_LOOP
      dbAppend()
      field->FNUM := int( ( n + 2 ) / 3 )
      field->FSTR := Chr( FNUM + 48 )
      // ? FNUM, FSTR, RecNo(), Eof(), Bof()
   next
   dbCommit()
   dbUnlock()
*/
   return


static procedure test_close()
   local cOut
#ifdef _TEST_CREATE_
   if s_hMake != F_ERROR
      cOut := EOL + ;
         'RETURN' + EOL
      if ! FWrite( s_hMake, cOut ) == Len( cOut )
         ? "write error."
         quit
      endif
      FClose( s_hMake )
   endif
#else
   ?
   ? "Number of tests:", LTrim( Str( s_nTested ) )
   ? "Number of errors:", LTrim( Str( s_nErrors ) )
#endif
   dbCloseArea()
   AEval( Directory( "./" + _DBNAME + ".??x" ), {| x | FErase( x[ 1 ] ) } )
   AEval( Directory( "./TG_?.??x" ), {| x | FErase( x[ 1 ] ) } )
   FErase( "./" + _DBNAME + ".dbf" )
   ?
   return

static procedure rdd_retval()
   return

static function rdd_state()
   return { RecNo(), Bof(), Eof(), Found() }


static function itm2str( itm )
   local cStr := "", i
   do case
   case itm == NIL
      cStr += "NIL"
   case ValType( itm ) == "C"
      cStr += '"' + StrTran( itm, '"', '" + Chr( 34 ) + "') + '"'
   case ValType( itm ) == "N"
      cStr += LTrim( Str( itm ) )
   case ValType( itm ) == "L"
      cStr += iif( itm, ".T.", ".F." )
   case ValType( itm ) == "D"
      cStr += "CToD(" + DToC( itm ) + ")"
   case ValType( itm ) == "B"
      cStr += "{||" + itm2str( Eval( itm ) ) + "}"
   case ValType( itm ) == "A"
      cStr += "{"
      for i := 1 to Len( itm )
         cStr += iif( i == 1, "", "," ) + itm2str( itm[ i ] )
      next
      cStr += "}"
   endcase
   return cStr


#ifdef _TEST_CREATE_
static procedure rddtst_wr( cAction, xRet )
   local aState, cOut

   if AScan( aBadRetFunc, {| x | Upper( cAction ) = x + "(" } ) != 0
      xRet := NIL
   endif
   aState := rdd_state()
   if PCount() > 1
      cOut := "RDDTESTF " + itm2str( xRet ) + ", " + itm2str( aState ) + ", " + cAction + EOL
   else
      cOut := "RDDTESTC " + itm2str( aState ) + ", " + cAction + EOL
   endif
   if ! FWrite( s_hMake, cOut ) == Len( cOut )
      ? "write error."
      quit
   endif
   return
#else
// rddtst_tst( #<x>, <s>, <x>, <r> )
static procedure rddtst_tst( cAction, aExState, xRet, xExRet )
   local aState, lOK := ( .T. ), s1, s2, i

   aState := rdd_state()
   if PCount() >= 4
      if AScan( aBadRetFunc, {| x | Upper( cAction ) = x + "(" } ) != 0
         xRet := NIL
      endif
      if ! ValType( xRet ) == ValType( xExRet ) .or. ;
         ! iif( ValType( xRet ) == "B", Eval( xRet ) == Eval( xExRet ), xRet == xExRet )
         lOK := .F.
      endif
      s1 := itm2str( xRet )
      s2 := itm2str( xExRet )
      s1 := PadR( s1, Max( Len( s1 ), Len( s2 ) ) + 1 )
      s2 := PadR( s2, Len( s1 ) )
   else
      s1 := s2 := ""
   endif
   if ! Empty( aExState ) .and. lOK
      for i := 1 to Len( aExState )
         if ! ValType( aState[ i ] ) == ValType( aExState[ i ] ) .or. ! aState[ i ] == aExState[ i ]
            lOK := .F.
            exit
         endif
      next
   endif
   ?
   ?? iif( lOK, "OK  ", "ERR " ) + cAction, "=>", s1 + itm2str( aState )
   if ! lOK
      ?
      ?? "    " + cAction, "=>", s2 + itm2str( aExState )
      s_nErrors++
   endif
   s_nTested++
   return
#endif
