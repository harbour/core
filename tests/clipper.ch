/*
 * Harbour compatibility header for
 * other Clipper compatible languages
 *
 * Copyright 2013 Viktor Szakats (vszakats.net/harbour)
 *
 */

#ifndef __HARBOUR__

#ifndef __XPP__
   #ifndef __CLIP__
      #ifndef FlagShip
         #define __CLIPPER__
      #endif
   #endif
#endif

#ifdef __CLIPPER__
   #ifndef __PLATFORM__DOS
   #define __PLATFORM__DOS
   #endif
#endif

#xtranslate hb_eol()                       => ( Chr( 13 ) + Chr( 10 ) )
#xtranslate hb_ps()                        => "\"
#xtranslate hb_osFileMask()                => "*.*"
#xtranslate hb_run( <c> )                  => __Run( <c> )

#xtranslate hb_dbExists( <t> )             => File( <t> )
#xtranslate hb_dbDrop( <t> )               => FErase( <t> )

#xtranslate HB_ISSTRING( <v> )             => ( ValType( <v> ) $ "CM" )
#xtranslate HB_ISNUMERIC( <v> )            => ( ValType( <v> ) == "N" )
#xtranslate HB_ISLOGICAL( <v> )            => ( ValType( <v> ) == "L" )
#xtranslate HB_ISARRAY( <v> )              => ( ValType( <v> ) == "A" )
#xtranslate HB_ISOBJECT( <v> )             => ( ValType( <v> ) == "O" )
#xtranslate HB_ISBLOCK( <v> )              => ( ValType( <v> ) == "B" )
#xtranslate HB_ISEVALITEM( <v> )           => ( ValType( <v> ) == "B" )

#xtranslate hb_keyCode( <n> )              => Asc( <n> )
#xtranslate hb_keyChar( <c> )              => Chr( <c> )
#xtranslate hb_keyPut( <n> )               => __Keyboard( Chr( <n> ) )
#xtranslate hb_keyIns( <n> )               => __Keyboard( Chr( <n> ) )

#xtranslate hb_BPeek( <c>, <p> )           => Asc( SubStr( <c>, <p>, 1 ) )
#xtranslate hb_BCode( <c> )                => Asc( <c> )
#xtranslate hb_BChar( <n> )                => Chr( <n> )
#xtranslate hb_BLen( <c> )                 => Len( <c> )
#xtranslate hb_BSubStr( <c>, <p>[, <l>] )  => SubStr( <c>, <p>, <l> )
#xtranslate hb_BLeft( <c>, <l> )           => Left( <c>, <l> )
#xtranslate hb_BRight( <c>, <l> )          => Right( <c>, <l> )
#xtranslate hb_BStrTran( <c>, <s>[, <r>] ) => StrTran( <c>, <s>, <r> )

#xtranslate hb_ntos( <n> )                 => LTrim( Str( <n> ) )
#xtranslate hb_SToD( [<s>] )               => Eval( {| s, df, dt | df := Set( _SET_DATEFORMAT, "YYYY/MM/DD" ), dt := CToD( Stuff( Stuff( s, 7, 0, "/" ), 5, 0, "/" ) ), Set( _SET_DATEFORMAT, df ), dt }, <s> )
#xtranslate hb_Compiler()                  => "C"
#xtranslate hb_cdpIsUTF8( [<c>] )          => .F.
#xtranslate hb_cdpCharMax()                => 255
#xtranslate hb_IsFunction( c )             => ( Type( c + "()" ) == "UI" )
#xtranslate hb_NToColor( <n> )             => LTrim( Str( Int( <n> % 16 ), 2 ) ) + "/" + LTrim( Str( Int( <n> / 16 ), 2 ) )
#xtranslate __mvScope()                    => -1
#xtranslate HB_SYMBOL_UNUSED( <v> )        =>

#endif
