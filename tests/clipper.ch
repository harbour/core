/*
 * Harbour compatibility header for other Clipper compatible languages
 *
 * Copyright 2013-2015 Viktor Szakats (vszakats.net/harbour)
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
   #ifndef __ARCH16BIT__
      #define __ARCH16BIT__
   #endif
   #ifndef __LITTLE_ENDIAN__
      #define __LITTLE_ENDIAN__
   #endif
#endif

#xtranslate hb_eol()                       => ( Chr( 13 ) + Chr( 10 ) )
#xtranslate hb_ps()                        => "\"
#xtranslate hb_run( <c> )                  => __Run( <c> )
#xtranslate hb_osFileMask()                => "*.*"
#xtranslate hb_osDriveSeparator()          => ":"
#xtranslate hb_osError()                   => DOSError()
#xtranslate hb_osPathDelimiters()          => "\:"
#xtranslate hb_osPathListSeparator()       => ";"
#xtranslate hb_UserName()                  => GetEnv( "USERNAME" )
#xtranslate __BreakBlock()                 => {| o | Break( o ) }

#xtranslate hb_DirSepToOS( <d> )           => StrTran( <d>, "/", "\" )

#xtranslate hb_MemoRead( <x> )             => MemoRead( <x> )
#xtranslate hb_MemoWrit( [<x,...>] )       => MemoWrit( <x> )
#xtranslate hb_vfExists( <x> )             => File( <x> )
#xtranslate hb_FileExists( <x> )           => File( <x> )
#xtranslate hb_FileMatch( <x>, <y> )       => ( Lower( <x> ) == Lower( <y> ) )  /* ~ */
#xtranslate hb_vfDirectory( [<x,...>] )    => Directory( <x> )
#xtranslate hb_Directory( [<x,...>] )      => Directory( <x> )
#xtranslate hb_vfRename( [<x,...>] )       => FRename( <x> )
#xtranslate hb_vfErase( <x> )              => FErase( <x> )
#xtranslate hb_vfCopyFile( [<x,...>] )     => __CopyFile( <x> )
#xtranslate hb_vfDirMake( <x> )            => MakeDir( <x> )
#xtranslate hb_DirCreate( <x> )            => MakeDir( <x> )
#xtranslate hb_vfDirSpace( <x> )           => DiskSpace( Left( <x>, 2 ) )
#xtranslate hb_DiskSpace( <x> )            => DiskSpace( Left( <x>, 2 ) )
#xtranslate hb_vfDirRemove( <x> )          => DirRemove( <x> )
#xtranslate hb_DirDelete( <x> )            => DirRemove( <x> )

#xtranslate hb_dbExists( <t> )             => File( <t> )
#xtranslate hb_dbDrop( <t> )               => FErase( <t> )
#xtranslate hb_dbZap()                     => __dbZap()
#xtranslate hb_dbPack()                    => __dbPack()

#xtranslate hb_default( @<v>, <x> )        => iif( StrTran( ValType( <v> ), "M", "C" ) == StrTran( ValType( <x> ), "M", "C" ),, <v> := <x>, )
#xtranslate hb_defaultValue( <v>, <x> )    => iif( StrTran( ValType( <v> ), "M", "C" ) == StrTran( ValType( <x> ), "M", "C" ), <v>, <x> )
#xtranslate __defaultNIL( <v>, <x> )       => iif( <v> == NIL, <x>, <v> )

#xtranslate HB_ISSTRING( <v> )             => ( ValType( <v> ) $ "CM" )
#xtranslate HB_ISNUMERIC( <v> )            => ( ValType( <v> ) == "N" )
#xtranslate HB_ISLOGICAL( <v> )            => ( ValType( <v> ) == "L" )
#xtranslate HB_ISDATE( <v> )               => ( ValType( <v> ) == "D" )
#xtranslate HB_ISARRAY( <v> )              => ( ValType( <v> ) == "A" )
#xtranslate HB_ISOBJECT( <v> )             => ( ValType( <v> ) == "O" )
#xtranslate HB_ISBLOCK( <v> )              => ( ValType( <v> ) == "B" )
#xtranslate HB_ISEVALITEM( <v> )           => ( ValType( <v> ) == "B" )
#xtranslate HB_ISCHAR( <v> )               => ( ValType( <v> ) == "C" )
#xtranslate HB_ISMEMO( <v> )               => ( ValType( <v> ) == "M" )
#xtranslate HB_ISDATETIME( <v> )           => .F.
#xtranslate HB_ISHASH( <v> )               => .F.
#xtranslate HB_ISHASHKEY( <v> )            => .F.
#xtranslate HB_ISNULL( <v> )               => Len( <v> ) == 0
#xtranslate HB_ISPOINTER( <v> )            => .F.
#xtranslate HB_ISSYMBOL( <v> )             => .F.
#xtranslate HB_ISTIMESTAMP( <v> )          => .F.

#xtranslate hb_LeftEq( <l>, <r> )          => ( Left( <l>, Len( <r> ) ) == <r> )
#xtranslate hb_LeftEqI( <l>, <r> )         => ( Left( Lower( <l> ), Len( <r> ) ) == Lower( <r> ) )
#xtranslate hb_StrShrink( <l> )            => Left( <l>, Len( <l> ) - 1 )
#xtranslate hb_StrShrink( <l>, <r> )       => Left( <l>, Len( <l> ) - <r> )
#xtranslate hb_RAt( <c>, <p> )             => RAt( <c>, <p> )

#xtranslate hb_keyStd( <n> )               => ( <n> )
#xtranslate hb_keyCode( <n> )              => Asc( <n> )
#xtranslate hb_keyChar( <c> )              => iif( <c> >= 32 .AND. <c> <= 255 .AND. <c> != 127, Chr( <c> ), "" )
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
#xtranslate hb_BAt( <c>, <p> )             => At( <c>, <p> )
#xtranslate hb_BRAt( <c>, <p> )            => RAt( <c>, <p> )

#xtranslate hb_UPeek( <c>, <p> )           => Asc( SubStr( <c>, <p>, 1 ) )
#xtranslate hb_UCode( <c> )                => Asc( <c> )
#xtranslate hb_UChar( <n> )                => Chr( <n> )
#xtranslate hb_ULen( <c> )                 => Len( <c> )
#xtranslate hb_USubStr( <c>, <p>[, <l>] )  => SubStr( <c>, <p>, <l> )
#xtranslate hb_ULeft( <c>, <l> )           => Left( <c>, <l> )
#xtranslate hb_URight( <c>, <l> )          => Right( <c>, <l> )
#xtranslate hb_UAt( <c>, <p> )             => At( <c>, <p> )

#ifdef B_SINGLE
   #define HB_B_SINGLE_UNI                 B_SINGLE
   #define HB_B_DOUBLE_UNI                 B_DOUBLE
   #define HB_B_SINGLE_DOUBLE_UNI          B_SINGLE_DOUBLE
   #define HB_B_DOUBLE_SINGLE_UNI          B_DOUBLE_SINGLE
#endif

#ifdef MENU_SEPARATOR
   #define HB_MENU_SEPARATOR_UNI           MENU_SEPARATOR
   #define HB_SEPARATOR_DOUBLE_UNI         SEPARATOR_DOUBLE
   #define HB_SEPARATOR_SINGLE_UNI         SEPARATOR_SINGLE
   #define HB_SEPARATOR_DOUBLE_SINGLE_UNI  SEPARATOR_DOUBLE_SINGLE
#endif

#xtranslate hb_DispBox( [<x,...>] )        => DispBox( <x> )
#xtranslate hb_DispOutAt( [<x,...>] )      => DispOutAt( <x> )
#xtranslate hb_DispOutAtBox( [<x,...>] )   => DispOutAt( <x> )

#xtranslate hb_gtVersion( [<n>] )          => "DOS"

#xtranslate hb_GetStdIn()                  => 0
#xtranslate hb_GetStdOut()                 => 1
#xtranslate hb_GetStdErr()                 => 2

#xtranslate hb_AIns( <a>, <p> )            => AIns( <a>, <p> )
#xtranslate hb_AIns( <a>, <p>, <v> )       => ( AIns( <a>, <p> ), <a>\[ <p> ]\ := <v>, <a> )
#xtranslate hb_AIns( <a>, <p>, <v>, <r> )  => ( iif( <r>, ( ASize( <a>, Len( <a> ) + 1 ) ), ), AIns( <a>, <p> ), <a>\[ <p> ]\ := <v>, <a> )
#xtranslate hb_ADel( <a>, <p> )            => ADel( <a>, <p> )
#xtranslate hb_ADel( <a>, <p>, <r> )       => ( ADel( <a>, <p> ), iif( <r>, ASize( <a>, Len( <a> ) - 1 ), ), <a> )

#xtranslate hb_ntos( <n> )                 => LTrim( Str( <n> ) )
#xtranslate hb_Date()                      => Date()
#xtranslate hb_Date( <y>, <m>, <d> )       => hb_SToD( StrZero( Int( <y> ), 4 ) + StrZero( Int( <m> ), 2 ) + StrZero( Int( <d> ), 2 ) )
#xtranslate hb_CToD( [<s>] )               => CToD( <s> )
#xtranslate hb_DToC( [<s>] )               => DToC( <s> )
#xtranslate hb_CToD( [<s>], <f> )          => Eval( {| s, df, dt | df := Set( _SET_DATEFORMAT, <f> ), dt := CToD( s ), Set( _SET_DATEFORMAT, df ), dt }, <s> )
#xtranslate hb_DToC( [<s>], <f> )          => Eval( {| s, df, dt | df := Set( _SET_DATEFORMAT, <f> ), dt := DToC( s ), Set( _SET_DATEFORMAT, df ), dt }, <s> )
#xtranslate hb_SToD( [<s>] )               => Eval( {| s, df, dt | df := Set( _SET_DATEFORMAT, "yyyy-mm-dd" ), dt := CToD( Stuff( Stuff( s, 7, 0, "-" ), 5, 0, "-" ) ), Set( _SET_DATEFORMAT, df ), dt }, <s> )
#xtranslate hb_Compiler()                  => "C"
#xtranslate hb_cdpIsUTF8( [<c>] )          => .F.
#xtranslate hb_cdpCharMax()                => 255
#xtranslate hb_IsFunction( <c> )           => ( Type( <c> + "()" ) == "UI" )
#xtranslate hb_NToColor( <n> )             => LTrim( Str( Int( <n> % 16 ), 2 ) ) + "/" + LTrim( Str( Int( <n> / 16 ), 2 ) )
#xtranslate __mvScope()                    => -1
#xtranslate HB_SYMBOL_UNUSED( <v> )        =>
#xtranslate hb_langSelect( [<x>] )         =>
#xtranslate hb_TToD( <x> )                 => <x>

#ifdef __CLIP__
   #xtranslate hb_SecondsCPU( [<x>] )      => SecondsCPU( [<x>] )
#endif
#ifdef FlagShip
   #xtranslate hb_SecondsCPU( [<x>] )      => SecondsCPU( [<x>] )
#endif
#ifdef __CLIPPER__
   #xtranslate hb_SecondsCPU( [<x>] )      => Seconds( [<x>] )
#endif

#endif
