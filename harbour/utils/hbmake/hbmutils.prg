#include "common.ch"
#ifndef __HARBOUR__
#include 'hbclip.ch'
#else
DECLARE extenprg( cExt AS STRING, nType AS NUMERIC ) AS STRING
declare exten( cExt as string, nType as numeric ) as string
DECLARE GetSourceFiles( lSubdir as logical ) as ARRAY
DECLARE GetDirs( cPat as USUAL ) as Array
DECLARE GetBccDir() as String
DECLARE GetVccDir() as String
DECLARE GetMakeDir() as String
DECLARE HB_ARGV( n as numeric ) as string
declare hbmake_filedate( c as String ) as string
declare listasArray2( cString as String, cSep as String ) as Array
#endif
Function GetSourceFiles( lSubdir )

     Local adirs AS ARRAY
     Local aRet AS ARRAY := {}
     Local lLinux    := At( 'linux', lower(Os() )) > 0
     Local cdir as String := If( !llinux, '\' + Curdir() + '\', '/' + Curdir() + '/' )
     Local aStru     := { cDir }
     Local aData AS ARRAY
     Local nCounter as numeric := 0
     Local nArrayLen as numeric
     Local nDatalen as numeric
     Local y as numeric
     Local cItem as String
     Local cext
     Local cpath
     Local cdrive
     Local nPos
     Local xItem
     local ccc,ddd
     Default lSubdir To .t.


     While ++ nCounter <= Len( aStru )
       If !Empty( adirs := GetDirs( astru[ nCounter ] ) )   // There are elements!
          Aeval( aDirs, { | xItem | Aadd( aStru, xItem ) } )
       Endif
     Enddo
     aDirs := {}

     Asort( aStru )
     nArrayLen := Len( aStru )

     For nCounter := 1 To nArrayLen
         
             
        If Len( aData := DIR_MULTI(aStru[ nCounter ]+"*.prg |"+aStru[ nCounter ]+"*.c") ) != 0
           
           nDataLen := Len( aData )
           
           For y := 1 To nDataLen
              If At( '.PRG', Upper( adata[ y, 1 ] ) ) > 0 .or. At( '.C', Upper( adata[ y, 1 ] ) ) > 0
                 If lSubdir
                    Aadd( aRet, Strtran( astru[ nCounter ], cDir, '' ) + Pad( aData[ y, 1 ], 13 ) + ;
                          Str( aData[ y, 2 ], 8 ) + '  ' + ;
                          Dtoc( aData[ y, 3 ] ) + '  ' + ;
                          aData[ y, 4 ] )
                 Elseif !lsubdir .and. At( If( lLinux, "/", "\" ), Strtran( astru[ nCounter ], cDir, '' ) ) == 0
                    Aadd( aRet, Pad( aData[ y, 1 ], 18 ) + ;
                          Str( aData[ y, 2 ], 8 ) + '  ' + ;
                          Dtoc( aData[ y, 3 ] ) + '  ' + ;
                          aData[ y, 4 ] )
                 Endif
              Endif
           Next
        Endif
     Next

     For nCounter := 1 To Len( aret )

        xItem := Substr( aret[ nCounter ], Rat( If( llinux, "/", '\' ), aret[ nCounter ] ) + 1 )

        nPos := Ascan( astru, { | x | x := Substr( x, Rat( If( llinux, "/", '\' ), x ) + 1 ), Left( x, At( ".", x ) ) == Left( xitem, At( ".", xitem ) ) } )
        If nPos > 0
           Adel( astru, nPos )
           Asize( astru, Len( astru ) - 1 )
        Endif

     Next
     For nCounter := 1 To Len( aStru )
        hb_FNAMESPLIT( Left( astru[ nCounter ], At( ' ', astru[ nCounter ] ) - 1 ), @cPath, @cItem, @cExt, @cDrive )
        If ( cExt == '.C' ) .or. ( cExt == ".c" )
           Aadd( aret, astru[ nCounter ] )
        Endif
     Next

Return aRet

Function extenprg( cExt, nType )

     Local aext AS ARRAY := { "C", "c" }
     Local nPos AS NUMERIC
     Local cTemp AS String := ""
     nPos := Ascan( aext, { | a | a == cExt } )
     If nPos > 0
        If nTYpe == 1
           cTemp := Strtran( cExt, aExt[ nPos ], 'prg' )
        Elseif ntype == 2
           cTemp := Strtran( cExt, aExt[ nPos ], 'prG' )
        Elseif ntype == 3
           cTemp := Strtran( cExt, aExt[ nPos ], 'pRg' )
        Elseif ntype == 4
           cTemp := Strtran( cExt, aExt[ nPos ], 'Prg' )
        Elseif ntype == 5
           cTemp := Strtran( cExt, aExt[ nPos ], 'PRg' )
        Elseif ntype == 6
           cTemp := Strtran( cExt, aExt[ nPos ], 'PrG' )
        Elseif ntype == 7
           cTemp := Strtran( cExt, aExt[ nPos ], 'PRG' )

        Endif
     Endif
Return ctemp

Static Function GetDirs( cPattern )

     Local aDir   := {}
     Local lLinux := At( 'linux', lower(Os()) ) > 0
     
     Aeval( Directory( cPattern + if(lLinux,"*","*."), "D" ), ;
            { | xItem | If( xItem[ 5 ] = "D" .and. ;
            ( xItem[ 1 ] != "." .and. xItem[ 1 ] != ".." ), ;
            ( Aadd( aDir, cPattern + xItem[ 1 ] + If( llinux, "/", '\' ) ), ;
            Outstd( "." ) ), "" ) } )

Return ( aDir )

Function GetBccDir()

     Local cPath := ''
     Local cEnv  := Gete( "PATH" )
     Local aEnv  := listasarray2( cEnv, ";" )
     Local nPos

     For nPos := 1 To Len( aEnv )
        If File( aenv[ nPos ] + '\bcc32.exe' ) .or. File( Upper( aenv[ nPos ] ) + '\BCC32.EXE' )
           cPath := aenv[ nPos ]
           cPath := Left( cPath, Rat( '\', cPath ) - 1 )
           Exit
        Endif
     Next

Return cPath
Function GetVccDir()

     Local cPath AS STRING := ''
     Local cEnv AS STRING := Gete( "PATH" )
     Local aEnv as array of string := listasarray2( cEnv, ";" )
     Local nPos as numeric

     For nPos := 1 To Len( aEnv )
        If File( aenv[ nPos ] + '\cl.exe' ) .or. File( Upper( aenv[ nPos ] ) + '\cl.EXE' )
           cPath := aenv[ nPos ]
           cPath := Left( cPath, Rat( '\', cPath ) - 1 )
           Exit
        Endif
     Next

Return cPath

Function exten( cExt, nType )

     Local aext as array := { 'C', 'c' }
     Local nPos as numeric
     Local cTemp as string := ""
     nPos := Ascan( aext, { | a | a == cExt } )
     If nPos > 0
        If nTYpe == 1
           cTemp := Strtran( cExt, aExt[ nPos ], 'o' )
        Elseif ntype == 2
           cTemp := Strtran( cExt, aExt[ nPos ], 'obj' )
        Endif
     Endif
Return ctemp
Function ListAsArray2( cList, cDelimiter )

     Local nPos as numeric
     Local aList as array := {}              // Define an empty array

     If cDelimiter = NIL
        cDelimiter := ","
     Endif
     //
     Do While ( nPos := At( cDelimiter, cList ) ) != 0
       Aadd( aList, Alltrim( Substr( cList, 1, nPos - 1 ) ) )                   // Add a new element
       cList := Substr( cList, nPos + 1 )
     Enddo
     Aadd( aList, Alltrim( cList ) )    // Add final element
     //
Return aList        // Return the array

Function GetMakeDir()

     Local cPath := ""
     Local cExe  := HB_ARGV( 0 )
    
     cExe:=strtran(cExe,"/","\")
     cPath := Left( cexe, Rat( "\", cexe ) - 1 )
     cPath := Left( cPath, Rat( "\", cPath ) - 1 )

Return cPath

Function GetSourceDirMacros()

     Local adirs AS ARRAY
     Local lLinux := At( 'linux', lower(Os()) ) > 0
     Local cdir as String := If( llinux, '/' + Curdir() + '/', '\' + Curdir() + '\' )
     Local aStru  := { cDir }

     Local nCounter as numeric := 0
     Local amacros as Array := {}
     While ++ nCounter <= Len( aStru )
       If !Empty( adirs := GetDirs( astru[ nCounter ] ) )   // There are elements!
          Aeval( aDirs, { | xItem | Aadd( aStru, xItem ) } )
       Endif
     Enddo
     For nCounter := 1 To Len( aStru )
        Aadd( amacros, { "SRC" + Strzero( nCounter, 2, 0 ), Strtran( astru[ nCounter ], cDir, '' ), .f. } )
     Next
Return amacros

Function hbmake_filedate( cFileName )

     Local aFiles := Directory( cFileName )

Return If( Len( aFiles ) == 1, aFiles[ 1, 3 ], Ctod( '' ) )

Function hbmake_filetime( cFileName )

     Local aFiles := Directory( cFileName )

Return If( Len( aFiles ) == 1, aFiles[ 1, 4 ], '' )

Function TD2JUL( CTIME, DDATE )

Return DDATE - Ctod( '01/01/1900' ) + ( PRB_INT( TTOS( CTIME ) / 100000,, 5 ) )

Function TTOS( CTIME )

Return ( Val( Substr( CTIME, 7, 2 ) ) ) + ;
         ( Val( Substr( CTIME, 4, 2 ) ) * 60 ) + ;
         ( Val( Substr( CTIME, 1, 2 ) ) * 3600 )

Function PRB_INT( SOMENUMBER, length, NUM_DECIMALS )

     Local NEGATIVE   := ( SOMENUMBER < 0 )
     Local SOMESTRING
     Local dotat

     Default NUM_DECIMALS To 0
     Default length To 19

     If NEGATIVE
        SOMENUMBER := Abs( SOMENUMBER )
     Endif

     SOMENUMBER += .0000000000000005

     SOMESTRING := Alltrim( Str( SOMENUMBER ) )

     dotat := At( '.', somestring )

     Do Case
         Case NUM_DECIMALS == 0
             If dotat > 0
                somestring := Left( somestring, dotat - 1 )
             Endif
         Case NUM_DECIMALS > 0
             If dotat > 0
                somestring := Left( somestring, dotat + num_decimals )
             Endif
     Endcase

     If NEGATIVE
        SOMESTRING := '-' + SOMESTRING
     Endif

Return Val( SOMESTRING )
Function exte( cExt, nType )

     Local aext  := { 'prg', 'prG', 'pRg', 'Prg', 'PRg', 'PrG', 'PRG' }
     Local nPos
     Local cTemp := ""
     nPos := Ascan( aext, { | a | a == cExt } )
     If nPos > 0
        If nTYpe == 1
           cTemp := Strtran( cExt, aExt[ nPos ], 'c' )
        Elseif ntype == 2
           cTemp := Strtran( cExt, aExt[ nPos ], 'obj' )
        Elseif ntype == 3
           cTemp := Strtran( cExt, aExt[ nPos ], 'o' )

        Endif
     Endif
Return ctemp
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

Function c( CSTRING )

Return Max( ( Maxcol() / 2 ) - Int( Len( CSTRING ) / 2 ), 0 )

Function ReadLN( leof )

     Local cBuffer := ""
     cBuffer := FT_FREADLN()
     cBuffer := Strtran( cBuffer, Chr( 13 ), '' )
     cBuffer := Strtran( cBuffer, Chr( 10 ), '' )
     FT_FSKIP( 1 )
     leof := ft_FEOF()
Return cBuffer

Function GetinstaledLibs( clibs, lGcc )

     Local adeflib     := { 'lang' + If( lgcc, '.a', '.lib' ), 'vm' + If( lgcc, '.a', '.lib' ), 'rtl' + If( lgcc, '.a', '.lib' ), 'rdd' + If( lgcc, '.a', '.lib' ), 'macro' + If( lgcc, '.a', '.lib' ), 'pp' + If( lgcc, '.a', '.lib' ), 'dbfntx' + If( lgcc, '.a', '.lib' ), 'dbfcdx' + If( lgcc, '.a', '.lib' ), 'common' + If( lgcc, '.a', '.lib' ), 'gtwin' + If( lgcc, '.a', '.lib' ), 'debug' + If( lgcc, '.a', '.lib' ), 'gtpca' + If( lgcc, '.a', '.lib' ), 'gtdos' + If( lgcc, '.a', '.lib' ), 'gtsln' + If( lgcc, '.a', '.lib' ), 'gtstd' + If( lgcc, '.a', '.lib' ), 'zlib1' + If( lgcc, '.a', '.lib' ), 'ziparchive' + If( lgcc, '.a', '.lib' ), 'rddads' + If( lgcc, '.a', '.lib' ), 'ace32' + If( lgcc, '.a', '.lib' ), 'libnf' + If( lgcc, '.a', '.lib' ), 'libct' + If( lgcc, '.a', '.lib' ), 'htmllib' + If( lgcc, '.a', '.lib' ), 'libgt' + If( lgcc, '.a', '.lib' ), 'libmisc' + If( lgcc, '.a', '.lib' ), 'mysql' + If( lgcc, '.a', '.lib' ), 'libmysql' + If( lgcc, '.a', '.lib' ), 'mysqlclient' + If( lgcc, '.a', '.lib' ), 'samples' + If( lgcc, '.a', '.lib' ), 'pdflib' + If( lgcc, '.a', '.lib' ), 'nulsys' + If( lgcc, '.a', '.lib' ), 'gtcgi' + If( lgcc, '.a', '.lib' ) }
     Local aReturnLibs := {}
     Local aLibs       := Directory( clibs )
     Local nPos
     Local nCount
     Local citem
     if lgcc
        aeval(aLibs,{|x,y| citem:=x[1] ,if(left(citem,3)=="lib", alibs[y,1]:=substr(cItem,4),)})
    endif

     For ncount := 1 To Len( alibs )
        citem := Lower( alibs[ ncount, 1 ] )
        npos  := Ascan( adeflib, { | a | Lower( a ) == citem } )
        If npos == 0
           Aadd( aReturnLibs, alibs[ ncount, 1 ] )
        Endif
     Next
    
Return aReturnLibs
Function Getlibs( lgcc ,cDir)
     Local lLinux:=at('linux',lower(os()))>0
     Local ainstaledlibs := Getinstaledlibs( If( !llinux, if(!lgcc, cDir+"\*.lib",cDir+"\*.a") , '/usr/lib/harbour/*.a' ),lGcc )
     Local aLibsDesc     := { { "Harbour Ct3 library - Libct", 'ct' + If( lgcc, '.a', '.lib' ) }, ;
                          { "Harbour Misc library - Libmisc", 'misc' + If( lgcc, '.a', '.lib' ) }, ;
                          { "Harbour html library - Htmllib", 'html' + If( lgcc, '.a', '.lib' ) }, ;
                          { "Harbour Nanfor library - Libnf", 'nf' + If( lgcc, '.a', '.lib' ) }, ;
                          { "Harbour Gt library - Libgt", 'nf' + If( lgcc, '.a', '.lib' ) }, ;
                          { "Harbour Zip library Zlib1", 'zlib1.lib ziparchive' + If( lgcc, '.a', '.lib' ) }, ;
                          { "Harbour Hbole library Hbole", 'hbole' + If( lgcc, '.a', '.lib' ) + ' ole2' + If( lgcc, '.a', '.lib' ) }, ;
                          { "Harbour Mysql library - MySql", 'mysql' + If( lgcc, '.a', '.lib' ) + ' libmysql' + If( lgcc, '.a', '.lib' ) + ' mysqlclient' + If( lgcc, '.a', '.lib' ) }, ;
                          { "Harbour Samples library - Samples", 'samples' + If( lgcc, '.a', '.lib' ) } }
     Aeval( ainstaledlibs, { | x | Aadd( aLibsDesc, { "User - " + x +" Library", x } ) } )
Return aLibsDesc

*+ EOF: HBMUTILS.PRG
FUNCTION DIR_MULTI( cFileMaskList, cAttr )
   LOCAL aList := ListasArray2( cFileMaskList, "|" )
   AEval( aList, {|tmp, tmp1| aList[ tmp1 ] := DIRECTORY( tmp, cAttr ) })
   RETURN ArrayAJoin(alist)
FUNCTION ArrayAJoin( array )
     LOCAL tmp
     LOCAL nLenArray := Len( array )
     LOCAL nLen
     LOCAL nPos := Len( array[ 1 ] ) + 1

     nLen := 0
     FOR tmp := 1 TO nLenArray
          nLen += Len( array[ tmp ] )
     NEXT

     ASize( array[ 1 ], nLen )

     FOR tmp := 2 TO nLenArray
          ACopy( array[ tmp ], array[ 1 ], , , nPos )
          nPos += Len( array[ tmp ] )
     NEXT

     RETURN array[ 1 ]
