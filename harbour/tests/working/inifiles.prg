//
// $Id$
//

#define IF_BUFFER 65535

function Main(cFilename, cSection)
   local oIni := TIniFile():New(Default( cFilename, "harbour.ini" ) )
   local s, n := Val( Default( cSection, "1" ) )

   qout('')
   qout('Sections:')
   s := oIni:ReadSections()
   aeval(s, {|x| qout('[' + x + ']')})

   qout('')
   qout('[' + s[n] + ']')
   s := oIni:ReadSection(s[n])
   aeval(s, {|x| qout(x)})

   oIni:WriteDate('Date Test', 'Today', Date() )
   oIni:WriteBool('Bool Test', 'True', .t.)
   qout( oIni:ReadBool('Bool Test', 'True', .f.) )

   oIni:UpdateFile()
return nil

function TIniFile()
   static oClass

   if oClass == nil
      oClass := TClass():New( "TINIFILE" ) // starts a new class definition

      oClass:AddData( "FileName" )           // define this class objects datas
      oClass:AddData( "Contents" )

      oClass:AddMethod( "New",  @New() )  // define this class objects methods
      oClass:AddMethod( "ReadString", @ReadString() )
      oClass:AddMethod( "WriteString", @WriteString() )
      oClass:AddMethod( "ReadNumber", @ReadNumber() )
      oClass:AddMethod( "WriteNumber", @WriteNumber() )
      oClass:AddMethod( "ReadDate", @ReadDate() )
      oClass:AddMethod( "WriteDate", @WriteDate() )
      oClass:AddMethod( "ReadBool", @ReadBool() )
      oClass:AddMethod( "WriteBool", @WriteBool() )
      oClass:AddMethod( "ReadSection", @ReadSection() )
      oClass:AddMethod( "ReadSections", @ReadSections() )
      oClass:AddMethod( "DeleteKey", @DeleteKey() )
      oClass:AddMethod( "EraseSection", @EraseSection() )
      oClass:AddMethod( "UpdateFile", @UpdateFile() )

      oClass:Create()                     // builds this class
   endif
return oClass:Instance()                  // builds an object of this class

static function New(cFileName)
   local Self := QSelf()
   local Done, hFile, cFile, cLine, cIdent, nPos
   local CurrArray

   if empty(cFileName)
      // raise an error?
      outerr('No filename passed to TIniFile():New()')
      return nil

   else
      ::FileName := cFilename
      ::Contents := {}
      CurrArray := ::Contents

      if File(cFileName)
         hFile := fopen(cFilename, 0)

      else
         hFile := fcreate(cFilename)
      endif

      Done := .f.
      cFile := space(1)
      while !Done
         cLine := ''

         while !Done
            Done := (fread(hFile, @cFile, 1) <= 0)

            if !cFile $ chr(10) + chr(13)
               cLine += cFile

            else
               exit
            endif
         end

         if !empty(cLine)
            if Left(cLine, 1) == '[' // new section
               if (nPos := At(']', cLine)) > 1
                  cLine := substr(cLine, 2, nPos - 2)
               else
                  cLine := substr(cLine, 2)
               endif

               AAdd(::Contents, { cLine, { /* this will be CurrArray */ } } )
               CurrArray := ::Contents[Len(::Contents)][2]

            elseif Left(cLine, 1) == ';' // preserve comments
               AAdd( CurrArray, { NIL, cLine } )

            else
               if (nPos := At('=', cLine)) > 0
                  cIdent := Left(cLine, nPos - 1)
                  cLine := SubStr(cLine, nPos + 1)

                  AAdd( CurrArray, { cIdent, cLine } )

               else
                  AAdd( CurrArray, { cLine, '' } )
               endif
            endif
         endif
      end

      fclose(hFile)
   endif
return Self

static function ReadString(cSection, cIdent, cDefault)
   local Self := QSelf()
   local cResult := cDefault
   local j, i := AScan( ::Contents, {|x| x[1] == cSection} )

   if i > 0
      j := AScan( ::Contents[i][2], {|x| x[1] == cIdent} )

      if j > 0
         cResult := ::Contents[i][2][j][2]
      endif
   endif
return cResult

static procedure WriteString(cSection, cIdent, cString)
   local Self := QSelf()
   local j, i

   if Empty(cIdent)
      outerr('Must specify an identifier')

   elseif Empty(cSection)
      j := AScan( ::Contents, {|x| x[1] == cIdent .and. ValType(x[2]) == 'C'} )

      if j > 0
         ::Contents[j][2] := cString

      else
         AAdd(::Contents, nil)
         AIns(::Contents, 1)
         ::Contents[1] := {cIdent, cString}
      endif

   elseif (i := AScan( ::Contents, ;
           {|x| x[1] == cSection .and. ValType(x[2]) == 'A'})) > 0
      j := AScan( ::Contents[i][2], {|x| x[1] == cIdent} )

      if j > 0
         ::Contents[i][2][j][2] := cString

      else
         AAdd( ::Contents[i][2], {cIdent, cString} )
      endif

   else
      AAdd( ::Contents, {cSection, {{cIdent, cString}}} )
   endif
return

static function ReadNumber(cSection, cIdent, nDefault)
   local Self := QSelf()
return Val( ::ReadString(cSection, cIdent, str(nDefault)) )

static procedure WriteNumber(cSection, cIdent, nNumber)
   local Self := QSelf()

   ::WriteString( cSection, cIdent, alltrim(str(nNumber)) )
return

static function ReadDate(cSection, cIdent, dDefault)
   local Self := QSelf()
return SToD( ::ReadString(cSection, cIdent, DToS(dDefault)) )

static procedure WriteDate(cSection, cIdent, dDate)
   local Self := QSelf()

   ::WriteString( cSection, cIdent, DToS(dDate) )
return

static function ReadBool(cSection, cIdent, lDefault)
   local Self := QSelf()
   local cDefault := Iif( lDefault, '.t.', '.f.' )

return ::ReadString(cSection, cIdent, cDefault) == '.t.'

static procedure WriteBool(cSection, cIdent, lBool)
   local Self := QSelf()

   ::WriteString( cSection, cIdent, Iif(lBool, '.t.', '.f.') )
return

static procedure DeleteKey(cSection, cIdent)
   local Self := QSelf()
   local j, i := AScan( ::Contents, {|x| x[1] == cSection} )

   if i > 0
      j := AScan( ::Contents[i][2], {|x| x[1] == cIdent} )

      ADel( ::Contents[i][2], j )
      ASize( ::Contents[i][2], Len(::Contents[i][2]) - 1 )
   endif
return

static procedure EraseSection(cSection)
   local Self := QSelf()
   local i

   if Empty(cSection)
      outerr('Must specify a section')

   elseif (i := AScan( ::Contents,;
           {|x| x[1] == cSection .and. ValType(x[2]) == 'A'})) > 0
      ADel( ::Contents, i )
      ASize( ::Contents, Len(::Contents) - 1 )
   endif
return

static function ReadSection(cSection)
   local Self := QSelf()
   local i, j, aSection := {}

   if Empty(cSection)
      outerr('Must specify a section')

   elseif (i := AScan( ::Contents, ;
           {|x| x[1] == cSection .and. ValType(x[2]) == 'A'})) > 0

      for j := 1 to Len(::Contents[i][2])

         if ::Contents[i][2][j][1] <> NIL
            AAdd(aSection, ::Contents[i][2][j][1])
         endif
      next
   endif
return aSection

static function ReadSections()
   local Self := QSelf()
   local i, aSections := {}

   for i := 1 to Len(::Contents)

      if ValType(::Contents[i][2]) == 'A'
         AAdd(aSections, ::Contents[i][1])
      endif
   next
return aSections

static procedure UpdateFile()
   local Self := QSelf()
   local i, j, hFile

   hFile := fcreate(::Filename)

   for i := 1 to Len(::Contents)
      if ::Contents[i][1] == NIL
         fwrite(hFile, ::Contents[i][2] + Chr(13) + Chr(10))

      elseif ValType(::Contents[i][2]) == 'A'
         fwrite(hFile, '[' + ::Contents[i][1] + ']' + Chr(13) + Chr(10))
         for j := 1 to Len(::Contents[i][2])

            if ::Contents[i][2][j][1] == NIL
               fwrite(hFile, ::Contents[i][2][j][2] + Chr(13) + Chr(10))

            else
               fwrite(hFile, ::Contents[i][2][j][1] + '=' + ;
                      ::Contents[i][2][j][2] + Chr(13) + Chr(10))
            endif
         next
         fwrite(hFile, Chr(13) + Chr(10))

      elseif ValType(::Contents[i][2]) == 'C'
         fwrite(hFile, ::Contents[i][1] + '=' + ::Contents[i][2] + ;
                       Chr(13) + Chr(10))

      endif
   next
   fclose(hFile)
return

