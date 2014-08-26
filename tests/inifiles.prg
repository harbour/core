/* Copyright (C) 1999 Matthew Hamilton */

PROCEDURE Main( cFilename, cSection )

   LOCAL oIni := TIniFile():New( hb_defaultValue( cFilename, hb_FNameExtSet( __FILE__, ".ini" ) ) )

   LOCAL n := Val( hb_defaultValue( cSection, "1" ) )
   LOCAL s

   ?
   ? "Sections:"
   AEval( s := oIni:ReadSections(), {| x | QOut( "[" + x + "]" ) } )

   ?
   ? "[" + s[ n ] + "]"
   AEval( oIni:ReadSection( s[ n ] ), {| x | QOut( x ) } )

   oIni:WriteDate( "Date Test", "Today", 0d20121208 )
   oIni:WriteBool( "Bool Test", "True", .T. )
   ? oIni:ReadBool( "Bool Test", "True", .F. )

   oIni:UpdateFile()

   RETURN

SET PROCEDURE TO "inifile.prg"
