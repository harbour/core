/* This demo shows tbnames.dbf consisting of LAST, FIRST, ADDR, CITY,
   STATE, ZIP with active index on LAST + FIRST.  It shows last name,
   first name, city only for those last names that begin with letter
   that you input for the cKey GET.

   tbnames.dbf/.ntx are automatically created by this test program */

#require "hbnf"

PROCEDURE Main()

   LOCAL aFields := {}, cKey := "O", cOldColor
   LOCAL nFreeze := 1, lSaveScrn := .T., nRecSel
   LOCAL cColorList := "N/W, N/BG, B/W, B/BG, B/W, B/BG, R/W, B/R"
   LOCAL cColorShad := "N/N"
   LOCAL GetList := {}

   FIELD last, first

   IF ! hb_dbExists( "tbnames.dbf" )
      make_dbf()
   ENDIF

   USE tbnames.dbf

   IF ! hb_dbExists( "tbnames.dbf", "tbnames.ntx" )
      INDEX ON FIELD->LAST + FIELD->FIRST TO tbnames.ntx
   ENDIF

   SET INDEX TO tbnames.ntx

   // Pass Heading as character and Field as Block including Alias
   // To eliminate the need to use FieldWBlock() function in ft_BrwsWhl()

   AAdd( aFields, { "Last Name" , {|| tbnames->LAST }  } )
   AAdd( aFields, { "First Name", {|| tbnames->FIRST } } )
   AAdd( aFields, { "City"      , {|| tbnames->CITY }  } )

   cOldColor := SetColor( "N/BG" )
   CLS
   @ 5, 10 SAY "Enter First Letter Of Last Name:" GET cKey PICTURE "!"
   READ

   // hb_LeftEq( tbnames->LAST, cKey ) is the Conditional Block passed to this function
   // you can make it as complicated as you want, but you would then
   // have to modify TBWhileSet() to find first and last records
   // matching your key.
   nRecSel := ft_BrwsWhl( aFields, {|| hb_LeftEq( tbnames->LAST, cKey ) }, cKey, nFreeze, ;
      lSaveScrn, cColorList, cColorShad, 3, 6, MaxRow() - 2, MaxCol() - 6 )
   // Note you can use Compound Condition
   // such as cLast := "Pierce            " and cFirst := "Hawkeye  "
   // by changing above block to:
   //    {|| hb_LeftEq( tbnames->LAST, cLast ) .AND. hb_LeftEq( tbnames->FIRST, cFirst ) }
   // and setting cKey := cLast + cFirst

   ?
   IF nRecSel == 0
      ? "Sorry, NO Records Were Selected"
   ELSE
      ? "You Selected", ;
         tbnames->LAST, ;
         tbnames->FIRST, ;
         tbnames->CITY
   ENDIF
   ?

   dbCloseArea()
   hb_dbDrop( "tbnames.dbf" )
   hb_dbDrop( "tbnames.dbf", "tbnames.ntx" )

   WAIT
   SetColor( cOldColor )
   CLS

   RETURN

STATIC PROCEDURE make_dbf()

   LOCAL x, aData := { ;
      { "SHAEFER", "KATHRYN", "415 WEST CITRUS ROAD #150", "LOS ANGELES", "CA", "90030" }, ;
      { "OLSON", "JAMES", "225 NORTH RANCH ROAD", "LOS ANGELES", "CA", "90023"          }, ;
      { "KAYBEE", "JOHN", "123 SANDS ROAD", "CAMARILLO", "CA", "93010"                  }, ;
      { "HERMAN", "JIM", "123 TOON PAGE ROAD", "VENTURA", "CA", "93001"                 }, ;
      { "BURNS", "FRANK", "123 VIRGINA STREET", "OXNARD", "CA", "93030"                 }, ;
      { "PIERCE", "HAWKEYE", "123 OLD TOWN ROAD", "PORT MUGU", "CA", "93043"            }, ;
      { "MORGAN", "JESSICA", "123 FRONTAGE ROAD", "CAMARILLO", "CA", "93010"            }, ;
      { "POTTER", "ROBERT", "123 FIR STREET", "OXNARD", "CA", "93030"                   }, ;
      { "WORTH", "MARY", "123-1/2 JOHNSON DRIVE", "OXNARD", "CA", "93033"               }, ;
      { "JOHNSON", "SUSAN", "123 QUEENS STREET", "OXNARD", "CA", "93030"                }, ;
      { "SAMSON", "SAM", "215 MAIN STREET", "OXNARD", "CA", "93030"                     }, ;
      { "NEWNAME", "JAMES", "215 MAIN STREET", "LOS ANGELES", "CA", "90000"             }, ;
      { "OLEANDAR", "JILL", "425 FLORAL PARK DRIVE", "FLORAL PARK", "NY", "10093"       }, ;
      { "SUGARMAN", "CANDY", "1541 SWEETHEART ROAD", "HERSHEY", "PA", "10132"           } }

   dbCreate( "tbnames.dbf", { ;
      { "LAST" , "C", 18, 0, }, ;
      { "FIRST", "C",  9, 0, }, ;
      { "ADDR" , "C", 28, 0, }, ;
      { "CITY" , "C", 21, 0, }, ;
      { "STATE", "C",  2, 0, }, ;
      { "ZIP"  , "C",  9, 0, } } )

   USE tbnames.dbf

   FOR EACH x IN aData
      dbAppend()
      AEval( x, {| e, n | FieldPut( n, e ) } )
   NEXT

   dbCloseArea()

   RETURN
