/*
 * $Id$
 */

/*
 *   THIS DEMO SHOWS tbnames.dbf CONSISTING OF LAST, FIRST, ADDR, CITY,
 *   STATE, ZIP WITH ACTIVE INDEX ON LAST + FIRST.  IT SHOWS LAST NAME,
 *   FIRST NAME, CITY ONLY FOR THOSE LAST NAMES THAT BEGIN WITH LETTER
 *   THAT YOU INPUT FOR THE CKEY GET.
 *
 *   tbnames.dbf/.ntx ARE AUTOMATICALLY CREATED BY THIS TEST PROGRAM
 */

#require "hbnf"

PROCEDURE Main()

   LOCAL aFields := {}, cKey := "O", cOldColor
   LOCAL nFreeze := 1, lSaveScrn := .T. , nRecSel
   LOCAL cColorList := "N/W, N/BG, B/W, B/BG, B/W, B/BG, R/W, B/R"
   LOCAL cColorShad := "N/N"
   FIELD last, first
   MEMVAR GetList

   IF ! hb_FileExists( "tbnames.dbf" )
      MAKE_DBF()
   ENDIF

   USE TBNames

   IF ! hb_FileExists( "tbnames.ntx" )
      INDEX ON last + first TO TBNAMES
   ENDIF

   SET INDEX TO TBNAMES

   // Pass Heading as character and Field as Block including Alias
   // To eliminate the need to use FIELDWBLOCK() function in FT_BRWSWHL()

   AAdd( aFields, { "Last Name" , {|| TBNames->Last }  } )
   AAdd( aFields, { "First Name", {|| TBNames->First } } )
   AAdd( aFields, { "City"      , {|| TBNames->City }  } )

   cOldColor := SetColor( "N/BG" )
   CLS
   @ 5, 10 SAY "Enter First Letter Of Last Name:" GET cKey PICTURE "!"
   READ

   // TBNames->Last = cKey is the Conditional Block passed to this function
   // you can make it as complicated as you want, but you would then
   // have to modify TBWhileSet() to find first and last records
   // matching your key.
   nRecSel := FT_BRWSWHL( aFields, {|| TBNames->Last = cKey }, cKey, nFreeze, ;
      lSaveScrn, cColorList, cColorShad, 3, 6, MaxRow() - 2, MaxCol() - 6 )
   // Note you can use Compound Condition
   // such as cLast =: "Pierce            " and cFirst =: "Hawkeye  "
   // by changing above block to:
   //    {|| TBNames->Last = cLast .AND. TBNames->First = cFirst }
   // and setting cKey := cLast + cFirst

   ?
   IF nRecSel == 0
      ? "Sorry, NO Records Were Selected"
   ELSE
      ? "You Selected " + TBNames->Last + " " + ;
         TBNames->First + " " + TBNames->City
   ENDIF
   ?

   WAIT
   SetColor( cOldColor )
   CLS

   RETURN

STATIC FUNCTION make_dbf()

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

   dbCreate( "TBNAMES", {;
      { "LAST ", "C", 18, 0, } , ;
      { "FIRST", "C",  9, 0, } , ;
      { "ADDR ", "C", 28, 0, } , ;
      { "CITY ", "C", 21, 0, } , ;
      { "STATE", "C",  2, 0, } , ;
      { "ZIP  ", "C",  9, 0, } } )
   USE tbnames
   FOR x := 1 TO Len( aData )
      APPEND BLANK
      AEval( aData[ x ], {| e, n | FieldPut( n, e ) } )
   NEXT
   USE

   RETURN NIL
