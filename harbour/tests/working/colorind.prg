//
// $Id$
//

#translate TEST_LINE(<x>, <result>) => TEST_CALL(<(x)>, {|| <x> }, <result>)

STATIC snPass := 0
STATIC snFail := 0

FUNCTION Main()

     TEST_LINE( __ColorIndex()                  , ""               )
     TEST_LINE( __ColorIndex("", 0)             , ""               )
     TEST_LINE( __ColorIndex("W/R", 0)          , "W/R"            )
     TEST_LINE( __ColorIndex("W/R", 1)          , ""               )
     TEST_LINE( __ColorIndex("W/R", 2)          , ""               )
     TEST_LINE( __ColorIndex("W/R,GR/0", 0)     , "W/R"            )
     TEST_LINE( __ColorIndex("W/R,GR/0", 1)     , "GR/0"           )
     TEST_LINE( __ColorIndex("W/R,GR/0", 2)     , ""               )
     TEST_LINE( __ColorIndex("W/R,GR/0", 3)     , ""               )
     TEST_LINE( __ColorIndex("W/R, GR/0", 0)    , "W/R"            )
     TEST_LINE( __ColorIndex("W/R, GR/0", 1)    , "GR/0"           )
     TEST_LINE( __ColorIndex("W/R, GR/0", 2)    , ""               )
     TEST_LINE( __ColorIndex("W/R, GR/0", 3)    , ""               )
     TEST_LINE( __ColorIndex("W/R,GR/0 ", 0)    , "W/R"            )
     TEST_LINE( __ColorIndex("W/R,GR/0 ", 1)    , "GR/0"           )
     TEST_LINE( __ColorIndex("W/R,GR/0 ", 2)    , ""               )
     TEST_LINE( __ColorIndex("W/R, GR/0 ", 0)   , "W/R"            )
     TEST_LINE( __ColorIndex("W/R, GR/0 ", 1)   , "GR/0"           )
     TEST_LINE( __ColorIndex("W/R, GR/0 ", 2)   , ""               )
     TEST_LINE( __ColorIndex("W/R, GR/0 ,", 0)  , "W/R"            )
     TEST_LINE( __ColorIndex("W/R, GR/0 ,", 1)  , "GR/0"           )
     TEST_LINE( __ColorIndex("W/R, GR/0 ,", 2)  , ""               )
     TEST_LINE( __ColorIndex(" W/R, GR/0 ,", 0) , "W/R"            )
     TEST_LINE( __ColorIndex(" W/R, GR/0 ,", 1) , "GR/0"           )
     TEST_LINE( __ColorIndex(" W/R, GR/0 ,", 2) , ""               )
     TEST_LINE( __ColorIndex(" W/R , GR/0 ,", 0), "W/R"            )
     TEST_LINE( __ColorIndex(" W/R , GR/0 ,", 1), "GR/0"           )
     TEST_LINE( __ColorIndex(" W/R , GR/0 ,", 2), ""               )
     TEST_LINE( __ColorIndex(" W/R ,   ,", 1)   , ""               )
     TEST_LINE( __ColorIndex(" W/R ,,", 1)      , ""               )
     TEST_LINE( __ColorIndex(",,", 0)           , ""               )
     TEST_LINE( __ColorIndex(",,", 1)           , ""               )
     TEST_LINE( __ColorIndex(",,", 2)           , ""               )
     TEST_LINE( __ColorIndex(",  ,", 2)         , ""               )

     /* Show results, return ERRORLEVEL and exit */

     TEST_STAT()

     RETURN NIL

STATIC FUNCTION TEST_CALL(cBlock, bBlock, xResultExpected)
     LOCAL xResult := Eval(bBlock)

     fWrite(1, PadR(StrTran(cBlock, Chr(0), "."), 40) + " -> " +;
               PadR('"' + StrTran(XToStr(xResult), Chr(0), ".") + '"', 15) + " " +;
               PadR('"' + StrTran(XToStr(xResultExpected), Chr(0), ".") + '"', 15))

     IF xResult == xResultExpected
          snPass++
     ELSE
          fWrite(1, "! *FAIL* !")
          snFail++
     ENDIF

     fWrite(1, Chr(13) + Chr(10))

     RETURN NIL

STATIC FUNCTION TEST_STAT()

     fWrite(1, Chr(13) + Chr(10) +;
               "Test calls passed: " + Str(snPass) + Chr(13) + Chr(10) +;
               "Test calls failed: " + Str(snFail) + Chr(13) + Chr(10))

     ErrorLevel(iif(snFail != 0, 1, 0))

     RETURN NIL

STATIC FUNCTION XToStr(xValue)
     LOCAL cType := ValType(xValue)

     DO CASE
     CASE cType == "C" ; RETURN xValue
     CASE cType == "N" ; RETURN LTrim(Str(xValue))
     CASE cType == "D" ; RETURN DToC(xValue)
     CASE cType == "L" ; RETURN iif(xValue, ".T.", ".F.")
     CASE cType == "O" ; RETURN xValue:className + " Object"
     CASE cType == "U" ; RETURN "NIL"
     CASE cType == "B" ; RETURN "{||...}"
     CASE cType == "A" ; RETURN "{...}"
     CASE cType == "M" ; RETURN xValue
     ENDCASE

     RETURN ""
