#include "hbclass.ch"

MEMVAR cStr, cStr_1, cVar_1, aVar, oVar, OtherVar, SomeVar, Private
MEMVAR cMainPrivate, GlobalPrivate, BornInRunTimeVar, Public

PROCEDURE Main()

   PRIVATE cStr := "cVar", cStr_1 := "cVar_1", aVar := { "cVar_1" }, oVar

   PRIVATE cVar_1, cMainPrivate := "cVar_1", GlobalPrivate := "BornInRunTimeVar"

   // to avoid unused STATIC FUNCTION warnings
   UDF_STATIC()
   UDF2_STATIC()

   &cStr_1 := "Simple "
   ? M->cVar_1

   &( "cVar" + "_1" ) := "Macro"
   ?? M->cVar_1

   M->&cStr_1 := "Aliased"
   ? M->cVar_1

   MEMVAR->&( "cVar" + "_1" ) := " Macro"
   ?? M->cVar_1

   cStr := "cVar_"
   &cStr.1 := "Concatenated Macro (Numeric)"
   ? M->cVar_1

   cStr := "cVar"
   &cStr._1 := "Concatenated Macro (String)"
   ? M->cVar_1

   &( aVar[1] ) := "Array Macro"
   ? M->cVar_1

   oVar := TValue():New()
   oVar:cVal := "cVar_1"
   &( oVar:cVal ) := "Class Macro"
   ? M->cVar_1

   SubFun()

   ? '"cVar_1" = [' + M->cVar_1 + '] AFTER SubFun() PRIVATE'

   ? M->NewPublicVar

   TEST_Type()

   RETURN

FUNCTION TValue()

   STATIC s_oClass

   IF s_oClass == NIL
      s_oClass := HBClass():New( "TValue" )

      s_oClass:AddData( "cVal" )
      s_oClass:AddMethod( "New", @New() ) // New() Method

      s_oClass:Create()

   ENDIF

   RETURN s_oClass:Instance()

STATIC FUNCTION New()

   LOCAL Self := QSelf()

   RETURN Self

FUNCTION SubFun()

   ? '"cVar_1" = [' + M->cVar_1 + '] BEFORE SubFun() PRIVATE'

   // Testing conflict with KEY WORDS
   PRIVATE PRIVATE := "I am a Var named PRIVATE ", &cMainPrivate, SomeVar, OtherVar := 1, &GlobalPrivate := "I was born in Run Time"
   PUBLIC PUBLIC := "NewPublicVar"
   PUBLIC &PUBLIC

   ? M->NewPublicVar

   M->NewPublicVar := "Still Alive because I am PUBLIC"

   ? M->PRIVATE + PRIVATE
   ? PRIVATE + M->PRIVATE

   ? BornInRunTimeVar

   &cMainPrivate := "In SubFun()"

   ? '"cVar_1" = [' + M->cVar_1 + '] in SubFun() PRIVATE'

   RETURN NIL

STATIC PROCEDURE TEST_Type()

   LOCAL v1, v2, v1a, v2a
   LOCAL bErr := ErrorBlock( {| e | Break( e ) } ), oE

   ?
   ? "=========== Type() function ================="
   v1 := "UDF()"
   ? "Test for Type('UDF()')        - should be 'UI': ", Type( v1 )
   v2 := "UDF_STATIC()"
   ? "Test for Type('UDF_STATIC()') - should be 'U': ", Type( v2 )
   ? "Test for &" + "'UDF()'  - should print 'udf': ", &v1
   ? "Test for &" + "'UDF_STATIC()'  - should print 'ERROR: undefined function': "
   BEGIN SEQUENCE
      ?? &v2
   RECOVER USING oE
      ? "ERROR: " + oE:Description
   END SEQUENCE
   ErrorBlock( bErr )

   v1 := "UDF"
   ? "Test for Type('UDF')        - should be 'U': ", Type( v1 )
   v2 := "UDF_STATIC"
   ? "Test for Type('UDF_STATIC') - should be 'U': ", Type( v2 )

   v1a := "UDF := 1"
   ? "Test for Type('UDF := 1')        - should be 'N': ", Type( v1a )
   v2a := "UDF_STATIC := 1"
   ? "Test for Type('UDF_STATIC := 1') - should be 'N': ", Type( v2a )

   ? "=== after the assignment ==="
   v1 := "UDF"
   ? "Test for Type('UDF')        - should be 'N': ", Type( v1 )
   v2 := "UDF_STATIC"
   ? "Test for Type('UDF_STATIC') - should be 'N': ", Type( v2 )

   v1 := "UDF()"
   ? "Test for Type('UDF()')        - should be 'UI': ", Type( v1 )
   v2 := "UDF_STATIC()"
   ? "Test for Type('UDF_STATIC()') - should be 'U': ", Type( v2 )


   ? "=== declared public variable ==="
   PUBLIC UDF2, UDF2_STATIC
   v1 := "UDF2()"
   ? "Test for Type('UDF2()')        - should be 'UI': ", Type( v1 )
   v2 := "UDF2_STATIC()"
   ? "Test for Type('UDF2_STATIC()') - should be 'U': ", Type( v2 )

   v1 := "UDF2"
   ? "Test for Type('UDF')        - should be 'L': ", Type( v1 )
   v2 := "UDF2_STATIC"
   ? "Test for Type('UDF_STATIC') - should be 'L': ", Type( v2 )

   ?

   RETURN

STATIC FUNCTION UDF_STATIC()

   RETURN "udf_static"

FUNCTION UDF()

   RETURN "udf"

STATIC FUNCTION UDF2_STATIC()

   RETURN "udf2_static"

FUNCTION UDF2()

   RETURN "udf2"
