#INCLUDE "HBCLASS.CH"

Function Main( )

	LOCAL cStr := 'cVar', cStr_1 := 'cVar_1', aVar := { 'cVar_1' }, oVar

	PRIVATE cVar_1, cMainPrivate := 'cVar_1', GlobalPrivate := 'BornInRunTimeVar'

	&cStr_1 = 'Simple '
	? M->cVar_1

	&( 'cVar' + '_1' ) := 'Macro'
	?? M->cVar_1

	M->&cStr_1 = 'Aliased'
	? M->cVar_1

	MEMVAR->&( 'cVar' + '_1' ) := ' Macro'
	?? M->cVar_1

	cStr := 'cVar_'
	&cStr.1 = 'Concatenated Macro (Numeric)'
	? M->cVar_1

	cStr := 'cVar'
	&cStr._1 = 'Concatenated Macro (String)'
	? M->cVar_1

	&aVar[1] := 'Array Macro'
	? M->cVar_1

	oVar := TValue():New()
	oVar:cVal := 'cVar_1'
	&oVar:cVal := 'Class Macro'
	? M->cVar_1

	SubFun()

	? '"cVar_1" = [' + M->cVar_1 + '] AFTER SubFun() PRIVATE'

	? M->NewPublicVar

RETURN NIL

FUNCTION TValue

   STATIC oClass

   IF oClass == NIL
      oClass = TClass():New( "TValue" )

      oClass:AddData( "cVal" )
      oClass:AddMethod( "New",        @New() )         // New Method

		oClass:Create()

   ENDIF

RETURN( oClass:Instance() )

STATIC FUNCTION New()

   LOCAL Self := QSelf()

RETURN Self

Function SubFun()

	? '"cVar_1" = [' + M->cVar_1 + '] BEFORE SubFun() PRIVATE'

	// Testing conflict with KEY WORD
	PRIVATE PRIVATE := 'I am a Var named PRIVATE ', &cMainPrivate, SomeVar, OtherVar := 1, &GlobalPrivate
	PUBLIC PUBLIC := 'My Name is PUBLIC', &( 'NewPublicVar' )

	M->NewPublicVar := 'Still Alive because I am PUBLIC'

	? M->PRIVATE + PRIVATE
	? PRIVATE + M->PRIVATE

	? BornInRunTimeVar

	&cMainPrivate := 'In SubFun()'

	? '"cVar_1" = [' + M->cVar_1 + '] in SubFun() PRIVATE'

RETURN
