FUNCTION Main()

	USE TEST

	REPLACE Age WITH 1
	? FIELD->Age

	//REPLACE 1->Age WITH 2 // Todo: complete support in harbour.y - AliasAddInt()
	//? FIELD->Age

	//REPLACE 1.5->Age WITH 3 // Will produce "Invalid alias expression"
	//? FIELD->Age

	REPLACE TEST->Age WITH 4
	? FIELD->Age

	TEST->Age := 5
	? FIELD->Age

	TEST->( FieldPut( FieldPos( 'AGE' ), 6 ) )
	? FIELD->Age

return NIL
