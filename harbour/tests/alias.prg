//NOTEST
/*
 * $Id$
 */

// It is used to check if pcode is generated correctly for aliased expressions
// (you must check it visually :)

PROCEDURE Main()

   LOCAL localVar
   STATIC staticVar
   FIELD fieldVar
   FIELD aliasedField IN aaa
   MEMVAR memvarVar
   PRIVATE privateVar

   ? privateVar
   ? memvarVar
   ? localVar
   ? staticVar
   ? fieldVar
   ? aliasedField
   ? unknVar

   ? 1->privateVar
   ? 1->memvarVar
   ? 1->localVar
   ? 1->staticVar
   ? 1->fieldVar
   ? 1->aliasedField
   ? 1->unknVar

   ? alias->privateVar
   ? alias->memvarVar
   ? alias->localVar
   ? alias->staticVar
   ? alias->fieldVar
   ? alias->aliasedField
   ? alias->unknVar

   ? ( localVar )->privateVar
   ? ( localVar )->memvarVar
   ? ( localVar )->localVar
   ? ( localVar )->staticVar
   ? ( localVar )->fieldVar
   ? ( localVar )->aliasedField
   ? ( localVar )->unknVar

   ? ( localVar )->( privateVar, memvarVar, localVar, staticVar, fieldVar, aliasedField, unknVar )
   ? alias->( privateVar, memvarVar, localVar, staticVar, fieldVar, aliasedField, unknVar )
   ? 2->( privateVar, memvarVar, localVar, staticVar, fieldVar, aliasedField, unknVar )
   ? ( localVar, 2 )->( privateVar, memvarVar, localVar, staticVar, fieldVar, aliasedField, unknVar )

   ? privateVar ++
   ? memvarVar ++
   ? localVar ++
   ? staticVar ++
   ? fieldVar ++
   ? aliasedField ++
   ? unknVar ++

   ? 1->privateVar ++
   ? 1->memvarVar ++
   ? 1->localVar ++
   ? 1->staticVar ++
   ? 1->fieldVar ++
   ? 1->aliasedField ++
   ? 1->unknVar ++

   ? alias->privateVar ++
   ? alias->memvarVar ++
   ? alias->localVar ++
   ? alias->staticVar ++
   ? alias->fieldVar ++
   ? alias->aliasedField ++
   ? alias->unknVar ++

   ? ( localVar )->privateVar ++
   ? ( localVar )->memvarVar ++
   ? ( localVar )->localVar ++
   ? ( localVar )->staticVar ++
   ? ( localVar )->fieldVar ++
   ? ( localVar )->aliasedField ++
   ? ( localVar )->unknVar ++

   ? privateVar    += privateVar
   ? memvarVar     += memvarVar
   ? localVar      += localVar
   ? staticVar     += staticVar
   ? fieldVar      += fieldVar
   ? aliasedField  += aliasedField
   ? unknVar       += unknVar

   ? 1->privateVar    += 1->privateVar
   ? 1->memvarVar     += 1->memvarVar
   ? 1->localVar      += 1->localVar
   ? 1->staticVar     += 1->staticVar
   ? 1->fieldVar      += 1->fieldVar
   ? 1->aliasedField  += 1->aliasedField
   ? 1->unknVar       += 1->unknVar

   ? alias->privateVar    += alias->privateVar
   ? alias->memvarVar     += alias->memvarVar
   ? alias->localVar      += alias->localVar
   ? alias->staticVar     += alias->staticVar
   ? alias->fieldVar      += alias->fieldVar
   ? alias->aliasedField  += alias->aliasedField
   ? alias->unknVar       += alias->unknVar

   ? ( localVar )->privateVar     += ( localVar )->privateVar
   ? ( localVar )->memvarVar      += ( localVar )->memvarVar
   ? ( localVar )->localVar       += ( localVar )->localVar
   ? ( localVar )->staticVar      += ( localVar )->staticVar
   ? ( localVar )->fieldVar       += ( localVar )->fieldVar
   ? ( localVar )->aliasedField   += ( localVar )->aliasedField
   ? ( localVar )->unknVar        += ( localVar )->unknVar

   ? ( localVar )->privateVar     += 2->privateVar
   ? ( localVar )->memvarVar      += 2->memvarVar
   ? ( localVar )->localVar       += 2->localVar
   ? ( localVar )->staticVar      += 2->staticVar
   ? ( localVar )->fieldVar       += 2->fieldVar
   ? ( localVar )->aliasedField   += 2->aliasedField
   ? ( localVar )->unknVar        += 2->unknVar

   ? alias->( aliasedField, MEMVAR->privateVar, 1->( Test( 2->fieldVar ) ) )

   MEMVAR->privateVar := 0
   M->localVar := 1
   MEMVA->fieldVar := 2

   FIELD->fieldVar := 0
   FIEL->aliasedFieldVar := 1

   RETURN
