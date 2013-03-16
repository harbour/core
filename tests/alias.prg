//NOTEST

// It is used to check if pcode is generated correctly for aliased expressions
// (you must check it visually :)

MEMVAR m_privateVar
MEMVAR unknVar

PROCEDURE Main()

   LOCAL localVar
   STATIC s_staticVar
   FIELD fieldVar
   FIELD aliasedField IN aaa
   MEMVAR memvarVar
   PRIVATE m_privateVar

   ? m_privateVar
   ? memvarVar
   ? localVar
   ? s_staticVar
   ? fieldVar
   ? aliasedField
   ? unknVar

   ? 1->m_privateVar
   ? 1->memvarVar
   ? 1->localVar
   ? 1->s_staticVar
   ? 1->fieldVar
   ? 1->aliasedField
   ? 1->unknVar

   ? alias->m_privateVar
   ? alias->memvarVar
   ? alias->localVar
   ? alias->s_staticVar
   ? alias->fieldVar
   ? alias->aliasedField
   ? alias->unknVar

   ? ( localVar )->m_privateVar
   ? ( localVar )->memvarVar
   ? ( localVar )->localVar
   ? ( localVar )->s_staticVar
   ? ( localVar )->fieldVar
   ? ( localVar )->aliasedField
   ? ( localVar )->unknVar

   ? ( localVar )->( m_privateVar, memvarVar, localVar, s_staticVar, fieldVar, aliasedField, unknVar )
   ? alias->( m_privateVar, memvarVar, localVar, s_staticVar, fieldVar, aliasedField, unknVar )
   ? 2->( m_privateVar, memvarVar, localVar, s_staticVar, fieldVar, aliasedField, unknVar )
   ? ( localVar, 2 )->( m_privateVar, memvarVar, localVar, s_staticVar, fieldVar, aliasedField, unknVar )

   ? m_privateVar++
   ? memvarVar++
   ? localVar++
   ? s_staticVar++
   ? fieldVar++
   ? aliasedField++
   ? unknVar++

   ? 1->m_privateVar++
   ? 1->memvarVar++
   ? 1->localVar++
   ? 1->s_staticVar++
   ? 1->fieldVar++
   ? 1->aliasedField++
   ? 1->unknVar++

   ? alias->m_privateVar++
   ? alias->memvarVar++
   ? alias->localVar++
   ? alias->s_staticVar++
   ? alias->fieldVar++
   ? alias->aliasedField++
   ? alias->unknVar++

   ? ( localVar )->m_privateVar++
   ? ( localVar )->memvarVar++
   ? ( localVar )->localVar++
   ? ( localVar )->s_staticVar++
   ? ( localVar )->fieldVar++
   ? ( localVar )->aliasedField++
   ? ( localVar )->unknVar++

   ? m_privateVar  += m_privateVar
   ? memvarVar     += memvarVar
   ? localVar      += localVar
   ? s_staticVar   += s_staticVar
   ? fieldVar      += fieldVar
   ? aliasedField  += aliasedField
   ? unknVar       += unknVar

   ? 1->m_privateVar  += 1->m_privateVar
   ? 1->memvarVar     += 1->memvarVar
   ? 1->localVar      += 1->localVar
   ? 1->s_staticVar   += 1->s_staticVar
   ? 1->fieldVar      += 1->fieldVar
   ? 1->aliasedField  += 1->aliasedField
   ? 1->unknVar       += 1->unknVar

   ? alias->m_privateVar  += alias->m_privateVar
   ? alias->memvarVar     += alias->memvarVar
   ? alias->localVar      += alias->localVar
   ? alias->s_staticVar   += alias->s_staticVar
   ? alias->fieldVar      += alias->fieldVar
   ? alias->aliasedField  += alias->aliasedField
   ? alias->unknVar       += alias->unknVar

   ? ( localVar )->m_privateVar   += ( localVar )->m_privateVar
   ? ( localVar )->memvarVar      += ( localVar )->memvarVar
   ? ( localVar )->localVar       += ( localVar )->localVar
   ? ( localVar )->s_staticVar    += ( localVar )->s_staticVar
   ? ( localVar )->fieldVar       += ( localVar )->fieldVar
   ? ( localVar )->aliasedField   += ( localVar )->aliasedField
   ? ( localVar )->unknVar        += ( localVar )->unknVar

   ? ( localVar )->m_privateVar   += 2->m_privateVar
   ? ( localVar )->memvarVar      += 2->memvarVar
   ? ( localVar )->localVar       += 2->localVar
   ? ( localVar )->s_staticVar    += 2->s_staticVar
   ? ( localVar )->fieldVar       += 2->fieldVar
   ? ( localVar )->aliasedField   += 2->aliasedField
   ? ( localVar )->unknVar        += 2->unknVar

   ? alias->( aliasedField, MEMVAR->m_privateVar, 1->( Test( 2->fieldVar ) ) )

   MEMVAR->m_privateVar := 0
   M->localVar := 1
   MEMVA->fieldVar := 2

   FIELD->fieldVar := 0
   FIEL->aliasedFieldVar := 1

   RETURN

STATIC FUNCTION Test()
   RETURN NIL
