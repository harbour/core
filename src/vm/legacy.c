/* Copyright 2015 Viktor Szakats (vszakats.net/harbour) */

#include "hbapi.h"

#undef hb_arrayScan

extern HB_SIZE hb_arrayScan( PHB_ITEM pArray, PHB_ITEM pValue, HB_SIZE * pnStart, HB_SIZE * pnCount, HB_BOOL fExact );

HB_SIZE hb_arrayScan( PHB_ITEM pArray, PHB_ITEM pValue, HB_SIZE * pnStart, HB_SIZE * pnCount, HB_BOOL fExact )
{
   return hb_arrayScanCase( pArray, pValue, pnStart, pnCount, fExact, HB_TRUE );
}
