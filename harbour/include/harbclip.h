#ifndef __HARBCLIP_H
#define __HARBCLIP_H

#define _param    hb_param
#define _parc     hb_parc
#define _parclen  hb_parclen
#define _pards    hb_pards
#define _parl     hb_parl
#define _parnd    hb_parnd
#define _parni    hb_parni
#define _parnl    hb_parnl
#define _parinfo  hb_parinfo
#define _pcount   hb_pcount

#define _ret      hb_ret
#define _retc     hb_retc
#define _retclen  hb_retclen
#define _retds    hb_retds
#define _retl     hb_retl
#define _retnd    hb_retnd
#define _retni    hb_retni
#define _retnl    hb_retnl

#define _storc    hb_storc
#define _storlen  hb_storlen
#define _stords   hb_stords
#define _storl    hb_storl
#define _stornd   hb_stornd
#define _storni   hb_storni
#define _stornl   hb_stornl

#define _xgrab    hb_xgrab
#define _xrealloc hb_xrealloc
#define _xfree    hb_xfree

#define _errLaunch         hb_errLaunch
#define _errNew            hb_errNew
#define _errPutDescription hb_errPutDescription
#define _errRelease        hb_errRelease

/* TODO: Change array functions so they conform to this:
#define _arrayNew    hb_arrayNew
#define _arrayLen    hb_arrayLen
#define _arrayGet    hb_arrayGet
#define _arrayPut    hb_arrayPut
#define _arrayResize hb_arrayResize
#define _arrayGetC   hb_arrayGetC
#define _arrayGetCL  hb_arrayGetCL
*/
#define _arrayNew    ArrayNew
#define _arrayLen    ArrayLen
#define _arrayGet    ArrayGet
#define _arrayPut    ArraySet
#define _arrayResize ArraySize
#define _arrayGetC   ArrayGetString
#define _arrayGetCL  ArrayGetStringLen

#define _evalLaunch   hb_evalLaunch
#define _evalNew      hb_evalNew
#define _evalPutParam hb_evalPutParam
#define _evalRelease  hb_evalRelease

#define _itemArrayGet   hb_itemArrayGet
#define _itemArrayNew   hb_itemArrayNew
#define _itemArrayPut   hb_itemArrayPut
#define _itemCopyC      hb_itemCopyC
#define _itemFreeC      hb_itemFreeC
#define _itemGetC       hb_itemGetC
#define _itemGetDS      hb_itemGetDS
#define _itemGetL       hb_itemGetL
#define _itemGetND      hb_itemGetND
#define _itemGetNL      hb_itemGetNL
#define _itemNew        hb_itemNew
#define _itemParam      hb_itemParam
#define _itemPutC       hb_itemPutC
#define _itemPutCL      hb_itemPutCL
#define _itemPutDS      hb_itemPutDS
#define _itemPutL       hb_itemPutL
#define _itemPutND      hb_itemPutND
#define _itemPutNL      hb_itemPutNL
#define _itemRelease    hb_itemRelease
#define _itemReturn     hb_itemReturn
#define _itemSize       hb_itemSize
#define _itemType       hb_itemType

#endif

