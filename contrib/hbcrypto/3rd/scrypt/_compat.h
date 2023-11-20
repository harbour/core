#ifndef HB_COMPAT_H_
#define HB_COMPAT_H_

#include "hbdefs.h"
#define uint8_t       HB_U8
#define uint32_t      HB_U32
#define uint64_t      HB_U64
#define le32dec(x)    HB_GET_LE_UINT32(x)
#define le32enc(x,y)  HB_PUT_LE_UINT32(x,y)
#define le64dec(x)    HB_GET_LE_UINT64(x)
#define le64enc(x,y)  HB_PUT_LE_UINT64(x,y)
#define be32dec(x)    HB_GET_BE_UINT32(x)
#define be32enc(x,y)  HB_PUT_BE_UINT32(x,y)
#define be64dec(x)    HB_GET_BE_UINT64(x)
#define be64enc(x,y)  HB_PUT_BE_UINT64(x,y)

#ifndef SIZE_MAX
#define SIZE_MAX  HB_SIZE_MAX
#endif

#endif
