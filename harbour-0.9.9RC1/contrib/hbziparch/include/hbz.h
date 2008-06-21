#ifndef HB_APIZLIB_H_
#define HB_APIZLIB_H_
#define HB_OS_WIN_32_USED
#include <hbsetup.h>
#include <hbapiitm.h>
#include <hbapifs.h>
#include <hbapigt.h>
#include <hbvmpub.h>
#include <hbvm.h>

#include <time.h>

#ifdef __cplusplus
#include <ZipArchive.h>
extern "C" {
#endif


#define filePos 1
#define Lenght 2
#define Method 3
#define Size 4
#define Ratio 5
#define Date 6
#define Time 7
#define Crc32 8
#define Attr  9
#define WRITEBUFFERSIZE (16384)
#define MAXFILENAME (256)
typedef struct _HB_ZIP_INTERNAL{
int iWrite;
int iExtract;
int iRead;
char * szComment;
PHB_ITEM pItem;
} HB_ZIP_INTERNAL,* PHB_ZIP_INTERNAL,* HB_ZIP_INTERNAL_PTR;
#ifndef LPCTSTR
typedef const char *LPCSTR;
typedef LPCSTR LPCTSTR;
#endif
class SpanCallback : public CZipSpanCallback
{
 bool Callback(int iProgress)
 {
      PHB_ITEM pDisk=hb_itemPutNL(NULL,m_uDiskNeeded);
      bool iReturn=true;
      hb_vmEvalBlockV( pChangeDiskBlock, 1,pDisk );
      hb_itemRelease(pDisk);
      return iReturn;
 }
};

class SpanActionCallback : public CZipActionCallback
{
 bool Callback(int iProgress)
 {
      int iReturn=1;
      PHB_ITEM pDisk;
      PHB_ITEM pTotal =hb_itemPutNL(NULL,m_uTotalToDo);
      pDisk=  hb_itemPutNL(NULL,m_uTotalSoFar);
      hb_vmEvalBlockV( pProgressInfo, 2,pDisk,pTotal);
      hb_itemRelease(pDisk);
      hb_itemRelease(pTotal);
      return iReturn;    
 }
};


class hbZip {
public:
BOOL bAdded;
hbZip(){
   bAdded=false
};
virtual ~hbZip();
CZipArchive szZip;
SpanCallback span;
SpanActionCallback spanac;
};

#ifdef __cplusplus
}
#endif
#endif
