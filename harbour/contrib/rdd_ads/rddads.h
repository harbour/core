#include "rddapi.h"
#include "ace.h"

/*
*  ADS WORKAREA
*  --------
*  The Workarea Structure of Advantage Database Server RDD
*
*/


typedef struct _ADSAREA_
{
   struct _RDDFUNCS * lprfsHost; /* Virtual method table for this workarea */
   USHORT   uiArea;              /* The number assigned to this workarea */
   void *   atomAlias;           /* Pointer to the alias symbol for this workarea */
   USHORT   uiFieldExtent;       /* Total number of fields allocated */
   USHORT   uiFieldCount;        /* Total number of fields used */
   LPFIELD  lpFields;            /* Pointer to an array of fields */
   void *   lpFieldExtents;      /* Void ptr for additional field properties */
   PHB_ITEM valResult;           /* All purpose result holder */
   BOOL fTop;                    /* TRUE if "top" */
   BOOL fBottom;                 /* TRUE if "bottom" */
   BOOL fBof;                    /* TRUE if "bof" */
   BOOL fEof;                    /* TRUE if "eof" */
   BOOL fFound;                  /* TRUE if "found" */
   DBSCOPEINFO  dbsi;            /* Info regarding last LOCATE */
   DBFILTERINFO dbfi;            /* Filter in effect */
   LPDBORDERCONDINFO lpdbOrdCondInfo;
   LPDBRELINFO  lpdbRelations;   /* Parent/Child relationships used */
   USHORT       uiParents;       /* Number of parents for this area */
   USHORT   heap;
   USHORT   heapSize;
   USHORT   rddID;
   LPFILEINFO lpDataInfo;        /* Data files used by this workarea */
   LPINDEXINFO lpIndexInfo;      /* Indexes used by this workarea */
   LPDBEXTENDINFO lpExtendInfo;  /* Additional properties */

   /*
   *  ADS's additions to the workarea structure
   *
   *  Warning: The above section MUST match WORKAREA exactly!  Any
   *  additions to the structure MUST be added below, as in this
   *  example.
   */

   ADSHANDLE hTable;
   ADSHANDLE hOrdCurrent;
} ADSAREA;

typedef ADSAREA far * ADSAREAP;
