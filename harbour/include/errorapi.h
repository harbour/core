/*
 * $Id$
 */

#ifndef ERRORAPI_H_
#define ERRORAPI_H_

/*
 * +
 *      Harbour project
 *
 *      99.04.25        initial posting.
 *                      compatible
 *-
 */

#include <extend.h>
#include "error.ch"


/*
 *  error flag definations
 */

#define EF_CANRETRY     1
#define EF_CANDEFAULT   4

/*
 *  error codes (returned from _errLaunch())
 */

#define E_BREAK         0xffff
#define E_RETRY         1
#define E_DEFAULT       0

PHB_ITEM hb_errNew( void );
char *   hb_errGetDescription( PHB_ITEM pError );
PHB_ITEM hb_errPutDescription( PHB_ITEM pError, char * szDescription );
char *   hb_errGetFileName( PHB_ITEM pError );
PHB_ITEM hb_errPutFileName( PHB_ITEM pError, char * szFileName );
USHORT   hb_errGetGenCode( PHB_ITEM pError );
PHB_ITEM hb_errPutGenCode( PHB_ITEM pError, USHORT uiGenCode );
char *   hb_errGetOperation( PHB_ITEM pError );
PHB_ITEM hb_errPutOperation( PHB_ITEM pError, char * szOperation );
USHORT   hb_errGetOsCode( PHB_ITEM pError );
PHB_ITEM hb_errPutOsCode( PHB_ITEM pError, USHORT uiOsCode );
PHB_ITEM hb_errPutSeverity( PHB_ITEM pError, USHORT uiSeverity );
PHB_ITEM hb_errPutSubCode( PHB_ITEM pError, USHORT uiSubCode );
PHB_ITEM hb_errPutSubSystem( PHB_ITEM pError, char * szSubSystem );
PHB_ITEM hb_errPutTries( PHB_ITEM pError, USHORT uiTries );
WORD     hb_errLaunch( PHB_ITEM pError );
void     hb_errRelease( PHB_ITEM pError );

#endif  /* ERRORAPI_H_ */
