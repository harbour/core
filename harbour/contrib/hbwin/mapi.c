
#include <windows.h>
#include <hbapi.h>
#include <hbstack.h>
#include <mapi.h>

static HINSTANCE hMapiLib = 0L;

BOOL static LoadMapiLib( void )
{
   if( ( hMapiLib = LoadLibrary( "mapi32.dll" ) ) >= (HINSTANCE) 32)
   {
      return TRUE;
   }

   return FALSE;
}

HB_FUNC( HB_MAPISENDMAIL )
{
    ULONG uError;
    int   iLen, i;
    LPMAPISENDMAIL MAPISendMail;
    MapiRecipDesc origin;

    MapiMessage note;

    ZeroMemory ( &note, sizeof ( MapiMessage ) );
    ZeroMemory ( &origin, sizeof ( MapiRecipDesc ) );

    note.ulReserved       = 0L;
    note.nRecipCount      = 0;
    note.lpRecips         = NULL;
    note.lpOriginator     = NULL;
    note.nFileCount       = 1;
    note.lpszSubject      = ISCHAR( 1 ) ? hb_parc( 1 ) : "";
    note.lpszNoteText     = ISCHAR( 2 ) ? hb_parc( 2 ) : "";
    note.lpszMessageType  = ISCHAR( 3 ) ? hb_parc( 3 ) : NULL;
    note.lpszDateReceived = ISCHAR( 4 ) ? hb_parc( 4 ) : "";
    note.flFlags          = ( ISLOG( 6 ) && hb_parl( 6 ) ) ? MAPI_RECEIPT_REQUESTED: 0;

    if( hb_pcount() >= 8 && ISARRAY( 8 ) && hb_arrayLen( hb_param( 8, HB_IT_ARRAY ) ) )
    {
       origin.lpszName    = hb_parvc( 8, 1 );
       origin.lpszAddress = hb_parvc( 8, 2 );
       note.lpOriginator  = &origin;
    }

    if( hb_pcount() >= 9 && ISARRAY( 9 ) && ( iLen = hb_arrayLen( hb_param( 9, HB_IT_ARRAY ) ) ) > 0 )
    {
       MapiRecipDesc target[ 100 ];

       ZeroMemory( target, sizeof ( MapiRecipDesc ) * 100 );

       for( i = 0; i < ( iLen < 100 ? iLen : 100 ); i++ )
       {
          hb_arrayGet( hb_param( 9, HB_IT_ARRAY ), i + 1, hb_stackReturnItem() );

          if ( hb_parvclen( -1, 1 ) > 0 )
          {
             target[ i ].lpszName = hb_parvc( -1, 1 );
             if ( hb_parvclen( -1, 2 ) > 0 )
             {
                target[ i ].lpszAddress = hb_parvc( -1, 2 );
             }
          }
          else
          {
             target[ i ].lpszName = hb_parvc( -1, 2 );
          }
          target[ i ].ulRecipClass = hb_parvnl( -1, 3 ) ;
       }
       note.nRecipCount = iLen;
       note.lpRecips    = target;
    }
    else
    {
       note.nRecipCount = 0;
    }

    if( hb_pcount() >= 10 && ISARRAY( 10 ) && ( iLen = hb_arrayLen( hb_param( 10, HB_IT_ARRAY ) ) ) > 0 )
    {
       MapiFileDesc FileDesc[ 100 ];

       ZeroMemory( FileDesc, sizeof( MapiFileDesc ) * 100 );

       for( i = 0; i < ( iLen < 100 ? iLen : 100 ); i++ )
       {
          hb_arrayGet( hb_param( 10, HB_IT_ARRAY ), i + 1, hb_stackReturnItem() );
          FileDesc[ i ].ulReserved   = 0 ;
          FileDesc[ i ].lpszPathName = hb_parvc( -1, 2 );
          FileDesc[ i ].lpszFileName = hb_parvc( -1, 1 );
          FileDesc[ i ].nPosition    = -1 ;
       }

       note.nFileCount = iLen;
       note.lpFiles    = FileDesc;
    }
    else
    {
       note.nFileCount = 0;
    }

    if( LoadMapiLib() )
    {
       MAPISendMail = ( LPMAPISENDMAIL ) GetProcAddress( hMapiLib, "MAPISendMail");

       if ( ISLOG( 7 ) && hb_parl( 7 ) )
       {
          uError = (*MAPISendMail) (0L, ( ULONG ) GetActiveWindow(), &note, MAPI_DIALOG | MAPI_LOGON_UI, 0L);
       }
       else
       {
          uError = (*MAPISendMail) (0L, ( ULONG ) GetActiveWindow(), &note, MAPI_LOGON_UI, 0L);
       }

       FreeLibrary( hMapiLib );

       hb_retnl( uError );
    }
    else
    {
       hb_retnl( -1 );
    }
}
