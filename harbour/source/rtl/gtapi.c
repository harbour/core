#include <stdio.h>
#include <types.h>

#ifdef __BORLANDC__
   #include <conio.h>
#endif

#define SC_NONE     0   /* None */
#define SC_NORMAL   1   /* Underline */
#define SC_INSERT   2   /* Lower half block */
#define SC_SPECIAL1 3   /* Full block */
#define SC_SPECIAL2 4   /* Upper half block */

int _gtGetPos( USHORT * uipRow, USHORT * uipCol )
{
   #ifdef __BORLANDC__
      * uipRow = wherey();
      * uipCol = wherex();
   #endif

   return 0;
}

int _gtSetPos( USHORT uiRow, USHORT uiCol )
{
   #ifdef __BORLANDC__
      gotoxy( uiRow, uiCol );
   #endif

   return 0;
}

int _gtMaxCol( void )
{
   #ifdef __BORLANDC__
      struct text_info ti;
      gettextinfo( &ti );

      return ti.screenwidth;
   #endif
}

int _gtMaxRow( void )
{
   #ifdef __BORLANDC__
      struct text_info ti;
      gettextinfo( &ti );

      return ti.screenheight;
   #endif
}

int _gtWrite( BYTE * fpStr, USHORT uiLen )
{
   USHORT u = 0;

   while( u < uiLen )
      printf( "%c", * ( fpStr + u++ ) );

   return 0;
}

int _gtGetCursor( USHORT * uipCursorShape )
{
   USHORT startLine = 0, endLine = 0;

   #ifdef __BORLANDC__
      asm pusha;
      _BH = 0;
      _AH = 3;
      asm int 0x10;
      startLine = _CH;
      endLine = _CL;
      asm popa;
   #endif

   if( ( startLine == 0 ) && ( endLine == 0 ) )
      * uipCursorShape = SC_NONE;

   else if( ( startLine == 0 ) && ( endLine == 1 ) )
      * uipCursorShape = SC_NORMAL;

   else if( ( startLine == 0 ) && ( endLine == 3 ) )
      * uipCursorShape = SC_INSERT;

   else if( ( startLine == 0 ) && ( endLine == 7 ) )
      * uipCursorShape = SC_SPECIAL1;

   else if( ( startLine == 4 ) && ( endLine == 7 ) )
      * uipCursorShape = SC_SPECIAL2;

   return 0;
}
