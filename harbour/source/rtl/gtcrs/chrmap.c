/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem based on ncurses screen library.
 *
 * Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>
 * www - http://www.harbour-project.org
 * Special thanks to Marek Paliwoda <paliwoda@inetia.pl>
 * author of gtsln from which I borrowed a lot of code and ideas.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* NOTE: User programs should never call this layer directly! */

#include "gtcrs.h"

#define MAX_CHAR_VAL	0xff
char *s_szDefaultCharMapFile = "/etc/harbour/hb-charmap.def";

static void skip_blank(char **buf)
{
    while ( **buf != '\0' && **buf == ' ' )
	++(*buf);
}

static int get_val(char **buf)
{
    int n = -1;
    char c;

    if ((*buf)[0] == '\'' && (*buf)[1] != '\0' && (*buf)[2] == '\'')
    {
	n = (*buf)[1] & 0xff;
	*buf+=3;
    }
    else if ((*buf)[0] == '0' && ((*buf)[1] == 'x' || (*buf)[1] == 'X') )
    {
	n = 0;
	*buf+=2;
	for (; (**buf >= '0' && **buf <= '9') ||
	       (**buf >= 'A' && **buf <= 'F') ||
	       (**buf >= 'a' && **buf <= 'f'); (*buf)++ )
	{
	    c = **buf | 0x20;
	    n = (n << 4) + c - (c > '9' ? ('a' - 10) : '0');
	}
    }
    else if (**buf >= '0' && **buf <= '9' )
    {
	n = 0;
	for (; (**buf >= '0' && **buf <= '9'); (*buf)++ )
	    n = n * 10 + (**buf - '0');
    }
    return n > 0xff ? -1 : n;
}

static int parse_line(char *buf, int *from, int *to, char *op, int *val, int *mod)
{
    char *s, *s2;
    int ret = 0, ina = 0;

    s = buf;
    while ( *s != '\0' )
    {
	switch ( *s )
	{
	    case '\t':
		*s = ' ';
		break;
	    case '\'':
		ina ^= 1;
		if( ina )
		    ++s;
		break;
	    case '\n':
	    case '\r':
	    case '#':
		*s = '\0';
		break;
	}
	if ( *s != '\0' )
	    ++s;
    }

    s=buf;
    skip_blank(&s);

    if ( *s == '@' )
    {
	++s;
	s2 = buf;
	while ( *s != '\0' && *s != ' ' )
	    *s2++ = *s++;
	*s2 = '\0';
	ret = strlen(buf) > 0 ? 2 : -1;
    }
    else if ( *s != '\0' )
    {
	ret = *from = *to = *val = *mod = -1;
	*op = '=';

	*from = get_val(&s);
	if ( *from >= 0 )
	{
	    if ( *s == '-' )
	    {
		++s;
		*to = get_val(&s);
	    }
	    else
		*to = *from;
	}

	if ( *to >= 0 && *s == ':' && s[1] == ' ' )
	{
	    ++s;
	    skip_blank(&s);
	    if ( *s == '*' && (s[1] == '+' || s[1] == '-' || s[1] == '&' ||
			       s[1] == '|' || s[1] == '^' || s[1] == '=' ||
			       s[1] == ' ') )
	    {
		*op = s[1];
		s+=2;
	    }
	    *val = *op == ' ' ? 0 : get_val(&s);
	    if ( *val >= 0 )
	    {
		skip_blank(&s);
		*mod = get_val(&s);
		skip_blank(&s);
		if ( *mod >=0 && *mod <= 5 && *s == '\0' )
		    ret = 1;
	    }
	}
    }
    return ret;
}

static void chrmap_init( int *piTransTbl )
{
    int i;

    for ( i = 0; i < 256; ++i)
	piTransTbl[i] = (i < 128 ? 1 : 0) << 16 | i;
}

static int chrmap_parse( FILE *fp, char *pszTerm, int *nTransTbl, char *pszFile )
{
    int line = 0, from = 0, to = 0, val = 0, mod = 0, i, n;
    char buf[256], *s, op = 0;
    int isTerm = 0;
    fpos_t pos;

    fgetpos(fp, &pos);
    rewind(fp);

    while ( !feof(fp) && isTerm < 2 )
    {
	++line;
	if ( fgets(buf, sizeof(buf), fp) != NULL )
	{
	    n = 0;
	    if ( *buf == ':' )
	    {
		if ( isTerm == 1 )
		    isTerm = 2;
		else
		{
		    *buf = '|';
		    s = buf;
		    while ( *s != '\0' && *s != ' ' && *s != '\t' &&
			    *s != '\n' && *s != '\r' )
			++s;
		    *s = '\0';
		    s = buf;
		    i = strlen(pszTerm);
		    while ( isTerm == 0 && (s = strstr(s+1, pszTerm)) != NULL )
		    {
			if ( *(s-1) == '|' && 
			     ( s[i] == '|' || s[i] == '\0' ) )
			    isTerm = 1;
		    }
		}
	    }
	    else if ( isTerm == 1 )
	    {
		n = parse_line(buf, &from, &to, &op, &val, &mod);
	    }

	    if( n == 2 )
	    {
		n = chrmap_parse( fp, buf, nTransTbl, pszFile );
	    }
	    else if( n == 1 )
	    {
		/* printf("line: %3d\tfrom=%d, to=%d, op='%c', val=%d, mod=%d\n", line, from, to, op, val, mod); */
		for ( i = from; i <= to; ++i)
		{
		    switch (op)
		    {
			case '|':
			    nTransTbl[i] = (i | val);
			    break;
			case '&':
			    nTransTbl[i] = (i & val);
			    break;
			case '^':
			    nTransTbl[i] = (i ^ val);
			    break;
			case '+':
			    nTransTbl[i] = (i + val) & 0xff;
			    break;
			case '-':
			    nTransTbl[i] = (i - val) & 0xff;
			    break;
			case '=':
			    nTransTbl[i] = val;
			    break;
			case '*':
			case ' ':
			default:
			    nTransTbl[i] = i;
			    break;
		    }
		    nTransTbl[i] |= mod << 16;
		}
	    }
	    else if ( n == -1 )
	    {
		printf("file: %s, parse error at line: %d\n", pszFile, line);
	    }
	}
    }

    fsetpos(fp, &pos);

    return isTerm;
}

static int hb_gt_crs_chrmapread( char *pszFile, char *pszTerm, int *nTransTbl )
{
    FILE *fp;
    char buf[256], *ptr, *pTerm;
    int isTerm = -1;

    fp = fopen(pszFile, "r");

    if ( fp != NULL )
    {
	strncpy(buf, pszTerm, sizeof(buf));
	buf[sizeof(buf) - 1] = '\0';
	isTerm = 0;
	pTerm = buf;
	while ( pTerm )
	{
	    if ( (ptr = strchr( pTerm, '/' )) != NULL )
		*ptr++ = '\0';

	    if ( *pTerm )
		if ( chrmap_parse( fp, pTerm, nTransTbl, pszFile ) > 0 )
		    isTerm = 1;

	    pTerm = ptr;
	}
	fclose(fp);
    }
    return isTerm;
}

int hb_gt_crs_chrmapinit( int *piTransTbl, char *pszTerm )
{
    char *pszFile, szFile[_POSIX_PATH_MAX + 1];
    int nRet = -1;

    chrmap_init( piTransTbl );
    
    if ( pszTerm == NULL || *pszTerm == '\0' )
	pszTerm = getenv("HB_TERM");
    if ( pszTerm == NULL || *pszTerm == '\0' )
	pszTerm = getenv("TERM");

    if ( pszTerm != NULL && *pszTerm != '\0' )
    {
	pszFile = getenv("HB_CHRMAP");
	if ( pszFile != NULL && *pszFile != '\0' )
	    nRet = hb_gt_crs_chrmapread( pszFile, pszTerm, piTransTbl );
	if ( nRet == -1 )
	{
	    pszFile = getenv("HB_ROOT");
	    if ( pszFile != NULL && sizeof(szFile) > 
		             strlen(pszFile) + strlen(s_szDefaultCharMapFile) )
	    {
		strcpy(szFile, pszFile);
		strcat(szFile, s_szDefaultCharMapFile);
		nRet = hb_gt_crs_chrmapread( szFile, pszTerm, piTransTbl );
	    }
	}
	if ( nRet == -1 )
	    nRet = hb_gt_crs_chrmapread( s_szDefaultCharMapFile, pszTerm, piTransTbl );
    }

    return nRet;
}

/*
int main(int argc, char **argv)
{
    int piTransTbl[256], i;
    
    if ( hb_gt_crs_chrmapinit( piTransTbl, NULL ) == -1 )
    {
	printf("cannot init charmap.\n");
	exit(1);
    }

    for ( i=0; i<256; i++)
	printf("%3d -> %3d : %d\n", i, piTransTbl[i]&0xff, piTransTbl[i]>>16);

    return 0;
}
*/
