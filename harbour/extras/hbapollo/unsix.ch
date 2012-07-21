/*
 * $Id: unsix.ch 9576 2012-07-17 16:41:57Z andijahja $
 */

/*
 * SixAPI Project source code:
 *
 * Copyright 2010 Andi Jahja <xharbour@telkom.net.id>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 */

#ifndef __UNSIX__
#define __UNSIX__

#xuntranslate USE <cDBF> [ALIAS <(cAlias)>] [<rdd: VIA, RDD, FLAVOR> <(cRDD)>] [VAR <nArea>] [<(cOpenMode)>]
#xuntranslate CREATE <file: TABLE, DBF, FILE> <cDBF> <stru: STRUCT, STRUCTURE> <aStruct> [VAR <nArea>] [<driver: RDD, VIA, FLAVOR> <cDriver>] [ALIAS <cAlias>]
#xuntranslate SET DATE <cCountry>
#xuntranslate REPLACE <cField> WITH <vData>
#xuntranslate INDEX ON <cExpression> TO <cIndexFile> [ FOR <cCondition> ] [<mod: UNIQUE, RYO>] [<order: DESCENDING>]
#xuntranslate Seek(<cSeek>)
#xuntranslate Found()

#xuntranslate SET TRIM <ON>
#xuntranslate SET SOFTSEEK <ON>
#xuntranslate SET CENTURY <ON>
#xuntranslate SET EXACT <ON>
#xuntranslate SET DELETED <ON>
#xuntranslate SET EPOCH [TO] <nEpoch>
#xuntranslate SET LOCK TIMEOUT <nTimeOut>
#xuntranslate SET [FILE] HANDLE [TO] <nHandles>

#xuntranslate SKIP [<nSkip>]
#xuntranslate Bof()
#xuntranslate Eof()
#xuntranslate RecCount()
#xuntranslate SetSoftSeek([<x>])
#xuntranslate RecNo()
#xuntranslate SetDateFormat( <cDateFormat> )

#xuntranslate Alias()
#xuntranslate FCount()
#xuntranslate FieldPut(<x>,<vData>)
#xuntranslate FieldGet(<x>)
#xuntranslate FieldGetStr(<x>)
#xuntranslate FieldGetDtos(<x>)
#xuntranslate FieldGetJulian(<x>)
#xuntranslate FieldPos(<x>)
#xuntranslate FieldName(<x>)
#xuntranslate FieldType(<x>)
#xuntranslate FieldWidth(<x>)
#xuntranslate FieldOffset(<x>)
#xuntranslate FieldDecimals(<x>)

#xuncommand APPEND BLANK
#xuncommand GO [TO] <nRecNo>
#xuncommand GO TOP
#xuncommand GO TO TOP
#xuncommand GOTOP
#xuncommand GO BOTTOM
#xuncommand GO TO BOTTOM
#xuncommand GOBOTTOM
#xuncommand ZAP
#xuncommand COMMIT
#xuncommand CLOSE ALL
#xuncommand CLOSE DATABASE
#xuncommand CLOSE DATA
#xuncommand PACK

#endif //  end __UNSIX__
