/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for TLabelForm and TReportForm Classes
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#ifndef HB_RPTLBL_CH_
#define HB_RPTLBL_CH_

#define _RFRM_PAGENO            3       // "Page No."
#define _RFRM_SUBTOTAL          4       // "** Subtotal **"
#define _RFRM_SUBSUBTOTAL       5       // "* Subsubtotal *"
#define _RFRM_TOTAL             6       // "*** Total ***"

#define RPT_HEADER      1       // Array of header strings
#define RPT_WIDTH       2       // Numeric, report page width
#define RPT_LMARGIN     3       // Numeric, report page offset
#define RPT_RMARGIN     4       // NIL, Not used
#define RPT_LINES       5       // Numeric, number of lines per page
#define RPT_SPACING     6       // Numeric, single=1, double=2
#define RPT_BEJECT      7       // Logical, eject before 1st page, .T.=Yes .F.=No
#define RPT_AEJECT      8       // Logical, eject after last page, .T.=Yes .F.=No
#define RPT_PLAIN       9       // Logical, plain report, .T.=Yes .F.=No
#define RPT_SUMMARY     10      // Logical, no detail lines, .T.=Yes .F.=No
#define RPT_COLUMNS     11      // Array of Column arrays
#define RPT_GROUPS      12      // Array of Group arrays
#define RPT_HEADING     13      // Character, heading for the report

#define RPT_COUNT       13      // Number of elements in the Report array


// Column array definitions ( one array per column definition )
#define RCT_EXP         1       // Block, contains compiled column expression
#define RCT_TEXT        2       // Character, contains text column expression
#define RCT_TYPE        3       // Character, type of expression
#define RCT_HEADER      4       // Array of column heading strings
#define RCT_WIDTH       5       // Numeric, column width including decimals and
                                // decimal point
#define RCT_DECIMALS    6       // Numeric, number of decimal places
#define RCT_TOTAL       7       // Logical, total this column, .T.=Yes .F.=No
#define RCT_PICT        8       // Character, picture string

#define RCT_COUNT       8       // Number of elements in the Column array


// Group array definitions ( one array per group definition )
#define RGT_EXP         1       // Block, contains compiled group expression
#define RGT_TEXT        2       // Character, contains text group expression
#define RGT_TYPE        3       // Character, type of expression
#define RGT_HEADER      4       // Character, column heading string
#define RGT_AEJECT      5       // Logical, eject after group, .T.=Yes .F.=No

#define RGT_COUNT       5       // Number of elements in the Group array

#define SIZE_FILE_BUFF          1990    // Size of report file
#define SIZE_LENGTHS_BUFF       110
#define SIZE_OFFSETS_BUFF       110
#define SIZE_EXPR_BUFF          1440
#define SIZE_FIELDS_BUFF        300
#define SIZE_PARAMS_BUFF        24

// Definitions for offsets into the FILE_BUFF string
#define LENGTHS_OFFSET          5       // Start of expression length array
#define OFFSETS_OFFSET          115     // Start of expression position array
#define EXPR_OFFSET             225     // Start of expression data area
#define FIELDS_OFFSET           1665    // Start of report columns (fields)
#define PARAMS_OFFSET           1965    // Start of report parameters block

// These are offsets into the FIELDS_BUFF string to actual values
// Values are added to a block offset FLD_OFFSET that is moved in
// increments of 12
#define FIELD_WIDTH_OFFSET      1
#define FIELD_TOTALS_OFFSET     6
#define FIELD_DECIMALS_OFFSET   7

// These are offsets into FIELDS_BUFF which are used to 'point' into
// the EXPR_BUFF string which contains the textual data
#define FIELD_CONTENT_EXPR_OFFSET       9
#define FIELD_HEADER_EXPR_OFFSET        11

// These are actual offsets into the PARAMS_BUFF string which
// are used to 'point' into the EXPR_BUFF string
#define PAGE_HDR_OFFSET         1
#define GRP_EXPR_OFFSET         3
#define SUB_EXPR_OFFSET         5
#define GRP_HDR_OFFSET          7
#define SUB_HDR_OFFSET          9

// These are actual offsets into the PARAMS_BUFF string to actual values
#define PAGE_WIDTH_OFFSET       11
#define LNS_PER_PAGE_OFFSET     13
#define LEFT_MRGN_OFFSET        15
#define RIGHT_MGRN_OFFSET       17
#define COL_COUNT_OFFSET        19
#define DBL_SPACE_OFFSET        21
#define SUMMARY_RPT_OFFSET      22
#define PE_OFFSET               23
#define OPTION_OFFSET           24

// File error definitions
#define F_OK            0       // No error
#define F_EMPTY         -3      // File is empty
#define F_ERROR         -1      // Some kind of error
#define F_NOEXIST       2       // File does not exist

#define _LF_SAMPLES     2       // "Do you want more samples?"
#define _LF_YN          12      // "Y/N"

#define LBL_REMARK      1       // Character, remark from label file
#define LBL_HEIGHT      2       // Numeric, label height
#define LBL_WIDTH       3       // Numeric, label width
#define LBL_LMARGIN     4       // Numeric, left margin
#define LBL_LINES       5       // Numeric, lines between labels
#define LBL_SPACES      6       // Numeric, spaces between labels
#define LBL_ACROSS      7       // Numeric, number of labels across
#define LBL_FIELDS      8       // Array of Field arrays

#define LBL_COUNT       8       // Numeric, number of label fields

// Field array definitions ( one array per field )
#define LF_EXP          1       // Block, field expression
#define LF_TEXT         2       // Character, text of field expression
#define LF_BLANK        3       // Logical, compress blank fields, .T.=Yes .F.=No

#define LF_COUNT        3       // Numeric, number of elements in field array

#define BUFFSIZE        1034    // Size of label file
#define FILEOFFSET      74      // Start of label content descriptions
#define FIELDSIZE       60
#define REMARKOFFSET    2
#define REMARKSIZE      60
#define HEIGHTOFFSET    62
#define HEIGHTSIZE      2
#define WIDTHOFFSET     64
#define WIDTHSIZE       2
#define LMARGINOFFSET   66
#define LMARGINSIZE     2
#define LINESOFFSET     68
#define LINESSIZE       2
#define SPACESOFFSET    70
#define SPACESSIZE      2
#define ACROSSOFFSET    72
#define ACROSSSIZE      2

#endif /* HB_RPTLBL_CH_ */

