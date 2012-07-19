/*
 * $Id$
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

#ifndef __SIXAPI__
#define __SIXAPI__

#command SET INDEX TO [ <(index1)> [, <(indexn)>]] [<add: ADDITIVE>]    ;
                                                                        ;
      => if !<.add.> ; sx_CloseIndexes() ; end                          ;
                                                                        ;
      [; sx_IndexOpen( <(index1)> )]                                    ;
      [; sx_IndexOpen( <(indexn)> )]

#command SET RELATION TO        => sx_ClearRelation()

#command SET RELATION                                                   ;
         [<add:ADDITIVE>]                                               ;
         [TO <key1> INTO <(alias1)> [, [TO] <keyn> INTO <(aliasn)>]]    ;
                                                                        ;
      => if ( !<.add.> )                                                ;
       ;    sx_ClearRelation()                                          ;
       ; end                                                            ;
                                                                        ;
       ; sx_SetRelation( <(alias1)>, <"key1"> )                         ;
      [; sx_SetRelation( <(aliasn)>, <"keyn"> )]

#command APPEND [FROM <file>]                                           ;
         [FOR <for>]                                                    ;
         [VIA <rdd>]                                                    ;
                                                                        ;
      => sx_AppendFrom(                                                 ;
                  <file>,                                               ;
                  <rdd>,<(for)>                                         ;
                )

#command REPLACE [ <f1> WITH <x1> [, <fn> WITH <xn>] ]                  ;
         [FOR <for>]                                                    ;
         [WHILE <while>]                                                ;
         [AREA  <alias>]                                                ;
         [NEXT <next>]                                                  ;
         [RECORD <rec>]                                                 ;
         [<rest:REST>]                                                  ;
         [ALL]                                                          ;
                                                                        ;
      => sx_DBEval(                                                     ;
             {|| sx_Replace(<(f1)>,<x1>) [, sx_Replace(<(fn)>,<xn>)]},  ;
             <{for}>, <{while}>, <next>, <rec>, <.rest.>,<(alias)>      ;
               )

#command DELETE                                                         ;
         [FOR <for>]                                                    ;
         [WHILE <while>]                                                ;
         [AREA  <alias>]                                                ;
         [NEXT <next>]                                                  ;
         [RECORD <rec>]                                                 ;
         [<rest:REST>]                                                  ;
         [ALL]                                                          ;
                                                                        ;
      => sx_DBEval(                                                     ;
                 {|| sx_Delete()},                                      ;
                 <{for}>, <{while}>, <next>, <rec>, <.rest.>,<(alias)>  ;
               )

#command RECALL                                                         ;
         [FOR <for>]                                                    ;
         [WHILE <while>]                                                ;
         [AREA  <alias>]                                                ;
         [NEXT <next>]                                                  ;
         [RECORD <rec>]                                                 ;
         [<rest:REST>]                                                  ;
         [ALL]                                                          ;
                                                                        ;
      => sx_DBEval(                                                     ;
                 {|| sx_Recall()},                                      ;
                 <{for}>, <{while}>, <next>, <rec>, <.rest.>,<(alias)>  ;
               )

#command COUNT [TO <var>]                                               ;
         [FOR <for>]                                                    ;
         [WHILE <while>]                                                ;
         [AREA  <alias>]                                                ;
         [NEXT <next>]                                                  ;
         [RECORD <rec>]                                                 ;
         [<rest:REST>]                                                  ;
         [ALL]                                                          ;
                                                                        ;
      => <var> := 0                                                     ;
       ; sx_DBEval(                                                     ;
                 {|| <var> := <var> + 1},                               ;
                 <{for}>, <{while}>, <next>, <rec>, <.rest.>,<(alias)>  ;
               )

#command SUM [ <x1> [, <xn>]  TO  <v1> [, <vn>] ]                       ;
         [FOR <for>]                                                    ;
         [WHILE <while>]                                                ;
         [AREA  <alias>]                                                ;
         [NEXT <next>]                                                  ;
         [RECORD <rec>]                                                 ;
         [<rest:REST>]                                                  ;
         [ALL]                                                          ;
                                                                        ;
      => <v1> := [ <vn> := ] 0                                          ;
       ; sx_DBEval(                                                     ;
         {|| <v1> := <v1> + sx_GetValue(<(x1)>) [, <vn> := <vn> + sx_GetValue(<(xn)>)]},;
         <{for}>, <{while}>, <next>, <rec>, <.rest.>, <(alias)>         ;
               )

#xtranslate AVERAGE [ <x1> [, <xn>]  TO  <v1> [, <vn>] ]                ;
        [FOR <for>]                                                     ;
        [WHILE <while>]                                                 ;
        [AREA  <alias>]                                                 ;
        [NEXT <next>]                                                   ;
        [RECORD <rec>]                                                  ;
        [<rest:REST>]                                                   ;
        [ALL]                                                           ;
                                                                        ;
     => M->__Avg := <v1> := [ <vn> := ] 0                               ;
      ; sx_DBEval(                                                      ;
           {|| M->__Avg := M->__Avg + 1,                                ;
           <v1> := <v1> + sx_GetValue(<(x1)>) [, <vn> := <vn> + sx_GetValue(<(xn)>)] },;
           <{for}>, <{while}>, <next>, <rec>, <.rest.>, <(alias)>)      ;
      ; <v1> := <v1> / M->__Avg [; <vn> := <vn> / M->__Avg ]

#command TOTAL [TO <(file)>] [ON <key>]                                 ;
         [FIELDS <fields,...>]                                          ;
         [FOR <for>]                                                    ;
         [WHILE <while>]                                                ;
         [NEXT <next>]                                                  ;
         [RECORD <rec>]                                                 ;
         [<rest:REST>]                                                  ;
         [ALL]                                                          ;
                                                                        ;
      => __sx_dbTotal(                                                  ;
                    <(file)>, <{key}>, { <(fields)> },                  ;
                    <{for}>, <{while}>, <next>, <rec>, <.rest.>         ;
                  )


#command COPY [STRUCTURE] [TO <(file)>] [FIELDS <fields,...>]           ;
      => sx_CopyStructure( <(file)>, { <(fields)> } )

#command COPY [STRUCTURE] EXTENDED [TO <(file)>]                        ;
      => sx_CopyStructureExtended (<(file)>)

#command COPY [TO <(file)>] [DELIMITED [WITH <*delim*>]]                ;
         [FIELDS <fields,...>]                                          ;
         [FOR <for>]                                                    ;
         [WHILE <while>]                                                ;
         [AREA  <alias>]                                                ;
         [NEXT <next>]                                                  ;
         [RECORD <rec>]                                                 ;
         [<rest:REST>]                                                  ;
         [ALL]                                                          ;
                                                                        ;
      => __sx_dbDelim(                                                  ;
                        <(file)>, <delim>, { <(fields)> },              ;
                        <{for}>, <{while}>, <next>, <rec>, <.rest.>,    ;
                        <(alias)>                                       ;
                      )

#command COPY [TO <(file)>] [SDF]                                       ;
         [FIELDS <fields,...>]                                          ;
         [FOR <for>]                                                    ;
         [WHILE <while>]                                                ;
         [AREA  <alias>]                                                ;
         [NEXT <next>]                                                  ;
         [RECORD <rec>]                                                 ;
         [<rest:REST>]                                                  ;
         [ALL]                                                          ;
                                                                        ;
      => __sx_dbDelim(                                                  ;
                        <(file)>, "SDF", { <(fields)> },                ;
                        <{for}>, <{while}>, <next>, <rec>, <.rest.>,    ;
                        <(alias)>                                       ;
                      )

#command COPY [TO <(file)>]                                             ;
         [FIELDS <fields,...>]                                          ;
         [FOR <for>]                                                    ;
         [WHILE <while>]                                                ;
         [AREA <alias>]                                                 ;
         [NEXT <next>]                                                  ;
         [RECORD <rec>]                                                 ;
         [<rest:REST>]                                                  ;
         [VIA <rdd>]                                                    ;
         [ALL]                                                          ;
                                                                        ;
      => __sx_dbCopy(                                                   ;
                   <(file)>, { <(fields)> },                            ;
                   <{for}>, <{while}>, <next>, <rec>, <.rest.>, <rdd>,  ;
                   <(alias)>                                            ;
                 )

#command CREATE <(file1)>                                               ;
            [FROM <(file2)>]                                            ;
            [<driver: VIA, RDD> <(rdd)>]                                               ;
            [ALIAS <a>]                                                 ;
         => sx_CreateFrom( <(file1)>, <(a)>, <(file2)>, <(rdd)> )

#command COPYTEXT TO <(cTextFile)> [DELIMITED WITH <delim>] [ALIAS <alias>] => ;
   ;
   sx_CopyFileText( <(cTextFile)>, <delim>, <(alias)> )

#command SORT [TO <(file)>] [ON <fields,...>]                           ;
         [FOR <for>]                                                    ;
         [WHILE <while>]                                                ;
         [AREA <alias>]                                                 ;
         [NEXT <next>]                                                  ;
         [RECORD <rec>]                                                 ;
         [<rdd:VIA RDD>]                                                ;
         [<rest:REST>]                                                  ;
         [<descend:DESCENDING>]                                         ;
         [ALL]                                                          ;
                                                                        ;
      => __sx_dbSort(                                                   ;
                   <(file)>, { <(fields)> },                            ;
                   <{for}>, <{while}>, <next>, <rec>, <.rest.>,<(rdd)>, ;
                   <.descend.>, <(alias)>                               ;
                 )

#command USE <cDBF> [ALIAS <(cAlias)>] [<rdd: VIA, RDD, FLAVOR> <(cRDD)>] [VAR <nArea>] [COMMITLEVEL <nCommitLevel>] [<(cOpenMode)>] =>;
   [<nArea> :=];
      sx_Use(;
        <cDBF>,;
        [<(cAlias)>],;
        [<(cOpenMode)>],;
        [<(cRDD)>],;
        [<nCommitLevel>];
        )

#command CREATE <file: TABLE, DBF, FILE> <cDBF> <stru: STRUCT, STRUCTURE> <aStruct> [VAR <nArea>] [<driver: RDD, VIA, FLAVOR> <cDriver>] [ALIAS <cAlias>] [COMMITLEVEL <nCommitLevel> ] =>;
   [<nArea> :=];
      sx_DbCreate(;
        <cDBF>,;
        <aStruct>,;
        [<(cDriver)>],;
        [<(cAlias)>],;
        [<nCommitLevel>])

#command SET DATE <cCountry> =>;
   sx_SetDateFormat(<(cCountry)>)

#command REPLACE <cField>  [ALIAS <alias>]  WITH <vData>         ;
              [, <cFieldN> [ALIAS <aliasN>] WITH <vDataN> ] =>   ;
                                                                 ;
   sx_Replace( <(cField)>, <vData>, <(alias)> )                  ;
   [; sx_Replace( <(cFieldN)>, <vDataN>, <(aliasN)> ) ]

#command INDEX ON <cExpression> TO <cIndexFile> [ FOR <cCondition> ] [<mod: UNIQUE, RYO>] [<order: DESCENDING>] =>;
   sx_Index(;
     <(cIndexFile)>,;
     <(cExpression)>,;
     [<(mod)>],;
     [<.order.>],;
     [<(cCondition)>])

#command SET COMMITLEVEL [TO] <n> => Sx_CommitLevel(<n>)
#command SET ERRORLEVEL [TO] <n> => Sx_ErrorLevel(<n>)
#command SET RDD [TO] <(rdd)> => Sx_RddsetDefault(<(rdd)>)
#command SET TRIM <ON> => Sx_SetGetTrimmed(<(ON)>)
#command SET SOFTSEEK <ON> => Sx_SetSoftSeek(<(ON)>)
#command SET CENTURY <ON> => Sx_SetCentury(<(ON)>)
#command SET EXACT <ON> => Sx_SetExact(<(ON)>)
#command SET DELETED <ON> => Sx_SetDeleted(<(ON)>)
#command SET EPOCH [TO] <nEpoch> => Sx_SetEpoch(<nEpoch>)
#command SET LOCK TIMEOUT <nTimeOut> => Sx_SetLockTimeOut(<nTimeOut>)
#command SET [FILE] HANDLE [TO] <nHandles> => Sx_SetHandles(<nHandles>)
#command RECALL => sx_Recall()
#command DELETE => sx_Delete()
#command SKIP [<nSkip>] => sx_Skip([<nSkip>])
#command SELECT <selectarea> => sx_Select(sx_WorkArea( <(selectarea)> ))

#xtranslate Seek(<cSeek>) => sx_Seek(<cSeek>)
#xtranslate Found() => sx_Found()
#xtranslate dbSkip(<nSkip>) => sx_Skip(<nSkip>)
#xtranslate Bof() => sx_Bof()
#xtranslate Eof() => sx_Eof()
#xtranslate Deleted() => sx_Deleted()
#xtranslate RecCount() => sx_RecCount()
#xtranslate SetSoftSeek([<x>]) => sx_SetSoftSeek([<x>])
#xtranslate RecNo() => sx_RecNo()
#xtranslate SetDateFormat( <cDateFormat> ) => ;
   sx_SetDateFormat(<(cDateFormat)>)

#xtranslate Alias()               => sx_Alias()
#xtranslate FCount()              => sx_FieldCount()
#xtranslate FieldPut(<x>,<vData>) => sx_Replace(<(x)>,<vData>)
#xtranslate FieldGet(<x>)         => sx_GetValue(<(x)>)
#xtranslate FieldGetStr(<x>)      => sx_GetValueStr(<(x)>)
#xtranslate FieldGetDtos(<x>)     => sx_GetValueDtos(<(x)>)
#xtranslate FieldGetJulian(<x>)   => sx_GetDateJulian(<(x)>)
#xtranslate FieldPos(<x>)         => sx_FieldNum(<(x)>)
#xtranslate FieldName(<x>)        => sx_FieldName(<x>)
#xtranslate FieldType(<x>)        => sx_FieldType(<(x)>)
#xtranslate FieldWidth(<x>)       => sx_FieldWidth(<(x)>)
#xtranslate FieldOffset(<x>)      => sx_FieldOffset(<(x)>)
#xtranslate FieldDecimals(<x>)    => sx_FieldDecimals(<(x)>)

#xcommand APPEND BLANK [TO <(cArea)>] [<nHowMany>] => sx_Append([<(cArea)>],<nHowMany>)
#xcommand GO [TO] <nRecNo> [ALIAS <alias>] => sx_Go(<nRecNo>,<(alias)>)
#xcommand GOTO <nRecNo> [ALIAS <alias>] => sx_Go(<nRecNo>,<(alias)>)
#xcommand GO TOP [ALIAS <alias>] => sx_GoTop(<(alias)>)
#xcommand GO TO TOP [ALIAS <alias>] => sx_GoTop(<(alias)>)
#xcommand GOTOP [ALIAS <alias>] => sx_GoTop(<(alias)>)
#xcommand GO BOTTOM [ALIAS <alias>] => sx_GoBottom(<(alias)>)
#xcommand GO TO BOTTOM [ALIAS <alias>] => sx_GoBottom(<(alias)>)
#xcommand GOBOTTOM [ALIAS <alias>] => sx_GoBottom(<(alias)>)
#xcommand ZAP [<alias>] => sx_Zap(<(alias)>)
#xcommand COMMIT [ALIAS <alias>] => sx_Commit(<(alias)>)
#xcommand CLOSE ALL => sx_CloseAll()
#xcommand CLOSE [DATABASE] [ALIAS <alias>] => sx_Close(<(alias)>)
#xcommand CLOSE DATA => sx_CloseAll()
#xcommand CLOSE INDEXES => sx_CloseIndexes()
#xcommand PACK [<alias>] => sx_Pack(<(alias)>)
#xcommand BROWSE => sx_Browse()
#xcommand PAUSE => inkey(0)

#define INDEX_STANDARD            1
#define INDEX_STANDARD_UNIQUE     2
#define INDEX_CONDITIONAL         3
#define INDEX_CONDITIONAL_UNIQUE  4

#define COMMA_DELIM               21
#define SDF_FILE                  22
#define TAB_DELIM                 23
#define SPACE_DELIM               24

#endif /* __SIXAPI__ */
