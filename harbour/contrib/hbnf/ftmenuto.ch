/*
 * $Id$
 */

/*
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   13 Oct 1992 20:43:10   GLENN
 * Ted re-wrote the @ prompt / menu to replacement in Clipper and dumped
 * the assembler version; this .ch file was updated as well.
 *
 *    Rev 1.2   15 Aug 1991 23:08:46   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:55:14   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:40   GLENN
 * Nanforum Toolkit
 *
 */

/*
   These commands will simplify use of the FT_PROMPT() and
   FT_MENUTO() functions.  Refer to MENUTO.PRG for more info.
   These commands can replace the existing Clipper @ PROMPT and
   MENU TO commands.
*/

#xcommand @ <Row>, <Col> PROMPT <Prompt>                    ;
                        [COLOR <Attr>]                      ;
                        [MESSAGE <Message>]                 ;
                        [MSGROW <MsgRow>]                   ;
                        [MSGCOL <MsgCol>]                   ;
                        [MSGCOLOR <MsgColor>]               ;
                        [TRIGGER <Trigger>]                 ;
                        [TRIGGERCOLOR <TriggerColor>]       ;
                        [HOME <Home>]                       ;
                        [END <End>]                         ;
                        [UP <Up>]                           ;
                        [DOWN <Down>]                       ;
                        [LEFT <Left>]                       ;
                        [RIGHT <Right>]                     ;
                        [EXECUTE <Block>]                   ;
      => FT_Prompt( <Row>,                                  ;
                    <Col>,                                  ;
                    <Prompt>,                               ;
                    <Attr>,                                 ;
                    <MsgRow>,                               ;
                    <MsgCol>,                               ;
                    <Message>,                              ;
                    <MsgColor>,                             ;
                    <Trigger>,                              ;
                    <TriggerColor>,                         ;
                    <Home>,                                 ;
                    <End>,                                  ;
                    <Up>,                                   ;
                    <Down>,                                 ;
                    <Left>,                                 ;
                    <Right>,                                ;
                    <Block>                                 )

#xcommand MENU TO <v> [<l : COLD>] => ;
   <v> := FT_MenuTo( {| _1 | iif( _1 == NIL, <v>, <v> := _1 ) }, #<v>, <.l.> )
