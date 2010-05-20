// This module implements the QsciCommandSet class.
//
// Copyright (c) 2010 Riverbank Computing Limited <info@riverbankcomputing.com>
// 
// This file is part of QScintilla.
// 
// This file may be used under the terms of the GNU General Public
// License versions 2.0 or 3.0 as published by the Free Software
// Foundation and appearing in the files LICENSE.GPL2 and LICENSE.GPL3
// included in the packaging of this file.  Alternatively you may (at
// your option) use any later version of the GNU General Public
// License if such license has been publicly approved by Riverbank
// Computing Limited (or its successors, if any) and the KDE Free Qt
// Foundation. In addition, as a special exception, Riverbank gives you
// certain additional rights. These rights are described in the Riverbank
// GPL Exception version 1.1, which can be found in the file
// GPL_EXCEPTION.txt in this package.
// 
// Please review the following information to ensure GNU General
// Public Licensing requirements will be met:
// http://trolltech.com/products/qt/licenses/licensing/opensource/. If
// you are unsure which license is appropriate for your use, please
// review the following information:
// http://trolltech.com/products/qt/licenses/licensing/licensingoverview
// or contact the sales department at sales@riverbankcomputing.com.
// 
// This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
// WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.


#include "qscicommandset.h"

#include <qsettings.h>

#include "qsciscintilla.h"
#include "qsciscintillabase.h"


// The ctor.
QsciCommandSet::QsciCommandSet(QsciScintilla *qs) : qsci(qs)
{
    struct sci_cmd {
        int msg;
        int key;
        int altkey;
        const char *desc;
    };

    // This is based on the default table in src/KeyMap.cxx.
    static struct sci_cmd cmd_table[] = {
        {
            QsciScintillaBase::SCI_LINEDOWN,
            Qt::Key_Down,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Move down one line")
        },
        {
            QsciScintillaBase::SCI_LINEDOWNEXTEND,
            Qt::Key_Down | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend selection down one line")
        },
        {
            QsciScintillaBase::SCI_LINESCROLLDOWN,
            Qt::Key_Down | Qt::CTRL,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Scroll view down one line")
        },
        {
            QsciScintillaBase::SCI_LINEDOWNRECTEXTEND,
            Qt::Key_Down | Qt::ALT | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend rectangular selection down one line")
        },
        {
            QsciScintillaBase::SCI_LINEUP,
            Qt::Key_Up,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Move up one line")
        },
        {
            QsciScintillaBase::SCI_LINEUPEXTEND,
            Qt::Key_Up | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend selection up one line")
        },
        {
            QsciScintillaBase::SCI_LINESCROLLUP,
            Qt::Key_Up | Qt::CTRL,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Scroll view up one line")
        },
        {
            QsciScintillaBase::SCI_LINEUPRECTEXTEND,
            Qt::Key_Up | Qt::ALT | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend rectangular selection up one line")
        },
        {
            QsciScintillaBase::SCI_PARAUP,
            Qt::Key_BracketLeft | Qt::CTRL,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Move up one paragraph")
        },
        {
            QsciScintillaBase::SCI_PARAUPEXTEND,
            Qt::Key_BracketLeft | Qt::CTRL | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend selection up one paragraph")
        },
        {
            QsciScintillaBase::SCI_PARADOWN,
            Qt::Key_BracketRight | Qt::CTRL,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Move down one paragraph")
        },
        {
            QsciScintillaBase::SCI_PARADOWNEXTEND,
            Qt::Key_BracketRight | Qt::CTRL | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend selection down one paragraph")
        },
        {
            QsciScintillaBase::SCI_CHARLEFT,
            Qt::Key_Left,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Move left one character")
        },
        {
            QsciScintillaBase::SCI_CHARLEFTEXTEND,
            Qt::Key_Left | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend selection left one character")
        },
        {
            QsciScintillaBase::SCI_WORDLEFT,
            Qt::Key_Left | Qt::CTRL,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Move left one word")
        },
        {
            QsciScintillaBase::SCI_WORDLEFTEXTEND,
            Qt::Key_Left | Qt::SHIFT | Qt::CTRL,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend selection left one word")
        },
        {
            QsciScintillaBase::SCI_CHARLEFTRECTEXTEND,
            Qt::Key_Left | Qt::ALT | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend rectangular selection left one character")
        },
        {
            QsciScintillaBase::SCI_CHARRIGHT,
            Qt::Key_Right,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Move right one character")
        },
        {
            QsciScintillaBase::SCI_CHARRIGHTEXTEND,
            Qt::Key_Right | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend selection right one character")
        },
        {
            QsciScintillaBase::SCI_WORDRIGHT,
            Qt::Key_Right | Qt::CTRL,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Move right one word")
        },
        {
            QsciScintillaBase::SCI_WORDRIGHTEXTEND,
            Qt::Key_Right | Qt::CTRL | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend selection right one word")
        },
        {
            QsciScintillaBase::SCI_CHARRIGHTRECTEXTEND,
            Qt::Key_Right | Qt::ALT | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend rectangular selection right one character")
        },
        {
            QsciScintillaBase::SCI_WORDPARTLEFT,
            Qt::Key_Slash | Qt::CTRL,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Move left one word part")
        },
        {
            QsciScintillaBase::SCI_WORDPARTLEFTEXTEND,
            Qt::Key_Slash | Qt::CTRL | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend selection left one word part")
        },
        {
            QsciScintillaBase::SCI_WORDPARTRIGHT,
            Qt::Key_Backslash | Qt::CTRL,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Move right one word part")
        },
        {
            QsciScintillaBase::SCI_WORDPARTRIGHTEXTEND,
            Qt::Key_Backslash | Qt::CTRL | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend selection right one word part")
        },
        {
            QsciScintillaBase::SCI_VCHOME,
            Qt::Key_Home,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Move to first visible character in line")
        },
        {
            QsciScintillaBase::SCI_VCHOMEEXTEND,
            Qt::Key_Home | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend selection to first visible character in line")
        },
        {
            QsciScintillaBase::SCI_DOCUMENTSTART,
            Qt::Key_Home | Qt::CTRL,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Move to start of text")
        },
        {
            QsciScintillaBase::SCI_DOCUMENTSTARTEXTEND,
            Qt::Key_Home | Qt::CTRL | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend selection to start of text")
        },
        {
            QsciScintillaBase::SCI_HOMEDISPLAY,
            Qt::Key_Home | Qt::ALT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Move to start of displayed line")
        },
        {
            QsciScintillaBase::SCI_HOMEDISPLAYEXTEND,
            0,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend selection to start of line")
        },
        {
            QsciScintillaBase::SCI_VCHOMERECTEXTEND,
            Qt::Key_Home | Qt::ALT | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend rectangular selection to first visible character in line")
        },
        {
            QsciScintillaBase::SCI_LINEEND,
            Qt::Key_End,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Move to end of line")
        },
        {
            QsciScintillaBase::SCI_LINEENDEXTEND,
            Qt::Key_End | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend selection to end of line")
        },
        {
            QsciScintillaBase::SCI_DOCUMENTEND,
            Qt::Key_End | Qt::CTRL,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Move to end of text")
        },
        {
            QsciScintillaBase::SCI_DOCUMENTENDEXTEND,
            Qt::Key_End | Qt::CTRL | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend selection to end of text")
        },
        {
            QsciScintillaBase::SCI_LINEENDDISPLAY,
            Qt::Key_End | Qt::ALT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Move to end of displayed line")
        },
        {
            QsciScintillaBase::SCI_LINEENDDISPLAYEXTEND,
            0,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend selection to end of displayed line")
        },
        {
            QsciScintillaBase::SCI_LINEENDRECTEXTEND,
            Qt::Key_End | Qt::ALT | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend rectangular selection to end of line")
        },
        {
            QsciScintillaBase::SCI_PAGEUP,
            Qt::Key_PageUp,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Move up one page")
        },
        {
            QsciScintillaBase::SCI_PAGEUPEXTEND,
            Qt::Key_PageUp | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend selection up one page")
        },
        {
            QsciScintillaBase::SCI_PAGEUPRECTEXTEND,
            Qt::Key_PageUp | Qt::ALT | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend rectangular selection up one page")
        },
        {
            QsciScintillaBase::SCI_PAGEDOWN,
            Qt::Key_PageDown,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Move down one page")
        },
        {
            QsciScintillaBase::SCI_PAGEDOWNEXTEND,
            Qt::Key_PageDown | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend selection down one page")
        },
        {
            QsciScintillaBase::SCI_PAGEDOWNRECTEXTEND,
            Qt::Key_PageDown | Qt::ALT | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Extend rectangular selection down one page")
        },
        {
            QsciScintillaBase::SCI_CLEAR,
            Qt::Key_Delete,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Delete current character")
        },
        {
            QsciScintillaBase::SCI_CUT,
            Qt::Key_X | Qt::CTRL,
            Qt::Key_Delete | Qt::SHIFT,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Cut selection")
        },
        {
            QsciScintillaBase::SCI_DELWORDRIGHT,
            Qt::Key_Delete | Qt::CTRL,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Delete word to right")
        },
        {
            QsciScintillaBase::SCI_DELLINERIGHT,
            Qt::Key_Delete | Qt::CTRL | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Delete line to right")
        },
        {
            QsciScintillaBase::SCI_EDITTOGGLEOVERTYPE,
            Qt::Key_Insert,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Toggle insert/overtype")
        },
        {
            QsciScintillaBase::SCI_PASTE,
            Qt::Key_V | Qt::CTRL,
            Qt::Key_Insert | Qt::SHIFT,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Paste")
        },
        {
            QsciScintillaBase::SCI_COPY,
            Qt::Key_C | Qt::CTRL,
            Qt::Key_Insert | Qt::CTRL,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Copy selection")
        },
        {
            QsciScintillaBase::SCI_CANCEL,
            Qt::Key_Escape,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Cancel")
        },
        {
            QsciScintillaBase::SCI_DELETEBACK,
            Qt::Key_Backspace,
            Qt::Key_Backspace | Qt::SHIFT,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Delete previous character")
        },
        {
            QsciScintillaBase::SCI_DELWORDLEFT,
            Qt::Key_Backspace | Qt::CTRL,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Delete word to left")
        },
        {
            QsciScintillaBase::SCI_UNDO,
            Qt::Key_Z | Qt::CTRL,
            Qt::Key_Backspace | Qt::ALT,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Undo the last command")
        },
        {
            QsciScintillaBase::SCI_DELLINELEFT,
            Qt::Key_Backspace | Qt::CTRL | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Delete line to left")
        },
        {
            QsciScintillaBase::SCI_REDO,
            Qt::Key_Y | Qt::CTRL,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Redo last command")
        },
        {
            QsciScintillaBase::SCI_SELECTALL,
            Qt::Key_A | Qt::CTRL,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Select all text")
        },
        {
            QsciScintillaBase::SCI_TAB,
            Qt::Key_Tab,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Indent one level")
        },
        {
            QsciScintillaBase::SCI_BACKTAB,
            Qt::Key_Tab | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Move back one indentation level")
        },
        {
            QsciScintillaBase::SCI_NEWLINE,
            Qt::Key_Return,
            Qt::Key_Return | Qt::SHIFT,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Insert new line")
        },
        {
            QsciScintillaBase::SCI_ZOOMIN,
            Qt::Key_Plus | Qt::CTRL,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Zoom in")
        },
        {
            QsciScintillaBase::SCI_ZOOMOUT,
            Qt::Key_Minus | Qt::CTRL,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Zoom out")
        },
        {
            QsciScintillaBase::SCI_SETZOOM,
            0,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Set zoom")
        },
        {
            QsciScintillaBase::SCI_FORMFEED,
            0,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Formfeed")
        },
        {
            QsciScintillaBase::SCI_LINECUT,
            Qt::Key_L | Qt::CTRL,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Cut current line")
        },
        {
            QsciScintillaBase::SCI_LINEDELETE,
            Qt::Key_L | Qt::CTRL | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Delete current line")
        },
        {
            QsciScintillaBase::SCI_LINECOPY,
            Qt::Key_T | Qt::CTRL | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Copy current line")
        },
        {
            QsciScintillaBase::SCI_LINETRANSPOSE,
            Qt::Key_T | Qt::CTRL,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Swap current and previous lines")
        },
        {
            QsciScintillaBase::SCI_SELECTIONDUPLICATE,
            Qt::Key_D | Qt::CTRL,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Duplicate selection")
        },
        {
            QsciScintillaBase::SCI_LOWERCASE,
            Qt::Key_U | Qt::CTRL,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Convert selection to lower case")
        },
        {
            QsciScintillaBase::SCI_UPPERCASE,
            Qt::Key_U | Qt::CTRL | Qt::SHIFT,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Convert selection to upper case")
        },
        {
            QsciScintillaBase::SCI_DELETEBACKNOTLINE,
            0,
            0,
            QT_TRANSLATE_NOOP("QsciCommand",
                "Delete previous character if not at line start")
        },
    };

    // Clear the default map.
    qsci->SendScintilla(QsciScintillaBase::SCI_CLEARALLCMDKEYS);

    // By default control characters don't do anything (rather than insert the
    // control character into the text).
    for (int k = 'A'; k <= 'Z'; ++k)
        qsci->SendScintilla(QsciScintillaBase::SCI_ASSIGNCMDKEY,
                k + (QsciScintillaBase::SCMOD_CTRL << 16),
                QsciScintillaBase::SCI_NULL);

    for (int i = 0; i < sizeof (cmd_table) / sizeof (cmd_table[0]); ++i)
        cmds.append(new QsciCommand(qsci, cmd_table[i].msg, cmd_table[i].key,
                    cmd_table[i].altkey, cmd_table[i].desc));
}


// The dtor.
QsciCommandSet::~QsciCommandSet()
{
    for (int i = 0; i < cmds.count(); ++i)
        delete cmds.at(i);
}


// Read the command set from settings.
bool QsciCommandSet::readSettings(QSettings &qs, const char *prefix)
{
    bool rc = true;
    QString skey;

    for (int i = 0; i < cmds.count(); ++i)
    {
        QsciCommand *cmd = cmds.at(i);

        skey.sprintf("%s/keymap/c%d/", prefix, cmd->msgId());

        int key;
        bool ok;

        // Read the key.
        ok = qs.contains(skey + "key");
        key = qs.value(skey + "key", 0).toInt();

        if (ok)
            cmd->setKey(key);
        else
            rc = false;

        // Read the alternate key.
        ok = qs.contains(skey + "alt");
        key = qs.value(skey + "alt", 0).toInt();

        if (ok)
            cmd->setAlternateKey(key);
        else
            rc = false;
    }

    return rc;
}


// Write the command set to settings.
bool QsciCommandSet::writeSettings(QSettings &qs, const char *prefix)
{
    bool rc = true;
    QString skey;

    for (int i = 0; i < cmds.count(); ++i)
    {
        QsciCommand *cmd = cmds.at(i);

        skey.sprintf("%s/keymap/c%d/", prefix, cmd->msgId());

        // Write the key.
        qs.setValue(skey + "key", cmd->key());

        // Write the alternate key.
        qs.setValue(skey + "alt", cmd->key());
    }

    return rc;
}


// Clear the key bindings.
void QsciCommandSet::clearKeys()
{
    for (int i = 0; i < cmds.count(); ++i)
        cmds.at(i)->setKey(0);
}


// Clear the alternate key bindings.
void QsciCommandSet::clearAlternateKeys()
{
    for (int i = 0; i < cmds.count(); ++i)
        cmds.at(i)->setAlternateKey(0);
}
