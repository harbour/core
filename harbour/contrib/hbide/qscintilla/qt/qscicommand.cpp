// This module implements the QsciCommand class.
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


#include "qscicommand.h"

#include <qnamespace.h>
#include <qapplication.h>

#include "qsciscintilla.h"
#include "qsciscintillabase.h"


static int convert(int key);


// The ctor.
QsciCommand::QsciCommand(QsciScintilla *qs, int msg, int key, int altkey,
        const char *desc)
    : qsCmd(qs), msgCmd(msg), qkey(key), qaltkey(altkey), descCmd(desc)
{
    scikey = convert(qkey);

    if (scikey)
        qsCmd->SendScintilla(QsciScintillaBase::SCI_ASSIGNCMDKEY, scikey,
                msgCmd);

    scialtkey = convert(qaltkey);

    if (scialtkey)
        qsCmd->SendScintilla(QsciScintillaBase::SCI_ASSIGNCMDKEY, scialtkey,
                msgCmd);
}


// Bind a key to a command.
void QsciCommand::setKey(int key)
{
    bindKey(key,qkey,scikey);
}


// Bind an alternate key to a command.
void QsciCommand::setAlternateKey(int altkey)
{
    bindKey(altkey,qaltkey,scialtkey);
}


// Do the hard work of binding a key.
void QsciCommand::bindKey(int key,int &qk,int &scik)
{
    int new_scikey;

    // Ignore if it is invalid, allowing for the fact that we might be
    // unbinding it.
    if (key)
    {
        new_scikey = convert(key);

        if (!new_scikey)
            return;
    }
    else
        new_scikey = 0;

    if (scik)
        qsCmd->SendScintilla(QsciScintillaBase::SCI_CLEARCMDKEY, scik);

    qk = key;
    scik = new_scikey;

    if (scik)
        qsCmd->SendScintilla(QsciScintillaBase::SCI_ASSIGNCMDKEY, scik, msgCmd);
}


// See if a key is valid.
bool QsciCommand::validKey(int key)
{
    return convert(key);
}


// Convert a Qt character to the Scintilla equivalent.  Return zero if it is
// invalid.
static int convert(int key)
{
    // Convert the modifiers.
    int sci_mod = 0;

    if (key & Qt::SHIFT)
        sci_mod |= QsciScintillaBase::SCMOD_SHIFT;

    if (key & Qt::CTRL)
        sci_mod |= QsciScintillaBase::SCMOD_CTRL;

    if (key & Qt::ALT)
        sci_mod |= QsciScintillaBase::SCMOD_ALT;

    key &= ~Qt::MODIFIER_MASK;

    // Convert the key.
    int sci_key;

    if (key > 0x7f)
        switch (key)
        {
        case Qt::Key_Down:
            sci_key = QsciScintillaBase::SCK_DOWN;
            break;

        case Qt::Key_Up:
            sci_key = QsciScintillaBase::SCK_UP;
            break;

        case Qt::Key_Left:
            sci_key = QsciScintillaBase::SCK_LEFT;
            break;

        case Qt::Key_Right:
            sci_key = QsciScintillaBase::SCK_RIGHT;
            break;

        case Qt::Key_Home:
            sci_key = QsciScintillaBase::SCK_HOME;
            break;

        case Qt::Key_End:
            sci_key = QsciScintillaBase::SCK_END;
            break;

        case Qt::Key_PageUp:
            sci_key = QsciScintillaBase::SCK_PRIOR;
            break;

        case Qt::Key_PageDown:
            sci_key = QsciScintillaBase::SCK_NEXT;
            break;

        case Qt::Key_Delete:
            sci_key = QsciScintillaBase::SCK_DELETE;
            break;

        case Qt::Key_Insert:
            sci_key = QsciScintillaBase::SCK_INSERT;
            break;

        case Qt::Key_Escape:
            sci_key = QsciScintillaBase::SCK_ESCAPE;
            break;

        case Qt::Key_Backspace:
            sci_key = QsciScintillaBase::SCK_BACK;
            break;

        case Qt::Key_Tab:
            sci_key = QsciScintillaBase::SCK_TAB;
            break;

        case Qt::Key_Return:
            sci_key = QsciScintillaBase::SCK_RETURN;
            break;

        default:
            sci_key = 0;
        }
    else
        sci_key = key;

    if (sci_key)
        sci_key |= (sci_mod << 16);

    return sci_key;
}


// Return the translated user friendly description.
QString QsciCommand::description() const
{
    return qApp->translate("QsciCommand", descCmd);
}
