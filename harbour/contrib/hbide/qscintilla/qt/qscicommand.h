// This defines the interface to the QsciCommand class.
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


#ifndef QSCICOMMAND_H
#define QSCICOMMAND_H

#ifdef __APPLE__
extern "C++" {
#endif

#include <qstring.h>

#include <qsciglobal.h>


class QsciScintilla;


//! \brief The QsciCommand class represents an internal editor command that may
//! have one or two keys bound to it.
//!
//! Methods are provided to change the keys bound to the command and to remove
//! a key binding.  Each command has a user friendly description of the command
//! for use in key mapping dialogs.
class QSCINTILLA_EXPORT QsciCommand
{
public:
    //! Binds the key \a key to the command.  If \a key is 0 then the key
    //! binding is removed.  If \a key is invalid then the key binding is
    //! unchanged.  Valid keys are any visible or control character or any
    //! of \c Key_Down, \c Key_Up, \c Key_Left, \c Key_Right, \c Key_Home,
    //! \c Key_End, \c Key_PageUp, \c Key_PageDown, \c Key_Delete,
    //! \c Key_Insert, \c Key_Escape, \c Key_Backspace, \c Key_Tab and
    //! \c Key_Return.  Keys may be modified with any combination of \c SHIFT,
    //! \c CTRL and \c ALT.
    //!
    //! \sa key(), setAlternateKey(), validKey()
    void setKey(int key);

    //! Binds the alternate key \a altkey to the command.  If \a key is 0
    //! then the alternate key binding is removed.
    //!
    //! \sa alternateKey(), setKey(), validKey()
    void setAlternateKey(int altkey);

    //! The key that is currently bound to the command is returned.
    //!
    //! \sa setKey(), alternateKey()
    int key() const {return qkey;}

    //! The alternate key that is currently bound to the command is
    //! returned.
    //!
    //! \sa setAlternateKey(), key()
    int alternateKey() const {return qaltkey;}

    //! If the key \a key is valid then true is returned.
    static bool validKey(int key);

    //! The user friendly description of the command is returned.
    QString description() const;

private:
    friend class QsciCommandSet;

    QsciCommand(QsciScintilla *qs, int msg, int key, int altkey,
            const char *desc);

    int msgId() const {return msgCmd;}
    void bindKey(int key,int &qk,int &scik);

    QsciScintilla *qsCmd;
    int msgCmd;
    int qkey, scikey, qaltkey, scialtkey;
    const char *descCmd;

    QsciCommand(const QsciCommand &);
    QsciCommand &operator=(const QsciCommand &);
};

#ifdef __APPLE__
}
#endif

#endif
