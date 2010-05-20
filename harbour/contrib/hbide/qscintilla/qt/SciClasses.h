// The definition of various Qt version independent classes used by the rest of
// the port.
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


#ifndef _SCICLASSES_H
#define _SCICLASSES_H

#include <qglobal.h>
#include <qwidget.h>


class QMouseEvent;
class QPaintEvent;
class ScintillaQt;


// A simple QWidget sub-class to implement a call tip.
class SciCallTip : public QWidget
{
    Q_OBJECT

public:
    SciCallTip(QWidget *parent, ScintillaQt *sci_);
    ~SciCallTip();

protected:
    void paintEvent(QPaintEvent *e);
    void mousePressEvent(QMouseEvent *e);

private:
    ScintillaQt *sci;
};


// A popup menu where options correspond to a numeric command.

#include <QMenu>
#include <QSignalMapper>

class SciPopup : public QMenu
{
    Q_OBJECT

public:
    SciPopup();

    void addItem(const QString &label, int cmd, bool enabled,
            ScintillaQt *sci_);

private slots:
    void on_triggered(int cmd);

private:
    ScintillaQt *sci;
    QSignalMapper mapper;
};



// This sub-class of QListBox is needed to provide slots from which we can call
// ListBox's double-click callback.  (And you thought this was a C++ program.)

class ListBoxQt;


#include <QListWidget>

class SciListBox : public QListWidget
{
    Q_OBJECT

public:
    SciListBox(QWidget *parent, ListBoxQt *lbx_);
    virtual ~SciListBox();

    void addItemPixmap(const QPixmap &pm, const QString &txt);

    int find(const QString &prefix);
    QString text(int n);

protected:
    void keyPressEvent(QKeyEvent *e);

private slots:
    void handleSelection();

private:
    ListBoxQt *lbx;
};


#endif
