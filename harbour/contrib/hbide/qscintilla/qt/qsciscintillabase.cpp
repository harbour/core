// This module implements the "official" low-level API.
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


#include "qsciscintillabase.h"

#include <qapplication.h>
#include <qclipboard.h>
#include <qcolor.h>
#include <qscrollbar.h>

#include <QContextMenuEvent>
#include <QDragEnterEvent>
#include <QDragMoveEvent>
#include <QDropEvent>
#include <QDragLeaveEvent>
#include <QFocusEvent>
#include <QKeyEvent>
#include <QList>
#include <QMimeData>
#include <QMouseEvent>
#include <QPaintEvent>

#include "ScintillaQt.h"


// The #defines in Scintilla.h and the enums in qsciscintillabase.h conflict
// (because we want to use the same names) so we have to undefine those we use
// in this file.
#undef  SCI_SETCARETPERIOD
#undef  SCK_DOWN
#undef  SCK_UP
#undef  SCK_LEFT
#undef  SCK_RIGHT
#undef  SCK_HOME
#undef  SCK_END
#undef  SCK_PRIOR
#undef  SCK_NEXT
#undef  SCK_DELETE
#undef  SCK_INSERT
#undef  SCK_ESCAPE
#undef  SCK_BACK
#undef  SCK_TAB
#undef  SCK_RETURN
#undef  SCK_ADD
#undef  SCK_SUBTRACT
#undef  SCK_DIVIDE
#undef  SCK_WIN
#undef  SCK_RWIN
#undef  SCK_MENU


// Remember if we have linked the lexers.
static bool lexersLinked = false;

// The list of instances.
static QList<QsciScintillaBase *> poolList;


// The ctor.
QsciScintillaBase::QsciScintillaBase(QWidget *parent)
    : QAbstractScrollArea(parent)
{
    connect(verticalScrollBar(), SIGNAL(valueChanged(int)),
            SLOT(handleVSb(int)));

    connect(horizontalScrollBar(), SIGNAL(valueChanged(int)),
            SLOT(handleHSb(int)));

    setAcceptDrops(true);
    setFocusPolicy(Qt::WheelFocus);
    setAttribute(Qt::WA_KeyCompression);

    viewport()->setBackgroundRole(QPalette::Base);
    viewport()->setMouseTracking(true);
    viewport()->setAttribute(Qt::WA_NoSystemBackground);

    triple_click.setSingleShot(true);

    sci = new ScintillaQt(this);

    SendScintilla(SCI_SETCARETPERIOD, QApplication::cursorFlashTime() / 2);

    // Make sure the lexers are linked in.
    if (!lexersLinked)
    {
        Scintilla_LinkLexers();
        lexersLinked = true;
    }

    QClipboard *cb = QApplication::clipboard();

    if (cb->supportsSelection())
        connect(cb, SIGNAL(selectionChanged()), SLOT(handleSelection()));

    // Add it to the pool.
    poolList.append(this);
}


// The dtor.
QsciScintillaBase::~QsciScintillaBase()
{
    // Remove it from the pool.
    poolList.removeAt(poolList.indexOf(this));

    delete sci;
}


// Return an instance from the pool.
QsciScintillaBase *QsciScintillaBase::pool()
{
    return poolList.first();
}


// Send a message to the real Scintilla widget using the low level Scintilla
// API.
long QsciScintillaBase::SendScintilla(unsigned int msg, unsigned long wParam,
        long lParam) const
{
    return sci->WndProc(msg, wParam, lParam);
}


// Overloaded message send.
long QsciScintillaBase::SendScintilla(unsigned int msg, unsigned long wParam,
        void *lParam) const
{
    return sci->WndProc(msg, wParam, reinterpret_cast<sptr_t>(lParam));
}


// Overloaded message send.
long QsciScintillaBase::SendScintilla(unsigned int msg, unsigned long wParam,
        const char *lParam) const
{
    return sci->WndProc(msg, wParam, reinterpret_cast<sptr_t>(lParam));
}


// Overloaded message send.
long QsciScintillaBase::SendScintilla(unsigned int msg,
        const char *lParam) const
{
    return sci->WndProc(msg, static_cast<uptr_t>(0),
            reinterpret_cast<sptr_t>(lParam));
}


// Overloaded message send.
long QsciScintillaBase::SendScintilla(unsigned int msg, const char *wParam,
        const char *lParam) const
{
    return sci->WndProc(msg, reinterpret_cast<uptr_t>(wParam),
            reinterpret_cast<sptr_t>(lParam));
}


// Overloaded message send.
long QsciScintillaBase::SendScintilla(unsigned int msg, long wParam) const
{
    return sci->WndProc(msg, static_cast<uptr_t>(wParam),
            static_cast<sptr_t>(0));
}


// Overloaded message send.
long QsciScintillaBase::SendScintilla(unsigned int msg, int wParam) const
{
    return sci->WndProc(msg, static_cast<uptr_t>(wParam),
            static_cast<sptr_t>(0));
}


// Overloaded message send.
long QsciScintillaBase::SendScintilla(unsigned int msg, long cpMin, long cpMax,
        char *lpstrText) const
{
    TextRange tr;

    tr.chrg.cpMin = cpMin;
    tr.chrg.cpMax = cpMax;
    tr.lpstrText = lpstrText;

    return sci->WndProc(msg, static_cast<uptr_t>(0),
            reinterpret_cast<sptr_t>(&tr));
}


// Overloaded message send.
long QsciScintillaBase::SendScintilla(unsigned int msg, unsigned long wParam,
        const QColor &col) const
{
    sptr_t lParam = (col.blue() << 16) | (col.green() << 8) | col.red();

    return sci->WndProc(msg, wParam, lParam);
}


// Overloaded message send.
long QsciScintillaBase::SendScintilla(unsigned int msg, const QColor &col) const
{
    uptr_t wParam = (col.blue() << 16) | (col.green() << 8) | col.red();

    return sci->WndProc(msg, wParam, static_cast<sptr_t>(0));
}


// Overloaded message send.
long QsciScintillaBase::SendScintilla(unsigned int msg, unsigned long wParam,
        QPainter *hdc, const QRect &rc, long cpMin, long cpMax) const
{
    RangeToFormat rf;

    rf.hdc = rf.hdcTarget = reinterpret_cast<SurfaceID>(hdc);

    rf.rc.left = rc.left();
    rf.rc.top = rc.top();
    rf.rc.right = rc.right() + 1;
    rf.rc.bottom = rc.bottom() + 1;

    rf.chrg.cpMin = cpMin;
    rf.chrg.cpMax = cpMax;

    return sci->WndProc(msg, wParam, reinterpret_cast<sptr_t>(&rf));
}


// Overloaded message send.
long QsciScintillaBase::SendScintilla(unsigned int msg, unsigned long wParam,
        const QPixmap &lParam) const
{
    return sci->WndProc(msg, wParam, reinterpret_cast<sptr_t>(&lParam));
}


// Send a message to the real Scintilla widget using the low level Scintilla
// API that returns a pointer result.
void *QsciScintillaBase::SendScintillaPtrResult(unsigned int msg) const
{
    return reinterpret_cast<void *>(sci->WndProc(msg, static_cast<uptr_t>(0),
            static_cast<sptr_t>(0)));
}


// Handle the timer on behalf of the ScintillaQt instance.
void QsciScintillaBase::handleTimer()
{
    sci->Tick();
}


// Re-implemented to handle the context menu.
void QsciScintillaBase::contextMenuEvent(QContextMenuEvent *e)
{
    sci->ContextMenu(Point(e->globalX(), e->globalY()));
}


// Re-implemented to tell the widget it has the focus.
void QsciScintillaBase::focusInEvent(QFocusEvent *)
{
    sci->SetFocusState(true);
}


// Re-implemented to tell the widget it has lost the focus.
void QsciScintillaBase::focusOutEvent(QFocusEvent *)
{
    // If an autocompletion list is being displayed (a Qt::Tool) and it is
    // clicked on, then we receive this event but the current focus event is 0.
    // We detect this and don't tell Scintilla as it would immediately destroy
    // the list.
    if (qApp->focusWidget())
        sci->SetFocusState(false);
}


// Re-implemented to make sure tabs are passed to the editor.
bool QsciScintillaBase::focusNextPrevChild(bool next)
{
    if (!sci->pdoc->IsReadOnly())
        return false;

    return QAbstractScrollArea::focusNextPrevChild(next);
}


// Handle the selection changing.
void QsciScintillaBase::handleSelection()
{
    if (!QApplication::clipboard()->ownsSelection())
        sci->UnclaimSelection();
}


// Handle key presses.
void QsciScintillaBase::keyPressEvent(QKeyEvent *e)
{
    unsigned key;
    QByteArray utf8;

    bool shift = e->modifiers() & Qt::ShiftModifier;
    bool ctrl = e->modifiers() & Qt::ControlModifier;
    bool alt = e->modifiers() & Qt::AltModifier;

    switch (e->key())
    {
    case Qt::Key_Down:
        key = SCK_DOWN;
        break;

    case Qt::Key_Up:
        key = SCK_UP;
        break;

    case Qt::Key_Left:
        key = SCK_LEFT;
        break;

    case Qt::Key_Right:
        key = SCK_RIGHT;
        break;

    case Qt::Key_Home:
        key = SCK_HOME;
        break;

    case Qt::Key_End:
        key = SCK_END;
        break;

    case Qt::Key_PageUp:
        key = SCK_PRIOR;
        break;

    case Qt::Key_PageDown:
        key = SCK_NEXT;
        break;

    case Qt::Key_Delete:
        key = SCK_DELETE;
        break;

    case Qt::Key_Insert:
        key = SCK_INSERT;
        break;

    case Qt::Key_Escape:
        key = SCK_ESCAPE;
        break;

    case Qt::Key_Backspace:
        key = SCK_BACK;
        break;

    case Qt::Key_Tab:
        key = SCK_TAB;
        break;

    case Qt::Key_Return:
    case Qt::Key_Enter:
        key = SCK_RETURN;
        break;

    case Qt::Key_Super_L:
        key = SCK_WIN;
        break;

    case Qt::Key_Super_R:
        key = SCK_RWIN;
        break;

    case Qt::Key_Menu:
        key = SCK_MENU;
        break;

    default:
        // See if the input was a single ASCII key.  If so it will be passed to
        // KeyDown to allow it to be filtered.  Correct the modifiers and key
        // for ASCII letters as Qt uses the ASCII code of uppercase letters for
        // Key_A etc.
        utf8 = e->text().toUtf8();

        if (utf8.length() == 0)
            key = e->key();
        else if (utf8.length() != 1)
            key = 0;
        else if ((key = utf8[0]) >= 0x80)
            key = 0;
        else if (key >= 0x01 && key <= 0x1a)
            key += 0x40;
        else if (key >= 'A' && key <= 'Z')
            shift = true;
        else if (key >= 'a' && key <= 'z')
        {
            key -= 0x20;
            shift = false;
        }
    }

    if (key)
    {
        bool consumed = false;

        sci->KeyDown(key, shift, ctrl, alt, &consumed);

        if (consumed)
        {
            e->accept();
            return;
        }
    }

    // Add the text if it has a compatible size depending on what Unicode mode
    // we are in.
    if (utf8.length() > 0 && (sci->IsUnicodeMode() || utf8.length() == 1))
    {
        sci->AddCharUTF(utf8.data(), utf8.length());
        e->accept();
    }
    else
        QAbstractScrollArea::keyPressEvent(e);
}


// Handle composed characters.  Note that this is the minumum needed to retain
// the QScintilla v1 functionality.
void QsciScintillaBase::inputMethodEvent(QInputMethodEvent *e)
{
    QByteArray utf8 = e->commitString().toUtf8();

    sci->AddCharUTF(utf8.data(), utf8.length());
    e->accept();
}


// Handle a mouse button double click.
void QsciScintillaBase::mouseDoubleClickEvent(QMouseEvent *e)
{
    if (e->button() != Qt::LeftButton)
    {
        e->ignore();
        return;
    }

    setFocus();

    // Make sure Scintilla will interpret this as a double-click.
    unsigned clickTime = sci->lastClickTime + Platform::DoubleClickTime() - 1;

    bool shift = e->modifiers() & Qt::ShiftModifier;
    bool ctrl = e->modifiers() & Qt::ControlModifier;
    bool alt = e->modifiers() & Qt::AltModifier;

    sci->ButtonDown(Point(e->x(), e->y()), clickTime, shift, ctrl, alt);

    // Remember the current position and time in case it turns into a triple
    // click.
    triple_click_at = e->globalPos();
    triple_click.start(QApplication::doubleClickInterval());
}


// Handle a mouse move.
void QsciScintillaBase::mouseMoveEvent(QMouseEvent *e)
{
    sci->ButtonMove(Point(e->x(), e->y()));
}


// Handle a mouse button press.
void QsciScintillaBase::mousePressEvent(QMouseEvent *e)
{
    setFocus();

    Point pt(e->x(), e->y());

    if (e->button() == Qt::LeftButton)
    {
        unsigned clickTime;

        // It is a triple click if the timer is running and the mouse hasn't
        // moved too much.
        if (triple_click.isActive() && (e->globalPos() - triple_click_at).manhattanLength() < QApplication::startDragDistance())
            clickTime = sci->lastClickTime + Platform::DoubleClickTime() - 1;
        else
            clickTime = sci->lastClickTime + Platform::DoubleClickTime() + 1;

        triple_click.stop();

        bool shift = e->modifiers() & Qt::ShiftModifier;
        bool ctrl = e->modifiers() & Qt::ControlModifier;
        bool alt = e->modifiers() & Qt::AltModifier;

        sci->ButtonDown(pt, clickTime, shift, ctrl, alt);
    }
    else if (e->button() == Qt::MidButton)
    {
        QClipboard *cb = QApplication::clipboard();

        if (cb->supportsSelection())
        {
            int pos = sci->PositionFromLocation(pt);

            sci->SetSelection(pos, pos);
            sci->pasteFromClipboard(QClipboard::Selection);
        }
    }
}


// Handle a mouse button releases.
void QsciScintillaBase::mouseReleaseEvent(QMouseEvent *e)
{
    if (sci->HaveMouseCapture() && e->button() == Qt::LeftButton)
    {
        bool ctrl = e->modifiers() & Qt::ControlModifier;

        sci->ButtonUp(Point(e->x(), e->y()), 0, ctrl);
    }
}


// Handle paint events.
void QsciScintillaBase::paintEvent(QPaintEvent *e)
{
    sci->paintEvent(e);
}


// Handle resize events.
void QsciScintillaBase::resizeEvent(QResizeEvent *)
{
    sci->ChangeSize();
}


// Re-implemented to suppress the default behaviour as Scintilla works at a
// more fundamental level.
void QsciScintillaBase::scrollContentsBy(int, int)
{
}


// Handle the vertical scrollbar.
void QsciScintillaBase::handleVSb(int value)
{
    sci->ScrollTo(value);
}


// Handle the horizontal scrollbar.
void QsciScintillaBase::handleHSb(int value)
{
    sci->HorizontalScrollTo(value);
}


// Handle drag enters.
void QsciScintillaBase::dragEnterEvent(QDragEnterEvent *e)
{
    QsciScintillaBase::dragMoveEvent(e);
}


// Handle drag leaves.
void QsciScintillaBase::dragLeaveEvent(QDragLeaveEvent *)
{
    sci->SetDragPosition(-1);
}


// Handle drag moves.
void QsciScintillaBase::dragMoveEvent(QDragMoveEvent *e)
{
    sci->SetDragPosition(sci->PositionFromLocation(Point(e->pos().x(),
                    e->pos().y())));

    acceptAction(e);
}


// Handle drops.
void QsciScintillaBase::dropEvent(QDropEvent *e)
{
    bool moving;
    const char *s;

    acceptAction(e);

    if (!e->isAccepted())
        return;

    moving = (e->dropAction() == Qt::MoveAction);

    QString qs = fromMimeData(e->mimeData());
    QByteArray ba;

    if (sci->IsUnicodeMode())
        ba = qs.toUtf8();
    else
        ba = qs.toLatin1();

    s = ba.data();

    sci->DropAt(sci->posDrop, s, moving, false);
    sci->Redraw();
}


void QsciScintillaBase::acceptAction(QDropEvent *e)
{
    if (sci->pdoc->IsReadOnly() || !canInsertFromMimeData(e->mimeData()))
    {
        e->ignore();
    }
    else if ((e->source() == this || e->source() == viewport()) && (e->keyboardModifiers() & Qt::ControlModifier) == 0)
    {
        e->setDropAction(Qt::MoveAction);
        e->accept();
    }
    else
    {
        e->acceptProposedAction();
    }
}



// See if a MIME data object can be decoded.
bool QsciScintillaBase::canInsertFromMimeData(const QMimeData *source) const
{
    return source->hasText() && !source->text().isEmpty();
}


// Create text from a MIME data object.
QString QsciScintillaBase::fromMimeData(const QMimeData *source) const
{
    return source->text();
}


// Create a MIME data object for some text.
QMimeData *QsciScintillaBase::toMimeData(const QString &text) const
{
    QMimeData *mime = new QMimeData;

    mime->setText(text);

    return mime;
}

