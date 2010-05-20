// This module implements the QsciStyle class.
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


#include "qscistyle.h"

#include <qapplication.h>

#include "qsciscintillabase.h"


// A ctor.
QsciStyle::QsciStyle(int style)
{
    init(style);

    QPalette pal = QApplication::palette();
    setColor(pal.text().color());
    setPaper(pal.base().color());

    setFont(QApplication::font());
    setEolFill(false);
}


// A ctor.
QsciStyle::QsciStyle(int style, const QString &description,
        const QColor &color, const QColor &paper, const QFont &font,
        bool eol_fill)
{
    init(style);

    setDescription(description);

    setColor(color);
    setPaper(paper);

    setFont(font);
    setEolFill(eol_fill);
}


// Initialisation common to all ctors.
void QsciStyle::init(int style)
{
    // The next style number to allocate.
    static int next_style_nr = QsciScintillaBase::STYLE_MAX;

    // See if a new style should be allocated.  Note that we allow styles to be
    // passed in that are bigger than STYLE_MAX because the styles used for
    // annotations are allowed to be.
    if (style < 0)
    {
        // Note that we don't deal with the situation where the newly allocated
        // style number has already been used explicitly.
        if (next_style_nr > QsciScintillaBase::STYLE_LASTPREDEFINED)
            style = next_style_nr--;
    }

    style_nr = style;

    // Initialise the minor attributes.
    setTextCase(QsciStyle::OriginalCase);
    setVisible(true);
    setChangeable(true);
    setHotspot(false);
}


// Set the color attribute.
void QsciStyle::setColor(const QColor &color)
{
    style_color = color;

    if (style_nr >= 0)
    {
        QsciScintillaBase *sci = QsciScintillaBase::pool();

        if (sci)
            sci->SendScintilla(QsciScintillaBase::SCI_STYLESETFORE, style_nr,
                    style_color);
    }
}


// Set the paper attribute.
void QsciStyle::setPaper(const QColor &paper)
{
    style_paper = paper;

    if (style_nr >= 0)
    {
        QsciScintillaBase *sci = QsciScintillaBase::pool();

        if (sci)
            sci->SendScintilla(QsciScintillaBase::SCI_STYLESETBACK, style_nr,
                    style_paper);
    }
}


// Set the font attribute.
void QsciStyle::setFont(const QFont &font)
{
    style_font = font;

    if (style_nr >= 0)
    {
        QsciScintillaBase *sci = QsciScintillaBase::pool();

        if (sci)
        {
            sci->SendScintilla(QsciScintillaBase::SCI_STYLESETFONT, style_nr,
                    style_font.family().toAscii().data());
            sci->SendScintilla(QsciScintillaBase::SCI_STYLESETSIZE, style_nr,
                    style_font.pointSize());
            sci->SendScintilla(QsciScintillaBase::SCI_STYLESETBOLD, style_nr,
                    style_font.bold());
            sci->SendScintilla(QsciScintillaBase::SCI_STYLESETITALIC, style_nr,
                    style_font.italic());
            sci->SendScintilla(QsciScintillaBase::SCI_STYLESETUNDERLINE,
                    style_nr, style_font.underline());
        }
    }
}


// Set the eol fill attribute.
void QsciStyle::setEolFill(bool eol_fill)
{
    style_eol_fill = eol_fill;

    if (style_nr >= 0)
    {
        QsciScintillaBase *sci = QsciScintillaBase::pool();

        if (sci)
            sci->SendScintilla(QsciScintillaBase::SCI_STYLESETEOLFILLED,
                    style_nr, style_eol_fill);
    }
}


// Set the text case attribute.
void QsciStyle::setTextCase(QsciStyle::TextCase text_case)
{
    style_case = text_case;

    if (style_nr >= 0)
    {
        QsciScintillaBase *sci = QsciScintillaBase::pool();

        if (sci)
            sci->SendScintilla(QsciScintillaBase::SCI_STYLESETCASE, style_nr,
                    (long)style_case);
    }
}


// Set the visible attribute.
void QsciStyle::setVisible(bool visible)
{
    style_visible = visible;

    if (style_nr >= 0)
    {
        QsciScintillaBase *sci = QsciScintillaBase::pool();

        if (sci)
            sci->SendScintilla(QsciScintillaBase::SCI_STYLESETVISIBLE,
                    style_nr, style_visible);
    }
}


// Set the changeable attribute.
void QsciStyle::setChangeable(bool changeable)
{
    style_changeable = changeable;

    if (style_nr >= 0)
    {
        QsciScintillaBase *sci = QsciScintillaBase::pool();

        if (sci)
            sci->SendScintilla(QsciScintillaBase::SCI_STYLESETCHANGEABLE,
                    style_nr, style_changeable);
    }
}


// Set the hotspot attribute.
void QsciStyle::setHotspot(bool hotspot)
{
    style_hotspot = hotspot;

    if (style_nr >= 0)
    {
        QsciScintillaBase *sci = QsciScintillaBase::pool();

        if (sci)
            sci->SendScintilla(QsciScintillaBase::SCI_STYLESETHOTSPOT,
                    style_nr, style_hotspot);
    }
}


// Refresh the style.
void QsciStyle::refresh()
{
    setColor(color());
    setPaper(paper());
    setFont(font());
    setEolFill(eolFill());
    setTextCase(textCase());
    setVisible(visible());
    setChangeable(changeable());
    setHotspot(hotspot());
}
