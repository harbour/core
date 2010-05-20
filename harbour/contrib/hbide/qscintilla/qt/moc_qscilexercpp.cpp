/****************************************************************************
** Meta object code from reading C++ file 'qscilexercpp.h'
**
** Created: Wed May 19 22:54:48 2010
**      by: The Qt Meta Object Compiler version 62 (Qt 4.6.2)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "qscilexercpp.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'qscilexercpp.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.6.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_QsciLexerCPP[] = {

 // content:
       4,       // revision
       0,       // classname
       0,    0, // classinfo
       5,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      19,   14,   13,   13, 0x0a,
      39,   14,   13,   13, 0x0a,
      61,   14,   13,   13, 0x0a,
      82,   14,   13,   13, 0x0a,
     114,  108,   13,   13, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_QsciLexerCPP[] = {
    "QsciLexerCPP\0\0fold\0setFoldAtElse(bool)\0"
    "setFoldComments(bool)\0setFoldCompact(bool)\0"
    "setFoldPreprocessor(bool)\0style\0"
    "setStylePreprocessor(bool)\0"
};

const QMetaObject QsciLexerCPP::staticMetaObject = {
    { &QsciLexer::staticMetaObject, qt_meta_stringdata_QsciLexerCPP,
      qt_meta_data_QsciLexerCPP, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &QsciLexerCPP::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *QsciLexerCPP::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *QsciLexerCPP::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_QsciLexerCPP))
        return static_cast<void*>(const_cast< QsciLexerCPP*>(this));
    return QsciLexer::qt_metacast(_clname);
}

int QsciLexerCPP::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QsciLexer::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: setFoldAtElse((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 1: setFoldComments((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 2: setFoldCompact((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 3: setFoldPreprocessor((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 4: setStylePreprocessor((*reinterpret_cast< bool(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 5;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
