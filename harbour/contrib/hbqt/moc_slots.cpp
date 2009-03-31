/****************************************************************************
** Meta object code from reading C++ file "slots.h"
**
** Created: Wed 18. Mar 17:25:39 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "hbqt.h"


#if QT_VERSION >= 0x040500

#include "hbqt_slots.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file [slots.h] doesn,t include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_Slots[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       7,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
       7,    6,    6,    6, 0x0a,
      17,    6,    6,    6, 0x0a,
      37,   29,    6,    6, 0x0a,
      53,    6,    6,    6, 0x0a,
      69,   63,    6,    6, 0x0a,
      87,    6,    6,    6, 0x0a,
      97,    6,    6,    6, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_Slots[] = {
    "Slots\0\0clicked()\0triggered()\0checked\0"
    "triggered(bool)\0hovered()\0state\0"
    "stateChanged(int)\0pressed()\0released()\0"
};

const QMetaObject Slots::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_Slots,
      qt_meta_data_Slots, 0 }
};

const QMetaObject *Slots::metaObject() const
{
    return &staticMetaObject;
}

void *Slots::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Slots))
        return static_cast<void*>(const_cast< Slots*>(this));
    return QObject::qt_metacast(_clname);
}

int Slots::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: clicked(); break;
        case 1: triggered(); break;
        case 2: triggered((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 3: hovered(); break;
        case 4: stateChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 5: pressed(); break;
        case 6: released(); break;
        default: ;
        }
        _id -= 7;
    }
    return _id;
}
QT_END_MOC_NAMESPACE


#endif

