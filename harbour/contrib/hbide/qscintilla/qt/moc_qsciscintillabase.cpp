/****************************************************************************
** Meta object code from reading C++ file 'qsciscintillabase.h'
**
** Created: Wed May 19 22:55:29 2010
**      by: The Qt Meta Object Compiler version 62 (Qt 4.6.2)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "qsciscintillabase.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'qsciscintillabase.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.6.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_QsciScintillaBase[] = {

 // content:
       4,       // revision
       0,       // classname
       0,    0, // classinfo
      30,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
      26,       // signalCount

 // signals: signature, parameters, type, tag, flags
      23,   19,   18,   18, 0x05,
      45,   18,   18,   18, 0x05,
      66,   18,   18,   18, 0x05,
     108,   89,   18,   18, 0x05,
     144,   18,   18,   18, 0x05,
     168,  158,   18,   18, 0x05,
     200,  190,   18,   18, 0x05,
     243,  219,   18,   18, 0x05,
     275,  272,   18,   18, 0x05,
     301,  272,   18,   18, 0x05,
     348,  329,   18,   18, 0x05,
     374,  329,   18,   18, 0x05,
     406,  329,   18,   18, 0x05,
     434,  329,   18,   18, 0x05,
     464,  272,   18,   18, 0x05,
     524,  498,   18,   18, 0x05,
     563,  553,   18,   18, 0x05,
     625,   18,   18,   18, 0x05,
     649,  647,   18,   18, 0x05,
     672,   18,   18,   18, 0x05,
     686,   18,   18,   18, 0x05,
     706,   18,   18,   18, 0x05,
     738,  729,   18,   18, 0x05,
     759,   18,   18,   18, 0x05,
     774,  647,   18,   18, 0x05,
     813,   18,   18,   18, 0x05,

 // slots: signature, parameters, type, tag, flags
     824,   18,   18,   18, 0x08,
     844,  838,   18,   18, 0x08,
     859,  838,   18,   18, 0x08,
     874,   18,   18,   18, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_QsciScintillaBase[] = {
    "QsciScintillaBase\0\0yes\0QSCN_SELCHANGED(bool)\0"
    "SCN_AUTOCCANCELLED()\0SCN_AUTOCCHARDELETED()\0"
    "selection,position\0"
    "SCN_AUTOCSELECTION(const char*,int)\0"
    "SCEN_CHANGE()\0direction\0SCN_CALLTIPCLICK(int)\0"
    "charadded\0SCN_CHARADDED(int)\0"
    "position,line,modifiers\0"
    "SCN_DOUBLECLICK(int,int,int)\0,,\0"
    "SCN_DWELLEND(int,int,int)\0"
    "SCN_DWELLSTART(int,int,int)\0"
    "position,modifiers\0SCN_HOTSPOTCLICK(int,int)\0"
    "SCN_HOTSPOTDOUBLECLICK(int,int)\0"
    "SCN_INDICATORCLICK(int,int)\0"
    "SCN_INDICATORRELEASE(int,int)\0"
    "SCN_MACRORECORD(uint,ulong,void*)\0"
    "position,modifiers,margin\0"
    "SCN_MARGINCLICK(int,int,int)\0,,,,,,,,,\0"
    "SCN_MODIFIED(int,int,const char*,int,int,int,int,int,int,int)\0"
    "SCN_MODIFYATTEMPTRO()\0,\0SCN_NEEDSHOWN(int,int)\0"
    "SCN_PAINTED()\0SCN_SAVEPOINTLEFT()\0"
    "SCN_SAVEPOINTREACHED()\0position\0"
    "SCN_STYLENEEDED(int)\0SCN_UPDATEUI()\0"
    "SCN_USERLISTSELECTION(const char*,int)\0"
    "SCN_ZOOM()\0handleTimer()\0value\0"
    "handleVSb(int)\0handleHSb(int)\0"
    "handleSelection()\0"
};

const QMetaObject QsciScintillaBase::staticMetaObject = {
    { &QAbstractScrollArea::staticMetaObject, qt_meta_stringdata_QsciScintillaBase,
      qt_meta_data_QsciScintillaBase, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &QsciScintillaBase::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *QsciScintillaBase::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *QsciScintillaBase::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_QsciScintillaBase))
        return static_cast<void*>(const_cast< QsciScintillaBase*>(this));
    return QAbstractScrollArea::qt_metacast(_clname);
}

int QsciScintillaBase::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QAbstractScrollArea::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: QSCN_SELCHANGED((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 1: SCN_AUTOCCANCELLED(); break;
        case 2: SCN_AUTOCCHARDELETED(); break;
        case 3: SCN_AUTOCSELECTION((*reinterpret_cast< const char*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 4: SCEN_CHANGE(); break;
        case 5: SCN_CALLTIPCLICK((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 6: SCN_CHARADDED((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 7: SCN_DOUBLECLICK((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2])),(*reinterpret_cast< int(*)>(_a[3]))); break;
        case 8: SCN_DWELLEND((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2])),(*reinterpret_cast< int(*)>(_a[3]))); break;
        case 9: SCN_DWELLSTART((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2])),(*reinterpret_cast< int(*)>(_a[3]))); break;
        case 10: SCN_HOTSPOTCLICK((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 11: SCN_HOTSPOTDOUBLECLICK((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 12: SCN_INDICATORCLICK((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 13: SCN_INDICATORRELEASE((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 14: SCN_MACRORECORD((*reinterpret_cast< uint(*)>(_a[1])),(*reinterpret_cast< ulong(*)>(_a[2])),(*reinterpret_cast< void*(*)>(_a[3]))); break;
        case 15: SCN_MARGINCLICK((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2])),(*reinterpret_cast< int(*)>(_a[3]))); break;
        case 16: SCN_MODIFIED((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2])),(*reinterpret_cast< const char*(*)>(_a[3])),(*reinterpret_cast< int(*)>(_a[4])),(*reinterpret_cast< int(*)>(_a[5])),(*reinterpret_cast< int(*)>(_a[6])),(*reinterpret_cast< int(*)>(_a[7])),(*reinterpret_cast< int(*)>(_a[8])),(*reinterpret_cast< int(*)>(_a[9])),(*reinterpret_cast< int(*)>(_a[10]))); break;
        case 17: SCN_MODIFYATTEMPTRO(); break;
        case 18: SCN_NEEDSHOWN((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 19: SCN_PAINTED(); break;
        case 20: SCN_SAVEPOINTLEFT(); break;
        case 21: SCN_SAVEPOINTREACHED(); break;
        case 22: SCN_STYLENEEDED((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 23: SCN_UPDATEUI(); break;
        case 24: SCN_USERLISTSELECTION((*reinterpret_cast< const char*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 25: SCN_ZOOM(); break;
        case 26: handleTimer(); break;
        case 27: handleVSb((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 28: handleHSb((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 29: handleSelection(); break;
        default: ;
        }
        _id -= 30;
    }
    return _id;
}

// SIGNAL 0
void QsciScintillaBase::QSCN_SELCHANGED(bool _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void QsciScintillaBase::SCN_AUTOCCANCELLED()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void QsciScintillaBase::SCN_AUTOCCHARDELETED()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}

// SIGNAL 3
void QsciScintillaBase::SCN_AUTOCSELECTION(const char * _t1, int _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 3, _a);
}

// SIGNAL 4
void QsciScintillaBase::SCEN_CHANGE()
{
    QMetaObject::activate(this, &staticMetaObject, 4, 0);
}

// SIGNAL 5
void QsciScintillaBase::SCN_CALLTIPCLICK(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 5, _a);
}

// SIGNAL 6
void QsciScintillaBase::SCN_CHARADDED(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 6, _a);
}

// SIGNAL 7
void QsciScintillaBase::SCN_DOUBLECLICK(int _t1, int _t2, int _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 7, _a);
}

// SIGNAL 8
void QsciScintillaBase::SCN_DWELLEND(int _t1, int _t2, int _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 8, _a);
}

// SIGNAL 9
void QsciScintillaBase::SCN_DWELLSTART(int _t1, int _t2, int _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 9, _a);
}

// SIGNAL 10
void QsciScintillaBase::SCN_HOTSPOTCLICK(int _t1, int _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 10, _a);
}

// SIGNAL 11
void QsciScintillaBase::SCN_HOTSPOTDOUBLECLICK(int _t1, int _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 11, _a);
}

// SIGNAL 12
void QsciScintillaBase::SCN_INDICATORCLICK(int _t1, int _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 12, _a);
}

// SIGNAL 13
void QsciScintillaBase::SCN_INDICATORRELEASE(int _t1, int _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 13, _a);
}

// SIGNAL 14
void QsciScintillaBase::SCN_MACRORECORD(unsigned int _t1, unsigned long _t2, void * _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 14, _a);
}

// SIGNAL 15
void QsciScintillaBase::SCN_MARGINCLICK(int _t1, int _t2, int _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 15, _a);
}

// SIGNAL 16
void QsciScintillaBase::SCN_MODIFIED(int _t1, int _t2, const char * _t3, int _t4, int _t5, int _t6, int _t7, int _t8, int _t9, int _t10)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)), const_cast<void*>(reinterpret_cast<const void*>(&_t4)), const_cast<void*>(reinterpret_cast<const void*>(&_t5)), const_cast<void*>(reinterpret_cast<const void*>(&_t6)), const_cast<void*>(reinterpret_cast<const void*>(&_t7)), const_cast<void*>(reinterpret_cast<const void*>(&_t8)), const_cast<void*>(reinterpret_cast<const void*>(&_t9)), const_cast<void*>(reinterpret_cast<const void*>(&_t10)) };
    QMetaObject::activate(this, &staticMetaObject, 16, _a);
}

// SIGNAL 17
void QsciScintillaBase::SCN_MODIFYATTEMPTRO()
{
    QMetaObject::activate(this, &staticMetaObject, 17, 0);
}

// SIGNAL 18
void QsciScintillaBase::SCN_NEEDSHOWN(int _t1, int _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 18, _a);
}

// SIGNAL 19
void QsciScintillaBase::SCN_PAINTED()
{
    QMetaObject::activate(this, &staticMetaObject, 19, 0);
}

// SIGNAL 20
void QsciScintillaBase::SCN_SAVEPOINTLEFT()
{
    QMetaObject::activate(this, &staticMetaObject, 20, 0);
}

// SIGNAL 21
void QsciScintillaBase::SCN_SAVEPOINTREACHED()
{
    QMetaObject::activate(this, &staticMetaObject, 21, 0);
}

// SIGNAL 22
void QsciScintillaBase::SCN_STYLENEEDED(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 22, _a);
}

// SIGNAL 23
void QsciScintillaBase::SCN_UPDATEUI()
{
    QMetaObject::activate(this, &staticMetaObject, 23, 0);
}

// SIGNAL 24
void QsciScintillaBase::SCN_USERLISTSELECTION(const char * _t1, int _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 24, _a);
}

// SIGNAL 25
void QsciScintillaBase::SCN_ZOOM()
{
    QMetaObject::activate(this, &staticMetaObject, 25, 0);
}
QT_END_MOC_NAMESPACE
