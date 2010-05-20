/****************************************************************************
** Meta object code from reading C++ file 'SciClasses.h'
**
** Created: Wed May 19 22:56:02 2010
**      by: The Qt Meta Object Compiler version 62 (Qt 4.6.2)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "SciClasses.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'SciClasses.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.6.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_SciCallTip[] = {

 // content:
       4,       // revision
       0,       // classname
       0,    0, // classinfo
       0,    0, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

       0        // eod
};

static const char qt_meta_stringdata_SciCallTip[] = {
    "SciCallTip\0"
};

const QMetaObject SciCallTip::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_SciCallTip,
      qt_meta_data_SciCallTip, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &SciCallTip::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *SciCallTip::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *SciCallTip::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_SciCallTip))
        return static_cast<void*>(const_cast< SciCallTip*>(this));
    return QWidget::qt_metacast(_clname);
}

int SciCallTip::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    return _id;
}
static const uint qt_meta_data_SciPopup[] = {

 // content:
       4,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      14,   10,    9,    9, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_SciPopup[] = {
    "SciPopup\0\0cmd\0on_triggered(int)\0"
};

const QMetaObject SciPopup::staticMetaObject = {
    { &QMenu::staticMetaObject, qt_meta_stringdata_SciPopup,
      qt_meta_data_SciPopup, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &SciPopup::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *SciPopup::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *SciPopup::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_SciPopup))
        return static_cast<void*>(const_cast< SciPopup*>(this));
    return QMenu::qt_metacast(_clname);
}

int SciPopup::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QMenu::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: on_triggered((*reinterpret_cast< int(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}
static const uint qt_meta_data_SciListBox[] = {

 // content:
       4,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      12,   11,   11,   11, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_SciListBox[] = {
    "SciListBox\0\0handleSelection()\0"
};

const QMetaObject SciListBox::staticMetaObject = {
    { &QListWidget::staticMetaObject, qt_meta_stringdata_SciListBox,
      qt_meta_data_SciListBox, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &SciListBox::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *SciListBox::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *SciListBox::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_SciListBox))
        return static_cast<void*>(const_cast< SciListBox*>(this));
    return QListWidget::qt_metacast(_clname);
}

int SciListBox::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QListWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: handleSelection(); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
