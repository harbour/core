// ZipCollections.h: various platform-dependent definitions to try to make
//   sanity of cross platform datatyping.
// Part of the ZipArchive library
// 
// Copyright (C) 2000 - 2001 Tadeusz Dracz.
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// For the licensing details see the file License.txt
////////////////////////////////////////////////////////////////////////////////

#ifndef ZIPCOLLECTIONS_DOT_H
#define ZIPCOLLECTIONS_DOT_H

#if _MSC_VER > 1000
	#pragma warning( push, 3 ) // STL requrements
	#pragma warning (disable : 4284)
	#pragma warning (disable : 4018)
#endif

#include <vector>
#include <list>
#include <algorithm>
#include<functional>


struct CZipFindFast;
#include "zipstring.h"


template<class TYPE>
class CZipArray : public std::vector<TYPE>
{
public:
        typedef std::vector<TYPE>::iterator iterator;
protected:
	iterator GetIterFromIndex(int uIndex)
	{
		iterator iter = begin();
		int t = 0; while (t != uIndex) {iter++;t++;}
		return iter;
	}
public:
	int GetSize() const{return size();	}
	int GetUpperBound() const {return size() - 1;}
	void Add(const TYPE& x) {push_back(x);}
	void RemoveAll() {clear();}
	void RemoveAt(int uIndex) { erase(GetIterFromIndex(uIndex));}
	void InsertAt(int uIndex, const TYPE& x){insert(GetIterFromIndex(uIndex), x);}
};

typedef CZipArray<CZipString> CZipStringArray;
typedef CZipArray<CZipFindFast> CZipFindFastArray;

ZEXPORT class CZipWordArray : public CZipArray<WORD>
{
public:
	void Sort(bool bAscending);
};

template<class TYPE>
class CZipPtrList : public std::list<TYPE>
{

public:
        typedef std::list<TYPE>::iterator iterator;
        int GetCount() const {return size();}
	void AddTail(const TYPE& x){push_back(x);}
	void AddHead(const TYPE& x){push_front(x);}
	void RemoveHead() {pop_front();}
	void RemoveTail() {pop_back();}
	void RemoveAll() {clear();}
	TYPE& GetHead() {return front();}
	TYPE GetHead() const {return front();}
	TYPE& GetTail() {return back();}
	TYPE GetTail() const {return back();}
	iterator GetHeadPosition() { return begin();}
	iterator GetTailPosition() { return back();}
	TYPE& GetNext(iterator& pos) { return *pos++;}
	TYPE GetNext(iterator& pos) const{ return *pos++;}
	TYPE& GetPrev(iterator& pos) { return *pos--;}
	TYPE GetPrev(iterator& pos) const{ return *pos--;}
	iterator Find(TYPE& x) { return std::find(begin(), end(), x);}
	void RemoveAt(iterator& pos) { erase(pos);}
	bool IteratorValid(const iterator &iter)
	{
		return iter != end();
	}
	iterator FindIndex(int iIndex)
	{
		iterator iter = begin();
		int t = 0; while (t != iIndex) {iter++;t++;}
		return iter;
	}
	TYPE& GetAt(const iterator& pos) { return *pos;}
	TYPE GetAt(const iterator& pos) const{ return *pos;}

};


#if defined(_MSC_VER) && (_MSC_VER > 1100)
	#pragma warning( pop)
#endif

#endif  /* ZIPCOLLECTIONS_DOT_H */
