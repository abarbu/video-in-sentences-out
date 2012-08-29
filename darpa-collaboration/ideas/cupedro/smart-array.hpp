
#pragma once

#include <boost/shared_ptr.hpp>

// ------------------------------------------------------------------------------ Typedefs
template <typename T> class Array;
template <typename T> class SmartArray;

typedef SmartArray<char> SmartChar;
typedef SmartArray<float> SmartFloat;
typedef SmartArray<double> SmartDouble;

// ------------------------------------------------------------------------------ Array
/**
 * A container for an array of T and its length
 */
template <typename T> class Array
{
private:
  unsigned int _len;
  T* _data;

public:
  Array<T>() : _len(0), _data(NULL) { set_size(0); }
  Array<T>(const Array<T>& rhs) : _len(0), _data(NULL) { *this = rhs; }
  explicit Array<T>(unsigned int size) : _len(0), _data(NULL) { set_size(size); }
  virtual ~Array<T>() { release(); }

  unsigned int size() const { return _len; }

  T* ptr() { return _data; }
  const T* ptr() const { return _data; }

  void memset(unsigned char value) { if(_len > 0 && _data) ::memset((void*) _data, value, sizeof(T) * _len); }

  void release()
  {
    if(_data) {
	_len = 0;
	delete[] _data;
    }
  }

  Array<T>& operator=(const Array<T> &rhs)
  {
    if(this != &rhs) {
	set_size(rhs.size());
	memcpy(_data, rhs._data, _len * sizeof(T));
    }
    return *this;
  }

  Array<T>& set_all(const T& rhs)
  {
    T* cursor = ptr();
    T* end = cursor + _len;
    while(cursor < end) *cursor++ = rhs;
    return *this;
  }

  Array<T>& operator+=(const Array<T>& rhs)
  {
    Array<T> temp(*this);
    set_size(temp.size() + rhs.size()); // Note that set_size appends a null byte
    memcpy(_data, temp.ptr(), temp.size());
    memcpy(_data + temp.size(), rhs.ptr(), rhs.size() * sizeof(T));

    return *this;
  }

  Array<T> operator+(const Array<T>& rhs) const
  {
    Array<T> res(*this);
    res += rhs;
    return res;
  }

  void set_size(unsigned int size)
  {
    if(size != _len || size == 0) {
	release();
	_len = size;
	_data = new T[_len];
	if(_data == NULL && size > 0) {
	  fprintf(stderr, "Warning, failed to allocate memory.\n");
	}
    }
  }

  bool operator==(const Array<T>& rhs) const
  {
    if(this == &rhs) return true; // same object
    if(_len != rhs._len) return false; // the lengths must be equal for equality

    T* ptr_lhs = _data;
    T* ptr_rhs = rhs._data;
    if(ptr_lhs == ptr_rhs) return true; // same underlying block of memory
    for(unsigned int i = 0; i < _len; ++i) {
	if(!(ptr_lhs[i] == ptr_rhs[i])) return false;
    }
    return true;
  }

  bool operator!=(const Array<T>& rhs) const { return !this->operator==(rhs); }
};

// ------------------------------------------------------------------------------ SmartArray
/**
 * This is a smart-ptr wrapper around ByteContainer with implemented fall-through methods
 */
template <typename T> class SmartArray
{
private:
  boost::shared_ptr< Array<T> > _container;

  /** Copy on write */
  void cow()
  {
    if(!_container.unique()) {
	_container = boost::shared_ptr< Array<T> >(new Array<T>(*(_ptr())));
    }
  }

  /** Encapsulated access to the underlying pointer */
  const Array<T>* _ptr() const { return _container.get(); }
  Array<T>* _ptr() { return _container.get(); }

public:
  SmartArray<T>() : _container(new Array<T>()) {}
  SmartArray<T>(const SmartArray<T>& rhs) : _container(new Array<T>()) { *this = rhs; }
  explicit SmartArray<T>(unsigned int size) : _container(new Array<T>()) { set_size(size); }
  virtual ~SmartArray<T>() {}

  SmartArray<T>& operator=(const SmartArray<T>& rhs) { cow(); _ptr(); _container = rhs._container; return *this; }
  SmartArray<T>& set_all(const T& rhs) { cow(); _ptr()->set_all(rhs); return *this; }

  SmartArray<T>& operator+=(const SmartArray<T>& rhs) { cow(); _ptr()->operator+=(*(rhs._ptr())); return *this;  }
  SmartArray<T> operator+(const SmartArray<T>& rhs) const { SmartArray<T> res(*this); res += rhs; return res; }

  SmartArray<T> duplicate() const { SmartArray<T> res(*this); res.cow(); return res; }
  SmartArray<T> reverse() const { SmartArray<T> res(size()); T* dst = res.ptr(); const T* end = ptr(); const T* cursor = end + size() - 1; while(cursor >= end) *dst++ = *cursor--; return res; }
  void memset(unsigned char value) { cow(); _ptr()->memset(value); }

  unsigned int size() const { return _ptr()->size(); }
  void set_size(unsigned int size) { cow(); _ptr()->set_size(size); }

  T* ptr() { return _ptr()->ptr(); }
  const T* ptr() const { return _ptr()->ptr(); }

  bool operator==(const SmartArray<T>& rhs) const { return _ptr()->operator==(*(rhs._ptr())); }
  bool operator!=(const SmartArray<T>& rhs) const { return !this->operator==(rhs); }
};



