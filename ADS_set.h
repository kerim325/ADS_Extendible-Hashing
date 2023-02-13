#ifndef ADS_SET_H
#define ADS_SET_H

#include <functional>
#include <algorithm>
#include <ostream>
#include <stdexcept>

template <typename Key, size_t N =11>     // N...Bucketgröße
class ADS_set {
  struct Bucket;
  struct Element;
  public:
  class Iterator;
  using value_type = Key;
  using key_type = Key;
  using reference = value_type&;
  using const_reference = const value_type&;
  using size_type = size_t;
  using difference_type = std::ptrdiff_t;
  using const_iterator = Iterator;
  using iterator = const_iterator;
  using key_equal = std::equal_to<key_type>;
  using hasher = std::hash<key_type>;
  using pointer = const value_type *;
  using pointer_b = ADS_set::Bucket*;

  private:
  size_type gt;                 // globale Tiefe
  Bucket** tabelle;             // Indextabelle
  size_type sz;                 // size
  
  inline size_type h(const key_type& key, const size_type& t) const
  { 
    return hasher{}(key) & ((1<<t)-1);
  }

  inline size_type add(const key_type& key)
  {
    size_type index { h(key, gt) };
    size_type free {0};
    bool found {false};
    for(size_type i{0}, el{0}; i<N; ++i)
    {
      if(tabelle[index]->bptr[i].belegt && ++el && key_equal{}(tabelle[index]->bptr[i].wert, key))
        return i;
      if(!found)
      {  
        if(!tabelle[index]->bptr[i].belegt)
        {
          free = i;
          found = true;
        }
      }
      else if(el >= tabelle[index]->bsz) 
        break;
    }
    if(found)
    {
      tabelle[index]->bptr[free].wert = key;
      tabelle[index]->bptr[free].belegt = true;
      sz+=1;
      tabelle[index]->bsz+=1;
      return free;
    }
    split(key, index);
    return add(key);
  }

  inline void split(const key_type& key, size_type& index)
  {
    
    if(tabelle[index]->lt >= gt) 
    {
      expand();
      index = h(key, gt);
    }
    
    ++tabelle[index]->lt;
    Bucket *ptr {tabelle[index]};    
    Bucket *b = new Bucket;           
    b->lt = tabelle[index]->lt;
    
    for(size_type i{0}, isize{size_type(1)<<(gt-tabelle[index]->lt)}, partner{index&((1<<tabelle[index]->lt)-1)}; i<isize; ++i)
      tabelle[(i<<tabelle[index]->lt) | partner] = b;

    index = index&((1<<ptr->lt)-1);
    for(size_type i {0}; i<N; ++i)
      if(h(ptr->bptr[i].wert, ptr->lt) == index)
      {
        ptr->bptr[i].belegt = false;
        ptr->bsz-=1;
        b->bptr[i].wert = ptr->bptr[i].wert;
        b->bptr[i].belegt = true;
        b->bsz+=1;
      }
  }

  inline void expand()
  {
    ++gt;
    Bucket** tab = new Bucket*[1<<gt];
    size_type max {size_type(1)<<gt};
    size_type half {max>>1};

    for(size_type i {0}; i<half; ++i)
      tab[i] = tabelle[i];

    for(size_type i {half}; i<(max); ++i) 
      tab[i] = tabelle[i-half];
      
    delete[] tabelle;
    tabelle = tab;
  }


  public:
  ADS_set() : gt{0}, tabelle{new Bucket*[1]}, sz{0}
  {
      tabelle[0] = new Bucket;
  }

  ADS_set(std::initializer_list<key_type> ilist) : ADS_set()
  {
      insert(ilist);
  }

  template<typename InputIt> ADS_set(InputIt first, InputIt last) : ADS_set()
  {
      insert(first, last);
  }

  ADS_set(const ADS_set& other)
  {
    gt = other.gt;
    sz = other.sz;

    size_type isize {size_type(1)<<gt};
    tabelle = new Bucket*[isize];
    for (size_type i{0}; i<isize; ++i)
    {
      size_type partner {i&((1<<other.tabelle[i]->lt)-1)};
      if(partner == i)
      {
        Bucket* b = new Bucket;           
        b->lt = other.tabelle[i]->lt;
        b->bsz = other.tabelle[i]->bsz;

        for(size_type j {0}, iisize {size_type(1)<<(gt-b->lt)}; j<iisize; ++j)
          tabelle[(j<<b->lt) | partner] = b;

        for(size_type j{0}, b{0}; j<N; ++j)
        {
          if(other.tabelle[i]->bptr[j].belegt && ++b)
          {
            tabelle[i]->bptr[j].belegt = true;
            tabelle[i]->bptr[j].wert = other.tabelle[i]->bptr[j].wert;
            if(b >= tabelle[i]->bsz) break;
          }
        }
      }
    }
  }

  ~ADS_set()
  { 
    for (size_type i{0}, isize{size_type(1)<<gt}; i<isize; ++i)
    {
      size_type partner {i&((1<<tabelle[i]->lt)-1)};
      if(i == partner)  //erster Index, was auf Bucket zeigt
      {
        delete[] tabelle[i]->bptr;
        if(1<<(gt-tabelle[i]->lt) <= 1)
          delete tabelle[i];
        continue;
      }
      if(i == (((((1<<gt)-1)>>tabelle[i]->lt)<<tabelle[i]->lt) | partner))  //letzter Index, was auf Bucket zeigt
        delete tabelle[i];  //bucket deleted
    }
    delete[] tabelle;
  }

  ADS_set& operator=(const ADS_set& other)
  {
    if(&other == this) 
      return *this;
    ADS_set t(other);
    swap(t);
    return *this;
  }

  ADS_set& operator=(std::initializer_list<key_type> ilist)
  {
    ADS_set t(ilist);
    swap(t);
    return *this;
  }

  size_type size() const
  {
    return sz;
  }

  bool empty() const
  {
    return sz==0;
  }

  void insert(std::initializer_list<key_type> ilist)
  {
    for (const auto& temp : ilist)
      add(temp);
  }

  std::pair<iterator, bool> insert(const key_type& key)
  {
    size_type old {sz};
    size_type added {add(key)};
    size_type index {h(key,gt)};
    return std::pair<iterator, bool>(iterator (this, &tabelle[index]->bptr[added].wert, (index & ((1<<tabelle[index]->lt)-1)), added), old!=sz);
  }

  template<typename InputIt> void insert(InputIt first, InputIt last)
  {
    for(auto it {first}; it != last; ++it)
      add(*it);
  }

  void clear()
  {
    ADS_set t;
    swap(t);
  }

  size_type erase(const key_type& key)
  {
    size_type index {h(key, gt)};
    for (size_type i {0}, el {0}; i < N; i++)
    {
      if(tabelle[index]->bptr[i].belegt && ++el && key_equal{}(tabelle[index]->bptr[i].wert, key))
      {
        --tabelle[index]->bsz;
        tabelle[index]->bptr[i].belegt = false;
        --sz;
        return true;
      }
      if(el >= tabelle[index]->bsz) break;
    }
    return false;
  }

  size_type count(const key_type& key) const
  {
    size_type index {h(key, gt)};
    for(size_type i{0}, el{0}; i<N; ++i)
    {
      if(tabelle[index]->bptr[i].belegt && ++el && key_equal{}(tabelle[index]->bptr[i].wert,key))
        return true;
      if(el >= tabelle[index]->bsz) break;
    }
    return false;
  }

  iterator find(const key_type& key) const
  {
    size_type index {h(key, gt)}; 
    for (size_type i{0}, el{0}; i < N; ++i)
    {
      if(tabelle[index]->bptr[i].belegt && ++el && key_equal{}(tabelle[index]->bptr[i].wert, key))
        return iterator (this, &tabelle[index]->bptr[i].wert, (index & ((1<<tabelle[index]->lt)-1)) , i);
      if(el >= tabelle[index]->bsz) break;
    }
    
    return iterator(this, nullptr, 0, 0);
  }

  void swap(ADS_set& other)
  {
    std::swap(gt, other.gt);
    std::swap(tabelle, other.tabelle);
    std::swap(sz, other.sz);
  }

  const_iterator begin() const
  {
    for (size_type i {0}; i < size_type(1)<<gt; ++i)
    {
      if((i&((1<<tabelle[i]->lt)-1)) < i || tabelle[i]->bsz <= 0) 
        continue;
      for (size_type j {0}; j < N; ++j)
        if(tabelle[i]->bptr[j].belegt)
          return const_iterator(this, &tabelle[i]->bptr[j].wert, i, j);
    }
        
    return const_iterator(this, nullptr, 0, 0);
  }

  const_iterator end() const
  {
    return const_iterator(this, nullptr, 0, 0);
  }

  std::ostream& dump(std::ostream& o = std::cout) const
  {
    o << "\n-------------------------------------\nglobale Tiefe gt = " << gt << "\n";
    o << "Bucketgroesse N = " << N << "\n";
    o << "Size sz = " << sz << "\n\n";

    for(size_type i {0}; i<(size_type(1)<<gt); ++i)
    {
      o << i << " --> " /* << tabelle[i]->id  */<<" [";
      for(size_t j {0}; j<N; ++j)
      {
        if(tabelle[i]->bptr[j].belegt) 
          o << tabelle[i]->bptr[j].wert;
        else 
          o << " ~ ";
        o << "__";
      }
      o << "] lt = " << tabelle[i]->lt << "; b_sz = " << tabelle[i]->bsz << "\n";
    }
    return o;
  }

  friend bool operator==(const ADS_set& lhs, const ADS_set& rhs)
  {
    if(lhs.sz != rhs.sz) return false;
    for (const auto& temp : lhs)
      if(!rhs.count(temp)) return false;
    return true;
  }

  friend bool operator!=(const ADS_set& lhs, const ADS_set& rhs)
  {
    return !(lhs==rhs);
  }

  //friend std::ostream& operator<<(std::ostream& o, const ADS_set& h)
  //{
  //  return h.dump(o);
  //}

  private:
  struct Bucket
  {
    Element *bptr {new Element[N]};
    size_type lt {0};
    size_type bsz {0};
  };

  struct Element
  {
    key_type wert;
    bool belegt {false};
  };
};



template <typename Key, size_t N>
class ADS_set<Key,N>::Iterator
{
  public:
  using value_type = ADS_set::value_type;
  using difference_type = ADS_set::difference_type;
  using reference = ADS_set::const_reference;
  using pointer = ADS_set::pointer;
  using iterator_category = std::forward_iterator_tag;
  using iterator = ADS_set::Iterator;
  using pointer_set = const ADS_set*;
  using key_equal = ADS_set::key_equal;
  using pointer_b = ADS_set::pointer_b;

  private:
  pointer_set eh;
  pointer ptr;
  size_type ix;
  size_type n;
  size_type count {1};

  void skip()
  {
    if(ptr == nullptr) return;
    bool first {false};
    for (size_type i {ix}, isize{size_type(1)<<eh->gt}; i<isize; ++i)
    {
      if( ! ( (i&((1<<eh->tabelle[i]->lt)-1))<i || eh->tabelle[i]->bsz<=0 || count>=eh->tabelle[i]->bsz )) 
        for (size_type j {n}; j<N; ++j)
        {
          if(first && eh->tabelle[i]->bptr[j].belegt)
          {
            count+=1;
            ptr = &eh->tabelle[i]->bptr[j].wert;
            ix = i;
            n = j;
            return;
          }
          else if(!first) 
            first = true;
        }
      count=0;
      n=0;
      first=true;
    }
    ptr = nullptr;
  }

  public:
  Iterator() : eh{nullptr}, ptr{nullptr}, ix{0}, n{0} { };
  explicit Iterator(pointer_set eh, pointer ptr, size_type ix, size_type n) : eh{eh}, ptr{ptr}, ix{ix}, n{n} { };
  reference operator*() const 
  { 
    return *ptr; 
  }
  pointer operator->() const 
  { 
    return ptr; 
  }
  iterator& operator++()
  {
    skip();
    return *this;
  }
  iterator operator++(int)
  {
    iterator ret (eh, ptr, ix, n);
    skip();
    return ret;
  }
  friend bool operator==(const iterator &lhs, const iterator &rhs)
  {
    return (lhs.ptr==rhs.ptr);
  }
  friend bool operator!=(const iterator &lhs, const iterator &rhs)
  {
    return !(lhs.ptr==rhs.ptr);
  }
};


template <typename Key, size_t N>
void swap(ADS_set<Key,N> &lhs, ADS_set<Key,N> &rhs)
{
    lhs.swap(rhs);
}


#endif