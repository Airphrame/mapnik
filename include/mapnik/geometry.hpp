/*****************************************************************************
 *
 * This file is part of Mapnik (c++ mapping toolkit)
 *
 * Copyright (C) 2015 Artem Pavlenko
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 *****************************************************************************/

#ifndef MAPNIK_GEOMETRY_HPP
#define MAPNIK_GEOMETRY_HPP

#include <mapnik/util/variant.hpp>
#include <mapnik/vertex.hpp>
#include <mapnik/util/noncopyable.hpp>
#include <mapnik/geometry_types.hpp>

#include <algorithm>
#include <vector>
#include <tuple>
#include <type_traits>
#include <cstddef>
#include <utility>

namespace mapnik { namespace geometry {

struct point
{
    point() {}
    point(double x_, double y_)
        : x(x_), y(y_)
    {}

    point(point const& other) = default;
    point(point && other) noexcept = default;
    point & operator=(point const& other) = default;
    double x;
    double y;
};

struct point_ref
{
    point_ref() = delete;
    point_ref(double &x_, double &y_)
        : x(x_), y(y_)
    {}
    
    double & x;
    double & y;    
};

struct bounding_box
{
    bounding_box() {} // no-init
    bounding_box(double lox, double loy, double hix, double hiy)
        : p0(lox,loy),
          p1(hix,hiy) {}
    point p0;
    point p1;
};


struct line_string
{
    template <typename T1, typename T2>
    class ls_iterator : std::iterator<std::random_access_iterator_tag, T2>
    {
      public:
        ls_iterator(T1 iter_x_, T1 iter_y_)
            : iter_x(iter_x_), iter_y(iter_y_)
        {}
        T2 operator *() const { return T2(*iter_x, *iter_y); }
        const ls_iterator &operator ++() { ++iter_x; ++iter_y; return *this; }
        const ls_iterator &operator --() { --iter_x; --iter_y; return *this; }
        ls_iterator operator ++(int) 
        { 
            ls_iterator copy(*this); 
            ++iter_x;
            ++iter_y;
            return copy;
        }
        ls_iterator operator --(int) 
        { 
            ls_iterator copy(*this); 
            --iter_y;
            --iter_x;
            return copy; 
        }
        ls_iterator & operator =(const ls_iterator & other) 
        { 
            this->iter_x = other.iter_x; 
            this->iter_y = other.iter_y;
            return *this;
        }
        bool operator ==(const ls_iterator &other) const { return iter_x == other.iter_x && iter_y == other.iter_y; }
        bool operator !=(const ls_iterator &other) const { return iter_x != other.iter_x || iter_y != other.iter_y; }
        bool operator  <(const ls_iterator &other) const { return iter_x < other.iter_x && iter_y < other.iter_y; }
        bool operator  >(const ls_iterator &other) const { return iter_x > other.iter_x && iter_y > other.iter_y; }
        bool operator  <=(const ls_iterator &other) const { return iter_x <= other.iter_x && iter_y <= other.iter_y; }
        bool operator  >=(const ls_iterator &other) const { return iter_x >= other.iter_x && iter_y >= other.iter_y; }
        ls_iterator & operator +(const long int &add) const 
        {
            ls_iterator copy(*this);
            iter_x + add;
            iter_y + add;
            return copy;
        }
        ls_iterator & operator +=(const long int &add)
        {
            iter_x + add;
            iter_y + add;
            return *this;
        }
        ls_iterator & operator -(const long int &add) const 
        {
            ls_iterator copy(*this);
            iter_x - add;
            iter_y - add;
            return copy;
        }
        ls_iterator & operator -=(const long int &add) 
        {
            iter_x - add;
            iter_y - add;
            return *this;
        }
        T2 operator [] (const long int &n) const
        {
            return T2(iter_x[n],iter_y[n]);
        }

      private:
        T1 iter_x, iter_y;
         
    };
    typedef ls_iterator<std::vector<double>::iterator, point_ref> iterator;
    typedef ls_iterator<std::vector<double>::const_iterator, point> const_iterator;
    typedef ls_iterator<std::vector<double>::reverse_iterator, point_ref> reverse_iterator;
    typedef ls_iterator<std::vector<double>::const_reverse_iterator, point> const_reverse_iterator;
    
    line_string() = default;
    line_string (line_string && other) = default ;
    line_string& operator=(line_string &&) = default;
    line_string (line_string const& ) = default;
    line_string& operator=(line_string const&) = default;

    point_ref operator [] (const long int &n)
    {    
        return point_ref(x[n],y[n]);
    }
    
    point operator [] (const long int &n) const
    {    
        return point(x[n],y[n]);
    }

    inline void reserve(std::size_t new_cap)
    {
        x.reserve(new_cap);
        y.reserve(new_cap);
    }

    inline void resize(std::size_t new_size)
    {
        x.resize(new_size);
        y.resize(new_size);
    }

    inline bool empty() const { return x.empty(); }
    inline std::size_t size() const { return x.size(); }
    
    inline void push_back(point const& p) 
    {
        x.push_back(p.x);
        y.push_back(p.y);
    }
    
    inline void push_back(double x_, double y_)
    {
        x.push_back(x_);
        y.push_back(y_);
    }

    inline void emplace_back(point const& p) 
    {
        x.emplace_back(p.x);
        y.emplace_back(p.y);
    }

    inline void emplace_back(double x_, double y_) 
    { 
        x.emplace_back(x_);
        y.emplace_back(y_);
    }
    
    inline void add_coord(double x_, double y_) 
    { 
        x.emplace_back(x_);
        y.emplace_back(y_);
    }
    std::vector<double> x;
    std::vector<double> y;

    inline iterator begin()
    {
        return iterator(x.begin(), y.begin());
    }
    
    inline iterator end()
    {
        return iterator(x.end(), y.end());
    }

    inline const_iterator begin() const
    {
        return const_iterator(x.begin(), y.begin());
    }
    
    inline const_iterator end() const
    {
        return const_iterator(x.end(), y.end());
    }

    inline const_iterator cbegin() const
    {
        return const_iterator(x.cbegin(), y.cbegin());
    }
    
    inline const_iterator cend() const
    {
        return const_iterator(x.cend(), y.cend());
    }
    
    inline reverse_iterator rbegin()
    {
        return reverse_iterator(x.rbegin(), y.rbegin());
    }
    
    inline reverse_iterator rend()
    {
        return reverse_iterator(x.rend(), y.rend());
    }
    
    inline const_reverse_iterator rbegin() const
    {
        return const_reverse_iterator(x.rbegin(), y.rbegin());
    }
    
    inline const_reverse_iterator rend() const
    {
        return const_reverse_iterator(x.rend(), y.rend());
    }
    
    inline const_reverse_iterator crbegin() const
    {
        return const_reverse_iterator(x.crbegin(), y.crbegin());
    }
    
    inline const_reverse_iterator crend() const
    {
        return const_reverse_iterator(x.crend(), y.crend());
    }
};


struct linear_ring : line_string {};

struct polygon
{
    linear_ring exterior_ring;
    std::vector<linear_ring> interior_rings;

    inline void set_exterior_ring(linear_ring && ring)
    {
        exterior_ring = std::move(ring);
    }

    inline void add_hole(linear_ring && ring)
    {
        interior_rings.emplace_back(std::move(ring));
    }

    inline bool empty() const { return exterior_ring.empty(); }

    inline std::size_t num_rings() const
    {
        return 1 + interior_rings.size();
    }
};

struct multi_point : line_string {};
struct multi_line_string : std::vector<line_string> {};
struct multi_polygon : std::vector<polygon> {};
struct geometry_collection;
struct geometry_empty {};
using geometry = mapnik::util::variant<geometry_empty,
                                       point,
                                       line_string,
                                       polygon,
                                       multi_point,
                                       multi_line_string,
                                       multi_polygon,
                                       mapnik::util::recursive_wrapper<geometry_collection> >;

struct geometry_collection : std::vector<geometry> {};

}}

#endif //MAPNIK_GEOMETRY_HPP
