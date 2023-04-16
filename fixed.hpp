#include <algorithm>
#include <type_traits>

template < typename, size_t >
struct fixed;

template < typename >
struct norm;

namespace std
{
	template < typename T >
	struct wide_type { using type = void; };

	template < typename T >
	using wide_type_t = typename wide_type< T >::type;

	template < typename T, size_t _Offset >
	struct wide_type< ::fixed< T, _Offset > > { using type = ::fixed< wide_type_t< T >, _Offset >; };

	template < typename T >
	struct wide_type< ::norm< T > > { using type = ::norm< wide_type_t< T > >; };

	template <> struct wide_type< int8_t      > { using type = int16_t; };
	template <> struct wide_type< int16_t     > { using type = int32_t; };
	template <> struct wide_type< int32_t     > { using type = int64_t; };
	template <> struct wide_type< int64_t     > { using type = int64_t; };
	template <> struct wide_type< uint8_t     > { using type = uint16_t; };
	template <> struct wide_type< uint16_t    > { using type = uint32_t; };
	template <> struct wide_type< uint32_t    > { using type = uint64_t; };
	template <> struct wide_type< uint64_t    > { using type = uint64_t; };
	template <> struct wide_type< float_t     > { using type = double; };
	template <> struct wide_type< double      > { using type = long double; };
	template <> struct wide_type< long double > { using type = long double; };

	template < typename T >
	struct is_fixed_point { static constexpr bool value = std::is_integral_v< T >; };

	template < typename T >
	constexpr bool is_fixed_point_v = is_fixed_point< T >::value;

	template < typename T, size_t _Offset >
	struct is_fixed_point< ::fixed< T, _Offset > > { static constexpr bool value = true; };

	template < typename T >
	struct is_fixed_point< ::norm< T > > { static constexpr bool value = true; };

	template < typename T, size_t _Offset >
	constexpr bool is_integral_v< ::fixed< T, _Offset > > = true;

	template < typename T >
	constexpr bool is_integral_v< ::norm< T > > = true;
}

template < typename _Base, size_t _Offset = sizeof( _Base ) * 4u >
struct fixed
{
	using base_type = _Base;
	using wide_type = std::wide_type_t < base_type >;
	using float_type = std::conditional_t< ( sizeof( wide_type ) < 8 ), float, double >;

	static constexpr size_t offset = _Offset;
	static constexpr wide_type correction = static_cast< wide_type >( 1 ) << offset;
	static constexpr float_type epsilon = 1.0 / correction;
	static constexpr base_type max_base = std::numeric_limits< base_type >::max();
	static constexpr base_type min_base = std::numeric_limits< base_type >::min();
	static constexpr float_type max_value = static_cast< float_type >( max_base ) * epsilon;
	static constexpr float_type min_value = static_cast< float_type >( min_base ) * epsilon;

	base_type base = 0u;

	constexpr fixed() = default;

	template < typename T, typename = std::enable_if_t< !std::is_fixed_point_v< T > > >
	constexpr fixed( const T val )
	{
		if constexpr ( std::is_same_v< T, fixed > )
		{
			base = val.base;
		}
		else
		{
			base = val * correction + bias( val );
		}
	}

	template < typename T >
	constexpr fixed& operator=( const T val )
	{
		if constexpr ( std::is_same_v< T, fixed > )
		{
			base = val.base;
		}
		else
		{
			base = val * correction + bias( val );
		}

		return *this;
	}

	template < typename T >
	constexpr operator T() const
	{
		if constexpr ( std::is_same_v< T, fixed > )
		{
			return form( base );
		}
		else
		{
			return epsilon * base;
		}
	}

	template < typename T >
	inline constexpr fixed operator+( const T val ) const
	{
		if constexpr ( std::is_same_v< T, fixed > )
		{
			return form( base + val.base );
		}
		else
		{
			return form( base + fixed( val ).base );
		}
	}

	template < typename T >
	inline constexpr fixed& operator+=( const T val )
	{
		if constexpr ( std::is_same_v< T, fixed > )
		{
			base += val.base;
		}
		else
		{
			base += fixed( val ).base;
		}

		return *this;
	}

	inline constexpr fixed operator-() const
	{
		return form( -base );
	}

	template < typename T >
	inline constexpr fixed operator-( const T val ) const
	{
		if constexpr ( std::is_same_v< T, fixed > )
		{
			return form( base - val.base );
		}
		else
		{
			return form( base - fixed( val ).base );
		}
	}

	template < typename T >
	inline constexpr fixed& operator -=( const T val )
	{
		if constexpr ( std::is_same_v< T, fixed > )
		{
			base -= val.base;
		}
		else
		{
			base -= fixed( val ).base;
		}

		return *this;
	}

	template < typename T >
	inline constexpr fixed operator*( const T val ) const
	{
		if constexpr ( std::is_same_v< T, fixed > )
		{
			return form( ( static_cast< wide_type >( base ) * static_cast< wide_type >( val.base ) ) >> offset );
		}
		else
		{
			return form( ( static_cast< wide_type >( base ) * static_cast< wide_type >( fixed( val ).base ) ) >> offset );
		}
	}

	template < typename T >
	inline constexpr fixed& operator*=( const T val )
	{
		if constexpr ( std::is_same_v< T, fixed > )
		{
			base = ( static_cast< wide_type >( base ) * static_cast< wide_type >( val.base ) ) >> offset;
		}
		else
		{
			base = ( static_cast< wide_type >( base ) * static_cast< wide_type >( fixed( val ).base ) ) >> offset;
		}

		return *this;
	}

	template < typename T >
	inline constexpr fixed operator/( const T val ) const
	{
		if constexpr ( std::is_same_v< T, fixed > )
		{
			return form( ( static_cast< wide_type >( base ) << offset ) / static_cast< wide_type >( val.base ) );
		}
		else
		{
			return form( ( static_cast< wide_type >( base ) << offset ) / static_cast< wide_type >( form( val ).base ) );
		}
	}

	template < typename T >
	inline constexpr fixed& operator/=( const T val )
	{
		if constexpr ( std::is_same_v< T, fixed > )
		{
			base = ( static_cast< wide_type >( base ) << offset ) / static_cast< wide_type >( val.base );
		}
		else
		{
			base = ( static_cast< wide_type >( base ) << offset ) / static_cast< wide_type >( form( val ).base );
		}

		return *this;
	}

	template < typename T >
	inline constexpr fixed operator%( const T val ) const
	{
		if constexpr ( std::is_same_v< T, fixed > )
		{
			return form( base % val.base );
		}
		else
		{
			return form( base % fixed( val ).base );
		}
	}

	template < typename T >
	inline constexpr fixed& operator%=( const T val )
	{
		if constexpr ( std::is_same_v< T, fixed > )
		{
			base = base % val.base;
		}
		else
		{
			base = base % fixed( val ).base;
		}

		return *this;
	}

	template < typename T >
	inline constexpr bool operator==( const T val ) const
	{
		if constexpr ( std::is_same_v< T, fixed > )
		{
			return base == val.base;
		}
		else
		{
			return base == fixed( val ).base;
		}
	}

	template < typename T >
	inline constexpr bool operator!=( const T val ) const
	{
		if constexpr ( std::is_same_v< T, fixed > )
		{
			return base != val.base;
		}
		else
		{
			return base != fixed( val ).base;
		}
	}

	template < typename T >
	inline constexpr bool operator>( const T val ) const
	{
		if constexpr ( std::is_same_v< T, fixed > )
		{
			return base > val.base;
		}
		else
		{
			return base > fixed( val ).base;
		}
	}

	template < typename T >
	inline constexpr bool operator>=( const T val ) const
	{
		if constexpr ( std::is_same_v< T, fixed > )
		{
			return base >= val.base;
		}
		else
		{
			return base >= fixed( val ).base;
		}
	}

	template < typename T >
	inline constexpr bool operator<( const T val ) const
	{
		if constexpr ( std::is_same_v< T, fixed > )
		{
			return base < val.base;
		}
		else
		{
			return base < fixed( val ).base;
		}
	}

	template < typename T >
	inline constexpr bool operator<=( const T val ) const
	{
		if constexpr ( std::is_same_v< T, fixed > )
		{
			return base <= val.base;
		}
		else
		{
			return base <= fixed( val ).base;
		}
	}

private:

	inline constexpr static fixed form( base_type val )
	{
		fixed temp;
		temp.base = val;
		return temp;
	}

	template < typename T >
	inline constexpr static float_type bias( const T val )
	{
		return static_cast< float_type >( val ) >= static_cast< float_type >( 0.0 ) ? static_cast< float_type >( 0.5 ) : static_cast< float_type >( -0.5 );
	}
};

template < typename _Base >
struct norm
{
	using base_type = _Base;
	using wide_type = std::make_signed_t< std::wide_type_t < base_type > >;
	using float_type = std::conditional_t< ( sizeof( wide_type ) < 8u ), float, double >;

	static constexpr size_t offset = sizeof( base_type ) * 8u - std::is_signed_v< base_type >;
	static constexpr base_type max_base = std::numeric_limits< base_type >::max();
	static constexpr base_type min_base = std::numeric_limits< base_type >::min() + std::is_signed_v< base_type >;
	static constexpr wide_type correction = ( 1 << offset ) - 1;
	static constexpr float_type epsilon = 1.0 / correction;
	static constexpr float_type max_value = static_cast< float_type >( max_base ) * epsilon;
	static constexpr float_type min_value = static_cast< float_type >( min_base ) * epsilon;

	base_type base = 0u;

	constexpr norm() = default;

	template < typename T, typename = std::enable_if_t< !std::is_fixed_point_v< T > > >
	constexpr norm( const T val )
	{
		if constexpr ( std::is_same_v< T, norm > )
		{
			base = val.base;
		}
		else
		{
			base = clamp( val * correction + bias( val ) );
		}
	}

	template < typename T >
	constexpr norm& operator=( const T val )
	{
		if constexpr ( std::is_same_v< T, norm > )
		{
			base = val.base;
		}
		else
		{
			base = clamp( val * correction + bias( val ) );
		}

		return *this;
	}

	template < typename T >
	constexpr operator T() const
	{
		if constexpr ( std::is_same_v< T, norm > )
		{
			return form( base );
		}
		else
		{
			return static_cast< T >( epsilon * base );
		}
	}

	template < typename T >
	inline constexpr norm operator+( const T val ) const
	{
		if constexpr ( std::is_same_v< T, norm > )
		{
			return form( clamp( static_cast< wide_type >( base ) + static_cast< wide_type >( val.base ) ) );
		}
		else
		{
			return form( clamp( val * correction + bias( val ) + base ) );
		}
	}

	template < typename T >
	inline constexpr norm& operator+=( const T val )
	{
		if constexpr ( std::is_same_v< T, norm > )
		{
			base = clamp( static_cast< wide_type >( base ) + static_cast< wide_type >( val.base ) );
		}
		else
		{
			base = clamp( base + val * correction + bias( val ) );
		}

		return *this;
	}

	inline constexpr auto operator-() const
	{
		if constexpr ( std::is_signed_v< base_type > )
		{
			return form( -base );
		}
		else
		{
			return -static_cast< float_type >( *this );
		}
	}

	template < typename T >
	inline constexpr norm operator-( const T val ) const
	{
		if constexpr ( std::is_same_v< T, norm > )
		{
			return form( clamp( static_cast< wide_type >( base ) - static_cast< wide_type >( val.base ) ) );
		}
		else
		{
			return form( clamp( base - val * correction + bias( val ) ) );
		}
	}

	template < typename T >
	inline constexpr norm& operator-=( const T val )
	{
		if constexpr ( std::is_same_v< T, norm > )
		{
			base = clamp( static_cast< wide_type >( base ) - static_cast< wide_type >( val.base ) );
		}
		else
		{
			base = clamp( base - val * correction + bias( val ) );
		}

		return *this;
	}

	template < typename T >
	inline constexpr norm operator*( const T val ) const
	{
		if constexpr ( std::is_same_v< T, norm > )
		{
			return form( clamp( static_cast< wide_type >( base ) * static_cast< wide_type >( val.base ) >> offset ) );
		}
		else
		{
			return form( clamp( static_cast< wide_type >( base ) * static_cast< wide_type >( val * correction + bias( val ) ) >> offset ) );
		}
	}

	template < typename T >
	inline constexpr norm& operator*=( const T val )
	{
		if constexpr ( std::is_same_v< T, norm > )
		{
			base = clamp( static_cast< wide_type >( base ) * static_cast< wide_type >( val.base ) >> offset );
		}
		else
		{
			base = clamp( static_cast< wide_type >( base ) * static_cast< wide_type >( val * correction + bias( val ) ) >> offset );
		}

		return *this;
	}

	template < typename T >
	inline constexpr norm operator/( const T val ) const
	{
		if constexpr ( std::is_same_v< T, norm > )
		{
			return form( clamp( ( static_cast< wide_type >( base ) << offset ) / static_cast< wide_type >( val.base ) ) );
		}
		else
		{
			return form( clamp( ( static_cast< wide_type >( base ) << offset ) / static_cast< wide_type >( val * correction + bias( val ) ) ) );
		}
	}

	template < typename T >
	inline constexpr norm& operator/=( const T val )
	{
		if constexpr ( std::is_same_v< T, norm > )
		{
			base = clamp( ( static_cast< wide_type >( base ) << offset ) / static_cast< wide_type >( val.base ) );
		}
		else
		{
			base = clamp( ( static_cast< wide_type >( base ) << offset ) / static_cast< wide_type >( val * correction + bias( val ) ) );
		}

		return *this;
	}

	template < typename T >
	inline constexpr norm operator%( const T val ) const
	{
		if constexpr ( std::is_same_v< T, norm > )
		{
			return form( clamp( base % val.base ) );
		}
		else
		{
			return form( clamp( base % norm( val ).base ) );
		}
	}

	template < typename T >
	inline constexpr norm operator%=( const T val )
	{
		if constexpr ( std::is_same_v< T, norm > )
		{
			base = clamp( base % val.base );
		}
		else
		{
			base = clamp( base % norm( val ).base );
		}

		return *this;
	}

	template < typename T >
	inline constexpr bool operator==( const T val ) const
	{
		if constexpr ( std::is_same_v< T, norm > )
		{
			return base == val.base;
		}
		else
		{
			return base == norm( val ).base;
		}
	}

	template < typename T >
	inline constexpr bool operator!=( const T val ) const
	{
		if constexpr ( std::is_same_v< T, norm > )
		{
			return base != val.base;
		}
		else
		{
			return base != norm( val ).base;
		}
	}

	template < typename T >
	inline constexpr bool operator>( const T val ) const
	{
		if constexpr ( std::is_same_v< T, norm > )
		{
			return base > val.base;
		}
		else
		{
			return base > norm( val ).base;
		}
	}

	template < typename T >
	inline constexpr bool operator>=( const T val ) const
	{
		if constexpr ( std::is_same_v< T, norm > )
		{
			return base >= val.base;
		}
		else
		{
			return base >= norm( val ).base;
		}
	}

	template < typename T >
	inline constexpr bool operator<( const T val ) const
	{
		if constexpr ( std::is_same_v< T, norm > )
		{
			return base < val.base;
		}
		else
		{
			return base < norm( val ).base;
		}
	}

	template < typename T >
	inline constexpr bool operator<=( const T val ) const
	{
		if constexpr ( std::is_same_v< T, norm > )
		{
			return base <= val.base;
		}
		else
		{
			return base <= norm( val ).base;
		}
	}

private:

	inline constexpr static norm form( base_type val )
	{
		norm temp;
		temp.base = val;
		return temp;
	}

	inline constexpr static wide_type clamp( wide_type val )
	{
		return std::clamp( val, static_cast< wide_type >( min_base ), static_cast< wide_type >( max_base ) );
	}

	template < typename T >
	inline constexpr static float_type bias( const T val )
	{
		return static_cast< float_type >( val ) >= static_cast< float_type >( 0.0 ) ? static_cast< float_type >( 0.5 ) : static_cast< float_type >( -0.5 );
	}
};

using fixed8_t = fixed< int8_t >;
using fixed16_t = fixed< int16_t >;
using fixed32_t = fixed< int32_t >;
using fixed64_t = fixed< int64_t >;
using ufixed8_t = fixed< uint8_t >;
using ufixed16_t = fixed< uint16_t >;
using ufixed32_t = fixed< uint32_t >;
using ufixed64_t = fixed< uint64_t >;
using norm8_t = norm< int8_t >;
using norm16_t = norm< int16_t >;
using norm32_t = norm< int32_t >;
using norm64_t = norm< int64_t >;
using unorm8_t = norm< uint8_t >;
using unorm16_t = norm< uint16_t >;
using unorm32_t = norm< uint32_t >;
using unorm64_t = norm< uint64_t >;