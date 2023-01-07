#include <algorithm>

#define ty typename

/// Compile-time string.
template <size_t n>
struct string {
    /// String data and size.
    char data[n];
    size_t size = 0;

    /// Capacity is fixed, obviously.
    static constexpr size_t capacity = n;

    /// Construct from a string literal.
    consteval string(const char (&s)[n]) {
        std::copy_n(s, n, data);
        size = n - 1;
    }

    /// Append a character.
    consteval auto& operator+=(char c) {
        data[size++] = c;
        return *this;
    }

    /// Append a string.
    consteval auto& operator+=(const string& s) {
        for (size_t i = 0; i < s.size; ++i) data[size++] = s.data[i];
        return *this;
    }

    /// Check if the strings are equal.
    template <size_t sz>
    consteval bool operator==(const string<sz>& s) const {
        if (size != s.size) return false;
        for (size_t i = 0; i < size; ++i) if (data[i] != s.data[i]) return false;
        return true;
    }
};


struct error_type {};
using nil = void;
using t = bool;

template <ty T> struct quote { using res = T; };
template <int64_t> struct n {};
template <ty T, ty U> struct cons_ {};
template <ty T, ty env> struct eval_ {};
template <ty T> struct car_ {};
template <ty T> struct cdr_ {};
template <ty T, ty U> struct equal_ {};
template <ty T> struct atom_ {};
template <ty ...T> struct s{};
template <ty ...T> struct cond {};
template <ty ...T> struct lambda {};
template <ty ...T> struct defun {};
template <ty ...T> struct let {};
template <string s> struct v { static constexpr string value = s; };

template <string s, ty T> struct defvar {
    static constexpr string symbol = s;
    using value = T;
    using variable = t;
};

template <string s, ty...> struct find_in_env;
template <string s> struct find_in_env<s> { using res = error_type; };
template <string a, ty entry, ty ...U> struct find_in_env<a, entry, U...> {
    using res = std::conditional_t<requires { ty entry::variable; } and a == entry::symbol, ty entry::value, ty find_in_env<a, U...>::res>;
};

template <ty ...vars> struct environment {
    template <string s>
    using lookup = find_in_env<s, vars...>::res;

    template <ty ...vars2>
    using extend = environment<vars2..., vars...>;
};

template <ty T, ty env = nil> using car = car_<ty eval_<T, env>::res>;
template <ty T, ty env = nil> using cdr = cdr_<ty eval_<T, env>::res>;
template <ty T, ty U, ty env = nil> using cons = cons_<ty eval_<T, env>::res, ty eval_<U, env>::res>;
template <ty T, ty U, ty env = nil> using equal = equal_<ty eval_<T, env>::res, ty eval_<U, env>::res>;
template <ty T, ty env = nil> using atom = atom_<ty eval_<T, env>::res>;

/// QUOTE
template <ty T, ty env> struct eval_<quote<T>, env> { using res = T; };

/// NIL/T
template <ty env> struct eval_<nil, env> { using res = nil; };
template <ty env> struct eval_<t, env> { using res = t; };

/// NUMBERS
template <int64_t v, ty env> struct eval_<n<v>, env> { using res = n<v>; };

/// PAIRS
template <ty T, ty U, ty env> struct eval_<cons_<T, U>, env> { using res = cons_<T, U>; };
template <ty T, ty U, ty env> struct eval_<car_<cons_<T, U>>, env> { using res = T; };
template <ty T, ty U, ty env> struct eval_<cdr_<cons_<T, U>>, env> { using res = U; };

/// EQUAL
template <ty T, ty U, ty env> struct eval_<equal_<T, U>, env> { using res = std::conditional_t<std::is_same_v<T, U>, t, nil>; };

/// ATOM
template <ty T, ty env> struct eval_<atom_<T>, env> { using res = std::conditional_t<requires { typename T::res; }, nil, t>; };

/// COND
template <ty a, ty b, ty env> struct eval_<cond<s<a, b>>, env> {
    using res = std::conditional_t<std::is_same_v<ty eval_<a, env>::res, t>, ty eval_<b, env>::res, nil>;
};
template <ty a, ty b, ty ...Ts, ty env> struct eval_<cond<s<a, b>, Ts...>, env> {
    using res = std::conditional_t<std::is_same_v<ty eval_<a, env>::res, t>, ty eval_<b, env>::res, ty eval_<cond<Ts...>, env>::res>;
};

/// VARIABLES
template <string s, ty env> struct eval_<v<s>, env> {
    using res = ty std::conditional_t<std::is_void_v<env>, environment<>, env>::template lookup<s>;
    static_assert(not std::is_same_v<res, error_type>, "Variable not found");
};

/// LAMBDAS
template <ty ...names, ty expr, ty ...params, ty env> struct eval_<s<lambda<s<names...>, expr>, params...>, env> {
    using res = eval_<expr, ty std::conditional_t<std::is_void_v<env>, environment<>, env>::
                                template extend<defvar<names::value, params>...>>::res;
};

/// LET
template <ty ...names, ty expr, ty env> struct eval_<let<s<names...>, expr>, env> {
    using res = eval_<expr, ty std::conditional_t<std::is_void_v<env>, environment<>, env>::
                                template extend<names...>>::res;
};

/// FUNCTION CALLS
template <ty func, ty ...params, ty env> struct eval_<s<func, params...>, env> {
    using res = eval_<s<ty eval_<func, env>::res, ty eval_<params, env>::res...>, env>::res;
};


template <ty T, ty env = nil> using eval = ty eval_<T, env>::res;

using a = eval<cond<s<nil, t>, s<t, n<5>>>>;
using b = eval<atom<eval<quote<quote<t>>>>>;
using c = eval<v<"a">, environment<defvar<"a", int>>>;
using d = eval<s<lambda<s<v<"a">>, v<"a">>, n<5>>>;
using e = eval<let<s<defvar<"a", n<5>>>, v<"a">>>;
using if_ = lambda<s<v<"cond">,
                     v<"true">,
                     v<"false">>,
                   cond<s<v<"cond">, v<"true">>,
                        s<t, v<"false">>>>;
using if_test = eval<s<if_, t, n<5>, n<6>>>;
static_assert(std::is_same_v<if_test, n<5>>);

using f = eval<let<s<defvar<"if",
                            lambda<s<v<"cond">,
                                     v<"true">,
                                     v<"false">>,
                                   cond<s<v<"cond">, v<"true">>,
                                        s<t, v<"false">>>>>,
                     s<v<"if">, t,
                                n<1>,
                                n<2>>>>>;

static_assert(std::is_same_v<a, n<5>>);
static_assert(std::is_same_v<b, t>);
static_assert(std::is_same_v<d, n<5>>);
static_assert(std::is_same_v<c, int>);
static_assert(std::is_same_v<e, n<5>>);
