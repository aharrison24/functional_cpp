#include <catch2/catch.hpp>

#include <experimental/type_traits>
#include <optional>
#include <tuple>

// -----------------------------------------------------------------------------
// general traits
namespace fcpp::traits {

using std::experimental::detected_t;
using std::experimental::is_detected_v;

template <typename T>
using remove_cvref_t = std::remove_cv_t<std::remove_reference_t<T>>;
}  // namespace fcpp::traits

// -----------------------------------------------------------------------------
// operator tags
namespace fcpp {
struct operator_tag {};
struct placeholder_tag : operator_tag {};
}  // namespace fcpp

// -----------------------------------------------------------------------------
// operator traits
namespace fcpp::operator_traits {

using namespace fcpp::traits;

template <typename T>
using placeholder_expr = typename remove_cvref_t<T>::operator_type;

namespace detail {

template <typename T, typename DetectedOpType = detected_t<placeholder_expr, T>>
constexpr bool is_operator() {
  // NOLINTNEXTLINE(readability-braces-around-statements, bugprone-suspicious-semicolon)
  if constexpr (!is_detected_v<placeholder_expr, T>) {
    return false;
  }
  // NOLINTNEXTLINE(readability-braces-around-statements, bugprone-suspicious-semicolon)
  if constexpr (!std::is_convertible_v<DetectedOpType, operator_tag>) {
    return false;
  }

  return true;
}
}  // namespace detail

template <typename T>
constexpr bool is_operator_v = detail::is_operator<T>();

template <typename T, typename R = T>
using IsOperator = std::enable_if_t<is_operator_v<T>, R>;

template <typename T, typename R = T>
using IsNotOperator = std::enable_if_t<!is_operator_v<T>, R>;

}  // namespace fcpp::operator_traits

// -----------------------------------------------------------------------------
// util functions
namespace fcpp {

template <int N, typename... Ts>
constexpr decltype(auto) get(Ts&&... ts) noexcept {
  return std::get<N>(std::forward_as_tuple(ts...));
}

}  // namespace fcpp

// -----------------------------------------------------------------------------
// operators
namespace fcpp {

using operator_traits::is_operator_v;
using operator_traits::IsNotOperator;
using operator_traits::IsOperator;

template <size_t N>
struct arg_t {
  using operator_type = placeholder_tag;

  template <typename... Ts>
  constexpr decltype(auto) operator()(Ts&&... ts) const noexcept {
    static_assert(N < sizeof...(ts),
                  "Argument index must not exceed size of parameter pack");
    return get<N>(ts...);
  }
};

template <typename T>
struct value_t {
  using operator_type = operator_tag;

  constexpr explicit value_t(T val) : val_(std::move(val)) {}

  template <typename... Args>
  constexpr T operator()([[maybe_unused]] Args&&... args) const
      noexcept(std::is_nothrow_copy_constructible_v<T>) {
    return val_;
  }

 private:
  T val_;
};

template <typename Right, typename Op>
struct unary_operator_t {
  using operator_type = operator_tag;

  constexpr unary_operator_t(Right f, Op op)
      : f_(std::move(f)), op_(std::move(op)) {}

  template <typename... Ts>
  constexpr decltype(auto) operator()(Ts&&... ts) const {
    return op_(f_(std::forward<Ts>(ts)...));
  }

 private:
  Right f_;
  Op op_;
};

template <typename T>
constexpr auto operator!(T t)
    -> IsOperator<T, unary_operator_t<T, std::logical_not<>>> {
  return {std::move(t), std::logical_not<>{}};
}

}  // namespace fcpp

// -----------------------------------------------------------------------------
// placeholders
namespace fcpp::args {

template <int N>
constexpr auto arg = ::fcpp::arg_t<N>{};

}  // namespace fcpp::args

// -----------------------------------------------------------------------------
// Tests
using namespace fcpp;
using namespace fcpp::args;
using namespace fcpp::operator_traits;
namespace stdex = std::experimental;

CATCH_SCENARIO("Placeholders bind to function parameters") {
  static_assert(is_operator_v<arg_t<0>>);

  constexpr auto args = std::make_tuple(0UL, "one", -2, std::optional<int>{3});

  static_assert(std::apply(arg<0>, args) == 0UL);
  static_assert(std::apply(arg<1>, args) == std::get<1>(args));
  static_assert(std::apply(arg<2>, args) == -2);
  static_assert(std::apply(arg<3>, args) == 3);
}

template <typename T>
using negate_expression = decltype(!std::declval<T>());

CATCH_SCENARIO("Ensure that Actor overloads don't fire for unrelated types") {
  struct S {};
  static_assert(!stdex::is_detected_v<negate_expression, S>);
}

CATCH_SCENARIO("value_t captures values for later") {
  static_assert(operator_traits::is_operator_v<value_t<int>>);
  static_assert(value_t{1234}() == 1234);
}

CATCH_SCENARIO("Bang operator negates bound value") {
  static_assert(is_operator_v<decltype(arg<0>)>);
  static_assert(is_operator_v<decltype(!arg<0>)>);

  constexpr auto args = std::make_tuple(false, true, 42, 0);

  static_assert(std::apply(!arg<0>, args));
  static_assert(!std::apply(!arg<1>, args));
  static_assert(!std::apply(!arg<2>, args));
  static_assert(std::apply(!arg<3>, args));
}
