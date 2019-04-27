#include <catch2/catch.hpp>

#include <experimental/type_traits>
#include <optional>
#include <tuple>
#include <vector>

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

template <typename T>
using IsOperator = std::enable_if_t<is_operator_v<T>>;

template <typename T>
using IsNotOperator = std::enable_if_t<!is_operator_v<T>>;

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

template <typename T, typename = IsOperator<T>>
constexpr auto to_operator(T t) noexcept -> T {
  return t;
}

template <typename T, typename = IsNotOperator<T>>
constexpr auto to_operator(T&& t) -> decltype(value_t{std::forward<T>(t)}) {
  return value_t{std::forward<T>(t)};
}

template <typename Right, typename Op, typename Enable = void>
struct unary_operator_t;

template <typename Right, typename Op>
struct unary_operator_t<Right, Op, std::enable_if_t<is_operator_v<Right>>> {
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

template <typename Right, typename Op>
unary_operator_t(Right, Op)->unary_operator_t<Right, Op>;

template <typename Left, typename Right, typename Op>
struct binary_operator_t {
  using operator_type = operator_tag;

  template <typename T1,
            typename T2,
            typename = std::enable_if_t<is_operator_v<T1> || is_operator_v<T2>>>
  constexpr binary_operator_t(T1&& left, T2&& right, Op op)
      : left_(to_operator(std::forward<T1>(left))),
        right_(to_operator(std::forward<T2>(right))),
        op_(std::move(op)) {}

  template <typename... Ts>
  constexpr decltype(auto) operator()(Ts&&... ts) const {
    return op_(left_(ts...), right_(ts...));
  }

 private:
  Left left_;
  Right right_;
  Op op_;
};

template <typename T1, typename T2, typename Op>
binary_operator_t(T1&& lhs, T2&& rhs, Op)
    ->binary_operator_t<decltype(to_operator(std::forward<T1>(lhs))),
                        decltype(to_operator(std::forward<T2>(rhs))),
                        Op>;

template <typename Right>
constexpr auto operator!(Right rhs)
    -> decltype(unary_operator_t(rhs, std::logical_not<>{})) {
  return unary_operator_t(rhs, std::logical_not<>{});
}

template <typename Left, typename Right>
constexpr auto operator+(Left lhs, Right rhs)
    -> decltype(binary_operator_t(lhs, rhs, std::plus<>{})) {
  return binary_operator_t(lhs, rhs, std::plus<>{});
}

template <typename Left, typename Right>
constexpr auto operator-(Left lhs, Right rhs)
    -> decltype(binary_operator_t(lhs, rhs, std::minus<>{})) {
  return binary_operator_t(lhs, rhs, std::minus<>{});
}

template <typename Left, typename Right>
constexpr auto operator*(Left lhs, Right rhs)
    -> decltype(binary_operator_t(lhs, rhs, std::multiplies<>{})) {
  return binary_operator_t(lhs, rhs, std::multiplies<>{});
}

template <typename Left, typename Right>
constexpr auto operator==(Left lhs, Right rhs)
    -> decltype(binary_operator_t(lhs, rhs, std::equal_to<>{})) {
  return binary_operator_t(lhs, rhs, std::equal_to<>{});
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

template <typename T, typename U>
using plus_expression = decltype(std::declval<T>() + std::declval<U>());

CATCH_SCENARIO("Ensure that Actor overloads don't fire for unrelated types") {
  struct S {};
  static_assert(!stdex::is_detected_v<negate_expression, S>);
  static_assert(!stdex::is_detected_v<plus_expression, S, S>);
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

CATCH_SCENARIO("Plus operator") {
  static_assert(is_operator_v<decltype(arg<0> + arg<1>)>);

  constexpr auto args = std::make_tuple(5, 42, 1290);

  static_assert(1 + 1 == 2);
  static_assert(std::apply(arg<0> + arg<0>, args) == 10);
  static_assert(std::apply(arg<0> + arg<1> + arg<2>, args) == 1337);
  static_assert((arg<0> + 122)(1) == 123);
  static_assert(('a' + arg<0>)(2) == 'c');
}

CATCH_SCENARIO("Minus operator") {
  static_assert(is_operator_v<decltype(arg<0> - arg<1>)>);

  constexpr auto args = std::make_tuple(5, 42, 1290);

  static_assert(1 - 1 == 0);
  static_assert(std::apply(arg<0> - arg<0>, args) == 0);
  static_assert((arg<0> - 122)(1) == -121);
  static_assert(('c' - arg<0>)(2) == 'a');
}

CATCH_SCENARIO("Multiplication operator") {
  static_assert((arg<0> * arg<1>)(4, 5) == 20);
}

CATCH_SCENARIO("Equality operator") {
  static_assert(!(arg<0> == 42)(0));
  static_assert((arg<0> == 42)(42));
}

CATCH_SCENARIO("Transform with lambda expression") {
  auto const input = {1, 2, 3, 4, 5};

  std::vector<int> result;
  std::transform(begin(input), end(input), std::back_inserter(result),
                 (arg<0> - 1) * (arg<0> - 1));

  auto const expected = {0, 1, 4, 9, 16};
  CATCH_REQUIRE(std::equal(begin(result), end(result), begin(expected)));
}
