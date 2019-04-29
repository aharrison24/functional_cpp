#include <catch2/catch.hpp>

#include <experimental/type_traits>
#include <optional>
#include <tuple>
#include <vector>

namespace stdex = std::experimental;

// -----------------------------------------------------------------------------
// general traits
namespace fcpp {

template <typename T>
using remove_cvref_t = std::remove_cv_t<std::remove_reference_t<T>>;

// -----------------------------------------------------------------------------
// actor tags
struct actor_tag {};
struct placeholder_tag : actor_tag {};

// -----------------------------------------------------------------------------
// actor traits

template <typename T>
using actor_type_expr = typename remove_cvref_t<T>::actor_type;

namespace detail {

template <typename T,
          typename DetectedOpType = stdex::detected_t<actor_type_expr, T>>
constexpr bool is_actor() {
  // NOLINTNEXTLINE(readability-braces-around-statements, bugprone-suspicious-semicolon)
  if constexpr (!stdex::is_detected_v<actor_type_expr, T>) {
    return false;
  }
  // NOLINTNEXTLINE(readability-braces-around-statements, bugprone-suspicious-semicolon)
  if constexpr (!std::is_convertible_v<DetectedOpType, actor_tag>) {
    return false;
  }

  return true;
}
}  // namespace detail

template <typename T>
constexpr bool is_actor_v = detail::is_actor<T>();

template <typename T>
using IsActor = std::enable_if_t<is_actor_v<T>>;

template <typename T>
using IsNotActor = std::enable_if_t<!is_actor_v<T>>;

// -----------------------------------------------------------------------------
// util functions

template <int N, typename... Ts>
constexpr decltype(auto) get(Ts&&... ts) noexcept {
  return std::get<N>(std::forward_as_tuple(ts...));
}

// -----------------------------------------------------------------------------
// actors

template <size_t N>
struct arg_t {
  using actor_type = placeholder_tag;

  template <typename... Ts>
  constexpr decltype(auto) operator()(Ts&&... ts) const noexcept {
    static_assert(N < sizeof...(ts),
                  "Argument index must not exceed size of parameter pack");
    return get<N>(ts...);
  }
};

// -------------------------
template <typename T>
struct value_t {
  using actor_type = actor_tag;

  constexpr explicit value_t(T val) : val_(std::move(val)) {}

  template <typename... Args>
  constexpr T operator()([[maybe_unused]] Args&&... args) const
      noexcept(std::is_nothrow_copy_constructible_v<T>) {
    return val_;
  }

 private:
  T val_;
};

// to_actor converts values to lazily-evaluated function objects
template <typename T, typename = IsNotActor<T>>
constexpr auto to_actor(T&& t) noexcept
    -> decltype(value_t{std::forward<T>(t)}) {
  return value_t{std::forward<T>(t)};
}

// If passed an existing actor then the function is a no-op.
template <typename T, typename = IsActor<T>>
constexpr auto to_actor(T t) noexcept -> T {
  return t;
}

// -------------------------
template <typename Right, typename Op, typename Enable = void>
struct unary_actor_t;

template <typename Right, typename Op>
struct unary_actor_t<Right, Op, std::enable_if_t<is_actor_v<Right>>> {
  using actor_type = actor_tag;

  constexpr unary_actor_t(Right f, Op op)
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
unary_actor_t(Right, Op)->unary_actor_t<Right, Op>;

// -------------------------
template <typename Left, typename Right, typename Op>
struct binary_actor_t {
  using actor_type = actor_tag;

  static_assert(is_actor_v<Left> && is_actor_v<Right>,
                "Both Left and Right template parameters must be actors");

  constexpr binary_actor_t(Left lhs, Right rhs, Op op)
      : left_(std::move(lhs)), right_(std::move(rhs)), op_(std::move(op)) {}

  template <typename... Ts>
  constexpr decltype(auto) operator()(Ts&&... ts) const {
    return op_(left_(ts...), right_(ts...));
  }

 private:
  Left left_;
  Right right_;
  Op op_;
};

template <typename Left, typename Right>
using AtLeastOneActor = std::enable_if_t<is_actor_v<Left> || is_actor_v<Right>>;

template <typename Left,
          typename Right,
          typename Op,
          typename = AtLeastOneActor<Left, Right>>
constexpr auto make_binary_actor(Left&& lhs, Right&& rhs, Op&& op) noexcept
    -> decltype(binary_actor_t{to_actor(std::forward<Left>(lhs)),
                               to_actor(std::forward<Right>(rhs)),
                               std::forward<Op>(op)}) {
  return binary_actor_t{to_actor(std::forward<Left>(lhs)),
                        to_actor(std::forward<Right>(rhs)),
                        std::forward<Op>(op)};
}

// -----------------------------------------------------------------------------
// operators

template <typename Right>
constexpr auto operator!(Right rhs)
    -> decltype(unary_actor_t(rhs, std::logical_not<>{})) {
  return unary_actor_t(rhs, std::logical_not<>{});
}

template <typename Left, typename Right>
constexpr auto operator+(Left lhs, Right rhs)
    -> decltype(make_binary_actor(lhs, rhs, std::plus<>{})) {
  return make_binary_actor(lhs, rhs, std::plus<>{});
}

template <typename Left, typename Right>
constexpr auto operator-(Left lhs, Right rhs)
    -> decltype(make_binary_actor(lhs, rhs, std::minus<>{})) {
  return make_binary_actor(lhs, rhs, std::minus<>{});
}

template <typename Left, typename Right>
constexpr auto operator*(Left lhs, Right rhs)
    -> decltype(make_binary_actor(lhs, rhs, std::multiplies<>{})) {
  return make_binary_actor(lhs, rhs, std::multiplies<>{});
}

template <typename Left, typename Right>
constexpr auto operator==(Left lhs, Right rhs)
    -> decltype(make_binary_actor(lhs, rhs, std::equal_to<>{})) {
  return make_binary_actor(lhs, rhs, std::equal_to<>{});
}

// -----------------------------------------------------------------------------
// placeholders

template <int N>
constexpr auto arg = ::fcpp::arg_t<N>{};

}  // namespace fcpp

// -----------------------------------------------------------------------------
// Tests
using namespace fcpp;

CATCH_SCENARIO("Placeholders bind to function parameters") {
  static_assert(is_actor_v<arg_t<0>>);

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
  static_assert(is_actor_v<value_t<int>>);
  static_assert(value_t{1234}() == 1234);
}

CATCH_SCENARIO("Bang operator negates bound value") {
  static_assert(is_actor_v<decltype(arg<0>)>);
  static_assert(is_actor_v<decltype(!arg<0>)>);

  constexpr auto args = std::make_tuple(false, true, 42, 0);

  static_assert(std::apply(!arg<0>, args));
  static_assert(!std::apply(!arg<1>, args));
  static_assert(!std::apply(!arg<2>, args));
  static_assert(std::apply(!arg<3>, args));
}

CATCH_SCENARIO("Plus operator") {
  static_assert(is_actor_v<decltype(arg<0> + arg<1>)>);

  constexpr auto args = std::make_tuple(5, 42, 1290);

  static_assert(1 + 1 == 2);
  static_assert(std::apply(arg<0> + arg<0>, args) == 10);
  static_assert(std::apply(arg<0> + arg<1> + arg<2>, args) == 1337);
  static_assert((arg<0> + 122)(1) == 123);
  static_assert(('a' + arg<0>)(2) == 'c');
}

CATCH_SCENARIO("Minus operator") {
  static_assert(is_actor_v<decltype(arg<0> - arg<1>)>);

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
