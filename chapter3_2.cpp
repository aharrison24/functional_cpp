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

CATCH_SCENARIO("Placeholders bind to function parameters") {
  static_assert(is_operator_v<arg_t<0>>);

  constexpr auto args = std::make_tuple(0UL, "one", -2, std::optional<int>{3});

  static_assert(std::apply(arg<0>, args) == 0UL);
  static_assert(std::apply(arg<1>, args) == std::get<1>(args));
  static_assert(std::apply(arg<2>, args) == -2);
  static_assert(std::apply(arg<3>, args) == 3);
}
