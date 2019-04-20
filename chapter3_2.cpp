#include <catch2/catch.hpp>

#include <optional>
#include <tuple>

namespace fcpp {

template <int N, typename... Ts>
constexpr decltype(auto) get(Ts&&... ts) noexcept {
  return std::get<N>(std::forward_as_tuple(ts...));
}

template <size_t N>
struct arg_t {
  template <typename... Ts>
  constexpr decltype(auto) operator()(Ts&&... ts) const noexcept {
    static_assert(N < sizeof...(ts),
                  "Argument index must not exceed size of parameter pack");
    return get<N>(ts...);
  }
};

namespace args {
template <int N>
constexpr auto arg = ::fcpp::arg_t<N>{};

}  // namespace args
}  // namespace fcpp

using namespace fcpp::args;

CATCH_SCENARIO("Placeholders bind to function parameters") {
  constexpr auto args = std::make_tuple(0UL, "one", -2, std::optional<int>{3});

  static_assert(std::apply(arg<0>, args) == 0UL);
  static_assert(std::apply(arg<1>, args) == std::get<1>(args));
  static_assert(std::apply(arg<2>, args) == -2);
  static_assert(std::apply(arg<3>, args) == 3);
}
