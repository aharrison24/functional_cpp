#include <catch2/catch.hpp>

#include <algorithm>
#include <utility>

// A simple expression template to make it possible to write very
// minimal-looking function objects for checking error state.

namespace fcpp {

class error_test_t {
 public:
  constexpr error_test_t() noexcept = default;

  constexpr explicit error_test_t(bool test_value) noexcept
      : test_value_(test_value) {}

  template <typename T>
  constexpr bool operator()(T const& t) const noexcept {
    return t.error() == test_value_;
  }

  constexpr error_test_t operator!() const noexcept {
    return error_test_t{!test_value_};
  }

  constexpr friend error_test_t operator==(error_test_t e, bool v) noexcept {
    return error_test_t{v ? e.test_value_ : !e.test_value_};
  }

  constexpr friend error_test_t operator==(bool v, error_test_t e) noexcept {
    return error_test_t{v ? e.test_value_ : !e.test_value_};
  }

 private:
  bool test_value_{true};
};

constexpr auto const error = error_test_t{};
constexpr auto const not_error = error_test_t{false};

}  // namespace fcpp

class obj_t {
 public:
  constexpr explicit obj_t(bool err = false) : err_(err) {}
  [[nodiscard]] constexpr bool error() const { return err_; }

 private:
  bool err_{false};
};

using namespace fcpp;
using std::count_if;

CATCH_SCENARIO() {
  CATCH_GIVEN("A collection of objects with error() methods") {
    auto const input = std::vector{obj_t{true}, obj_t{true}, obj_t{false},
                                   obj_t{true}, obj_t{false}};

    CATCH_THEN(
        "'error' is a function object that selects "
        "objects where error flag is set") {
      CATCH_REQUIRE(count_if(begin(input), end(input), error) == 3);
    }

    CATCH_THEN(
        "'!error' returns a function object that selects "
        "objects where error flag is not set") {
      CATCH_REQUIRE(count_if(begin(input), end(input), !error) == 2);
    }

    CATCH_THEN(
        "'not_error' returns a function object that selects "
        "objects where error flag is not set") {
      CATCH_REQUIRE(count_if(begin(input), end(input), not_error) == 2);
    }

    CATCH_THEN(
        "'!not_error' returns a function object that selects "
        "objects where error flag is set") {
      CATCH_REQUIRE(count_if(begin(input), end(input), !not_error) == 3);
    }

    CATCH_THEN(
        "'error == true' returns a function object that selects "
        "objects where error flag is set") {
      CATCH_REQUIRE(count_if(begin(input), end(input), error == true) == 3);
    }

    CATCH_THEN(
        "'error == false' returns a function object that selects "
        "objects where error flag is set") {
      CATCH_REQUIRE(count_if(begin(input), end(input), error == false) == 2);
    }

    CATCH_THEN(
        "'not_error == true' returns a function object that selects "
        "objects where error flag is not set") {
      CATCH_REQUIRE(count_if(begin(input), end(input), not_error == true) == 2);
    }

    CATCH_THEN(
        "'not_error == false' returns a function object that selects "
        "objects where error flag is set") {
      CATCH_REQUIRE(count_if(begin(input), end(input), not_error == false) ==
                    3);
    }

    CATCH_THEN(
        "'!(error == true)' returns a function object that selects "
        "objects where error flag is set") {
      CATCH_REQUIRE(count_if(begin(input), end(input), !(error == true)) == 2);
    }

    CATCH_THEN(
        "'!(error == false)' returns a function object that selects "
        "objects where error flag is set") {
      CATCH_REQUIRE(count_if(begin(input), end(input), !(error == false)) == 3);
    }

    CATCH_THEN(
        "'true == error' returns a function object that selects "
        "objects where error flag is set") {
      CATCH_REQUIRE(count_if(begin(input), end(input), true == error) == 3);
    }

    CATCH_THEN(
        "'false == error' returns a function object that selects "
        "objects where error flag is not set") {
      CATCH_REQUIRE(count_if(begin(input), end(input), false == error) == 2);
    }
  }
}
