#include <catch2/catch.hpp>

#include <iterator>
#include <utility>

// Implementing some common algorithms in terms of std::accumulate

namespace fcpp {

template <typename T, typename ForwardIter, typename BinaryOperation>
constexpr auto foldl(ForwardIter first,
                     ForwardIter last,
                     T init_value,
                     BinaryOperation op) -> T {
  for (; first != last; ++first) {
    init_value = op(std::move(init_value), *first);
  }
  return init_value;
}

template <typename ForwardIter, typename Predicate>
constexpr bool any_of(ForwardIter first, ForwardIter last, Predicate p) {
  return foldl(
      first, last, false,
      [p = std::move(p)](bool init, auto const& v) { return init | p(v); });
}

template <typename ForwardIter, typename Predicate>
constexpr bool all_of(ForwardIter first, ForwardIter last, Predicate p) {
  return foldl(first, last, true, [p = std::move(p)](bool init, auto const& v) {
    return init && p(v);
  });
}

template <typename ForwardIter, typename Predicate>
constexpr auto find_if(ForwardIter first, ForwardIter last, Predicate p) {
  struct accumulator_t {
    typename std::iterator_traits<ForwardIter>::difference_type count{0};
    bool found{false};
  };

  auto increment_if_not = [p = std::move(p)](accumulator_t init,
                                             auto const& val) {
    return (init.found || p(val)) ? accumulator_t{init.count, true}
                                  : accumulator_t{init.count + 1, false};
  };

  auto result = foldl(first, last, accumulator_t{}, increment_if_not);
  return std::next(first, result.count);
}

}  // namespace fcpp

template <typename T>
auto greater_than(T lim) {
  return [lim = std::move(lim)](auto const& v) { return v > lim; };
}

CATCH_SCENARIO("any_of") {
  CATCH_GIVEN("An empty collection") {
    std::vector<int> input;
    CATCH_THEN("any_of returns false") {
      CATCH_REQUIRE(!fcpp::any_of(begin(input), end(input), greater_than(5)));
    }
  }

  CATCH_GIVEN("A collection containing only items passing the predicate") {
    std::vector<int> input{6, 7, 8, 9};
    CATCH_THEN("any_of returns true") {
      CATCH_REQUIRE(fcpp::any_of(begin(input), end(input), greater_than(1)));
    }
  }

  CATCH_GIVEN("A collection containing only items not-passing the predicate") {
    std::vector<int> input{1, 2, 3, 4};
    CATCH_THEN("any_of returns false") {
      CATCH_REQUIRE(!fcpp::any_of(begin(input), end(input), greater_than(5)));
    }
  }

  CATCH_GIVEN(
      "A collection with only a subset of items passing the predicate") {
    std::vector<int> input{1, 6, 2, 10, 100};
    CATCH_THEN("any_of returns true") {
      CATCH_REQUIRE(fcpp::any_of(begin(input), end(input), greater_than(5)));
    }
  }
}

CATCH_SCENARIO("all_of") {
  CATCH_GIVEN("An empty collection") {
    std::vector<int> input;
    CATCH_THEN("all_of returns true") {
      CATCH_REQUIRE(fcpp::all_of(begin(input), end(input), greater_than(5)));
    }
  }

  CATCH_GIVEN("A collection containing only items passing the predicate") {
    std::vector<int> input{6, 7, 8, 9};
    CATCH_THEN("all_of returns true") {
      CATCH_REQUIRE(fcpp::all_of(begin(input), end(input), greater_than(1)));
    }
  }

  CATCH_GIVEN("A collection containing only items not-passing the predicate") {
    std::vector<int> input{1, 2, 3, 4};
    CATCH_THEN("all_of returns false") {
      CATCH_REQUIRE(!fcpp::all_of(begin(input), end(input), greater_than(5)));
    }
  }

  CATCH_GIVEN(
      "A collection with only a subset of items passing the predicate") {
    std::vector<int> input{1, 6, 2, 10, 100};
    CATCH_THEN("all_of returns false") {
      CATCH_REQUIRE(!fcpp::all_of(begin(input), end(input), greater_than(5)));
    }
  }
}

CATCH_SCENARIO("find_if") {
  CATCH_GIVEN("An empty collection") {
    std::vector<int> input;
    CATCH_THEN("find_if returns end iterator") {
      CATCH_REQUIRE(fcpp::find_if(begin(input), end(input), greater_than(5)) ==
                    end(input));
    }
  }

  CATCH_GIVEN("A collection containing only items passing the predicate") {
    std::vector<int> input{6, 7, 8, 9};
    CATCH_THEN("find_if returns an iterator to the first") {
      CATCH_REQUIRE(fcpp::find_if(begin(input), end(input), greater_than(5)) ==
                    begin(input));
    }
  }

  CATCH_GIVEN(
      "A collection where the initial elements don't pass the predicate") {
    std::vector<int> input{1, 2, 3, 4, 5, 6, 7, 8};
    CATCH_THEN("find_if returns an iterator to the first one that does") {
      CATCH_REQUIRE(fcpp::find_if(begin(input), end(input), greater_than(5)) ==
                    std::next(begin(input), 5));
    }
  }
}
