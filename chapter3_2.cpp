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
struct dynamic_actor_tag : actor_tag {};

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
constexpr bool is_dynamic_actor_v =
    stdex::is_detected_exact_v<dynamic_actor_tag, actor_type_expr, T>;

template <typename T>
using IsActor = std::enable_if_t<is_actor_v<T>>;

template <typename T>
using IsNotActor = std::enable_if_t<!is_actor_v<T>>;

template <typename T>
using IsNonDynamicActor =
    std::enable_if_t<is_actor_v<T> && !is_dynamic_actor_v<T>>;

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
constexpr auto to_actor(T&& t) noexcept -> value_t<remove_cvref_t<T>> {
  return value_t<remove_cvref_t<T>>{std::forward<T>(t)};
}

// If passed an existing actor then the function is a no-op.
template <typename T, typename = IsActor<T>>
constexpr auto to_actor(T t) noexcept -> remove_cvref_t<T> {
  return t;
}

// -------------------------
template <typename Right, typename Op>
struct unary_actor_t {
  using actor_type = actor_tag;

  template <typename Right_, typename Op_, typename = IsActor<Right_>>
  constexpr unary_actor_t(Right_&& f, Op_&& op)
      : f_(std::forward<Right_>(f)), op_(std::forward<Op_>(op)) {}

  template <typename... Ts>
  constexpr decltype(auto) operator()(Ts&&... ts) const {
    return op_(f_(std::forward<Ts>(ts)...));
  }

 private:
  Right f_;
  Op op_;
};

template <typename Right, typename Op, typename = IsActor<Right>>
constexpr auto make_unary_actor(Right&& rhs, Op&& op) noexcept
    -> unary_actor_t<remove_cvref_t<Right>, remove_cvref_t<Op>> {
  return {std::forward<Right>(rhs), std::forward<Op>(op)};
}

// -------------------------
template <typename Left, typename Right, typename Op>
struct binary_actor_t {
  using actor_type = actor_tag;

  template <
      typename Left_,
      typename Right_,
      typename Op_,
      typename = std::enable_if_t<is_actor_v<Left_> && is_actor_v<Right_>>>
  constexpr binary_actor_t(Left_&& lhs, Right_&& rhs, Op_&& op)
      : left_(std::forward<Left_>(lhs)),
        right_(std::forward<Right_>(rhs)),
        op_(std::forward<Op_>(op)) {}

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
    -> binary_actor_t<decltype(to_actor(std::forward<Left>(lhs))),
                      decltype(to_actor(std::forward<Right>(rhs))),
                      remove_cvref_t<Op>> {
  return {to_actor(std::forward<Left>(lhs)), to_actor(std::forward<Right>(rhs)),
          std::forward<Op>(op)};
}

// -----------------------------------------------------------------------------
// type erasure

template <typename R, typename... Args>
class dynamic_actor_t;

template <typename R, typename... Args>
class dynamic_actor_t<R(Args...)> {
 public:
  using actor_type = dynamic_actor_tag;

 private:
  struct concept_t {
    virtual R invoke(Args&&... args) const = 0;
    virtual std::unique_ptr<concept_t> clone() const = 0;
    virtual ~concept_t() = default;
  };

  template <typename Actor>
  struct model_t : concept_t {
    template <typename A>
    explicit model_t(A&& actor) : actor_(std::forward<A>(actor)) {}

    std::unique_ptr<concept_t> clone() const override {
      return std::make_unique<model_t<Actor>>(actor_);
    }

    R invoke(Args&&... args) const override {
      // Using std::move because Args&&... are *not* forwarding references,
      // but rvalue references.
      return std::invoke(actor_, std::move(args)...);
    }

    Actor actor_;
  };

 public:
  template <typename Actor, typename = IsNonDynamicActor<Actor>>
  explicit dynamic_actor_t(Actor&& actor)
      : ptr_(std::make_unique<model_t<remove_cvref_t<Actor>>>(
            std::forward<Actor>(actor))) {}

  ~dynamic_actor_t() = default;
  dynamic_actor_t(dynamic_actor_t const& rhs) : ptr_(rhs.ptr_->clone()) {}
  dynamic_actor_t(dynamic_actor_t&&) = default;
  dynamic_actor_t& operator=(dynamic_actor_t&&) = default;

  dynamic_actor_t& operator=(dynamic_actor_t const& rhs) {
    dynamic_actor_t temp(rhs);
    using std::swap;
    swap(this->ptr_, temp.ptr_);
    return *this;
  }

  R operator()(Args... args) const {
    // Args are captured by value and unconditionally moved. We have to capture
    // by value *somewhere* because virtual methods don't allow perfect
    // forwarding. We therefore choose to capture by value in the public API so
    // that callers can see that it's happening. After this the args are always
    // moved unconditionally.
    return ptr_->invoke(std::move(args)...);
  }

 private:
  std::unique_ptr<concept_t> ptr_;
};

template <typename FuncPrototype, typename Actor>
auto erase_type(Actor&& actor) noexcept -> dynamic_actor_t<FuncPrototype> {
  return dynamic_actor_t<FuncPrototype>{std::forward<Actor>(actor)};
}

// -----------------------------------------------------------------------------
// operators

#define FCPP_ADD_UNARY_OP(op_name, function_obj)                               \
  template <typename Right>                                                    \
  constexpr auto op_name(Right&& rhs)                                          \
      ->decltype(make_unary_actor(std::forward<Right>(rhs), function_obj)) {   \
    return make_unary_actor(std::forward<Right>(rhs), function_obj);           \
  }

#define FCPP_ADD_BINARY_OP(op_name, function_obj)                              \
  template <typename Left, typename Right>                                     \
  constexpr auto op_name(Left&& lhs, Right&& rhs)                              \
      ->decltype(make_binary_actor(std::forward<Left>(lhs),                    \
                                   std::forward<Right>(rhs), function_obj)) {  \
    return make_binary_actor(std::forward<Left>(lhs),                          \
                             std::forward<Right>(rhs), function_obj);          \
  }

FCPP_ADD_UNARY_OP(operator!, std::logical_not<>{})

FCPP_ADD_BINARY_OP(operator+, std::plus<>{})
FCPP_ADD_BINARY_OP(operator-, std::minus<>{})
FCPP_ADD_BINARY_OP(operator*, std::multiplies<>{})
FCPP_ADD_BINARY_OP(operator==, std::equal_to<>{})
FCPP_ADD_BINARY_OP(operator<, std::less<>{})
FCPP_ADD_BINARY_OP(operator<=, std::less_equal<>{})

#undef FCPP_ADD_BINARY_OP
#undef FCPP_ADD_UNARY_OP

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

CATCH_SCENARIO("Less-than operator") {
  static_assert((arg<0> < 5)(4));
  static_assert(!(arg<0> < 5)(6));
  static_assert((arg<0> < arg<1>)(0, 100));
  static_assert(!(arg<0> < arg<1>)(10, 10));
}

CATCH_SCENARIO("Less-than-or-equal operator") {
  static_assert((arg<0> <= arg<1>)(0, 100));
  static_assert((arg<0> <= arg<1>)(10, 10));
}

CATCH_SCENARIO("Transform with lambda expression") {
  auto const input = {1, 2, 3, 4, 5};

  std::vector<int> result;
  std::transform(begin(input), end(input), std::back_inserter(result),
                 (arg<0> - 1) * (arg<0> - 1));

  auto const expected = {0, 1, 4, 9, 16};
  CATCH_REQUIRE(std::equal(begin(result), end(result), begin(expected)));
}

CATCH_SCENARIO("Type erasure for actors") {
  CATCH_GIVEN("An actor with a complicated static type") {
    auto actor = arg<0> * 100;

    static_assert(is_actor_v<decltype(actor)>);

    CATCH_THEN("we can erase it's type with a call to erase_type") {
      auto dynamic_actor = erase_type<int(int)>(std::move(actor));
      static_assert(
          std::is_same_v<decltype(dynamic_actor), dynamic_actor_t<int(int)>>);

      CATCH_REQUIRE(dynamic_actor(10) == 1000);
      CATCH_AND_THEN("we can also use it in further actor expressions") {
        auto aggregated_actor = !dynamic_actor;
        CATCH_REQUIRE(aggregated_actor(10) == false);
      }
    }
    CATCH_GIVEN("A pair of type-erased actors") {
      auto dynamic1 = erase_type<int(int)>(arg<0> + 5);
      auto dynamic2 = erase_type<int(int)>(arg<0> - 5);

      CATCH_THEN("we can re-bind a dynamic actor via copy assignment") {
        dynamic2 = dynamic1;
        CATCH_REQUIRE(dynamic1(10) == 15);
        CATCH_REQUIRE(dynamic2(10) == 15);
      }

      CATCH_THEN("we can re-bind a dynamic actor via move assignment") {
        dynamic1 = std::move(dynamic2);
        CATCH_REQUIRE(dynamic1(10) == 5);
      }
    }
  }

  CATCH_GIVEN("A vector of type-erased actors") {
    std::vector<dynamic_actor_t<bool(int, int)>> actors;
    actors.emplace_back(arg<0> + arg<1> < 42);
    actors.emplace_back(value_t(false));
    actors.emplace_back(arg<0> * arg<0> == 100);

    CATCH_AND_GIVEN("a set of arguments") {
      constexpr auto args = std::make_tuple(10, 10);

      CATCH_THEN("we can apply each of the actors to the arguments") {
        std::vector<bool> results;
        std::transform(
            begin(actors), end(actors), std::back_inserter(results),
            [&args](auto const& actor) { return std::apply(actor, args); });

        std::vector<bool> expected{true, false, true};
        using Catch::Matchers::Equals;
        CATCH_REQUIRE_THAT(results, Equals(expected));
      }
    }
  }
}
