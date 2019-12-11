#include "fcpp/expected_utils.h"
#include "fcpp/filter.h"
#include "fcpp/join.h"
#include "fcpp/macros.h"
#include "fcpp/mtry.h"
#include "fcpp/sink.h"
#include "fcpp/source.h"
#include "fcpp/transform.h"
#include "fcpp/values.h"

#include <boost/algorithm/string.hpp>

#include <iostream>

namespace asio = boost::asio;

namespace {
auto print_message(tl::expected<std::string, std::exception_ptr>&& message) {
  if (!message) {
    try {
      std::rethrow_exception(FWD(message).error());
    } catch (std::exception const& e) {
      std::cout << "ERROR: " << e.what() << std::endl;
    }
  } else {
    std::cout << *FWD(message) << std::endl;
  }
}

template <typename Message>
auto print_message(Message&& message) {
  std::cout << FWD(message) << std::endl;
}

bool is_greeting(std::string const& s) {
  return boost::iequals(s, "HELLO") || boost::iequals(s, "HOLA");
}

bool is_even(int v) {
  return v % 2 == 0;
}

std::string xenophobic_identity(std::string const& s) {
  if (boost::iequals(s, "HOLA")) {
    throw std::runtime_error(s + " is a foreign greeting!");
  }
  return s;
}

}  // namespace

int main() {
  using namespace fcpp::operators;
  using fcpp::service, fcpp::values, fcpp::mtry;

  asio::io_service event_loop;

  auto const source_strings = {"Hello", "Goodbye", "Hola"};
  auto pipeline = service(event_loop, source_strings)              //
                  | transform(OVERLOAD_SET(boost::to_upper_copy))  //
                  | filter(is_greeting)                            //
                  | transform([](std::string const& s) {
                      return mtry([&] { return xenophobic_identity(s); });
                    })  //
                  | transform([](auto const& s) { return transform() }) |
                  sink(OVERLOAD_SET(print_message));

  auto pipeline2 = values{values{1, 2, 3}, values{4, 5, 6}, values{7, 8, 9}}  //
                   | join()                                                   //
                   | transform([](auto v) { return v * 3; })                  //
                   | filter(is_even)                                          //
                   | sink(OVERLOAD_SET(print_message));

  event_loop.run();
}
