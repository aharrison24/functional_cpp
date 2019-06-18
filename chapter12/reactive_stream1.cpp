#include "fcpp/filter.h"
#include "fcpp/join.h"
#include "fcpp/macros.h"
#include "fcpp/sink.h"
#include "fcpp/source.h"
#include "fcpp/transform.h"
#include "fcpp/values.h"

#include <boost/algorithm/string.hpp>

#include <iostream>

namespace asio = boost::asio;

namespace {
auto print_message = [](auto&& message) {
  std::cout << FWD(message) << std::endl;
};

bool is_greeting(std::string const& s) {
  return boost::iequals(s, "HELLO");
}

bool is_even(int v) {
  return v % 2 == 0;
}
}  // namespace

int main() {
  using namespace fcpp::operators;
  using fcpp::service, fcpp::values;

  asio::io_service event_loop;

  auto pipeline = service(event_loop)                              //
                  | transform(OVERLOAD_SET(boost::to_upper_copy))  //
                  | filter(is_greeting)                            //
                  | sink(print_message);

  auto pipeline2 = values{values{1, 2, 3}, values{4, 5, 6}, values{7, 8, 9}}  //
                   | join()                                                   //
                   | transform([](auto v) { return v * 3; })                  //
                   | filter(is_even)                                          //
                   | sink(print_message);

  event_loop.run();
}
