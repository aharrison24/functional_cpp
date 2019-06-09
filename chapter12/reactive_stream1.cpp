#include "fcpp/filter.h"
#include "fcpp/macros.h"
#include "fcpp/sink.h"
#include "fcpp/source.h"
#include "fcpp/transform.h"

#include <boost/algorithm/string.hpp>

#include <iostream>

namespace asio = boost::asio;
using fcpp::filter;
using fcpp::service;
using fcpp::sink;
using fcpp::transform;

namespace {
auto print_message = [](auto&& message) {
  std::cout << FWD(message) << std::endl;
};

bool is_greeting(std::string const& s) {
  return boost::iequals(s, "HELLO");
}
}  // namespace

int main() {
  asio::io_service event_loop;

  auto pipeline = service(event_loop)                              //
                  | transform(OVERLOAD_SET(boost::to_upper_copy))  //
                  | filter(is_greeting)                            //
                  | sink(print_message);

  event_loop.run();
}
