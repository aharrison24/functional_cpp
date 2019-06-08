#include "fcpp/macros.h"
#include "fcpp/sink.h"
#include "fcpp/source.h"
#include "fcpp/transform.h"

#include <boost/algorithm/string.hpp>

#include <iostream>

namespace asio = boost::asio;
using fcpp::service;
using fcpp::sink;
using fcpp::transform;

namespace {
auto print_message = [](auto&& message) {
  std::cout << FWD(message) << std::endl;
};
}

int main() {
  asio::io_service event_loop;

  auto pipeline =
      transform(service(event_loop), OVERLOAD_SET(boost::to_upper_copy))  //
      | sink(print_message);

  event_loop.run();
}
