#include "fcpp/sink.h"
#include "fcpp/source.h"

#include <iostream>

namespace asio = boost::asio;
using fcpp::service;
using fcpp::sink;

namespace {
auto print_message = [](auto const& message) {
  std::cout << message << std::endl;
};
}

int main() {
  asio::io_service event_loop;

  auto pipeline = service(event_loop)  //
                  | sink(print_message);

  event_loop.run();
}
