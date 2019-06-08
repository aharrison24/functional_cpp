#pragma once

#include <boost/asio/deadline_timer.hpp>
#include <boost/asio/io_service.hpp>

#include <functional>
#include <random>
#include <string>

namespace fcpp {

class service {
 public:
  using value_type = std::string;

  service(boost::asio::io_service& io_service) : timer_(io_service) {}

  void set_message_handler(std::function<void(std::string&&)> emit) {
    emit_ = std::move(emit);
    start_sending();
  }

 private:
  void start_sending() {
    using namespace std::string_literals;
    timer_.expires_from_now(boost::posix_time::milliseconds(500));
    timer_.async_wait([this](auto const& error) {
      if (!error) {
        emit_(decision_(random_generator_) ? "Hello"s : "Goodbye"s);
      }
      start_sending();
    });
  }

 private:
  boost::asio::deadline_timer timer_;
  std::mt19937 random_generator_{std::random_device{}()};
  std::uniform_int_distribution<> decision_{0, 1};
  std::function<void(std::string&&)> emit_;
};

}  // namespace fcpp
