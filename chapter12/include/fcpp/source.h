#pragma once

#include <boost/asio/deadline_timer.hpp>
#include <boost/asio/io_service.hpp>

#include <functional>
#include <random>
#include <string>
#include <vector>

namespace fcpp {

class service {
 public:
  using value_type = std::string;

  template <typename Range>
  service(boost::asio::io_service& io_service, Range const& outputs)
      : outputs_(begin(outputs), end(outputs)), timer_(io_service) {}

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
        std::uniform_int_distribution<std::size_t> decision{0, outputs_.size()};
        emit_(std::string{outputs_[decision(random_generator_)]});
      }
      start_sending();
    });
  }

 private:
  std::vector<std::string> outputs_;
  boost::asio::deadline_timer timer_;
  std::mt19937 random_generator_{std::random_device{}()};
  std::function<void(std::string&&)> emit_;
};

}  // namespace fcpp
