add_library(reactive_streams INTERFACE)

target_link_libraries(reactive_streams
  INTERFACE
    Boost::boost
    Boost::system
    TL::expected
)
target_include_directories(reactive_streams
  INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
)
target_compile_options(reactive_streams
  INTERFACE
    "-Wno-padded"
)

################################################################################
add_executable(reactive_stream_tests reactive_stream_tests.cpp)
target_link_libraries(reactive_stream_tests
  PRIVATE
    Catch2::Main
    reactive_streams
)
add_test(NAME reactive_stream_tests COMMAND reactive_stream_tests)

################################################################################
add_executable(reactive_stream1 reactive_stream1.cpp)
target_link_libraries(reactive_stream1
  PRIVATE
    reactive_streams
)
