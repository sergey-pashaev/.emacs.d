cmake_minimum_required(VERSION 3.1..3.11)

if(${CMAKE_VERSION} VERSION_LESS 3.12)
  cmake_policy(VERSION ${CMAKE_VERSION})
endif()

project(app_test)

add_executable(app_test app_test.cpp catch2-main.cpp)
target_include_directories(app_test PUBLIC ../thirdparty)
target_compile_features(app_test PRIVATE cxx_std_14)
set_target_properties(app_test PROPERTIES CXX_EXTENSIONS off)
