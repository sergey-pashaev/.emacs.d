cmake_minimum_required(VERSION 3.1..3.11)

if(${CMAKE_VERSION} VERSION_LESS 3.12)
  cmake_policy(VERSION ${CMAKE_VERSION})
endif()

project(app)

add_executable(app main.cpp)
target_link_libraries(app PUBLIC Boost::system)
target_compile_features(app PRIVATE cxx_std_14)
set_target_properties(app PROPERTIES CXX_EXTENSIONS off)
