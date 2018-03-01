cmake_minimum_required(VERSION 2.8)

project(__PROJECT-NAME__)

add_definitions(-O3)
add_definitions(-Wall)
add_definitions(-g)
add_definitions(-Wextra)
add_definitions(-Werror)
add_definitions(-std=c++11)

add_executable(${PROJECT_NAME} main.cpp)
