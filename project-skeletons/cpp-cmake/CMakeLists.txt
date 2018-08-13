cmake_minimum_required(VERSION 3.1..3.11)

if(${CMAKE_VERSION} VERSION_LESS 3.12)
  cmake_policy(VERSION ${CMAKE_VERSION})
endif()

project(__PROJECT-NAME__)

# use ccache
find_program(CCACHE_PROGRAM ccache)
if(CCACHE_PROGRAM)
  set(CMAKE_CXX_COMPILER_LAUNCHER "${CCACHE_PROGRAM}")
endif()

option(USE_CLANG_TIDY "Use clang-tidy to perform code analysis" ON)
if(USE_CLANG_TIDY)
  # add clang-tidy if available
  if(CMAKE_VERSION VERSION_GREATER 3.6)
    option(CLANG_TIDY_FIX "Perform fixes for Clang-Tidy" OFF)
    find_program(
      CLANG_TIDY_PATH
      NAMES clang-tidy
      )

    if(CLANG_TIDY_PATH)
      if(CLANG_TIDY_FIX)
	set(CMAKE_CXX_CLANG_TIDY "${CLANG_TIDY_PATH}" "-fix")
      else()
	set(CMAKE_CXX_CLANG_TIDY "${CLANG_TIDY_PATH}")
      endif()
    endif()
  endif()
endif()

option(USE_IWYU "Use include-what-you-use to perform include dependencies check" OFF)
if(USE_IWYU)
  # add iwyu if available
  if(CMAKE_VERSION VERSION_GREATER 3.3)
    find_program(
      IWYU_PATH
      NAMES include-what-you-use iwyu
      )

    if(IWYU_PATH)
      set(CMAKE_CXX_INCLUDE_WHAT_YOU_USE "${IWYU_PATH}")
    endif()
  endif()
endif()

# boost
set(Boost_USE_STATIC_LIBS OFF)
set(Boost_USE_MULTITHREADED ON)
set(Boost_USE_STATIC_RUNTIME OFF)

if(MSVC)
  find_package(Boost 1.60 REQUIRED COMPONENTS system thread)
else()
  find_package(Boost 1.54 REQUIRED COMPONENTS system thread)
endif()
message(STATUS "Boost version: ${Boost_VERSION}")

add_definitions(-Wall)
add_definitions(-g)
add_definitions(-Wextra)

add_executable("${PROJECT_NAME}" main.cpp)
target_compile_features("${PROJECT_NAME}" PRIVATE cxx_std_14)
set_target_properties("${PROJECT_NAME}" PROPERTIES CXX_EXTENSIONS off)
