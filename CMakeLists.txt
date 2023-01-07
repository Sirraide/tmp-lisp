cmake_minimum_required(VERSION 3.25)
project(tmp_lisp CXX)

set(CMAKE_CXX_STANDARD 23)

include_directories(.)

add_executable(tmp_lisp
        foo.cc)
