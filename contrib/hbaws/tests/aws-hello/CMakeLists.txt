cmake_minimum_required(VERSION 3.13)
project(hello_s3)

set(CMAKE_CXX_STANDARD 11)

if (WIN32)
    set(AWS_INSTALL_PATH "C:\\Users\\Fran\\Desktop\\aws-install")
else()
    set(AWS_INSTALL_PATH "/home/fran/Desktop/aws-install")
endif()


set(CMAKE_PREFIX_PATH "${AWS_INSTALL_PATH}/lib/cmake" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWS_INSTALL_PATH}/lib/aws-crt-cpp" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWS_INSTALL_PATH}/lib/aws-c-http" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWS_INSTALL_PATH}/lib/aws-c-io" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWS_INSTALL_PATH}/lib/aws-c-common" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWS_INSTALL_PATH}/lib/aws-c-cal" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWS_INSTALL_PATH}/lib/aws-c-compression" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWS_INSTALL_PATH}/lib/aws-c-mqtt" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWS_INSTALL_PATH}/lib/aws-c-auth" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWS_INSTALL_PATH}/lib/aws-c-sdkutils" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWS_INSTALL_PATH}/lib/aws-c-event-stream" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWS_INSTALL_PATH}/lib/aws-checksums" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWS_INSTALL_PATH}/lib/aws-c-s3" ${CMAKE_PREFIX_PATH})

if (WIN32)
else()
    set(CMAKE_PREFIX_PATH "${AWS_INSTALL_PATH}/lib/s2n" ${CMAKE_PREFIX_PATH})
endif()

find_package(AWSSDK REQUIRED COMPONENTS s3)

add_executable(${PROJECT_NAME} hello.cpp)
target_link_libraries(${PROJECT_NAME} ${AWSSDK_LINK_LIBRARIES})
