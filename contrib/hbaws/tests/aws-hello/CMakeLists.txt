cmake_minimum_required(VERSION 3.13)
project(hello_s3)

set(CMAKE_CXX_STANDARD 11)

set(AWSROOT "$ENV{AWS_SDK_ROOT}")

if (AWSROOT)
    message("- AWS_SDK_ROOT: ${AWSROOT}")
else()
    message(FATAL_ERROR "AWS_SDK_ROOT environment variable is required to compile ${PROJECT_NAME}")
endif()

if (CMAKE_GENERATOR MATCHES "Visual Studio")
    set(AWSROOT ${AWSROOT}/msvc64)

elseif(CMAKE_GENERATOR MATCHES "MinGW Makefiles")
    set(AWSROOT ${AWSROOT}/mingw64)

elseif (${CMAKE_CXX_COMPILER_ID} STREQUAL "GNU")
    set(AWSROOT ${AWSROOT}/gcc)

else()
    message(FATAL_ERROR "Unknown compiler")

endif()

set(AWSROOT ${AWSROOT}/Release)
set(CMAKE_PREFIX_PATH "${AWSROOT}/lib/cmake" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWSROOT}/lib/aws-crt-cpp" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWSROOT}/lib/aws-c-http" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWSROOT}/lib/aws-c-io" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWSROOT}/lib/aws-c-common" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWSROOT}/lib/aws-c-cal" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWSROOT}/lib/aws-c-compression" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWSROOT}/lib/aws-c-mqtt" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWSROOT}/lib/aws-c-auth" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWSROOT}/lib/aws-c-sdkutils" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWSROOT}/lib/aws-c-event-stream" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWSROOT}/lib/aws-checksums" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "${AWSROOT}/lib/aws-c-s3" ${CMAKE_PREFIX_PATH})

if (${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
    set(CMAKE_PREFIX_PATH "${AWSROOT}/lib/s2n" ${CMAKE_PREFIX_PATH})
endif()

find_package(AWSSDK REQUIRED COMPONENTS s3)

add_executable(${PROJECT_NAME} hello.cpp)
target_link_libraries(${PROJECT_NAME} ${AWSSDK_LINK_LIBRARIES})
