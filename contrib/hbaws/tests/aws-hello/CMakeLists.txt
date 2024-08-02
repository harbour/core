cmake_minimum_required(VERSION 3.13)
project(hello_s3)

set(CMAKE_CXX_STANDARD 11)

set(CMAKE_PREFIX_PATH "C:\\Users\\Fran\\Desktop\\aws-install\\lib\\cmake" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "C:\\Users\\Fran\\Desktop\\aws-install\\lib\\aws-crt-cpp" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "C:\\Users\\Fran\\Desktop\\aws-install\\lib\\aws-c-http" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "C:\\Users\\Fran\\Desktop\\aws-install\\lib\\aws-c-io" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "C:\\Users\\Fran\\Desktop\\aws-install\\lib\\aws-c-common" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "C:\\Users\\Fran\\Desktop\\aws-install\\lib\\aws-c-cal" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "C:\\Users\\Fran\\Desktop\\aws-install\\lib\\aws-c-compression" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "C:\\Users\\Fran\\Desktop\\aws-install\\lib\\aws-c-mqtt" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "C:\\Users\\Fran\\Desktop\\aws-install\\lib\\aws-c-auth" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "C:\\Users\\Fran\\Desktop\\aws-install\\lib\\aws-c-sdkutils" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "C:\\Users\\Fran\\Desktop\\aws-install\\lib\\aws-c-event-stream" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "C:\\Users\\Fran\\Desktop\\aws-install\\lib\\aws-checksums" ${CMAKE_PREFIX_PATH})
set(CMAKE_PREFIX_PATH "C:\\Users\\Fran\\Desktop\\aws-install\\lib\\aws-c-s3" ${CMAKE_PREFIX_PATH})
#set(AWSSDK_ROOT_DIR "C:/aws-sdk")

find_package(AWSSDK REQUIRED COMPONENTS s3)

add_executable(${PROJECT_NAME} hello.cpp)
target_link_libraries(${PROJECT_NAME} ${AWSSDK_LINK_LIBRARIES})
