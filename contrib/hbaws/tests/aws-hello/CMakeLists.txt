cmake_minimum_required(VERSION 3.13)
project(hello_s3)

set(CMAKE_CXX_STANDARD 11)

if (WIN32)
    set(AWS_INSTALL_PATH "C:\\aws-sdk")
else()
    set(AWS_INSTALL_PATH "/home/fran/Desktop/aws-install")
endif()


#set(CMAKE_IGNORE_PATH "C:\\msys64\\mingw64\\lib\\cmake")
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

# if (WIN32)
# else()
#     set(CMAKE_PREFIX_PATH "${AWS_INSTALL_PATH}/lib/s2n" ${CMAKE_PREFIX_PATH})
# endif()

find_package(AWSSDK REQUIRED COMPONENTS s3)

message("EHHHHHHHHHHHHHH: ${AWSSDK_LINK_LIBRARIES}")

# get_cmake_property(_variableNames VARIABLES)
# list (SORT _variableNames)
# foreach (_variableName ${_variableNames})
#     message(STATUS "${_variableName}=${${_variableName}}")
# endforeach()


#AWSSDK_LIB_DIR
add_executable(${PROJECT_NAME} hello.cpp)
target_link_libraries(${PROJECT_NAME} ${AWSSDK_LINK_LIBRARIES})

#set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-cpp-sdk-config.a)
# set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-crt-cpp.a)
# set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-cpp-sdk-s3.a)
# set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-cpp-sdk-core.a)

# set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-c-auth.a)
# set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-c-cal.a)
# set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-c-common.a)
# set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-c-compression.a)
# set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-c-event-stream.a)
# set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-checksums.a)
# set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-c-http.a)
# set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-c-io.a)
# set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-c-mqtt.a)
# set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-cpp-sdk-access-management.a)
# set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-cpp-sdk-cognito-identity.a)
# set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-cpp-sdk-iam.a)
# set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-cpp-sdk-identity-management.a)
# set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-cpp-sdk-kinesis.a)
# set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-cpp-sdk-lambda.a)
# set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-cpp-sdk-sts.a)
# set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-cpp-sdk-transfer.a)
# set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-crt-cpp.a)
# set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-c-s3.a)
# set(AWS_LINK_LIBS ${AWS_LINK_LIBS} ${AWSSDK_LIB_DIR}libaws-c-sdkutils.a)




# message("EHHHHHHHHHHHHHH: ${AWS_LINK_LIBS}")

# target_link_libraries(${PROJECT_NAME} ${AWS_LINK_LIBS})
