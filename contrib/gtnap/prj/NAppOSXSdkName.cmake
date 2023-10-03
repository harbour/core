
# Get macOS name from SDK
# https://ss64.com/osx/sw_vers.html

function(nap_macos_sdk_name sdkVersion _ret)
    # 13 = "Ventura"
    if (sdkVersion VERSION_GREATER "12.9999")
        set(${_ret} "Ventura" PARENT_SCOPE)

    # 12 = "Monterey"
    elseif (sdkVersion VERSION_GREATER "11.9999")
        set(${_ret} "Monterey" PARENT_SCOPE)

    # 11 = "Big Sur"
    elseif (sdkVersion VERSION_GREATER "10.9999")
        set(${_ret} "Big Sur" PARENT_SCOPE)

    # 10.15 = "Catalina"
    elseif (sdkVersion VERSION_GREATER "10.14.9999")
        set(${_ret} "Catalina" PARENT_SCOPE)

    # 10.14 = "Mojave"
    elseif (${sdkVersion} VERSION_GREATER "10.13.9999")
        set(${_ret} "Mojave" PARENT_SCOPE)

    # 10.13 = "High Sierra"
    elseif (${sdkVersion} VERSION_GREATER "10.12.9999")
        set(${_ret} "High Sierra" PARENT_SCOPE)

    # 10.12 = "Sierra"
    elseif (${sdkVersion} VERSION_GREATER "10.11.9999")
        set(${_ret} "Sierra" PARENT_SCOPE)

    # 10.11 = "El Capitan"
    elseif (${sdkVersion} VERSION_GREATER "10.10.9999")
        set(${_ret} "El Capitan" PARENT_SCOPE)

    # 10.10 = "Yosemite"
    elseif (${sdkVersion} VERSION_GREATER "10.9.9999")
        set(${_ret} "Yosemite" PARENT_SCOPE)

    # 10.9 = "Mavericks"
    elseif (${sdkVersion} VERSION_GREATER "10.8.9999")
        set(${_ret} "Mavericks" PARENT_SCOPE)

    # 10.8 = "Mountian Lion"
    elseif (${sdkVersion} VERSION_GREATER "10.7.9999")
        set(${_ret} "Mountian Lion" PARENT_SCOPE)

    # 10.7 = "Lion"
    elseif (${sdkVersion} VERSION_GREATER "10.6.9999")
        set(${_ret} "Lion" PARENT_SCOPE)

    # 10.6 = "Snow Leopard"
    elseif (${sdkVersion} VERSION_GREATER "10.5.9999")
        set(${_ret} "Snow Leopard" PARENT_SCOPE)

    else()
        message(FATAL_ERROR "Unsupported macOS version: ${sdkVersion}")

    endif()

endfunction()
