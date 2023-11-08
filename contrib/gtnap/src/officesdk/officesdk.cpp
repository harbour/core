#include "officesdk.h"
#include <core/strings.h>
#include <sewer/blib.h>
#include <sewer/cassert.h>

#include <cppuhelper/bootstrap.hxx>
#include <rtl/bootstrap.hxx>
#include <beans/XPropertySet.hpp>
#include <bridge/XUnoUrlResolver.hpp>
#include <frame/Desktop.hpp>
#include <frame/XComponentLoader.hpp>
#include <frame/XStorable.hpp>
#include <lang/XMultiComponentFactory.hpp>
#include <text/XTextDocument.hpp>

class OfficeSdk
{
public:
    OfficeSdk();

    ~OfficeSdk();

    sdkres_t Init();

public:
    bool init;
    String *liboff_path;
    css::uno::Reference<css::uno::XComponentContext> xComponentContext;
};

/*---------------------------------------------------------------------------*/

OfficeSdk::OfficeSdk()
{
    init = false;
    liboff_path = NULL;
}

/*---------------------------------------------------------------------------*/

OfficeSdk::~OfficeSdk()
{
    str_destopt(&liboff_path);
}

/*---------------------------------------------------------------------------*/

sdkres_t OfficeSdk::Init()
{
    if (init == false)
    {
        const char_t *env = blib_getenv("LIBREOFFICE_HOME");
        if (str_empty_c(env) == FALSE)
        {
            String *types = str_printf("file://%s/program/types/offapi.rdb", env);
            String *boots = str_printf("vnd.sun.star.pathname:%s/program/fundamentalrc", env);
            rtl::Bootstrap::set("URE_MORE_TYPES", rtl::OUString(tc(types), str_len(types), RTL_TEXTENCODING_UTF8));
            rtl::Bootstrap::set("URE_BOOTSTRAP", rtl::OUString(tc(boots), str_len(boots), RTL_TEXTENCODING_UTF8));
            str_destroy(&types);
            str_destroy(&boots);
        }
        else
        {
            return ekSDKRES_NOENV;
        }
    }

    return ekSDKRES_OK;
}


/*---------------------------------------------------------------------------*/

static OfficeSdk i_OFFICE_SDK;

/*---------------------------------------------------------------------------*/

sdkres_t officesdk_text_to_pdf(const char_t *src_pathname, const char_t *dest_pathname)
{
    cassert_no_null(src_pathname);
    cassert_no_null(dest_pathname);
    rtl::OUString sDocUrl("file:///home/fran/harbour_nappgui/contrib/gtnap/tests/unotest/test.odt"), sPDFUrl("file:///home/fran/harbour_nappgui/contrib/gtnap/tests/unotest/test.pdf");
    rtl::OUString sConnectionString("uno:socket,host=localhost,port=2083;urp;StarOffice.ServiceManager");
    rtl::Bootstrap::set("URE_MORE_TYPES", "file:///usr/lib/libreoffice/program/types/offapi.rdb");
    rtl::Bootstrap::set("URE_BOOTSTRAP", "vnd.sun.star.pathname:/usr/lib/libreoffice/program/fundamentalrc");

    // if (i_OFFICE_SDK.xComponentContext.get() == nullptr)
    //     return ekSDKRES_NOENV;
    // else
        //return ekSDKRES_OK;
        return ekSDKRES_NOENV;
}

/*---------------------------------------------------------------------------*/

const char_t* officesdk_error(const sdkres_t code)
{
    switch(code) {
    case ekSDKRES_OK:
        return "Ok";
    case ekSDKRES_NOENV:
        return "No environment variable";
    cassert_default();
    }
    return "";
}

/*---------------------------------------------------------------------------*/
