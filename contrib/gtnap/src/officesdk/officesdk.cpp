#include "officesdk.h"
#include <sewer/cassert.h>

#include <cppuhelper/bootstrap.hxx>
// #include <rtl/bootstrap.hxx>
// #include <beans/XPropertySet.hpp>
// #include <bridge/XUnoUrlResolver.hpp>
// #include <frame/Desktop.hpp>
// #include <frame/XComponentLoader.hpp>
// #include <frame/XStorable.hpp>
// #include <lang/XMultiComponentFactory.hpp>
// #include <text/XTextDocument.hpp>

class OfficeSdk
{
public:
    ~OfficeSdk();

public:
    css::uno::Reference<css::uno::XComponentContext> xComponentContext;
};

/*---------------------------------------------------------------------------*/

OfficeSdk::~OfficeSdk()
{

}

/*---------------------------------------------------------------------------*/

static OfficeSdk i_OFFICE_SDK;

/*---------------------------------------------------------------------------*/

sdkres_t officesdk_text_to_pdf(const char_t *src_pathname, const char_t *dest_pathname)
{
    cassert_no_null(src_pathname);
    cassert_no_null(dest_pathname);

    if (i_OFFICE_SDK.xComponentContext.get() == nullptr)
        return ekSDKRES_NOENV;
    else
        return ekSDKRES_OK;
}

/*---------------------------------------------------------------------------*/
