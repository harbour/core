#include "officesdk.h"
#include <core/strings.h>
#include <osbs/bproc.h>
#include <osbs/bthread.h>
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
#include <iostream>

class OfficeSdk
{
public:
    OfficeSdk();

    ~OfficeSdk();

    sdkres_t Init();

    sdkres_t KillLibreOffice();

    sdkres_t WakeUpServer();

    sdkres_t ConnectServer();

    sdkres_t OpenTextDocument(const char_t *url, css::uno::Reference<css::text::XTextDocument> &xDocument);

    sdkres_t SaveTextDocument(const css::uno::Reference<css::text::XTextDocument> &xDocument, const char_t *url, const fileformat_t format);

public:
    bool init;

    css::uno::Reference<css::lang::XMultiComponentFactory> xMultiComponentFactoryClient;

    css::uno::Reference<css::frame::XDesktop2> xComponentLoader;

};

/*---------------------------------------------------------------------------*/

OfficeSdk::OfficeSdk()
{
    init = false;
}

/*---------------------------------------------------------------------------*/

OfficeSdk::~OfficeSdk()
{
    this->KillLibreOffice();

    if (this->xComponentLoader.get() != nullptr)
    {
        this->xComponentLoader->dispose();
        this->xComponentLoader.set(nullptr);
    }
}

/*---------------------------------------------------------------------------*/

static ::rtl::OUString i_OUStringFromString(const String *str)
{
    return rtl::OUString(tc(str), str_len(str), RTL_TEXTENCODING_UTF8);
}

/*---------------------------------------------------------------------------*/

static ::rtl::OUString i_OUStringFromUTF8(const char_t *str)
{
    cassert_no_null(str);
    return rtl::OUString(str, str_len_c(str), RTL_TEXTENCODING_UTF8);
}

/*---------------------------------------------------------------------------*/

sdkres_t OfficeSdk::Init()
{
    sdkres_t res = ekSDKRES_OK;
    if (this->init == false)
    {
        const char_t *env = blib_getenv("LIBREOFFICE_HOME");

        // Check the LIBREOFFICE_HOME environment variable
        if (str_empty_c(env) == TRUE)
            res = ekSDKRES_NO_ENVAR;

        // Apache UNO global variables
        if (res == ekSDKRES_OK)
        {
            String *types = str_printf("file://%s/program/types/offapi.rdb", env);
            String *boots = str_printf("vnd.sun.star.pathname:%s/program/fundamentalrc", env);
            rtl::Bootstrap::set("URE_MORE_TYPES", i_OUStringFromString(types));
            rtl::Bootstrap::set("URE_BOOTSTRAP", i_OUStringFromString(boots));
            str_destroy(&types);
            str_destroy(&boots);
        }

        // Kill a previous instance of LibreOffice
        if (res == ekSDKRES_OK)
            res = KillLibreOffice();

        // WakeUp LibreOffice
        if (res == ekSDKRES_OK)
            res = WakeUpServer();

        // Wait a little to LibreOffice wake up
        if (res == ekSDKRES_OK)
            bthread_sleep(2000);

        // Connect to LibreOffice instance
        if (res == ekSDKRES_OK)
            res = ConnectServer();

        if (res == ekSDKRES_OK)
            this->init = true;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

sdkres_t OfficeSdk::KillLibreOffice()
{
    sdkres_t res = ekSDKRES_OK;
    const char_t *kill = "killall soffice.bin";
    Proc *proc = bproc_exec(kill, NULL);
    if (proc != NULL)
        bproc_close(&proc);
    else
        res = ekSDKRES_PROC_KILL_FAIL;
    return res;
}

/*---------------------------------------------------------------------------*/

sdkres_t OfficeSdk::WakeUpServer()
{
    sdkres_t res = ekSDKRES_OK;
    const char_t *connect = "libreoffice \"--accept=socket,host=0,port=2083;urp;\" --invisible";
    Proc *proc = bproc_exec(connect, NULL);
    if (proc != NULL)
        bproc_close(&proc);
    else
        res = ekSDKRES_PROC_INIT_FAIL;
    return res;
}

/*---------------------------------------------------------------------------*/

sdkres_t OfficeSdk::ConnectServer()
{
    sdkres_t res = ekSDKRES_OK;
    rtl::OUString sConnectionString("uno:socket,host=localhost,port=2083;urp;StarOffice.ServiceManager");
    css::uno::Reference<css::uno::XComponentContext> xComponentContext(cppu::defaultBootstrap_InitialComponentContext());
    css::uno::Reference<css::lang::XMultiComponentFactory> multiComponentFactoryClient(xComponentContext->getServiceManager());
    css::uno::Reference<css::uno::XInterface> xInterface = multiComponentFactoryClient->createInstanceWithContext("com.sun.star.bridge.UnoUrlResolver", xComponentContext);
    this->xMultiComponentFactoryClient = multiComponentFactoryClient;
    try
    {
        css::uno::Reference<css::bridge::XUnoUrlResolver> xResolver = css::uno::Reference<css::bridge::XUnoUrlResolver>(xInterface, css::uno::UNO_QUERY);
        xInterface = css::uno::Reference<css::uno::XInterface>(xResolver->resolve(sConnectionString), css::uno::UNO_QUERY_THROW);
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_CONECT_FAIL;
    }

    // Try to create a component loader
    if (res == ekSDKRES_OK)
    {
        css::uno::Reference<css::beans::XPropertySet> propSet = css::uno::Reference<css::beans::XPropertySet>(xInterface, css::uno::UNO_QUERY);
        propSet->getPropertyValue("DefaultContext") >>= xComponentContext;
        if (this->xComponentLoader.get() != nullptr)
            this->xComponentLoader->dispose();

        this->xComponentLoader = css::frame::Desktop::create(xComponentContext);
        if (this->xComponentLoader.get() == nullptr)
            res = ekSDKRES_COMPONENT_LOADER;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

sdkres_t OfficeSdk::OpenTextDocument(const char_t *url, css::uno::Reference<css::text::XTextDocument> &xDocument)
{
    sdkres_t res = ekSDKRES_OK;
    ::rtl::OUString docUrl =  "file://" + i_OUStringFromUTF8(url);

    try
    {
        css::uno::Sequence<css::beans::PropertyValue> loadProperties(1);
        loadProperties[0].Name = "Hidden";
        loadProperties[0].Value <<= true;
        css::uno::Reference<css::lang::XComponent> xComponent = this->xComponentLoader->loadComponentFromURL(docUrl, "_blank", 0, loadProperties);
        xDocument = css::uno::Reference<css::text::XTextDocument>(xComponent, css::uno::UNO_QUERY_THROW);
    }
    catch(css::uno::Exception &e)
    {
        std::cout << "Error loading " << url << " file: " << e.Message;
        res = ekSDKRES_OPEN_FILE_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

sdkres_t OfficeSdk::SaveTextDocument(const css::uno::Reference<css::text::XTextDocument> &xDocument, const char_t *url, const fileformat_t format)
{
    sdkres_t res = ekSDKRES_OK;
    ::rtl::OUString docUrl = "file://" + i_OUStringFromUTF8(url);

    try
    {
        css::uno::Reference<css::frame::XStorable> xStorable = css::uno::Reference<css::frame::XStorable>(xDocument, css::uno::UNO_QUERY_THROW);
        css::uno::Sequence<css::beans::PropertyValue> storeProps;

        switch (format) {
        case ekFORMAT_PDF:
        {
            css::uno::Sequence<css::beans::PropertyValue> pdfProps = css::uno::Sequence<css::beans::PropertyValue>(3);
            pdfProps[0].Name = "FilterName";
            pdfProps[0].Value <<= rtl::OUString("writer_pdf_Export");
            pdfProps[1].Name = "Overwrite";
            pdfProps[1].Value <<= true;
            pdfProps[2].Name = "SelectPdfVersion";
            pdfProps[2].Value <<= 1;
            storeProps = pdfProps;
            break;
        }

        cassert_default();
        }

        xStorable->storeToURL(docUrl, storeProps);
    }
    catch(css::uno::Exception &e)
    {
        std::cout << "Error saving " << url << " file: " << e.Message;
        res = ekSDKRES_SAVE_FILE_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static OfficeSdk i_OFFICE_SDK;

/*---------------------------------------------------------------------------*/

void officesdk_finish(void)
{
    i_OFFICE_SDK.~OfficeSdk();
}

/*---------------------------------------------------------------------------*/

sdkres_t officesdk_text_to_pdf(const char_t *src_pathname, const char_t *dest_pathname)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::text::XTextDocument> xDocument;
    cassert_no_null(src_pathname);
    cassert_no_null(dest_pathname);
    res = i_OFFICE_SDK.Init();
    if (res == ekSDKRES_OK)
        res = i_OFFICE_SDK.OpenTextDocument(src_pathname, xDocument);

    if (res == ekSDKRES_OK)
        res = i_OFFICE_SDK.SaveTextDocument(xDocument, dest_pathname, ekFORMAT_PDF);

    if (xDocument.get() != nullptr)
        xDocument->dispose();

    return res;
}

/*---------------------------------------------------------------------------*/

const char_t* officesdk_error(const sdkres_t code)
{
    switch(code) {
    case ekSDKRES_OK:
        return "Ok";
    case ekSDKRES_NO_ENVAR:
        return "No 'LIBREOFFICE_HOME' environment variable";
    case ekSDKRES_PROC_KILL_FAIL:
        return "Failed to kill LibreOffice server";
    case ekSDKRES_PROC_INIT_FAIL:
        return "Failed to launch LibreOffice server";
    case ekSDKRES_CONECT_FAIL:
        return "Failed to connect LibreOffice server";
    case ekSDKRES_COMPONENT_LOADER:
        return "Failed to create a component loader";
    case ekSDKRES_OPEN_FILE_ERROR:
        return "Failed to open file";
    case ekSDKRES_SAVE_FILE_ERROR:
        return "Failed to save file";
    cassert_default();
    }
    return "";
}

/*---------------------------------------------------------------------------*/
