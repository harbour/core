#include "officesdk.h"
#include <core/strings.h>
#include <osbs/bproc.h>
#include <osbs/bthread.h>
#include <osbs/osbs.h>
#include <sewer/blib.h>
#include <sewer/cassert.h>
#include <sewer/ptr.h>

#include <sewer/nowarn.hxx>
#include <cppuhelper/bootstrap.hxx>
#include <rtl/bootstrap.hxx>
#include <beans/XPropertySet.hpp>
#include <bridge/XUnoUrlResolver.hpp>
#include <frame/Desktop.hpp>
#include <frame/XComponentLoader.hpp>
#include <frame/XStorable.hpp>
#include <lang/XMultiComponentFactory.hpp>
#include <text/XTextDocument.hpp>
#include <sheet/XSpreadsheetDocument.hpp>
#include <sheet/XSpreadsheet.hpp>
#include <table/XCell.hpp>
#include <awt/FontSlant.hpp>
#include <awt/FontWeight.hpp>

//#include <io/XCloseable.hpp>
#include <iostream>
#include <sewer/warn.hxx>

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

    sdkres_t OpenSheetDocument(const char_t *url, css::uno::Reference<css::sheet::XSpreadsheetDocument> &xDocument);

    sdkres_t CreateSheetDocument(css::uno::Reference<css::sheet::XSpreadsheetDocument> &xDocument);

    sdkres_t SaveTextDocument(const css::uno::Reference<css::text::XTextDocument> &xDocument, const char_t *url, const fileformat_t format);

    sdkres_t SaveSheetDocument(const css::uno::Reference<css::sheet::XSpreadsheetDocument> &xDocument, const char_t *url, const fileformat_t format);

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
    if (this->xComponentLoader.get() != nullptr)
    {
        try
        {
            this->xComponentLoader->dispose();
            this->xComponentLoader.set(nullptr);
        }
        catch (css::uno::Exception&)
        {

        }
    }

    this->KillLibreOffice();
}

/*---------------------------------------------------------------------------*/

static String *i_url_spaces(const char_t *url)
{
    return str_repl(url, " ", "%20", 0);
}

/*---------------------------------------------------------------------------*/

static String *i_file_url(const char_t *url)
{
    String *str = NULL;
    String *curl = NULL;
    cassert_no_null(url);
    if (url[0] == '/' || url[0] == '\\')
        str = str_path(ekLINUX, "file://%s", url);
    else
        str = str_path(ekLINUX, "file:///%s", url);

    curl = i_url_spaces(tc(str));
    str_destroy(&str);
    return curl;
}

/*---------------------------------------------------------------------------*/

static ::rtl::OUString i_OUStringFromString(const String *str)
{
    return rtl::OUString(tc(str), (sal_Int32)str_len(str), RTL_TEXTENCODING_UTF8);
}

/*---------------------------------------------------------------------------*/

static ::rtl::OUString i_OUStringFromUTF8(const char_t *str)
{
    cassert_no_null(str);
    return rtl::OUString(str, (sal_Int32)str_len_c(str), RTL_TEXTENCODING_UTF8);
}

/*---------------------------------------------------------------------------*/

static ::rtl::OUString i_OUStringFileUrl(const char_t *str)
{
    String *url = i_file_url(str);
    ::rtl::OUString ostr = i_OUStringFromString(url);
    str_destroy(&url);
    return ostr;
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
            String *types = NULL;
            String *boots = NULL;

            {
                String *path = str_path(ekLINUX, "%s/program/types/offapi.rdb", env);
                types = i_file_url(tc(path));
                str_destroy(&path);
            }

            {
                String *path = str_path(ekLINUX, "vnd.sun.star.pathname:%s/program/fundamentalrc", env);
                boots = i_url_spaces(tc(path));
                str_destroy(&path);
            }

            rtl::Bootstrap::set("URE_MORE_TYPES", i_OUStringFromString(types));
            rtl::Bootstrap::set("URE_BOOTSTRAP", i_OUStringFromString(boots));
            str_destroy(&types);
            str_destroy(&boots);
        }

        // Kill a previous instance of LibreOffice
        if (res == ekSDKRES_OK)
            res = KillLibreOffice();

        if (res == ekSDKRES_OK)
            bthread_sleep(1000);

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
    const char_t *kill = NULL;
    Proc *proc = NULL;
    platform_t pt = osbs_platform();

    switch(pt) {
    case ekWINDOWS:
        kill = "taskkill /IM \"soffice.bin\" /F";
        break;
    case ekLINUX:
        kill = "killall soffice.bin";
        break;
    case ekIOS:
    case ekMACOS:
    cassert_default();
    }

    proc = bproc_exec(kill, NULL);

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
    const char_t *connect = NULL;
    Proc *proc = NULL;
    platform_t pt = osbs_platform();

    switch(pt) {
    case ekWINDOWS:
        connect = "soffice \"--accept=socket,host=localhost,port=2083;urp;StarOffice.ServiceManager\" --invisible";
        break;
    case ekLINUX:
        connect = "libreoffice \"--accept=socket,host=0,port=2083;urp;\" --invisible";
        break;
    case ekIOS:
    case ekMACOS:
    cassert_default();
    }

    proc = bproc_exec(connect, NULL);

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
        css::uno::Reference<css::beans::XPropertySet> propSet = css::uno::Reference<css::beans::XPropertySet>(xInterface, css::uno::UNO_QUERY_THROW);
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
    ::rtl::OUString docUrl = i_OUStringFileUrl(url);

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

sdkres_t OfficeSdk::OpenSheetDocument(const char_t *url, css::uno::Reference<css::sheet::XSpreadsheetDocument> &xDocument)
{
    sdkres_t res = ekSDKRES_OK;
    ::rtl::OUString docUrl = i_OUStringFileUrl(url);

    try
    {
        css::uno::Sequence<css::beans::PropertyValue> loadProperties(1);
        loadProperties[0].Name = "Hidden";
        loadProperties[0].Value <<= true;
        css::uno::Reference<css::lang::XComponent> xComponent = this->xComponentLoader->loadComponentFromURL(docUrl, "_blank", 0, loadProperties);
        xDocument = css::uno::Reference<css::sheet::XSpreadsheetDocument>(xComponent, css::uno::UNO_QUERY_THROW);
    }
    catch(css::uno::Exception &e)
    {
        std::cout << "Error loading " << url << " file: " << e.Message;
        res = ekSDKRES_OPEN_FILE_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

sdkres_t OfficeSdk::CreateSheetDocument(css::uno::Reference<css::sheet::XSpreadsheetDocument> &xDocument)
{
    unref(xDocument);
    return ekSDKRES_NO_ENVAR;
    // try
    // {
    //     css::uno::Sequence<css::beans::PropertyValue> loadProperties(1);
    //     loadProperties[0].Name = "Hidden";
    //     loadProperties[0].Value <<= true;
    //     css::uno::Reference<css::lang::XComponent> xComponent = this->xComponentLoader->loadComponentFromURL(docUrl, "_blank", 0, loadProperties);
    //     xDocument = css::uno::Reference<css::sheet::XSpreadsheetDocument>(xComponent, css::uno::UNO_QUERY_THROW);
    // }
    // catch(css::uno::Exception &e)
    // {
    //     std::cout << "Error loading " << url << " file: " << e.Message;
    //     res = ekSDKRES_OPEN_FILE_ERROR;
    // }

}
// Reference<XComponentLoader> xComponentLoader(xServiceManager, UNO_QUERY);
// OUString docServiceName = OUString::createFromAscii("com.sun.star.sheet.SpreadsheetDocument");
// Reference<XInterface> xComponent = xComponentLoader->createInstanceWithArgumentsAndContext(docServiceName, Sequence<Any>(), xContext);
// Reference<XSpreadsheetDocument> xCalcDocument(xComponent, UNO_QUERY)



/*---------------------------------------------------------------------------*/

sdkres_t OfficeSdk::SaveTextDocument(const css::uno::Reference<css::text::XTextDocument> &xDocument, const char_t *url, const fileformat_t format)
{
    sdkres_t res = ekSDKRES_OK;
    ::rtl::OUString docUrl = i_OUStringFileUrl(url);

    try
    {
        css::uno::Reference<css::frame::XStorable> xStorable = css::uno::Reference<css::frame::XStorable>(xDocument, css::uno::UNO_QUERY_THROW);
        css::uno::Sequence<css::beans::PropertyValue> storeProps;

        switch (format) {
        case ekFORMAT_OPEN_OFFICE:
        {
            css::uno::Sequence<css::beans::PropertyValue> ofProps = css::uno::Sequence<css::beans::PropertyValue>(1);
            ofProps[0].Name = "Overwrite";
            ofProps[0].Value <<= true;
            storeProps = ofProps;
            break;
        }

        case ekFORMAT_PDF:
        {
            css::uno::Sequence<css::beans::PropertyValue> pdfProps = css::uno::Sequence<css::beans::PropertyValue>(3);
            pdfProps[0].Name = "FilterName";
            pdfProps[0].Value <<= rtl::OUString("writer_pdf_Export");
            pdfProps[1].Name = "Overwrite";
            pdfProps[1].Value <<= true;
            pdfProps[2].Name = "SelectPdfVersion";
            pdfProps[2].Value <<= (sal_uInt32)1;
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

sdkres_t OfficeSdk::SaveSheetDocument(const css::uno::Reference<css::sheet::XSpreadsheetDocument> &xDocument, const char_t *url, const fileformat_t format)
{
    sdkres_t res = ekSDKRES_OK;
    ::rtl::OUString docUrl = i_OUStringFileUrl(url);

    try
    {
        css::uno::Reference<css::frame::XStorable> xStorable = css::uno::Reference<css::frame::XStorable>(xDocument, css::uno::UNO_QUERY_THROW);
        css::uno::Sequence<css::beans::PropertyValue> storeProps;

        switch (format) {
        case ekFORMAT_OPEN_OFFICE:
        {
            css::uno::Sequence<css::beans::PropertyValue> ofProps = css::uno::Sequence<css::beans::PropertyValue>(1);
            ofProps[0].Name = "Overwrite";
            ofProps[0].Value <<= true;
            storeProps = ofProps;
            break;
        }

        case ekFORMAT_PDF:
        {
            css::uno::Sequence<css::beans::PropertyValue> pdfProps = css::uno::Sequence<css::beans::PropertyValue>(2);
            pdfProps[0].Name = "FilterName";
            pdfProps[0].Value <<= rtl::OUString("calc_pdf_Export");
            pdfProps[1].Name = "Overwrite";
            pdfProps[1].Value <<= true;
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
    case ekSDKRES_CLOSE_DOC_ERROR:
        return "Failed to close document";
    case ekSDKRES_ACCESS_DOC_ERROR:
        return "Failed to access document";
    case ekSDKRES_ACCESS_CELL_ERROR:
        return "Failed to access cell";
    case ekSDKRES_EDIT_CELL_ERROR:
        return "Failed to edit cell content";
    case ekSDKRES_FORMAT_CELL_ERROR:
        return "Failed to change cell format";
    default:
        return "Unknown error";
    }
}

/*---------------------------------------------------------------------------*/

SheetDoc *officesdk_sheetdoc_open(const char_t *pathname, sdkres_t *err)
{
    sdkres_t res = i_OFFICE_SDK.Init();
    SheetDoc *doc = NULL;
    css::uno::Reference<css::sheet::XSpreadsheetDocument> *xDocument = nullptr;
    cassert_no_null(pathname);
    if (res == ekSDKRES_OK)
    {
        xDocument = new css::uno::Reference<css::sheet::XSpreadsheetDocument>();
        res = i_OFFICE_SDK.OpenSheetDocument(pathname, *xDocument);
    }

    if (res == ekSDKRES_OK)
    {
        doc = reinterpret_cast<SheetDoc*>(xDocument);
    }
    else
    {
        if (xDocument != nullptr)
        {
            delete xDocument;
            xDocument = nullptr;
        }
    }

    ptr_assign(err, res);
    return doc;
}

/*---------------------------------------------------------------------------*/

SheetDoc *officesdk_sheetdoc_new(sdkres_t *err)
{
    unref(err);
    return NULL;
}

/*---------------------------------------------------------------------------*/

void officesdk_sheetdoc_save(SheetDoc *doc, const char_t *pathname, sdkres_t *err)
{
    sdkres_t res = i_OFFICE_SDK.Init();
    cassert_no_null(doc);
    cassert_no_null(pathname);
    
    if (res == ekSDKRES_OK)
    {
        css::uno::Reference<css::sheet::XSpreadsheetDocument> *xDocument = reinterpret_cast<css::uno::Reference<css::sheet::XSpreadsheetDocument>*>(doc);
        res = i_OFFICE_SDK.SaveSheetDocument(*xDocument, pathname, ekFORMAT_OPEN_OFFICE);
    }

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheetdoc_close(SheetDoc *doc, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    try
    {
        css::uno::Reference<css::sheet::XSpreadsheetDocument> *xDocument = reinterpret_cast<css::uno::Reference<css::sheet::XSpreadsheetDocument>*>(doc);
        css::uno::Reference<css::lang::XComponent> xComponent = css::uno::Reference<css::lang::XComponent>(*xDocument, css::uno::UNO_QUERY_THROW);
        // This remove the lock file
        xComponent->dispose();
        delete xDocument;
        xDocument = nullptr;
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_CLOSE_DOC_ERROR;
    }

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_get_sheet(
                    SheetDoc *doc, 
                    uint32_t sheet_id, 
                    css::uno::Reference<css::sheet::XSpreadsheet> &xSheet)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        css::uno::Reference<css::sheet::XSpreadsheetDocument> *xDocument = reinterpret_cast<css::uno::Reference<css::sheet::XSpreadsheetDocument>*>(doc);
        css::uno::Reference<css::sheet::XSpreadsheets> xSheets = (*xDocument)->getSheets();
        css::uno::Reference<css::container::XIndexAccess> xIndexAccess(xSheets, css::uno::UNO_QUERY_THROW);
        css::uno::Any item = xIndexAccess->getByIndex((sal_Int32)sheet_id);
        xSheet = css::uno::Reference<css::sheet::XSpreadsheet>(item, css::uno::UNO_QUERY_THROW); 
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_ACCESS_DOC_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_get_cell(
                    css::uno::Reference<css::sheet::XSpreadsheet> &xSheet,
                    uint32_t col_id,
                    uint32_t row_id,
                    css::uno::Reference<css::table::XCell> &xCell)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        xCell = xSheet->getCellByPosition((sal_Int32)col_id, (sal_Int32)row_id);
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_ACCESS_DOC_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_set_cell_text(
                    css::uno::Reference<css::table::XCell> &xCell,
                    const char_t *text)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        css::uno::Reference<css::text::XText> xText(xCell, css::uno::UNO_QUERY_THROW);
        ::rtl::OUString str = i_OUStringFromUTF8(text);
        xText->setString(str);
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_EDIT_CELL_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_format_cell(
                    css::uno::Reference<css::table::XCell> &xCell,
                    const char_t *font_family, 
                    const real32_t font_size, 
                    const bool_t bold, 
                    const bool_t italic)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        css::uno::Reference<css::text::XText> xText(xCell, css::uno::UNO_QUERY_THROW);
        css::uno::Reference<css::text::XTextCursor> xTextCursor = xText->createTextCursor();
        css::uno::Reference<css::beans::XPropertySet> xCursorProps = css::uno::Reference<css::beans::XPropertySet>::query(xTextCursor);

        // Select all text in cell
        xTextCursor->gotoStart(false);
        xTextCursor->gotoEnd(true);
        
        // Text properties
        ::rtl::OUString fname = i_OUStringFromUTF8(font_family);
        css::awt::FontSlant slant = italic ? css::awt::FontSlant::FontSlant_ITALIC : css::awt::FontSlant::FontSlant_NONE;
        float weight = bold ? css::awt::FontWeight::BOLD : css::awt::FontWeight::LIGHT;
        xCursorProps->setPropertyValue("CharFontName", css::uno::makeAny(fname));
        xCursorProps->setPropertyValue("CharHeight", css::uno::makeAny(font_size));
        xCursorProps->setPropertyValue("CharPosture", css::uno::makeAny(slant));
        xCursorProps->setPropertyValue("CharWeight", css::uno::makeAny(weight));
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_EDIT_CELL_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

void officesdk_sheetdoc_cell_text(SheetDoc *doc, const uint32_t sheet_id, const uint32_t col, const uint32_t row, const char_t *text, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::sheet::XSpreadsheet> xSheet;
    css::uno::Reference<css::table::XCell> xCell;

    if (res == ekSDKRES_OK)
        res = i_get_sheet(doc, sheet_id, xSheet);

    if (res == ekSDKRES_OK)
        res = i_get_cell(xSheet, col, row, xCell);

    if (res == ekSDKRES_OK)
        res = i_set_cell_text(xCell, text);


    // More over text handing...
    // https://wiki.openoffice.org/wiki/Documentation/DevGuide/FirstSteps/Common_Mechanisms_for_Text,_Tables_and_Drawings

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheetdoc_cell_format(SheetDoc *doc, const uint32_t sheet_id, const uint32_t col, const uint32_t row, const char_t *font_family, const real32_t font_size, const bool_t bold, const bool_t italic, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::sheet::XSpreadsheet> xSheet;
    css::uno::Reference<css::table::XCell> xCell;

    if (res == ekSDKRES_OK)
        res = i_get_sheet(doc, sheet_id, xSheet);

    if (res == ekSDKRES_OK)
        res = i_get_cell(xSheet, col, row, xCell);

    if (res == ekSDKRES_OK)
        res = i_format_cell(xCell, font_family, font_size, bold, italic);

    ptr_assign(err, res);
}
