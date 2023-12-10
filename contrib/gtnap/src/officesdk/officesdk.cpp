#include "officesdk.h"
#include <osapp/osapp.h>
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
#include <container/XNamed.hpp>
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
#include <table/XTable.hpp>
#include <table/XTableColumns.hpp>
#include <table/CellHoriJustify.hpp>
#include <table/CellVertJustify2.hpp>
#include <util/XMergeable.hpp>
#include <util/XProtectable.hpp>
#include <awt/FontSlant.hpp>
#include <awt/FontWeight.hpp>
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
        connect = "soffice \"--accept=socket,host=localhost,port=2083;urp;StarOffice.ServiceManager\" --nodefault --nologo";
        break;
    case ekLINUX:
        connect = "libreoffice \"--accept=socket,host=0,port=2083;urp;\" --nodefault --nologo";
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
        css::uno::Reference<css::bridge::XUnoUrlResolver> xResolver = css::uno::Reference<css::bridge::XUnoUrlResolver>(xInterface, css::uno::UNO_QUERY_THROW);
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
    sdkres_t res = ekSDKRES_OK;
    try
    {
        // https://wiki.openoffice.org/wiki/ES/Manuales/GuiaAOO/TemasAvanzados/Macros/StarBasic/TrabajandoConOOo/TrabajandoConDocumentos
        css::uno::Sequence<css::beans::PropertyValue> loadProperties(1);
        loadProperties[0].Name = "Hidden";
        loadProperties[0].Value <<= true;
        css::uno::Reference<css::lang::XComponent> xComponent = this->xComponentLoader->loadComponentFromURL("private:factory/scalc", "_blank", 0, loadProperties);
        xDocument = css::uno::Reference<css::sheet::XSpreadsheetDocument>(xComponent, css::uno::UNO_QUERY_THROW);
    }
    catch(css::uno::Exception &e)
    {
        std::cout << "Error creating new spreadsheet: " << e.Message;
        res = ekSDKRES_CREATE_FILE_ERROR;
    }

    return res;
}

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
    case ekSDKRES_CREATE_FILE_ERROR:
        return "Failed to create a new file";
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
    case ekSDKRES_ACCESS_COLUMN_ERROR:
        return "Failed to access column";
    case ekSDKRES_FORMAT_COLUMN_ERROR:
        return "Failed to change column format";

    default:
        return "Unknown error";
    }
}

/*---------------------------------------------------------------------------*/

void officesdk_browse_doc(const char_t *pathname, sdkres_t *err)
{
    osapp_browse_file(pathname);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

uint32_t officesdk_rgb(const uint8_t red, const uint8_t green, const uint8_t blue)
{
    return (uint32_t)((0 << 24) | (red << 16) | (green << 8) | (blue));
}

/*---------------------------------------------------------------------------*/

Sheet *officesdk_sheet_open(const char_t *pathname, sdkres_t *err)
{
    sdkres_t res = i_OFFICE_SDK.Init();
    Sheet *sheet = NULL;
    css::uno::Reference<css::sheet::XSpreadsheetDocument> *xDocument = nullptr;
    cassert_no_null(pathname);
    if (res == ekSDKRES_OK)
    {
        xDocument = new css::uno::Reference<css::sheet::XSpreadsheetDocument>();
        res = i_OFFICE_SDK.OpenSheetDocument(pathname, *xDocument);
    }

    if (res == ekSDKRES_OK)
    {
        sheet = reinterpret_cast<Sheet*>(xDocument);
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
    return sheet;
}

/*---------------------------------------------------------------------------*/

Sheet *officesdk_sheet_create(sdkres_t *err)
{
    sdkres_t res = i_OFFICE_SDK.Init();
    Sheet *sheet = NULL;
    css::uno::Reference<css::sheet::XSpreadsheetDocument> *xDocument = nullptr;
    if (res == ekSDKRES_OK)
    {
        xDocument = new css::uno::Reference<css::sheet::XSpreadsheetDocument>();
        res = i_OFFICE_SDK.CreateSheetDocument(*xDocument);
    }

    if (res == ekSDKRES_OK)
    {
        sheet = reinterpret_cast<Sheet*>(xDocument);
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
    return sheet;
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_save(Sheet *sheet, const char_t *pathname, sdkres_t *err)
{
    sdkres_t res = i_OFFICE_SDK.Init();
    cassert_no_null(sheet);
    cassert_no_null(pathname);
    
    if (res == ekSDKRES_OK)
    {
        css::uno::Reference<css::sheet::XSpreadsheetDocument> *xDocument = reinterpret_cast<css::uno::Reference<css::sheet::XSpreadsheetDocument>*>(sheet);
        res = i_OFFICE_SDK.SaveSheetDocument(*xDocument, pathname, ekFORMAT_OPEN_OFFICE);
    }

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_close(Sheet *sheet, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    try
    {
        css::uno::Reference<css::sheet::XSpreadsheetDocument> *xDocument = reinterpret_cast<css::uno::Reference<css::sheet::XSpreadsheetDocument>*>(sheet);
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
                    Sheet *sheet, 
                    uint32_t page, 
                    css::uno::Reference<css::sheet::XSpreadsheet> &xSheet)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        css::uno::Reference<css::sheet::XSpreadsheetDocument> *xDocument = reinterpret_cast<css::uno::Reference<css::sheet::XSpreadsheetDocument>*>(sheet);
        css::uno::Reference<css::sheet::XSpreadsheets> xSheets = (*xDocument)->getSheets();
        css::uno::Reference<css::container::XIndexAccess> xIndexAccess(xSheets, css::uno::UNO_QUERY_THROW);
        css::uno::Any item = xIndexAccess->getByIndex((sal_Int32)page);
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
                    uint32_t col,
                    uint32_t row,
                    css::uno::Reference<css::table::XCell> &xCell)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        xCell = xSheet->getCellByPosition((sal_Int32)col, (sal_Int32)row);
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_ACCESS_DOC_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_get_range(
                    css::uno::Reference<css::sheet::XSpreadsheet> &xSheet,
                    uint32_t st_col,
                    uint32_t st_row,
                    uint32_t ed_col,
                    uint32_t ed_row,
                    css::uno::Reference<css::table::XCellRange> &xCellRange) 
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        xCellRange = xSheet->getCellRangeByPosition((sal_Int32)st_col, (sal_Int32)st_row, (sal_Int32)ed_col, (sal_Int32)ed_row);
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_ACCESS_DOC_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_get_column(
                    css::uno::Reference<css::sheet::XSpreadsheet> &xSheet,
                    uint32_t col,
                    css::uno::Reference<css::beans::XPropertySet> &xTableCol)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        css::uno::Reference<css::table::XColumnRowRange> xRange(xSheet, css::uno::UNO_QUERY_THROW);
        css::uno::Reference<css::table::XTableColumns> xColumns = xRange->getColumns();
        css::uno::Any item = xColumns->getByIndex((sal_Int32)col);
        xTableCol = css::uno::Reference<css::beans::XPropertySet>(item, css::uno::UNO_QUERY_THROW);
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_ACCESS_COLUMN_ERROR;
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

static sdkres_t i_set_cell_value(
                    css::uno::Reference<css::table::XCell> &xCell,
                    const real64_t value)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        xCell->setValue((double)value);
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_EDIT_CELL_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_set_cell_text_property(
                    css::uno::Reference<css::table::XCell> &xCell,
                    const char_t *prop_name,
                    const css::uno::Any &value)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        css::uno::Reference<css::text::XText> xText(xCell, css::uno::UNO_QUERY_THROW);
        css::uno::Reference<css::text::XTextCursor> xTextCursor = xText->createTextCursor();
        css::uno::Reference<css::beans::XPropertySet> xCursorProps = css::uno::Reference<css::beans::XPropertySet>::query(xTextCursor);

        // Select all content in cell
        xTextCursor->gotoStart(false);
        xTextCursor->gotoEnd(true);
        
        ::rtl::OUString prop = i_OUStringFromUTF8(prop_name);
        xCursorProps->setPropertyValue(prop, value);
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_EDIT_CELL_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_set_cell_property(
                    css::uno::Reference<css::table::XCell> &xCell,
                    const char_t *prop_name,
                    const css::uno::Any &value)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        css::uno::Reference<css::beans::XPropertySet> xCellProps = css::uno::Reference<css::beans::XPropertySet>::query(xCell);        
        ::rtl::OUString prop = i_OUStringFromUTF8(prop_name);
        xCellProps->setPropertyValue(prop, value);
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_EDIT_CELL_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_set_column_property(
                    css::uno::Reference<css::beans::XPropertySet> &xTableCol,
                    const char_t *prop_name,
                    const css::uno::Any &value)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        ::rtl::OUString prop = i_OUStringFromUTF8(prop_name);
        xTableCol->setPropertyValue(prop, value);
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_FORMAT_COLUMN_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_doc_cell(
                        Sheet *sheet, 
                        const uint32_t page, 
                        const uint32_t col, 
                        const uint32_t row, 
                        css::uno::Reference<css::table::XCell> &xCell)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::sheet::XSpreadsheet> xSheet;

    if (res == ekSDKRES_OK)
        res = i_get_sheet(sheet, page, xSheet);

    if (res == ekSDKRES_OK)
        res = i_get_cell(xSheet, col, row, xCell);

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_doc_range(
                        Sheet *sheet, 
                        const uint32_t page, 
                        const uint32_t st_col, 
                        const uint32_t st_row, 
                        const uint32_t ed_col, 
                        const uint32_t ed_row, 
                        css::uno::Reference<css::table::XCellRange> &xCellRange) 
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::sheet::XSpreadsheet> xSheet;

    if (res == ekSDKRES_OK)
        res = i_get_sheet(sheet, page, xSheet);

    if (res == ekSDKRES_OK)
        res = i_get_range(xSheet, st_col, st_row, ed_col, ed_row, xCellRange);

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_doc_column(
                        Sheet *sheet, 
                        const uint32_t page, 
                        const uint32_t col, 
                        css::uno::Reference<css::beans::XPropertySet> &xTableCol)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::sheet::XSpreadsheet> xSheet;

    if (res == ekSDKRES_OK)
        res = i_get_sheet(sheet, page, xSheet);

    if (res == ekSDKRES_OK)
        res = i_get_column(xSheet, col, xTableCol);

    return res;
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_name(Sheet *sheet, const uint32_t page, const char_t *name, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::sheet::XSpreadsheet> xSheet;

    if (res == ekSDKRES_OK)
        res = i_get_sheet(sheet, page, xSheet);
    
    if (res == ekSDKRES_OK)
    {
        try 
        {
            css::uno::Reference<css::container::XNamed> xNamed = css::uno::Reference<css::container::XNamed>(xSheet, css::uno::UNO_QUERY_THROW);
            ::rtl::OUString str = i_OUStringFromUTF8(name);
            xNamed->setName(str);
        }
        catch (css::uno::Exception&)
        {
            res = ekSDKRES_ACCESS_DOC_ERROR;
        }
    }

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_protect(Sheet *sheet, const uint32_t page, const bool_t protect, const char_t *pass, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::sheet::XSpreadsheet> xSheet;

    if (res == ekSDKRES_OK)
        res = i_get_sheet(sheet, page, xSheet);
    
    if (res == ekSDKRES_OK)
    {
        try 
        {
            css::uno::Reference<css::util::XProtectable> xProtect = css::uno::Reference<css::util::XProtectable>(xSheet, css::uno::UNO_QUERY_THROW);
            ::rtl::OUString spass = i_OUStringFromUTF8(pass);
            if (protect == TRUE)
                xProtect->protect(spass);
            else
                xProtect->unprotect(spass);
        }
        catch (css::uno::Exception&)
        {
            res = ekSDKRES_ACCESS_DOC_ERROR;
        }
    }

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_text(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const char_t *text, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::table::XCell> xCell;

    res = i_doc_cell(sheet, page, col, row, xCell);

    if (res == ekSDKRES_OK)
        res = i_set_cell_text(xCell, text);

    // More over text handing...
    // https://wiki.openoffice.org/wiki/Documentation/DevGuide/FirstSteps/Common_Mechanisms_for_Text,_Tables_and_Drawings

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_value(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const real64_t value, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::table::XCell> xCell;

    res = i_doc_cell(sheet, page, col, row, xCell);

    if (res == ekSDKRES_OK)
        res = i_set_cell_value(xCell, value);

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_font_family(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const char_t *font_family, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::table::XCell> xCell;

    res = i_doc_cell(sheet, page, col, row, xCell);

    if (res == ekSDKRES_OK)
    {
        ::rtl::OUString fname = i_OUStringFromUTF8(font_family);
        res = i_set_cell_text_property(xCell, "CharFontName", css::uno::makeAny(fname));
    }

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_font_size(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const real32_t font_size, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::table::XCell> xCell;

    res = i_doc_cell(sheet, page, col, row, xCell);

    if (res == ekSDKRES_OK)
        res = i_set_cell_text_property(xCell, "CharHeight", css::uno::makeAny(font_size));

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_bold(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t bold, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::table::XCell> xCell;

    res = i_doc_cell(sheet, page, col, row, xCell);

    if (res == ekSDKRES_OK)
    {
        float weight = bold ? css::awt::FontWeight::BOLD : css::awt::FontWeight::LIGHT;
        res = i_set_cell_text_property(xCell, "CharWeight", css::uno::makeAny(weight));
    }

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_italic(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t italic, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::table::XCell> xCell;

    res = i_doc_cell(sheet, page, col, row, xCell);

    if (res == ekSDKRES_OK)
    {
        css::awt::FontSlant slant = italic ? css::awt::FontSlant::FontSlant_ITALIC : css::awt::FontSlant::FontSlant_NONE;
        res = i_set_cell_text_property(xCell, "CharPosture", css::uno::makeAny(slant));
    }

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_halign(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint32_t align, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::table::XCell> xCell;

    res = i_doc_cell(sheet, page, col, row, xCell);

    if (res == ekSDKRES_OK)
    {
        css::table::CellHoriJustify just = css::table::CellHoriJustify::CellHoriJustify_LEFT;
        switch (align) {
        case 0:
            just = css::table::CellHoriJustify::CellHoriJustify_LEFT;
            break;
        case 1:
            just = css::table::CellHoriJustify::CellHoriJustify_CENTER;
            break;
        case 2:
            just = css::table::CellHoriJustify::CellHoriJustify_RIGHT;
            break;
        }

        res = i_set_cell_property(xCell, "HoriJustify", css::uno::makeAny(just));
    }

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_valign(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint32_t align, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::table::XCell> xCell;

    res = i_doc_cell(sheet, page, col, row, xCell);

    if (res == ekSDKRES_OK)
    {
        ::sal_Int32 just = css::table::CellVertJustify2::CENTER;

        switch (align) {
        case 0:
            just = css::table::CellVertJustify2::TOP;
            break;
        case 1:
            just = css::table::CellVertJustify2::CENTER;
            break;
        case 2:
            just = css::table::CellVertJustify2::BOTTOM;
            break;
        }

        res = i_set_cell_property(xCell, "VertJustify", css::uno::makeAny(just));
    }

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_backcolor(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint32_t rgb, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::table::XCell> xCell;

    res = i_doc_cell(sheet, page, col, row, xCell);

    if (res == ekSDKRES_OK)
        res = i_set_cell_property(xCell, "CellBackColor", css::uno::makeAny((sal_uInt32)rgb));

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_merge(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::table::XCellRange> xCellRange;

    res = i_doc_range(sheet, page, st_col, st_row, ed_col, ed_row, xCellRange);

    if (res == ekSDKRES_OK)
    {
        try 
        {
            css::uno::Reference<css::util::XMergeable> xMergeCellRange = css::uno::Reference<css::util::XMergeable>(xCellRange, css::uno::UNO_QUERY_THROW);
            xMergeCellRange->merge(sal_True);
        }
        catch (css::uno::Exception&)
        {
            res = ekSDKRES_EDIT_CELL_ERROR;
        }
    }

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_column_visible(Sheet *sheet, const uint32_t page, const uint32_t col, const bool_t visible, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::beans::XPropertySet> xTableCol;

    res = i_doc_column(sheet, page, col, xTableCol);

    if (res == ekSDKRES_OK)
        res = i_set_column_property(xTableCol, "IsVisible", css::uno::makeAny((sal_Bool)visible));

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_column_optimal_width(Sheet *sheet, const uint32_t page, const uint32_t col, const bool_t optimal_width, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::beans::XPropertySet> xTableCol;

    res = i_doc_column(sheet, page, col, xTableCol);

    if (res == ekSDKRES_OK)
        res = i_set_column_property(xTableCol, "OptimalWidth", css::uno::makeAny((sal_Bool)optimal_width));

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_column_width(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t width, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::beans::XPropertySet> xTableCol;

    res = i_doc_column(sheet, page, col, xTableCol);

    if (res == ekSDKRES_OK)
        res = i_set_column_property(xTableCol, "Width", css::uno::makeAny((sal_Int32)width));

    ptr_assign(err, res);
}
