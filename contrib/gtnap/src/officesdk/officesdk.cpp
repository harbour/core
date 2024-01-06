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
#include <drawing/XDrawPageSupplier.hpp>
#include <drawing/XDrawPagesSupplier.hpp>
#include <frame/Desktop.hpp>
#include <frame/XComponentLoader.hpp>
#include <frame/XStorable.hpp>
#include <graphic/XGraphicProvider.hpp>
#include <i18n/NumberFormatIndex.hpp>
#include <lang/Locale.hpp>
#include <lang/XMultiComponentFactory.hpp>
#include <lang/XMultiServiceFactory.hpp>
#include <text/XTextDocument.hpp>
#include <sheet/XCellRangeAddressable.hpp>
#include <sheet/XSpreadsheetDocument.hpp>
#include <sheet/XSpreadsheet.hpp>
#include <sheet/XSpreadsheetView.hpp>
#include <sheet/XViewFreezable.hpp>
#include <table/XCell.hpp>
#include <table/XTable.hpp>
#include <table/XTableColumns.hpp>
#include <table/CellHoriJustify.hpp>
#include <table/CellVertJustify2.hpp>
#include <table/TableBorder.hpp>
#include <table/TableBorder2.hpp>
#include <table/BorderLineStyle.hpp>
#include <util/XMergeable.hpp>
#include <util/XProtectable.hpp>
#include <util/XNumberFormatsSupplier.hpp>
#include <util/XNumberFormatTypes.hpp>
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

    sdkres_t LoadImage(const char_t *url, css::uno::Reference<css::graphic::XGraphic> &xGraphic);

    sdkres_t CreateSheetDocument(css::uno::Reference<css::sheet::XSpreadsheetDocument> &xDocument);

    sdkres_t SaveTextDocument(const css::uno::Reference<css::text::XTextDocument> &xDocument, const char_t *url, const fileformat_t format);

    sdkres_t SaveSheetDocument(const css::uno::Reference<css::sheet::XSpreadsheetDocument> &xDocument, const char_t *url, const fileformat_t format);

public:
    bool m_init;

    css::uno::Reference<css::uno::XComponentContext> m_xComponentContext;

    css::uno::Reference<css::lang::XMultiComponentFactory> m_xMultiComponentFactory;

    css::uno::Reference<css::frame::XDesktop2> m_xComponentLoader;    
};

/*---------------------------------------------------------------------------*/

OfficeSdk::OfficeSdk()
{
    this->m_init = false;
}

/*---------------------------------------------------------------------------*/

OfficeSdk::~OfficeSdk()
{
    if (this->m_xComponentLoader.get() != nullptr)
    {
        try
        {
            this->m_xComponentLoader->dispose();
            this->m_xComponentLoader.set(nullptr);
        }
        catch (css::uno::Exception&)
        {

        }
    }

    if (this->m_xMultiComponentFactory.get() != nullptr)
    {
        try
        {
            this->m_xMultiComponentFactory.set(nullptr);
        }
        catch (css::uno::Exception&)
        {

        }
    }

    if (this->m_xComponentContext.get() != nullptr)
    {
        try
        {
            this->m_xComponentContext.set(nullptr);
        }
        catch (css::uno::Exception&)
        {

        }
    }

    this->KillLibreOffice();
    this->m_init = false;
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
    if (this->m_init == false)
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
            this->m_init = true;
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

    try
    {
        rtl::OUString sConnectionString("uno:socket,host=localhost,port=2083;urp;StarOffice.ServiceManager");
        css::uno::Reference<css::uno::XComponentContext> xComponentContext(cppu::bootstrap());
        css::uno::Reference<css::lang::XMultiComponentFactory> xMultiComponentFactory(xComponentContext->getServiceManager());
        css::uno::Reference<css::uno::XInterface> xin = xMultiComponentFactory->createInstanceWithContext("com.sun.star.frame.Desktop", xComponentContext);
        this->m_xComponentLoader = css::uno::Reference<css::frame::XDesktop2>(xin, css::uno::UNO_QUERY_THROW);
        this->m_xMultiComponentFactory = xMultiComponentFactory;
        this->m_xComponentContext = xComponentContext;

        if (this->m_xComponentLoader.get() == nullptr)
            res = ekSDKRES_COMPONENT_LOADER;
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_CONECT_FAIL;
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
        css::uno::Reference<css::lang::XComponent> xComponent = this->m_xComponentLoader->loadComponentFromURL(docUrl, "_blank", 0, loadProperties);
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
        css::uno::Reference<css::lang::XComponent> xComponent = this->m_xComponentLoader->loadComponentFromURL(docUrl, "_blank", 0, loadProperties);
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

sdkres_t OfficeSdk::LoadImage(
                        const char_t *url, 
                        css::uno::Reference<css::graphic::XGraphic> &xGraphic)
{
    sdkres_t res = ekSDKRES_OK;
    try
    {
        css::uno::Reference<css::uno::XInterface> xInterface = this->m_xMultiComponentFactory->createInstanceWithContext("com.sun.star.graphic.GraphicProvider", this->m_xComponentContext); 
        css::uno::Reference<css::graphic::XGraphicProvider> xGraphicProvider(xInterface, css::uno::UNO_QUERY_THROW);       
        css::uno::Sequence<css::beans::PropertyValue> loadProperties(1);
        loadProperties[0].Name = "URL";
        loadProperties[0].Value <<= i_OUStringFileUrl(url);
        xGraphic = xGraphicProvider->queryGraphic(loadProperties);
    }
    catch(css::uno::Exception &e)
    {
        std::cout << "Error loading image: " << e.Message;
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
        css::uno::Reference<css::lang::XComponent> xComponent = this->m_xComponentLoader->loadComponentFromURL("private:factory/scalc", "_blank", 0, loadProperties);
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
    case ekSDKRES_ACCESS_ROW_ERROR:
        return "Failed to access row";
    case ekSDKRES_FORMAT_ROW_ERROR:
        return "Failed to change row format";
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

static sdkres_t i_get_row(
                    css::uno::Reference<css::sheet::XSpreadsheet> &xSheet,
                    uint32_t row,
                    css::uno::Reference<css::beans::XPropertySet> &xTableRow)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        css::uno::Reference<css::table::XColumnRowRange> xRange(xSheet, css::uno::UNO_QUERY_THROW);
        css::uno::Reference<css::table::XTableRows> xRows = xRange->getRows();
        css::uno::Any item = xRows->getByIndex((sal_Int32)row);
        xTableRow = css::uno::Reference<css::beans::XPropertySet>(item, css::uno::UNO_QUERY_THROW);
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

static sdkres_t i_get_column_property(
                    const css::uno::Reference<css::beans::XPropertySet> &xTableCol,
                    const char_t *prop_name,
                    css::uno::Any &value)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        ::rtl::OUString prop = i_OUStringFromUTF8(prop_name);
        value = xTableCol->getPropertyValue(prop);
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_ACCESS_COLUMN_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_get_column_width(
                    css::uno::Reference<css::sheet::XSpreadsheet> &xSheet,
                    const uint32_t column_id,
                    sal_Int32 &width)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::beans::XPropertySet> xTableCol;

    res = i_get_column(xSheet, column_id, xTableCol);
    if (res == ekSDKRES_OK)
    {
        css::uno::Any value;
        res = i_get_column_property(xTableCol, "Width", value);
        value >>= width;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_get_row_property(
                    const css::uno::Reference<css::beans::XPropertySet> &xTableRow,
                    const char_t *prop_name,
                    css::uno::Any &value)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        ::rtl::OUString prop = i_OUStringFromUTF8(prop_name);
        value = xTableRow->getPropertyValue(prop);
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_ACCESS_ROW_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_get_row_height(
                    css::uno::Reference<css::sheet::XSpreadsheet> &xSheet,
                    const uint32_t row_id,
                    sal_Int32 &height)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::beans::XPropertySet> xTableRow;

    res = i_get_row(xSheet, row_id, xTableRow);
    if (res == ekSDKRES_OK)
    {
        css::uno::Any value;
        res = i_get_row_property(xTableRow, "Height", value);
        value >>= height;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_get_range_frame(
                    css::uno::Reference<css::sheet::XSpreadsheet> &xSheet,
                    css::uno::Reference<css::sheet::XCellRangeAddressable> &xRange,
                    sal_Int32 &x,
                    sal_Int32 &y,
                    sal_Int32 &width,
                    sal_Int32 &height)
{
    sdkres_t res = ekSDKRES_OK;
    x = 0;
    y = 0;
    width = 0;
    height = 0;

    try
    {
        css::table::CellRangeAddress range = xRange->getRangeAddress();

        if (res == ekSDKRES_OK)
        {
            for (uint32_t i = 0; i < (uint32_t)range.StartColumn; ++i)
            {
                sal_Int32 lwidth;
                res = i_get_column_width(xSheet, i, lwidth);
                x += lwidth;

                if (res != ekSDKRES_OK)
                    break;
            }
        }

        if (res == ekSDKRES_OK)
        {
            for (uint32_t i = (uint32_t)range.StartColumn; i <= (uint32_t)range.EndColumn; ++i)
            {
                sal_Int32 lwidth;
                res = i_get_column_width(xSheet, i, lwidth);
                width += lwidth;

                if (res != ekSDKRES_OK)
                    break;
            }
        }

        if (res == ekSDKRES_OK)
        {
            for (uint32_t i = 0; i < (uint32_t)range.StartRow; ++i)
            {
                sal_Int32 lheight;
                res = i_get_row_height(xSheet, i, lheight);
                y += lheight;

                if (res != ekSDKRES_OK)
                    break;
            }
        }

        if (res == ekSDKRES_OK)
        {
            for (uint32_t i = (uint32_t)range.StartRow; i <= (uint32_t)range.EndRow; ++i)
            {
                sal_Int32 lheight;
                res = i_get_row_height(xSheet, i, lheight);
                height += lheight;

                if (res != ekSDKRES_OK)
                    break;
            }
        }
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_ACCESS_CELL_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_get_cell_frame(
                    css::uno::Reference<css::sheet::XSpreadsheet> &xSheet,
                    css::uno::Reference<css::table::XCell> &xCell,
                    sal_Int32 &x,
                    sal_Int32 &y,
                    sal_Int32 &width,
                    sal_Int32 &height)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        css::uno::Reference<css::sheet::XCellRangeAddressable> xRange = css::uno::Reference<css::sheet::XCellRangeAddressable>(xCell, css::uno::UNO_QUERY_THROW);
        res = i_get_range_frame(xSheet, xRange, x, y, width, height);
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_ACCESS_CELL_ERROR;
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

static sdkres_t i_set_row_property(
                    css::uno::Reference<css::beans::XPropertySet> &xTableRow,
                    const char_t *prop_name,
                    const css::uno::Any &value)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        ::rtl::OUString prop = i_OUStringFromUTF8(prop_name);
        xTableRow->setPropertyValue(prop, value);
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_FORMAT_ROW_ERROR;
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

static sdkres_t i_doc_row(
                        Sheet *sheet,
                        const uint32_t page,
                        const uint32_t col,
                        css::uno::Reference<css::beans::XPropertySet> &xTableRow)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::sheet::XSpreadsheet> xSheet;

    if (res == ekSDKRES_OK)
        res = i_get_sheet(sheet, page, xSheet);

    if (res == ekSDKRES_OK)
        res = i_get_row(xSheet, col, xTableRow);

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_doc_locale(
                        Sheet *sheet,
                        css::lang::Locale &xLocale)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        css::uno::Reference<css::sheet::XSpreadsheetDocument> *xDocument = reinterpret_cast<css::uno::Reference<css::sheet::XSpreadsheetDocument>*>(sheet);
        css::uno::Reference<css::beans::XPropertySet> xProp = css::uno::Reference<css::beans::XPropertySet>(*xDocument, css::uno::UNO_QUERY_THROW);
        css::uno::Any item = xProp->getPropertyValue("CharLocale");
        item >>= xLocale;
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_ACCESS_DOC_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_doc_number_formats(
                        Sheet *sheet,
                        css::uno::Reference<css::util::XNumberFormatTypes> &xFormats)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        css::uno::Reference<css::sheet::XSpreadsheetDocument> *xDocument = reinterpret_cast<css::uno::Reference<css::sheet::XSpreadsheetDocument>*>(sheet);
        css::uno::Reference<css::util::XNumberFormatsSupplier> xFormatsSupplier(*xDocument, css::uno::UNO_QUERY_THROW);
        css::uno::Reference<com::sun::star::util::XNumberFormats> xNumberFormats = xFormatsSupplier->getNumberFormats();
        xFormats = css::uno::Reference<css::util::XNumberFormatTypes>(xNumberFormats, css::uno::UNO_QUERY_THROW);
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_ACCESS_DOC_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_create_shape(
                        css::uno::Reference<css::frame::XModel> &xModel,
                        const char_t *shapeType, 
                        css::uno::Reference<css::drawing::XShape> &xShape)
{
    sdkres_t res = ekSDKRES_OK;
    try
    {
        ::rtl::OUString type = i_OUStringFromUTF8(shapeType);
        css::uno::Reference<css::lang::XMultiServiceFactory> xServiceFactory(xModel, css::uno::UNO_QUERY_THROW);
        css::uno::Reference<css::uno::XInterface> xInterface = xServiceFactory->createInstance(type);
        xShape = css::uno::Reference<css::drawing::XShape>(xInterface, css::uno::UNO_QUERY_THROW);
    }
    catch(css::uno::Exception &e)
    {
        std::cout << "Error creating new shape: " << e.Message;
        res = ekSDKRES_CREATE_FILE_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_get_draw_page(
                        css::uno::Reference<css::frame::XModel> &xModel,
                        const uint32_t page,
                        css::uno::Reference<css::drawing::XDrawPage> &xDrawPage)
{
    sdkres_t res = ekSDKRES_OK;
    try
    {
        css::uno::Reference<css::drawing::XDrawPagesSupplier> xPagesSupplier = css::uno::Reference<css::drawing::XDrawPagesSupplier>(xModel, css::uno::UNO_QUERY_THROW);
        css::uno::Reference<css::drawing::XDrawPages> xDrawPages = xPagesSupplier->getDrawPages(); 
        css::uno::Any item = xDrawPages->getByIndex((sal_Int32)page);
        xDrawPage = css::uno::Reference<css::drawing::XDrawPage>(item, css::uno::UNO_QUERY_THROW);
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_ACCESS_DOC_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_insert_image(
                        css::uno::Reference<css::frame::XModel> &xModel,
                        const uint32_t page,
                        const char_t *image_path,
                        const sal_Int32 x,
                        const sal_Int32 y,
                        const sal_Int32 width,
                        const sal_Int32 height)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        css::uno::Reference<css::drawing::XDrawPage> xDrawPage;
        css::uno::Reference<css::graphic::XGraphic> xGraphic;
        css::uno::Reference<css::drawing::XShape> xShape;

        // Get a draw page
        res = i_get_draw_page(xModel, page, xDrawPage);

        // Get the image graphic from file
        if (res == ekSDKRES_OK)
            res = i_OFFICE_SDK.LoadImage(image_path, xGraphic);

        // Create the shape
        if (res == ekSDKRES_OK)
            res = i_create_shape(xModel, "com.sun.star.drawing.GraphicObjectShape", xShape);

        // Configure the shape and add the image graphic
        if (res == ekSDKRES_OK)
        {
            css::uno::Reference<css::beans::XPropertySet> xProps(xShape, css::uno::UNO_QUERY_THROW);
            xProps->setPropertyValue("Graphic", css::uno::makeAny(xGraphic));
            xShape->setPosition(css::awt::Point(x, y));
            xShape->setSize(css::awt::Size(width, height));
        }

        // Finally, we draw the shape in the page
        xDrawPage->add(xShape);
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_ACCESS_DOC_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

uint32_t officesdk_sheet_add(Sheet *sheet, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    uint32_t id = UINT32_MAX;
    String *defname = NULL;

    try
    {
        css::uno::Reference<css::sheet::XSpreadsheetDocument> *xDocument = reinterpret_cast<css::uno::Reference<css::sheet::XSpreadsheetDocument>*>(sheet);
        css::uno::Reference<css::sheet::XSpreadsheets> xSheets = (*xDocument)->getSheets();
        css::uno::Reference<css::container::XIndexAccess> xIndexAccess(xSheets, css::uno::UNO_QUERY_THROW);
        sal_Int32 n = xIndexAccess->getCount();
        String *defname = str_printf("Sheet%d", n);
        xSheets->insertNewByName(i_OUStringFromString(defname), (sal_Int16)n);
        id = (uint32_t)n;
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_ACCESS_DOC_ERROR;
    }

    str_destopt(&defname);
    ptr_assign(err, res);
    return id;
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

void officesdk_sheet_freeze(Sheet *sheet, const uint32_t page, const uint32_t ncols, const uint32_t nrows, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::sheet::XSpreadsheet> xSheet;

    if (res == ekSDKRES_OK)
        res = i_get_sheet(sheet, page, xSheet);

    if (res == ekSDKRES_OK)
    {
        try
        {
            css::uno::Reference<css::sheet::XSpreadsheetView> xView(xSheet, css::uno::UNO_QUERY_THROW);
            css::uno::Reference<css::sheet::XViewFreezable> xFreezable(xView, css::uno::UNO_QUERY_THROW);
            xFreezable->freezeAtPosition((sal_Int32)ncols, (sal_Int32)nrows);
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

void officesdk_sheet_cell_numformat(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const numformat_t format, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::table::XCell> xCell;
    css::lang::Locale xLocale;
    css::uno::Reference<css::util::XNumberFormatTypes> xFormats;

    res = i_doc_cell(sheet, page, col, row, xCell);

    if (res == ekSDKRES_OK)
        res = i_doc_locale(sheet, xLocale);

    if (res == ekSDKRES_OK)
        res = i_doc_number_formats(sheet, xFormats);

    if (res == ekSDKRES_OK)
    {
        sal_Int16 nformat = css::i18n::NumberFormatIndex::NUMBER_STANDARD;
        long formatIndex = 0;

        switch (format) {
        case ekNUMFORMAT_INT:
            nformat = css::i18n::NumberFormatIndex::NUMBER_INT;
            break;
        case ekNUMFORMAT_INT_1000:
            nformat = css::i18n::NumberFormatIndex::NUMBER_1000INT;
            break;
        case ekNUMFORMAT_DEC2:
            nformat = css::i18n::NumberFormatIndex::NUMBER_DEC2;
            break;
        case ekNUMFORMAT_DEC2_1000:
            nformat = css::i18n::NumberFormatIndex::NUMBER_1000DEC2;
            break;
        case ekNUMFORMAT_PERC_INT:
            nformat = css::i18n::NumberFormatIndex::PERCENT_INT;
            break;
        case ekNUMFORMAT_PERC_DEC2:
            nformat = css::i18n::NumberFormatIndex::PERCENT_DEC2;
            break;
        }

        formatIndex = xFormats->getStandardFormat(nformat, xLocale);
        res = i_set_cell_property(xCell, "NumberFormat", css::uno::makeAny(formatIndex));
    }

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

void officesdk_sheet_cell_wrap(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t wrapped, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::table::XCell> xCell;

    res = i_doc_cell(sheet, page, col, row, xCell);

    if (res == ekSDKRES_OK)
        res = i_set_cell_property(xCell, "IsTextWrapped", css::uno::makeAny((sal_Bool)wrapped));

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_color(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint32_t rgb, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::table::XCell> xCell;

    res = i_doc_cell(sheet, page, col, row, xCell);

    if (res == ekSDKRES_OK)
        res = i_set_cell_text_property(xCell, "CharColor", css::uno::makeAny((sal_uInt32)rgb));

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

void officesdk_sheet_cell_image(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const char_t *image_path, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::sheet::XSpreadsheet> xSheet;
    css::uno::Reference<css::table::XCell> xCell;
    css::uno::Reference<css::frame::XModel> xModel;
    sal_Int32 x = 0, y = 0, width = 0, height = 0;
    
    res = i_get_sheet(sheet, page, xSheet);

    // Cell for insertion
    if (res == ekSDKRES_OK)
        res = i_doc_cell(sheet, page, col, row, xCell);

    // The insertion frame
    if (res == ekSDKRES_OK)
        res = i_get_cell_frame(xSheet, xCell, x, y, width, height);

    // The document model (required for creating new graphic objects)
    if (res == ekSDKRES_OK)
    {
        try
        {
            css::uno::Reference<css::sheet::XSpreadsheetDocument> *xDocument = reinterpret_cast<css::uno::Reference<css::sheet::XSpreadsheetDocument>*>(sheet);
            xModel = css::uno::Reference<css::frame::XModel>(*xDocument, css::uno::UNO_QUERY_THROW);
        }
        catch (css::uno::Exception&)
        {
            res = ekSDKRES_ACCESS_DOC_ERROR;
        }
    }

    if (res == ekSDKRES_OK)
        res = i_insert_image(xModel, page, image_path, x, y, width, height);

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

static sal_Int16 i_line_style(const linestyle_t style)
{
    switch(style) {
    case ekLINE_STYLE_NONE:
        return css::table::BorderLineStyle::NONE;
    case ekLINE_STYLE_SOLID:
        return css::table::BorderLineStyle::SOLID;
    case ekLINE_STYLE_DOTTED:
        return css::table::BorderLineStyle::DOTTED;
    case ekLINE_STYLE_DASHED:
        return css::table::BorderLineStyle::DASHED;
    case ekLINE_STYLE_DOUBLE:
        return css::table::BorderLineStyle::DOUBLE;
    case ekLINE_STYLE_THINTHICK_SMALLGAP:
        return css::table::BorderLineStyle::THINTHICK_SMALLGAP;
    case ekLINE_STYLE_THINTHICK_MEDIUMGAP:
        return css::table::BorderLineStyle::THINTHICK_MEDIUMGAP;
    case ekLINE_STYLE_THINTHICK_LARGEGAP:
        return css::table::BorderLineStyle::THINTHICK_LARGEGAP;
    case ekLINE_STYLE_THICKTHIN_SMALLGAP:
        return css::table::BorderLineStyle::THICKTHIN_SMALLGAP;
    case ekLINE_STYLE_THICKTHIN_MEDIUMGAP:
        return css::table::BorderLineStyle::THICKTHIN_MEDIUMGAP;
    case ekLINE_STYLE_THICKTHIN_LARGEGAP:
        return css::table::BorderLineStyle::THICKTHIN_LARGEGAP;
    case ekLINE_STYLE_EMBOSSED:
        return css::table::BorderLineStyle::EMBOSSED;
    case ekLINE_STYLE_ENGRAVED:
        return css::table::BorderLineStyle::ENGRAVED;
    case ekLINE_STYLE_OUTSET:
        return css::table::BorderLineStyle::OUTSET;
    case ekLINE_STYLE_INSET:
        return css::table::BorderLineStyle::INSET;
    case ekLINE_STYLE_FINE_DASHED:
        return css::table::BorderLineStyle::DASHED;
    case ekLINE_STYLE_DOUBLE_THIN:
        return css::table::BorderLineStyle::DOUBLE_THIN;
    case ekLINE_STYLE_DASH_DOT:
        return css::table::BorderLineStyle::DASH_DOT;
    case ekLINE_STYLE_DASH_DOT_DOT:
        return css::table::BorderLineStyle::DASH_DOT_DOT;
    }

    return css::table::BorderLineStyle::NONE;
}

/*---------------------------------------------------------------------------*/

static css::table::BorderLine2 i_border_line(const linestyle_t style, const uint32_t thickness, const uint32_t rgb)
{
    css::table::BorderLine2 border;
    border.LineStyle = i_line_style(style);
    border.LineWidth = (sal_uInt32)thickness;
    border.Color = (sal_Int32)rgb;
    return border;
}

/*---------------------------------------------------------------------------*/

static css::table::TableBorder2 i_table_border(const linestyle_t style, const uint32_t thickness, const uint32_t rgb)
{
    css::table::TableBorder2 border;
    border.LeftLine = i_border_line(style, thickness, rgb);
    border.RightLine = i_border_line(style, thickness, rgb);
    border.TopLine = i_border_line(style, thickness, rgb);
    border.BottomLine = i_border_line(style, thickness, rgb);
    border.IsLeftLineValid = sal_True;
    border.IsRightLineValid = sal_True;
    border.IsTopLineValid = sal_True;
    border.IsBottomLineValid = sal_True;
    return border;
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_border(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const linestyle_t style, const uint32_t thickness, const uint32_t rgb, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::table::XCell> xCell;

    res = i_doc_cell(sheet, page, col, row, xCell);

    if (res == ekSDKRES_OK)
    {
        css::table::TableBorder2 border = i_table_border(style, thickness, rgb);
        res = i_set_cell_property(xCell, "TableBorder2", css::uno::makeAny(border));
    }

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

/*---------------------------------------------------------------------------*/

void officesdk_sheet_row_visible(Sheet *sheet, const uint32_t page, const uint32_t row, const bool_t visible, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::beans::XPropertySet> xTableRow;

    res = i_doc_row(sheet, page, row, xTableRow);

    if (res == ekSDKRES_OK)
        res = i_set_row_property(xTableRow, "IsVisible", css::uno::makeAny((sal_Bool)visible));

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_row_optimal_height(Sheet *sheet, const uint32_t page, const uint32_t row, const bool_t optimal_height, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::beans::XPropertySet> xTableRow;

    res = i_doc_row(sheet, page, row, xTableRow);

    if (res == ekSDKRES_OK)
        res = i_set_row_property(xTableRow, "OptimalHeight", css::uno::makeAny((sal_Bool)optimal_height));

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_row_height(Sheet *sheet, const uint32_t page, const uint32_t row, const uint32_t height, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::beans::XPropertySet> xTableRow;

    res = i_doc_row(sheet, page, row, xTableRow);

    if (res == ekSDKRES_OK)
        res = i_set_row_property(xTableRow, "Height", css::uno::makeAny((sal_Int32)height));

    ptr_assign(err, res);
}
