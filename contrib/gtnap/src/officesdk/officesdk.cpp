#include "officesdk.h"
#include "sheetsdk.h"
#include "writersdk.h"
#include <core/strings.h>
#include <sewer/cassert.h>
#include <sewer/ptr.h>

#ifdef GTNAP_LIBREOFFICE

#include <osapp/osapp.h>
#include <osbs/bproc.h>
#include <osbs/bthread.h>
#include <osbs/btime.h>
#include <osbs/osbs.h>
#include <sewer/blib.h>
#include <sewer/bstd.h>
#include <sewer/unicode.h>

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
#include <text/HoriOrientation.hpp>
#include <text/VertOrientation.hpp>
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
#include <text/ControlCharacter.hpp>
#include <text/XTextContent.hpp>
#include <text/TextContentAnchorType.hpp>
#include <text/XTextField.hpp>
#include <text/XTextFieldsSupplier.hpp>
#include <text/PageNumberType.hpp>
#include <util/XMergeable.hpp>
#include <util/XProtectable.hpp>
#include <util/XNumberFormatsSupplier.hpp>
#include <util/XNumberFormatTypes.hpp>
#include <style/ParagraphAdjust.hpp>
#include <style/BreakType.hpp>
#include <style/LineSpacing.hpp>
#include <style/LineSpacingMode.hpp>
#include <style/XStyle.hpp>
#include <style/XStyleFamiliesSupplier.hpp>
#include <style/NumberingType.hpp>
#include <view/XPrintable.hpp>
#include <view/PaperOrientation.hpp>
#include <view/PaperFormat.hpp>
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

    sdkres_t CreateTextDocument(css::uno::Reference<css::text::XTextDocument> &xDocument);

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

static String *i_StringFromOUString(const ::rtl::OUString &ostr)
{
    sal_Int32 l = ostr.getLength();
    const sal_Unicode *b = ostr.getStr();
    uint32_t nbytes = 0, nused = 0;
    String *str = NULL;
    cassert(sizeof(sal_Unicode) == 2);
    nbytes = unicode_convers_nbytes_n((const char_t*)b, (uint32_t)l * sizeof(sal_Unicode), ekUTF16, ekUTF8);
    str = str_reserve(nbytes);
    nused = unicode_convers((const char_t*)b, tcc(str), ekUTF16, ekUTF8, nbytes);
    cassert(nused == nbytes);
    return str;
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

sdkres_t OfficeSdk::CreateTextDocument(css::uno::Reference<css::text::XTextDocument> &xDocument)
{
    sdkres_t res = ekSDKRES_OK;
    try
    {
        // https://wiki.openoffice.org/wiki/ES/Manuales/GuiaAOO/TemasAvanzados/Macros/StarBasic/TrabajandoConOOo/TrabajandoConDocumentos
        css::uno::Sequence<css::beans::PropertyValue> loadProperties(1);
        loadProperties[0].Name = "Hidden";
        loadProperties[0].Value <<= true;
        css::uno::Reference<css::lang::XComponent> xComponent = this->m_xComponentLoader->loadComponentFromURL("private:factory/swriter", "_blank", 0, loadProperties);
        xDocument = css::uno::Reference<css::text::XTextDocument>(xComponent, css::uno::UNO_QUERY_THROW);
    }
    catch(css::uno::Exception &e)
    {
        std::cout << "Error creating new text document: " << e.Message;
        res = ekSDKRES_CREATE_FILE_ERROR;
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
    case ekSDKRES_TEXT_PROPERTY_ERROR:
        return "Failed setting text properties";
    case ekSDKRES_TEXT_ADD_ERROR:
        return "Failed adding text";
    case ekSDKRES_PAGE_PROPERTY_ERROR:
        return "Failed setting page style property";
    case ekSDKRES_PRINTER_CONFIG_ERROR:
        return "Error in printer configuration";
    case ekSDKRES_PRINT_ERROR:
        return "Error when printing a document";
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

static void i_sheet_save(Sheet *sheet, const char_t *pathname, const fileformat_t format, sdkres_t *err)
{
    sdkres_t res = i_OFFICE_SDK.Init();
    cassert_no_null(sheet);
    cassert_no_null(pathname);

    if (res == ekSDKRES_OK)
    {
        css::uno::Reference<css::sheet::XSpreadsheetDocument> *xDocument = reinterpret_cast<css::uno::Reference<css::sheet::XSpreadsheetDocument>*>(sheet);
        res = i_OFFICE_SDK.SaveSheetDocument(*xDocument, pathname, format);
    }

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_save(Sheet *sheet, const char_t *pathname, sdkres_t *err)
{
    i_sheet_save(sheet, pathname, ekFORMAT_OPEN_OFFICE, err);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_pdf(Sheet *sheet, const char_t *pathname, sdkres_t *err)
{
    i_sheet_save(sheet, pathname, ekFORMAT_PDF, err);
}

/*---------------------------------------------------------------------------*/

static css::view::PaperOrientation i_paper_orient(const paperorient_t orient)
{
    switch(orient) {
    case ekPAPERORIENT_PORTRAIT:
        return css::view::PaperOrientation::PaperOrientation_PORTRAIT;
    case ekPAPERORIENT_LANSCAPE:
        return css::view::PaperOrientation::PaperOrientation_LANDSCAPE;
    }

    return (css::view::PaperOrientation)SAL_MAX_ENUM;
}

/*---------------------------------------------------------------------------*/

static css::view::PaperFormat i_paper_format(const paperformat_t format)
{
    switch(format) {
    case ekPAPERFORMAT_A3:
        return css::view::PaperFormat::PaperFormat_A3;
    case ekPAPERFORMAT_A4:
        return css::view::PaperFormat::PaperFormat_A4;
    case ekPAPERFORMAT_A5:
        return css::view::PaperFormat::PaperFormat_A5;
    case ekPAPERFORMAT_B4:
        return css::view::PaperFormat::PaperFormat_B4;
    case ekPAPERFORMAT_B5:
        return css::view::PaperFormat::PaperFormat_B5;
    case ekPAPERFORMAT_LETTER:
        return css::view::PaperFormat::PaperFormat_LETTER;
    case ekPAPERFORMAT_LEGAL:
        return css::view::PaperFormat::PaperFormat_LEGAL;
    case ekPAPERFORMAT_TABLOID:
        return css::view::PaperFormat::PaperFormat_TABLOID;
    case ekPAPERFORMAT_USER:
        return css::view::PaperFormat::PaperFormat_USER;
    }

    return (css::view::PaperFormat)SAL_MAX_ENUM;
}

/*---------------------------------------------------------------------------*/

// https://wiki.documentfoundation.org/Documentation/DevGuide/Spreadsheet_Documents#Printer_and_Print_Job_Settings
static sdkres_t i_xprintable_print(
                            css::uno::Reference<css::view::XPrintable> &xPrintable, 
                            const char_t *filename, 
                            const char_t *printer, 
                            const paperorient_t orient, 
                            const paperformat_t format, 
                            const uint32_t paper_width, 
                            const uint32_t paper_height, 
                            const uint32_t num_copies, 
                            const bool_t collate_copies, 
                            const char_t *pages)
{
    sdkres_t res = ekSDKRES_OK;

    // Configure the printer
    if (res == ekSDKRES_OK)
    {
        try
        {
            css::uno::Sequence<css::beans::PropertyValue> printerProps = xPrintable->getPrinter();
            css::uno::Sequence<css::beans::PropertyValue> newPrinterProps(printerProps.getLength());

            for (sal_Int32 i = 0; i < printerProps.getLength(); ++i)
            {
                bool isChanged = false;
                const ::rtl::OUString name = printerProps[i].Name;
                newPrinterProps[i].Name = name;

                // Change the default printer name
                if (name.equalsAscii("Name"))
                {
                    if (str_empty_c(printer) == FALSE)
                    {
                        ::rtl::OUString value = i_OUStringFromUTF8(printer);
                        newPrinterProps[i].Value <<= value;
                        isChanged = true;
                    }
                }

                // Change the default paper orientation
                else if (name.equalsAscii("PaperOrientation"))
                {
                    css::view::PaperOrientation norient = i_paper_orient(orient);
                    if (norient != SAL_MAX_ENUM)
                    {
                        newPrinterProps[i].Value <<= norient;
                        isChanged = true;
                    }
                }

                // Change the default paper format
                else if (name.equalsAscii("PaperFormat"))
                {
                    css::view::PaperFormat nformat = i_paper_format(format);
                    if (nformat != SAL_MAX_ENUM)
                    {
                        newPrinterProps[i].Value <<= nformat;
                        isChanged = true;
                    }
                }

                // Change the default paper size
                else if (name.equalsAscii("PaperSize"))
                {
                    if (format == ekPAPERFORMAT_USER && paper_width > 0 && paper_height > 0)
                    {
                        css::awt::Size size((sal_Int32)paper_width, (sal_Int32)paper_height);
                        newPrinterProps[i].Value <<= size;
                        isChanged = true;
                    }
                }

                // Leave the default value
                if (!isChanged)
                    newPrinterProps[i].Value <<= printerProps[i].Value;
            }

            xPrintable->setPrinter(newPrinterProps);
        }
        catch (css::uno::Exception&)
        {
            res = ekSDKRES_PRINTER_CONFIG_ERROR;
        }
    }

    // Configure the printing
    if (res == ekSDKRES_OK)
    {
        try
        {
            // num_copies, collate_copies
            sal_Int32 nprops = 2, n = 0;
            if (str_empty_c(filename) == FALSE)
                nprops += 1;
            if (str_empty_c(pages) == FALSE)
                nprops += 1;

            css::uno::Sequence<css::beans::PropertyValue> printProps(nprops);
            printProps[n].Name = "CopyCount";
            printProps[n].Value <<= (sal_Int16)num_copies;
            n += 1;
            printProps[n].Name = "Collate";
            printProps[n].Value <<= (sal_Bool)collate_copies;
            n += 1;
            if (str_empty_c(filename) == FALSE)
            {
                ::rtl::OUString value = i_OUStringFromUTF8(filename);
                printProps[n].Name = "FileName";
                printProps[n].Value <<= value;
                n += 1;
            }

            if (str_empty_c(pages) == FALSE)
            {
                ::rtl::OUString value = i_OUStringFromUTF8(pages);
                printProps[n].Name = "Pages";
                printProps[n].Value <<= value;
                n += 1;
            }

            cassert(n == nprops);
            xPrintable->print(printProps);
        }
        catch (css::uno::Exception&)
        {
            res = ekSDKRES_PRINT_ERROR;
        }
    }

    return res;
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_print(Sheet *sheet, const char_t *filename, const char_t *printer, const paperorient_t orient, const paperformat_t format, const uint32_t paper_width, const uint32_t paper_height, const uint32_t num_copies, const bool_t collate_copies, const char_t *pages, sdkres_t *err)
{
    sdkres_t res = i_OFFICE_SDK.Init();
    css::uno::Reference<css::view::XPrintable> xPrintable;
    cassert_no_null(sheet);

    if (res == ekSDKRES_OK)
    {
        try
        {
            css::uno::Reference<css::sheet::XSpreadsheetDocument> *xDocument = reinterpret_cast<css::uno::Reference<css::sheet::XSpreadsheetDocument>*>(sheet);
            xPrintable = css::uno::Reference<css::view::XPrintable>(*xDocument, css::uno::UNO_QUERY_THROW);
        }
        catch (css::uno::Exception&)
        {
            res = ekSDKRES_ACCESS_DOC_ERROR;
        }
    }

    res = i_xprintable_print(xPrintable, filename, printer, orient, format, paper_width, paper_height, num_copies, collate_copies, pages);
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

static sdkres_t i_set_cell_date(
                    css::uno::Reference<css::table::XCell> &xCell,
                    const uint8_t day,
                    const uint8_t month,
                    const int16_t year)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        Date date;
        uint64_t epoch;
        double value;
        date.mday = day;
        date.month = month;
        date.year = year;
        date.hour = 0;
        date.minute = 0;
        date.second = 0;

        // Number of micro-seconds from epoch to date
        epoch = btime_to_micro(&date);

        // We want seconds
        epoch = epoch / 1000000;

        // Days from epoch + 25569 offset
        // Difference to 1/1/1970 (epoch) since 12/30/1899 (LibreOffice 0-day) in days
        // https://unix.stackexchange.com/questions/421354/convert-epoch-time-to-human-readable-in-libreoffice-calc
        value = (double)((epoch/(24 * 60 * 60)) + 25569);

        xCell->setValue(value);
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_EDIT_CELL_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_set_cell_formula(
                    css::uno::Reference<css::table::XCell> &xCell,
                    const char_t *formula)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        String *form = str_printf("=%s", formula);
        ::rtl::OUString sformula = i_OUStringFromUTF8(tc(form));
        xCell->setFormula(sformula);
        str_destroy(&form);
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

static sdkres_t i_set_cell_range_property(
                    css::uno::Reference<css::table::XCellRange> &xCellRange,
                    const char_t *prop_name,
                    const css::uno::Any &value)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        css::uno::Reference<css::beans::XPropertySet> xCellProps = css::uno::Reference<css::beans::XPropertySet>::query(xCellRange);
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
        defname = str_printf("Sheet%d", n);
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

static void i_column_id(const uint32_t col, char_t *id, const uint32_t n)
{
    const uint32_t n_ascii = 26;
    uint32_t temp = col;
    uint32_t i = 0;

    for (;i < n - 1;)
    {
        id[i] = 'A' + (char_t)(temp % n_ascii);
        i += 1;

        if (temp < n_ascii)
        {
            id[i] = '\0';
            break;
        }
        else
        {
            temp -= n_ascii;
        }
    }
}

/*---------------------------------------------------------------------------*/

String *officesdk_sheet_cell_ref(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::sheet::XSpreadsheet> xSheet;
    ::rtl::OUString pageName;
    String *str = NULL;

    if (res == ekSDKRES_OK)
        res = i_get_sheet(sheet, page, xSheet);

    if (res == ekSDKRES_OK)
    {
        try
        {
            css::uno::Reference<css::container::XNamed> xNamed = css::uno::Reference<css::container::XNamed>(xSheet, css::uno::UNO_QUERY_THROW);
            pageName = xNamed->getName();
        }
        catch (css::uno::Exception&)
        {
            res = ekSDKRES_ACCESS_DOC_ERROR;
        }
    }

    if (res == ekSDKRES_OK)
    {
        char_t col_id[64];
        String *page_id = i_StringFromOUString(pageName);
        i_column_id(col, col_id, sizeof(col_id));
        str = str_printf("$'%s'.%s%d", tc(page_id), col_id, row + 1);
        str_destroy(&page_id);
    }
    else
    {
        str = str_c("");
    }

    ptr_assign(err, res);
    return str;
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

void officesdk_sheet_cell_date(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint8_t day, const uint8_t month, const int16_t year, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::table::XCell> xCell;

    res = i_doc_cell(sheet, page, col, row, xCell);

    if (res == ekSDKRES_OK)
        res = i_set_cell_date(xCell, day, month, year);

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_formula(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const char_t *formula, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::table::XCell> xCell;

    res = i_doc_cell(sheet, page, col, row, xCell);

    if (res == ekSDKRES_OK)
        res = i_set_cell_formula(xCell, formula);

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
        case ekNUMFORMAT_DATE_SYS_DDMMM:
            nformat = css::i18n::NumberFormatIndex::DATE_SYS_DDMMM;
            break;
        case ekNUMFORMAT_DATE_SYS_DDMMYY:
            nformat = css::i18n::NumberFormatIndex::DATE_SYS_DDMMYY;
            break;
        case ekNUMFORMAT_DATE_SYS_DDMMYYYY:
            nformat = css::i18n::NumberFormatIndex::DATE_SYS_DDMMYYYY;
            break;
        case ekNUMFORMAT_DATE_SYS_DMMMMYYYY:
            nformat = css::i18n::NumberFormatIndex::DATE_SYS_DMMMMYYYY;
            break;
        case ekNUMFORMAT_DATE_SYS_DMMMYY:
            nformat = css::i18n::NumberFormatIndex::DATE_SYS_DMMMYY;
            break;
        case ekNUMFORMAT_DATE_SYS_DMMMYYYY:
            nformat = css::i18n::NumberFormatIndex::DATE_SYS_DMMMYYYY;
            break;
        case ekNUMFORMAT_DATE_SYS_MMYY:
            nformat = css::i18n::NumberFormatIndex::DATE_SYS_MMYY;
            break;
        case ekNUMFORMAT_DATE_SYS_NNDMMMMYYYY:
            nformat = css::i18n::NumberFormatIndex::DATE_SYS_NNDMMMMYYYY;
            break;
        case ekNUMFORMAT_DATE_SYS_NNDMMMYY:
            nformat = css::i18n::NumberFormatIndex::DATE_SYS_NNDMMMYY;
            break;
        case ekNUMFORMAT_DATE_SYS_NNNNDMMMMYYYY:
            nformat = css::i18n::NumberFormatIndex::DATE_SYS_NNNNDMMMMYYYY;
            break;
        }

        formatIndex = xFormats->getFormatIndex(nformat, xLocale);
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

void officesdk_sheet_cell_halign(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const halign_t align, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::table::XCell> xCell;

    res = i_doc_cell(sheet, page, col, row, xCell);

    if (res == ekSDKRES_OK)
    {
        css::table::CellHoriJustify just = css::table::CellHoriJustify::CellHoriJustify_LEFT;
        switch (align) {
        case ekHALIGN_LEFT:
            just = css::table::CellHoriJustify::CellHoriJustify_LEFT;
            break;
        case ekHALIGN_CENTER:
            just = css::table::CellHoriJustify::CellHoriJustify_CENTER;
            break;
        case ekHALIGN_RIGHT:
            just = css::table::CellHoriJustify::CellHoriJustify_RIGHT;
            break;
        case ekHALIGN_JUSTIFY:
            just = css::table::CellHoriJustify::CellHoriJustify_LEFT;
            break;
        }

        res = i_set_cell_property(xCell, "HoriJustify", css::uno::makeAny(just));
    }

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_valign(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const valign_t align, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::table::XCell> xCell;

    res = i_doc_cell(sheet, page, col, row, xCell);

    if (res == ekSDKRES_OK)
    {
        ::sal_Int32 just = css::table::CellVertJustify2::CENTER;

        switch (align) {
        case ekVALIGN_TOP:
            just = css::table::CellVertJustify2::TOP;
            break;
        case ekVALIGN_CENTER:
            just = css::table::CellVertJustify2::CENTER;
            break;
        case ekVALIGN_BOTTOM:
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

void officesdk_sheet_cells_backcolor(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, const uint32_t rgb, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::table::XCellRange> xCellRange;

    res = i_doc_range(sheet, page, st_col, st_row, ed_col, ed_row, xCellRange);

    if (res == ekSDKRES_OK)
        res = i_set_cell_range_property(xCellRange, "CellBackColor", css::uno::makeAny((sal_uInt32)rgb));

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

void officesdk_sheet_cells_border(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, const linestyle_t style, const uint32_t thickness, const uint32_t rgb, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::table::XCellRange> xCellRange;

    res = i_doc_range(sheet, page, st_col, st_row, ed_col, ed_row, xCellRange);

    if (res == ekSDKRES_OK)
    {
        css::table::TableBorder2 border = i_table_border(style, thickness, rgb);
        res = i_set_cell_range_property(xCellRange, "TableBorder2", css::uno::makeAny(border));
    }

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cells_merge(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, sdkres_t *err)
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

/*---------------------------------------------------------------------------*/

Writer *officesdk_writer_open(const char_t *pathname, sdkres_t *err)
{
    sdkres_t res = i_OFFICE_SDK.Init();
    Writer *writer = NULL;
    css::uno::Reference<css::text::XTextDocument> *xDocument = nullptr;
    cassert_no_null(pathname);
    if (res == ekSDKRES_OK)
    {
        xDocument = new css::uno::Reference<css::text::XTextDocument>();
        res = i_OFFICE_SDK.OpenTextDocument(pathname, *xDocument);
    }

    if (res == ekSDKRES_OK)
    {
        writer = reinterpret_cast<Writer*>(xDocument);
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
    return writer;
}

/*---------------------------------------------------------------------------*/

Writer *officesdk_writer_create(sdkres_t *err)
{
    sdkres_t res = i_OFFICE_SDK.Init();
    Writer *writer = NULL;
    css::uno::Reference<css::text::XTextDocument> *xDocument = nullptr;
    if (res == ekSDKRES_OK)
    {
        xDocument = new css::uno::Reference<css::text::XTextDocument>();
        res = i_OFFICE_SDK.CreateTextDocument(*xDocument);
    }

    if (res == ekSDKRES_OK)
    {
        writer = reinterpret_cast<Writer*>(xDocument);
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
    return writer;
}

/*---------------------------------------------------------------------------*/

static void i_writer_save(Writer *writer, const char_t *pathname, const fileformat_t format, sdkres_t *err)
{
    sdkres_t res = i_OFFICE_SDK.Init();
    cassert_no_null(writer);
    cassert_no_null(pathname);

    if (res == ekSDKRES_OK)
    {
        css::uno::Reference<css::text::XTextDocument> *xDocument = reinterpret_cast<css::uno::Reference<css::text::XTextDocument>*>(writer);
        res = i_OFFICE_SDK.SaveTextDocument(*xDocument, pathname, format);
    }

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_save(Writer *writer, const char_t *pathname, sdkres_t *err)
{
    i_writer_save(writer, pathname, ekFORMAT_OPEN_OFFICE, err);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_pdf(Writer *writer, const char_t *pathname, sdkres_t *err)
{
    i_writer_save(writer, pathname, ekFORMAT_PDF, err);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_print(Writer *writer, const char_t *filename, const char_t *printer, const paperorient_t orient, const paperformat_t format, const uint32_t paper_width, const uint32_t paper_height, const uint32_t num_copies, const bool_t collate_copies, const char_t *pages, sdkres_t *err)
{
    sdkres_t res = i_OFFICE_SDK.Init();
    css::uno::Reference<css::view::XPrintable> xPrintable;
    cassert_no_null(writer);

    if (res == ekSDKRES_OK)
    {
        try
        {
            css::uno::Reference<css::text::XTextDocument> *xDocument = reinterpret_cast<css::uno::Reference<css::text::XTextDocument>*>(writer);
            xPrintable = css::uno::Reference<css::view::XPrintable>(*xDocument, css::uno::UNO_QUERY_THROW);
        }
        catch (css::uno::Exception&)
        {
            res = ekSDKRES_ACCESS_DOC_ERROR;
        }
    }

    res = i_xprintable_print(xPrintable, filename, printer, orient, format, paper_width, paper_height, num_copies, collate_copies, pages);
    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_close(Writer *writer, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    try
    {
        css::uno::Reference<css::text::XTextDocument> *xDocument = reinterpret_cast<css::uno::Reference<css::text::XTextDocument>*>(writer);
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

static sdkres_t i_get_style(
                    Writer *writer,
                    css::uno::Reference<css::beans::XPropertySet> &xPageStyle)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        css::uno::Reference<css::text::XTextDocument> *xDocument = reinterpret_cast<css::uno::Reference<css::text::XTextDocument>*>(writer);
        css::uno::Reference<css::style::XStyleFamiliesSupplier> xStyleFamiliesSupplier(*xDocument, css::uno::UNO_QUERY_THROW);
        css::uno::Reference<css::container::XNameAccess> xStyles = xStyleFamiliesSupplier->getStyleFamilies();
        css::uno::Any styleFamily = xStyles->getByName("PageStyles");
        css::uno::Reference<css::container::XNameAccess> xPageStyles(styleFamily, css::uno::UNO_QUERY_THROW);
        css::uno::Any styleElement = xPageStyles->hasByName("Standard") ? xPageStyles->getByName("Standard") : xPageStyles->getByName("Default");
        css::uno::Reference<css::style::XStyle> xStyle(styleElement, css::uno::UNO_QUERY_THROW);
        css::uno::Reference<css::beans::XPropertySet> xProperties(xStyle, css::uno::UNO_QUERY_THROW);
        xPageStyle = xProperties;
        //Just for Debug
        //css::uno::Reference<css::beans::XPropertySetInfo> info = xProperties->getPropertySetInfo();
        //css::uno::Sequence<css::beans::Property> props = info->getProperties();
        //for (sal_Int32 i = 0; i < props.getLength(); ++i)
        //{
        //    ::rtl::OUString ostr = props.getConstArray()[i].Name;
        //    String *str = i_StringFromOUString(ostr);
        //    bstd_printf("%s\n", tc(str));
        //    str_destroy(&str);
        //}
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_ACCESS_DOC_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_get_page_property(
                    css::uno::Reference<css::beans::XPropertySet> xPageStyle,
                    const char_t *prop_name,
                    css::uno::Any &value)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        ::rtl::OUString prop = i_OUStringFromUTF8(prop_name);
        value = xPageStyle->getPropertyValue(prop);
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_PAGE_PROPERTY_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_get_text(
                    Writer *writer,
                    const textspace_t space,
                    css::uno::Reference<css::text::XText> &xText)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        css::uno::Reference<css::text::XTextDocument> *xDocument = reinterpret_cast<css::uno::Reference<css::text::XTextDocument>*>(writer);
        if (space == ekTEXT_SPACE_PAGE)
        {
            xText = (*xDocument)->getText();
        }
        else
        {
            css::uno::Reference<css::beans::XPropertySet> xPageStyle;
            css::uno::Any textValue;

            if (res == ekSDKRES_OK)
                res = i_get_style(writer, xPageStyle);

            if (res == ekSDKRES_OK)
            {
                if (space == ekTEXT_SPACE_HEADER)
                {
                    res = i_get_page_property(xPageStyle, "HeaderText", textValue);
                }
                else
                {
                    cassert(space == ekTEXT_SPACE_FOOTER);
                    res = i_get_page_property(xPageStyle, "FooterText", textValue);
                }
            }

            if (res == ekSDKRES_OK)
                textValue >>= xText;
        }
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_ACCESS_DOC_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_set_text_property(
                    css::uno::Reference<css::text::XText> &xText,
                    const char_t *prop_name,
                    const css::uno::Any &value)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        css::uno::Reference<css::text::XTextRange> xTextRange = xText->getEnd();
        css::uno::Reference<css::beans::XPropertySet> xTextProperties(xTextRange, css::uno::UNO_QUERY_THROW);
        ::rtl::OUString prop = i_OUStringFromUTF8(prop_name);
        xTextProperties->setPropertyValue(prop, value);
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_TEXT_PROPERTY_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_set_page_property(
                    css::uno::Reference<css::beans::XPropertySet> xPageStyle,
                    const char_t *prop_name,
                    const css::uno::Any &value)
{
    sdkres_t res = ekSDKRES_OK;

    try
    {
        ::rtl::OUString prop = i_OUStringFromUTF8(prop_name);
        xPageStyle->setPropertyValue(prop, value);
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_PAGE_PROPERTY_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_page_header_show(Writer *writer, const bool_t show, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::beans::XPropertySet> xPageStyle;

    if (res == ekSDKRES_OK)
        res = i_get_style(writer, xPageStyle);

    if (res == ekSDKRES_OK)
        res = i_set_page_property(xPageStyle, "HeaderIsOn", css::uno::makeAny((sal_Bool)show));

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_page_header_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t spacing, const uint32_t height, const bool_t dynamic_spacing, const bool_t dynamic_height, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::beans::XPropertySet> xPageStyle;

    if (res == ekSDKRES_OK)
        res = i_get_style(writer, xPageStyle);

    if (res == ekSDKRES_OK)
        res = i_set_page_property(xPageStyle, "HeaderLeftMargin", css::uno::makeAny((sal_Int32)left));

    if (res == ekSDKRES_OK)
        res = i_set_page_property(xPageStyle, "HeaderRightMargin", css::uno::makeAny((sal_Int32)right));

    if (res == ekSDKRES_OK)
        res = i_set_page_property(xPageStyle, "HeaderBodyDistance", css::uno::makeAny((sal_Int32)spacing));

    if (res == ekSDKRES_OK)
        res = i_set_page_property(xPageStyle, "HeaderHeight", css::uno::makeAny((sal_Int32)height));

    if (res == ekSDKRES_OK)
        res = i_set_page_property(xPageStyle, "HeaderDynamicSpacing", css::uno::makeAny((sal_Bool)dynamic_spacing));

    if (res == ekSDKRES_OK)
        res = i_set_page_property(xPageStyle, "HeaderIsDynamicHeight", css::uno::makeAny((sal_Bool)dynamic_height));

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_page_footer_show(Writer *writer, const bool_t show, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::beans::XPropertySet> xPageStyle;

    if (res == ekSDKRES_OK)
        res = i_get_style(writer, xPageStyle);

    if (res == ekSDKRES_OK)
        res = i_set_page_property(xPageStyle, "FooterIsOn", css::uno::makeAny((sal_Bool)show));

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_page_footer_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t spacing, const uint32_t height, const bool_t dynamic_spacing, const bool_t dynamic_height, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::beans::XPropertySet> xPageStyle;

    if (res == ekSDKRES_OK)
        res = i_get_style(writer, xPageStyle);

    if (res == ekSDKRES_OK)
        res = i_set_page_property(xPageStyle, "FooterLeftMargin", css::uno::makeAny((sal_Int32)left));

    if (res == ekSDKRES_OK)
        res = i_set_page_property(xPageStyle, "FooterRightMargin", css::uno::makeAny((sal_Int32)right));

    if (res == ekSDKRES_OK)
        res = i_set_page_property(xPageStyle, "FooterBodyDistance", css::uno::makeAny((sal_Int32)spacing));

    if (res == ekSDKRES_OK)
        res = i_set_page_property(xPageStyle, "FooterHeight", css::uno::makeAny((sal_Int32)height));

    if (res == ekSDKRES_OK)
        res = i_set_page_property(xPageStyle, "FooterDynamicSpacing", css::uno::makeAny((sal_Bool)dynamic_spacing));

    if (res == ekSDKRES_OK)
        res = i_set_page_property(xPageStyle, "FooterIsDynamicHeight", css::uno::makeAny((sal_Bool)dynamic_height));

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_page_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t top, const uint32_t bottom, const uint32_t gutter, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::beans::XPropertySet> xPageStyle;

    if (res == ekSDKRES_OK)
        res = i_get_style(writer, xPageStyle);

    if (res == ekSDKRES_OK)
        res = i_set_page_property(xPageStyle, "LeftMargin", css::uno::makeAny((sal_Int32)left));

    if (res == ekSDKRES_OK)
        res = i_set_page_property(xPageStyle, "RightMargin", css::uno::makeAny((sal_Int32)right));

    if (res == ekSDKRES_OK)
        res = i_set_page_property(xPageStyle, "TopMargin", css::uno::makeAny((sal_Int32)top));

    if (res == ekSDKRES_OK)
        res = i_set_page_property(xPageStyle, "BottomMargin", css::uno::makeAny((sal_Int32)bottom));

    if (res == ekSDKRES_OK)
        res = i_set_page_property(xPageStyle, "GutterMargin", css::uno::makeAny((sal_Int32)gutter));

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_font_family(Writer *writer, const textspace_t space, const char_t *font_family, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::text::XText> xText;

    if (res == ekSDKRES_OK)
        res = i_get_text(writer, space, xText);

    if (res == ekSDKRES_OK && str_empty_c(font_family) == FALSE)
    {
        ::rtl::OUString fname = i_OUStringFromUTF8(font_family);
        res = i_set_text_property(xText, "CharFontName", css::uno::makeAny(fname));
    }

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_font_size(Writer *writer, const textspace_t space, const real32_t font_size, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::text::XText> xText;

    if (res == ekSDKRES_OK)
        res = i_get_text(writer, space, xText);

    if (res == ekSDKRES_OK && font_size > 0)
        res = i_set_text_property(xText, "CharHeight", css::uno::makeAny(font_size));
   
    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_bold(Writer *writer, const textspace_t space, const bool_t bold, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::text::XText> xText;

    if (res == ekSDKRES_OK)
        res = i_get_text(writer, space, xText);

    if (res == ekSDKRES_OK)
    {
        float weight = bold ? css::awt::FontWeight::BOLD : css::awt::FontWeight::LIGHT;
        res = i_set_text_property(xText, "CharWeight", css::uno::makeAny(weight));
    }
   
    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_italic(Writer *writer, const textspace_t space, const bool_t italic, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::text::XText> xText;

    if (res == ekSDKRES_OK)
        res = i_get_text(writer, space, xText);

    if (res == ekSDKRES_OK)
    {
        css::awt::FontSlant slant = italic ? css::awt::FontSlant::FontSlant_ITALIC : css::awt::FontSlant::FontSlant_NONE;
        res = i_set_text_property(xText, "CharPosture", css::uno::makeAny(slant));
    }
   
    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_paragraph_halign(Writer *writer, const textspace_t space, const halign_t align, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::text::XText> xText;

    if (res == ekSDKRES_OK)
        res = i_get_text(writer, space, xText);

    if (res == ekSDKRES_OK)
    {
        css::style::ParagraphAdjust adjust = css::style::ParagraphAdjust::ParagraphAdjust_LEFT;
        switch(align) {
        case ekHALIGN_LEFT:
            adjust = css::style::ParagraphAdjust::ParagraphAdjust_LEFT;
            break;
        case ekHALIGN_CENTER:
            adjust = css::style::ParagraphAdjust::ParagraphAdjust_CENTER;
            break;
        case ekHALIGN_RIGHT:
            adjust = css::style::ParagraphAdjust::ParagraphAdjust_RIGHT;
            break;
        case ekHALIGN_JUSTIFY:
            adjust = css::style::ParagraphAdjust::ParagraphAdjust_BLOCK;
            break;
        }

        res = i_set_text_property(xText, "ParaAdjust", css::uno::makeAny(adjust));
    }
   
    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_paragraph_lspacing(Writer *writer, const textspace_t space, const uint32_t height, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::text::XText> xText;

    if (res == ekSDKRES_OK)
        res = i_get_text(writer, space, xText);

    if (res == ekSDKRES_OK)
    {
        css::style::LineSpacing spacing;
        spacing.Mode = css::style::LineSpacingMode::FIX;
        spacing.Height = (sal_Int16)height;
        res = i_set_text_property(xText, "ParaLineSpacing", css::uno::makeAny(spacing));
    }
   
    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_insert_text(Writer *writer, const textspace_t space, const char_t *text, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::text::XText> xText;

    if (res == ekSDKRES_OK)
        res = i_get_text(writer, space, xText);

    if (res == ekSDKRES_OK)
    {
        try
        {
            css::uno::Reference<css::text::XTextRange> xTextRange = xText->getEnd();
            ::rtl::OUString str = i_OUStringFromUTF8(text);
            xText->insertString(xTextRange, str, sal_False);
        }
        catch (css::uno::Exception&)
        {
            res = ekSDKRES_TEXT_ADD_ERROR;
        }
    }

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

static sdkres_t i_create_text_content(
                        css::uno::Reference<css::frame::XModel> &xModel,
                        const char_t *contentType,
                        css::uno::Reference<css::text::XTextContent> &xTextContent)
{
    sdkres_t res = ekSDKRES_OK;
    try
    {
        ::rtl::OUString type = i_OUStringFromUTF8(contentType);
        css::uno::Reference<css::lang::XMultiServiceFactory> xServiceFactory(xModel, css::uno::UNO_QUERY_THROW);
        css::uno::Reference<css::uno::XInterface> xInterface = xServiceFactory->createInstance(type);
        xTextContent = css::uno::Reference<css::text::XTextContent>(xInterface, css::uno::UNO_QUERY_THROW);
    }
    catch(css::uno::Exception &e)
    {
        std::cout << "Error creating new text content: " << e.Message;
        res = ekSDKRES_CREATE_FILE_ERROR;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_insert_image(Writer *writer, const textspace_t space, const anchortype_t anchor, const uint32_t width, const uint32_t height, const halign_t halign, const valign_t valign, const char_t *image_path, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::text::XText> xText;
    css::uno::Reference<css::frame::XModel> xModel;
    css::uno::Reference<css::graphic::XGraphic> xGraphic;
    css::uno::Reference<css::text::XTextContent> xTextContent;

    if (res == ekSDKRES_OK)
        res = i_get_text(writer, space, xText);

    // The document model (required for creating new graphic objects)
    if (res == ekSDKRES_OK)
    {
        try
        {
            css::uno::Reference<css::text::XTextDocument> *xDocument = reinterpret_cast<css::uno::Reference<css::text::XTextDocument>*>(writer);
            xModel = css::uno::Reference<css::frame::XModel>(*xDocument, css::uno::UNO_QUERY_THROW);
        }
        catch (css::uno::Exception&)
        {
            res = ekSDKRES_ACCESS_DOC_ERROR;
        }
    }

    // Get the image graphic from file
    if (res == ekSDKRES_OK)
        res = i_OFFICE_SDK.LoadImage(image_path, xGraphic);

    // Create the text content object
    if (res == ekSDKRES_OK)
        res = i_create_text_content(xModel, "com.sun.star.text.TextGraphicObject", xTextContent);

    // Configure the text object and add the image graphic
    if (res == ekSDKRES_OK)
    {
        try
        {
            css::uno::Reference<css::beans::XPropertySet> xProps(xTextContent, css::uno::UNO_QUERY_THROW);
            css::text::TextContentAnchorType anchorType = css::text::TextContentAnchorType::TextContentAnchorType_AS_CHARACTER;
            sal_Int16 horient = css::text::HoriOrientation::LEFT;
            sal_Int16 vorient = css::text::VertOrientation::CENTER;

            switch (anchor) {
            case ekANCHOR_AT_PARAGRAPH:
                anchorType = css::text::TextContentAnchorType::TextContentAnchorType_AT_PARAGRAPH;
                break;
            case ekANCHOR_AS_CHARACTER:
                anchorType = css::text::TextContentAnchorType::TextContentAnchorType_AS_CHARACTER;
                break;
            case ekANCHOR_AT_PAGE:
                anchorType = css::text::TextContentAnchorType::TextContentAnchorType_AT_PAGE;
                break;
            case ekANCHOR_AT_FRAME:
                anchorType = css::text::TextContentAnchorType::TextContentAnchorType_AT_FRAME;
                break;
            case ekANCHOR_AT_CHARACTER:
                anchorType = css::text::TextContentAnchorType::TextContentAnchorType_AT_CHARACTER;
                break;
            }

            switch(halign) {
            case ekHALIGN_LEFT:
                horient = css::text::HoriOrientation::LEFT;
                break;
            case ekHALIGN_CENTER:
                horient = css::text::HoriOrientation::CENTER;
                break;
            case ekHALIGN_RIGHT:
                horient = css::text::HoriOrientation::RIGHT;
                break;
            case ekHALIGN_JUSTIFY:
                horient = css::text::HoriOrientation::LEFT;
                break;
            }

            switch(valign) {
            case ekVALIGN_TOP:
                vorient = css::text::VertOrientation::TOP;
                break;
            case ekVALIGN_CENTER:
                vorient = css::text::VertOrientation::CENTER;
                break;
            case ekVALIGN_BOTTOM:
                vorient = css::text::VertOrientation::BOTTOM;
                break;
            }

            xProps->setPropertyValue("Graphic", css::uno::makeAny(xGraphic));
            xProps->setPropertyValue("AnchorType", css::uno::makeAny(anchorType));
            xProps->setPropertyValue("HoriOrient", css::uno::makeAny(horient));
            xProps->setPropertyValue("VertOrient", css::uno::makeAny(vorient));

            if (width != 0 && height != 0)
            {
                css::uno::Reference<css::drawing::XShape> xShape(xTextContent, css::uno::UNO_QUERY_THROW);
                xShape->setSize(css::awt::Size((sal_Int32)width, (sal_Int32)height));
            }
        }
        catch (css::uno::Exception&)
        {
            res = ekSDKRES_ACCESS_DOC_ERROR;
        }
    }

    if (res == ekSDKRES_OK)
    {
        try
        {
            css::uno::Reference<css::text::XTextRange> xTextRange = xText->getEnd();
            xText->insertTextContent(xTextRange, xTextContent, sal_False);
        }
        catch (css::uno::Exception&)
        {
            res = ekSDKRES_TEXT_ADD_ERROR;
        }
    }

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_insert_page_number(Writer *writer, const textspace_t space, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::text::XText> xText;
    css::uno::Reference<css::frame::XModel> xModel;
    css::uno::Reference<css::text::XTextContent> xTextContent;

    if (res == ekSDKRES_OK)
        res = i_get_text(writer, space, xText);

    // The document model (required for creating new text field objects)
    if (res == ekSDKRES_OK)
    {
        try
        {
            css::uno::Reference<css::text::XTextDocument> *xDocument = reinterpret_cast<css::uno::Reference<css::text::XTextDocument>*>(writer);
            xModel = css::uno::Reference<css::frame::XModel>(*xDocument, css::uno::UNO_QUERY_THROW);
        }
        catch (css::uno::Exception&)
        {
            res = ekSDKRES_ACCESS_DOC_ERROR;
        }
    }

    // Create the text content object
    if (res == ekSDKRES_OK)
        res = i_create_text_content(xModel, "com.sun.star.text.TextField.PageNumber", xTextContent);

    // Configure the text object and add the image graphic
    if (res == ekSDKRES_OK)
    {
        try
        {
            css::uno::Reference<css::beans::XPropertySet> xProps(xTextContent, css::uno::UNO_QUERY_THROW);
            sal_Int16 numberType = css::style::NumberingType::ARABIC;
            xProps->setPropertyValue("NumberingType", css::uno::makeAny(numberType));
            xProps->setPropertyValue("SubType", css::uno::makeAny(css::text::PageNumberType::PageNumberType_CURRENT));
        }
        catch (css::uno::Exception&)
        {
            res = ekSDKRES_ACCESS_DOC_ERROR;
        }
    }

    if (res == ekSDKRES_OK)
    {
        try
        {
            css::uno::Reference<css::text::XTextRange> xTextRange = xText->getEnd();
            xText->insertTextContent(xTextRange, xTextContent, sal_False);
        }
        catch (css::uno::Exception&)
        {
            res = ekSDKRES_TEXT_ADD_ERROR;
        }
    }

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

static void i_insert_control_character(Writer *writer, const textspace_t space, const css::style::BreakType &breakType, sal_Int16 ctrlchar, sdkres_t *err)
{
    sdkres_t res = ekSDKRES_OK;
    css::uno::Reference<css::text::XText> xText;

    if (res == ekSDKRES_OK)
        res = i_get_text(writer, space, xText);

    if (res == ekSDKRES_OK)
    {
        if (breakType != css::style::BreakType::BreakType_MAKE_FIXED_SIZE)
            res = i_set_text_property(xText, "BreakType", css::uno::makeAny(breakType));
    }

    if (res == ekSDKRES_OK)
    {
        try
        {
            css::uno::Reference<css::text::XTextRange> xTextRange = xText->getEnd();
            xText->insertControlCharacter(xTextRange, ctrlchar, sal_False);
        }
        catch (css::uno::Exception&)
        {
            res = ekSDKRES_TEXT_ADD_ERROR;
        }
    }

    ptr_assign(err, res);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_insert_new_line(Writer *writer, const textspace_t space, sdkres_t *err)
{
    i_insert_control_character(writer, space, css::style::BreakType::BreakType_MAKE_FIXED_SIZE, css::text::ControlCharacter::LINE_BREAK, err);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_insert_paragraph(Writer *writer, const textspace_t space, sdkres_t *err)
{
    i_insert_control_character(writer, space, css::style::BreakType::BreakType_NONE, css::text::ControlCharacter::APPEND_PARAGRAPH, err);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_insert_page_break(Writer *writer, sdkres_t *err)
{
    i_insert_control_character(writer, ekTEXT_SPACE_PAGE, css::style::BreakType::BreakType_PAGE_AFTER, css::text::ControlCharacter::APPEND_PARAGRAPH, err);
}

#else   /* !GTNAP_LIBREOFFICE */

/*---------------------------------------------------------------------------*/

void officesdk_finish(void)
{
}

/*---------------------------------------------------------------------------*/

sdkres_t officesdk_text_to_pdf(const char_t *src_pathname, const char_t *dest_pathname)
{
    unref(src_pathname);
    unref(dest_pathname);
    return ekSDKRES_OK;
}

/*---------------------------------------------------------------------------*/

const char_t* officesdk_error(const sdkres_t code)
{
    unref(code);
    return "";
}

/*---------------------------------------------------------------------------*/

void officesdk_browse_doc(const char_t *pathname, sdkres_t *err)
{
    unref(pathname);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

uint32_t officesdk_rgb(const uint8_t red, const uint8_t green, const uint8_t blue)
{
    unref(red);
    unref(green);
    unref(blue);
    return 0;
}

/*---------------------------------------------------------------------------*/

Sheet *officesdk_sheet_open(const char_t *pathname, sdkres_t *err)
{
    unref(pathname);
    ptr_assign(err, ekSDKRES_OK);
    return NULL;
}

/*---------------------------------------------------------------------------*/

Sheet *officesdk_sheet_create(sdkres_t *err)
{
    ptr_assign(err, ekSDKRES_OK);
    return NULL;
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_save(Sheet *sheet, const char_t *pathname, sdkres_t *err)
{
    unref(sheet);
    unref(pathname);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_pdf(Sheet *sheet, const char_t *pathname, sdkres_t *err)
{
    unref(sheet);
    unref(pathname);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_print(Sheet *sheet, const char_t *filename, const char_t *printer, const paperorient_t orient, const paperformat_t format, const uint32_t paper_width, const uint32_t paper_height, const uint32_t num_copies, const bool_t collate_copies, const char_t *pages, sdkres_t *err)
{
    unref(sheet);
    unref(filename);
    unref(printer);
    unref(orient);
    unref(format);
    unref(paper_width);
    unref(paper_height);
    unref(num_copies);
    unref(collate_copies);
    unref(pages);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_close(Sheet *sheet, sdkres_t *err)
{
    unref(sheet);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

uint32_t officesdk_sheet_add(Sheet *sheet, sdkres_t *err)
{
    unref(sheet);
    ptr_assign(err, ekSDKRES_OK);
    return 0;
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_name(Sheet *sheet, const uint32_t page, const char_t *name, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(name);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_protect(Sheet *sheet, const uint32_t page, const bool_t protect, const char_t *pass, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(protect);
    unref(pass);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_freeze(Sheet *sheet, const uint32_t page, const uint32_t ncols, const uint32_t nrows, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(ncols);
    unref(nrows);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

String *officesdk_sheet_cell_ref(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(col);
    unref(row);
    ptr_assign(err, ekSDKRES_OK);
    return str_c("");
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_text(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const char_t *text, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(col);
    unref(row);
    unref(text);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_value(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const real64_t value, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(col);
    unref(row);
    unref(value);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_date(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint8_t day, const uint8_t month, const int16_t year, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(col);
    unref(row);
    unref(day);
    unref(month);
    unref(year);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_formula(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const char_t *formula, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(col);
    unref(row);
    unref(formula);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_numformat(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const numformat_t format, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(col);
    unref(row);
    unref(format);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_font_family(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const char_t *font_family, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(col);
    unref(row);
    unref(font_family);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_font_size(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const real32_t font_size, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(col);
    unref(row);
    unref(font_size);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_bold(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t bold, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(col);
    unref(row);
    unref(bold);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_italic(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t italic, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(col);
    unref(row);
    unref(italic);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_halign(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const halign_t align, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(col);
    unref(row);
    unref(align);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_valign(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const valign_t align, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(col);
    unref(row);
    unref(align);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_wrap(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const bool_t wrapped, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(col);
    unref(row);
    unref(wrapped);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_color(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint32_t rgb, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(col);
    unref(row);
    unref(rgb);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_backcolor(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const uint32_t rgb, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(col);
    unref(row);
    unref(rgb);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cells_backcolor(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, const uint32_t rgb, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(st_col);
    unref(st_row);
    unref(ed_col);
    unref(ed_row);
    unref(rgb);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_image(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const char_t *image_path, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(col);
    unref(row);
    unref(image_path);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cell_border(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t row, const linestyle_t style, const uint32_t thickness, const uint32_t rgb, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(col);
    unref(row);
    unref(style);
    unref(thickness);
    unref(rgb);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cells_border(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, const linestyle_t style, const uint32_t thickness, const uint32_t rgb, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(st_col);
    unref(st_row);
    unref(ed_col);
    unref(ed_row);
    unref(style);
    unref(thickness);
    unref(rgb);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_cells_merge(Sheet *sheet, const uint32_t page, const uint32_t st_col, const uint32_t st_row, const uint32_t ed_col, const uint32_t ed_row, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(st_col);
    unref(st_row);
    unref(ed_col);
    unref(ed_row);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_column_visible(Sheet *sheet, const uint32_t page, const uint32_t col, const bool_t visible, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(col);
    unref(visible);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_column_optimal_width(Sheet *sheet, const uint32_t page, const uint32_t col, const bool_t optimal_width, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(col);
    unref(optimal_width);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_column_width(Sheet *sheet, const uint32_t page, const uint32_t col, const uint32_t width, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(col);
    unref(width);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_row_visible(Sheet *sheet, const uint32_t page, const uint32_t row, const bool_t visible, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(row);
    unref(visible);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_row_optimal_height(Sheet *sheet, const uint32_t page, const uint32_t row, const bool_t optimal_height, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(row);
    unref(optimal_height);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_sheet_row_height(Sheet *sheet, const uint32_t page, const uint32_t row, const uint32_t height, sdkres_t *err)
{
    unref(sheet);
    unref(page);
    unref(row);
    unref(height);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

Writer *officesdk_writer_open(const char_t *pathname, sdkres_t *err)
{
    unref(pathname);
    ptr_assign(err, ekSDKRES_OK);
    return NULL;
}

/*---------------------------------------------------------------------------*/

Writer *officesdk_writer_create(sdkres_t *err)
{
    ptr_assign(err, ekSDKRES_OK);
    return NULL;
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_save(Writer *writer, const char_t *pathname, sdkres_t *err)
{
    unref(writer);
    unref(pathname);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_pdf(Writer *writer, const char_t *pathname, sdkres_t *err)
{
    unref(writer);
    unref(pathname);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_print(Writer *writer, const char_t *filename, const char_t *printer, const paperorient_t orient, const paperformat_t format, const uint32_t paper_width, const uint32_t paper_height, const uint32_t num_copies, const bool_t collate_copies, const char_t *pages, sdkres_t *err)
{
    unref(writer);
    unref(filename);
    unref(printer);
    unref(orient);
    unref(format);
    unref(paper_width);
    unref(paper_height);
    unref(num_copies);
    unref(collate_copies);
    unref(pages);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_close(Writer *writer, sdkres_t *err)
{
    unref(writer);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_page_header_show(Writer *writer, const bool_t show, sdkres_t *err)
{
    unref(writer);
    unref(show);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_page_header_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t spacing, const uint32_t height, const bool_t dynamic_spacing, const bool_t dynamic_height, sdkres_t *err)
{
    unref(writer);
    unref(left);
    unref(right);
    unref(spacing);
    unref(height);
    unref(dynamic_spacing);
    unref(dynamic_height);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_page_footer_show(Writer *writer, const bool_t show, sdkres_t *err)
{
    unref(writer);
    unref(show);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_page_footer_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t spacing, const uint32_t height, const bool_t dynamic_spacing, const bool_t dynamic_height, sdkres_t *err)
{
    unref(writer);
    unref(left);
    unref(right);
    unref(spacing);
    unref(height);
    unref(dynamic_spacing);
    unref(dynamic_height);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_page_margins(Writer *writer, const uint32_t left, const uint32_t right, const uint32_t top, const uint32_t bottom, const uint32_t gutter, sdkres_t *err)
{
    unref(writer);
    unref(left);
    unref(right);
    unref(top);
    unref(bottom);
    unref(gutter);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_font_family(Writer *writer, const textspace_t space, const char_t *font_family, sdkres_t *err)
{
    unref(writer);
    unref(space);
    unref(font_family);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_font_size(Writer *writer, const textspace_t space, const real32_t font_size, sdkres_t *err)
{
    unref(writer);
    unref(space);
    unref(font_size);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_bold(Writer *writer, const textspace_t space, const bool_t bold, sdkres_t *err)
{
    unref(writer);
    unref(space);
    unref(bold);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_italic(Writer *writer, const textspace_t space, const bool_t italic, sdkres_t *err)
{
    unref(writer);
    unref(space);
    unref(italic);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_paragraph_halign(Writer *writer, const textspace_t space, const halign_t align, sdkres_t *err)
{
    unref(writer);
    unref(space);
    unref(align);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_paragraph_lspacing(Writer *writer, const textspace_t space, const uint32_t height, sdkres_t *err)
{
    unref(writer);
    unref(space);
    unref(height);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_insert_text(Writer *writer, const textspace_t space, const char_t *text, sdkres_t *err)
{
    unref(writer);
    unref(space);
    unref(text);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_insert_image(Writer *writer, const textspace_t space, const anchortype_t anchor, const uint32_t width, const uint32_t height, const halign_t halign, const valign_t valign, const char_t *image_path, sdkres_t *err)
{
    unref(writer);
    unref(space);
    unref(anchor);
    unref(width);
    unref(height);
    unref(halign);
    unref(valign);
    unref(image_path);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_insert_page_number(Writer *writer, const textspace_t space, sdkres_t *err)
{
    unref(writer);
    unref(space);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_insert_new_line(Writer *writer, const textspace_t space, sdkres_t *err)
{
    unref(writer);
    unref(space);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_insert_paragraph(Writer *writer, const textspace_t space, sdkres_t *err)
{
    unref(writer);
    unref(space);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

void officesdk_writer_insert_page_break(Writer *writer, sdkres_t *err)
{
    unref(writer);
    ptr_assign(err, ekSDKRES_OK);
}

/*---------------------------------------------------------------------------*/

#endif  /* GTNAP_LIBREOFFICE */
