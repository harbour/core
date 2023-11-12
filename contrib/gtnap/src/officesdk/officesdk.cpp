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
    //Proc *liboffice_proc;
    //String *liboff_path;
    //css::uno::Reference<css::uno::XComponentContext> xComponentContext;
    css::uno::Reference<css::uno::XInterface> xInterface;
    //css::uno::Reference<css::bridge::XUnoUrlResolver> xResolver;
    css::uno::Reference<css::beans::XPropertySet> xPropSet;// = css::uno::Reference<css::beans::XPropertySet>(this->xInterface, css::uno::UNO_QUERY);

    css::uno::Reference<css::uno::XComponentContext> xComponentContext;
    css::uno::Reference<css::lang::XMultiComponentFactory> xMultiComponentFactoryClient;
    css::uno::Reference<css::frame::XDesktop2> xComponentLoader;// = css::frame::Desktop::create(xComponentContext);

};

/*---------------------------------------------------------------------------*/

OfficeSdk::OfficeSdk()
{
    init = false;
    //liboffice_proc = NULL;
    //liboff_path = NULL;
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

// static sdkres_t i_exception(css::uno::Exception& e)
// Inherited by IllegalAccessibleComponentStateException, AuthenticationFailedException, InvalidArgumentException, InvalidContextException, InvalidCredentialException, InvalidPrincipalException, PersistenceFailureException, UnsupportedException, PrinterException, IllegalTypeException, IntrospectionException, NotRemoveableException, PropertyExistException, PropertyVetoException, UnknownPropertyException, BridgeExistsException, InvalidProtocolChangeException, MalformedDataException, MergeRecoveryRequest, CannotLoadConfigurationException, AlreadyAcceptingException, ConnectionSetupException, NoConnectException, ElementExistException, NoSuchElementException, UnsupportedFlavorException, DependencyException, DeploymentException, ExtensionRemovedException, InstallException, InvalidRemovedParameterException, LicenseException, PlatformException, VersionException, AmbigousFilterRequest, BrokenPackageRequest, ChangedByOthersRequest, ExoticFileLoadException, FilterOptionsRequest, LockedDocumentRequest, LockedOnSavingRequest, NoSuchFilterRequest, OwnLockOnDocumentRequest, UndoFailedException, GraphicFilterRequest, LinkageMisuseException, NoVisualAreaSizeException, ObjectSaveVetoException, UnreachableStateException, WrongStateException, IncompatibleTypesException, InvalidBindingStateException, DoubleInitializationException, TerminationVetoException, UnknownModuleException, MultipleCharsOutputException, IOException, WrongJavaVersionException, ClassNotFoundException, IllegalAccessException, IndexOutOfBoundsException, InvalidListenerException, ListenerExistException, NoSuchFieldException, NoSuchMethodException, NoSupportException, NullPointerException, ServiceNotRegisteredException, WrappedTargetException, LdapConnectionException, LdapGenericException, CannotActivateFactoryException, MailException, EncryptionNotAllowedException, NoEncryptionException, WrongPasswordException, ZipException, ParseException, QueryException, RepositoryException, InvalidTypeNameException, NoSuchTypeNameException, CannotRegisterImplementationException, InvalidRegistryException, InvalidValueException, MergeConflictException, VolatileContentDestroyedException, ScannerException, BasicErrorException, CannotConvertException, CannotCreateAdapterException, LibraryNotLoadedException, ModuleSizeExceededRequest, ScriptErrorRaisedException, ScriptFrameworkErrorException, SQLException, NoConvergenceException, SystemShellExecuteException, ClassifiedInteractionRequest, ErrorCodeRequest, PDFExportException, InvalidTextContentException, AlreadyInitializedException, CommandAbortedException, CommandFailedException, ContentCreationException, DuplicateCommandIdentifierException, DuplicateProviderException, IllegalIdentifierException, InteractiveBadTransferURLException, ListenerAlreadySetException, MissingInputStreamException, MissingPropertiesException, ServiceNotFoundException, UnsupportedCommandException, UnsupportedDataSinkException, UnsupportedNameClashException, UnsupportedOpenModeException, ExecutableDialogException, RuntimeException, CloseVetoException, InvalidStateException, MalformedNumberFormatException, NotNumericException, VetoException, DOMException, EventException, SAXException, and XPathException.

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

        if (res == ekSDKRES_OK)
            res = KillLibreOffice();

        // WakeUp LibreOffice
        if (res == ekSDKRES_OK)
            res = WakeUpServer();

        // Wait a little to openoffice wake up
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
    css::uno::Reference<css::uno::XComponentContext> _xComponentContext(cppu::defaultBootstrap_InitialComponentContext());
    css::uno::Reference<css::lang::XMultiComponentFactory> _xMultiComponentFactoryClient(_xComponentContext->getServiceManager());

    this->xComponentContext = _xComponentContext;
    this->xMultiComponentFactoryClient = _xMultiComponentFactoryClient;
    this->xInterface = this->xMultiComponentFactoryClient->createInstanceWithContext("com.sun.star.bridge.UnoUrlResolver", this->xComponentContext);

    try
    {
        css::uno::Reference<css::bridge::XUnoUrlResolver> xResolver = css::uno::Reference<css::bridge::XUnoUrlResolver>(this->xInterface, css::uno::UNO_QUERY);
        this->xInterface = css::uno::Reference<css::uno::XInterface>(xResolver->resolve(sConnectionString), css::uno::UNO_QUERY_THROW);
    }
    catch (css::uno::Exception&)
    {
        res = ekSDKRES_CONECT_FAIL;
    }

    // Try to create a component loader
    if (res == ekSDKRES_OK)
    {
        css::uno::Reference<css::beans::XPropertySet> propSet = css::uno::Reference<css::beans::XPropertySet>(this->xInterface, css::uno::UNO_QUERY);
        propSet->getPropertyValue("DefaultContext") >>= this->xComponentContext;
        if (this->xComponentLoader.get() != nullptr)
            this->xComponentLoader->dispose();

        this->xComponentLoader = css::frame::Desktop::create(this->xComponentContext);
        if (this->xComponentLoader.get() == nullptr)
            res = ekSDKRES_COMPONENT_LOADER;
    }

    return res;
}

/*---------------------------------------------------------------------------*/
#include <iostream>
sdkres_t OfficeSdk::OpenTextDocument(const char_t *url, css::uno::Reference<css::text::XTextDocument> &xDocument)
{
    sdkres_t res = ekSDKRES_OK;
    ::rtl::OUString docUrl =  "file://" + i_OUStringFromUTF8(url);


    try
    {
        // rtl::OUString sConnectionString("uno:socket,host=localhost,port=2083;urp;StarOffice.ServiceManager");
        // css::uno::Reference<css::uno::XComponentContext> xComponentContext(cppu::defaultBootstrap_InitialComponentContext());
        // css::uno::Reference<css::lang::XMultiComponentFactory> xMultiComponentFactoryClient(xComponentContext->getServiceManager());
        // css::uno::Reference<css::uno::XInterface> xInterface = xMultiComponentFactoryClient->createInstanceWithContext("com.sun.star.bridge.UnoUrlResolver", xComponentContext);
        // css::uno::Reference<css::bridge::XUnoUrlResolver> resolver = css::uno::Reference<css::bridge::XUnoUrlResolver>(xInterface, css::uno::UNO_QUERY);
        // xInterface = css::uno::Reference<css::uno::XInterface>(resolver->resolve(sConnectionString), css::uno::UNO_QUERY_THROW);


        // css::uno::Reference<css::beans::XPropertySet> xPropSet = css::uno::Reference<css::beans::XPropertySet>(xInterface, css::uno::UNO_QUERY);
        // xPropSet->getPropertyValue("DefaultContext") >>= xComponentContext;
        // std::cout << "xComponentContext=" << xComponentContext.get() << std::endl;
        // //css::uno::Reference<css::lang::XMultiComponentFactory> xMultiComponentFactoryServer(xComponentContext->getServiceManager());
        // css::uno::Reference<css::frame::XDesktop2> xComponentLoader = css::frame::Desktop::create(xComponentContext);
        // css::uno::Sequence<css::beans::PropertyValue> loadProperties(1);
        // loadProperties[0].Name = "Hidden";
        // loadProperties[0].Value <<= true;

        // css::uno::Reference<css::lang::XComponent> xComponent = xComponentLoader->loadComponentFromURL(docUrl, "_blank", 0, loadProperties);
        // xDocument = css::uno::Reference<css::text::XTextDocument>(xComponent, css::uno::UNO_QUERY_THROW);

        css::uno::Sequence<css::beans::PropertyValue> loadProperties(1);
        loadProperties[0].Name = "Hidden";
        loadProperties[0].Value <<= true;
        css::uno::Reference<css::lang::XComponent> xComponent = this->xComponentLoader->loadComponentFromURL(docUrl, "_blank", 0, loadProperties);
        xDocument = css::uno::Reference<css::text::XTextDocument>(xComponent, css::uno::UNO_QUERY_THROW);




    }
    catch(css::uno::Exception &e)
    {
        std::cout << "LOAD EXCEPTION: " << e.Message << std::endl;
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
        std::cout << "SAVE EXCEPTION: " << e.Message << std::endl;
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
    // rtl::OUString sDocUrl("file:///home/fran/harbour_nappgui/contrib/gtnap/tests/unotest/test.odt"), sPDFUrl("file:///home/fran/harbour_nappgui/contrib/gtnap/tests/unotest/test.pdf");
    // rtl::OUString sConnectionString("uno:socket,host=localhost,port=2083;urp;StarOffice.ServiceManager");
    // rtl::Bootstrap::set("URE_MORE_TYPES", "file:///usr/lib/libreoffice/program/types/offapi.rdb");
    // rtl::Bootstrap::set("URE_BOOTSTRAP", "vnd.sun.star.pathname:/usr/lib/libreoffice/program/fundamentalrc");

    // if (i_OFFICE_SDK.xComponentContext.get() == nullptr)
    //     return ekSDKRES_NOENV;
    // else
        //return ekSDKRES_OK;
        //return ekSDKRES_NOENV;
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
