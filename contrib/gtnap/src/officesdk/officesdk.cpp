#include "officesdk.h"
#include <core/strings.h>
#include <osbs/bproc.h>
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

    sdkres_t WakeUpServer();

    sdkres_t ConnectServer();

public:
    bool init;
    Proc *liboffice_proc;
    //String *liboff_path;
    //css::uno::Reference<css::uno::XComponentContext> xComponentContext;
    css::uno::Reference<css::frame::XDesktop2> xComponentLoader;// = css::frame::Desktop::create(xComponentContext);

};

/*---------------------------------------------------------------------------*/

OfficeSdk::OfficeSdk()
{
    init = false;
    liboffice_proc = NULL;
    //liboff_path = NULL;
}

/*---------------------------------------------------------------------------*/

OfficeSdk::~OfficeSdk()
{
    if (this->liboffice_proc != NULL)
        bproc_close(&this->liboffice_proc);

    if (this->xComponentLoader.get() != nullptr)
    {
        this->xComponentLoader->dispose();
        this->xComponentLoader.set(nullptr);
    }
    //str_destopt(&liboff_path);
}

/*---------------------------------------------------------------------------*/

// static sdkres_t i_exception(css::uno::Exception& e)
// Inherited by IllegalAccessibleComponentStateException, AuthenticationFailedException, InvalidArgumentException, InvalidContextException, InvalidCredentialException, InvalidPrincipalException, PersistenceFailureException, UnsupportedException, PrinterException, IllegalTypeException, IntrospectionException, NotRemoveableException, PropertyExistException, PropertyVetoException, UnknownPropertyException, BridgeExistsException, InvalidProtocolChangeException, MalformedDataException, MergeRecoveryRequest, CannotLoadConfigurationException, AlreadyAcceptingException, ConnectionSetupException, NoConnectException, ElementExistException, NoSuchElementException, UnsupportedFlavorException, DependencyException, DeploymentException, ExtensionRemovedException, InstallException, InvalidRemovedParameterException, LicenseException, PlatformException, VersionException, AmbigousFilterRequest, BrokenPackageRequest, ChangedByOthersRequest, ExoticFileLoadException, FilterOptionsRequest, LockedDocumentRequest, LockedOnSavingRequest, NoSuchFilterRequest, OwnLockOnDocumentRequest, UndoFailedException, GraphicFilterRequest, LinkageMisuseException, NoVisualAreaSizeException, ObjectSaveVetoException, UnreachableStateException, WrongStateException, IncompatibleTypesException, InvalidBindingStateException, DoubleInitializationException, TerminationVetoException, UnknownModuleException, MultipleCharsOutputException, IOException, WrongJavaVersionException, ClassNotFoundException, IllegalAccessException, IndexOutOfBoundsException, InvalidListenerException, ListenerExistException, NoSuchFieldException, NoSuchMethodException, NoSupportException, NullPointerException, ServiceNotRegisteredException, WrappedTargetException, LdapConnectionException, LdapGenericException, CannotActivateFactoryException, MailException, EncryptionNotAllowedException, NoEncryptionException, WrongPasswordException, ZipException, ParseException, QueryException, RepositoryException, InvalidTypeNameException, NoSuchTypeNameException, CannotRegisterImplementationException, InvalidRegistryException, InvalidValueException, MergeConflictException, VolatileContentDestroyedException, ScannerException, BasicErrorException, CannotConvertException, CannotCreateAdapterException, LibraryNotLoadedException, ModuleSizeExceededRequest, ScriptErrorRaisedException, ScriptFrameworkErrorException, SQLException, NoConvergenceException, SystemShellExecuteException, ClassifiedInteractionRequest, ErrorCodeRequest, PDFExportException, InvalidTextContentException, AlreadyInitializedException, CommandAbortedException, CommandFailedException, ContentCreationException, DuplicateCommandIdentifierException, DuplicateProviderException, IllegalIdentifierException, InteractiveBadTransferURLException, ListenerAlreadySetException, MissingInputStreamException, MissingPropertiesException, ServiceNotFoundException, UnsupportedCommandException, UnsupportedDataSinkException, UnsupportedNameClashException, UnsupportedOpenModeException, ExecutableDialogException, RuntimeException, CloseVetoException, InvalidStateException, MalformedNumberFormatException, NotNumericException, VetoException, DOMException, EventException, SAXException, and XPathException.


/*---------------------------------------------------------------------------*/

#include <iostream>
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
            rtl::Bootstrap::set("URE_MORE_TYPES", rtl::OUString(tc(types), str_len(types), RTL_TEXTENCODING_UTF8));
            rtl::Bootstrap::set("URE_BOOTSTRAP", rtl::OUString(tc(boots), str_len(boots), RTL_TEXTENCODING_UTF8));
            str_destroy(&types);
            str_destroy(&boots);
        }

        // WakeUp LibreOffice
        if (res == ekSDKRES_OK)
            res = WakeUpServer();

        // Connect to LibreOffice instance
        if (res == ekSDKRES_OK)
            res = ConnectServer();

        if (res == ekSDKRES_OK)
            this->init = true;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

sdkres_t OfficeSdk::WakeUpServer()
{
    sdkres_t res = ekSDKRES_OK;

    // Wake up a background libreoffice proccess execution
    if (this->liboffice_proc == NULL)
    {
        const char_t *connect = "libreoffice \"--accept=socket,host=0,port=2083;urp;\" --invisible";
        this->liboffice_proc = bproc_exec(connect, NULL);
        if (this->liboffice_proc == NULL)
            return ekSDKRES_PROC_FAIL;
    }

    return res;
}

/*---------------------------------------------------------------------------*/

sdkres_t OfficeSdk::ConnectServer()
{
    sdkres_t res = ekSDKRES_OK;
    cassert(this->liboffice_proc != NULL);

    // Connection attempts
    for (size_t i = 0; i < 2; ++i)
    {
        rtl::OUString sConnectionString("uno:socket,host=localhost,port=2083;urp;StarOffice.ServiceManager");
        css::uno::Reference<css::uno::XComponentContext> xComponentContext(cppu::defaultBootstrap_InitialComponentContext());
        css::uno::Reference<css::lang::XMultiComponentFactory> xMultiComponentFactoryClient(xComponentContext->getServiceManager());
        css::uno::Reference<css::uno::XInterface> xInterface = xMultiComponentFactoryClient->createInstanceWithContext("com.sun.star.bridge.UnoUrlResolver", xComponentContext);
        css::uno::Reference<css::bridge::XUnoUrlResolver> resolver = css::uno::Reference<css::bridge::XUnoUrlResolver>(xInterface, css::uno::UNO_QUERY);

        try
        {
            xInterface = css::uno::Reference<css::uno::XInterface>(resolver->resolve(sConnectionString), css::uno::UNO_QUERY_THROW);
        }
        catch (css::uno::Exception&)
        {
            res = ekSDKRES_CONECT_FAIL;
        }

        // Try to create a component loader
        if (res == ekSDKRES_OK)
        {
            css::uno::Reference<css::beans::XPropertySet> xPropSet = css::uno::Reference<css::beans::XPropertySet>(xInterface, css::uno::UNO_QUERY);
            xPropSet->getPropertyValue("DefaultContext") >>= xComponentContext;
            if (this->xComponentLoader.get() != nullptr)
                this->xComponentLoader->dispose();

            this->xComponentLoader = css::frame::Desktop::create(xComponentContext);
            if (this->xComponentLoader.get() == nullptr)
                res = ekSDKRES_COMPONENT_LOADER;

            // No more attemps
            if (res == ekSDKRES_OK)
                break;
        }
        // Try to wake up the server again
        else
        {
            cassert(res == ekSDKRES_CONECT_FAIL);
            bproc_close(&this->liboffice_proc);
            WakeUpServer();
        }
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
    cassert_no_null(src_pathname);
    cassert_no_null(dest_pathname);
    res = i_OFFICE_SDK.Init();
    if (res == ekSDKRES_OK)
    {

    }

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
    case ekSDKRES_PROC_FAIL:
        return "Failed to launch LibreOffice server";
    case ekSDKRES_CONECT_FAIL:
        return "Failed to connect LibreOffice server";
    case ekSDKRES_COMPONENT_LOADER:
        return "Failed to create a component loader";
    cassert_default();
    }
    return "";
}

/*---------------------------------------------------------------------------*/
