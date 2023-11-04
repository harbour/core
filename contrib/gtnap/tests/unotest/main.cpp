#include <iostream>
#include <cppuhelper/bootstrap.hxx>
#include <rtl/bootstrap.hxx>
#include <beans/XPropertySet.hpp>
#include <bridge/XUnoUrlResolver.hpp>
#include <frame/Desktop.hpp>
#include <frame/XComponentLoader.hpp>
#include <frame/XStorable.hpp>
#include <lang/XMultiComponentFactory.hpp>
#include <text/XTextDocument.hpp>

// libreoffice "--accept=socket,host=0,port=2083;urp;" --invisible
//using namespace std;
//using namespace cppu;
//using namespace rtl;

// css == com.sun.star
//using namespace css::uno;
//using namespace css::beans;
//using namespace css::bridge;
//using namespace css::frame;
//using namespace css::lang;
//using namespace css::text;

int main()
{

    rtl::OUString sDocUrl("file:///home/fran/harbour_nappgui/contrib/gtnap/tests/unotest/test.odt"), sPDFUrl("file:///home/fran/harbour_nappgui/contrib/gtnap/tests/unotest/test.pdf");
    rtl::OUString sConnectionString("uno:socket,host=localhost,port=2083;urp;StarOffice.ServiceManager");
    rtl::Bootstrap::set("URE_MORE_TYPES", "file:///usr/lib/libreoffice/program/types/offapi.rdb");
    rtl::Bootstrap::set("URE_BOOTSTRAP", "vnd.sun.star.pathname:/usr/lib/libreoffice/program/fundamentalrc");

    css::uno::Reference<css::uno::XComponentContext> xComponentContext(cppu::defaultBootstrap_InitialComponentContext());
    css::uno::Reference<css::lang::XMultiComponentFactory> xMultiComponentFactoryClient(xComponentContext->getServiceManager());
    css::uno::Reference<css::uno::XInterface> xInterface = xMultiComponentFactoryClient->createInstanceWithContext("com.sun.star.bridge.UnoUrlResolver", xComponentContext);
    css::uno::Reference<css::bridge::XUnoUrlResolver> resolver = css::uno::Reference<css::bridge::XUnoUrlResolver>(xInterface, css::uno::UNO_QUERY);

    std::cout << "xComponentContext=" << xComponentContext.get() << std::endl;
    try
    {
        xInterface = css::uno::Reference<css::uno::XInterface>(resolver->resolve(sConnectionString), css::uno::UNO_QUERY_THROW);
    }
    catch (css::uno::Exception& e)
    {
        std::cout << "Error: cannot establish a connection using '" << sConnectionString << "'" << std::endl << e.Message << std::endl;
        exit(1);
    }

    css::uno::Reference<css::beans::XPropertySet> xPropSet = css::uno::Reference<css::beans::XPropertySet>(xInterface, css::uno::UNO_QUERY);
    xPropSet->getPropertyValue("DefaultContext") >>= xComponentContext;
    std::cout << "xComponentContext=" << xComponentContext.get() << std::endl;
    //css::uno::Reference<css::lang::XMultiComponentFactory> xMultiComponentFactoryServer(xComponentContext->getServiceManager());
    css::uno::Reference<css::frame::XDesktop2> xComponentLoader = css::frame::Desktop::create(xComponentContext);
    css::uno::Sequence<css::beans::PropertyValue> loadProperties(1);
    loadProperties[0].Name = "Hidden";
    loadProperties[0].Value <<= true;
    try
    {
        css::uno::Reference<css::lang::XComponent> xComponent = xComponentLoader->loadComponentFromURL(sDocUrl, "_blank", 0, loadProperties);
        css::uno::Reference<css::text::XTextDocument> xDocument = css::uno::Reference<css::text::XTextDocument>(xComponent, css::uno::UNO_QUERY_THROW);
        css::uno::Reference<css::frame::XStorable> xStorable = css::uno::Reference<css::frame::XStorable>(xDocument, css::uno::UNO_QUERY_THROW);
        css::uno::Sequence<css::beans::PropertyValue> storeProps = css::uno::Sequence<css::beans::PropertyValue>(3);
        storeProps[0].Name = "FilterName";
        storeProps[0].Value <<= rtl::OUString("writer_pdf_Export");
        storeProps[1].Name = "Overwrite";
        storeProps[1].Value <<= true;
        storeProps[2].Name = "SelectPdfVersion";
        storeProps[2].Value <<= 1;
        xStorable->storeToURL(sPDFUrl, storeProps);

        // This remove the lock.test.odt file
        xComponent->dispose();
    }
    catch(css::uno::Exception& e) {
        std::cout << "Can not open the file ~/test.odt" << std::endl;
    }

    css::uno::Reference<css::lang::XComponent>::query(xMultiComponentFactoryClient)->dispose();
    std::cout << "Output ~/test.pdf generated." << std::endl;
}
