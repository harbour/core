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
using namespace css::uno;
//using namespace css::beans;
//using namespace css::bridge;
//using namespace css::frame;
//using namespace css::lang;
//using namespace css::text;

int main()
{
    rtl::OUString sDocUrl("file:///home/fran/unotest/test.odt"), sPDFUrl("file:///home/fran/unotest/test.pdf");
    rtl::OUString sConnectionString("uno:socket,host=localhost,port=2083;urp;StarOffice.ServiceManager");
    rtl::Bootstrap::set("URE_MORE_TYPES", "file:///usr/lib/libreoffice/program/types/offapi.rdb");
    rtl::Bootstrap::set("URE_BOOTSTRAP", "vnd.sun.star.pathname:/usr/lib/libreoffice/program/fundamentalrc");

    css::uno::Reference<css::uno::XComponentContext> xComponentContext(cppu::defaultBootstrap_InitialComponentContext());
    auto xMultiComponentFactoryClient(xComponentContext->getServiceManager());
    auto xInterface = xMultiComponentFactoryClient->createInstanceWithContext(
            "com.sun.star.bridge.UnoUrlResolver", xComponentContext);
    auto resolver = Reference<css::bridge::XUnoUrlResolver>(xInterface, UNO_QUERY);
    try
    {
        xInterface = Reference<XInterface>(resolver->resolve(sConnectionString)
                                           , UNO_QUERY_THROW);
    }
    catch (Exception& e)
    {
        std::cout << "Error: cannot establish a connection using '"
             << sConnectionString << "'" << std::endl << e.Message << std::endl;
        exit(1);
    }

    auto xPropSet = Reference<XPropertySet>(xInterface, UNO_QUERY);
    xPropSet->getPropertyValue("DefaultContext") >>= xComponentContext;
    auto xMultiComponentFactoryServer(xComponentContext->getServiceManager());
    auto xComponentLoader = css::frame::Desktop::create(xComponentContext);
    Sequence<PropertyValue> loadProperties(1);
    loadProperties[0].Name = "Hidden";
    loadProperties[0].Value <<= true;
    try
    {
        auto xComponent = xComponentLoader->loadComponentFromURL(sDocUrl, "_blank"
                                   , 0, loadProperties);
        auto xDocument = Reference<css::text::XTextDocument>(xComponent, UNO_QUERY_THROW);
        auto xStorable = Reference<css::frame::XStorable>(xDocument, UNO_QUERY_THROW);
        auto storeProps = Sequence<PropertyValue>(3);
        storeProps[0].Name = "FilterName";
        storeProps[0].Value <<= rtl::OUString("writer_pdf_Export");
        storeProps[1].Name = "Overwrite";
        storeProps[1].Value <<= true;
        storeProps[2].Name = "SelectPdfVersion";
        storeProps[2].Value <<= 1;
        xStorable->storeToURL(sPDFUrl, storeProps);
    }
    catch(Exception& e) {
        std::cout << "Can not open the file ~/test.odt" << std::endl;
    }

    Reference<css::lang::XComponent>::query(xMultiComponentFactoryClient)->dispose();
    std::cout << "Output ~/test.pdf generated." << std::endl;
}
