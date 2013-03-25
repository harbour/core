/*
 Global data var declaration
*/

/**
 * Open a connection to the specified URL, which is
 * intended to provide an XML message.  The specified data
 * is sent to the server as parameters.  This is the same as
 * calling xmlOpen("POST", url, toSend, responseHandler).
 *
 * @param string url    The URL to connect to.
 * @param string toSend The data to send to the server; must be URL encoded.
 * @param function responseHandler The Javascript function handling server response.
 */
function xmlPost(url, toSend, responseHandler)
{
    StartProgress();
    xmlOpen("POST", url, toSend, responseHandler);
}

/**
 * Open a connection to the specified URL, which is
 * intended to provide an XML message.  No other data is
 * sent to the server.  This is the same as calling
 * xmlOpen("GET", url, null, responseHandler).
 *
 * @param string url    The URL to connect to.
 * @param function responseHandler The Javascript function handling server response.
 */
function xmlGet(url, responseHandler)
{
    StartProgress();
    xmlOpen("GET", url, null, responseHandler);
}

/**
 * Open a connection to the specified URL, which is
 * intended to respond with an XML message.
 *
 * @param string method The connection method; either "GET" or "POST".
 * @param string url    The URL to connect to.
 * @param string toSend The data to send to the server; must be URL encoded.
 * @param function responseHandler The Javascript function handling server response.
 */
function xmlOpen(method, url, toSend, responseHandler)
{
    req = null;
    if (window.XMLHttpRequest)
    {
        // browser has native support for XMLHttpRequest object
        req = new XMLHttpRequest();
    }
    else if (window.ActiveXObject)
    {
        // try XMLHTTP ActiveX (Internet Explorer) version
        req = new ActiveXObject("Microsoft.XMLHTTP");
    }

    if(req)
    {
        req.onreadystatechange = responseHandler;
        req.open(method, url, true);
        req.setRequestHeader("content-type","application/x-www-form-urlencoded");
        req.send(toSend);
    }
    else
    {
        alert('Your browser does not seem to support XMLHttpRequest.');
    }
}

/**
 * Gets the first child node of <code>parent</code> with the
 * specified tag name.
 *
 * @param parent the parent XML DOM node to search
 * @param tagName the tag name of the child node to search for
 */
function getNode(parent, tagName)
{
    var i;
    var max = parent.childNodes.length;

    // Check each child node
    for(i = 0; i < max; i++)
    {
        if(parent.childNodes[i].tagName)
        {
            if(parent.childNodes[i].tagName.toUpperCase() == tagName.toUpperCase())
            {
                // We found a matching child node; return it.
                return parent.childNodes[i];
            }
        }
    }
    // One was not found; return null
    return null;
}

/**
 * Gets the first child node of <code>parent</code> with the
 * specified tag name and whose value of the 'key' attribute
 * is <code>key</code>.
 *
 * @param parent the parent XML DOM node to search
 * @param tagName the tag name of the child nodes to search in
 * @param key the value of the 'key' attribute to search on
 */
function getNodesWithKey(parent, tagName, key)
{
    var i;
    var cellNodes = parent.getElementsByTagName(tagName);
    var max = cellNodes.length;

    // Check each cell node for the specified value for
    // the 'key' attribute
    for(i = 0; i < max; i++)
    {
        if(cellNodes[i].getAttribute('key') == key)
        {
            // We found a matching cell node; return it.
            return cellNodes[i];
        }
    }
    // One was not found; return null
    return null;
}

// ----- xslT functions --------------------------------------------------------------------

// Immediately try to load the xsl file asynchronously
var xsldocloaded = false;
var xsldoc;

function xslGet( xslfile )
{

    if (window.XSLTProcessor)
    {
        // support Mozilla/Gecko based browsers
        xsldoc = document.implementation.createDocument("", "", null);
        xsldoc.addEventListener("load", onXslLoad, false);
        xsldoc.load( xslfile );
    }
    else if(window.ActiveXObject)
    {
        // support Windows / ActiveX
        xsldoc = new ActiveXObject("Microsoft.XMLDOM");
        xsldoc.ondataavailable = onXslLoad;
        xsldoc.load( xslfile );
    }

}

function onXslLoad()
{
    // flag that the xsl is loaded
    xsldocloaded = true;
    //alert( "xsl loaded: " + xsldocloaded )
}

// ----- xslT to HTML functions -----------

function combine_XLM_XSLT_HTML( xlm, xsl, html, html_id )
{

    var swappableSection = html.getElementById( html_id );

    if (window.XSLTProcessor)
    {
        // support Mozilla/Gecko based browsers
        var xsltProcessor = new XSLTProcessor();
        xsltProcessor.importStylesheet( xsl );
        var outputXHTML = xsltProcessor.transformToFragment( xlm.responseXML, html );
        //alert( outputXHTML );
        swappableSection.innerHTML = "";
        swappableSection.appendChild( outputXHTML );
    }
    else if(window.ActiveXObject)
    {
        // support Windows/ActiveX enabled browsers
        var outputXHTML = xlm.responseXML.transformNode( xsl );
        //alert( outputXHTML );
        swappableSection.innerHTML = outputXHTML;
    }

}


// ----- show or hide a progress indicator -----

var progress = false;
var progressTimer = null;

// show a progress indicator if it takes longer...
function StartProgress()
{
  //alert( "progress = " + progress );
  progress = true;
  if (progressTimer != null)
    window.clearTimeout(progressTimer);
  progressTimer = window.setTimeout(ShowProgress, 220);
} // StartProgress


// hide any progress indicator soon.
function EndProgress()
{
  progress = false;
  if (progressTimer != null)
    window.clearTimeout(progressTimer);
  progressTimer = window.setTimeout(ShowProgress, 20);
} // EndProgress


// this function is called by a timer to show or hide a progress indicator
function ShowProgress()
{
  //alert( "Showprogress = " + progress );
  progressTimer = null;
  var a = document.getElementById("AjaxProgressIndicator");

  if (progress && (a != null)) {
    // just display the existing object
    a.style.top = document.documentElement.scrollTop + 2 + "px";
    a.style.display = "";

  } else if (progress) {

    // find a relative link to the ajaxcore folder containing ajax.js
    var path = "/images/"
    //for (var n in document.scripts) {
    //  s = document.scripts[n].src;
    //  if ((s != null) && (s.length >= 7) && (s.substr(s.length -7).toLowerCase() == "ajax.js"))
    //    path = s.substr(0,s.length -7);
    //} // for

    // create new standard progress object
    a = document.createElement("div");
    a.id = "AjaxProgressIndicator";
    a.style.position = "absolute";
    a.style.right = "2px";
    a.style.top = document.documentElement.scrollTop + 2 + "px";
    a.style.width = "130px";
    a.style.height = "16px"
    a.style.padding = "2px";
    a.style.verticalAlign = "bottom";
    a.style.backgroundColor="#9FCDFF";

    a.innerHTML = "<img style='vertical-align:bottom' src='" + path + "ajax-loader.gif?a'>&nbsp;please wait...";
    document.body.appendChild(a);

  } else if (a) {
    a.style.display="none";
  } // if
} // ShowProgress
