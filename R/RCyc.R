#### Code documentation:

## Code for interacting with Pathway Tools via a UNIX socket.

## TODO:
## * Use more elegant means of creating XML rather than string concatenation.
## * Support more R data structures beyond lists for passing to and from PTools
## * Dynamically export functions from PTools to RCyc
## * Use StatDataML package to auto-create XML for R data structures. Then, use something
##   like XSLT to auto-transform it into the format that PTools can process.
## * Come up with DTD for XML data transfer language used

library(XML)

#### Code for setting up or terminating the PTools API daemon:

##dyn.load("~/project/socket_ex/libclient.so")

setUpPathwayToolsApiDaemon <- function (path = "") {

  lisp.file <- paste(system.file(package="taltman1.RCyc"),
                     "/lisp/api-server.lisp",sep="")
        
  if ( lisp.file != "" )
    lisp.load.file.arg <- "-load "
  else
    lisp.load.file.arg <- ""
  
  system(paste(path,
               "pathway-tools ",
               lisp.load.file.arg,
               lisp.file,
               " -api ",
               sep=""),
         wait=FALSE,
         ignore.stdout = TRUE)
  print("Please note that Pathway Tools must have an attached X-Window display in order to function properly. Feel free to minimize the window once it has opened.")
}

shutDownPathwayToolsApiDaemon <- function ()
  .lisp2socket("(exit)")
  
## Send in a lisp string, receive back R objects
## this will need tob e modified to allow auto-calling based on XML API published & auto-transformed R lists/objects.

#### Code for sending XML to and from the socket:

.xmlReplReply <- function (sexpr.xml) {

  .XML2list(.socketStr2RXML(.xmlSocket(sexpr.xml)))
           
}

## A utility function for testing out the socket using Lisp directly:

.lispReplReply <- function (lispStr) {
  
  .XML2list(.socketStr2RXML(.lisp2socket(lispStr)))
  
}

.xmlSocket <- function (xml.string,
                       socket = "/tmp/ptools-socket") {
  
  wrapped.xml.string <- paste("(xml-repl \"",
                              xml.string,
                              "\"))\\n",
                              sep="")
  
  .Call("clientSocket", socket, c(wrapped.xml.string))
  
}

.lisp2socket <- function (lispString,
                         socket = "/tmp/ptools-socket") {

  wrappedLispString <- paste("(sexpr->sXML (deep-object-name ",
                             lispString,
                             "))\\n",
                             sep="")
  
  .Call("clientSocket", socket, c(wrappedLispString))

}

.socketStr2RXML <- function (string) {

  require(XML)
  sizeStr <- nchar(string)
  
  xmlInternalTreeParse(file = substr(string,
                         2,
                         nchar(string)-2),
                       asText=TRUE)  
}

.XML2list <- function (XMLObj) {

  require(XML)
  if ( length(getNodeSet(XMLObj,
                         "/sexpr")) == 0)
    unlist(xpathApply(XMLObj,"/atom",.atomXML2value))
  else
    unlist(xpathApply(XMLObj,"/sexpr/atom",.atomXML2value))

}

## Go from a Lisp value within <atom> tags to an atomic value understood by R:
## Currently supports strings, numerics, and symbols.

.atomXML2value <- function(atomXML) {

  require(XML)
  
  rawString <- xmlValue(atomXML)

  stringLength <- nchar(rawString)

  if ( substr(rawString,1,2) == "\\\""
      &&
      substr(rawString,stringLength-1,stringLength) == "\\\"")
    strsplit(rawString,
             "\\\\\"")[[1]][2]
  else if ( ! is.na(as.numeric(rawString)))
    as.numeric(rawString)
  else
    ## R is more picky on what characters are valid as object names,
    ## so we need to standardize the Lisp symbols:
    ## as.symbol(tolower(gsub("-",
    ##                        ".",
    ##                        rawString)))

    ## We tack on a leading apostrophe to indicate that this string is
    ## representing a Common Lisp symbol, and not a string literal:
    paste("'",rawString,sep="")
}

## Given a list, convert it into SEXPR XML:
## Supports nested lists.

.list2XML <- function (list) {

  if ( ! is.list(list) )

    paste("<atom>",
          as.character(list),
          "</atom>",
          sep="")

  else {

    list.names <- names(list)

    xml.string <- "<sexpr>"
  
    for ( i in 1:length(list) ) {

      current.name <- list.names[[i]]
      current.value <- list[[i]]

      if ( is.null(current.name) || current.name == "" )      
        if (is.list(current.value))
          xml.string <- paste(xml.string,
                              .list2XML(current.value),
                              sep="")
        else
          xml.string <- paste(xml.string,
                              "<atom>",
                              as.character(current.value),
                              "</atom>",
                              sep="")
      else
        if (is.list(current.value))
          xml.string <- paste(xml.string,
                              "<atom>:", ## colon added to create keyword syntax
                              current.name,
                              "</atom>",
                              .list2XML(current.value),
                              sep="")
        else
          xml.string <- paste(xml.string,
                              "<atom>:", ## colon added to create keyword syntax
                              current.name,
                              "</atom>",
                              "<atom>",
                              as.character(current.value),
                              "</atom>",
                              sep="")
    
    } ## end for-loop
  
    paste(xml.string,"</sexpr>",sep="")

  } ## end if-then-else
    
} ## end list2XML function


#### User-facing code for calling PTools API functions

callPToolsFn <- function(function.name = "progn",
                         args = list(),
                         optional.args = list(),
                         keyword.args = list() ) {
  
  .xmlReplReply(.list2XML(c(list(function.name),
                          args,
                          optional.args,
                          keyword.args)))
               
}
               
               
#### Code for automatically creating R functions for PTools API functions

### This code is not working as of yet.
  
##.loadPtoolsApiXML <- function ()
##  xmlInternalTreeParse(file="~/project/ptools-api.xml")
## XXX fix me: this path needs to be relative to the package.

## Take a PTools API function signature in XML format,
## and return an R expression.

## I can't seem to get the constructed closures to work correctly.
## Punting for now.

## .xmlSignature2Rexpression <- function (xmlSignature) {

##   ptools.function.name <- xmlValue(getNodeSet(xmlSignature,"atom")[[1]])
##   r.function.name.symbol <- .atomXML2value(getNodeSet(xmlSignature,"atom")[[1]])
  
##   argument.expressions <- xmlChildren(xmlChildren(xmlSignature)[[2]])
  
##   args <- list()
##   keyword.args.p <- FALSE
##   keyword.args <- list()
##   optional.args.p <- FALSE
##   optional.args <- list()
##   formal.args <- ""
  
##   for ( signature.exp in argument.expressions ) {

##     if ( .atomXML2value(signature.exp) == as.symbol("&optional") )
##       optional.args.p <- TRUE
##     else if ( .atomXML2value(signature.exp) == as.symbol("&key") )
##       keyword.args.p <- TRUE
##     else if ( optional.args.p &&
##              length(getNodeSet(signature.exp, "sexpr"))) {
##       opt.arg <- .atomXML2value(getNodeSet(signature.exp,"atom")[[1]])
##       print(opt.arg)
##       print(typeof(opt.arg))
##       optional.args <- c(optional.args,
##                          opt.arg)
##       formal.args <- paste(formal.args,
##                            tolower(as.character(opt.arg)),
##                            "=, ",
##                            sep="")
##     }
##     else if (optional.args.p) {
##       opt.arg <- .atomXML2value(signature.exp)
##       print(opt.arg)
##       print(typeof(opt.arg))

##       optional.args <- c(optional.args,
##                          opt.arg)
##       formal.args <- paste(formal.args,
##                            tolower(as.character(opt.arg)),
##                            "=, ",
##                            sep="")
##     }
##     else if (keyword.args.p &&
##              length(getNodeSet(signature.exp, "sexpr"))) {
##       keyword.args <- c(keyword.args,
##                         atomXML2value(getNodeSet(signature.exp,"atom")[[1]]))
##       formal.args <- paste(formal.args,
##                            tolower(as.character(atomXML2value(getNodeSet(signature.exp,"atom")[[1]]))),
##                            "=, ",
##                            sep="")
##     }
##     else if (keyword.args.p) {
##       keyword.args <- c(keyword.args,
##                         atomXML2value(signature.exp))
##       formal.args <- paste(formal.args,
##                            tolower(as.character(atomXML2value(signature.exp))),
##                            "=, ",
##                            sep="")
##     }
##     else {
##       args <- c(args, atomXML2value(signature.exp))
##       formal.args <- paste(formal.args,
##                            tolower(as.character(atomXML2value(signature.exp))),
##                            "=, ",
##                            sep="")
##     }
    
##   } ## end for-loop 

##   print(formal.args)
  
##   formal.args <- eval(parse(text=paste("alist(",
##                               substr(formal.args,
##                                      1,
##                                      nchar(formal.args)-2),
##                               ")",
##                               sep="")))
##   print(typeof(formal.args[[1]]))
  
##   fn.exp <- substitute(expression(function() {
##     force(substring)
##     print(substring)
##     print(typeof(substring))
##     print(force(optional.args.replace.me[[1]]))
##     print(typeof(optional.args.replace.me[[1]]))

##     my.opt.args <- optional.args.replace.me
    
##     ptoolsApiXML <- ptoolsCall2XML(function.name=function.name.replace.me,
##                                    args = args.replace.me,
##                                    optional.args = my.opt.args,
##                                    keyword.args = keyword.args.replace.me)
    
##     .xmlReplReply(ptoolsApiXML)
##   }),
##                        list(function.name.replace.me = ptools.function.name,
##                             args.replace.me = args,
##                             optional.args.replace.me = optional.args,
##                             keyword.args.replace.me = keyword.args))

##   final.fn <- eval(eval(fn.exp))
  
##   formals(final.fn) <- formal.args

##   final.fn
  
## }
