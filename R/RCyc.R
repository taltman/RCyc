## RCyc: Bidirectional communication between R and Pathway Tools
## Copyright (c) 2012 Tomer Altman
## Issued under the MIT license (see LICENSE file)

#### Code documentation:

## R package for interacting with Pathway Tools via a UNIX socket.

## Table of Contents
## (each section starts with a line beginning with '####')
##
## * Code documentation
## * TODOs
## * Library statements
## * User-facing code for calling PTools API functions
## * Setting up or terminating the PTools API daemon
## * Sending XML to and from the socket
## * Automatically creating R functions for PTools API functions

#### TODO:
## * Use more elegant means of creating XML rather than string concatenation.
## * Support more R data structures beyond lists for passing to and from PTools
## * Extract XML for fn definitions via socket
## * Dynamically export functions from PTools to RCyc
## * Use StatDataML package to auto-create XML for R data structures. Then, use something
##   like XSLT to auto-transform it into the format that PTools can process.
## * Instead of a nondescriptive <atom> tag, make use of full Common Lisp object ontology in order to remove guess-work in translating the data into R objects.
## * Come up with DTD for XML data transfer language used
## * Check to see if we need to close the Unix socket file handle
##   after we shut down Pathway Tools API server.

#### Library statements:
library(XML)


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



#### Setting up or terminating the PTools API daemon:

##dyn.load("~/project/socket_ex/libclient.so")

### setUpPathwayToolsApiDaemon
##
## Description:
## This function will start up Pathway Tools in API mode, along with
## loading an RCyc Common Lisp file that extends the Pathway Tools API
## socket communication code to allow XML-based communications.
##
## Arguments:
## path: A string providing the (full or relative) path to the
##       "pathway-tools" invocation script for starting the Pathway Tools
##       runtime. Optional if this script is already in the $PATH
##       environment variable for R.

setUpPathwayToolsApiDaemon <- function (path = "") {

  lisp.file <- paste(system.file(package="taltman1.RCyc"),
                     "/inst/lisp/api-server.lisp",sep="")
        
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


### shutDownPathwayToolsApiDaemon
##
## Description:
## Closes Pathway Tools by sending the Lisp "(exit)" command over the socket.
##
## TODO:
## * I think we need to add a routine to the C code to close the Unix
## socket file handle. Otherwise, it's left open by default until the R
## image closes, which could possibly be problematic. 

shutDownPathwayToolsApiDaemon <- function ()
  .lisp2socket("(exit)")
  
## Send in a lisp string, receive back R objects
## this will need tob e modified to allow auto-calling based on XML API published & auto-transformed R lists/objects.

#### Sending XML to and from the socket:

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

  ## Is the atom a string?
  if ( substr(rawString,1,2) == "\\\""
      &&
      substr(rawString,stringLength-1,stringLength) == "\\\"")
    strsplit(rawString,
             "\\\\\"")[[1]][2]

  ## Is the atom a numeric?
  else if ( ! is.na(as.numeric(rawString)))
    as.numeric(rawString)  

  ## Assumption: atom is a symbol.
  else

    ## If we reach here, we assume that we've encountered a Lisp symbol, and so we transform it into a R symbol:    
    as.symbol(rawString)

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


               
               
#### Automatically creating R functions for PTools API functions

### This code is not working as of yet.

## This function should extract the XML via the socket, not through some static file.  
.loadPtoolsApiXML <- function ()
  xmlInternalTreeParse(file=paste(system.file(package="taltman1.RCyc"),
                         "/inst/xml/ptools-api.xml"))


## Take a PTools API function signature in XML format,
## and return an R expression.

## I can't seem to get the constructed closures to work correctly.
## Punting for now.

## This function is too large and unwieldy. Let's break it up conceptually into parts:
## 1. Have template function expression
## 2. Come up with signature part of function
## 3. splice in signature part of function into template
## 4. eval expression, binding the function to a

## read the help for the following functions/concepts to get this right:
## symbol or name, quote or expression, parse, eval
## formals, body, function, call
## environment, assign

.createPToolsFn <- function (name) {

  ##fn.symbol <- as.symbol(name)
  new.fn <- function () {}
  
  function.body <- expression(
      
      
      ## resultList <- callPToolsFn(function.name="placeholder",
      ##                            args="placeholder",
      ##                            optional.args="placeholder",
      ##                            keyword.args="placeholder"),
      resultList <- list(function.name="placeholder",
                                 args="placeholder",
                                 optional.args="placeholder",
                                 keyword.args="placeholder"),

      return(resultList)
      )
  
  ## Modify "placeholder" strings here:

  body(new.fn) <- as.call(c(as.name("{"),function.body))

  ## Finally, place the new function as a binding in the global environment:
  assign(name, new.fn, envir=.GlobalEnv)

}

    
    
.xmlSignature2Rexpression <- function (xmlSignature) {

  ptools.function.name <- xmlValue(getNodeSet(xmlSignature,"atom")[[1]])
  r.function.name.symbol <- .atomXML2value(getNodeSet(xmlSignature,"atom")[[1]])
  
  argument.expressions <- xmlChildren(xmlChildren(xmlSignature)[[2]])
  
  args <- list()
  keyword.args.p <- FALSE
  keyword.args <- list()
  optional.args.p <- FALSE
  optional.args <- list()
  formal.args <- ""
  
  for ( signature.exp in argument.expressions ) {

    if ( .atomXML2value(signature.exp) == as.symbol("&optional") )
      optional.args.p <- TRUE
    else if ( .atomXML2value(signature.exp) == as.symbol("&key") )
      keyword.args.p <- TRUE
    else if ( optional.args.p &&
             length(getNodeSet(signature.exp, "sexpr"))) {
      opt.arg <- .atomXML2value(getNodeSet(signature.exp,"atom")[[1]])
      print(opt.arg)
      print(typeof(opt.arg))
      optional.args <- c(optional.args,
                         opt.arg)
      formal.args <- paste(formal.args,
                           tolower(as.character(opt.arg)),
                           "=, ",
                           sep="")
    }
    else if (optional.args.p) {
      opt.arg <- .atomXML2value(signature.exp)
      print(opt.arg)
      print(typeof(opt.arg))

      optional.args <- c(optional.args,
                         opt.arg)
      formal.args <- paste(formal.args,
                           tolower(as.character(opt.arg)),
                           "=, ",
                           sep="")
    }
    else if (keyword.args.p &&
             length(getNodeSet(signature.exp, "sexpr"))) {
      keyword.args <- c(keyword.args,
                        atomXML2value(getNodeSet(signature.exp,"atom")[[1]]))
      formal.args <- paste(formal.args,
                           tolower(as.character(atomXML2value(getNodeSet(signature.exp,"atom")[[1]]))),
                           "=, ",
                           sep="")
    }
    else if (keyword.args.p) {
      keyword.args <- c(keyword.args,
                        atomXML2value(signature.exp))
      formal.args <- paste(formal.args,
                           tolower(as.character(atomXML2value(signature.exp))),
                           "=, ",
                           sep="")
    }
    else {
      args <- c(args, atomXML2value(signature.exp))
      formal.args <- paste(formal.args,
                           tolower(as.character(atomXML2value(signature.exp))),
                           "=, ",
                           sep="")
    }
    
  } ## end for-loop 

  print(formal.args)
  
  formal.args <- eval(parse(text=paste("alist(",
                              substr(formal.args,
                                     1,
                                     nchar(formal.args)-2),
                              ")",
                              sep="")))
  print(typeof(formal.args[[1]]))
  
  fn.exp <- substitute(expression(function() {
    force(substring)
    print(substring)
    print(typeof(substring))
    print(force(optional.args.replace.me[[1]]))
    print(typeof(optional.args.replace.me[[1]]))

    my.opt.args <- optional.args.replace.me
    
    ptoolsApiXML <- ptoolsCall2XML(function.name=function.name.replace.me,
                                   args = args.replace.me,
                                   optional.args = my.opt.args,
                                   keyword.args = keyword.args.replace.me)
    
    .xmlReplReply(ptoolsApiXML)
  }),
                       list(function.name.replace.me = ptools.function.name,
                            args.replace.me = args,
                            optional.args.replace.me = optional.args,
                            keyword.args.replace.me = keyword.args))

  final.fn <- eval(eval(fn.exp))
  
  formals(final.fn) <- formal.args

  final.fn
  
}
