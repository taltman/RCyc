\name{taltman1.RCyc-package}
\alias{taltman1.RCyc-package}
\alias{taltman1.RCyc}
\docType{package}
\title{
RCyc: Pathway Bioinformatics for R
}
\description{
A package for interacting with Pathway Tools from R via a Unix (i.e.,
local) socket connection. The entirety of the Pathway Tools API is
available for computation or data extraction from pathway / genome
databases (PGDBs) installed with Pathway Tools.
}
\details{
\tabular{ll}{
Package: \tab taltman1.RCyc\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2012-03-21\cr
License: \tab Unlimited\cr
}

Pathway Tools is a suite of sofware for biochemical pathway
bioinformatics. It allows for the creation, curation, browsing, and
analysis of pathway / genome databases (PGDBs). There are over 1700
PGDBs built, including important model organisms such as Escherichia coli,
Saccharomyces cerevisiae, and Homo sapiens. There is
also MetaCyc, a database of experimentally-verified biochemical
reactions and pathways from across the tree of life.

The RCyc package provides a link between an R session and a Pathway
Tools server running in API mode. The two processes communicate over a
local (i.e., UNIX) filesystem socket. This was inspired by existing
client-side programs in Java and Perl (named, appropriately, JavaCyc and
PerlCyc) for interacting with the Pathway Tools API .

This package assumes that Pathway Tools has been set up on your system,
and that the user has sufficient privileges to execute Pathway
Tools. Visit the \href{http://www.biocyc.org/download.shtml}{Pathway Tools
  download page} for more information on obtaining Pathway Tools.

To start up Pathway Tools in API-mode (i.e., to set up the socket), one simply calls
`setUpPathwayToolsApiDaemon'. `CallPToolsFn' is used to
issue calls to the Pathway Tools API
functions. `shutDownPathwayToolsApiDaemon' terminates the socket by
instructing Pathway Tools to shut down.

The Pathway Tools API daemon is run over a local socket due to security
concerns. The daemon provides unrestricted access to the Pathway Tools
Common Lisp environment, which gives the client process access to
everything on the system that the user running the Pathway Tools API
daemon is able to access. 

}
\author{
Tomer Altman, Balasubramanian Narasimhan. C socket code adapted from
Dave Marshall (see rclient.c).

Maintainer: Tomer Altman <taltman@gmail.com>
}
\references{
\href{http://bib.oxfordjournals.org/content/11/1/40.abstract}{Pathway Tools version 13.0: integrated software for pathway/genome
informatics and systems biology}
\href{http://bioinformatics.ai.sri.com/ptools/api/}{Pathway Tools API
  Documentation}
\href{http://www.biocyc.org}{BioCyc Pathway/Genome Database Collection}
\href{http://arabidopsis.org/biocyc/perlcyc/index.jsp}{JavaCyc and PerlCyc client programs}
}
\keyword{ ~package }
\keyword{ ~interface }
\keyword{ ~connection }
\seealso{
\code{\link{setUpPathwayToolsApiDaemon}}, \code{\link{callPToolsFn}}, \code{\link{shutDownPathwayToolsApiDaemon}}
}
\examples{

setUpPathwayToolsApiDaemon(path="/home/taltman1/pathway-tools/")
%%,lisp.file="/home/taltman1/project/lisp/api-server.lisp")

## Give the daemon some time to start up
Sys.sleep(15)

callPToolsFn("so",list("'meta"))
callPToolsFn("get-slot-value",list("'proton", "'common-name"))
callPToolsFn("get-class-all-instances",list("'|Reactions|"))

shutDownPathwayToolsApiDaemon()
}
