\name{setUpPathwayToolsApiDaemon}
\alias{setUpPathwayToolsApiDaemon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Start up Pathway Tools as a daemon in API mode. 
}
\description{
  Start Pathway Tools as an asynchronous process/daemon in API
  mode. Pathway Tools will create a socket by default at
  `/tmp/ptools-socket', so `/tmp' must be writeable. 
}
\usage{
setUpPathwayToolsApiDaemon(path = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
    If the executable `pathway-tools' is not set up on your system path,
    you may provide the full path to the executable, including a
    trailing slash character.
}
%   \item{lisp.file}{
%     If you wish to have Pathway Tools also load in a file of custom Common Lisp
%     commands, you may provide a path to a file of Common Lisp code to be
%     loaded (interpreted) by Pathway Tools at start up (and prior to the
%     set-up of the API socket).
% }
}
\details{
  Pathway Tools requires an X-window server in order to launch
  correctly. Assuming you are using R via X-windows, running this
  command will lead to the Pathway Tools window opening. It can be safely
  minimized or otherwise ignored for the duration of the usage of the
  Pathway Tools API daemon.
}
\value{
  No return value.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Tomer Altman
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{callPToolsFn}}, \code{\link{shutDownPathwayToolsApiDaemon}}
}
\examples{
\dontrun{

setUpPathwayToolsApiDaemon(path="/home/taltman1/pathway-tools/")
%%lisp.file="/home/taltman1/project/lisp/api-server.lisp")

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
\keyword{ ~interface }
\keyword{ ~connection }
