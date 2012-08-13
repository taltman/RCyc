\name{shutDownPathwayToolsApiDaemon}
\alias{shutDownPathwayToolsApiDaemon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Shut down Pathway Tools API daemon cleanly.
}
\description{
Sends a message to the Pathway Tools API daemon to shut down. 
}
\usage{
shutDownPathwayToolsApiDaemon()
}
%- maybe also 'usage' for other objects documented here.
\details{
When invoked, this function sends the string ``(exit)'' to the Pathway
Tools API daemon, which then initiates the clean termination of all
Pathway Tools Lisp processes and ends the process.
}
\value{
No value.
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
\code{\link{setUpPathwayToolsApiDaemon}}, \code{\link{callPToolsFn}}
}
\examples{
\dontrun{
shutDownPathwayToolsApiDaemon()
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~interface }
\keyword{ ~connection }
