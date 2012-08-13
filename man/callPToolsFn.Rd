\name{callPToolsFn}
\alias{callPToolsFn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Call a Pathway Tools API function.

}
\description{
Call a Pathway Tools API function with R object arguments, and return an
R object result. Automatically supports use of optional and keyword arguments.
}
\usage{
callPToolsFn(function.name = "progn", args = list(), optional.args = list(), keyword.args = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{function.name}{
    A string representing the Pathway Tools API function name. There
    should be no leading apostrophe in the string. 
}
  \item{args}{
    A list of arguments that will be passed to the Pathway Tools API
    function. Argument values may be nested lists consisting only of
    numbers and strings. Any list names will be ignored. 
}
  \item{optional.args}{
    A list of arguments that will be passed to the Pathway Tools API
    function after the \code{args} list. Argument values may be nested lists consisting only of
    numbers and strings. Any list names will be ignored.
}
\item{keyword.args}{
  A list of arguments that will be passed to the Pathway Tools API
    function as keyword arguments. Argument values may be nested lists consisting only of
    numbers and strings. Any list names will be used to auto-generate a leading
    keyword indicator (i.e., the symbol starting with a colon that
    precedes the named argument). 

}
}
\details{
  
  If a return value (or part of a return value) is a Common Lisp symbol
  (such as a Pathway Tools frame identifier), it will be represented as
  a string literal with a leading apostrophe. While R has a concept of
  symbols, the syntax for R symbols is restricted in comparison with
  Common Lisp. For example, the Pathway Tools API makes heavy use of hyphens
  in symbol names, where this would lead to a syntax error in R. Instead
  of trying to auto-convert the symbols between two representations, it
  will be easier for users of both systems to keep the respresentation
  as similar in appearance as possible.

  All communication to and from the Pathway Tools daemon is handled via
  a simple XML format for representing nested lists of strings, numbers,
  and symbols. It is not possible
  to exchange other Common Lisp or R objects, such as arrays and hash
  tables, currently.
  
}
\value{
  A numeric, a string literal, a list, or a nested list consisting of
  lists, numerics, or string literals.
}
\references{
\href{http://bioinformatics.ai.sri.com/ptools/api/}{Pathway Tools API
  Documentation}
}
\author{
Tomer Altman
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{setUpPathwayToolsApiDaemon}}, \code{\link{shutDownPathwayToolsApiDaemon}}
}
\examples{
\dontrun{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

callPToolsFn("so",list("'meta"))
callPToolsFn("get-slot-value",list("'proton", "'common-name"))
callPToolsFn("get-class-all-instances",list("'|Reactions|"))

## The function is currently defined as
function (function.name = "progn", args = list(), optional.args = list(), 
    keyword.args = list()) 
{
    .xmlReplReply(.list2XML(c(list(function.name), args, optional.args, 
        keyword.args)))
  }
}


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~interface }
\keyword{ ~connection }