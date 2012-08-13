require(taltman1.RCyc)

setUpPathwayToolsApiDaemon(path="/home/taltman1/pathway-tools/")

## Give the daemon some time to start up
Sys.sleep(15)

stopifnot(identical(callPToolsFn("so",list("'meta")),"'T"),
          identical(callPToolsFn("get-slot-value",list("'proton", "'common-name")),"H+"),
          identical(callPToolsFn("get-class-direct-subs",list("'|Reactions|")),
                    c("'|Reactions-Classified-By-Conversion-Type|",
                      "'|Reactions-Classified-By-Substrate|",
                      "'|Unclassified-Reactions|")))

shutDownPathwayToolsApiDaemon()
