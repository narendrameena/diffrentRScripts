library(futile.logger)
flog.info("My first log statement with futile.logger")
flog.warn("This statement has higher severity")
flog.fatal("This one is really scary")



flog.info("This is my %s log statement, %s", 'second', 'ever')
x <- rnorm(400)
flog.info("The variance of the sample is %s", var(x))

flog.threshold(WARN)
flog.info("Log statement %s is hidden!", 3)
flog.warn("However warning messages will still appear")

flog.threshold(ERROR)
flog.info("Will print: %s", FALSE)
flog.warn("Will print: %s", FALSE)
flog.error("Will print: %s", TRUE)
flog.fatal("Will print: %s", TRUE)