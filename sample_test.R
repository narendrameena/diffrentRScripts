library('RUnit')

source('/Users/naru/Documents/R_workshop/runit.R')

test.suite <- defineTestSuite("example",
                              dirs = file.path("/Users/naru/Documents/R_workshop/tests"),
                              testFileRegexp = '^\\d+\\.R')


test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)



