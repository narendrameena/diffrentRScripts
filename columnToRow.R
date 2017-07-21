

df <- read.csv("macdoanalsCombinedTime.csv", header = T, stringsAsFactors = F)

head(df)


s <- strsplit(df$policies_violated, split = "\n")
#value <- strsplit(toString(df$policies_violated_confidence), split = "\n")
s
#value
result <- data.frame(Location = rep(df$Location, sapply(s, length)),
                     Age = rep(df$Age, sapply(s, length)),
                     Time = rep(df$TotalTime, sapply(s, length)),
                     policies_violated = unlist(s),
                     #policies_violated:confidence = unlist(value),
                     city = rep(df$city, sapply(s, length)),
                     review =rep(df$review, sapply(s, length))
                     )

head(result)


# Write CSV in R
write.csv(result, file = "macdonalAnalysis.csv")