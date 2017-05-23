\name{apng-package}
\alias{apng-package}
\alias{apng}
\docType{package}
\title{
\packageTitle{apng}
}
\description{
\packageDescription{apng}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{apng}
\packageIndices{apng}
Call the apng function with a vector of file names (which should be png files) to convert them to a single animated png file.
}
\author{
\packageAuthor{apng}

Maintainer: \packageMaintainer{apng}
}
\keyword{ package }
\examples{
pdf(NULL)
png(filename="1.png", type="cairo-png")
plot(1:40, (1:40)^2)
dev.off()
png(filename="2.png", type="cairo-png")
plot(1:40, .5*(1:40)^2)
dev.off()
apng(c("1.png", "2.png"))
}
