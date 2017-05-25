\name{apng}
\alias{apng}
\title{apng}
\usage{
apng(...)
}
\arguments{
  \item{...}{A vector of file names}
}
\value{
Returns nothing, output is written to ``output.png''.
}
\description{
Combine multiple pngs.
}
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
