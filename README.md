# apng

Pure R package to create animated png files from png files.

## Installing (Latest GitHub W.I.P.)

```R
library(devtools)
install_github("qstokkink/apng")
```

## Installing (Latest Official C-RAN Release)

```R
install.packages("apng")
```

## Using

```R
library(apng)

apng(my_vector_of_file_names)
```

## Examples

```R
library(apng)

pdf(NULL)
png(filename="1.png")
plot(1:40, (1:40)^2)
dev.off()
png(filename="2.png")
plot(1:40, (-1*1:40)^3)
dev.off()
apng(c("1.png", "2.png"))
```

![Example output](https://github.com/qstokkink/qstokkink.github.io/blob/master/output.png)

```R
x <- 1:40
frame_count <- 40

for (i in 1:frame_count) {
	png(filename=paste(i, ".png"))
	plot(x, sin((i/x)%% 1) * (-1)^x,
		ylim=c(-1,1),
		col = ifelse(x == i, "red", "black"),
		pch = ifelse(x == i, max((-1)^i, 0)*5, ifelse(x > i, 8, 1)))
	dev.off()
}

apng(paste(1:frame_count, ".png"))
```

![Example output2](https://github.com/qstokkink/qstokkink.github.io/blob/master/output2.png)
