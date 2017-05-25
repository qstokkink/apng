##
# 
# apng.R
# ----------
# Create an animated png from several source png images.
#
# Author: Quinten Stokkink
#
# Design documents used:
#  - https://en.wikipedia.org/wiki/APNG
#  - https://www.w3.org/TR/PNG/
#
##

apng <- function(files) {
    file_output <- file("output.png", "wb")
    sequence_number <- 0

    for (f in 1:length(files)) {
        file_input <- file(files[f], "rb")

        # [contents]
        #  signature: PNG signature
        #  ihdr: IHDR chunk
        #  idats: list of IDAT chunks
        #  iend: IEND chunk
        contents <- READ_PNG(file_input)

        ihdr_width <- contents[['ihdr']][['ihdr']][['width']]
        ihdr_height <- contents[['ihdr']][['ihdr']][['height']]

        if (f == 1){
            WRITE_PNG_SIGNATURE(file_output)
            WRITE_CHUNK(file_output, contents[['ihdr']])
            if (!is.null(contents[['plte']])) {
                WRITE_CHUNK(file_output, contents[['plte']])
            }
            WRITE_CHUNK(file_output, TO_ACTL_CHUNK(length(files)))
            WRITE_CHUNK(file_output, TO_FCTL_CHUNK(sequence_number, ihdr_width, ihdr_height))
            sequence_number <- sequence_number + 1
            for (idat in contents[['idats']]){
                WRITE_CHUNK(file_output, idat)
            }
        } else {
            WRITE_CHUNK(file_output, TO_FCTL_CHUNK(sequence_number, ihdr_width, ihdr_height))
            sequence_number <- sequence_number + 1
            for (idat in contents[['idats']]){
                WRITE_CHUNK(file_output, TO_FDAT_CHUNK(sequence_number, idat[['data']]))
                sequence_number <- sequence_number + 1
            }
        }

        if (f == length(files)){
            WRITE_CHUNK(file_output, contents[['iend']])
        }

        close(file_input)
    }
    close(file_output)
}

# EXAMPLE USAGE:
# apng(c("1.png", "2.png"))
