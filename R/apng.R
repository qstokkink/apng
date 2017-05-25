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

apng <- function(...) {
    options <- GENERATE_OPTIONS(...)
    file_output <- file(options[['OUTPUT_FILE']], "wb")
    sequence_number <- 0
    input_file_count <- length(options[['INPUT_FILES']])

    for (f in 1:input_file_count) {
        file_input <- file(options[['INPUT_FILES']][f], "rb")

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
            WRITE_CHUNK(file_output, TO_ACTL_CHUNK(input_file_count, options[['NUM_PLAYS']]))
            WRITE_CHUNK(file_output, TO_FCTL_CHUNK(sequence_number,
                                                   ihdr_width,
                                                   ihdr_height,
                                                   delay_num = options[['DELAY_NUM']],
                                                   delay_den = options[['DELAY_DEN']],
                                                   dispose_op = options[['DISPOSE_OP']],
                                                   blend_op = options[['BLEND_OP']]))
            sequence_number <- sequence_number + 1
            for (idat in contents[['idats']]){
                WRITE_CHUNK(file_output, idat)
            }
        } else {
            WRITE_CHUNK(file_output, TO_FCTL_CHUNK(sequence_number,
                                                   ihdr_width,
                                                   ihdr_height,
                                                   delay_num = options[['DELAY_NUM']],
                                                   delay_den = options[['DELAY_DEN']],
                                                   dispose_op = options[['DISPOSE_OP']],
                                                   blend_op = options[['BLEND_OP']]))
            sequence_number <- sequence_number + 1
            for (idat in contents[['idats']]){
                WRITE_CHUNK(file_output, TO_FDAT_CHUNK(sequence_number, idat[['data']]))
                sequence_number <- sequence_number + 1
            }
        }

        if (f == input_file_count){
            WRITE_CHUNK(file_output, contents[['iend']])
        }

        close(file_input)
    }
    close(file_output)
}

# EXAMPLE USAGE:
# apng(c("1.png", "2.png"))
