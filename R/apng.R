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

##
#
# UTIL
#
##

APNG_DISPOSE_OP_NONE <- 0
APNG_DISPOSE_OP_BACKGROUND <- 1
APNG_DISPOSE_OP_PREVIOUS <- 2

APNG_BLEND_OP_SOURCE <- 0
APNG_BLEND_OP_OVER <- 1

int_from_4_bytes <- function(raw) {
    if (length(raw) == 0){
        return(0)
    }
    return(sum(as.integer(raw) * c(0x01000000, 0x010000, 0x0100, 1)))
}

int_to_4_bytes <- function(i) {
    hh <- bitShiftR(bitAnd(i, 0xff000000), 24)
    hl <- bitShiftR(bitAnd(i, 0xff0000), 16)
    lh <- bitShiftR(bitAnd(i, 0xff00), 8)
    ll <- bitAnd(i, 0xff)
    return(as.raw(c(hh, hl, lh, ll)))
}

int_to_2_bytes <- function(i) {
    h <- bitShiftR(bitAnd(i, 0xff00), 8)
    l <- bitAnd(i, 0xff)
    return(as.raw(c(h, l)))
}

int_to_1_byte <- function(i) {
    return(as.raw(c(i)))
}

##
#
# READING
#
##

READ_BYTES <- function(f, count){
    if (count == 0) {
        return(c())
    }
    r <- readBin(f, "raw", count, 1, F, "little")
    if (length(r) == 0){
        stop('EOF')
    }
    return(r)
}

READ_PNG_SIGNATURE <- function(f) {
    return(READ_BYTES(f, 8))
}

READ_CHUNK <- function(f) {
    chunk <- list()

    chunk[['length']] <- READ_BYTES(f, 4)
    chunk[['type']] <- READ_BYTES(f, 4)

    i_chunk_length <- int_from_4_bytes(chunk[['length']])

    chunk[['data']] <- READ_BYTES(f, i_chunk_length)
    chunk[['crc']] <- READ_BYTES(f, 4)

    return(list(chunk))
}

READ_REMAINING_CHUNKS <- function(f) {
    all_chunks <- list()

    while (T) {
        chunk <- tryCatch(READ_CHUNK(f), error=function(e){return(c())})
        if (length(chunk) == 0) {
            break
        }
        t <- as.integer(chunk[[1]][['type']])
        if ((t == c(73, 72, 68, 82)) || (t == c(73, 68, 65, 84)) || (t == c(73, 69, 78, 68)) || (t == c(80, 76, 84, 69))) {
            all_chunks <- append(all_chunks, chunk)
        } else {
            warning(paste("IGNORING PNG CHUNK TYPE", rawToChar(chunk[[1]][['type']]), "DATA LENGTH:", int_from_4_bytes(chunk[[1]][['length']])))
        }
    }

    return(all_chunks)
}

PARSE_IHDR <- function(chunk) {
    ihdr <- list()
    ihdr[['width']] <- int_from_4_bytes(chunk[['data']][1:4])
    ihdr[['height']] <- int_from_4_bytes(chunk[['data']][5:8])
    ihdr[['bit_depth']] <- as.integer(chunk[['data']][9])
    ihdr[['colour_type']] <- as.integer(chunk[['data']][10])
    ihdr[['compression_method']] <- as.integer(chunk[['data']][11])
    ihdr[['filter_method']] <- as.integer(chunk[['data']][12])
    ihdr[['interlace_method']] <- as.integer(chunk[['data']][13])
    
    chunk[['ihdr']] <- ihdr

    return(chunk)
}

READ_PNG <- function(f) {
    file_descriptor <- list()

    file_descriptor[['signature']] <- READ_PNG_SIGNATURE(f)

    all_chunks <- READ_REMAINING_CHUNKS(f)
    chunk_length <- length(all_chunks)

    file_descriptor[['idats']] <- list()
    file_descriptor[['ihdr']] <- PARSE_IHDR(all_chunks[[1]])
    for (i in 2:(chunk_length-1)){
        chunk <- all_chunks[[i]]
        if (all(as.integer(chunk[['type']]) == c(80, 76, 84, 69))){
            file_descriptor[['plte']] <- chunk
            warning("PNG CONTAINS A PALETTE, PALETTE-LESS PNG (ex. png(type=\"cairo-png\")) IS VERY MUCH SUGGESTED")
        } else {
            file_descriptor[['idats']] <- append(file_descriptor[['idats']], list(chunk))
        }
    }
    file_descriptor[['iend']] <- all_chunks[[chunk_length]]

    return(file_descriptor)
}

##
#
# CRC
#
##
local_env <- new.env(parent = baseenv())
local_env$CRC_TABLE <- vector(,256)
local_env$CRC_TABLE_COMPUTED <- F
MAKE_CRC_TABLE <- function() {
    for (n in 1:256) {
        c <- n - 1
        for (k in 1:8) {
            if (bitAnd(c, 1) == 1) {
                c <- bitXor(0xedb88320, bitShiftR(c, 1))
            } else {
                c <- bitShiftR(c, 1)
            }
        }
        local_env$CRC_TABLE[n] <- c
    }
    local_env$CRC_TABLE_COMPUTED <- T
}

UPDATE_CRC <- function(crc, raw) {
    c <- crc

    if (!local_env$CRC_TABLE_COMPUTED) {
        MAKE_CRC_TABLE()
    }

    for (n in 1:length(raw)) {
        c <- bitXor(local_env$CRC_TABLE[bitAnd(bitXor(c, raw[n]), 0xff) + 1], bitShiftR(c, 8))
    }

    return(c)
}

CRC <- function(raw) {
    int_crc <- bitXor(UPDATE_CRC(0xffffffff, raw), 0xffffffff)
    return(int_to_4_bytes(int_crc))
}

ADD_CRC_TO_CHUNK <- function(chunk) {
    type <- as.integer(chunk[['type']])
    data <- as.integer(chunk[['data']])
    my_crc <- CRC(c(type, data))
    chunk[['crc']] <- my_crc
    return(chunk)
}

##
#
# WRITING
#
##

WRITE_BYTES <- function(f, raw) {
    if (!is.null(raw)){
        writeBin(raw, f, 1, "little", T)
    }
}

WRITE_PNG_SIGNATURE <- function(f) {
    WRITE_BYTES(f, as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)))
}

WRITE_CHUNK <- function(f, chunk) {
    WRITE_BYTES(f, chunk[['length']])
    WRITE_BYTES(f, chunk[['type']])
    WRITE_BYTES(f, chunk[['data']])
    WRITE_BYTES(f, chunk[['crc']])
}

TO_CHUNK <- function(type, data) {
    chunk <- list()

    chunk[['length']] <- int_to_4_bytes(0)
    if (!is.null(data)){
        chunk[['length']] <- int_to_4_bytes(length(data))
    }

    chunk[['type']] <- type
    chunk[['data']] <- data
    chunk <- ADD_CRC_TO_CHUNK(chunk)

    return(chunk)
}

TO_ACTL_CHUNK <- function(num_frames, num_plays=0) {
    raw_num_frames <- int_to_4_bytes(num_frames)
    raw_num_plays <- int_to_4_bytes(num_plays)
    data <- c(raw_num_frames, raw_num_plays)
    type <- as.raw(c(97, 99, 84, 76))

    return(TO_CHUNK(type, data))
}

TO_FCTL_CHUNK <- function(sequence_number, width, height, x_offset=0, y_offset=0, delay_num=0, delay_den=0, dispose_op=APNG_DISPOSE_OP_NONE, blend_op=APNG_BLEND_OP_SOURCE) {
    raw_sequence_number <- int_to_4_bytes(sequence_number)
    raw_width <- int_to_4_bytes(width)
    raw_height <- int_to_4_bytes(height)
    raw_x_offset <- int_to_4_bytes(x_offset)
    raw_y_offset <- int_to_4_bytes(y_offset)
    raw_delay_num <- int_to_2_bytes(delay_num)
    raw_delay_den <- int_to_2_bytes(delay_den)
    raw_dispose_op <- int_to_1_byte(dispose_op)
    raw_blend_op <- int_to_1_byte(blend_op)
    data <- c(raw_sequence_number, raw_width, raw_height, raw_x_offset, raw_y_offset, raw_delay_num, raw_delay_den, raw_dispose_op, raw_blend_op)
    type <- as.raw(c(102, 99, 84, 76))

    return(TO_CHUNK(type, data))
}

TO_FDAT_CHUNK <- function(sequence_number, data) {
    raw_sequence_number <- int_to_4_bytes(sequence_number)
    type <- as.raw(c(102, 100, 65, 84))

    return(TO_CHUNK(type, c(raw_sequence_number, data)))
}

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
