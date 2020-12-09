# Base method to read some bytes from a file
# Errors if the EOF is detected
# Returns c() if count == 0, otherwise the read bytes as raw
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

# Read the 8 byte PNG signature and return it
READ_PNG_SIGNATURE <- function(f) {
    return(READ_BYTES(f, 8))
}

# Read an entire chunk from a file and return it
READ_CHUNK <- function(f) {
    chunk <- list()

    chunk[['length']] <- READ_BYTES(f, 4)
    chunk[['type']] <- READ_BYTES(f, 4)

    i_chunk_length <- int_from_4_bytes(chunk[['length']])

    chunk[['data']] <- READ_BYTES(f, i_chunk_length)
    chunk[['crc']] <- READ_BYTES(f, 4)

    return(list(chunk))
}

# Read all remaining chunks in a file and return them
READ_REMAINING_CHUNKS <- function(f) {
    all_chunks <- list()

    while (T) {
        chunk <- tryCatch(READ_CHUNK(f), error=function(e){return(c())})
        if (length(chunk) == 0) {
            break
        }
        t <- as.integer(chunk[[1]][['type']])
        if (all(t == c(73, 72, 68, 82)) || all(t == c(73, 68, 65, 84)) || all(t == c(73, 69, 78, 68)) || all(t == c(80, 76, 84, 69))) {
            all_chunks <- append(all_chunks, chunk)
        } else {
            warning(paste("IGNORING PNG CHUNK TYPE", rawToChar(chunk[[1]][['type']]), "DATA LENGTH:", int_from_4_bytes(chunk[[1]][['length']])))
        }
    }

    return(all_chunks)
}

# Parse an IHDR chunk and add its parsed contents to chunk[['ihdr']]
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

# Read a full PNG file and return a parsed description.
# Returns a list with members:
#  signature: PNG signature
#  ihdr: IHDR chunk
#  idats: list of IDAT chunks
#  iend: IEND chunk
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
