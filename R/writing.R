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
