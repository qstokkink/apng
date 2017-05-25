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
