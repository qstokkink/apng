# Dispose OP enum values
APNG_DISPOSE_OP_NONE <- 0
APNG_DISPOSE_OP_BACKGROUND <- 1
APNG_DISPOSE_OP_PREVIOUS <- 2

# Blend OP enum values
APNG_BLEND_OP_SOURCE <- 0
APNG_BLEND_OP_OVER <- 1

# Convert a raw array of length 4 to an int
int_from_4_bytes <- function(raw) {
    if (length(raw) == 0){
        return(0)
    }
    return(sum(as.integer(raw) * c(0x01000000, 0x010000, 0x0100, 1)))
}

# Convert an int to a 4 byte raw array
int_to_4_bytes <- function(i) {
    hh <- bitShiftR(bitAnd(i, 0xff000000), 24)
    hl <- bitShiftR(bitAnd(i, 0xff0000), 16)
    lh <- bitShiftR(bitAnd(i, 0xff00), 8)
    ll <- bitAnd(i, 0xff)
    return(as.raw(c(hh, hl, lh, ll)))
}

# Convert an int to a 2 byte raw array
int_to_2_bytes <- function(i) {
    h <- bitShiftR(bitAnd(i, 0xff00), 8)
    l <- bitAnd(i, 0xff)
    return(as.raw(c(h, l)))
}

# Convert an int to a 1 byte raw array
int_to_1_byte <- function(i) {
    return(as.raw(c(i)))
}
