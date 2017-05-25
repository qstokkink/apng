# Validate and amend user input to form a complete set of options
GENERATE_OPTIONS <- function(input_files=c(), output_file="output.png", num_plays=0, delay_num=0, delay_den=0, dispose_op=APNG_DISPOSE_OP_NONE, blend_op=APNG_BLEND_OP_SOURCE) {
    options <- list()

    options[['OUTPUT_FILE']] <- output_file

    stopifnot(file.access(input_files, 4) == 0)
    options[['INPUT_FILES']] <- input_files

    stopifnot(num_plays >= 0)
    options[['NUM_PLAYS']] <- num_plays

    stopifnot(delay_num >= 0)
    options[['DELAY_NUM']] <- delay_num

    stopifnot(delay_den >= 0)
    options[['DELAY_DEN']] <- delay_den

    stopifnot(dispose_op >= 0, dispose_op <= 2)
    options[['DISPOSE_OP']] <- dispose_op

    stopifnot(blend_op >= 0, blend_op <= 1)
    options[['BLEND_OP']] <- blend_op

    return(options)
}
