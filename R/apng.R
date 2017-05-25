# Create an apng from png files
# Forwards num_plays to the acTL chunk
# Forwards delay_num .. blend_op to the fcTL chunks
apng <- function(input_files=c(), output_file="output.png", num_plays=0, delay_num=0, delay_den=0, dispose_op=APNG_DISPOSE_OP_NONE, blend_op=APNG_BLEND_OP_SOURCE) {
    options <- GENERATE_OPTIONS(input_files, output_file, num_plays, delay_num, delay_den, dispose_op, blend_op)
    file_output <- file(options[['OUTPUT_FILE']], "wb")
    sequence_number <- 0
    input_file_count <- length(options[['INPUT_FILES']])

    # Loop through the input files
    # Extract IDAT chunks from the inputs to form the new animated png
    # Extract other chunks as needed
    for (f in 1:input_file_count) {
        file_input <- file(options[['INPUT_FILES']][f], "rb")

        # [contents]
        #  signature: PNG signature
        #  ihdr: IHDR chunk
        #  idats: list of IDAT chunks
        #  iend: IEND chunk
        contents <- READ_PNG(file_input)

        # The IHDR contains the image width and height
        # Note that the 'ihdr' member of the 'ihdr' chunk contains the parsed info
        ihdr_width <- contents[['ihdr']][['ihdr']][['width']]
        ihdr_height <- contents[['ihdr']][['ihdr']][['height']]

        if (f == 1){
            # Write the header information if this is the first file we process
            # We need an IHDR chunk
            # We need an acTL chunk
            # If the source png included a PLTE, include it and hope for the best
            # Note that the first frame is an IDAT and not an fdAT
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
            # We don't need to convert the first IDAT chunks into fdAT chunks
            # So we can just write them like we read them
            for (idat in contents[['idats']]){
                WRITE_CHUNK(file_output, idat)
            }
        } else {
            # We are mid-stream
            # Write an fcTL chunk and an fdAT chunk
            WRITE_CHUNK(file_output, TO_FCTL_CHUNK(sequence_number,
                                                   ihdr_width,
                                                   ihdr_height,
                                                   delay_num = options[['DELAY_NUM']],
                                                   delay_den = options[['DELAY_DEN']],
                                                   dispose_op = options[['DISPOSE_OP']],
                                                   blend_op = options[['BLEND_OP']]))
            sequence_number <- sequence_number + 1
            # IDATs have to be renamed to fdATs and receive a sequence number
            for (idat in contents[['idats']]){
                WRITE_CHUNK(file_output, TO_FDAT_CHUNK(sequence_number, idat[['data']]))
                sequence_number <- sequence_number + 1
            }
        }

        if (f == input_file_count){
            # This is the last file to process, insert the IEND chunk
            WRITE_CHUNK(file_output, contents[['iend']])
        }

        close(file_input)
    }
    close(file_output)
}
