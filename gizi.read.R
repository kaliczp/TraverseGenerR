gizi.read <- function(file, firstrow = 1) {
    ## Read in
    raw <- scan(file, character(), sep="\n")
    ## Change curly braces separator to pipe
    simpleraw <- gsub("\\} \\{","|", raw)
    ## Clear first last curly braces
    simpleraw <- sub("\\{","", simpleraw)
    simpleraw <- sub("\\}","", simpleraw)
    ## Split all rows
    splitted <- strsplit(simpleraw, split = "\\|")
    ## Process first selected row
    first <- unlist(strsplit(splitted[[firstrow]], " "))
    ## Substract codes and values in increasing order of codes
    codes <- first[seq(1, length(first), 2)]
    curr.order <- order(as.numeric(codes))
    values <- first[seq(2, length(first), 2)]
    ## Create data.frame based on selected first row
    values.df <- as.data.frame(matrix(values[curr.order], nrow = 1))
    names(values.df) <- paste0("X", codes[curr.order])
    ## If the processing begins not in the first row
    ## Generate empty rows
    if(firstrow > 1) {
        for(repna in 1:(firstrow - 1))
            values.df <- rbind(NA, values.df)
    }
    ## Process all remaining rows
    for(row in (firstrow + 1):length(splitted)) {
        first <- unlist(strsplit(splitted[[row]], " "))
        codes <- first[seq(1, length(first), 2)]
        ## If the row has the same length as the selected
        if(length(codes) == ncol(values.df)) {
            ## Process codes
            curr.order <- order(as.numeric(codes))
            curr.names <- paste0("X", codes[curr.order])
            ## Iff all codes the same
            if(all(curr.names == names(values.df))) {
                ## Process values
                curr.values <- first[seq(2, length(first), 2)]
                curr.values.df <- as.data.frame(matrix(curr.values[curr.order],
                                                       nrow = 1))
                names(curr.values.df) <- curr.names
                ## bind the row to the already processed data.frame
                values.df <- rbind(values.df, curr.values.df)
            } else {
                ## If codes or columns are different replace NA.    
                curr.values.df <- data.frame
                values.df <- rbind(values.df, NA)
            }
        } else {
            curr.values.df <- data.frame
            values.df <- rbind(values.df, NA)
        }
    }
    values.df
}
