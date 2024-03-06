export.gsi <- function(projectname = "Default", angle = NULL, coordinates = NULL, allcoords = FALSE) {
result <- character()
### Coordinates
    if(!is.null(coordinates)) {
        coo.only <- coordinates
        for(coordrow.num in 1:nrow(coo.only)) {
            result <- c(result,
                        paste(c(
                            align.field(coo.only[coordrow.num, "n"], width = 16,
                                        alignment = "right", fill.with = "0"),
                            paste0("81..10+",
                                   align.field(coo.only[coordrow.num, "x"] * 1000, width = 16,
                                               alignment = "right", fill.with = "0")),
                            paste0("82..10+",
                            align.field(coo.only[coordrow.num, "y"] * 1000, width = 16,
                                        alignment = "right", fill.with = "0")),
                            paste0("83..10+",
                            align.field(coo.only[coordrow.num, "z"] * 1000, width = 16,
                                        alignment = "right", fill.with = "0"))
                        ),
                        collapse = " "
                        )
                        )
        }
    }
### Rowname added
result <- paste(paste0("*11", align.field(1:length(result), width = 4,
                                  alignment = "right", fill.with = "0")), result, sep = "+")
result
}
