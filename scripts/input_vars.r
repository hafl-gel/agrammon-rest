
## 0. header ----------------------------------------

library(jsonlite)
library(data.table)

path_file <- 'tests/inputs.json'

## 1. read dump ----------------------------------------

dj <- read_json(path_file)

names(dj)

names(dj[[1]])

names(dj[[1]][[1]])

names(dj[[1]][[1]][[1]])


read_input_vars <- function(x, language = 'en', module = '') {
    nms <- names(x)
    # capture instances
    if ('instances' %in% nms) {
        module <- paste0(module, '[instance_name]')
    }
    nms <- setdiff(nms, 'instances')
    if (module == '') {
        modules <- nms
    } else {
        modules <- paste(module, nms, sep = '::')
    }
    names(modules) <- nms
    out <- NULL
    if ('inputs' %in% nms) {
        y <- x[['inputs']]
        # loop over list
        for (i in seq_along(y)) {
            # extract enums
            if ('enums' %in% names(y[[i]])) {
                enum_index <- which(names(y[[i]]) %in% 'enums')
                enums <- y[[i]][['enums']]
                tmp <- cbind(
                    as.data.table(y[[i]][-enum_index]),
                    enums = list(names(enums)),
                    enums_help_text = list(sapply(enums, '[[', language))
                    )
            } else {
                tmp <- as.data.table(y[[i]])
            }
            # add instance
            tmp[, instances := grepl('[instance_name]', module, fixed = TRUE)]
            # bind to out
            out <- rbind(out, cbind(module = module, tmp), fill = TRUE, use.names = TRUE)
        }
        # subtract 'inputs' from names
        nms <- setdiff(nms, 'inputs')
        # are there any sub lists left?
        for (nm in nms) {
            out <- rbind(out, read_input_vars(x[[nm]], module = modules[nm]), use.names = TRUE, fill = TRUE)
        }
    } else {
        for (nm in nms) {
            out <- rbind(out, read_input_vars(x[[nm]], module = modules[nm]), use.names = TRUE, fill = TRUE)
        }
    }
    out
}


x <- read_input_vars(dj)

