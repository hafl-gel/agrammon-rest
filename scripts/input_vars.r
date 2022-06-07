
## 0. header ----------------------------------------

library(jsonlite)
library(data.table)

path_file <- 'tests/inputs.json'

## 1. read dump ----------------------------------------

dj <- read_json(path_file)


read_input_vars <- function(x, language = 'de', module = '') {
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
            # get animal category and slurry phase
            if (grepl('Livestock', module, fixed = TRUE)) {
                tmp[, animal_cat := sub('Livestock::([a-zA-Z]*)[[].*', '\\1', module)]
            }
            # add top module
            tmp[, top_module := sub('^([a-zA-Z]*)::.*', '\\1', module)]
            # bind to out
            out <- rbind(out, cbind(module = module, tmp), fill = TRUE, use.names = TRUE)
        }
        # subtract 'inputs' from names
        nms <- setdiff(nms, 'inputs')
    }
    # loop over sub-modules
    for (nm in nms) {
        out <- rbind(out, read_input_vars(x[[nm]], module = modules[nm]), use.names = TRUE, fill = TRUE)
    }
    out
}


x <- read_input_vars(dj)[, .(module, variable, enums, label, unit, instances, animal_cat, top_module)]

x[!is.na(animal_cat)]
x[lengths(enums) > 0, ]

x[top_module %in% 'Storage']
x[animal_cat %in% 'DairyCow']


## 2. create user template ----------------------------------------

# NOTE:
#   - function with arguments
#       - livestock = list(dairy_cows = c('DC_1', 'DC_2'), heifers_1yr = c('H1_1', 'H1_2'), ...)
#       - storage = list('tank1', 'tank2')
#       - print.livestock = FALSE
#       - oder print.livestock = TRUE, falls empty arguments

create_template <- function(livestock = list(), storage = NULL) {
    # check livestock
    if (!is.list(livestock)) {
        stop('argument livestock must be a named list. Run create_template() to see valid options.')
    }
    # get input dump
    inp_var <- read_input_vars(read_json('tests/inputs.json'))[, .(module, variable, enums, label, unit, instances, animal_cat, top_module)]
    # get names of livestock
    animal_cats <- inp_var[top_module %in% 'Livestock'][variable %in% 'animalcategory', c(unlist(enums), animal_cat)]
    # if names don't match - print valid
    if (length(livestock) == 0 || is.null(names(livestock)) || any(!(names(livestock) %in% animal_cats))) {
        # print invalid livestock input
        if (length(livestock) > 0) {
            nms <- names(livestock)
            # unnamed
            if (is.null(nms) || '' %in% nms) {
                warning('Unnamed list entries in argument "livestock" are invalid!', immediate. = TRUE)
                nms <- setdiff(nms, '')
            }
            # wrong names
            if (length(nms) > 0 && any(wn <- !(nms %in% animal_cats))) {
                if (sum(wn) > 1) {
                    warning('list entry names "', paste(nms[wn], collapse = '", "'),'" in argument "livestock" are invalid!', immediate. = TRUE)
                } else {
                    warning('list entry name "', nms[wn],'" in argument "livestock" is invalid!', immediate. = TRUE)
                }
            }
        }
        # print valid livestock input
        # TODO: change below to Equides: -... -... -.. OtherCattle -... -... etc. | also, add help/label?
        cat('\n*** Valid list entry names for argument "livestock"\nproviding the animal category:\n\t- ')
        inp_var[top_module %in% 'Livestock'][variable %in% 'animalcategory', cat(paste(unlist(enums), collapse = "\n\t- "))]
        cat('\nproviding the animal category\'s parent module:\n\t- ')
        inp_var[top_module %in% 'Livestock'][variable %in% 'animalcategory', cat(paste(animal_cat, collapse = "\n\t- "))]
        cat('\n***\n\n')
    }
}

create_template()
create_template('test1')
create_template(list('test2a'))
create_template(list('test2a', dairy_cows = 'b'))
create_template(list(dc = 'test2b', dairy_cows = 'b'))
create_template(list(dc = 'test2b', bc = 'a', dairy_cows = 'b'))

