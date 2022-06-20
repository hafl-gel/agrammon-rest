
library(data.table)

file_json <- './tests/inputs-version6.json'
file_csv <- './tests/inputs-version6-rest.csv'

# check as csv:
# check mandatory input vars -> ok?
# check residual input -> does it match any optional vars?
# remove invalid input with message?

## 1. input file - conversion from csv to json ----------------------------------------

csv_to_json <- function(file_path) {
    # file_path = file_csv
    # read file 
    # TODO: all characters, what if more than 3 columns provided (template)
    in_dt <- fread(file_path)
    # get instances
    in_dt[, c('is_instance', 'module', 'instance', 'module2') := list(grepl('[', V1, fixed = TRUE), '', '', V1)]
    # get instance names
    in_dt[(is_instance), c('module', 'instance', 'module2') := tstrsplit(V1, split = '[]]|[[]')]
    # clean module2 entry
    in_dt[(is_instance) & !is.na(module2), module2 := sub('^::', '', module2)]
    in_dt[(is_instance) & is.na(module2), module2 := '']
    # setkey (for ordering)
    setkey(in_dt, module, instance, module2)
    # loop over data
    last_by <- list(module = '', instance = '', module2 = 'first')
    e <- environment()
    assign('json_string', '{\n', envir = e)
    in_dt[, {
        # initiate output
        json_out <- ''
        # finish last module except for first time
        if (last_by$module2 != 'first') {
            json_out <- paste0(json_out, '}')
        }
        # finish last instance
        if (.BY$instance != last_by$instance && last_by$instance != '') {
            json_out <- paste0(json_out, '\n}\n}')
        }
        # finish last parent
        if (.BY$module != last_by$module && last_by$module != '') {
            json_out <- paste0(json_out, ']')
        }
        # add comma if not first time
        if (last_by$module2 != 'first') {
            json_out <- paste0(json_out, ',\n')
        }
        # add parent module
        if (.BY$module != last_by$module) {
            # add parent module
            json_out <- paste0(json_out, '"', .BY$module, '": [\n')
        } 
        # add instance
        if (.BY$instance != last_by$instance) {
            # add instance
            json_out <- paste0(json_out, '{\n"name": "', .BY$instance, '",\n"values": {\n')
        }
        # add new (sub-) module
        json_out <- paste0(json_out, '"', .BY$module2, '": {\n')
        # add consecutive values
        for (i in seq_len(.N - 1)) {
            json_out <- paste0(json_out, '"', V2[i], '": ', add_value(V3[i]), ',\n')
        }
        # add last value
        json_out <- paste0(json_out, '"', V2[.N], '": ', add_value(V3[.N]), '\n')
        # get last by
        last_by <- copy(.BY)
        # assign
        assign('json_out', json_out, envir = e)
        evalq(json_string <- paste0(json_string, json_out), envir = e)
    }, by = .(module, instance, module2)]
    # get string
    json_string <- get('json_string', envir = e)
    # finish last instance & parent & end
    json_string <- paste0(json_string, '}\n}\n}\n]\n}')
    # return
    json_string
}

x <- csv_to_json(file_csv)
# write to test file
conv_file <- './tests/converted-csv.json'
writeLines(x, conv_file)
system(paste0('jq "." ', conv_file, ' > ', conv_file, '.tmp && mv ', conv_file, '.tmp ', conv_file))

# try to call model
library(curl)
h <- new_handle()
handle_setheaders(h,
    'Content-Type' = 'multipart/form-data',
    'Authorization' = paste0('Bearer ', Sys.getenv('AGRAMMON_TOKEN')),
    'Accept' = 'application/json'
    )
handle_setopt(h, customrequest = 'POST')
handle_setform(h,
    variants = 'Base',
    model = 'version6',
    technical = 'technical2010.cfg',
    simulation = 'ChristophTest',
    dataset = 'TestSet',
    language = 'de',
    'print-only' = 'SummaryTotal',
    inputs = form_data(x, "application/json")
    # inputs = form_file('./tests/inputs-version6-rest.csv', "text/csv")
)

req <- curl_fetch_memory("https://model.agrammon.ch/singleRest/api/v1/run", handle = h)
rawToChar(req$content)
# stimmt mit csv überein!!



add_value <- function(x) {
    # convert to numeric
    suppressWarnings(y <- as.numeric(x))
    # return value
    if (is.na(y)) {
        paste0('"', x, '"')
    } else {
        x
    }
}


# conversion from json to csv


## 1. output file - conversion from csv to json ----------------------------------------

# conversion from csv to json

# conversion from json to csv
