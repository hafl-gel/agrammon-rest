#' title
#'
#' description
#'
#' @param token Agrammon REST-API access token
#' @return  result from an Agrammon model run
#' @export
#' @examples
#' ## examples here
run_model <- function(token) {

    # check if curl is installed
    if (!requireNamespace('curl')) {
        stop('package "curl" is not available!\n\n', 
            '    install.packages("curl")\n\n')
    }

    # create handle
    hdl <- new_handle()

    # set request option to post:
    handle_setopt(hdl, customrequest = 'POST')

    # add header part
    handle_setheaders(hdl,
        'Content-Type' = 'multipart/form-data',
        'Authorization' = paste0('Bearer ', token = token),
        'Accept' = 'text/csv'
        )

}

# TODO:
#   - add function to write csv template with option to provide animal categories & storage tanks?
#   - validate input file
#   - extract simulation & ... from input file, if given
#   - add option to use filters (animal cat)
