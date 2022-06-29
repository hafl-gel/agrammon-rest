
# loading namespace agrammon
.onLoad <- function(libname, pkgname) {
    # set agrammon rest api url
    Sys.setenv(agrammon_rest_url =  "https://model.agrammon.ch/singleRest/api/v1")
    invisible()
}

# unloading namespace agrammon
.onUnload <- function(libpath) {
    # unset agrammon rest api url
    Sys.unsetenv('agrammon_rest_url')
    invisible()
}

