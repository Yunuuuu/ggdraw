.onAttach <- function(libname, pkgname) {

  version <- utils::packageDescription(pkgname, fields = "Version")

  packageStartupMessage(
    "Package: ", pkgname, " (version: ", version, ")\n",
    "Layer with Grid or Function!!"
  )
  invisible()

}

# if(getRversion() >= "2.15.1") utils::globalVariables(
#
# )
