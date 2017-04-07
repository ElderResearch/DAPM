.onLoad <- function(libname, pkgname) {
  # Must be before anything tries to load Java!
  options (java.parameters = "-Xmx6g")

  invisible()
}
