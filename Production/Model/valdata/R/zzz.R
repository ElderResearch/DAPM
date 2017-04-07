#run when package is loaded
#this forms the data dictionary when the package is loaded
.onLoad <- function(libname = find.package("valdata"), pkgname = "valdata"){
form_data_dictionary()
}
