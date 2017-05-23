
.onAttach <- function(libname, pkgname) {
    packageStartupMessage("Welcome to rspecies package.")
}


.onLoad <- function(libname, pkgname) {
    op <- options()
    op.rspecies <- list(rspecies.path = "~/R", rspecies.install.args = "", rspecies.name = "Enrique Del Callejo-Canal", 
        rspecies.desc.author = "\"Enrique Del Callejo-Canal <edelcallejoc@gmail.com> [aut, cre]\"", 
        rspecies.desc.license = "Pendiente", rspecies.desc.suggests = NULL, rspecies.desc = list())
    toset <- !(names(op.rspecies) %in% names(op))
    if (any(toset)) 
        options(op.rspecies[toset])
    
    invisible()
} 
