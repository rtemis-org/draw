# ::rtemis.draw::
# 2026- EDG rtemis.org

pkgversion <- utils::packageVersion("rtemis.draw")

.onLoad <- function(libname, pkgname) {
  S7::methods_register()
}

.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage(
      rtemis_welcome(
        pkgname = pkgname,
        pkgversion = pkgversion
      )
    )
  }
}
