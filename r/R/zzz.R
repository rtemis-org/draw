# ::rtemis.draw::
# 2026- EDG rtemis.org

rtemis.draw_version <- utils::packageVersion("rtemis.draw")

.onLoad <- function(libname, pkgname) {
  S7::methods_register()
  # rtemis is a Suggests: the classes these methods dispatch on exist only when
  # it is installed, so they are registered here rather than at build time.
  .register_rtemis_methods()
}

.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage(
      paste0(
        ".:",
        pkgname,
        " ",
        rtemis.draw_version,
        " \U1F58C",
        " ",
        utils::sessionInfo()[[2]]
      )
    )
  }
}

#' Restore shared plotting registration when the namespace unloads
#' @param libpath Character: Library path supplied by the namespace loader.
#' @return NULL, invisibly.
#' @keywords internal
#' @noRd
.onUnload <- function(libpath) {
  restore_massglm_plot()
}
