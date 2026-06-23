#' Install Zensical
#'
#' @description
#' Installs the Zensical static site generator into a Python virtual
#' environment using the `uv` package manager, as recommended by the Zensical
#' documentation. The virtual environment is created at `.venv_altdoc` (or at
#' the path specified by the `ALTDOC_VENV` environment variable) in the package
#' root directory.
#'
#' This function is a convenience wrapper around the installation commands
#' documented at <https://zensical.org/docs/get-started/>. It is the
#' recommended way to install Zensical for use with `altdoc`.
#'
#' If `uv` is not available on the system, the function falls back to using
#' `pip` in a standard Python virtual environment.
#'
#' @param path Path to the package root directory.
#' @param force Logical. If `TRUE`, recreates the virtual environment even if
#'   it already exists.
#' @export
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' if (interactive()) {
#'   install_zensical()
#' }
install_zensical <- function(path = ".", force = FALSE) {
    path <- .convert_path(path)

    venv_path <- Sys.getenv("ALTDOC_VENV")
    if (identical(venv_path, "")) {
        venv_path <- fs::path_abs(".venv_altdoc", start = path)
    }

    # Check if venv already exists
    if (fs::dir_exists(venv_path) && !isTRUE(force)) {
        cli::cli_alert_info(
            "Virtual environment already exists at {.path {venv_path}}. Use {.code force = TRUE} to recreate it."
        )
        return(invisible())
    }

    # Try uv first (recommended approach per Zensical docs)
    uv_available <- .uv_available()

    if (isTRUE(uv_available)) {
        .install_zensical_uv(path = path, venv_path = venv_path, force = force)
    } else {
        cli::cli_alert_warning(
            c(
                "{.code uv} is not available on this system. Falling back to {.code pip}.",
                "i" = "The recommended approach is to use {.code uv}. See {.url https://docs.astral.sh/uv/getting-started/installation/} for installation instructions."
            )
        )
        .install_zensical_pip(path = path, venv_path = venv_path, force = force)
    }

    .add_gitignore(".venv_altdoc", path = path)
    .add_rbuildignore(".venv_altdoc", path = path)

    cli::cli_alert_success("Zensical installed at {.path {venv_path}}.")
}

.uv_available <- function() {
    status <- tryCatch(
        {
            if (.is_windows()) {
                system2("where", "uv", stdout = FALSE, stderr = FALSE)
            } else {
                system2("which", "uv", stdout = FALSE, stderr = FALSE)
            }
        },
        error = function(e) 1L
    )
    isTRUE(status == 0L)
}

.install_zensical_uv <- function(path, venv_path, force = FALSE) {
    if (fs::dir_exists(venv_path) && isTRUE(force)) {
        fs::dir_delete(venv_path)
    }

    if (.is_windows()) {
        # Create venv with uv
        system2(
            "uv",
            c("venv", fs::path_abs(venv_path)),
            stdout = TRUE,
            stderr = TRUE
        )
        # Install zensical into the venv
        system2(
            "uv",
            c("pip", "install", "--python", fs::path(venv_path, "Scripts", "python"), "zensical"),
            stdout = TRUE,
            stderr = TRUE
        )
    } else {
        system2(
            "uv",
            c("venv", fs::path_abs(venv_path)),
            stdout = TRUE,
            stderr = TRUE
        )
        system2(
            "uv",
            c("pip", "install", "--python", fs::path(venv_path, "bin", "python"), "zensical"),
            stdout = TRUE,
            stderr = TRUE
        )
    }
}

.install_zensical_pip <- function(path, venv_path, force = FALSE) {
    if (fs::dir_exists(venv_path) && isTRUE(force)) {
        fs::dir_delete(venv_path)
    }

    if (.is_windows()) {
        system2(
            "python",
            c("-m", "venv", fs::path_abs(venv_path)),
            stdout = TRUE,
            stderr = TRUE
        )
        system2(
            fs::path(venv_path, "Scripts", "pip"),
            c("install", "zensical"),
            stdout = TRUE,
            stderr = TRUE
        )
    } else {
        system2(
            "python3",
            c("-m", "venv", fs::path_abs(venv_path)),
            stdout = TRUE,
            stderr = TRUE
        )
        system2(
            fs::path(venv_path, "bin", "pip"),
            c("install", "zensical"),
            stdout = TRUE,
            stderr = TRUE
        )
    }
}

# Check if the zensical command is available in the venv
.zensical_installed <- function(path = ".") {
    venv_path <- Sys.getenv("ALTDOC_VENV")
    if (identical(venv_path, "")) {
        venv_path <- fs::path_abs(".venv_altdoc", start = path)
    }

    if (!fs::dir_exists(venv_path)) {
        return(FALSE)
    }

    if (.is_windows()) {
        zensical_bin <- fs::path(venv_path, "Scripts", "zensical.exe")
    } else {
        zensical_bin <- fs::path(venv_path, "bin", "zensical")
    }

    fs::file_exists(zensical_bin)
}

# Check if a uv-managed environment exists (analogous to .venv_exists for mkdocs)
.uv_exists <- function(path = ".") {
    venv_path <- Sys.getenv("ALTDOC_VENV")
    if (identical(venv_path, "")) {
        venv_path <- fs::path_abs(".venv_altdoc", start = path)
    }
    fs::dir_exists(venv_path)
}
