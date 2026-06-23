.finalize_zensical <- function(settings, path, ...) {
    # fix links
    settings <- gsub(": \\/", ": ", settings)

    # Fix vignette relative links before calling `zensical`
    vignettes <- list.files(
        fs::path_join(c(.doc_path(path), "vignettes")),
        pattern = "\\.md"
    )
    for (v in vignettes) {
        fn <- fs::path_join(c(.doc_path(path), "vignettes", v))
        txt <- .readlines(fn)
        txt <- gsub(
            paste0("![](", .doc_path(path), "/vignettes/"),
            "![](",
            txt,
            fixed = TRUE
        )
        txt <- gsub(
            sprintf('src=\\"%s.markdown_strict_files', v),
            sprintf('src=\\"\\.\\.\\/%s.markdown_strict_files', v),
            txt
        )
        writeLines(txt, fn)
    }

    # Fix man page relative links
    man <- list.files(
        fs::path_join(c(.doc_path(path), "man")),
        pattern = "\\.md"
    )
    for (v in man) {
        fn <- fs::path_join(c(.doc_path(path), "man", v))
        txt <- .readlines(fn)
        txt <- gsub(
            paste0("![](", .doc_path(path), "/man/"),
            "![](",
            txt,
            fixed = TRUE
        )
        writeLines(txt, fn)
    }

    # write mutable config
    fn <- fs::path_join(c(path, "zensical.toml"))
    writeLines(settings, fn)

    # clean and rebuild index
    if (fs::file_exists(fn)) {
        # read back to allow programmatic manipulation if needed
        settings <- .readlines(fn)
    }

    fn <- fs::path_join(c(path, "docs", "index.html"))
    if (fs::file_exists(fn)) {
        fs::file_delete(fn)
    }

    venv_path <- Sys.getenv("ALTDOC_VENV")
    if (identical(venv_path, "")) {
        venv_path <- ".venv_altdoc"
    }

    # render zensical
    if (.is_windows()) {
        shell(
            paste0(
                "cd ",
                fs::path_abs(path),
                " && ",
                fs::path(venv_path, "Scripts", "zensical"),
                " build -q"
            )
        )
    } else {
        system2(
            "bash",
            paste0(
                "-c 'cd ",
                fs::path_abs(path),
                "&& ",
                venv_path,
                "/bin/zensical build -q'"
            )
        )
    }

    # move to docs/
    fs::file_move(
        fs::path_join(c(path, "zensical.toml")),
        .doc_path(path)
    )
    src <- fs::dir_ls(fs::path_join(c(path, "site/")), recurse = TRUE)
    tar <- sub("/site/", "/docs/", src, fixed = TRUE)

    for (i in seq_along(src)) {
        fs::dir_create(fs::path_dir(tar[i]))
        if (fs::is_file(src[i])) {
            fs::file_copy(src[i], tar[i], overwrite = TRUE)
        }
    }

    fs::dir_delete(fs::path_join(c(path, "site")))

    # remove gitignore that prevents HTML files from being committed
    # (same issue as with mkdocs)
    if (
        fs::file_exists(
            fs::path_join(c(.doc_path(path), "vignettes/.gitignore"))
        )
    ) {
        fs::file_delete(
            fs::path_join(c(.doc_path(path), "vignettes/.gitignore"))
        )
    }
}

.sidebar_vignettes_zensical <- function(sidebar, path) {
    dn <- fs::path_join(c(.doc_path(path), "vignettes"))
    fn_vignettes <- list.files(
        dn,
        pattern = "\\.md$|\\.pdf$",
        full.names = TRUE
    )

    # before gsub on paths
    titles <- sapply(fn_vignettes, .get_vignettes_titles)
    fn_vignettes <- sapply(fn_vignettes, function(x) {
        fs::path_join(c("vignettes", basename(x)))
    })

    if (length(fn_vignettes) > 0) {
        # Replace the $ALTDOC_VIGNETTE_BLOCK placeholder in the nav section
        # of the zensical.toml file with the list of vignettes.
        title_links <- vapply(
            seq_along(fn_vignettes),
            function(i) {
                sprintf(
                    '{ "%s" = "%s" }',
                    titles[[i]],
                    fn_vignettes[[i]]
                )
            },
            character(1)
        )
        vignette_block <- paste0("[", paste(title_links, collapse = ", "), "]")
        sidebar <- gsub(
            '"\\$ALTDOC_VIGNETTE_BLOCK"',
            vignette_block,
            sidebar,
            fixed = FALSE
        )
    } else {
        # Remove the line containing the vignette block placeholder
        sidebar <- sidebar[!grepl("\\$ALTDOC_VIGNETTE_BLOCK", sidebar)]
    }

    return(sidebar)
}

.sidebar_man_zensical <- function(sidebar, path) {
    dn_man <- fs::path_join(c(.doc_path(path), "man"))

    if (fs::dir_exists(dn_man) && length(fs::dir_ls(dn_man)) > 0) {
        fn_man <- list.files(dn_man, pattern = "\\.md$", full.names = TRUE)
        fn_man <- sapply(
            fn_man,
            function(x) fs::path_join(c("man", basename(x)))
        )
        titles <- fs::path_ext_remove(basename(fn_man))

        # Replace the $ALTDOC_MAN_BLOCK placeholder in the nav section
        # of the zensical.toml file with the list of man pages.
        title_links <- vapply(
            seq_along(fn_man),
            function(i) {
                sprintf(
                    '{ "%s" = "%s" }',
                    titles[[i]],
                    fn_man[[i]]
                )
            },
            character(1)
        )
        man_block <- paste0("[", paste(title_links, collapse = ", "), "]")
        sidebar <- gsub(
            '"\\$ALTDOC_MAN_BLOCK"',
            man_block,
            sidebar,
            fixed = FALSE
        )
    } else {
        sidebar <- sidebar[!grepl("\\$ALTDOC_MAN_BLOCK", sidebar)]
    }

    return(sidebar)
}
