#' Download snapshot
#'
#' Downloads a full copy (snapshot) of an R package repository. The resulting
#' folder can be hosted on any HTTP services to create a cran-like package
#' repository that can be used with `install.packages()` and others.
#'
#' @export
#' @rdname snapshot
#' @param repo url of the cran-like repository to snapshot
#' @param destdir directory in which to store the snapshot repository.
#' @param packages names of packages to include. Default is all packages in the repo.
#' @param win_binaries download binary packages for Windows
#' @param mac_binaries download binary packages for MacOS
#' @param bin_versions vector with versions of R to download the win/mac binary
#' packages. The default is to download binaries only for your local R version.
#' @examples repo_snapshot('https://jeroen.r-universe.dev', 'test', bin_versions = c("4.0", "4.1", "4.2", "4.3"))
#' unlink("test", recursive = TRUE)
repo_snapshot <- function(repo, destdir = NULL, packages = NULL, win_binaries = TRUE, mac_binaries = TRUE, bin_versions = r_version()){
  if(!length(destdir))
    destdir <- chartr('.', '_', sub("/.*$", "", sub("^.*//", "", repo)))
  unlink(destdir, recursive = TRUE)
  dir.create(destdir, showWarnings = FALSE, recursive = TRUE)
  download_single_repo(contrib_path(repo, 'src'), contrib_path(destdir, 'src'), 'src', packages = packages)
  for(rver in unique(major_version(as.character(bin_versions)))){
    if(isTRUE(win_binaries)){
      download_single_repo(contrib_path(repo, 'win', rver), contrib_path(destdir, 'win', rver), 'win', packages = packages)
    }
    if(isTRUE(mac_binaries)){
      download_single_repo(contrib_path(repo, 'mac', rver), contrib_path(destdir, 'mac', rver), 'mac', packages = packages)
    }
  }
  list.files(destdir, recursive = TRUE)
}

repo_index <- function(url){
  con <- curl::curl(file.path(url, c("PACKAGES")))
  on.exit(close(con), add = TRUE)
  as.data.frame(read.dcf(con), stringsAsFactors = FALSE)
}

download_single_repo <- function(url, destdir, type = 'src', packages = NULL){
  unlink(destdir, recursive = TRUE)
  dir.create(destdir, showWarnings = FALSE, recursive = TRUE)
  withr::local_dir(destdir)
  print_err("Mirroring repo: %s ", url)
  df <- repo_index(url)
  if(length(packages)){
    missing <- setdiff(packages, df$Package)
    if(length(missing)){
      stop(sprintf("Requested package %s missing from %s", paste(missing, collapse = ', '), url), call. = FALSE)
    }
    df <- df[df$Package %in% packages,]
  } else if(nrow(df) == 0){
    warning("Repository is empty: ", url, call. = FALSE)
    return(df)
  }
  writeLines(sprintf('Mirror from %s at %s', url, as.character(Sys.time())), 'timestamp.txt')
  pkgfiles <- pkg_file(df$Package, df$Version, type)
  downloads <- paste0(url, '/', pkgfiles)
  results <- curl::multi_download(downloads)
  unlink(results$destfile[results$status_code != 200])
  failed <- pkgfiles[!file.exists(pkgfiles)]
  if(length(failed)){
    stop("Downloading failed for some files: ", paste(failed, collapse = ', '))
  }
  checksums <- unname(tools::md5sum(pkgfiles))
  if(length(df$MD5sum)){
    stopifnot(all.equal(checksums, df$MD5sum))
  } else {
    message(sprintf("No MD5sum values found for %s (adding them now)", url))
    df$MD5sum <- checksums
  }
  write_indexes(df)
  df
}

# See .write_repository_package_db
write_indexes <- function(db){
  write.dcf(db, 'PACKAGES')
  con <- gzfile("PACKAGES.gz", "wt")
  write.dcf(db, con)
  close(con)
  saveRDS(db, "PACKAGES.rds", compress = "xz")
}

pkg_file <- function(package, version, type){
  ext <- switch(type, src = 'tar.gz', win = 'zip', mac = 'tgz', stop("Invalid pkg type"))
  sprintf('%s_%s.%s', package, version, ext)
}

contrib_path <- function(repo, type = 'src', rver = getRversion()){
  ver <- sub("(\\d+\\.\\d+).*", "\\1", rver)
  stopifnot("Invalid R version" = grepl('^\\d+\\.\\d+$', ver))
  switch(type,
         src = sprintf("%s/src/contrib", repo),
         win = sprintf("%s/bin/windows/contrib/%s", repo, ver),
         mac = sprintf("%s/bin/macosx/contrib/%s", repo, ver),
         stop("Invalid type: ", type))
}

download_file_verbose <- function(url){
  cat("Downloading:", url, "\n", file = stderr())
  curl::curl_download(url, basename(url), quiet = TRUE)
}

#' @export
#' @rdname snapshot
r_version <- function(){
  major_version(getRversion())
}

major_version <- function(str){
  sub("^(\\d+\\.\\d+).*", "\\1", str)
}

print_err <- function(...){
  cat(sprintf(...), file = stderr())
}
