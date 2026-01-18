lib_loc <- "/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/"
old_pkgs <- installed.packages(lib.loc = lib_loc)
# pak::pkg_install(rownames(old_pkgs) )
install.packages(rownames(old_pkgs) )
