let
  maybeAdd = x: list: if builtins.elem x list then list else list ++ [ x ];
in
self: super: {
  gdb-debuginfod = (super.gdb.override { enableDebuginfod = true; }).overrideAttrs (old: {
    configureFlags = maybeAdd "--with-system-gdbinit-dir=/etc/gdb/gdbinit.d" old.configureFlags;
  });
}
