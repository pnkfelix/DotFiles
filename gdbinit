set auto-load local-gdbinit

# add-auto-load-safe-path $debugdir:$datadir/auto-load:$cdir/shell
# add-auto-load-safe-path $debugdir:$datadir/auto-load:$cwd/shell
# add-auto-load-safe-path $cdir/objdir-gcgen-dbg-js/shell
# add-auto-load-safe-path /home/pnkfelix/Dev/Mozilla/iontrail-wip/objdir-gcgen-dbg-js/shell
# add-auto-load-safe-path /home/pnkfelix/Dev/Mozilla

# Struggled for a while with variants like the above, but eventually
# found that the below works (though it may need to be narrowed
# eventually)
add-auto-load-safe-path ~/Dev

# add-auto-load-safe-path ~/Dev/Mozilla/rust-issue27401/src/etc/gdb_load_rust_pretty_printers.py

# Woo!  Did not know about this before!
set disassembly-flavor intel
