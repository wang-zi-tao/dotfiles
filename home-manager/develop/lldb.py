# lldb types

#!/usr/bin/env python3

"""
Rust LLDB launcher script

This script launches LLDB with Rust pretty-printing support.
"""

import subprocess
import sys
import os


def load_rust_types(debugger_obj, internal_dict) -> None:
    """Main entry point for the script."""
    try:
        # Find the host triple so we can find lldb in rustlib
        host_result = subprocess.run(
            ["rustc", "--print", "host-tuple"],
            capture_output=True,
            text=True,
            check=True
        )
        host = host_result.stdout.strip()
        
        # Find out where to look for the pretty printer Python module
        sysroot_result = subprocess.run(
            ["rustc", "--print", "sysroot"],
            capture_output=True,
            text=True,
            check=True
        )
        rustc_sysroot = sysroot_result.stdout.strip()
        
        rust_lldb = os.path.join(rustc_sysroot, "lib", "rustlib", host, "bin", "lldb")
        
        lldb = "lldb"
        if os.path.isfile(rust_lldb):
            lldb = rust_lldb
        else:
            # Check if lldb is available in PATH
            try:
                subprocess.run([lldb, "--version"], capture_output=True, check=True)
            except (subprocess.CalledProcessError, FileNotFoundError):
                print(f"{lldb} not found! Please install it.", file=sys.stderr)
                sys.exit(1)
            
            # Check LLDB version
            version_result = subprocess.run(
                [lldb, "--version"],
                capture_output=True,
                text=True,
                check=True
            )
            version_output = version_result.stdout
            # Extract version number (third field)
            version_parts = version_output.split()
            if len(version_parts) >= 3:
                lldb_version = version_parts[2]
                if lldb_version == "3.5.0":
                    print(
                        "***\n"
                        "WARNING: This version of LLDB has known issues with Rust and cannot display the contents of local variables!\n"
                        "***",
                        file=sys.stderr
                    )
        
        debugger_obj.HandleCommand(f'command script import "{rustc_sysroot}/lib/rustlib/etc/lldb_lookup.py"')
        debugger_obj.HandleCommand(f"command source -s 0 {rustc_sysroot}/lib/rustlib/etc/lldb_commands")
        
    except subprocess.CalledProcessError as e:
        print(f"Error running rustc: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Unexpected error: {e}", file=sys.stderr)
        sys.exit(1)

def __lldb_init_module(debugger_obj, internal_dict):  # pyright: ignore
    load_rust_types(debugger_obj, internal_dict)
