
try:
    # Attempt to load the Intel OpenMP runtime
    import ctypes.util

    openmp_path = ctypes.util.find_library("iomp5")
    if openmp_path:
        ctypes.CDLL(openmp_path, ctypes.RTLD_GLOBAL)
        logger.debug("Successfully loaded Intel OpenMP runtime")
    else:
        logger.error("Intel OpenMP runtime not found.")

    # Load the librmn.so library
    from .sharedlib import librmn
    logger.debug(f"Found librmn.so at: {lib_path}")

    # Setup function signatures
    setup_function_signatures(librmn)

    # Check librmn version
    version = lib.c_fst_version()
    if version < 200001:  # Version 2.0.1
        logger.error(f"Version too old: got {version}, need at least 200001")
        raise ImportError(f"Version too old: got {version}, need at least 200001")
    logger.debug(f"Successfully loaded librmn version {version}")

    # Set C library logging level - try multiple approaches
    result1 = lib.c_fstopi(b"MSGLVL", 3, 0)  # Set to DEBUG level
    result2 = lib.c_fstopi(b"MSGLVL", -1, -1)  # Query current level
    result3 = lib.c_fstopi(b"MSGLVL", 3, 1)  # Set to DEBUG level with different mode
    logger.debug(f"Set C library logging level to DEBUG (results: {result1}, {result2}, {result3})")

except OSError as e:
    logger.error(f"Could not load librmn.so: {e}")
    raise ImportError(f"Could not load librmn.so. Make sure it's in your library path. Error: {e}")
except AttributeError:
    logger.error("Could not verify librmn version")
    raise ImportError("Could not verify librmn version. Library might be incompatible.")


