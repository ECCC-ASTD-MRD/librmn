import sys
import numpy
import rmn
import rmn.virtualizarr
import tempfile

# This validates the creation of manifests by
#
#     rmn.virtualizarr.generate_manifest(_, MANIFEST_FILE)
#
# by opening it using
#
#     ds = xarray.open_dataset(MANIFEST_FILE, engine="kerchunk", chunks={})
#

try:
    import xarray
except ValueError as e:
    # Not sure what the deal is but with python3.9, I get this error when
    # importing xarray, but when I load a newer version of Python it doesn't
    # happen.
    if "numpy.dtype size changed, may indicate binary incompatibility" in str(e):
        print(f"Error during import of xarray: '{e}'")
        print(f"Using a newer version of Python should solve this")
    else:
        raise
    sys.exit(1)

rmn.virtualizarr.register_codec()

with tempfile.TemporaryDirectory() as d:
    manifest = f"{d}/manifest.json"
    with rmn.fst24_file("/home/smsh001/arcsfc/2026/04/26/regeta/2026042600_000") as f:
        rmn.virtualizarr.generate_manifest(f, manifest)

    # The test itself:
    ds = xarray.open_dataset(manifest, engine="kerchunk", chunks={})
    value = ds['TT_00001'].values[0,0,0]

    # Validation
    # 1. The xarray.open_dataset() was successful
    # 2. The data from TT_00001 at index (0,0,0) is -1.536377
    print(ds, file=sys.stderr)
    expected = -1.536377
    if abs(value - expected) > 0.0001:
        print(f"Expected TT[0,0,0] to be {expected} but got {value}", file=sys.stderr)
        sys.exit(1)
    else:
        sys.exit(0)
