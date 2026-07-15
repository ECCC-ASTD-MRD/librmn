"""VirtualiZarr support for librmn fst24 files.

Provides two things:

1. generate_manifest — generates a Kerchunk-style manifest JSON from an
   open :class:`rmn.fst24_file`. No data is read, only metadata
   (offset, length, shape, dtype).

2. Fst24Codec / register_codec — a numcodecs codec that decodes raw
   FST record buffers through librmn. Third-party tools import this codec
   and use it to access FST data.
"""

from __future__ import annotations

import json
import re
from pathlib import Path
from typing import Any

import numpy as np

from .fst24file import fst24_file
from .fstrecord import decode_raw_buffer, fst_record


def _safe_name(value: str) -> str:
    """Convert an FST nomvar to a safe Zarr variable name."""
    safe = re.sub(r"[^0-9A-Za-z_]+", "_", value.strip())
    safe = safe.strip("_") or "record"
    if safe[0].isdigit():
        safe = f"v_{safe}"
    return safe


def _record_attrs(path: str, is_rsf: bool, rec: fst_record) -> dict[str, Any]:
    """Build the .zattrs metadata dict for one FST record."""
    return {
        "fst_path": path,
        "fst_backend": "rsf" if is_rsf else "xdf",
        "fst_file_offset": int(rec.file_offset),
        "fst_total_stored_bytes": int(rec.total_stored_bytes),
        "fst_nomvar": rec.nomvar,
        "fst_typvar": rec.typvar,
        "fst_grtyp": rec.grtyp,
        "fst_etiket": rec.etiket,
        "fst_dateo": int(rec.dateo),
        "fst_datev": int(rec.datev),
        "fst_deet": int(rec.deet),
        "fst_npas": int(rec.npas),
        "fst_ip1": int(rec.ip1),
        "fst_ip2": int(rec.ip2),
        "fst_ip3": int(rec.ip3),
    }


def generate_manifest(
    fst_file: fst24_file,
    output_filename: str | Path,
    *,
    max_records: int | None = None,
) -> Path:
    """Generate a Kerchunk-style manifest JSON from an open fst24 file.

    Each physical FST record becomes its own virtual Zarr array. No data is
    read — only metadata (offset, length, shape, dtype, FST attributes).

    The manifest stores for each record:
    - .zarray: shape, dtype, and which codec (fst24) to use
    - .zattrs: FST metadata (nomvar, ip1, ip2, datev, ...)
    - chunk ref: [file_uri, offset, length] — where the raw bytes are

    Args:
        fst_file: An already-open :class:rmn.fst24_file instance.
        output_filename: Path where the manifest JSON will be written.
        max_records: If set, only include the first N records (useful for
            quick tests).

    Returns:
        The path to the written manifest file.
    """
    path = str(fst_file.filename)
    is_rsf = Path(path).suffix == ".rsf"
    abs_uri = Path(path).resolve().as_uri()  # file:///absolute/path
    backend = "rsf" if is_rsf else "xdf"

    refs: dict[str, Any] = {
        ".zgroup": json.dumps({"zarr_format": 2}, separators=(",", ":")),
        ".zattrs": json.dumps(
            {
                "Conventions": "fst-virtualizarr",
                "fst_virtualizarr_layout": "one_zarr_array_per_physical_record",
            },
            separators=(",", ":"),
        ),
    }

    counters: dict[str, int] = {}
    n = 0

    for rec in fst_file:
        if max_records is not None and n >= max_records:
            break
        if int(rec.file_offset) < 0 or int(rec.total_stored_bytes) <= 0:
            continue

        dtype = np.dtype(rec.numpy_type())
        base_name = _safe_name(rec.nomvar or "record")
        idx = counters.get(base_name, 0)
        counters[base_name] = idx + 1
        var_name = f"{base_name}_{idx:05d}"
        shape = [int(rec.ni), int(rec.nj), int(rec.nk)]
        dims = [f"{var_name}_x", f"{var_name}_y", f"{var_name}_z"]
        chunk_key = ".".join("0" for _ in shape)

        # .zarray: shape, dtype, and codec config
        # compressor tells the reader: use Fst24Codec with this backend
        refs[f"{var_name}/.zarray"] = json.dumps(
            {
                "zarr_format": 2,
                "shape": shape,
                "chunks": shape,
                "dtype": dtype.str,
                "fill_value": None,
                "order": "F",
                "filters": None,
                "compressor": Fst24Codec(backend=backend,order="F",).get_config(),
            },
            separators=(",", ":"),
        )

        # .zattrs: FST metadata
        refs[f"{var_name}/.zattrs"] = json.dumps(
            {"_ARRAY_DIMENSIONS": dims, **_record_attrs(path, is_rsf, rec)},
            separators=(",", ":"),
        )

        # chunk ref: [uri, byte_offset, byte_length]
        # VirtualiZarr/fsspec will read exactly these bytes and pass them to
        # Fst24Codec.decode() when data is accessed
        refs[f"{var_name}/{chunk_key}"] = [
            abs_uri,
            int(rec.file_offset),
            int(rec.total_stored_bytes),
        ]

        n += 1

    with open(output_filename, 'w') as f:
        json.dump({"version": 1, "refs": refs}, f, separators=(",", ":"), ensure_ascii=False)
    return output_filename


def _decode_buffer(buf: bytes, backend: str) -> np.ndarray:
    return decode_raw_buffer(buf, backend)


class Fst24Codec:
    """numcodecs codec that decodes raw FST record buffers via librmn.

    Registered under the id "fst24" in numcodecs. VirtualiZarr/Zarr
    calls :meth:`decode` automatically when a chunk is accessed.

    Third-party tools should never need to call this directly — just call
    :func:`register_codec` once, then open the manifest normally::

        import rmn.virtualizarr
        rmn.virtualizarr.register_codec()

        from virtualizarr import open_virtual_dataset
        ds = open_virtual_dataset("manifest.json", filetype="kerchunk")
        # access data normally — Fst24Codec is called automatically
    """

    codec_id = "fst24"

    def __init__(self, backend: str = "rsf", order: str = "F") -> None:
        backend = backend.lower()
        if backend not in {"rsf", "xdf"}:
            raise ValueError("backend must be 'rsf' or 'xdf'")
        if order not in {"C", "F"}:
            raise ValueError("order must be 'C' or 'F'")
        self.backend = backend
        self.order = order

    def encode(self, buf: Any) -> bytes:
        raise NotImplementedError("Fst24Codec is decode-only")

    def decode(self, buf: Any, out: Any = None) -> bytes:
        arr = _decode_buffer(bytes(buf), backend=self.backend)
        decoded = arr.tobytes(order=self.order)
        if out is not None:
            out_view = memoryview(out)
            out_view[: len(decoded)] = decoded
            return out
        return decoded

    def get_config(self) -> dict:
        return {"id": self.codec_id, "backend": self.backend, "order": self.order}

    @classmethod
    def from_config(cls, config: dict) -> "Fst24Codec":
        return cls(
            backend=config.get("backend", "rsf"),
            order=config.get("order", "F"),
        )


def register_codec() -> None:
    """Register :class:`Fst24Codec` with numcodecs.

    Must be called once before opening a manifest with VirtualiZarr/Zarr.
    Safe to call multiple times.
    """
    try:
        from numcodecs.registry import codec_registry, register_codec as _register
    except ImportError as exc:
        raise ImportError(
            "numcodecs is required to use Fst24Codec. "
            "Install it with: pip install numcodecs"
        ) from exc

    if Fst24Codec.codec_id not in codec_registry:
        _register(Fst24Codec)


register_codec()
