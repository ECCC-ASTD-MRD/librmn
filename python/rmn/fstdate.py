import ctypes
import numpy as np
from datetime import datetime
from ._sharedlib import librmn

newdate_c = librmn.newdate_c
newdate_c.restype = ctypes.c_int

class fst_date:
    def encode_date(init_date):
        init_date = str(init_date)
        date = ctypes.c_int(int(init_date[:10].replace("-", "")))
        time = ctypes.c_int(int(init_date[11:19].replace(":", "") + "00"))
        datev = ctypes.c_int(0)
        mode = ctypes.c_int(3)
        
        rc = newdate_c(ctypes.byref(datev), ctypes.byref(date), ctypes.byref(time), ctypes.byref(mode))
        if rc != 0:
            raise RuntimeError(f"newdate_c failed: {rc}")
        return datev.value

    
    def decode_date(enc_date):
        date = ctypes.c_int(0)
        time = ctypes.c_int(0)
        datev = ctypes.c_int(int(enc_date))
        mode = ctypes.c_int(-3)
        
        rc = newdate_c(ctypes.byref(datev), ctypes.byref(date), ctypes.byref(time), ctypes.byref(mode))
        if rc != 0:
            raise RuntimeError(f"newdate_c failed: {rc}")
        d = f"{date.value:08d}"
        t = f"{time.value:08d}"
        dt = datetime.strptime(d + t[:6], "%Y%m%d%H%M%S")
        date64 = np.datetime64(dt)
        return date64
