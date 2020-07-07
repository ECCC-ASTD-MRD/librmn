interface
    ! If read_or_write is READ_CKPT (.true.) , read full checkpoint (all types)
    ! If read_or_write is WRIT_CKPT (.false.) , write full checkpoint (all types)
    integer function gmm_checkpoint_all(read_or_write)
        logical read_or_write
    end function gmm_checkpoint_all

    integer function gmm_checkpoint_184(read_or_write)
        logical read_or_write
    end function gmm_checkpoint_184

    integer function gmm_checkpoint_144(read_or_write)
        logical read_or_write
    end function gmm_checkpoint_144

    integer function gmm_checkpoint_284(read_or_write)
        logical read_or_write
    end function gmm_checkpoint_284

    integer function gmm_checkpoint_244(read_or_write)
        logical read_or_write
    end function gmm_checkpoint_244

    integer function gmm_checkpoint_384(read_or_write)
        logical read_or_write
    end function gmm_checkpoint_384

    integer function gmm_checkpoint_183(read_or_write)
        logical read_or_write
    end function gmm_checkpoint_183

    integer function gmm_checkpoint_143(read_or_write)
        logical read_or_write
    end function gmm_checkpoint_143

    integer function gmm_checkpoint_283(read_or_write)
        logical read_or_write
    end function gmm_checkpoint_283

    integer function gmm_checkpoint_243(read_or_write)
        logical read_or_write
    end function gmm_checkpoint_243

    integer function gmm_checkpoint_383(read_or_write)
        logical read_or_write
    end function gmm_checkpoint_383

    integer function gmm_checkpoint_182(read_or_write)
        logical read_or_write
    end function gmm_checkpoint_182

    integer function gmm_checkpoint_142(read_or_write)
        logical read_or_write
    end function gmm_checkpoint_142

    integer function gmm_checkpoint_282(read_or_write)
        logical read_or_write
    end function gmm_checkpoint_282

    integer function gmm_checkpoint_242(read_or_write)
        logical read_or_write
    end function gmm_checkpoint_242

    integer function gmm_checkpoint_382(read_or_write)
        logical read_or_write
    end function gmm_checkpoint_382

    integer function gmm_checkpoint_181(read_or_write)
        logical read_or_write
    end function gmm_checkpoint_181

    integer function gmm_checkpoint_141(read_or_write)
        logical read_or_write
    end function gmm_checkpoint_141

    integer function gmm_checkpoint_281(read_or_write)
        logical read_or_write
    end function gmm_checkpoint_281

    integer function gmm_checkpoint_241(read_or_write)
        logical read_or_write
    end function gmm_checkpoint_241

    integer function gmm_checkpoint_381(read_or_write)
        logical read_or_write
    end function gmm_checkpoint_381
end interface
