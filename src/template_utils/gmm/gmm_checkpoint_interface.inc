interface
    ! If read_or_write is READ_CKPT (.true.) , read full checkpoint (all types)
    ! If read_or_write is WRIT_CKPT (.false.) , write full checkpoint (all types)
    integer function gmm_checkpoint_all(read_or_write)
        logical read_or_write
    end function gmm_checkpoint_all

    integer function gmm_checkpoint184(read_or_write)
        logical read_or_write
    end function gmm_checkpoint184

    integer function gmm_checkpoint144(read_or_write)
        logical read_or_write
    end function gmm_checkpoint144

    integer function gmm_checkpoint284(read_or_write)
        logical read_or_write
    end function gmm_checkpoint284

    integer function gmm_checkpoint244(read_or_write)
        logical read_or_write
    end function gmm_checkpoint244

    integer function gmm_checkpoint384(read_or_write)
        logical read_or_write
    end function gmm_checkpoint384

    integer function gmm_checkpoint183(read_or_write)
        logical read_or_write
    end function gmm_checkpoint183

    integer function gmm_checkpoint143(read_or_write)
        logical read_or_write
    end function gmm_checkpoint143

    integer function gmm_checkpoint283(read_or_write)
        logical read_or_write
    end function gmm_checkpoint283

    integer function gmm_checkpoint243(read_or_write)
        logical read_or_write
    end function gmm_checkpoint243

    integer function gmm_checkpoint383(read_or_write)
        logical read_or_write
    end function gmm_checkpoint383

    integer function gmm_checkpoint182(read_or_write)
        logical read_or_write
    end function gmm_checkpoint182

    integer function gmm_checkpoint142(read_or_write)
        logical read_or_write
    end function gmm_checkpoint142

    integer function gmm_checkpoint282(read_or_write)
        logical read_or_write
    end function gmm_checkpoint282

    integer function gmm_checkpoint242(read_or_write)
        logical read_or_write
    end function gmm_checkpoint242

    integer function gmm_checkpoint382(read_or_write)
        logical read_or_write
    end function gmm_checkpoint382

    integer function gmm_checkpoint181(read_or_write)
        logical read_or_write
    end function gmm_checkpoint181

    integer function gmm_checkpoint141(read_or_write)
        logical read_or_write
    end function gmm_checkpoint141

    integer function gmm_checkpoint281(read_or_write)
        logical read_or_write
    end function gmm_checkpoint281

    integer function gmm_checkpoint241(read_or_write)
        logical read_or_write
    end function gmm_checkpoint241

    integer function gmm_checkpoint381(read_or_write)
        logical read_or_write
    end function gmm_checkpoint381
end interface
