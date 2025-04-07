
# -----------------------------------
# Setup Logger
# -----------------------------------
def setup_rmn_logger():
    logging.basicConfig(
        level=logging.DEBUG,
        stream=sys.stdout,
        format="[%(asctime)s.%(msecs)03d] [%(levelname)-7s] %(message)s",
        datefmt="%H:%M:%S",
    )
    logger = logging.getLogger("root")
    return logger


