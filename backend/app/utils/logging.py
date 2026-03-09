from __future__ import annotations

import logging
from pathlib import Path


def configure_logging(log_path: Path, debug: bool = False) -> None:
    log_path.parent.mkdir(parents=True, exist_ok=True)
    level = logging.DEBUG if debug else logging.INFO
    root_logger = logging.getLogger()
    if root_logger.handlers:
        root_logger.setLevel(level)
        return

    formatter = logging.Formatter(
        "%(asctime)s %(levelname)s %(name)s %(message)s",
        "%Y-%m-%dT%H:%M:%SZ",
    )

    stream_handler = logging.StreamHandler()
    stream_handler.setFormatter(formatter)

    file_handler = logging.FileHandler(log_path)
    file_handler.setFormatter(formatter)

    root_logger.setLevel(level)
    root_logger.addHandler(stream_handler)
    root_logger.addHandler(file_handler)
