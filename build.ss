#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("s3/env"
    "s3/sigv4"
    "s3/s3"))
