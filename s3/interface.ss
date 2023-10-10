(import :std/interface
        :std/contract
        :std/misc/alist)

(export #t)

(interface BucketMap
  (get (name :~ string?))
  (make! (name :~ string?)
         (opts :~ (maybe alist?) := #f))
  (remove! (name :~ string?))
  (exists? (name :~ string?))
  (enumerate))

(interface ObjectMap
  (get (name :~ string?))
  (put! (name :~ string?)
       (data :~ u8vector?)
       ; Additional options. See implementation for additional details
       (opts :~ (maybe alist?) := #f))
  (delete! (name :~ string?))
  (enumerate))
