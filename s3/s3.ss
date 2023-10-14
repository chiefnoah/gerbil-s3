;;; -*- Gerbil -*-
;;; (C) vyzo
;;; AWS S3 client
(import "sigv4"
        :std/net/request
        :std/contract
        :std/net/uri
        :std/crypto/digest
        :std/text/hex
        :std/xml
        :std/error
        :std/sugar
        :std/srfi/19)
(export (struct-out s3-client bucket) S3ClientException)

; We're just supporting Amazon Signature v4.

; TODO: define more struct fields
(defstruct s3-client (endpoint access-key secret-key region)
  final: #t
  constructor: :init!)

(defalias S3ClientException RuntimeException)

; Initializes a `s3-client`. Primarily responsible for holding onto credentials
(defmethod {:init! s3-client}
  (lambda (self
            (endpoint "s3.amazonaws.com")
            (access-key (getenv "AWS_ACCESS_KEY_ID" #f))
            (secret-key (getenv "AWS_SECRET_ACCESS_KEY" #f))
            (region (getenv "AWS_DEFAULT_REGION" "us-east-1")))
    (using (self self :- s3-client)
      (set! self.endpoint endpoint)
      (set! self.access-key access-key)
      (set! self.secret-key secret-key)
      (set! self.region region))))

(defmethod {list-buckets s3-client}
  (lambda (self)
    (using (self self :- s3-client)
           (let* ((req {self.request verb: 'GET})
                  (xml (s3-parse-xml req))
                  (buckets (sxml-find xml (sxml-e? 's3:Buckets) sxml-children))
                  (names (map (chain <>
                                     (sxml-select <> (sxml-e? 's3:Name))
                                     (cadar <>)
                                     (make-bucket self <> (s3-client-region self)))
                              buckets)))
           ; buckets is #f if none are returned
           (request-close req)
           names))))

;; NOTE: all bucket operations need the correct region for the bucket or they will 400
#;(defmethod {create-bucket! s3-client}
  (lambda (bucket)
    (using (self self :- s3-client)
      (let (req (s3-request/error verb: 'PUT bucket: bucket))
        (request-close req)
        (void)))))

(defstruct bucket (client name region)
  final: #t)

#;(defmethod {delete-bucket! s3-client}
  (lambda (self b)
    (using ((self self :- s3-client)
            (b b :- bucket))
      (let (req (s3-request/error verb: 'DELETE bucket: b))
        (request-close req)
        (void)))))

#;(defmethod {list-objects bucket}
  (lambda (self)
    (using (self self :- bucket)
           (let* ((req (s3-request/error verb: 'GET bucket: self))
                  (xml (s3-parse-xml req))
                  (keys (sxml-select xml (sxml-e? 's3:Key) cadr)))
             (request-close req)
             keys))))


#;(def (s3-get bucket key)
  (let* ((req (s3-request/error verb: 'GET bucket: bucket
                                path: (string-append "/" key)))
         (data (request-content req)))
    (request-close req)
    data))

#;(def (s3-put! bucket key data
              content-type: (content-type "binary/octet-stream"))
  (let (req (s3-request/error verb: 'PUT bucket: bucket
                              path: (string-append "/" key)
                              body: data
                              content-type: content-type))
    (request-close req)
    (void)))

#;(def (s3-delete! bucket key)
  (let (req (s3-request/error verb: 'DELETE bucket: bucket
                              path: (string-append "/" key)))
    (request-close req)
    (void)))

(defmethod {request s3-client}
  (lambda (self
            verb:   (verb 'GET)
            bucket: (bucket #f)
            path:   (path "/")
            query:  (query #f)
            body:   (body #f)
            content-type: (content-type #f)) ; must be specified if body is specified
    (using (self self :- s3-client)
           (let* ((now (current-date))
                  (ts (date->string now "~Y~m~dT~H~M~SZ"))
                  (scopets (date->string now "~Y~m~d"))
                  (scope (string-append scopets "/" (s3-client-region self) "/s3"))
                  (hash (sha256 (or body '#u8())))
                  (host (if bucket
                          (string-append bucket "." (s3-client-endpoint self))
                          (s3-client-endpoint self)))
                  (headers [["Host" :: (string-append host ":443")]
                            ["x-amz-date" :: ts]
                            ["x-amz-content-sha256" :: (hex-encode hash)]
                            (if body [["Content-Type" :: content-type]] []) ...])
                  (creq (aws4-canonical-request
                          verb: verb
                          uri: path
                          query: query
                          headers: headers
                          hash: hash))
                  (headers [["Authorization" :: (aws4-auth scope creq ts headers
                                                           (s3-client-secret-key self) (s3-client-access-key self))]
                            :: headers])
                  (url (string-append "https://" host path)))
             (case verb
               ((GET)
                (http-get url headers: headers params: query))
               ((PUT)
                (http-put url headers: headers params: query data: body))
               ((DELETE)
                (http-delete url headers: headers params: query))
               ((HEAD)
                (http-head url headers: headers params: query))
               (else
                 (error "Bad request verb" verb)))))))

(defrule (s3-request/error self ...)
         (with-request-error
           (s3-client::s3-request self ...)))

(def (s3-parse-xml req)
     (read-xml (request-content req)
               namespaces: '(("http://s3.amazonaws.com/doc/2006-03-01/" . "s3"))))

(def (with-request-error req)
  (if (and (fx>= (request-status req) 200)
           (fx< (request-status req) 300))
    req
    ;; TODO: proper exception
    (begin
      (request-close req)
      (error "AWS request error"
        (request-status req)
        (request-status-text req)))))
