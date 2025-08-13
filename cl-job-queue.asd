(asdf:defsystem "cl-job-queue"
 :version "0.2.0"
 :author "ryuei sasaki"
 :license ""
 :depends-on ("bordeaux-threads"
              "logur")
 :components ((:module "src"
               :components ((:file "package")
                            (:file "queue" :depends-on ("package"))))))

(asdf:defsystem "cl-job-queue/test"
 :depends-on ("cl-job-queue" "fiveam")
 :components ((:module "test"
               :components ((:file "cl-job-queue-test")))))
