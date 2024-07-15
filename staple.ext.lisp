(defmethod staple:subsystems ((_ (eql (asdf:find-system "raster")))) ())

(defpackage "raster-docs"
  (:use #:cl)
  (:local-nicknames
   (#:raster #:org.shirakumo.raster)))

(defclass page* (staple:simple-page)
  ()
  (:default-initargs :document-package (find-package "raster-docs")))

(defmethod staple:page-type ((system (eql (asdf:find-system :raster))))
  'page*)
