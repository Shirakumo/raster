(asdf:defsystem raster
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A library for software rasterisation"
  :homepage "https://shirakumo.github.io/raster/"
  :bug-tracker "https://github.com/shirakumo/raster/issues"
  :source-control (:git "https://github.com/shirakumo/raster.git")
  :serial T
  :components ((:file "package")
               (:file "protocol")
               (:file "sampler")
               (:file "sdf")
               (:file "composite")
               (:file "raster")
               (:file "documentation"))
  :depends-on (:documentation-utils))
