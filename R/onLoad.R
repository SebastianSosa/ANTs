# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He,Peng Zhang, Xiaohua Xie, Cédric Sueur
#
# This file is part of Animal Network Toolkit (ANT).
#
# ANT is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# ANT is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

.onLoad<-function(libname, pkgname){
  X=c(
    'Animal Network Toolkit (ANT) is a free open R package for analysing animal networks.','\n',
    'ANT is a multi-collaborative project and is meant to continuously evolve. If you wish to contribute by
     providing suggestions or additional codes please contact us at: ant@s-sosa.com','\n',
    'If you need help or if you want to report bugs, please contact us at: ant.help@s-sosa.com','\n',
    "For citation information, type: citation('ant')",'\n',
    'For more information, visit our websites:','\n',
    '    - www.s-sosa.com/ant','\n',
    '    - https://github.com/SebastianSosa/ant','\n',
    'Authors: Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He, Peng Zhang, Xiaohua Xie, C\xe9dric Sueur'
  )
  X=writeLines(strwrap(X,width = 60, indent = 2,exdent = 2))
  packageStartupMessage(X)

}
