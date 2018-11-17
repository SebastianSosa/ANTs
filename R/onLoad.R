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


.onAttach <- function(...) {
  		packageStartupMessage("Animal Network Toolkit (ANT) is a free open R package for analysing animal networks.", appendLF = FALSE)
  		packageStartupMessage("ANT is a multi-collaborative project and is meant to continuously evolve. If you wish to contribute by providing suggestions or additional codes please contact us at: ant@s-sosa.com")
  		packageStartupMessage("If you need help or if you want to report bugs, please contact us at: ant.help@s-sosa.com")
  		packageStartupMessage("For citation information, type: citation('ant')")
  		packageStartupMessage("For more information, visit our websites:")
  		packageStartupMessage("    - www.s-sosa.com/ant")
  		packageStartupMessage("    - https://github.com/SebastianSosa/ant")
  		packageStartupMessage("Authors: Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He, Xiaohua Xie, C",paste("\u00e9"),"dric Sueur")
  }
